{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ten.Derivation (
    -- Core types reexported from Ten.Core
    DerivationChain,

    -- Derivation creation and inspection
    mkDerivation,
    instantiateDerivation,
    derivationEquals,
    derivationOutputPaths,
    derivationInputPaths,

    -- Return-Continuation pattern
    joinDerivation,
    buildToGetInnerDerivation,
    isReturnContinuationDerivation,

    -- Phase-specific derivation storage
    storeDerivationEval,
    storeDerivationBuild,
    retrieveDerivationEval,
    retrieveDerivationBuild,

    -- Serialization
    serializeDerivation,
    deserializeDerivation,
    derivationToJSON,
    derivationFromJSON,

    -- Hashing and identity
    hashDerivation,
    hashDerivationInputs,

    -- Build strategy analysis
    analyzeDerivationStrategy,

    -- Derivation chain handling
    newDerivationChain,
    addToDerivationChain,
    derivationChainLength,
) where

import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (modify, gets, get)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromMaybe)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Vector as Vector
import System.FilePath
import System.Directory (doesFileExist, createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Posix.Files as Posix
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode, StdStream(NoStream, CreatePipe))
import System.Exit
import Control.Exception (try, catch, finally, SomeException)
import Data.Kind (Type)
import Crypto.Hash (Digest, SHA256(..), hash)
import qualified Crypto.Hash as Crypto
import Data.ByteArray.Encoding (convertToBase, Base(Base16))

import qualified Ten.Core as Core
import qualified Ten.Store as Store

-- | Represents a chain of recursive derivations
data DerivationChain = DerivationChain [Text] -- Chain of derivation hashes
    deriving (Show, Eq)

-- | Create a new derivation chain
newDerivationChain :: Core.Derivation -> DerivationChain
newDerivationChain drv = DerivationChain [Core.derivHash drv]

-- | Get chain length
derivationChainLength :: DerivationChain -> Int
derivationChainLength (DerivationChain hashes) = length hashes

-- | Hash a derivation to get its textual representation
hashDerivation :: Core.Derivation -> Text
hashDerivation drv = hashBlob (serializeDerivation drv)

-- | Create a new derivation - works in Eval phase only
mkDerivation :: Text -> Core.StorePath -> [Text] -> Set Core.DerivationInput
             -> Set Text -> Map Text Text -> Text -> Core.TenM 'Core.Eval t Core.Derivation
mkDerivation name builder args inputs outputNames env system = do
    -- First calculate a deterministic hash for the derivation
    let hashBase = T.concat
            [ name
            , T.pack $ show builder
            , T.intercalate "," args
            , T.intercalate "," $ map (T.pack . show) $ Set.toList inputs
            , T.intercalate "," $ Set.toList outputNames
            , T.intercalate ";" $ map (\(k, v) -> k <> "=" <> v) $ Map.toList env
            , system
            ]

    let derivHash' = hashBlob (TE.encodeUtf8 hashBase)

    -- Create output specifications with predicted paths
    let outputs = Set.map (\outName -> Core.DerivationOutput
                              { Core.outputName = outName
                              , Core.outputPath = Core.StorePath derivHash' outName
                              }) outputNames

    -- Analyze what build strategy this derivation should use
    let strategy = analyzeDerivationStrategy name args env

    -- Add metadata if it's a return-continuation derivation
    let meta = if strategy == Core.MonadicStrategy && isReturnContinuationDerivation name args env
                  then Map.singleton "returnContinuation" "true"
                  else Map.empty

    -- Record proof that this is a valid derivation
    Core.addProof Core.TypeProof

    return Core.Derivation
        { Core.derivName = name
        , Core.derivHash = derivHash'
        , Core.derivBuilder = builder
        , Core.derivArgs = args
        , Core.derivInputs = inputs
        , Core.derivOutputs = outputs
        , Core.derivEnv = env
        , Core.derivSystem = system
        , Core.derivStrategy = strategy
        , Core.derivMeta = meta
        }

-- | Hash a ByteString blob and return as Text
hashBlob :: BS.ByteString -> Text
hashBlob bs =
    let digest = hash bs :: Digest SHA256
        hexBytes = convertToBase Base16 digest
    in TE.decodeUtf8 hexBytes

-- | Analyze a derivation to determine optimal build strategy
analyzeDerivationStrategy :: Text -> [Text] -> Map Text Text -> Core.BuildStrategy
analyzeDerivationStrategy name args env =
    -- Check various indicators that this derivation uses return-continuation
    if isReturnContinuationDerivation name args env
        then Core.MonadicStrategy
        else
            -- Check for other indicators that require dynamic dependency resolution
            if any isDynamicDependencyArg args || Map.member "TEN_DYNAMIC_DEPS" env
                then Core.MonadicStrategy
                else Core.ApplicativeStrategy
  where
    -- Check if an argument suggests dynamic dependencies
    isDynamicDependencyArg arg =
        "dynamicDeps" `T.isInfixOf` arg ||
        "loadDeps" `T.isInfixOf` arg ||
        "resolveAt" `T.isInfixOf` arg

-- | Check if a derivation uses return-continuation
isReturnContinuationDerivation :: Text -> [Text] -> Map Text Text -> Bool
isReturnContinuationDerivation name args env =
    -- Check various indicators that this derivation uses return-continuation
    Map.member "TEN_RETURN_CONTINUATION" env ||
    Map.member "TEN_INNER_DERIVATION" env ||
    "return-drv" `T.isInfixOf` name ||
    "returnDerivation" `T.isInfixOf` name ||
    any (`T.isInfixOf` name) ["stage1", "phase1", "bootstrap-"] ||
    any returnsDerivation args
  where
    -- Check if an argument suggests returning a derivation
    returnsDerivation arg =
        "return-drv" `T.isInfixOf` arg ||
        "returnDerivation" `T.isInfixOf` arg ||
        "--return" `T.isPrefixOf` arg ||
        "--output-derivation" `T.isPrefixOf` arg

-- | Store a derivation in the Eval phase
-- This function is specific to the Eval phase to enforce phase separation
storeDerivationEval :: Core.Derivation -> Core.TenM 'Core.Eval t Core.StorePath
storeDerivationEval drv = do
    -- Validate the derivation before storing
    validateDerivation drv

    -- Serialize the derivation to a ByteString
    let serialized = serializeDerivation drv
    let fileName = Core.derivName drv <> ".drv"

    -- Check the privilege tier and use appropriate method
    env <- ask
    case Core.currentPrivilegeTier env of
        Core.Daemon ->
            -- Use direct store access for daemon privilege
            Core.storeDerivationDaemon drv
        Core.Builder ->
            -- Use protocol to request storage in builder context
            Core.storeDerivationBuilder drv

-- | Store a derivation in the Build phase
-- This function is specific to the Build phase to enforce phase separation
storeDerivationBuild :: Core.Derivation -> Core.TenM 'Core.Build t Core.StorePath
storeDerivationBuild drv = do
    -- Validate the derivation before storing
    validateDerivation drv

    -- Serialize the derivation to a ByteString
    let serialized = serializeDerivation drv
    let fileName = Core.derivName drv <> ".drv"

    -- Check the privilege tier and use appropriate method
    env <- ask
    case Core.currentPrivilegeTier env of
        Core.Daemon ->
            -- Use direct store access for daemon privilege
            Core.storeDerivationDaemon drv
        Core.Builder ->
            -- Use protocol to request storage in builder context
            Core.storeDerivationBuilder drv

-- | Retrieve a derivation in the Eval phase
retrieveDerivationEval :: Core.StorePath -> Core.TenM 'Core.Eval t (Maybe Core.Derivation)
retrieveDerivationEval path = retrieveDerivation path

-- | Retrieve a derivation in the Build phase
retrieveDerivationBuild :: Core.StorePath -> Core.TenM 'Core.Build t (Maybe Core.Derivation)
retrieveDerivationBuild path = retrieveDerivation path

-- | Retrieve a derivation from the store - common implementation for both phases
retrieveDerivation :: Core.StorePath -> Core.TenM p t (Maybe Core.Derivation)
retrieveDerivation path = do
    -- Validate the path before attempting to retrieve
    unless (Core.validateStorePath path) $
        throwError $ Core.StoreError $ "Invalid store path format: " <> Core.storePathToText path

    -- Check if the path exists in the store
    exists <- Store.storePathExists path
    if not exists
        then return Nothing
        else do
            -- Get content based on privilege tier
            env <- ask
            content <- case Core.currentPrivilegeTier env of
                Core.Daemon -> do
                    -- Direct read in daemon context using proper type-safe API
                    result <- Core.readDerivationDaemon path
                    case result of
                        Left err -> throwError err
                        Right drv -> return $ serializeDerivation drv

                Core.Builder -> do
                    -- First try direct read (works if file is accessible)
                    -- If that fails, use protocol request
                    let filePath = Core.storePathToFilePath path env
                    fileExists <- liftIO $ doesFileExist filePath
                    if fileExists
                        then do
                            contentEither <- liftIO $ try $ BS.readFile filePath
                            case contentEither of
                                Right content -> return content
                                Left (_ :: SomeException) -> readViaProtocolBuilder path
                        else readViaProtocolBuilder path

            -- Deserialize the content
            case deserializeDerivation content of
                Left err -> do
                    Core.logMsg 1 $ "Error deserializing derivation: " <> T.pack (show err)
                    return Nothing
                Right drv -> return $ Just drv

-- | Helper to read via daemon protocol - Builder specific version
readViaProtocolBuilder :: Core.StorePath -> Core.TenM p 'Core.Builder ByteString
readViaProtocolBuilder path = do
    result <- Core.readDerivationBuilder path
    case result of
        Left err -> throwError err
        Right drv -> return $ serializeDerivation drv

-- | Instantiate a derivation for building - context-aware implementation
instantiateDerivation :: Core.Derivation -> Core.TenM 'Core.Build t ()
instantiateDerivation deriv = do
    -- Store the derivation using the phase-specific implementation
    derivPath <- storeDerivationBuild deriv

    -- Verify inputs exist
    forM_ (Core.derivInputs deriv) $ \input -> do
        exists <- Store.storePathExists (Core.inputPath input)
        unless exists $ throwError $
            Core.BuildFailed $ "Input does not exist: " <> T.pack (show $ Core.inputPath input)

    -- Record inputs in build state
    let inputPaths = Set.map Core.inputPath (Core.derivInputs deriv)
    modify $ \s -> s { Core.buildInputs = Set.union inputPaths (Core.buildInputs s) }

    -- Verify builder exists
    builderExists <- Store.storePathExists (Core.derivBuilder deriv)
    unless builderExists $ throwError $
        Core.BuildFailed $ "Builder does not exist: " <> T.pack (show $ Core.derivBuilder deriv)

    -- Track this derivation in the build chain for recursion detection
    chain <- gets Core.buildChain
    limit <- asks Core.maxRecursionDepth
    when (length chain >= limit) $
        throwError $ Core.RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show limit)

    -- Check for cycles in build chain using isInDerivationChain from Ten.Core
    isCyclic <- Core.isInDerivationChain deriv
    when isCyclic $
        throwError $ Core.CyclicDependency $ "Cyclic derivation detected: " <> Core.derivName deriv

    -- Add to build chain
    modify $ \s -> s { Core.buildChain = deriv : Core.buildChain s }

    -- Add input proof
    Core.addProof Core.InputProof

    Core.logMsg 1 $ "Instantiated derivation: " <> Core.derivName deriv

-- | Add a derivation to the chain
addToDerivationChain :: Core.Derivation -> DerivationChain -> DerivationChain
addToDerivationChain drv (DerivationChain hashes) =
    DerivationChain (Core.derivHash drv : hashes)

-- | The monadic join operation for Return-Continuation - context-aware implementation
joinDerivation :: Core.Derivation -> Core.TenM 'Core.Build t Core.Derivation
joinDerivation outerDrv = do
    -- Build the outer derivation just enough to get inner derivation
    innerDrv <- buildToGetInnerDerivation outerDrv

    -- Record that we performed a join operation
    Core.addProof Core.ReturnProof

    -- Return the inner derivation to be built
    return innerDrv

-- | Build a derivation to the point where it returns an inner derivation
buildToGetInnerDerivation :: Core.Derivation -> Core.TenM 'Core.Build t Core.Derivation
buildToGetInnerDerivation drv = do
    env <- ask
    state <- get

    -- Create a sandbox configuration with return-continuation support
    let sandboxConfig = defaultSandboxConfig {
            sandboxReturnSupport = True,
            sandboxPrivileged = Core.currentPrivilegeTier env == Core.Daemon
        }

    -- Use withSandbox which handles both contexts correctly
    withSandbox (Set.map Core.inputPath $ Core.derivInputs drv) sandboxConfig $ \sandboxDir -> do
        -- Get the builder from the store - context-aware implementation
        builderContent <- Store.readFromStore (Core.derivBuilder drv)

        -- Write the builder to the sandbox
        let builderPath = sandboxDir </> "builder"
        liftIO $ BS.writeFile builderPath builderContent
        liftIO $ Posix.setFileMode builderPath 0o755

        -- Prepare environment variables
        let buildEnv = prepareSandboxEnvironment env state sandboxDir (Core.derivEnv drv)

        -- Add context-specific environment variables
        let contextEnv = case Core.currentPrivilegeTier env of
                            Core.Daemon -> Map.singleton "TEN_DAEMON" "1"
                            Core.Builder -> Map.singleton "TEN_BUILDER" "1"

        let fullEnv = Map.union buildEnv contextEnv

        -- Configure the process
        let processConfig = sandboxedProcessConfig
                sandboxDir
                builderPath
                (map T.unpack $ Core.derivArgs drv)
                fullEnv
                sandboxConfig

        -- Run the builder
        (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode processConfig ""

        -- Check if the builder wrote a derivation file
        let returnPath = returnDerivationPath sandboxDir
        hasReturnDrv <- liftIO $ doesFileExist returnPath

        -- Handle different outcomes
        if exitCode == ExitSuccess && hasReturnDrv
            then do
                -- Read the returned derivation
                content <- liftIO $ BS.readFile returnPath
                case deserializeDerivation content of
                    Left err -> throwError $ Core.SerializationError $
                        "Failed to deserialize returned derivation: " <> T.pack (show err)
                    Right innerDrv -> do
                        -- Add proof that we successfully got a returned derivation
                        Core.addProof Core.RecursionProof

                        -- Store the inner derivation - context-aware implementation
                        void $ storeDerivationBuild innerDrv

                        return innerDrv
            else
                throwError $ Core.BuildFailed $
                    "Builder did not return a derivation at: " <> T.pack returnPath <>
                    "\nExit code: " <> T.pack (show exitCode) <>
                    "\nStdout: " <> T.pack stdout <>
                    "\nStderr: " <> T.pack stderr

-- | Get output paths from a derivation
derivationOutputPaths :: Core.Derivation -> Set Core.StorePath
derivationOutputPaths = Set.map Core.outputPath . Core.derivOutputs

-- | Get input paths from a derivation
derivationInputPaths :: Core.Derivation -> Set Core.StorePath
derivationInputPaths = Set.map Core.inputPath . Core.derivInputs

-- | Serialize a derivation to a ByteString
serializeDerivation :: Core.Derivation -> BS.ByteString
serializeDerivation = LBS.toStrict . Aeson.encode . derivationToJSON

-- | Convert derivation to JSON
derivationToJSON :: Core.Derivation -> Aeson.Value
derivationToJSON Core.Derivation{..} = Aeson.object
    [ "name" .= Core.derivName
    , "hash" .= Core.derivHash
    , "builder" .= storePathToJSON Core.derivBuilder
    , "args" .= Core.derivArgs
    , "inputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList Core.derivInputs)
    , "outputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList Core.derivOutputs)
    , "env" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList Core.derivEnv)
    , "system" .= Core.derivSystem
    , "strategy" .= (case Core.derivStrategy of
                        Core.ApplicativeStrategy -> "applicative" :: Text
                        Core.MonadicStrategy -> "monadic")
    , "meta" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList Core.derivMeta)
    ]

-- Helper to convert StorePath to JSON
storePathToJSON :: Core.StorePath -> Aeson.Value
storePathToJSON (Core.StorePath hash name) = Aeson.object
    [ "hash" .= hash
    , "name" .= name
    ]

-- Make DerivationInput JSON serializable
instance Aeson.ToJSON Core.DerivationInput where
    toJSON (Core.DerivationInput path name) = Aeson.object
        [ "path" .= storePathToJSON path
        , "name" .= name
        ]

-- Make DerivationOutput JSON serializable
instance Aeson.ToJSON Core.DerivationOutput where
    toJSON (Core.DerivationOutput name path) = Aeson.object
        [ "name" .= name
        , "path" .= storePathToJSON path
        ]

-- | Deserialize a derivation from a ByteString
deserializeDerivation :: BS.ByteString -> Either Core.BuildError Core.Derivation
deserializeDerivation bs =
    case Aeson.eitherDecode (LBS.fromStrict bs) of
        Left err -> Left $ Core.SerializationError $ "JSON parse error: " <> T.pack err
        Right json -> case derivationFromJSON json of
            Left err -> Left $ Core.SerializationError err
            Right drv -> Right drv

-- | Convert JSON to Derivation
derivationFromJSON :: Aeson.Value -> Either Text Core.Derivation
derivationFromJSON v =
    case Aeson.parseEither parseDerivation v of
        Left err -> Left $ T.pack err
        Right drv -> Right drv
  where
    parseDerivation = Aeson.withObject "Derivation" $ \o -> do
        name <- o .: "name"
        hash <- o .: "hash"
        builderJSON <- o .: "builder"
        args <- o .: "args"
        inputsJSON <- o .: "inputs"
        outputsJSON <- o .: "outputs"
        envObj <- o .: "env"
        system <- o .: "system"
        strategyText <- o .: "strategy" :: Aeson.Parser Text
        metaObj <- o .: "meta"

        -- Parse nested objects
        builder <- parseStorePath builderJSON

        -- Parse arrays
        inputs <- parseInputs inputsJSON
        outputs <- parseOutputs outputsJSON

        -- Parse maps
        env <- parseEnvMap envObj
        meta <- parseEnvMap metaObj

        -- Parse strategy
        let strategy = case strategyText of
                "monadic" -> Core.MonadicStrategy
                _ -> Core.ApplicativeStrategy

        return Core.Derivation
            { Core.derivName = name
            , Core.derivHash = hash
            , Core.derivBuilder = builder
            , Core.derivArgs = args
            , Core.derivInputs = Set.fromList inputs
            , Core.derivOutputs = Set.fromList outputs
            , Core.derivEnv = env
            , Core.derivSystem = system
            , Core.derivStrategy = strategy
            , Core.derivMeta = meta
            }

    parseStorePath :: Aeson.Value -> Aeson.Parser Core.StorePath
    parseStorePath = Aeson.withObject "StorePath" $ \o -> do
        hash <- o .: "hash"
        name <- o .: "name"
        return $ Core.StorePath hash name

    parseInputs :: Aeson.Value -> Aeson.Parser [Core.DerivationInput]
    parseInputs = Aeson.withArray "Inputs" $ \arr ->
        mapM parseInput (Vector.toList arr)

    parseInput :: Aeson.Value -> Aeson.Parser Core.DerivationInput
    parseInput = Aeson.withObject "DerivationInput" $ \o -> do
        pathObj <- o .: "path"
        name <- o .: "name"
        path <- parseStorePath pathObj
        return $ Core.DerivationInput path name

    parseOutputs :: Aeson.Value -> Aeson.Parser [Core.DerivationOutput]
    parseOutputs = Aeson.withArray "Outputs" $ \arr ->
        mapM parseOutput (Vector.toList arr)

    parseOutput :: Aeson.Value -> Aeson.Parser Core.DerivationOutput
    parseOutput = Aeson.withObject "DerivationOutput" $ \o -> do
        name <- o .: "name"
        pathObj <- o .: "path"
        path <- parseStorePath pathObj
        return $ Core.DerivationOutput name path

    parseEnvMap :: Aeson.Value -> Aeson.Parser (Map Text Text)
    parseEnvMap = Aeson.withObject "Environment" $ \obj ->
        return $ Map.fromList $ catMaybes $ map convertKeyValue $ Aeson.KeyMap.toList obj
      where
        convertKeyValue (k, Aeson.String v) = Just (Aeson.toText k, v)
        convertKeyValue _ = Nothing  -- Ignore non-string values

-- | Hash a derivation's inputs for dependency tracking
hashDerivationInputs :: Core.Derivation -> Text
hashDerivationInputs drv =
    T.pack $ show $ Aeson.encode $
        map (\input -> Core.storeHash $ Core.inputPath input) $
        Set.toList $ Core.derivInputs drv

-- | Create a sandbox process configuration
sandboxedProcessConfig :: FilePath -> FilePath -> [String] -> Map Text Text -> SandboxConfig -> CreateProcess
sandboxedProcessConfig sandboxDir programPath args envVars config =
    (proc programPath args)
        { cwd = Just sandboxDir
        , env = Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList envVars
        , std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

-- | Validate a derivation before storage or building
validateDerivation :: Core.Derivation -> Core.TenM p t ()
validateDerivation drv = do
    -- Validate the hash
    unless (T.length (Core.derivHash drv) >= 8 && T.all isHexDigit (Core.derivHash drv)) $
        throwError $ Core.SerializationError $ "Invalid derivation hash: " <> Core.derivHash drv

    -- Validate the builder path
    unless (Core.validateStorePath (Core.derivBuilder drv)) $
        throwError $ Core.SerializationError $ "Invalid builder path: " <> T.pack (show (Core.derivBuilder drv))

    -- Validate all input paths
    forM_ (Core.derivInputs drv) $ \input ->
        unless (Core.validateStorePath (Core.inputPath input)) $
            throwError $ Core.SerializationError $ "Invalid input path: " <> T.pack (show (Core.inputPath input))

    -- Validate all output paths
    forM_ (Core.derivOutputs drv) $ \output ->
        unless (Core.validateStorePath (Core.outputPath output)) $
            throwError $ Core.SerializationError $ "Invalid output path: " <> T.pack (show (Core.outputPath output))

    -- Validate that name isn't empty
    when (T.null (Core.derivName drv)) $
        throwError $ Core.SerializationError "Derivation name cannot be empty"

-- | Type for sandbox configuration
data SandboxConfig = SandboxConfig {
    sandboxReturnSupport :: Bool,
    sandboxPrivileged :: Bool,
    sandboxAllowNetwork :: Bool,
    sandboxReadOnlyStore :: Bool,
    sandboxUseChroot :: Bool,
    sandboxEnableSyslog :: Bool,
    sandboxKeepBuildOutput :: Bool,
    sandboxUseCgroups :: Bool,
    sandboxMountProc :: Bool,
    sandboxEnvWhitelist :: [Text],
    sandboxResourceLimits :: Map Text Int
}

-- | Default sandbox configuration
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig {
    sandboxReturnSupport = False,
    sandboxPrivileged = False,
    sandboxAllowNetwork = False,
    sandboxReadOnlyStore = True,
    sandboxUseChroot = True,
    sandboxEnableSyslog = False,
    sandboxKeepBuildOutput = False,
    sandboxUseCgroups = True,
    sandboxMountProc = False,
    sandboxEnvWhitelist = ["PATH", "HOME", "USER", "LOGNAME", "LANG", "LC_ALL", "TZ"],
    sandboxResourceLimits = Map.fromList [
        ("memory", 2 * 1024 * 1024),  -- 2 GB in KB
        ("cpu-time", 3600),           -- 1 hour in seconds
        ("stack-size", 8 * 1024),     -- 8 MB in KB
        ("processes", 20)             -- Max child processes
    ]
}

-- | Create a sandbox for building
withSandbox :: Set Core.StorePath -> SandboxConfig -> (FilePath -> Core.TenM 'Core.Build t a) -> Core.TenM 'Core.Build t a
withSandbox inputs config action = do
    env <- ask

    -- Determine the current build ID
    buildId <- gets Core.currentBuildId

    -- Create a temporary directory for the sandbox
    let sandboxDir = Core.workDir env </> "sandbox" </> "temp-" ++ show buildId
    liftIO $ createDirectoryIfMissing True sandboxDir

    -- Set up the sandbox with appropriate privilege and isolation
    case Core.currentPrivilegeTier env of
        Core.Daemon ->
            -- For daemon, we can set up a full sandbox with proper isolation
            setupDaemonSandbox env inputs config sandboxDir

        Core.Builder ->
            -- For builder, we use a simplified sandbox since we can't drop privileges
            setupBuilderSandbox env inputs config sandboxDir

    -- Run the action and ensure cleanup happens even if it fails
    result <- catchError (action sandboxDir) $ \e -> do
        -- Clean up sandbox on error unless we're keeping the output
        unless (sandboxKeepBuildOutput config) $
            liftIO $ cleanupSandbox sandboxDir
        throwError e

    -- Clean up sandbox unless we're keeping the output
    unless (sandboxKeepBuildOutput config) $
        liftIO $ cleanupSandbox sandboxDir

    return result
  where
    -- Full privileged sandbox setup for daemon tier
    setupDaemonSandbox env inputs config dir = do
        -- Make store inputs available in the sandbox
        forM_ (Set.toList inputs) $ \path -> do
            -- Create symlinks to store paths
            let storePath = Core.storePathToFilePath path env
            let targetPath = dir </> "store" </> T.unpack (T.concat [Core.storeHash path, T.pack "-", Core.storeName path])
            liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
            liftIO $ Posix.createSymbolicLink storePath targetPath

        -- Set up isolation if using chroot
        when (sandboxUseChroot config) $ do
            -- Setup basic directory structure
            liftIO $ createDirectoryIfMissing True (dir </> "etc")
            liftIO $ createDirectoryIfMissing True (dir </> "bin")
            liftIO $ createDirectoryIfMissing True (dir </> "tmp")
            liftIO $ createDirectoryIfMissing True (dir </> "dev")

            -- Create minimal /etc files needed for builds
            liftIO $ writeFile (dir </> "etc/passwd") "root:x:0:0::/:/bin/sh\nnobody:x:65534:65534:Nobody:/:/bin/false\n"
            liftIO $ writeFile (dir </> "etc/group") "root:x:0:\nnobody:x:65534:\n"

            -- Create /dev nodes if privileged
            when (sandboxPrivileged config) $ do
                -- Use system calls to create basic device nodes
                void $ liftIO $ runSystemCommand $ "mknod " ++ dir </> "dev/null" ++ " c 1 3"
                void $ liftIO $ runSystemCommand $ "mknod " ++ dir </> "dev/zero" ++ " c 1 5"
                void $ liftIO $ runSystemCommand $ "mknod " ++ dir </> "dev/urandom" ++ " c 1 9"

                -- Mount /proc if configured
                when (sandboxMountProc config) $
                    void $ liftIO $ runSystemCommand $ "mount -t proc proc " ++ dir </> "proc"

            -- Setup resources limits with cgroups if enabled
            when (sandboxUseCgroups config) $
                setupCgroups dir (sandboxResourceLimits config)

    -- Simplified sandbox for builder tier (can't use most isolation features)
    setupBuilderSandbox env inputs config dir = do
        -- Make store inputs available in the sandbox via symlinks
        forM_ (Set.toList inputs) $ \path -> do
            let storePath = Core.storePathToFilePath path env
            let targetPath = dir </> "inputs" </> T.unpack (Core.storeName path)
            liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
            liftIO $ Posix.createSymbolicLink storePath targetPath

        -- Create workspace directories
        liftIO $ createDirectoryIfMissing True (dir </> "work")
        liftIO $ createDirectoryIfMissing True (dir </> "outputs")

    -- Set up cgroups for resource limiting
    setupCgroups dir limits = do
        -- Create a cgroup for this build
        let cgroupName = "ten-build-" ++ takeFileName dir
        void $ liftIO $ runSystemCommand $ "cgcreate -g cpu,memory,pids:" ++ cgroupName

        -- Set resource limits
        forM_ (Map.toList limits) $ \(resource, value) ->
            case resource of
                "memory" ->
                    void $ liftIO $ runSystemCommand $ "cgset -r memory.limit_in_bytes=" ++ show (value * 1024) ++ " " ++ cgroupName
                "cpu-time" ->
                    void $ liftIO $ runSystemCommand $ "cgset -r cpu.cfs_quota_us=" ++ show (value * 1000) ++ " " ++ cgroupName
                "processes" ->
                    void $ liftIO $ runSystemCommand $ "cgset -r pids.max=" ++ show value ++ " " ++ cgroupName
                _ -> return ()

-- | Run a system command and return success/failure
runSystemCommand :: String -> IO Bool
runSystemCommand cmd = do
    (exitCode, _, _) <- readCreateProcessWithExitCode (proc "sh" ["-c", cmd]) ""
    return $ exitCode == ExitSuccess

-- | Clean up a sandbox directory safely
cleanupSandbox :: FilePath -> IO ()
cleanupSandbox dir = do
    -- Safety check: make sure we're not removing anything dangerous
    let sanitizedDir = normalise dir
    when (isValidSandboxDir sanitizedDir) $ do
        -- Recursively remove the sandbox directory and its contents
        removeDirectoryRecursive sanitizedDir `catch` \(_ :: SomeException) -> do
            -- If Haskell removal fails, use a safer system call
            let quotedPath = "'" ++ sanitizedDir ++ "'"
            void $ runSystemCommand $ "rm -rf " ++ quotedPath

-- | Check if this is a valid sandbox directory to delete
isValidSandboxDir :: FilePath -> Bool
isValidSandboxDir path =
    -- Make sure it's a non-empty path with "sandbox" in it
    not (null path) &&
    "sandbox" `List.isInfixOf` path &&
    -- Not trying to delete root directories
    not ("/" `List.isPrefixOf` path) &&
    not (".." `List.isInfixOf` path)

-- | Path where return-continuation derivations should be written
returnDerivationPath :: FilePath -> FilePath
returnDerivationPath sandboxDir = sandboxDir </> "result.drv"

-- | Create sandbox environment for builder
prepareSandboxEnvironment :: Core.BuildEnv -> Core.BuildState p -> FilePath -> Map Text Text -> Map Text Text
prepareSandboxEnvironment env state sandboxDir userEnv =
    let
        -- Basic environment variables
        baseEnv = Map.fromList [
            ("TEN_SANDBOX", T.pack sandboxDir),
            ("TEN_BUILD_ID", T.pack $ show $ Core.currentBuildId state),
            ("TEN_STORE", T.pack $ Core.storeLocation env)
            ]

        -- Add strategy information
        strategyEnv = case Core.buildStrategy env of
            Core.MonadicStrategy -> Map.singleton "TEN_STRATEGY" "monadic"
            Core.ApplicativeStrategy -> Map.singleton "TEN_STRATEGY" "applicative"

        -- Merge all environments, with user env taking precedence
        finalEnv = Map.unions [userEnv, strategyEnv, baseEnv]
    in
        finalEnv

-- | Compare two derivations for equality
derivationEquals :: Core.Derivation -> Core.Derivation -> Bool
derivationEquals d1 d2 = Core.derivHash d1 == Core.derivHash d2
