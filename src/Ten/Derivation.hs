{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Ten.Derivation (
    -- Core types
    Derivation(..),
    DerivationInput(..),
    DerivationOutput(..),

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

    -- Context-aware derivation storage
    storeDerivation,
    retrieveDerivation,

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
    DerivationChain,
    newDerivationChain,
    addToDerivationChain,
    isInDerivationChain,
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Vector as Vector
import System.FilePath
import System.Directory (doesFileExist)
import qualified System.Posix.Files as Posix
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode, StdStream(NoStream, CreatePipe))
import System.Exit
import Control.Exception (try, SomeException)

import Ten.Core
import Ten.Store

-- | Represents a chain of recursive derivations
data DerivationChain = DerivationChain [Text] -- Chain of derivation hashes
    deriving (Show, Eq)

-- | Create a new derivation chain
newDerivationChain :: Derivation -> DerivationChain
newDerivationChain drv = DerivationChain [derivHash drv]

-- | Get chain length
derivationChainLength :: DerivationChain -> Int
derivationChainLength (DerivationChain hashes) = length hashes

-- | Detect recursion cycles in derivation chains
detectRecursionCycle :: [Derivation] -> Bool
detectRecursionCycle derivations =
    length (List.nubBy derivationEquals derivations) < length derivations

-- | Hash a derivation to get its textual representation
hashDerivation :: Derivation -> Text
hashDerivation drv = hashByteString (serializeDerivation drv)

-- | Create a new derivation - context-neutral implementation
mkDerivation :: Text -> StorePath -> [Text] -> Set DerivationInput
             -> Set Text -> Map Text Text -> Text -> TenM 'Eval ctx Derivation
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

    let derivHash' = hashByteString (TE.encodeUtf8 hashBase)

    -- Create output specifications with predicted paths
    let outputs = Set.map (\outName -> DerivationOutput
                              { outputName = outName
                              , outputPath = StorePath derivHash' outName
                              }) outputNames

    -- Analyze what build strategy this derivation should use
    let strategy = analyzeDerivationStrategy name args env

    -- Add metadata if it's a return-continuation derivation
    let meta = if strategy == MonadicStrategy && isReturnContinuationDerivation name args env
                  then Map.singleton "returnContinuation" "true"
                  else Map.empty

    -- Record proof that this is a valid derivation
    addProof TypeProof

    return Derivation
        { derivName = name
        , derivHash = derivHash'
        , derivBuilder = builder
        , derivArgs = args
        , derivInputs = inputs
        , derivOutputs = outputs
        , derivEnv = env
        , derivSystem = system
        , derivStrategy = strategy
        , derivMeta = meta
        }

-- | Analyze a derivation to determine optimal build strategy
analyzeDerivationStrategy :: Text -> [Text] -> Map Text Text -> BuildStrategy
analyzeDerivationStrategy name args env =
    -- Check various indicators that this derivation uses return-continuation
    if isReturnContinuationDerivation name args env
        then MonadicStrategy
        else
            -- Check for other indicators that require dynamic dependency resolution
            if any isDynamicDependencyArg args || Map.member "TEN_DYNAMIC_DEPS" env
                then MonadicStrategy
                else ApplicativeStrategy
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

-- | Store a derivation in the store - context-aware implementation
storeDerivation :: Derivation -> TenM p ctx StorePath
storeDerivation drv = do
    -- Validate the derivation before storing
    validateDerivation drv

    -- Serialize the derivation to a ByteString
    let serialized = serializeDerivation drv
    let fileName = derivName drv <> ".drv"

    -- Check the privilege context and use appropriate method
    ctx <- asks privilegeContext
    case ctx of
        Privileged ->
            -- Direct store in privileged context
            addToStore fileName serialized

        Unprivileged -> do
            -- Use protocol to request storage in unprivileged context
            let request = StoreDerivationRequest serialized
            response <- sendStoreDaemonRequest request
            case response of
                StoreDerivationResponse path -> return path
                StoreErrorResponse err -> throwError $ StoreError err
                _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Retrieve a derivation from the store - context-aware implementation
retrieveDerivation :: StorePath -> TenM p ctx (Maybe Derivation)
retrieveDerivation path = do
    -- Validate the path before attempting to retrieve
    unless (validateStorePath path) $
        throwError $ StoreError $ "Invalid store path format: " <> storePathToText path

    -- Check if the path exists in the store
    exists <- storePathExists path
    if not exists
        then return Nothing
        else do
            -- Check privilege context and use appropriate method
            ctx <- asks privilegeContext
            content <- case ctx of
                Privileged ->
                    -- Direct read in privileged context
                    readFromStore path

                Unprivileged ->
                    -- First try direct read (works if file is accessible)
                    -- If that fails, use protocol request
                    do
                        env <- ask
                        let filePath = storePathToFilePath path env
                        fileExists <- liftIO $ doesFileExist filePath
                        if fileExists
                            then liftIO $ BS.readFile filePath `catch` \(_ :: SomeException) -> do
                                -- If direct read fails due to permissions, use protocol
                                let request = StoreReadRequest path
                                response <- sendStoreDaemonRequest request
                                case response of
                                    StoreReadResponse content -> return content
                                    _ -> throwError $ StoreError $ "Failed to read via protocol: " <> storePathToText path
                            else do
                                -- File doesn't exist or isn't accessible, use protocol
                                let request = StoreReadRequest path
                                response <- sendStoreDaemonRequest request
                                case response of
                                    StoreReadResponse content -> return content
                                    _ -> throwError $ StoreError $ "Failed to read via protocol: " <> storePathToText path

            -- Deserialize the content
            case deserializeDerivation content of
                Left err -> do
                    logMsg 1 $ "Error deserializing derivation: " <> err
                    return Nothing
                Right drv -> return $ Just drv

-- | Instantiate a derivation for building - context-aware implementation
instantiateDerivation :: Derivation -> TenM 'Build ctx ()
instantiateDerivation deriv = do
    -- Store the derivation using the context-aware implementation
    derivPath <- storeDerivation deriv

    -- Verify inputs exist
    forM_ (derivInputs deriv) $ \input -> do
        exists <- storePathExists (inputPath input)
        unless exists $ throwError $
            BuildFailed $ "Input does not exist: " <> T.pack (show $ inputPath input)

    -- Record inputs in build state
    let inputPaths = Set.map inputPath (derivInputs deriv)
    modify $ \s -> s { buildInputs = Set.union inputPaths (buildInputs s) }

    -- Verify builder exists
    builderExists <- storePathExists (derivBuilder deriv)
    unless builderExists $ throwError $
        BuildFailed $ "Builder does not exist: " <> T.pack (show $ derivBuilder deriv)

    -- Track this derivation in the build chain for recursion detection
    chain <- gets buildChain
    limit <- asks maxRecursionDepth
    when (length chain >= limit) $
        throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show limit)

    -- Check for cycles in build chain
    let isCyclic = any (derivationEquals deriv) chain
    when isCyclic $
        throwError $ CyclicDependency $ "Cyclic derivation detected: " <> derivName deriv

    -- Add to build chain
    addToDerivationChain deriv

    -- Add input proof
    addProof InputProof

    logMsg 1 $ "Instantiated derivation: " <> derivName deriv

-- | The monadic join operation for Return-Continuation - context-aware implementation
joinDerivation :: Derivation -> TenM 'Build ctx Derivation
joinDerivation outerDrv = do
    -- Build the outer derivation just enough to get inner derivation
    innerDrv <- buildToGetInnerDerivation outerDrv

    -- Record that we performed a join operation
    addProof ReturnProof

    -- Return the inner derivation to be built
    return innerDrv

-- | Build a derivation to the point where it returns an inner derivation
buildToGetInnerDerivation :: Derivation -> TenM 'Build ctx Derivation
buildToGetInnerDerivation drv = do
    env <- ask
    state <- get

    -- Create a sandbox configuration with return-continuation support
    let sandboxConfig = defaultSandboxConfig {
            sandboxReturnSupport = True,
            sandboxPrivileged = privilegeContext env == Privileged
        }

    -- Use withSandbox which handles both contexts correctly
    withSandbox (Set.map inputPath $ derivInputs drv) sandboxConfig $ \sandboxDir -> do
        -- Get the builder from the store - context-aware implementation
        builderContent <- readFromStore (derivBuilder drv)

        -- Write the builder to the sandbox
        let builderPath = sandboxDir </> "builder"
        liftIO $ BS.writeFile builderPath builderContent
        liftIO $ Posix.setFileMode builderPath 0o755

        -- Prepare environment variables
        let buildEnv = prepareSandboxEnvironment env state sandboxDir (derivEnv drv)

        -- Add context-specific environment variables
        let contextEnv = if privilegeContext env == Privileged
                         then Map.singleton "TEN_PRIVILEGED" "1"
                         else Map.singleton "TEN_UNPRIVILEGED" "1"

        let fullEnv = Map.union buildEnv contextEnv

        -- Configure the process
        let processConfig = sandboxedProcessConfig
                sandboxDir
                builderPath
                (map T.unpack $ derivArgs drv)
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
                    Left err -> throwError $ SerializationError $
                        "Failed to deserialize returned derivation: " <> err
                    Right innerDrv -> do
                        -- Add proof that we successfully got a returned derivation
                        addProof RecursionProof

                        -- Store the inner derivation - context-aware implementation
                        void $ storeDerivation innerDrv

                        return innerDrv
            else
                throwError $ BuildFailed $
                    "Builder did not return a derivation at: " <> T.pack returnPath <>
                    "\nExit code: " <> T.pack (show exitCode) <>
                    "\nStdout: " <> T.pack stdout <>
                    "\nStderr: " <> T.pack stderr

-- | Get output paths from a derivation
derivationOutputPaths :: Derivation -> Set StorePath
derivationOutputPaths = Set.map outputPath . derivOutputs

-- | Get input paths from a derivation
derivationInputPaths :: Derivation -> Set StorePath
derivationInputPaths = Set.map inputPath . derivInputs

-- | Serialize a derivation to a ByteString
serializeDerivation :: Derivation -> BS.ByteString
serializeDerivation = LBS.toStrict . Aeson.encode . derivationToJSON

-- | Convert derivation to JSON
derivationToJSON :: Derivation -> Aeson.Value
derivationToJSON Derivation{..} = Aeson.object
    [ "name" .= derivName
    , "hash" .= derivHash
    , "builder" .= storePathToJSON derivBuilder
    , "args" .= derivArgs
    , "inputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList derivInputs)
    , "outputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList derivOutputs)
    , "env" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList derivEnv)
    , "system" .= derivSystem
    , "strategy" .= (case derivStrategy of
                        ApplicativeStrategy -> "applicative" :: Text
                        MonadicStrategy -> "monadic")
    , "meta" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList derivMeta)
    ]

-- Helper to convert StorePath to JSON
storePathToJSON :: StorePath -> Aeson.Value
storePathToJSON (StorePath hash name) = Aeson.object
    [ "hash" .= hash
    , "name" .= name
    ]

-- Make DerivationInput JSON serializable
instance Aeson.ToJSON DerivationInput where
    toJSON (DerivationInput path name) = Aeson.object
        [ "path" .= storePathToJSON path
        , "name" .= name
        ]

-- Make DerivationOutput JSON serializable
instance Aeson.ToJSON DerivationOutput where
    toJSON (DerivationOutput name path) = Aeson.object
        [ "name" .= name
        , "path" .= storePathToJSON path
        ]

-- | Deserialize a derivation from a ByteString
deserializeDerivation :: BS.ByteString -> Either Text Derivation
deserializeDerivation bs =
    case Aeson.eitherDecode (LBS.fromStrict bs) of
        Left err -> Left $ "JSON parse error: " <> T.pack err
        Right json -> derivationFromJSON json

-- | Convert JSON to Derivation
derivationFromJSON :: Aeson.Value -> Either Text Derivation
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
                "monadic" -> MonadicStrategy
                _ -> ApplicativeStrategy

        return Derivation
            { derivName = name
            , derivHash = hash
            , derivBuilder = builder
            , derivArgs = args
            , derivInputs = Set.fromList inputs
            , derivOutputs = Set.fromList outputs
            , derivEnv = env
            , derivSystem = system
            , derivStrategy = strategy
            , derivMeta = meta
            }

    parseStorePath :: Aeson.Value -> Aeson.Parser StorePath
    parseStorePath = Aeson.withObject "StorePath" $ \o -> do
        hash <- o .: "hash"
        name <- o .: "name"
        return $ StorePath hash name

    parseInputs :: Aeson.Value -> Aeson.Parser [DerivationInput]
    parseInputs = Aeson.withArray "Inputs" $ \arr ->
        mapM parseInput (Vector.toList arr)

    parseInput :: Aeson.Value -> Aeson.Parser DerivationInput
    parseInput = Aeson.withObject "DerivationInput" $ \o -> do
        pathObj <- o .: "path"
        name <- o .: "name"
        path <- parseStorePath pathObj
        return $ DerivationInput path name

    parseOutputs :: Aeson.Value -> Aeson.Parser [DerivationOutput]
    parseOutputs = Aeson.withArray "Outputs" $ \arr ->
        mapM parseOutput (Vector.toList arr)

    parseOutput :: Aeson.Value -> Aeson.Parser DerivationOutput
    parseOutput = Aeson.withObject "DerivationOutput" $ \o -> do
        name <- o .: "name"
        pathObj <- o .: "path"
        path <- parseStorePath pathObj
        return $ DerivationOutput name path

    parseEnvMap :: Aeson.Value -> Aeson.Parser (Map Text Text)
    parseEnvMap = Aeson.withObject "Environment" $ \obj ->
        return $ Map.fromList $ map convertKeyValue $ Aeson.KeyMap.toList obj
      where
        convertKeyValue (k, Aeson.String v) = (Aeson.toText k, v)
        convertKeyValue _ = ("", "") -- Fallback for non-string values

-- | Hash a derivation's inputs for dependency tracking
hashDerivationInputs :: Derivation -> Text
hashDerivationInputs drv =
    T.pack $ show $ Aeson.encode $
        map (\input -> storeHash $ inputPath input) $
        Set.toList $ derivInputs drv

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
validateDerivation :: Derivation -> TenM p ctx ()
validateDerivation drv = do
    -- Validate the hash
    unless (T.length (derivHash drv) >= 8 && T.all isHexDigit (derivHash drv)) $
        throwError $ SerializationError $ "Invalid derivation hash: " <> derivHash drv

    -- Validate the builder path
    unless (validateStorePath (derivBuilder drv)) $
        throwError $ SerializationError $ "Invalid builder path: " <> T.pack (show (derivBuilder drv))

    -- Validate all input paths
    forM_ (derivInputs drv) $ \input ->
        unless (validateStorePath (inputPath input)) $
            throwError $ SerializationError $ "Invalid input path: " <> T.pack (show (inputPath input))

    -- Validate all output paths
    forM_ (derivOutputs drv) $ \output ->
        unless (validateStorePath (outputPath output)) $
            throwError $ SerializationError $ "Invalid output path: " <> T.pack (show (outputPath output))

    -- Validate that name isn't empty
    when (T.null (derivName drv)) $
        throwError $ SerializationError "Derivation name cannot be empty"
  where
    isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Check if a derivation is in the build chain
isInDerivationChain :: Derivation -> DerivationChain -> Bool
isInDerivationChain drv (DerivationChain hashes) =
    derivHash drv `elem` hashes

-- | Add a derivation to the chain
addToDerivationChain :: Derivation -> DerivationChain -> DerivationChain
addToDerivationChain drv (DerivationChain hashes) =
    DerivationChain (derivHash drv : hashes)

-- | Type for sandbox configuration
data SandboxConfig = SandboxConfig {
    sandboxReturnSupport :: Bool,
    sandboxPrivileged :: Bool
}

-- | Default sandbox configuration
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig {
    sandboxReturnSupport = False,
    sandboxPrivileged = False
}

-- | Create sandbox environment for builder
prepareSandboxEnvironment :: BuildEnv -> BuildState -> FilePath -> Map Text Text -> Map Text Text
prepareSandboxEnvironment env state sandboxDir userEnv =
    let
        -- Basic environment variables
        baseEnv = Map.fromList [
            ("TEN_SANDBOX", T.pack sandboxDir),
            ("TEN_BUILD_ID", T.pack $ show $ currentBuildId state),
            ("TEN_STORE", T.pack $ storeLocation env)
            ]

        -- Add strategy information
        strategyEnv = case buildStrategy env of
            MonadicStrategy -> Map.singleton "TEN_STRATEGY" "monadic"
            ApplicativeStrategy -> Map.singleton "TEN_STRATEGY" "applicative"

        -- Merge all environments, with user env taking precedence
        finalEnv = Map.unions [userEnv, strategyEnv, baseEnv]
    in
        finalEnv

-- | Path where return-continuation derivations should be written
returnDerivationPath :: FilePath -> FilePath
returnDerivationPath sandboxDir = sandboxDir </> "result.drv"

-- | Execute an operation in a sandbox
withSandbox :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build ctx a) -> TenM 'Build ctx a
withSandbox inputs config action = do
    env <- ask

    -- Create a temporary directory for the sandbox
    let tmpDir = workDir env </> "sandbox"
    liftIO $ createDirectoryIfMissing True tmpDir

    -- Create a unique sandbox directory
    sandboxDir <- liftIO $ do
        -- In a real implementation, we'd use proper unique directory creation
        uniqueId <- show <$> getCurrentTime
        let dir = tmpDir </> ("sandbox-" ++ uniqueId)
        createDirectory dir
        return dir

    -- Set up the sandbox
    setupSandbox sandboxDir inputs

    -- Run the action
    result <- action sandboxDir `catchError` \err -> do
        -- Clean up on error
        liftIO $ removeDirectoryRecursive sandboxDir
        throwError err

    -- Clean up the sandbox
    liftIO $ removeDirectoryRecursive sandboxDir

    -- Return the result
    return result

-- | Set up a sandbox with input store paths
setupSandbox :: FilePath -> Set StorePath -> TenM 'Build ctx ()
setupSandbox sandboxDir inputs = do
    env <- ask

    -- Create necessary directories
    liftIO $ do
        -- Create input directory
        createDirectoryIfMissing True (sandboxDir </> "inputs")

        -- Create output directory
        createDirectoryIfMissing True (sandboxDir </> "outputs")

        -- Create tmp directory
        createDirectoryIfMissing True (sandboxDir </> "tmp")

    -- Symlink or copy input paths
    forM_ (Set.toList inputs) $ \input -> do
        let srcPath = storePathToFilePath input env
        let destPath = sandboxDir </> "inputs" </> T.unpack (storeName input)

        -- Create symlink/copy based on privilege level
        ctx <- asks privilegeContext
        case ctx of
            Privileged ->
                -- Can create symlinks in privileged mode
                liftIO $ createFileLink srcPath destPath

            Unprivileged -> do
                -- Copy in unprivileged mode
                content <- readFromStore input
                liftIO $ BS.writeFile destPath content

-- Helper for file linking
createFileLink :: FilePath -> FilePath -> IO ()
createFileLink src dest = do
    -- Try to create a symlink first (more efficient)
    result <- try $ createSymbolicLink src dest
    case result of
        Right () -> return ()
        Left (_ :: SomeException) -> do
            -- Fall back to file copy if symlink fails
            content <- BS.readFile src
            BS.writeFile dest content
