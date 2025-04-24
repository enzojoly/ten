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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}

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

    -- Type classes for operations based on privilege tier
    CanStoreDerivation,
    CanStoreBuildDerivation,
    CanRetrieveDerivation,
    CanCreateSandbox
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
import Data.Char (isHexDigit)
import Data.Singletons (SingI, fromSing)

import Ten.Core

-- | Type class for operations that can store derivations in the Eval phase
class CanStoreDerivation (t :: PrivilegeTier) where
    storeDerivationEval :: Derivation -> TenM 'Eval t StorePath

-- | Daemon instance for storing derivations
instance CanStoreDerivation 'Daemon where
    storeDerivationEval drv = do
        validateDerivation drv
        storeDerivationDaemon drv

-- | Builder instance for storing derivations
instance CanStoreDerivation 'Builder where
    storeDerivationEval drv = do
        validateDerivation drv
        storeDerivationBuilder drv

-- | Type class for operations that can store derivations in the Build phase
class CanStoreBuildDerivation (t :: PrivilegeTier) where
    storeDerivationBuild :: Derivation -> TenM 'Build t StorePath

-- | Daemon instance for storing build derivations
instance CanStoreBuildDerivation 'Daemon where
    storeDerivationBuild drv = do
        validateDerivation drv
        -- For build phase, we need to serialize and store directly
        let serialized = serializeDerivation drv
        let fileName = derivName drv <> ".drv"

        -- Store in content-addressed store
        env <- ask
        let storeDir = storeLocation env
        let hash = hashBlob serialized
        let storePath = StorePath hash fileName

        -- Write to store path
        let fullPath = storePathToFilePath storePath env
        liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)
        liftIO $ BS.writeFile fullPath serialized
        liftIO $ Posix.setFileMode fullPath 0o444  -- Read-only for all

        return storePath

-- | Builder instance for storing build derivations
instance CanStoreBuildDerivation 'Builder where
    storeDerivationBuild drv = do
        validateDerivation drv
        -- For builder, we need to send to daemon
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Serialize and prepare request
                let serialized = serializeDerivation drv
                let request = Request {
                    reqId = 0,
                    reqType = "store-derivation-build",
                    reqParams = Map.singleton "name" (derivName drv <> ".drv"),
                    reqPayload = Just serialized
                }

                -- Send request to daemon
                response <- liftIO $ sendRequestSync conn request 30000000  -- 30 second timeout

                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then case Map.lookup "path" (respData resp) of
                                Just pathText ->
                                    case parseStorePath pathText of
                                        Just path -> return path
                                        Nothing -> throwError $ StoreError "Invalid path in response"
                                Nothing -> throwError $ StoreError "Missing path in response"
                            else throwError $ StoreError $ respMessage resp

            _ -> throwError $ PrivilegeError "Cannot store derivation in build phase without daemon connection"

-- | Type class for operations that can retrieve derivations
class CanRetrieveDerivation p (t :: PrivilegeTier) where
    retrieveDerivation :: StorePath -> TenM p t (Maybe Derivation)

-- | Daemon instance for retrieving derivations
instance CanRetrieveDerivation p 'Daemon where
    retrieveDerivation path = do
        -- Validate the path before attempting to retrieve
        unless (validateStorePath path) $
            throwError $ StoreError $ "Invalid store path format: " <> storePathToText path

        -- Check if the path exists in the store
        exists <- storePathExists path
        if not exists
            then return Nothing
            else do
                -- Direct function call instead of using withSPhase
                env <- ask

                -- Read from store path directly
                let filePath = storePathToFilePath path env
                fileExists <- liftIO $ doesFileExist filePath

                if fileExists
                    then do
                        content <- liftIO $ BS.readFile filePath
                        case deserializeDerivation content of
                            Left _ -> return Nothing
                            Right drv -> return $ Just drv
                    else return Nothing

-- | Builder instance for retrieving derivations
instance CanRetrieveDerivation p 'Builder where
    retrieveDerivation path = do
        -- Validate the path before attempting to retrieve
        unless (validateStorePath path) $
            throwError $ StoreError $ "Invalid store path format: " <> storePathToText path

        -- Read from store
        env <- ask
        -- First try direct read if file is accessible
        let filePath = storePathToFilePath path env
        fileExists <- liftIO $ doesFileExist filePath

        if fileExists
            then do
                contentEither <- liftIO $ try $ BS.readFile filePath
                case contentEither of
                    Right content ->
                        case deserializeDerivation content of
                            Left _ -> return Nothing
                            Right drv -> return $ Just drv
                    Left (_ :: SomeException) ->
                        -- If direct read fails, use protocol
                        retrieveViaProtocol path
            else retrieveViaProtocol path
      where
        retrieveViaProtocol :: StorePath -> TenM p 'Builder (Maybe Derivation)
        retrieveViaProtocol p = do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create request
                    let request = Request {
                        reqId = 0,  -- Will be set by sendRequest
                        reqType = "get-derivation",
                        reqParams = Map.singleton "path" (storePathToText p),
                        reqPayload = Nothing
                    }

                    -- Send request and get response
                    response <- liftIO $ sendRequestSync conn request 30000000

                    case response of
                        Left _ -> return Nothing
                        Right resp ->
                            if respStatus resp == "ok"
                                then case respPayload resp of
                                    Just content ->
                                        case deserializeDerivation content of
                                            Left _ -> return Nothing
                                            Right drv -> return $ Just drv
                                    Nothing -> return Nothing
                                else return Nothing

                _ -> return Nothing  -- Not in client mode

-- | Convenience functions for phase-specific derivation retrieval
retrieveDerivationEval :: (CanRetrieveDerivation 'Eval t) => StorePath -> TenM 'Eval t (Maybe Derivation)
retrieveDerivationEval = retrieveDerivation

retrieveDerivationBuild :: (CanRetrieveDerivation 'Build t) => StorePath -> TenM 'Build t (Maybe Derivation)
retrieveDerivationBuild = retrieveDerivation

-- | Type class for operations that can create sandboxes
class CanCreateSandbox (t :: PrivilegeTier) where
    setupSandbox :: BuildEnv -> Set StorePath -> SandboxConfig -> FilePath -> TenM 'Build t ()

-- | Daemon instance for creating sandboxes
instance CanCreateSandbox 'Daemon where
    setupSandbox e i c dir = do
        -- Make store inputs available in the sandbox
        forM_ (Set.toList i) $ \path -> do
            -- Create symlinks to store paths
            let storePath = storePathToFilePath path e
            let targetPath = dir </> "store" </> T.unpack (T.concat [storeHash path, T.pack "-", storeName path])
            liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
            liftIO $ Posix.createSymbolicLink storePath targetPath

        -- Set up isolation if using chroot
        when (sandboxUseChroot c) $ do
            -- Setup basic directory structure
            liftIO $ createDirectoryIfMissing True (dir </> "etc")
            liftIO $ createDirectoryIfMissing True (dir </> "bin")
            liftIO $ createDirectoryIfMissing True (dir </> "tmp")
            liftIO $ createDirectoryIfMissing True (dir </> "dev")

            -- Create minimal /etc files needed for builds
            liftIO $ writeFile (dir </> "etc/passwd") "root:x:0:0::/:/bin/sh\nnobody:x:65534:65534:Nobody:/:/bin/false\n"
            liftIO $ writeFile (dir </> "etc/group") "root:x:0:\nnobody:x:65534:\n"

            -- Create /dev nodes if privileged
            when (sandboxPrivileged c) $ do
                -- Use system calls to create basic device nodes
                void $ liftIO $ runSystemCommand $ "mknod " ++ dir </> "dev/null" ++ " c 1 3"
                void $ liftIO $ runSystemCommand $ "mknod " ++ dir </> "dev/zero" ++ " c 1 5"
                void $ liftIO $ runSystemCommand $ "mknod " ++ dir </> "dev/urandom" ++ " c 1 9"

                -- Mount /proc if configured
                when (sandboxMountProc c) $
                    void $ liftIO $ runSystemCommand $ "mount -t proc proc " ++ dir </> "proc"

            -- Setup resources limits with cgroups if enabled
            when (sandboxUseCgroups c) $
                setupCgroups dir (sandboxResourceLimits c)

-- | Builder instance for creating sandboxes
instance CanCreateSandbox 'Builder where
    setupSandbox e i c dir = do
        -- Make store inputs available in the sandbox via symlinks
        forM_ (Set.toList i) $ \path -> do
            let storePath = storePathToFilePath path e
            let targetPath = dir </> "inputs" </> T.unpack (storeName path)
            liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
            liftIO $ Posix.createSymbolicLink storePath targetPath

        -- Create workspace directories
        liftIO $ createDirectoryIfMissing True (dir </> "work")
        liftIO $ createDirectoryIfMissing True (dir </> "outputs")

-- | Represents a chain of recursive derivations
data DerivationChain = DerivationChain [Text] -- Chain of derivation hashes
    deriving (Show, Eq)

-- | Create a new derivation chain
newDerivationChain :: Derivation -> DerivationChain
newDerivationChain drv = DerivationChain [derivHash drv]

-- | Get chain length
derivationChainLength :: DerivationChain -> Int
derivationChainLength (DerivationChain hashes) = length hashes

-- | Hash a derivation to get its textual representation
hashDerivation :: Derivation -> Text
hashDerivation drv = hashBlob (serializeDerivation drv)

-- | Create a new derivation - works in Eval phase only
mkDerivation :: Text -> StorePath -> [Text] -> Set DerivationInput
             -> Set Text -> Map Text Text -> Text -> TenM 'Eval t Derivation
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

-- | Hash a ByteString blob and return as Text
hashBlob :: BS.ByteString -> Text
hashBlob bs =
    let digest = hash bs :: Digest SHA256
        hexBytes = convertToBase Base16 digest
    in TE.decodeUtf8 hexBytes

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

-- | Helper to read via daemon protocol - Builder specific version
readViaProtocolBuilder :: StorePath -> TenM p 'Builder ByteString
readViaProtocolBuilder path = do
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create request for derivation content
            let request = Request {
                reqId = 0,
                reqType = "store-read",
                reqParams = Map.singleton "path" (storePathToText path),
                reqPayload = Nothing
            }

            -- Send request and wait for response
            response <- liftIO $ sendRequestSync conn request 30000000  -- 30 second timeout

            case response of
                Left err -> throwError err
                Right resp ->
                    if respStatus resp == "ok"
                        then case respPayload resp of
                            Just content -> return content
                            Nothing -> throwError $ StoreError "Missing content in response"
                        else throwError $ StoreError $ respMessage resp

        _ -> throwError $ PrivilegeError "Cannot read via protocol without daemon connection"

-- | Instantiate a derivation for building - context-aware implementation
instantiateDerivation :: (CanStoreBuildDerivation t) => Derivation -> TenM 'Build t ()
instantiateDerivation deriv = do
    -- Store the derivation using the phase-specific implementation
    derivPath <- storeDerivationBuild deriv

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

    -- Check for cycles in build chain using isInDerivationChain from Ten.Core
    isCyclic <- isInDerivationChain deriv
    when isCyclic $
        throwError $ CyclicDependency $ "Cyclic derivation detected: " <> derivName deriv

    -- Add to build chain
    modify $ \s -> s { buildChain = deriv : buildChain s }

    -- Add input proof
    addProof InputProof

    logMsg 1 $ "Instantiated derivation: " <> derivName deriv

-- | Add a derivation to the chain
addToDerivationChain :: Derivation -> DerivationChain -> DerivationChain
addToDerivationChain drv (DerivationChain hashes) =
    DerivationChain (derivHash drv : hashes)

-- | The monadic join operation for Return-Continuation - context-aware implementation
joinDerivation :: (CanStoreBuildDerivation t, CanCreateSandbox t) => Derivation -> TenM 'Build t Derivation
joinDerivation outerDrv = do
    -- Build the outer derivation just enough to get inner derivation
    innerDrv <- buildToGetInnerDerivation outerDrv

    -- Record that we performed a join operation
    addProof ReturnProof

    -- Return the inner derivation to be built
    return innerDrv

-- | Build a derivation to the point where it returns an inner derivation
buildToGetInnerDerivation :: (CanStoreBuildDerivation t, CanCreateSandbox t) => Derivation -> TenM 'Build t Derivation
buildToGetInnerDerivation drv = do
    env <- ask
    state <- get

    -- Create a sandbox configuration with return-continuation support
    let sandboxConfig = defaultSandboxConfig {
            sandboxReturnSupport = True,
            sandboxPrivileged = currentPrivilegeTier env == Daemon
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
        let contextEnv = case currentPrivilegeTier env of
                            Daemon -> Map.singleton "TEN_DAEMON" "1"
                            Builder -> Map.singleton "TEN_BUILDER" "1"

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
                        "Failed to deserialize returned derivation: " <> T.pack (show err)
                    Right innerDrv -> do
                        -- Add proof that we successfully got a returned derivation
                        addProof RecursionProof

                        -- Store the inner derivation - context-aware implementation
                        void $ storeDerivationBuild innerDrv

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
serializeDerivation :: Derivation -> ByteString
serializeDerivation = LBS.toStrict . Aeson.encode . derivationToJSON

-- | Convert derivation to JSON
derivationToJSON :: Derivation -> Aeson.Value
derivationToJSON drv = Aeson.object
    [ "name" .= derivName drv
    , "hash" .= derivHash drv
    , "builder" .= storePathToJSON (derivBuilder drv)
    , "args" .= derivArgs drv
    , "inputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList $ derivInputs drv)
    , "outputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList $ derivOutputs drv)
    , "env" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList $ derivEnv drv)
    , "system" .= derivSystem drv
    , "strategy" .= (case derivStrategy drv of
                        ApplicativeStrategy -> "applicative" :: Text
                        MonadicStrategy -> "monadic")
    , "meta" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList $ derivMeta drv)
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
deserializeDerivation :: ByteString -> Either BuildError Derivation
deserializeDerivation bs =
    case Aeson.eitherDecode (LBS.fromStrict bs) of
        Left err -> Left $ SerializationError $ "JSON parse error: " <> T.pack err
        Right json -> case derivationFromJSON json of
            Left err -> Left $ SerializationError err
            Right drv -> Right drv

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
        return $ Map.fromList $ catMaybes $ map convertKeyValue $ Aeson.KeyMap.toList obj
      where
        convertKeyValue (k, Aeson.String v) = Just (Aeson.toText k, v)
        convertKeyValue _ = Nothing  -- Ignore non-string values

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
validateDerivation :: Derivation -> TenM p t ()
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
withSandbox :: (CanCreateSandbox t) => Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build t a) -> TenM 'Build t a
withSandbox inputs config action = do
    env <- ask

    -- Determine the current build ID
    buildId <- gets currentBuildId

    -- Create a temporary directory for the sandbox
    let sandboxDir = workDir env </> "sandbox" </> "temp-" ++ show buildId
    liftIO $ createDirectoryIfMissing True sandboxDir

    -- Set up the sandbox with appropriate privilege tier using the type class
    setupSandbox env inputs config sandboxDir

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

-- | Set up cgroups for resource limiting
setupCgroups :: FilePath -> Map Text Int -> TenM 'Build 'Daemon ()
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
prepareSandboxEnvironment :: BuildEnv -> BuildState p -> FilePath -> Map Text Text -> Map Text Text
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

-- | Compare two derivations for equality
derivationEquals :: Derivation -> Derivation -> Bool
derivationEquals d1 d2 = derivHash d1 == derivHash d2
