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
import Data.Kind (Type)
import Crypto.Hash (Digest, SHA256(..), hash)
import qualified Crypto.Hash as Crypto
import Network.Socket (Socket)
import qualified Network.Socket as Network
import Network.Socket.ByteString (sendAll, recv)
import System.IO (Handle, hPutStrLn, stderr)

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

-- | Hash a derivation to get its textual representation
hashDerivation :: Derivation -> Text
hashDerivation drv = hashByteString (serializeDerivation drv)

-- | Create a new derivation - works in any phase/tier
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
    in T.pack $ show digest

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
storeDerivation :: Derivation -> TenM p t StorePath
storeDerivation drv = do
    -- Validate the derivation before storing
    validateDerivation drv

    -- Serialize the derivation to a ByteString
    let serialized = serializeDerivation drv
    let fileName = derivName drv <> ".drv"

    -- Check the privilege tier and use appropriate method
    env <- ask
    case currentPrivilegeTier env of
        Daemon ->
            -- Direct store in daemon context
            withSPrivilegeTier sDaemon $ \_ ->
                addToStore fileName serialized

        Builder -> do
            -- Use protocol to request storage in builder context
            let request = StoreDerivationRequest serialized
            response <- withSPrivilegeTier sBuilder $ \_ ->
                sendStoreDaemonRequest request
            case response of
                StoreDerivationResponse path -> return path
                StoreErrorResponse err -> throwError $ StoreError err
                _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Retrieve a derivation from the store - context-aware implementation
retrieveDerivation :: StorePath -> TenM p t (Maybe Derivation)
retrieveDerivation path = do
    -- Validate the path before attempting to retrieve
    unless (validateStorePath path) $
        throwError $ StoreError $ "Invalid store path format: " <> storePathToText path

    -- Check if the path exists in the store
    exists <- storePathExists path
    if not exists
        then return Nothing
        else do
            -- Get content based on privilege tier
            env <- ask
            content <- case currentPrivilegeTier env of
                Daemon ->
                    -- Direct read in daemon context
                    withSPrivilegeTier sDaemon $ \_ ->
                        readFromStore path

                Builder ->
                    -- First try direct read (works if file is accessible)
                    -- If that fails, use protocol request
                    do
                        let filePath = storePathToFilePath path env
                        fileExists <- liftIO $ doesFileExist filePath
                        if fileExists
                            then liftIO $ BS.readFile filePath `catch` \(_ :: SomeException) -> do
                                -- If direct read fails due to permissions, use protocol
                                withSPrivilegeTier sBuilder $ \_ -> do
                                    let request = StoreReadRequest path
                                    response <- sendStoreDaemonRequest request
                                    case response of
                                        StoreReadResponse content -> return content
                                        _ -> throwError $ StoreError $ "Failed to read via protocol: " <> storePathToText path
                            else do
                                -- File doesn't exist or isn't accessible, use protocol
                                withSPrivilegeTier sBuilder $ \_ -> do
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
instantiateDerivation :: Derivation -> TenM 'Build t ()
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
    modify $ \s -> s { buildChain = deriv : buildChain s }

    -- Add input proof
    addProof InputProof

    logMsg 1 $ "Instantiated derivation: " <> derivName deriv

-- | Add a derivation to the chain
addToDerivationChain :: Derivation -> DerivationChain -> DerivationChain
addToDerivationChain drv (DerivationChain hashes) =
    DerivationChain (derivHash drv : hashes)

-- | Check if a derivation is in the build chain
isInDerivationChain :: Derivation -> DerivationChain -> Bool
isInDerivationChain drv (DerivationChain hashes) =
    derivHash drv `elem` hashes

-- | The monadic join operation for Return-Continuation - context-aware implementation
joinDerivation :: Derivation -> TenM 'Build t Derivation
joinDerivation outerDrv = do
    -- Build the outer derivation just enough to get inner derivation
    innerDrv <- buildToGetInnerDerivation outerDrv

    -- Record that we performed a join operation
    addProof ReturnProof

    -- Return the inner derivation to be built
    return innerDrv

-- | Build a derivation to the point where it returns an inner derivation
buildToGetInnerDerivation :: Derivation -> TenM 'Build t Derivation
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
  where
    isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Send a request to the daemon through the store layer
sendStoreDaemonRequest :: StoreRequest -> TenM p 'Builder StoreResponse
sendStoreDaemonRequest request = withDaemon $ \conn -> do
    -- Create proper protocol request from the Store request type
    let (requestType, params, payload) = storeRequestToProtocol request

    -- Create a proper protocol Request object
    let protocolRequest = Request {
        reqId = 0,  -- Will be set by sendRequest
        reqType = requestType,
        reqParams = params,
        reqPayload = payload
    }

    -- Send request and wait for response
    response <- sendRequestSync conn protocolRequest 30000000  -- 30 second timeout

    case response of
        Left err -> return $ Left $ storeErrorFromProtocol err
        Right resp -> return $ Right $ protocolToStoreResponse resp
  where
    -- Convert store request to protocol parameters
    storeRequestToProtocol :: StoreRequest -> (Text, Map Text Text, Maybe BS.ByteString)
    storeRequestToProtocol (StoreDerivationRequest content) =
        ("store-derivation", Map.empty, Just content)
    storeRequestToProtocol (StoreReadRequest path) =
        ("store-read", Map.singleton "path" (storePathToText path), Nothing)
    storeRequestToProtocol (StoreVerifyRequest path) =
        ("store-verify", Map.singleton "path" (storePathToText path), Nothing)
    storeRequestToProtocol (StoreAddRequest name content) =
        ("store-add", Map.singleton "name" name, Just content)
    storeRequestToProtocol (StoreListRequest) =
        ("store-list", Map.empty, Nothing)
    storeRequestToProtocol (StoreReferenceRequest path) =
        ("store-reference", Map.singleton "path" (storePathToText path), Nothing)
    storeRequestToProtocol (StoreGCRequest force) =
        ("store-gc", Map.singleton "force" (if force then "true" else "false"), Nothing)

    -- Convert protocol error to store error
    storeErrorFromProtocol :: BuildError -> StoreResponse
    storeErrorFromProtocol (StoreError msg) = StoreErrorResponse msg
    storeErrorFromProtocol (DaemonError msg) = StoreErrorResponse $ "Daemon error: " <> msg
    storeErrorFromProtocol (AuthError msg) = StoreErrorResponse $ "Authentication error: " <> msg
    storeErrorFromProtocol err = StoreErrorResponse $ T.pack $ show err

    -- Convert protocol response to store response
    protocolToStoreResponse :: Response -> StoreResponse
    protocolToStoreResponse resp =
        if respStatus resp == "ok"
            then
                case respType resp of
                    "store-derivation" -> case Map.lookup "path" (respData resp) of
                        Just pathText -> case parseStorePath pathText of
                            Just path -> StoreDerivationResponse path
                            Nothing -> StoreErrorResponse $ "Invalid store path: " <> pathText
                        Nothing -> StoreErrorResponse "Missing path in response"

                    "store-read" -> case respPayload resp of
                        Just content -> StoreReadResponse content
                        Nothing -> StoreErrorResponse "Missing content in response"

                    "store-verify" -> case Map.lookup "exists" (respData resp) of
                        Just "true" -> StoreVerifyResponse True
                        _ -> StoreVerifyResponse False

                    "store-add" -> case Map.lookup "path" (respData resp) of
                        Just pathText -> case parseStorePath pathText of
                            Just path -> StoreAddResponse path
                            Nothing -> StoreErrorResponse $ "Invalid store path: " <> pathText
                        Nothing -> StoreErrorResponse "Missing path in response"

                    "store-list" -> case Map.lookup "paths" (respData resp) of
                        Just pathsText ->
                            let pathsList = T.splitOn "," pathsText
                                paths = catMaybes $ map parseStorePath pathsList
                            in StoreListResponse paths
                        Nothing -> StoreListResponse []

                    "store-reference" -> case Map.lookup "refs" (respData resp) of
                        Just refsText ->
                            let refsList = T.splitOn "," refsText
                                paths = catMaybes $ map parseStorePath refsList
                            in StoreReferenceResponse (Set.fromList paths)
                        Nothing -> StoreReferenceResponse Set.empty

                    "store-gc" ->
                        let collected = maybe 0 (read . T.unpack) $ Map.lookup "collected" (respData resp)
                            remaining = maybe 0 (read . T.unpack) $ Map.lookup "remaining" (respData resp)
                            bytes = maybe 0 (read . T.unpack) $ Map.lookup "bytes" (respData resp)
                        in StoreGCResponse collected remaining bytes

                    _ -> StoreErrorResponse $ "Unknown response type: " <> respType resp
            else
                StoreErrorResponse $ respMessage resp

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

-- | Path where return-continuation derivations should be written
returnDerivationPath :: FilePath -> FilePath
returnDerivationPath sandboxDir = sandboxDir </> "result.drv"

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
withSandbox :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build t a) -> TenM 'Build t a
withSandbox inputs config action = do
    env <- ask

    -- Create a temporary directory for the sandbox
    let sandboxDir = workDir env </> "sandbox" </> "temp-" ++ show (currentBuildId env)
    liftIO $ createDirectoryIfMissing True sandboxDir

    -- Set up the sandbox with appropriate privilege and isolation
    case currentPrivilegeTier env of
        Daemon ->
            -- For daemon, we can set up a full sandbox with proper isolation
            setupDaemonSandbox env inputs config sandboxDir

        Builder ->
            -- For builder, we use a simplified sandbox since we can't drop privileges
            setupBuilderSandbox env inputs config sandboxDir

    -- Run the action and ensure cleanup happens even if it fails
    finally (action sandboxDir) $ do
        -- Clean up sandbox unless we're keeping the output
        unless (sandboxKeepBuildOutput config) $
            liftIO $ removeDirectoryRecursive sandboxDir
  where
    -- Full privileged sandbox setup for daemon tier
    setupDaemonSandbox env inputs config dir = do
        -- Make store inputs available in the sandbox
        forM_ (Set.toList inputs) $ \path -> do
            -- Create symlinks to store paths
            let storePath = storePathToFilePath path env
            let targetPath = dir </> "store" </> T.unpack (storeHash path ++ "-" ++ storeName path)
            liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
            liftIO $ createSymbolicLink storePath targetPath

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
                -- Use mknod to create basic device nodes
                void $ liftIO $ system $ "mknod " ++ dir </> "dev/null" ++ " c 1 3"
                void $ liftIO $ system $ "mknod " ++ dir </> "dev/zero" ++ " c 1 5"
                void $ liftIO $ system $ "mknod " ++ dir </> "dev/urandom" ++ " c 1 9"

                -- Mount /proc if configured
                when (sandboxMountProc config) $
                    void $ liftIO $ system $ "mount -t proc proc " ++ dir </> "proc"

            -- Setup resources limits with cgroups if enabled
            when (sandboxUseCgroups config) $
                setupCgroups dir (sandboxResourceLimits config)

    -- Simplified sandbox for builder tier (can't use most isolation features)
    setupBuilderSandbox env inputs config dir = do
        -- Make store inputs available in the sandbox via symlinks
        forM_ (Set.toList inputs) $ \path -> do
            let storePath = storePathToFilePath path env
            let targetPath = dir </> "inputs" </> T.unpack (storeName path)
            liftIO $ createDirectoryIfMissing True (takeDirectory targetPath)
            liftIO $ createSymbolicLink storePath targetPath

        -- Create workspace directories
        liftIO $ createDirectoryIfMissing True (dir </> "work")
        liftIO $ createDirectoryIfMissing True (dir </> "outputs")

    -- Set up cgroups for resource limiting (simplified implementation)
    setupCgroups dir limits = do
        -- Create a cgroup for this build
        let cgroupName = "ten-build-" ++ takeFileName dir
        void $ liftIO $ system $ "cgcreate -g cpu,memory,pids:" ++ cgroupName

        -- Set resource limits
        forM_ (Map.toList limits) $ \(resource, value) ->
            case resource of
                "memory" ->
                    void $ liftIO $ system $ "cgset -r memory.limit_in_bytes=" ++ show (value * 1024) ++ " " ++ cgroupName
                "cpu-time" ->
                    void $ liftIO $ system $ "cgset -r cpu.cfs_quota_us=" ++ show (value * 1000) ++ " " ++ cgroupName
                "processes" ->
                    void $ liftIO $ system $ "cgset -r pids.max=" ++ show value ++ " " ++ cgroupName
                _ -> return ()

    -- Run a system command (simplified)
    system cmd = do
        (exitCode, _, _) <- readCreateProcessWithExitCode (proc "sh" ["-c", cmd]) ""
        return $ exitCode == ExitSuccess

    -- Run an action and perform cleanup afterwards
    finally action cleanup = do
        result <- catchError action (\e -> cleanup >> throwError e)
        cleanup
        return result

    -- Recursive directory removal
    removeDirectoryRecursive path = do
        -- Use rm -rf for simplicity in this implementation
        void $ system $ "rm -rf " ++ path
