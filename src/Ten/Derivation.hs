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
    PrivilegeContext(..),

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

    -- Derivation storage - privileged operations
    storeDerivation,
    retrieveDerivation,

    -- Derivation storage - unprivileged operations
    requestStoreDerivation,
    requestRetrieveDerivation,

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

    -- Protocol helpers for privilege boundary crossing
    sendDerivationRequest,
    receiveDerivationResponse
) where

import Control.Concurrent.STM
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
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import System.FilePath
import System.Directory (doesFileExist)
import qualified System.Posix.Files as Posix
import System.IO (withFile, IOMode(..))
import System.Process (CreateProcess(..), proc, readCreateProcessWithExitCode, StdStream(NoStream, CreatePipe))
import System.Exit
import Control.Exception (try, SomeException, finally, catch)
import Database.SQLite.Simple (Only(..))
import Data.Int (Int64)
import Network.Socket (Socket)

import Ten.Core
import qualified Ten.Hash as Hash
import Ten.Store
import Ten.Sandbox (returnDerivationPath, prepareSandboxEnvironment,
                   SandboxConfig, withSandbox, defaultSandboxConfig)
import Ten.DB.Core (initDatabase, closeDatabase, withDatabase)

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
hashDerivation drv = Hash.showHash $ Hash.hashDerivation (derivName drv) (derivArgs drv) (derivEnv drv)

-- | Create a new derivation - allowed in both contexts
mkDerivation :: (PrivilegeContext c) => Text -> StorePath -> [Text] -> Set DerivationInput
             -> Set Text -> Map Text Text -> Text -> TenM 'Eval c Derivation
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

    let derivHash' = T.pack $ show $ Hash.hashText hashBase

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

-- | Store a derivation in the content-addressed store (privileged operation)
storeDerivation :: Derivation -> TenM p 'Privileged StorePath
storeDerivation drv = do
    -- Validate the derivation before storing
    validateDerivation drv

    -- Serialize the derivation to a ByteString
    let serialized = serializeDerivation drv

    -- Add to store with .drv extension (privileged operation)
    addToStore (derivName drv <> ".drv") serialized

-- | Request to store a derivation via daemon (unprivileged operation)
requestStoreDerivation :: Derivation -> TenM p 'Unprivileged StorePath
requestStoreDerivation drv = do
    -- Ensure we have a daemon connection
    conn <- getDaemonConnection

    -- Validate the derivation before sending
    validateDerivation drv

    -- Send the derivation storage request via protocol
    let serialized = serializeDerivation drv
    let request = StoreDerivationRequest (derivName drv <> ".drv") serialized

    -- Send request to daemon and receive response
    response <- sendDerivationRequest conn request

    case response of
        StoreDerivationResponse path -> return path
        ErrorResponse err -> throwError $ DaemonError $ "Failed to store derivation: " <> err
        _ -> throwError $ DaemonError "Unexpected response from daemon"

-- | Retrieve a derivation from the store (privileged operation)
retrieveDerivation :: StorePath -> TenM p 'Privileged (Maybe Derivation)
retrieveDerivation path = do
    -- Validate the path before retrieving
    validateStorePath path

    -- Check if the derivation exists in the store
    exists <- storePathExists path
    if not exists
        then return Nothing
        else do
            -- Read from store
            content <- readFromStore path

            -- Deserialize
            case deserializeDerivation content of
                Left err -> do
                    logMsg 1 $ "Error deserializing derivation: " <> err
                    return Nothing
                Right drv -> return $ Just drv

-- | Request to retrieve a derivation via daemon (unprivileged operation)
requestRetrieveDerivation :: StorePath -> TenM p 'Unprivileged (Maybe Derivation)
requestRetrieveDerivation path = do
    -- Ensure we have a daemon connection
    conn <- getDaemonConnection

    -- Validate the path before sending
    validateStorePath path

    -- Create and send the request
    let request = RetrieveDerivationRequest path
    response <- sendDerivationRequest conn request

    case response of
        RetrieveDerivationResponse Nothing ->
            return Nothing
        RetrieveDerivationResponse (Just drv) ->
            return $ Just drv
        ErrorResponse err ->
            throwError $ DaemonError $ "Failed to retrieve derivation: " <> err
        _ ->
            throwError $ DaemonError "Unexpected response from daemon"

-- | Instantiate a derivation for building - context-aware implementation
instantiateDerivation :: (PrivilegeContext c) => Derivation -> TenM 'Build c ()
instantiateDerivation deriv = do
    -- Store the derivation - use the appropriate method based on context
    ctx <- getPrivilegeContext
    derivPath <- case ctx of
        Privileged -> storeDerivation deriv
        Unprivileged -> requestStoreDerivation deriv

    -- Verify inputs exist - works in both contexts
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

-- | The monadic join operation for Return-Continuation - context-aware
joinDerivation :: (PrivilegeContext c) => Derivation -> TenM 'Build c Derivation
joinDerivation outerDrv = do
    -- Build the outer derivation just enough to get inner derivation
    innerDrv <- buildToGetInnerDerivation outerDrv

    -- Record that we performed a join operation
    addProof ReturnProof

    -- Return the inner derivation to be built
    return innerDrv

-- | Build a derivation to the point where it returns an inner derivation - context-aware
buildToGetInnerDerivation :: (PrivilegeContext c) => Derivation -> TenM 'Build c Derivation
buildToGetInnerDerivation drv = do
    env <- ask
    state <- get
    ctx <- getPrivilegeContext

    -- Create a sandbox configuration with return-continuation support
    let sandboxConfig = defaultSandboxConfig {
            sandboxReturnSupport = True,
            sandboxPrivileged = ctx == Privileged
        }

    -- Use withSandbox which handles both contexts correctly
    withSandbox (Set.map inputPath $ derivInputs drv) sandboxConfig $ \sandboxDir -> do
        -- Get the builder from the store - works in both contexts
        builderContent <- readFromStore (derivBuilder drv)

        -- Write the builder to the sandbox
        let builderPath = sandboxDir </> "builder"
        liftIO $ BS.writeFile builderPath builderContent
        liftIO $ Posix.setFileMode builderPath 0o755

        -- Prepare environment variables
        let buildEnv = prepareSandboxEnvironment env state sandboxDir (derivEnv drv)

        -- Add context-specific environment variables
        let contextEnv = case ctx of
                Privileged -> Map.singleton "TEN_PRIVILEGED" "1"
                Unprivileged -> Map.singleton "TEN_UNPRIVILEGED" "1"

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

                        -- Store the inner derivation - use the appropriate method based on context
                        case ctx of
                            Privileged -> void $ storeDerivation innerDrv
                            Unprivileged -> void $ requestStoreDerivation innerDrv

                        return innerDrv
            else
                throwError $ BuildFailed $
                    "Builder did not return a derivation at: " <> T.pack returnPath <>
                    "\nExit code: " <> T.pack (show exitCode) <>
                    "\nStdout: " <> T.pack stdout <>
                    "\nStderr: " <> T.pack stderr

-- | Get output paths from a derivation - works in any context
derivationOutputPaths :: Derivation -> Set StorePath
derivationOutputPaths = Set.map outputPath . derivOutputs

-- | Get input paths from a derivation - works in any context
derivationInputPaths :: Derivation -> Set StorePath
derivationInputPaths = Set.map inputPath . derivInputs

-- | Serialize a derivation to a ByteString - works in any context
serializeDerivation :: Derivation -> BS.ByteString
serializeDerivation = LBS.toStrict . Aeson.encode . derivationToJSON

-- | Convert derivation to JSON - works in any context
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

-- Helper to convert StorePath to JSON - works in any context
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

-- | Deserialize a derivation from a ByteString - works in any context
deserializeDerivation :: BS.ByteString -> Either Text Derivation
deserializeDerivation bs =
    case Aeson.eitherDecode (LBS.fromStrict bs) of
        Left err -> Left $ T.pack $ "JSON parse error: " ++ err
        Right json -> derivationFromJSON json

-- | Convert JSON to Derivation - works in any context
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

-- | Hash a derivation's inputs for dependency tracking - works in any context
hashDerivationInputs :: Derivation -> Text
hashDerivationInputs drv =
    T.pack $ show $ Hash.hashText $ T.intercalate ":" $
        map (\input -> storeHash $ inputPath input) $
        Set.toList $ derivInputs drv

-- | Create a sandbox process configuration - works in any context
sandboxedProcessConfig :: FilePath -> FilePath -> [String] -> Map Text Text -> SandboxConfig -> CreateProcess
sandboxedProcessConfig sandboxDir programPath args envVars config =
    (proc programPath args)
        { cwd = Just sandboxDir
        , env = Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList envVars
        , std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

-- | Validate a derivation before storage or building - works in any context
validateDerivation :: Derivation -> TenM p c ()
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

-- | Get the daemon connection for unprivileged operations
getDaemonConnection :: TenM p 'Unprivileged DaemonConnection
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ DaemonError "No daemon connection available in unprivileged context"

-- | Get the current privilege context
getPrivilegeContext :: TenM p c PrivilegeContext
getPrivilegeContext = do
    env <- ask
    return $ privilegeContext env

-- | Protocol request type for derivation operations
data DerivationRequest
    = StoreDerivationRequest Text BS.ByteString
    | RetrieveDerivationRequest StorePath
    | ValidateDerivationRequest StorePath
    deriving (Show, Eq)

-- | Protocol response type for derivation operations
data DerivationResponse
    = StoreDerivationResponse StorePath
    | RetrieveDerivationResponse (Maybe Derivation)
    | ValidateDerivationResponse Bool
    | ErrorResponse Text
    deriving (Show, Eq)

-- | Send a derivation request to the daemon
sendDerivationRequest :: DaemonConnection -> DerivationRequest -> TenM p 'Unprivileged DerivationResponse
sendDerivationRequest conn request = do
    -- Convert to protocol message
    let protocolMessage = case request of
            StoreDerivationRequest name content ->
                ProtocolRequest StoreDerivation (Aeson.object [
                    "name" .= name,
                    "content" .= TE.decodeUtf8 content
                ])
            RetrieveDerivationRequest path ->
                ProtocolRequest RetrieveDerivation (Aeson.object [
                    "path" .= storePathToText path
                ])
            ValidateDerivationRequest path ->
                ProtocolRequest ValidateDerivation (Aeson.object [
                    "path" .= storePathToText path
                ])

    -- Send via client interface and wait for response
    result <- clientRequest conn protocolMessage

    -- Process response
    case result of
        ProtocolResponse StoreDerivationOp (Aeson.Object obj) ->
            case Aeson.KeyMap.lookup "path" obj of
                Just (Aeson.String pathText) ->
                    case parseStorePath pathText of
                        Just path -> return $ StoreDerivationResponse path
                        Nothing -> return $ ErrorResponse $ "Invalid path in response: " <> pathText
                _ -> return $ ErrorResponse "Missing path in response"

        ProtocolResponse RetrieveDerivationOp (Aeson.Object obj) ->
            case Aeson.KeyMap.lookup "derivation" obj of
                Just Aeson.Null ->
                    return $ RetrieveDerivationResponse Nothing
                Just derivObj ->
                    case derivationFromJSON derivObj of
                        Right drv -> return $ RetrieveDerivationResponse (Just drv)
                        Left err -> return $ ErrorResponse $ "Failed to parse derivation: " <> err
                _ -> return $ ErrorResponse "Missing derivation in response"

        ProtocolResponse ValidateDerivationOp (Aeson.Object obj) ->
            case Aeson.KeyMap.lookup "valid" obj of
                Just (Aeson.Bool valid) -> return $ ValidateDerivationResponse valid
                _ -> return $ ErrorResponse "Missing validity in response"

        ProtocolResponse ErrorOp (Aeson.Object obj) ->
            case Aeson.KeyMap.lookup "message" obj of
                Just (Aeson.String msg) -> return $ ErrorResponse msg
                _ -> return $ ErrorResponse "Unknown error"

        _ -> return $ ErrorResponse "Unexpected response format"

-- | Receive and handle a derivation request from a client (daemon side)
receiveDerivationResponse :: Socket -> ProtocolRequest -> TenM p 'Privileged ProtocolResponse
receiveDerivationResponse clientSocket request = do
    case request of
        ProtocolRequest StoreDerivation (Aeson.Object obj) -> do
            -- Extract name and content
            case (Aeson.KeyMap.lookup "name" obj, Aeson.KeyMap.lookup "content" obj) of
                (Just (Aeson.String name), Just (Aeson.String contentText)) -> do
                    -- Deserialize the derivation
                    let content = TE.encodeUtf8 contentText
                    case deserializeDerivation content of
                        Right drv -> do
                            -- Store the derivation (privileged operation)
                            path <- storeDerivation drv
                            -- Return the resulting path
                            return $ ProtocolResponse StoreDerivationOp $ Aeson.object [
                                "path" .= storePathToText path
                                ]
                        Left err ->
                            return $ ProtocolResponse ErrorOp $ Aeson.object [
                                "message" .= ("Invalid derivation: " <> err)
                                ]
                _ ->
                    return $ ProtocolResponse ErrorOp $ Aeson.object [
                        "message" .= ("Missing required fields" :: Text)
                        ]

        ProtocolRequest RetrieveDerivation (Aeson.Object obj) ->
            case Aeson.KeyMap.lookup "path" obj of
                Just (Aeson.String pathText) ->
                    case parseStorePath pathText of
                        Just path -> do
                            -- Retrieve the derivation (privileged operation)
                            mDrv <- retrieveDerivation path
                            -- Return the derivation or null
                            return $ ProtocolResponse RetrieveDerivationOp $ Aeson.object [
                                "derivation" .= maybe Aeson.Null derivationToJSON mDrv
                                ]
                        Nothing ->
                            return $ ProtocolResponse ErrorOp $ Aeson.object [
                                "message" .= ("Invalid path format" :: Text)
                                ]
                _ ->
                    return $ ProtocolResponse ErrorOp $ Aeson.object [
                        "message" .= ("Missing path field" :: Text)
                        ]

        ProtocolRequest ValidateDerivation (Aeson.Object obj) ->
            case Aeson.KeyMap.lookup "path" obj of
                Just (Aeson.String pathText) ->
                    case parseStorePath pathText of
                        Just path -> do
                            -- Check if the path exists (privileged operation)
                            exists <- storePathExists path
                            -- Return the result
                            return $ ProtocolResponse ValidateDerivationOp $ Aeson.object [
                                "valid" .= exists
                                ]
                        Nothing ->
                            return $ ProtocolResponse ErrorOp $ Aeson.object [
                                "message" .= ("Invalid path format" :: Text)
                                ]
                _ ->
                    return $ ProtocolResponse ErrorOp $ Aeson.object [
                        "message" .= ("Missing path field" :: Text)
                        ]
        _ ->
            return $ ProtocolResponse ErrorOp $ Aeson.object [
                "message" .= ("Unsupported request type" :: Text)
                ]

-- | Type for protocol request operations
data ProtocolOp
    = StoreDerivation
    | RetrieveDerivation
    | ValidateDerivation
    | ErrorOp
    deriving (Show, Eq)

-- | Type for protocol requests
data ProtocolRequest
    = ProtocolRequest ProtocolOp Aeson.Value
    deriving (Show, Eq)

-- | Type for protocol responses
data ProtocolResponse
    = ProtocolResponse ProtocolOp Aeson.Value
    deriving (Show, Eq)

-- | Send a request to the daemon (client-side)
clientRequest :: DaemonConnection -> ProtocolRequest -> TenM p 'Unprivileged ProtocolResponse
clientRequest conn request = do
    -- In a real implementation, this would send the request over the socket
    -- and wait for a response
    liftIO $ do
        -- Simulate sending request
        let requestStr = T.pack $ show request

        -- Simulate receiving response (for the example)
        let responseStr = case request of
                ProtocolRequest StoreDerivation _ ->
                    "ProtocolResponse StoreDerivationOp {\"path\":\"abcdef1234-output.drv\"}"
                ProtocolRequest RetrieveDerivation _ ->
                    "ProtocolResponse RetrieveDerivationOp {\"derivation\":null}"
                _ ->
                    "ProtocolResponse ErrorOp {\"message\":\"Unimplemented\"}"

        -- Parse response (in a real implementation this would be proper protocol parsing)
        return $ read $ T.unpack responseStr
