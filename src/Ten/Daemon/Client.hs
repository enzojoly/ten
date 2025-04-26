{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Ten.Daemon.Client (
    -- Socket management with privilege awareness
    connectToDaemon,
    disconnectFromDaemon,
    getDefaultSocketPath,
    withDaemonConnection,

    -- Client communication with privilege checks
    sendRequest,
    receiveResponse,
    sendRequestSync,

    -- Status checking
    isDaemonRunning,
    getDaemonStatus,

    -- High-level build operations
    buildFile,
    evalFile,
    buildDerivation,
    cancelBuild,
    getBuildStatus,
    getBuildOutput,
    listBuilds,

    -- Store operations (with proper privilege handling)
    addFileToStore,
    verifyStorePath,
    getStorePathForFile,
    listStore,

    -- Derivation operations
    storeDerivation,
    retrieveDerivation,
    queryDerivationForOutput,
    queryOutputsForDerivation,
    listDerivations,
    getDerivationInfo,

    -- GC operations (daemon-only, via protocol)
    collectGarbage,
    getGCStatus,
    addGCRoot,
    removeGCRoot,
    listGCRoots,

    -- Daemon management
    startDaemonIfNeeded,
    shutdownDaemon,
    getDaemonConfig,

    -- Authentication types re-exports
    UserCredentials(..),

    -- Internal utilities
    createSocketAndConnect,
    readResponseWithTimeout,
    encodeRequest,
    decodeResponse
) where

import Control.Concurrent (forkIO, ThreadId, threadDelay, myThreadId, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, newMVar, readMVar)
import Control.Concurrent.STM
import Control.Exception (catch, finally, bracketOnError, bracket, throwIO, SomeException, try, IOException)
import Control.Monad (void, when, forever, unless)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString, word32BE, byteString)
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Word (Word32)
import Network.Socket (Socket, Family(..), SocketType(..), SockAddr(..), socket, connect, close, socketToHandle)
import Network.Socket.ByteString (sendAll, recv)
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory, getXdgDirectory, XdgDirectory(..), findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), hClose, hFlush, hPutStrLn, stderr, hPutStr, BufferMode(..), hSetBuffering)
import System.Process (createProcess, proc, waitForProcess, StdStream(..), CreateProcess(..))
import System.Timeout (timeout)

import Ten.Core (
    DaemonConnection(..),
    Request(..),
    Response(..),
    PrivilegeTier(..),
    SPrivilegeTier(..),
    Phase(..),
    BuildError(..),
    BuildId(..),
    BuildStatus(..),
    StorePath(..),
    storePathToText,
    createDaemonConnection,
    closeDaemonConnection,
    AuthToken(..),
    UserId(..),
    AuthResult(..),
    Derivation(..),
    parseStorePath,
    BuildResult(..),
    GCStats(..),
    GCRoot(..),
    DaemonConfig(..),
    sBuilder,
    ProtocolVersion(..),
    currentProtocolVersion)
import qualified Ten.Core as Core

import Ten.Daemon.Protocol (DaemonRequest(..), DaemonResponse(..))
import qualified Ten.Daemon.Protocol as Protocol

import Ten.Daemon.Auth (UserCredentials(..))
import Ten.Daemon.Config (getDefaultSocketPath)

import Ten.Derivation (serializeDerivation, deserializeDerivation)
import qualified Ten.Derivation as Derivation

-- | Connect to the Ten daemon - always in Builder context
connectToDaemon :: FilePath -> UserCredentials -> IO (Either BuildError (DaemonConnection 'Builder))
connectToDaemon socketPath credentials = try $ do
    -- Check if daemon is running
    running <- isDaemonRunning socketPath

    -- If not running, try to start it if autostart is enabled
    unless running $ do
        startResult <- startDaemonIfNeeded socketPath
        case startResult of
            Left err -> throwIO err
            Right _ ->
                -- Brief pause to allow daemon to initialize
                threadDelay 500000 -- 0.5 seconds

    -- Create socket and connect
    (sock, handle) <- createSocketAndConnect socketPath

    -- Set up proper handle buffering
    hSetBuffering handle (BlockBuffering Nothing)

    -- Authenticate with the daemon
    let authReq = AuthRequest {
            reqType = "auth",
            reqParams = Map.fromList [
                ("version", T.pack $ show currentProtocolVersion),
                ("username", username credentials),
                ("token", token credentials),
                ("requestedTier", "Builder")
            ],
            reqPayload = Nothing,
            reqId = 0
        }

    -- Encode auth request
    let reqBS = Core.encodeRequest authReq

    -- Send auth request
    BS.hPut handle reqBS
    hFlush handle

    -- Read auth response
    respBS <- readMessageWithTimeout handle 5000000 -- 5 seconds timeout

    -- Parse auth response
    case Core.decodeResponse respBS of
        Left err ->
            throwIO $ AuthError $ "Authentication failed: " <> err

        Right resp ->
            if respStatus resp == "ok" then
                case parseAuthResult (respData resp) of
                    AuthAccepted userId authToken capabilities -> do
                        -- Create daemon connection with proper privilege tier evidence
                        conn <- Core.createDaemonConnection sock handle userId authToken sBuilder
                        return conn
                    AuthRejected reason ->
                        throwIO $ AuthError $ "Authentication rejected: " <> reason
                    _ ->
                        throwIO $ AuthError "Unexpected authentication response"
            else
                throwIO $ AuthError $ respMessage resp

-- | Parse authentication result from response data
parseAuthResult :: Map Text Text -> AuthResult
parseAuthResult data_ =
    let status = Map.findWithDefault "rejected" "status" data_
    in case status of
        "accepted" ->
            let userId = UserId $ Map.findWithDefault "" "userId" data_
                token = AuthToken $ Map.findWithDefault "" "token" data_
                capStrs = T.splitOn "," $ Map.findWithDefault "" "capabilities" data_
                caps = Set.fromList $ map parseCapability capStrs
            in AuthAccepted userId token caps
        "success" ->
            let userId = UserId $ Map.findWithDefault "" "userId" data_
                token = AuthToken $ Map.findWithDefault "" "token" data_
            in AuthSuccess userId token
        _ -> AuthRejected $ Map.findWithDefault "Authentication rejected" "reason" data_

-- | Parse a capability from text
parseCapability :: Text -> Protocol.DaemonCapability
parseCapability "StoreAccess" = Protocol.StoreAccess
parseCapability "SandboxCreation" = Protocol.SandboxCreation
parseCapability "GarbageCollection" = Protocol.GarbageCollection
parseCapability "DerivationRegistration" = Protocol.DerivationRegistration
parseCapability "DerivationBuild" = Protocol.DerivationBuild
parseCapability "StoreQuery" = Protocol.StoreQuery
parseCapability "BuildQuery" = Protocol.BuildQuery
parseCapability _ = Protocol.BuildQuery  -- Default to safe capability

-- | Disconnect from the Ten daemon
disconnectFromDaemon :: DaemonConnection 'Builder -> IO ()
disconnectFromDaemon conn = do
    -- Use the closeDaemonConnection function from Ten.Core
    Core.closeDaemonConnection conn

-- | Check if the daemon is running
isDaemonRunning :: FilePath -> IO Bool
isDaemonRunning socketPath = do
    -- Check if socket file exists
    socketExists <- doesFileExist socketPath

    if not socketExists
        then return False
        else do
            -- Try to connect to the socket
            result <- try $ createSocketAndConnect socketPath
            case result of
                Left (_ :: SomeException) ->
                    -- Connection failed, daemon not running
                    return False

                Right (sock, handle) -> do
                    -- Connection succeeded, daemon is running
                    hClose handle
                    close sock
                    return True

-- | Execute an action with a daemon connection
withDaemonConnection :: FilePath -> UserCredentials -> (DaemonConnection 'Builder -> IO a) -> IO (Either BuildError a)
withDaemonConnection socketPath credentials action = try $
    bracket
        (do
            connResult <- connectToDaemon socketPath credentials
            case connResult of
                Left err -> throwIO err
                Right conn -> return conn)
        disconnectFromDaemon
        (\conn -> do
            result <- try (action conn)
            case result of
                Left err -> throwIO err
                Right value -> return value)

-- | Create a socket and connect to the daemon
createSocketAndConnect :: FilePath -> IO (Socket, Handle)
createSocketAndConnect socketPath = do
    -- Ensure parent directory exists
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Create socket
    sock <- socket AF_UNIX Stream 0

    -- Connect to socket with error handling
    bracketOnError
        (return sock)
        close
        (\s -> do
            connect s (SockAddrUnix socketPath)
            handle <- socketToHandle s ReadWriteMode
            return (s, handle))

-- | Send a request to the daemon with privilege checking
sendRequest :: DaemonConnection 'Builder -> DaemonRequest -> IO (Either BuildError Int)
sendRequest conn request = do
    -- Convert domain request to core Request
    let req = Request {
            reqId = 0,  -- Will be set by sendRequest
            reqType = requestTypeToString request,
            reqParams = requestToParams request,
            reqPayload = requestToPayload request
        }

    -- Send request to daemon using Core implementation
    reqId <- Core.sendRequest conn req
    return $ Right reqId

-- | Convert request type to string
requestTypeToString :: DaemonRequest -> Text
requestTypeToString = \case
    Protocol.AuthRequest {} -> "auth"
    Protocol.BuildRequest {} -> "build"
    Protocol.EvalRequest {} -> "eval"
    Protocol.BuildDerivationRequest {} -> "build-derivation"
    Protocol.BuildStatusRequest {} -> "build-status"
    Protocol.CancelBuildRequest {} -> "cancel-build"
    Protocol.QueryBuildOutputRequest {} -> "build-output"
    Protocol.ListBuildsRequest {} -> "list-builds"
    Protocol.StoreAddRequest {} -> "store-add"
    Protocol.StoreVerifyRequest {} -> "store-verify"
    Protocol.StorePathRequest {} -> "store-path"
    Protocol.StoreListRequest -> "store-list"
    Protocol.StoreDerivationRequest {} -> "store-derivation"
    Protocol.RetrieveDerivationRequest {} -> "retrieve-derivation"
    Protocol.QueryDerivationRequest {} -> "query-derivation"
    Protocol.GetDerivationForOutputRequest {} -> "get-derivation-for-output"
    Protocol.ListDerivationsRequest {} -> "list-derivations"
    Protocol.GCRequest {} -> "gc"
    Protocol.GCStatusRequest {} -> "gc-status"
    Protocol.AddGCRootRequest {} -> "add-gc-root"
    Protocol.RemoveGCRootRequest {} -> "remove-gc-root"
    Protocol.ListGCRootsRequest -> "list-gc-roots"
    Protocol.PingRequest -> "ping"
    Protocol.ShutdownRequest -> "shutdown"
    Protocol.StatusRequest -> "status"
    Protocol.ConfigRequest -> "config"

-- | Convert request to parameters map
requestToParams :: DaemonRequest -> Map Text Text
requestToParams = \case
    Protocol.AuthRequest {} -> Map.empty  -- Handled specially in connectToDaemon
    Protocol.BuildRequest path contentMaybe info -> Map.fromList [
        ("path", path),
        ("hasContent", if isJust contentMaybe then "true" else "false"),
        ("timeout", maybe "" (T.pack . show) (buildTimeout info)),
        ("flags", T.intercalate "," (buildFlags info))
        ]
    Protocol.EvalRequest path contentMaybe info -> Map.fromList [
        ("path", path),
        ("hasContent", if isJust contentMaybe then "true" else "false"),
        ("timeout", maybe "" (T.pack . show) (buildTimeout info)),
        ("flags", T.intercalate "," (buildFlags info))
        ]
    Protocol.BuildDerivationRequest _ info -> Map.fromList [
        ("hasDerivation", "true"),
        ("timeout", maybe "" (T.pack . show) (buildTimeout info)),
        ("flags", T.intercalate "," (buildFlags info))
        ]
    Protocol.BuildStatusRequest buildId -> Map.fromList [
        ("buildId", renderBuildId buildId)
        ]
    Protocol.CancelBuildRequest buildId -> Map.fromList [
        ("buildId", renderBuildId buildId)
        ]
    Protocol.QueryBuildOutputRequest buildId -> Map.fromList [
        ("buildId", renderBuildId buildId)
        ]
    Protocol.ListBuildsRequest limit -> Map.fromList [
        ("limit", maybe "" (T.pack . show) limit)
        ]
    Protocol.StoreAddRequest path _ -> Map.fromList [
        ("path", path),
        ("hasContent", "true")
        ]
    Protocol.StoreVerifyRequest path -> Map.fromList [
        ("path", storePathToText path)
        ]
    Protocol.StorePathRequest path _ -> Map.fromList [
        ("path", path),
        ("hasContent", "true")
        ]
    Protocol.StoreListRequest -> Map.empty
    Protocol.StoreDerivationRequest _ -> Map.singleton "hasContent" "true"
    Protocol.RetrieveDerivationRequest path -> Map.singleton "path" (storePathToText path)
    Protocol.QueryDerivationRequest qType qValue qLimit -> Map.fromList [
        ("queryType", qType),
        ("queryValue", qValue),
        ("limit", maybe "" (T.pack . show) qLimit)
        ]
    Protocol.GetDerivationForOutputRequest path -> Map.singleton "path" path
    Protocol.ListDerivationsRequest limit -> Map.singleton "limit" (maybe "" (T.pack . show) limit)
    Protocol.GCRequest force -> Map.singleton "force" (if force then "true" else "false")
    Protocol.GCStatusRequest forceCheck -> Map.singleton "forceCheck" (if forceCheck then "true" else "false")
    Protocol.AddGCRootRequest path name permanent -> Map.fromList [
        ("path", storePathToText path),
        ("name", name),
        ("permanent", if permanent then "true" else "false")
        ]
    Protocol.RemoveGCRootRequest name -> Map.singleton "name" name
    Protocol.ListGCRootsRequest -> Map.empty
    Protocol.PingRequest -> Map.empty
    Protocol.ShutdownRequest -> Map.empty
    Protocol.StatusRequest -> Map.empty
    Protocol.ConfigRequest -> Map.empty

-- | Convert request to payload
requestToPayload :: DaemonRequest -> Maybe BS.ByteString
requestToPayload = \case
    Protocol.BuildRequest _ (Just content) _ -> Just content
    Protocol.EvalRequest _ (Just content) _ -> Just content
    Protocol.BuildDerivationRequest drv _ -> Just (serializeDerivation drv)
    Protocol.StoreAddRequest _ content -> Just content
    Protocol.StorePathRequest _ content -> Just content
    Protocol.StoreDerivationRequest content -> Just content
    _ -> Nothing

-- | Render a BuildId to Text
renderBuildId :: BuildId -> Text
renderBuildId (BuildId u) = "build-" <> T.pack (show (Core.hashUnique u))
renderBuildId (BuildIdFromInt n) = "build-" <> T.pack (show n)

-- | Parse a BuildId from Text
parseBuildId :: Text -> BuildId
parseBuildId txt =
    case T.stripPrefix "build-" txt of
        Just numStr ->
            case readMaybe (T.unpack numStr) of
                Just n -> BuildIdFromInt n
                Nothing -> BuildIdFromInt 0  -- Default if invalid
        Nothing ->
            case readMaybe (T.unpack txt) of
                Just n -> BuildIdFromInt n
                Nothing -> BuildIdFromInt 0  -- Default if invalid

-- Helper function for readMaybe
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection 'Builder -> Int -> Int -> IO (Either BuildError DaemonResponse)
receiveResponse conn reqId timeoutMicros = do
    -- Use the core receiveResponse function
    result <- Core.receiveResponse conn reqId timeoutMicros

    -- Convert from core Response to domain DaemonResponse
    case result of
        Left err -> return $ Left err
        Right resp -> return $ parseResponse resp

-- | Send a request and wait for response (synchronous)
sendRequestSync :: DaemonConnection 'Builder -> DaemonRequest -> Int -> IO (Either BuildError DaemonResponse)
sendRequestSync conn request timeoutMicros = do
    -- Send request
    reqIdResult <- sendRequest conn request

    -- Process result
    case reqIdResult of
        Left err ->
            return $ Left err
        Right reqId ->
            receiveResponse conn reqId timeoutMicros

-- | Parse a response from core Response to domain DaemonResponse
parseResponse :: Response -> Either BuildError DaemonResponse
parseResponse resp
    | respStatus resp == "error" =
        Left $ parseErrorType (Map.findWithDefault "unknown" "type" (respData resp)) (respMessage resp)
    | otherwise = parseSuccessResponse resp

-- | Parse error type from response
parseErrorType :: Text -> Text -> BuildError
parseErrorType "eval" = EvalError
parseErrorType "build" = BuildFailed
parseErrorType "store" = StoreError
parseErrorType "sandbox" = SandboxError
parseErrorType "input" = \msg -> InputNotFound (T.unpack msg)
parseErrorType "hash" = HashError
parseErrorType "graph" = GraphError
parseErrorType "resource" = ResourceError
parseErrorType "daemon" = DaemonError
parseErrorType "auth" = AuthError
parseErrorType "cycle" = CyclicDependency
parseErrorType "serialization" = SerializationError
parseErrorType "recursion" = RecursionLimit
parseErrorType "network" = NetworkError
parseErrorType "parse" = ParseError
parseErrorType "db" = DBError
parseErrorType "gc" = GCError
parseErrorType "phase" = PhaseError
parseErrorType "privilege" = PrivilegeError
parseErrorType "protocol" = ProtocolError
parseErrorType _ = InternalError

-- | Parse successful response based on type
parseSuccessResponse :: Response -> Either BuildError DaemonResponse
parseSuccessResponse resp =
    case Map.lookup "type" (respData resp) of
        Just "auth" -> Right $ Protocol.AuthResponse $ parseAuthResult (respData resp)
        Just "build-started" -> Right $ Protocol.BuildStartedResponse $ parseBuildId (Map.findWithDefault "" "buildId" (respData resp))
        Just "build-result" -> Right $ Protocol.BuildResultResponse $ parseBuildResult (respData resp) (respPayload resp)
        Just "build-status" -> Right $ Protocol.BuildStatusResponse $ parseBuildStatus (respData resp)
        Just "build-output" -> Right $ Protocol.BuildOutputResponse $ Map.findWithDefault "" "output" (respData resp)
        Just "build-list" -> Right $ Protocol.BuildListResponse $ parseBuildList (respData resp)
        Just "build-cancelled" -> Right $ Protocol.CancelBuildResponse $ Map.findWithDefault "false" "success" (respData resp) == "true"
        Just "store-add" -> Right $ Protocol.StoreAddResponse $ parseStorePath (Map.findWithDefault "" "path" (respData resp))
        Just "store-verify" -> Right $ Protocol.StoreVerifyResponse $ Map.findWithDefault "false" "valid" (respData resp) == "true"
        Just "store-path" -> Right $ Protocol.StorePathResponse $ parseStorePath (Map.findWithDefault "" "path" (respData resp))
        Just "store-list" -> Right $ Protocol.StoreListResponse $ parseStorePaths (Map.findWithDefault "" "paths" (respData resp))
        Just "derivation" ->
            case respPayload resp of
                Just content ->
                    case deserializeDerivation content of
                        Left err -> Left $ SerializationError $ buildErrorToText err
                        Right drv -> Right $ Protocol.DerivationResponse drv
                Nothing -> Left $ SerializationError "Missing derivation content"
        Just "derivation-stored" -> Right $ Protocol.DerivationStoredResponse $ parseStorePath (Map.findWithDefault "" "path" (respData resp))
        Just "derivation-retrieved" ->
            case respPayload resp of
                Just content ->
                    case deserializeDerivation content of
                        Left err -> Left $ SerializationError $ buildErrorToText err
                        Right drv -> Right $ Protocol.DerivationRetrievedResponse (Just drv)
                Nothing -> Right $ Protocol.DerivationRetrievedResponse Nothing
        Just "derivation-query" ->
            case respPayload resp of
                Just content -> parseDerivationList content
                Nothing -> Right $ Protocol.DerivationQueryResponse []
        Just "derivation-outputs" -> Right $ Protocol.DerivationOutputResponse $ parseStorePathSet (Map.findWithDefault "" "outputs" (respData resp))
        Just "derivation-list" -> Right $ Protocol.DerivationListResponse $ parseStorePaths (Map.findWithDefault "" "paths" (respData resp))
        Just "gc-result" -> Right $ Protocol.GCResultResponse $ parseGCStats (respData resp)
        Just "gc-started" -> Right Protocol.GCStartedResponse
        Just "gc-status" -> Right $ Protocol.GCStatusResponse $ parseGCStatus (respData resp)
        Just "gc-root-added" -> Right $ Protocol.GCRootAddedResponse $ Map.findWithDefault "" "name" (respData resp)
        Just "gc-root-removed" -> Right $ Protocol.GCRootRemovedResponse $ Map.findWithDefault "" "name" (respData resp)
        Just "gc-roots-list" -> Right $ Protocol.GCRootListResponse $ parseGCRoots (respData resp)
        Just "pong" -> Right Protocol.PongResponse
        Just "shutdown" -> Right Protocol.ShutdownResponse
        Just "status" -> Right $ Protocol.StatusResponse $ parseDaemonStatus (respData resp)
        Just "config" -> Right $ Protocol.ConfigResponse $ parseDaemonConfig (respData resp)
        Just "eval" ->
            case respPayload resp of
                Just content ->
                    case deserializeDerivation content of
                        Left err -> Left $ SerializationError $ buildErrorToText err
                        Right drv -> Right $ Protocol.EvalResponse drv
                Nothing -> Left $ SerializationError "Missing derivation content"
        Just respType -> Left $ ProtocolError $ "Unknown response type: " <> respType
        Nothing -> Left $ ProtocolError "Missing response type"

-- | BuildRequestInfo with standard fields
data BuildRequestInfo = BuildRequestInfo {
    buildTimeout :: Maybe Int,    -- Build timeout in seconds
    buildEnv :: Map Text Text,    -- Extra environment variables
    buildFlags :: [Text]          -- Build flags (e.g., --keep-failed, --no-out-link)
} deriving (Show, Eq)

-- | Default build request info
defaultBuildRequestInfo :: BuildRequestInfo
defaultBuildRequestInfo = BuildRequestInfo {
    buildTimeout = Nothing,
    buildEnv = Map.empty,
    buildFlags = []
}

-- | Build status update info
data BuildStatusUpdate = BuildStatusUpdate {
    buildId :: BuildId,
    buildStatus :: BuildStatus,
    buildTimeElapsed :: Double,   -- Time elapsed in seconds
    buildTimeRemaining :: Maybe Double,  -- Estimated time remaining (if available)
    buildLogUpdate :: Maybe Text,  -- New log messages since last update
    buildResourceUsage :: Map Text Double  -- Resource usage metrics
} deriving (Show, Eq)

-- | GC status information
data GCStatusInfo = GCStatusInfo {
    gcRunning :: Bool,           -- Whether GC is currently running
    gcOwner :: Maybe Text,       -- Process/username owning the GC lock (if running)
    gcLockTime :: Maybe UTCTime  -- When the GC lock was acquired (if running)
} deriving (Show, Eq)

-- | Daemon status information
data DaemonStatus = DaemonStatus {
    daemonStatus :: Text,           -- "running", "starting", etc.
    daemonUptime :: Double,         -- Uptime in seconds
    daemonActiveBuilds :: Int,      -- Number of active builds
    daemonCompletedBuilds :: Int,   -- Number of completed builds since startup
    daemonFailedBuilds :: Int,      -- Number of failed builds since startup
    daemonGcRoots :: Int,           -- Number of GC roots
    daemonStoreSize :: Integer,     -- Store size in bytes
    daemonStorePaths :: Int         -- Number of paths in store
} deriving (Show, Eq)

-- | Derivation information type
data DerivationInfo = DerivationInfo {
    derivationResponseDrv :: Derivation,
    derivationResponseOutputs :: Set StorePath,
    derivationResponseInputs :: Set StorePath,
    derivationResponseStorePath :: StorePath,
    derivationResponseMetadata :: Map Text Text
} deriving (Show, Eq)

-- | Parse a BuildResult from response data and payload
parseBuildResult :: Map Text Text -> Maybe BS.ByteString -> BuildResult
parseBuildResult data_ _ =
    BuildResult {
        brOutputPaths = parseStorePathSet $ Map.findWithDefault "" "outputs" data_,
        brExitCode = if Map.findWithDefault "false" "success" data_ == "true"
                     then ExitSuccess
                     else ExitFailure $ read $ T.unpack $ Map.findWithDefault "1" "exitCode" data_,
        brLog = Map.findWithDefault "" "log" data_,
        brReferences = parseStorePathSet $ Map.findWithDefault "" "references" data_
    }

-- | Parse a BuildStatus from response data
parseBuildStatus :: Map Text Text -> BuildStatusUpdate
parseBuildStatus data_ =
    BuildStatusUpdate {
        buildId = parseBuildId $ Map.findWithDefault "build-0" "buildId" data_,
        buildStatus = parseStatus $ Map.findWithDefault "pending" "status" data_,
        buildTimeElapsed = read $ T.unpack $ Map.findWithDefault "0" "timeElapsed" data_,
        buildTimeRemaining = if Map.member "timeRemaining" data_
                             then Just $ read $ T.unpack $ Map.findWithDefault "0" "timeRemaining" data_
                             else Nothing,
        buildLogUpdate = if Map.member "logUpdate" data_
                         then Just $ Map.findWithDefault "" "logUpdate" data_
                         else Nothing,
        buildResourceUsage = Map.empty  -- Not parsed for simplicity
    }
  where
    parseStatus :: Text -> BuildStatus
    parseStatus "pending" = BuildPending
    parseStatus "running" = BuildRunning $ read $ T.unpack $ Map.findWithDefault "0" "progress" data_
    parseStatus "recursing" = BuildRecursing $ parseBuildId $ Map.findWithDefault "build-0" "innerBuildId" data_
    parseStatus "completed" = BuildCompleted
    parseStatus "failed" = BuildFailed'
    parseStatus _ = BuildPending

-- | Parse a list of builds from response data
parseBuildList :: Map Text Text -> [(BuildId, BuildStatus, Float)]
parseBuildList data_ =
    let buildCount = read $ T.unpack $ Map.findWithDefault "0" "count" data_
        builds = [0..buildCount-1]
    in map (\i -> parseBuild i data_) builds
  where
    parseBuild :: Int -> Map Text Text -> (BuildId, BuildStatus, Float)
    parseBuild i data_ =
        let prefix = "build." <> T.pack (show i) <> "."
            idStr = Map.findWithDefault ("build-" <> T.pack (show i)) (prefix <> "id") data_
            statusStr = Map.findWithDefault "pending" (prefix <> "status") data_
            progressStr = Map.findWithDefault "0" (prefix <> "progress") data_
        in (parseBuildId idStr, parseStatus statusStr progressStr, read $ T.unpack progressStr)

    parseStatus :: Text -> Text -> BuildStatus
    parseStatus "pending" _ = BuildPending
    parseStatus "running" progress = BuildRunning $ read $ T.unpack progress
    parseStatus "recursing" innerBuildId = BuildRecursing $ parseBuildId innerBuildId
    parseStatus "completed" _ = BuildCompleted
    parseStatus "failed" _ = BuildFailed'
    parseStatus _ _ = BuildPending

-- | Parse a list of StorePaths from comma-separated text
parseStorePaths :: Text -> [StorePath]
parseStorePaths text =
    let pathStrs = T.splitOn "," text
        paths = map parseStorePath $ filter (not . T.null) pathStrs
    in paths

-- | Parse a set of StorePaths from comma-separated text
parseStorePathSet :: Text -> Set StorePath
parseStorePathSet text = Set.fromList $ parseStorePaths text

-- | Parse a list of derivations from binary content
parseDerivationList :: BS.ByteString -> Either BuildError DaemonResponse
parseDerivationList content =
    -- For simplicity, assume we're parsing a single derivation for now
    case deserializeDerivation content of
        Left err -> Left $ SerializationError $ buildErrorToText err
        Right drv -> Right $ Protocol.DerivationQueryResponse [drv]

-- | Parse GC stats from response data
parseGCStats :: Map Text Text -> GCStats
parseGCStats data_ =
    GCStats {
        gcTotal = read $ T.unpack $ Map.findWithDefault "0" "total" data_,
        gcLive = read $ T.unpack $ Map.findWithDefault "0" "live" data_,
        gcCollected = read $ T.unpack $ Map.findWithDefault "0" "collected" data_,
        gcBytes = read $ T.unpack $ Map.findWithDefault "0" "bytes" data_,
        gcElapsedTime = read $ T.unpack $ Map.findWithDefault "0" "elapsedTime" data_
    }

-- | Parse GC status from response data
parseGCStatus :: Map Text Text -> GCStatusInfo
parseGCStatus data_ =
    GCStatusInfo {
        gcRunning = Map.findWithDefault "false" "running" data_ == "true",
        gcOwner = if Map.member "owner" data_ then Just $ Map.findWithDefault "" "owner" data_ else Nothing,
        gcLockTime = Nothing  -- Not parsed for simplicity
    }

-- | Parse GC roots from response data
parseGCRoots :: Map Text Text -> [(StorePath, Text, Bool)]
parseGCRoots data_ =
    let rootCount = read $ T.unpack $ Map.findWithDefault "0" "count" data_
        roots = [0..rootCount-1]
    in map (\i -> parseRoot i data_) roots
  where
    parseRoot :: Int -> Map Text Text -> (StorePath, Text, Bool)
    parseRoot i data_ =
        let prefix = "root." <> T.pack (show i) <> "."
            pathStr = Map.findWithDefault "" (prefix <> "path") data_
            name = Map.findWithDefault "" (prefix <> "name") data_
            permanent = Map.findWithDefault "false" (prefix <> "permanent") data_ == "true"
        in (parseStorePath pathStr, name, permanent)

-- | Parse daemon status from response data
parseDaemonStatus :: Map Text Text -> DaemonStatus
parseDaemonStatus data_ =
    DaemonStatus {
        daemonStatus = Map.findWithDefault "unknown" "status" data_,
        daemonUptime = read $ T.unpack $ Map.findWithDefault "0" "uptime" data_,
        daemonActiveBuilds = read $ T.unpack $ Map.findWithDefault "0" "activeBuilds" data_,
        daemonCompletedBuilds = read $ T.unpack $ Map.findWithDefault "0" "completedBuilds" data_,
        daemonFailedBuilds = read $ T.unpack $ Map.findWithDefault "0" "failedBuilds" data_,
        daemonGcRoots = read $ T.unpack $ Map.findWithDefault "0" "gcRoots" data_,
        daemonStoreSize = read $ T.unpack $ Map.findWithDefault "0" "storeSize" data_,
        daemonStorePaths = read $ T.unpack $ Map.findWithDefault "0" "storePaths" data_
    }

-- | Parse daemon config from response data
parseDaemonConfig :: Map Text Text -> DaemonConfig
parseDaemonConfig data_ =
    DaemonConfig {
        daemonSocketPath = T.unpack $ Map.findWithDefault "/var/run/ten/daemon.sock" "socketPath" data_,
        daemonStorePath = T.unpack $ Map.findWithDefault "/var/lib/ten/store" "storePath" data_,
        daemonStateFile = T.unpack $ Map.findWithDefault "/var/lib/ten/state.json" "stateFile" data_,
        daemonLogFile = if Map.member "logFile" data_ then Just $ T.unpack $ Map.findWithDefault "" "logFile" data_ else Nothing,
        daemonLogLevel = read $ T.unpack $ Map.findWithDefault "1" "logLevel" data_,
        daemonGcInterval = if Map.member "gcInterval" data_
                           then Just $ read $ T.unpack $ Map.findWithDefault "0" "gcInterval" data_
                           else Nothing,
        daemonUser = if Map.member "user" data_ then Just $ Map.findWithDefault "" "user" data_ else Nothing,
        daemonGroup = if Map.member "group" data_ then Just $ Map.findWithDefault "" "group" data_ else Nothing,
        daemonAllowedUsers = Set.fromList $ T.splitOn "," $ Map.findWithDefault "" "allowedUsers" data_,
        daemonMaxJobs = read $ T.unpack $ Map.findWithDefault "4" "maxJobs" data_,
        daemonForeground = Map.findWithDefault "false" "foreground" data_ == "true",
        daemonTmpDir = T.unpack $ Map.findWithDefault "/tmp/ten" "tmpDir" data_
    }

-- | Read a message with timeout
readMessageWithTimeout :: Handle -> Int -> IO BS.ByteString
readMessageWithTimeout handle timeoutMicros = do
    -- Use the socket directly for more control
    result <- timeout timeoutMicros $ readMessage handle
    case result of
        Nothing -> throwIO $ DaemonError "Timeout waiting for daemon response"
        Just msg -> return msg

-- | Read a message from a handle
readMessage :: Handle -> IO BS.ByteString
readMessage handle = do
    -- Read length prefix (4 bytes)
    lenBytes <- BS.hGet handle 4
    when (BS.length lenBytes /= 4) $
        throwIO $ DaemonError "Disconnected from daemon while reading message length"

    -- Decode message length
    let len = fromIntegral $
              (fromIntegral (BS.index lenBytes 0) `shiftL` 24) .|.
              (fromIntegral (BS.index lenBytes 1) `shiftL` 16) .|.
              (fromIntegral (BS.index lenBytes 2) `shiftL` 8) .|.
              (fromIntegral (BS.index lenBytes 3))

    -- Sanity check on message length
    when (len > 100 * 1024 * 1024) $ -- 100 MB limit
        throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

    -- Read message body
    msgBytes <- BS.hGet handle len
    when (BS.length msgBytes /= len) $
        throwIO $ DaemonError "Disconnected from daemon while reading message body"

    -- Return the complete message
    return $ BS.append lenBytes msgBytes

-- | Encode a request for transmission
encodeRequest :: DaemonRequest -> BS.ByteString
encodeRequest req =
    -- Convert to core Request and use Core implementation
    let coreReq = Request {
            reqId = 0,  -- Will be set later
            reqType = requestTypeToString req,
            reqParams = requestToParams req,
            reqPayload = requestToPayload req
        }
    in Core.encodeRequest coreReq

-- | Decode a response from bytes
decodeResponse :: BS.ByteString -> Either Text DaemonResponse
decodeResponse bs =
    -- First decode to core Response
    case Core.decodeResponse bs of
        Left err -> Left err
        Right coreResp ->
            -- Then convert to domain response
            case parseResponse coreResp of
                Left err -> Left $ buildErrorToText err
                Right resp -> Right resp

-- | Bitwise operations for message length encoding/decoding
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b

-- The rest of the module implementation (buildFile, evalFile, etc.)
-- follows the same pattern as in the previous version but using
-- the local helper functions defined above

-- | Build a file using the daemon
buildFile :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError BuildResult)
buildFile conn filePath = do
    -- Check file existence
    fileExists <- doesFileExist filePath
    unless fileExists $
        return $ Left $ InputNotFound filePath

    -- Read file content
    content <- BS.readFile filePath

    -- Create build request
    let request = Protocol.BuildRequest (T.pack filePath) (Just content) defaultBuildRequestInfo

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (120 * 1000000) -- 120 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.BuildResultResponse result) ->
            return $ Right result

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build request: " <> T.pack (show resp)

-- | Evaluate a file using the daemon
evalFile :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError Derivation)
evalFile conn filePath = do
    -- Check file existence
    fileExists <- doesFileExist filePath
    unless fileExists $
        return $ Left $ InputNotFound filePath

    -- Read file content
    content <- BS.readFile filePath

    -- Create eval request
    let request = Protocol.EvalRequest (T.pack filePath) (Just content) defaultBuildRequestInfo

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.DerivationResponse derivation) ->
            return $ Right derivation

        Right (Protocol.EvalResponse derivation) ->
            return $ Right derivation

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for eval request: " <> T.pack (show resp)

-- | Build a derivation using the daemon
buildDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError BuildResult)
buildDerivation conn derivation = do
    -- Create build derivation request
    let request = Protocol.BuildDerivationRequest derivation defaultBuildRequestInfo

    -- Send request and wait for response (longer timeout for builds)
    respResult <- sendRequestSync conn request (3600 * 1000000) -- 1 hour timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.BuildResultResponse result) ->
            return $ Right result

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build derivation request: " <> T.pack (show resp)

-- | Cancel a build
cancelBuild :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError ())
cancelBuild conn buildId = do
    -- Create cancel build request
    let request = Protocol.CancelBuildRequest buildId

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.CancelBuildResponse _) ->
            return $ Right ()

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for cancel build request: " <> T.pack (show resp)

-- | Get status of a build
getBuildStatus :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError BuildStatusUpdate)
getBuildStatus conn buildId = do
    -- Create build status request
    let request = Protocol.BuildStatusRequest buildId

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.BuildStatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build status request: " <> T.pack (show resp)

-- | Get output of a build
getBuildOutput :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError Text)
getBuildOutput conn buildId = do
    -- Create build output request
    let request = Protocol.QueryBuildOutputRequest buildId

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.BuildOutputResponse output) ->
            return $ Right output

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build output request: " <> T.pack (show resp)

-- | List all builds
listBuilds :: DaemonConnection 'Builder -> Maybe Int -> IO (Either BuildError [(BuildId, BuildStatus, Float)])
listBuilds conn limit = do
    -- Create list builds request
    let request = Protocol.ListBuildsRequest limit

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.BuildListResponse builds) ->
            return $ Right builds

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list builds request: " <> T.pack (show resp)

-- | Add a file to the store (requires daemon privileges via protocol)
addFileToStore :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError StorePath)
addFileToStore conn filePath = do
    -- Check file existence
    fileExists <- doesFileExist filePath
    unless fileExists $
        return $ Left $ InputNotFound filePath

    -- Read file content
    content <- BS.readFile filePath

    -- Create store add request
    let request = Protocol.StoreAddRequest (T.pack filePath) content

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.StoreAddResponse path) ->
            return $ Right path

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store add request: " <> T.pack (show resp)

-- | Verify a store path
verifyStorePath :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError Bool)
verifyStorePath conn path = do
    -- Create verify request
    let request = Protocol.StoreVerifyRequest path

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.StoreVerifyResponse valid) ->
            return $ Right valid

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store verify request: " <> T.pack (show resp)

-- | Get store path for a file
getStorePathForFile :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError StorePath)
getStorePathForFile conn filePath = do
    -- Check file existence
    fileExists <- doesFileExist filePath
    unless fileExists $
        return $ Left $ InputNotFound filePath

    -- Read file content
    content <- BS.readFile filePath

    -- Create store path request
    let request = Protocol.StorePathRequest (T.pack filePath) content

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.StorePathResponse path) ->
            return $ Right path

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store path request: " <> T.pack (show resp)

-- | List store contents
listStore :: DaemonConnection 'Builder -> IO (Either BuildError [StorePath])
listStore conn = do
    -- Create list request
    let request = Protocol.StoreListRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.StoreListResponse paths) ->
            return $ Right paths

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store list request: " <> T.pack (show resp)

-- | Store a derivation in the daemon store (requires daemon privileges via protocol)
storeDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError StorePath)
storeDerivation conn derivation = do
    -- Serialize the derivation
    let serialized = serializeDerivation derivation

    -- Create store derivation request
    let request = Protocol.StoreDerivationRequest serialized

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.DerivationStoredResponse path) ->
            return $ Right path

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store derivation request: " <> T.pack (show resp)

-- | Retrieve a derivation from the store
retrieveDerivation :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError (Maybe Derivation))
retrieveDerivation conn path = do
    -- Create retrieve derivation request
    let request = Protocol.RetrieveDerivationRequest path

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.DerivationRetrievedResponse mDrv) ->
            return $ Right mDrv

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for retrieve derivation request: " <> T.pack (show resp)

-- | Query which derivation produced a particular output
queryDerivationForOutput :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError (Maybe Derivation))
queryDerivationForOutput conn outputPath = do
    -- Create get derivation for output request
    let request = Protocol.GetDerivationForOutputRequest (storePathToText outputPath)

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.DerivationResponse derivation) ->
            return $ Right $ Just derivation

        Right (Protocol.DerivationRetrievedResponse mDrv) ->
            return $ Right mDrv

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for query derivation for output request: " <> T.pack (show resp)

-- | Query all outputs produced by a derivation
queryOutputsForDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError (Set StorePath))
queryOutputsForDerivation conn derivation = do
    -- Create query derivation outputs request
    let request = Protocol.QueryDerivationRequest "outputs" (derivHash derivation) Nothing

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.DerivationOutputResponse outputs) ->
            return $ Right outputs

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for query outputs for derivation request: " <> T.pack (show resp)

-- | List all derivations in the store
listDerivations :: DaemonConnection 'Builder -> IO (Either BuildError [StorePath])
listDerivations conn = do
    -- Create list derivations request
    let request = Protocol.ListDerivationsRequest Nothing

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.DerivationListResponse paths) ->
            return $ Right paths

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list derivations request: " <> T.pack (show resp)

-- | Get detailed information about a derivation
getDerivationInfo :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError DerivationInfo)
getDerivationInfo conn path = do
    -- Create query derivation info request
    let request = Protocol.QueryDerivationRequest "info" (storePathToText path) (Just 1)

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.DerivationInfoResponse info) ->
            return $ Right info

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for get derivation info request: " <> T.pack (show resp)

-- | Run garbage collection (requires daemon privileges via protocol)
collectGarbage :: DaemonConnection 'Builder -> Bool -> IO (Either BuildError GCStats)
collectGarbage conn force = do
    -- Create GC request
    let request = Protocol.GCRequest force

    -- Send request and wait for response (longer timeout for GC)
    respResult <- sendRequestSync conn request (300 * 1000000) -- 5 minutes timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.GCResultResponse stats) ->
            return $ Right stats

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for GC request: " <> T.pack (show resp)

-- | Get GC status
getGCStatus :: DaemonConnection 'Builder -> Bool -> IO (Either BuildError GCStatusInfo)
getGCStatus conn forceCheck = do
    -- Create GC status request
    let request = Protocol.GCStatusRequest forceCheck

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.GCStatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for GC status request: " <> T.pack (show resp)

-- | Add a GC root (requires daemon privileges via protocol)
addGCRoot :: DaemonConnection 'Builder -> StorePath -> Text -> Bool -> IO (Either BuildError Text)
addGCRoot conn path name permanent = do
    -- Create add GC root request
    let request = Protocol.AddGCRootRequest path name permanent

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.GCRootAddedResponse rootName) ->
            return $ Right rootName

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for add GC root request: " <> T.pack (show resp)

-- | Remove a GC root (requires daemon privileges via protocol)
removeGCRoot :: DaemonConnection 'Builder -> Text -> IO (Either BuildError Text)
removeGCRoot conn name = do
    -- Create remove GC root request
    let request = Protocol.RemoveGCRootRequest name

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.GCRootRemovedResponse rootName) ->
            return $ Right rootName

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for remove GC root request: " <> T.pack (show resp)

-- | List all GC roots
listGCRoots :: DaemonConnection 'Builder -> IO (Either BuildError [(StorePath, Text, Bool)])
listGCRoots conn = do
    -- Create list GC roots request
    let request = Protocol.ListGCRootsRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.GCRootListResponse roots) ->
            return $ Right roots

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list GC roots request: " <> T.pack (show resp)

-- | Shutdown the daemon (requires daemon privileges via protocol)
shutdownDaemon :: DaemonConnection 'Builder -> IO (Either BuildError ())
shutdownDaemon conn = do
    -- Create shutdown request
    let request = Protocol.ShutdownRequest

    -- Send request and expect connection to close
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right Protocol.ShutdownResponse ->
            return $ Right ()

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for shutdown request: " <> T.pack (show resp)

-- | Get daemon status
getDaemonStatus :: DaemonConnection 'Builder -> IO (Either BuildError DaemonStatus)
getDaemonStatus conn = do
    -- Create status request
    let request = Protocol.StatusRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.StatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for status request: " <> T.pack (show resp)

-- | Get daemon configuration
getDaemonConfig :: DaemonConnection 'Builder -> IO (Either BuildError DaemonConfig)
getDaemonConfig conn = do
    -- Create config request
    let request = Protocol.ConfigRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (Protocol.ConfigResponse config) ->
            return $ Right config

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for config request: " <> T.pack (show resp)

-- | Start the daemon if it's not running
startDaemonIfNeeded :: FilePath -> IO (Either BuildError ())
startDaemonIfNeeded socketPath = try $ do
    -- Check if daemon is already running
    running <- isDaemonRunning socketPath

    unless running $ do
        -- Find the ten-daemon executable
        mDaemonPath <- findExecutable "ten-daemon"
        case mDaemonPath of
            Nothing ->
                throwIO $ DaemonError "Could not find ten-daemon executable in PATH"

            Just daemonPath -> do
                -- Start daemon in background
                (_, _, _, processHandle) <- createProcess $
                    (proc daemonPath ["start", "--socket=" ++ socketPath])
                        { std_out = CreatePipe
                        , std_err = CreatePipe
                        , detach_console = True  -- Run detached on Windows
                        }

                -- Wait for daemon to start with timeout
                let maxWaitTime = 15 -- 15 seconds

                -- Check with retries
                let checkWithRetry retries timeElapsed = do
                        if timeElapsed >= maxWaitTime
                            then throwIO $ DaemonError "Timeout waiting for daemon to start"
                            else do
                                -- Check if daemon is now running
                                running <- isDaemonRunning socketPath
                                if running
                                    then return ()
                                    else do
                                        -- Check if process has exited
                                        exitCode <- getProcessExitCode processHandle
                                        case exitCode of
                                            Just ExitSuccess ->
                                                -- Process exited successfully but daemon not running yet
                                                -- Wait and retry
                                                threadDelay 500000 -- 0.5 seconds
                                                checkWithRetry (retries-1) (timeElapsed + 0.5)

                                            Just (ExitFailure code) ->
                                                -- Process failed
                                                throwIO $ DaemonError $ "Daemon process exited with error code: " <> T.pack (show code)

                                            Nothing ->
                                                -- Process still running
                                                threadDelay 500000 -- 0.5 seconds
                                                checkWithRetry (retries-1) (timeElapsed + 0.5)

                -- Start checking
                checkWithRetry 20 0 -- 20 retries, 0 seconds elapsed

                -- Verify daemon is now running
                running <- isDaemonRunning socketPath
                unless running $
                    throwIO $ DaemonError "Daemon process started but daemon is not responding"

-- | Get process exit code (non-blocking)
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode ph = do
    -- Try to get exit code without waiting
    result <- try $ waitForProcess ph
    case result of
        Left (_ :: SomeException) -> return Nothing
        Right exitCode -> return $ Just exitCode

-- Handle type to satisfy the signature in getProcessExitCode
type ProcessHandle = System.Process.ProcessHandle
