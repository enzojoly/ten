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

import Ten.Core
import Ten.Daemon.Protocol
import Ten.Daemon.Auth (UserCredentials(..))
import Ten.Daemon.Config (getDefaultSocketPath)
import Ten.Derivation (serializeDerivation, deserializeDerivation)

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
            authVersion = currentProtocolVersion,
            authUser = username credentials,
            authToken = token credentials,
            authRequestedTier = Builder -- Always request Builder tier
        }

    -- Encode auth request
    let reqBS = serializeMessage (AuthRequestMsg authReq)

    -- Send auth request
    BS.hPut handle reqBS
    hFlush handle

    -- Read auth response
    respBS <- readMessageWithTimeout handle 5000000 -- 5 seconds timeout

    -- Parse and handle the response
    case deserializeMessage respBS of
        Left err ->
            throwIO $ AuthError $ "Authentication failed: " <> err

        Right (AuthResponseMsg (AuthAccepted userId authToken capabilities)) -> do
            -- Create daemon connection with proper privilege tier evidence
            conn <- createDaemonConnection sock handle userId authToken sBuilder

            return conn

        Right (AuthResponseMsg (AuthRejected reason)) ->
            throwIO $ AuthError $ "Authentication rejected: " <> reason

-- | Disconnect from the Ten daemon
disconnectFromDaemon :: DaemonConnection 'Builder -> IO ()
disconnectFromDaemon conn = do
    -- Use the closeDaemonConnection function from Ten.Core
    closeDaemonConnection conn

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
    reqId <- Ten.Core.sendRequest conn req
    return $ Right reqId

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection 'Builder -> Int -> Int -> IO (Either BuildError DaemonResponse)
receiveResponse conn reqId timeoutMicros = do
    -- Use the core receiveResponse function
    result <- Ten.Core.receiveResponse conn reqId timeoutMicros

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

-- | Convert request type to string
requestTypeToString :: DaemonRequest -> Text
requestTypeToString = \case
    AuthRequest {} -> "auth"
    BuildRequest {} -> "build"
    EvalRequest {} -> "eval"
    BuildDerivationRequest {} -> "build-derivation"
    BuildStatusRequest {} -> "build-status"
    CancelBuildRequest {} -> "cancel-build"
    QueryBuildOutputRequest {} -> "build-output"
    ListBuildsRequest {} -> "list-builds"
    StoreAddRequest {} -> "store-add"
    StoreVerifyRequest {} -> "store-verify"
    StorePathRequest {} -> "store-path"
    StoreListRequest -> "store-list"
    StoreDerivationRequest {} -> "store-derivation"
    RetrieveDerivationRequest {} -> "retrieve-derivation"
    QueryDerivationRequest {} -> "query-derivation"
    GetDerivationForOutputRequest {} -> "get-derivation-for-output"
    ListDerivationsRequest {} -> "list-derivations"
    GCRequest {} -> "gc"
    GCStatusRequest {} -> "gc-status"
    AddGCRootRequest {} -> "add-gc-root"
    RemoveGCRootRequest {} -> "remove-gc-root"
    ListGCRootsRequest -> "list-gc-roots"
    PingRequest -> "ping"
    ShutdownRequest -> "shutdown"
    StatusRequest -> "status"
    ConfigRequest -> "config"

-- | Convert request to parameters map
requestToParams :: DaemonRequest -> Map Text Text
requestToParams = \case
    AuthRequest version creds -> Map.fromList [
        ("version", T.pack $ show version),
        ("username", username creds),
        ("token", token creds),
        ("requestedTier", "Builder")
        ]
    BuildRequest path _ options -> Map.fromList [
        ("path", path),
        ("hasContent", if isJust _ then "true" else "false"),
        ("timeout", maybe "" (T.pack . show) (buildTimeout options)),
        ("flags", T.intercalate "," (buildFlags options))
        ]
    EvalRequest path _ options -> Map.fromList [
        ("path", path),
        ("hasContent", if isJust _ then "true" else "false"),
        ("timeout", maybe "" (T.pack . show) (buildTimeout options)),
        ("flags", T.intercalate "," (buildFlags options))
        ]
    BuildDerivationRequest _ options -> Map.fromList [
        ("hasDerivation", "true"),
        ("timeout", maybe "" (T.pack . show) (buildTimeout options)),
        ("flags", T.intercalate "," (buildFlags options))
        ]
    BuildStatusRequest buildId -> Map.fromList [
        ("buildId", T.pack $ show buildId)
        ]
    CancelBuildRequest buildId -> Map.fromList [
        ("buildId", T.pack $ show buildId)
        ]
    QueryBuildOutputRequest buildId -> Map.fromList [
        ("buildId", T.pack $ show buildId)
        ]
    ListBuildsRequest limit -> Map.fromList [
        ("limit", maybe "" (T.pack . show) limit)
        ]
    StoreAddRequest path _ -> Map.fromList [
        ("path", path),
        ("hasContent", "true")
        ]
    StoreVerifyRequest path -> Map.fromList [
        ("path", path)
        ]
    StorePathRequest path _ -> Map.fromList [
        ("path", path),
        ("hasContent", "true")
        ]
    StoreListRequest -> Map.empty
    StoreDerivationRequest {} -> Map.singleton "hasDerivation" "true"
    RetrieveDerivationRequest path -> Map.singleton "path" (storePathToText path)
    QueryDerivationRequest qType qValue qLimit -> Map.fromList [
        ("queryType", qType),
        ("queryValue", qValue),
        ("limit", maybe "" (T.pack . show) qLimit)
        ]
    GetDerivationForOutputRequest path -> Map.singleton "path" path
    ListDerivationsRequest limit -> Map.singleton "limit" (maybe "" (T.pack . show) limit)
    GCRequest force -> Map.singleton "force" (if force then "true" else "false")
    GCStatusRequest forceCheck -> Map.singleton "forceCheck" (if forceCheck then "true" else "false")
    AddGCRootRequest path name permanent -> Map.fromList [
        ("path", storePathToText path),
        ("name", name),
        ("permanent", if permanent then "true" else "false")
        ]
    RemoveGCRootRequest name -> Map.singleton "name" name
    ListGCRootsRequest -> Map.empty
    PingRequest -> Map.empty
    ShutdownRequest -> Map.empty
    StatusRequest -> Map.empty
    ConfigRequest -> Map.empty

-- | Convert request to payload
requestToPayload :: DaemonRequest -> Maybe BS.ByteString
requestToPayload = \case
    BuildRequest _ (Just content) _ -> Just content
    EvalRequest _ (Just content) _ -> Just content
    BuildDerivationRequest drv _ -> Just (serializeDerivation drv)
    StoreAddRequest _ content -> Just content
    StorePathRequest _ content -> Just content
    StoreDerivationRequest content -> Just content
    _ -> Nothing

-- | Parse a response from core Response to domain DaemonResponse
parseResponse :: Response -> Either BuildError DaemonResponse
parseResponse resp
    | respStatus resp == "error" =
        Left $ BuildError $ parseErrorType (Map.findWithDefault "unknown" "type" (respData resp)) (respMessage resp)
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
    case respType resp of
        "auth" -> Right $ AuthResponse $ parseAuthResult (respData resp) (respPayload resp)
        "build-started" -> Right $ BuildStartedResponse $ parseBuildId (Map.findWithDefault "" "buildId" (respData resp))
        "build-result" -> Right $ BuildResponse $ parseBuildResult (respData resp) (respPayload resp)
        "build-status" -> Right $ BuildStatusResponse $ parseBuildStatus (respData resp)
        "build-output" -> Right $ BuildOutputResponse $ Map.findWithDefault "" "output" (respData resp)
        "build-list" -> Right $ BuildListResponse $ parseBuildList (respData resp)
        "build-cancelled" -> Right $ CancelBuildResponse $ Map.findWithDefault "false" "success" (respData resp) == "true"
        "store-add" -> Right $ StoreAddResponse $ parseStorePath (Map.findWithDefault "" "path" (respData resp))
        "store-verify" -> Right $ StoreVerifyResponse $ Map.findWithDefault "false" "valid" (respData resp) == "true"
        "store-path" -> Right $ StorePathResponse $ parseStorePath (Map.findWithDefault "" "path" (respData resp))
        "store-list" -> Right $ StoreListResponse $ parseStorePaths (Map.findWithDefault "" "paths" (respData resp))
        "derivation" ->
            case respPayload resp of
                Just content ->
                    case deserializeDerivation content of
                        Left err -> Left $ SerializationError err
                        Right drv -> Right $ DerivationResponse drv
                Nothing -> Left $ SerializationError "Missing derivation content"
        "derivation-stored" -> Right $ DerivationStoredResponse $ parseStorePath (Map.findWithDefault "" "path" (respData resp))
        "derivation-retrieved" ->
            case respPayload resp of
                Just content ->
                    case deserializeDerivation content of
                        Left err -> Left $ SerializationError err
                        Right drv -> Right $ DerivationRetrievedResponse (Just drv)
                Nothing -> Right $ DerivationRetrievedResponse Nothing
        "derivation-query" ->
            case respPayload resp of
                Just content -> parseDerivationList content
                Nothing -> Right $ DerivationQueryResponse []
        "derivation-outputs" -> Right $ DerivationOutputResponse $ parseStorePathSet (Map.findWithDefault "" "outputs" (respData resp))
        "derivation-list" -> Right $ DerivationListResponse $ parseStorePaths (Map.findWithDefault "" "paths" (respData resp))
        "gc-result" -> Right $ GCResponse $ parseGCStats (respData resp)
        "gc-started" -> Right GCStartedResponse
        "gc-status" -> Right $ GCStatusResponse $ parseGCStatus (respData resp)
        "gc-root-added" -> Right $ GCRootAddedResponse $ Map.findWithDefault "" "name" (respData resp)
        "gc-root-removed" -> Right $ GCRootRemovedResponse $ Map.findWithDefault "" "name" (respData resp)
        "gc-roots-list" -> Right $ GCRootsListResponse $ parseGCRoots (respData resp)
        "pong" -> Right PongResponse
        "shutdown" -> Right ShutdownResponse
        "status" -> Right $ StatusResponse $ parseDaemonStatus (respData resp)
        "config" -> Right $ ConfigResponse $ parseDaemonConfig (respData resp)
        "eval" ->
            case respPayload resp of
                Just content ->
                    case deserializeDerivation content of
                        Left err -> Left $ SerializationError err
                        Right drv -> Right $ EvalResponse drv
                Nothing -> Left $ SerializationError "Missing derivation content"
        _ -> Left $ ProtocolError $ "Unknown response type: " <> respType resp

-- | Parse auth result from response data
parseAuthResult :: Map Text Text -> Maybe BS.ByteString -> AuthResult
parseAuthResult data_ _ =
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
parseCapability :: Text -> DaemonCapability
parseCapability "StoreAccess" = StoreAccess
parseCapability "SandboxCreation" = SandboxCreation
parseCapability "GarbageCollection" = GarbageCollection
parseCapability "DerivationRegistration" = DerivationRegistration
parseCapability "DerivationBuild" = DerivationBuild
parseCapability "StoreQuery" = StoreQuery
parseCapability "BuildQuery" = BuildQuery
parseCapability _ = BuildQuery  -- Default to safe capability

-- | Parse a BuildId from text
parseBuildId :: Text -> BuildId
parseBuildId str =
    case reads (T.unpack str) of
        [(n, "")] -> BuildIdFromInt n
        _ -> BuildIdFromInt 0  -- Default to zero if parsing fails

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

-- | Parse a StorePath from text
parseStorePath :: Text -> StorePath
parseStorePath text =
    case Ten.Core.parseStorePath text of
        Just path -> path
        Nothing -> StorePath "invalid" "invalid"  -- Fallback for invalid paths

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
parseDerivationList :: BS.ByteString -> Either BuildError (DaemonResponse)
parseDerivationList content =
    -- For simplicity, assume we're parsing a single derivation for now
    case deserializeDerivation content of
        Left err -> Left $ SerializationError err
        Right drv -> Right $ DerivationQueryResponse [drv]

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
parseGCStatus :: Map Text Text -> GCStatusResponse
parseGCStatus data_ =
    GCStatusResponse {
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
    -- Convert to core Request and use its serialization
    let coreReq = Request {
            reqId = 0,  -- Will be set later
            reqType = requestTypeToString req,
            reqParams = requestToParams req,
            reqPayload = requestToPayload req
        }
        encoded = Ten.Core.encodeRequest coreReq
    in encoded

-- | Decode a response from bytes
decodeResponse :: BS.ByteString -> Either Text DaemonResponse
decodeResponse bs =
    -- First decode to core Response
    case Ten.Core.decodeResponse bs of
        Left err -> Left err
        Right coreResp ->
            -- Then convert to domain response
            case parseResponse coreResp of
                Left err -> Left $ T.pack $ show err
                Right resp -> Right resp

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
    let request = BuildRequest
            { buildFilePath = T.pack filePath
            , buildFileContent = Just content
            , buildOptions = defaultBuildRequestInfo
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (120 * 1000000) -- 120 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildResponse result) ->
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
    let request = EvalRequest
            { evalFilePath = T.pack filePath
            , evalFileContent = Just content
            , evalOptions = defaultBuildRequestInfo
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationResponse derivation) ->
            return $ Right derivation

        Right (EvalResponse derivation) ->
            return $ Right derivation

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for eval request: " <> T.pack (show resp)

-- | Build a derivation using the daemon
buildDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError BuildResult)
buildDerivation conn derivation = do
    -- Create build derivation request
    let request = BuildDerivationRequest
            { buildDerivation = derivation
            , buildDerivOptions = defaultBuildRequestInfo
            }

    -- Send request and wait for response (longer timeout for builds)
    respResult <- sendRequestSync conn request (3600 * 1000000) -- 1 hour timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildResponse result) ->
            return $ Right result

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build derivation request: " <> T.pack (show resp)

-- | Cancel a build
cancelBuild :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError ())
cancelBuild conn buildId = do
    -- Create cancel build request
    let request = CancelBuildRequest
            { cancelBuildId = buildId
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right CancelBuildResponse {} ->
            return $ Right ()

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for cancel build request: " <> T.pack (show resp)

-- | Get status of a build
getBuildStatus :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError BuildStatusUpdate)
getBuildStatus conn buildId = do
    -- Create build status request
    let request = BuildStatusRequest
            { statusBuildId = buildId
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildStatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build status request: " <> T.pack (show resp)

-- | Get output of a build
getBuildOutput :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError Text)
getBuildOutput conn buildId = do
    -- Create build output request
    let request = QueryBuildOutputRequest
            { outputBuildId = buildId
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildOutputResponse output) ->
            return $ Right output

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build output request: " <> T.pack (show resp)

-- | List all builds
listBuilds :: DaemonConnection 'Builder -> Maybe Int -> IO (Either BuildError [(BuildId, BuildStatus, Float)])
listBuilds conn limit = do
    -- Create list builds request
    let request = ListBuildsRequest
            { listLimit = limit
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildListResponse builds) ->
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
    let request = StoreAddRequest
            { storeAddPath = T.pack filePath
            , storeAddContent = content
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StoreAddResponse path) ->
            return $ Right path

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store add request: " <> T.pack (show resp)

-- | Verify a store path
verifyStorePath :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError Bool)
verifyStorePath conn path = do
    -- Create verify request
    let request = StoreVerifyRequest
            { storeVerifyPath = storePathToText path
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StoreVerifyResponse valid) ->
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
    let request = StorePathRequest
            { storePathForFile = T.pack filePath
            , storePathContent = content
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StorePathResponse path) ->
            return $ Right path

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store path request: " <> T.pack (show resp)

-- | List store contents
listStore :: DaemonConnection 'Builder -> IO (Either BuildError [StorePath])
listStore conn = do
    -- Create list request
    let request = StoreListRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StoreListResponse paths) ->
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
    let request = StoreDerivationRequest
            { derivationContent = serialized
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationStoredResponse path) ->
            return $ Right path

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store derivation request: " <> T.pack (show resp)

-- | Retrieve a derivation from the store
retrieveDerivation :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError (Maybe Derivation))
retrieveDerivation conn path = do
    -- Create retrieve derivation request
    let request = RetrieveDerivationRequest
            { derivationPath = path
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationRetrievedResponse mDrv) ->
            return $ Right mDrv

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for retrieve derivation request: " <> T.pack (show resp)

-- | Query which derivation produced a particular output
queryDerivationForOutput :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError (Maybe Derivation))
queryDerivationForOutput conn outputPath = do
    -- Create get derivation for output request
    let request = GetDerivationForOutputRequest
            { getDerivationForPath = storePathToText outputPath
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationResponse derivation) ->
            return $ Right $ Just derivation

        Right (DerivationRetrievedResponse mDrv) ->
            return $ Right mDrv

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for query derivation for output request: " <> T.pack (show resp)

-- | Query all outputs produced by a derivation
queryOutputsForDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError (Set StorePath))
queryOutputsForDerivation conn derivation = do
    -- Create query derivation outputs request
    let request = QueryDerivationRequest
            { queryType = "outputs"
            , queryValue = derivHash derivation
            , queryLimit = Nothing
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationOutputResponse outputs) ->
            return $ Right outputs

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for query outputs for derivation request: " <> T.pack (show resp)

-- | List all derivations in the store
listDerivations :: DaemonConnection 'Builder -> IO (Either BuildError [StorePath])
listDerivations conn = do
    -- Create list derivations request
    let request = ListDerivationsRequest
            { listDerivLimit = Nothing
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationListResponse paths) ->
            return $ Right paths

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list derivations request: " <> T.pack (show resp)

-- | Get detailed information about a derivation
getDerivationInfo :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError DerivationInfo)
getDerivationInfo conn path = do
    -- Create query derivation info request
    let request = QueryDerivationRequest
            { queryType = "info"
            , queryValue = storePathToText path
            , queryLimit = Just 1
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationInfoResponse info) ->
            return $ Right info

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for get derivation info request: " <> T.pack (show resp)

-- | Run garbage collection (requires daemon privileges via protocol)
collectGarbage :: DaemonConnection 'Builder -> Bool -> IO (Either BuildError GCStats)
collectGarbage conn force = do
    -- Create GC request
    let request = GCRequest
            { gcForce = force
            }

    -- Send request and wait for response (longer timeout for GC)
    respResult <- sendRequestSync conn request (300 * 1000000) -- 5 minutes timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCResponse stats) ->
            return $ Right stats

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for GC request: " <> T.pack (show resp)

-- | Get GC status
getGCStatus :: DaemonConnection 'Builder -> Bool -> IO (Either BuildError GCStatusResponse)
getGCStatus conn forceCheck = do
    -- Create GC status request
    let request = GCStatusRequest
            { gcForceCheck = forceCheck
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCStatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for GC status request: " <> T.pack (show resp)

-- | Add a GC root (requires daemon privileges via protocol)
addGCRoot :: DaemonConnection 'Builder -> StorePath -> Text -> Bool -> IO (Either BuildError Text)
addGCRoot conn path name permanent = do
    -- Create add GC root request
    let request = AddGCRootRequest
            { rootPath = path
            , rootName = name
            , rootPermanent = permanent
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCRootAddedResponse rootName) ->
            return $ Right rootName

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for add GC root request: " <> T.pack (show resp)

-- | Remove a GC root (requires daemon privileges via protocol)
removeGCRoot :: DaemonConnection 'Builder -> Text -> IO (Either BuildError Text)
removeGCRoot conn name = do
    -- Create remove GC root request
    let request = RemoveGCRootRequest
            { rootNameToRemove = name
            }

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCRootRemovedResponse rootName) ->
            return $ Right rootName

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for remove GC root request: " <> T.pack (show resp)

-- | List all GC roots
listGCRoots :: DaemonConnection 'Builder -> IO (Either BuildError [(StorePath, Text, Bool)])
listGCRoots conn = do
    -- Create list GC roots request
    let request = ListGCRootsRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCRootsListResponse roots) ->
            return $ Right roots

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list GC roots request: " <> T.pack (show resp)

-- | Shutdown the daemon (requires daemon privileges via protocol)
shutdownDaemon :: DaemonConnection 'Builder -> IO (Either BuildError ())
shutdownDaemon conn = do
    -- Create shutdown request
    let request = ShutdownRequest

    -- Send request and expect connection to close
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right ShutdownResponse ->
            return $ Right ()

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for shutdown request: " <> T.pack (show resp)

-- | Get daemon status
getDaemonStatus :: DaemonConnection 'Builder -> IO (Either BuildError DaemonStatus)
getDaemonStatus conn = do
    -- Create status request
    let request = StatusRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for status request: " <> T.pack (show resp)

-- | Get daemon configuration
getDaemonConfig :: DaemonConnection 'Builder -> IO (Either BuildError DaemonConfig)
getDaemonConfig conn = do
    -- Create config request
    let request = ConfigRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (ConfigResponse config) ->
            return $ Right config

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for config request: " <> T.pack (show resp)

-- | Bitwise operations for message length encoding/decoding
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b
