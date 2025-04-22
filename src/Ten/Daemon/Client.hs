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
import Network.Socket (Socket, Family(..), SocketType(..), SockAddr(..), socket, connect, close, socketToFd, socketToHandle)
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
import Ten.Derivation (Derivation, serializeDerivation, deserializeDerivation)

-- | Connection state for daemon communication
data ConnectionState = ConnectionState {
    csSocket :: Socket,                     -- ^ Socket connected to daemon
    csHandle :: Handle,                     -- ^ Handle for socket I/O
    csUserId :: UserId,                     -- ^ Authenticated user ID
    csToken :: AuthToken,                   -- ^ Authentication token
    csRequestMap :: TVar (Map Int (MVar Response)), -- ^ Map of pending requests
    csNextReqId :: TVar Int,                -- ^ Next request ID
    csReaderThread :: ThreadId,             -- ^ Thread ID of the response reader
    csShutdown :: TVar Bool,                -- ^ Flag to indicate connection shutdown
    csLastError :: TVar (Maybe BuildError), -- ^ Last error encountered
    csCapabilities :: Set DaemonCapability, -- ^ Granted capabilities from auth
    csPrivilegeTier :: PrivilegeTier        -- ^ Always 'Builder for client
}

-- | Daemon connection type with privilege context
data DaemonConnection = DaemonConnection {
    connSocket :: Socket,
    connUserId :: UserId,
    connAuthToken :: AuthToken,
    connState :: ConnectionState,
    -- Builder privilege singleton for runtime evidence
    connPrivEvidence :: SPrivilegeTier 'Builder
}

-- | Connect to the Ten daemon - always in Builder context
connectToDaemon :: FilePath -> UserCredentials -> IO (Either BuildError DaemonConnection)
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

    -- Initialize request tracking
    requestMap <- newTVarIO Map.empty
    nextReqId <- newTVarIO 1
    shutdownFlag <- newTVarIO False
    lastError <- newTVarIO Nothing

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
            -- Set up proper handle buffering
            hSetBuffering handle (BlockBuffering Nothing)

            -- Start background thread to read responses
            readerThread <- forkIO $ responseReaderThread handle requestMap shutdownFlag lastError

            -- Create connection state
            let connState = ConnectionState {
                    csSocket = sock,
                    csHandle = handle,
                    csUserId = userId,
                    csToken = authToken,
                    csRequestMap = requestMap,
                    csNextReqId = nextReqId,
                    csReaderThread = readerThread,
                    csShutdown = shutdownFlag,
                    csLastError = lastError,
                    csCapabilities = capabilities,
                    csPrivilegeTier = Builder -- Always 'Builder for client
                }

            -- Get singleton evidence for Builder context
            let privilegeEvidence = SBuilder

            -- Return connection object with privilege evidence
            return $ DaemonConnection sock userId authToken connState privilegeEvidence

        Right (AuthResponseMsg (AuthRejected reason)) ->
            throwIO $ AuthError $ "Authentication rejected: " <> reason

-- | Disconnect from the Ten daemon
disconnectFromDaemon :: DaemonConnection -> IO ()
disconnectFromDaemon conn = do
    -- Extract connection state
    let state = connState conn

    -- Signal reader thread to shut down
    atomically $ writeTVar (csShutdown state) True

    -- Close socket handle and socket
    catch (hClose $ csHandle state) (\(_ :: IOException) -> return ())
    catch (close $ csSocket state) (\(_ :: IOException) -> return ())

    -- Kill reader thread if still running
    catch (killThread $ csReaderThread state) (\(_ :: IOException) -> return ())

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
withDaemonConnection :: FilePath -> UserCredentials -> (DaemonConnection -> IO a) -> IO (Either BuildError a)
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
sendRequest :: SPrivilegeTier 'Builder -> DaemonConnection -> DaemonRequest -> IO (Either PrivilegeError Int)
sendRequest st conn request = do
    -- Get request capabilities and privilege requirement
    let capabilities = requestCapabilities request
        privReq = requestPrivilegeRequirement request

    -- Verify capabilities against Builder tier
    case verifyCapabilities st capabilities of
        Left err -> return $ Left err
        Right () ->
            -- Check privilege requirement (Builder vs Daemon)
            case checkPrivilegeRequirement st privReq of
                Left err -> return $ Left err
                Right () -> do
                    -- Extract connection state
                    let state = connState conn

                    -- Generate request ID
                    reqId <- atomically $ do
                        rid <- readTVar (csNextReqId state)
                        writeTVar (csNextReqId state) (rid + 1)
                        return rid

                    -- Create response MVar
                    respVar <- newEmptyMVar

                    -- Register request
                    atomically $ modifyTVar' (csRequestMap state) $ Map.insert reqId respVar

                    -- Create protocol message with privilege evidence
                    let reqMsg = RequestMessage {
                            reqId = reqId,
                            reqTag = requestTypeToTag request,
                            reqPayload = Aeson.toJSON request,
                            reqCapabilities = capabilities,
                            reqPrivilege = privReq,
                            reqAuth = Just (connAuthToken conn)
                        }

                    -- Serialize message
                    let serialized = serializeMessage (RequestMsg reqMsg)

                    -- Send message
                    BS.hPut (csHandle state) serialized
                    hFlush (csHandle state)

                    -- Return request ID for tracking
                    return $ Right reqId

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection -> Int -> Int -> IO (Either BuildError Response)
receiveResponse conn reqId timeoutMicros = do
    -- Extract connection state
    let state = connState conn

    -- Get response MVar
    reqMap <- atomically $ readTVar (csRequestMap state)
    case Map.lookup reqId reqMap of
        Nothing ->
            -- No such request ID
            return $ Left $ DaemonError $ "Unknown request ID: " <> T.pack (show reqId)

        Just respVar -> do
            -- Wait for response with timeout
            result <- timeout timeoutMicros $ takeMVar respVar

            -- Clean up request map
            atomically $ modifyTVar' (csRequestMap state) $ Map.delete reqId

            -- Check for error
            lastErr <- atomically $ readTVar (csLastError state)

            -- Return response or error
            case (result, lastErr) of
                (Nothing, _) ->
                    return $ Left $ DaemonError "Timeout waiting for daemon response"
                (_, Just err) -> do
                    -- Clear the error
                    atomically $ writeTVar (csLastError state) Nothing
                    return $ Left err
                (Just resp, _) ->
                    return $ Right resp

-- | Send a request and wait for response (synchronous) with privilege checking
sendRequestSync :: DaemonConnection -> DaemonRequest -> Int -> IO (Either BuildError DaemonResponse)
sendRequestSync conn request timeoutMicros = do
    -- Send request with privilege evidence
    reqIdResult <- sendRequest (connPrivEvidence conn) conn request

    -- Process result
    case reqIdResult of
        Left err ->
            -- Return privilege error
            return $ Left $ PrivilegeError $ T.pack $ show err

        Right reqId -> do
            -- Wait for response
            respResult <- receiveResponse conn reqId timeoutMicros

            -- Process the response
            case respResult of
                Left err ->
                    return $ Left err

                Right resp ->
                    -- Convert to Response type
                    case responseToResponseData resp of
                        Left err ->
                            return $ Left $ DaemonError $ "Failed to decode response: " <> err
                        Right respData ->
                            return $ Right respData

-- | Background thread to read and dispatch responses
responseReaderThread :: Handle -> TVar (Map Int (MVar Response)) -> TVar Bool -> TVar (Maybe BuildError) -> IO ()
responseReaderThread handle requestMap shutdownFlag lastError = do
    let loop = do
            -- Check if we should shut down
            shutdown <- atomically $ readTVar shutdownFlag
            unless shutdown $ do
                -- Try to read a message with error handling
                result <- try $ readMessage handle
                case result of
                    Left (e :: SomeException) -> do
                        -- Socket error, write to lastError and exit thread
                        atomically $ writeTVar lastError $ Just $ DaemonError $ "Connection error: " <> T.pack (show e)
                        return ()

                    Right msgBS -> do
                        -- Process message if valid
                        case deserializeMessage msgBS of
                            Left err -> do
                                -- Parsing error, write to lastError and continue
                                atomically $ writeTVar lastError $ Just $ DaemonError $ "Protocol error: " <> err
                                loop

                            Right (ResponseMsg reqId resp) -> do
                                -- Look up request
                                reqMap <- atomically $ readTVar requestMap
                                case Map.lookup reqId reqMap of
                                    Nothing ->
                                        -- Unknown request ID, ignore and continue
                                        loop

                                    Just respVar -> do
                                        -- Check privilege requirements for response
                                        let privRequirement = responsePrivilegeRequirement resp

                                        -- Builder context can only receive unprivileged responses
                                        case privRequirement of
                                            PrivilegedResponse -> do
                                                -- Cannot receive privileged response in builder context
                                                atomically $ writeTVar lastError $ Just $ PrivilegeError
                                                    "Received privileged response in builder context"
                                                -- Still deliver to unblock waiting thread
                                                putMVar respVar resp
                                                loop

                                            UnprivilegedResponse -> do
                                                -- Deliver response
                                                putMVar respVar resp
                                                loop

                            Right _ ->
                                -- Other message type, ignore and continue
                                loop

    -- Start the loop and handle exceptions
    loop `catch` (\(e :: SomeException) -> do
        -- Store the error
        atomically $ writeTVar lastError $ Just $ DaemonError $ "Connection error: " <> T.pack (show e)
        -- Continue loop if the socket is still open
        unlessM (atomically $ readTVar shutdownFlag) loop)

-- | Read a message with timeout
readMessageWithTimeout :: Handle -> Int -> IO BS.ByteString
readMessageWithTimeout handle timeoutMicros = do
    result <- timeout timeoutMicros $ readMessage handle
    case result of
        Nothing -> throwIO $ DaemonError "Timeout waiting for daemon response"
        Just msg -> return msg

-- | Read a message from a handle
readMessage :: Handle -> IO BS.ByteString
readMessage handle = do
    -- Read length header (4 bytes)
    lenBytes <- BS.hGet handle 4
    when (BS.length lenBytes /= 4) $
        throwIO $ DaemonError "Disconnected from daemon while reading message length"

    -- Decode message length
    let len = fromIntegral (BS.index lenBytes 0) `shiftL` 24 .|.
              fromIntegral (BS.index lenBytes 1) `shiftL` 16 .|.
              fromIntegral (BS.index lenBytes 2) `shiftL` 8 .|.
              fromIntegral (BS.index lenBytes 3)

    -- Sanity check on message length
    when (len > 100 * 1024 * 1024) $ -- 100 MB limit
        throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

    -- Read message body
    msgBytes <- BS.hGet handle len
    when (BS.length msgBytes /= len) $
        throwIO $ DaemonError "Disconnected from daemon while reading message body"

    return msgBytes

-- | Encode a request for transmission
encodeRequest :: DaemonRequest -> BS.ByteString
encodeRequest req =
    -- Convert to Protocol.Request
    let protocolReq = Request (requestTypeToTag req) (Aeson.toJSON req)
        -- Convert to bytes (without message framing)
        bytes = LBS.toStrict $ Aeson.encode protocolReq
        -- Add length prefix
        len = fromIntegral $ BS.length bytes
        lenBytes = LBS.toStrict $ toLazyByteString $ word32BE len
    in
        -- Combine length and payload
        lenBytes <> bytes

-- | Decode a response from bytes
decodeResponse :: BS.ByteString -> Either Text DaemonResponse
decodeResponse bs = do
    -- First parse as Protocol.Response
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "JSON parse error: " <> T.pack err
        Right resp -> responseToResponseData resp

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
buildFile :: DaemonConnection -> FilePath -> IO (Either BuildError BuildResult)
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
evalFile :: DaemonConnection -> FilePath -> IO (Either BuildError Derivation)
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

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for eval request: " <> T.pack (show resp)

-- | Build a derivation using the daemon
buildDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError BuildResult)
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
cancelBuild :: DaemonConnection -> BuildId -> IO (Either BuildError ())
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

        Right CancelBuildResponse ->
            return $ Right ()

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for cancel build request: " <> T.pack (show resp)

-- | Get status of a build
getBuildStatus :: DaemonConnection -> BuildId -> IO (Either BuildError BuildStatusUpdate)
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
getBuildOutput :: DaemonConnection -> BuildId -> IO (Either BuildError Text)
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
listBuilds :: DaemonConnection -> Maybe Int -> IO (Either BuildError [(BuildId, BuildStatus, Float)])
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
addFileToStore :: DaemonConnection -> FilePath -> IO (Either BuildError StorePath)
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
verifyStorePath :: DaemonConnection -> StorePath -> IO (Either BuildError Bool)
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
getStorePathForFile :: DaemonConnection -> FilePath -> IO (Either BuildError StorePath)
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
listStore :: DaemonConnection -> IO (Either BuildError [StorePath])
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
storeDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError StorePath)
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
retrieveDerivation :: DaemonConnection -> StorePath -> IO (Either BuildError (Maybe Derivation))
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
queryDerivationForOutput :: DaemonConnection -> StorePath -> IO (Either BuildError (Maybe Derivation))
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
queryOutputsForDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError (Set StorePath))
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
listDerivations :: DaemonConnection -> IO (Either BuildError [StorePath])
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
getDerivationInfo :: DaemonConnection -> StorePath -> IO (Either BuildError DerivationInfo)
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
collectGarbage :: DaemonConnection -> Bool -> IO (Either BuildError GCStats)
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
getGCStatus :: DaemonConnection -> Bool -> IO (Either BuildError GCStatusResponse)
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
addGCRoot :: DaemonConnection -> StorePath -> Text -> Bool -> IO (Either BuildError Text)
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
removeGCRoot :: DaemonConnection -> Text -> IO (Either BuildError Text)
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
listGCRoots :: DaemonConnection -> IO (Either BuildError [(StorePath, Text, Bool)])
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
shutdownDaemon :: DaemonConnection -> IO (Either BuildError ())
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
getDaemonStatus :: DaemonConnection -> IO (Either BuildError DaemonStatus)
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
getDaemonConfig :: DaemonConnection -> IO (Either BuildError DaemonConfig)
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

-- | Helper function for control flow - unlessM
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond action = do
    result <- cond
    unless result action

-- | Map protocol request type to tag
requestTypeToTag :: DaemonRequest -> RequestTag
requestTypeToTag = \case
    BuildRequest {} -> TagBuild
    EvalRequest {} -> TagEval
    BuildDerivationRequest {} -> TagBuildDerivation
    BuildStatusRequest {} -> TagBuildStatus
    CancelBuildRequest {} -> TagCancelBuild
    QueryBuildOutputRequest {} -> TagQueryBuildOutput
    ListBuildsRequest {} -> TagListBuilds
    StoreAddRequest {} -> TagStoreAdd
    StoreVerifyRequest {} -> TagStoreVerify
    StorePathRequest {} -> TagStorePath
    StoreListRequest -> TagStoreList
    StoreDerivationRequest {} -> TagStoreDerivation
    RetrieveDerivationRequest {} -> TagRetrieveDerivation
    QueryDerivationRequest {} -> TagQueryDerivation
    GetDerivationForOutputRequest {} -> TagGetDerivationForOutput
    ListDerivationsRequest {} -> TagListDerivations
    GCRequest {} -> TagGC
    GCStatusRequest {} -> TagGCStatus
    AddGCRootRequest {} -> TagAddGCRoot
    RemoveGCRootRequest {} -> TagRemoveGCRoot
    ListGCRootsRequest -> TagListGCRoots
    PingRequest -> TagPing
    ShutdownRequest -> TagShutdown
    StatusRequest -> TagStatus
    ConfigRequest -> TagConfig

-- | Check if a response requires privileged context
responsePrivilegeRequirement :: Response -> ResponsePrivilege
responsePrivilegeRequirement = \case
    -- Responses containing privileged information
    StoreAddResponse {} -> PrivilegedResponse
    GCResponse {} -> PrivilegedResponse
    DerivationStoredResponse {} -> PrivilegedResponse
    GCRootAddedResponse {} -> PrivilegedResponse
    GCRootRemovedResponse {} -> PrivilegedResponse

    -- Responses that can be received in any context
    BuildResponse {} -> UnprivilegedResponse
    BuildStatusResponse {} -> UnprivilegedResponse
    StoreVerifyResponse {} -> UnprivilegedResponse
    StoreListResponse {} -> UnprivilegedResponse
    DerivationResponse {} -> UnprivilegedResponse
    DerivationRetrievedResponse {} -> UnprivilegedResponse
    BuildOutputResponse {} -> UnprivilegedResponse
    BuildListResponse {} -> UnprivilegedResponse
    DerivationOutputResponse {} -> UnprivilegedResponse
    DerivationListResponse {} -> UnprivilegedResponse
    GCStatusResponse {} -> UnprivilegedResponse
    GCRootsListResponse {} -> UnprivilegedResponse
    CancelBuildResponse {} -> UnprivilegedResponse
    StatusResponse {} -> UnprivilegedResponse
    ConfigResponse {} -> UnprivilegedResponse
    ShutdownResponse -> UnprivilegedResponse
    PongResponse -> UnprivilegedResponse

    -- By default, treat as unprivileged
    _ -> UnprivilegedResponse

-- | Required capabilities for requests
requestCapabilities :: DaemonRequest -> Set DaemonCapability
requestCapabilities = \case
    -- Store operations
    StoreAddRequest {} -> Set.singleton StoreAccess
    StoreVerifyRequest {} -> Set.singleton StoreQuery
    StorePathRequest {} -> Set.singleton StoreQuery
    StoreListRequest -> Set.singleton StoreQuery

    -- Build operations
    BuildRequest {} -> Set.singleton DerivationBuild
    EvalRequest {} -> Set.singleton DerivationBuild
    BuildDerivationRequest {} -> Set.singleton DerivationBuild
    BuildStatusRequest {} -> Set.singleton BuildQuery
    CancelBuildRequest {} -> Set.singleton BuildQuery
    QueryBuildOutputRequest {} -> Set.singleton BuildQuery
    ListBuildsRequest {} -> Set.singleton BuildQuery

    -- GC operations
    GCRequest {} -> Set.singleton GarbageCollection
    GCStatusRequest {} -> Set.singleton BuildQuery
    AddGCRootRequest {} -> Set.singleton GarbageCollection
    RemoveGCRootRequest {} -> Set.singleton GarbageCollection
    ListGCRootsRequest -> Set.singleton BuildQuery

    -- Derivation operations
    StoreDerivationRequest {} -> Set.fromList [DerivationRegistration, StoreAccess]
    RetrieveDerivationRequest {} -> Set.singleton StoreQuery
    QueryDerivationRequest {} -> Set.singleton StoreQuery
    GetDerivationForOutputRequest {} -> Set.singleton StoreQuery
    ListDerivationsRequest {} -> Set.singleton StoreQuery

    -- Administrative operations
    StatusRequest -> Set.singleton BuildQuery
    ConfigRequest -> Set.singleton BuildQuery
    ShutdownRequest -> Set.singleton GarbageCollection
    PingRequest -> Set.singleton BuildQuery

-- | Privilege requirement for requests
requestPrivilegeRequirement :: DaemonRequest -> RequestPrivilege
requestPrivilegeRequirement = \case
    -- Operations requiring daemon privileges
    StoreAddRequest {} -> PrivilegedRequest
    StoreDerivationRequest {} -> PrivilegedRequest
    GCRequest {} -> PrivilegedRequest
    AddGCRootRequest {} -> PrivilegedRequest
    RemoveGCRootRequest {} -> PrivilegedRequest
    ShutdownRequest -> PrivilegedRequest

    -- Operations that can be done from either context
    BuildRequest {} -> UnprivilegedRequest
    EvalRequest {} -> UnprivilegedRequest
    BuildDerivationRequest {} -> UnprivilegedRequest
    BuildStatusRequest {} -> UnprivilegedRequest
    CancelBuildRequest {} -> UnprivilegedRequest
    QueryBuildOutputRequest {} -> UnprivilegedRequest
    ListBuildsRequest {} -> UnprivilegedRequest
    StoreVerifyRequest {} -> UnprivilegedRequest
    StorePathRequest {} -> UnprivilegedRequest
    StoreListRequest -> UnprivilegedRequest
    RetrieveDerivationRequest {} -> UnprivilegedRequest
    QueryDerivationRequest {} -> UnprivilegedRequest
    GetDerivationForOutputRequest {} -> UnprivilegedRequest
    ListDerivationsRequest {} -> UnprivilegedRequest
    GCStatusRequest {} -> UnprivilegedRequest
    ListGCRootsRequest -> UnprivilegedRequest
    StatusRequest -> UnprivilegedRequest
    ConfigRequest -> UnprivilegedRequest
    PingRequest -> UnprivilegedRequest

-- | Bitwise operations for message length encoding/decoding
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b
