{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Ten.Daemon.Client (
    -- Socket management
    connectToDaemon,
    disconnectFromDaemon,
    getDefaultSocketPath,
    withDaemonConnection,

    -- Basic client communication
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

    -- Store operations
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

    -- GC operations
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

    -- Internal utilities exposed for testing
    createSocketAndConnect,
    readResponseWithTimeout,
    encodeRequest,
    decodeResponse
) where

import Control.Concurrent (forkIO, ThreadId, threadDelay, myThreadId, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, newMVar, readMVar)
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
    csRequestMap :: IORef (Map Int (MVar Response)), -- ^ Map of pending requests
    csNextReqId :: IORef Int,               -- ^ Next request ID
    csReaderThread :: ThreadId,             -- ^ Thread ID of the response reader
    csShutdown :: IORef Bool,               -- ^ Flag to indicate connection shutdown
    csLastError :: IORef (Maybe BuildError) -- ^ Last error encountered
}

-- | Daemon connection type
data DaemonConnection = DaemonConnection {
    connSocket :: Socket,
    connUserId :: UserId,
    connAuthToken :: AuthToken,
    connState :: ConnectionState
}

-- | Connect to the Ten daemon
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
    requestMap <- newIORef Map.empty
    nextReqId <- newIORef 1
    shutdownFlag <- newIORef False
    lastError <- newIORef Nothing

    -- Authenticate with the daemon
    let authReq = AuthRequest {
            authVersion = currentProtocolVersion,
            authUser = username credentials,
            authToken = token credentials
        }

    -- Encode auth request
    let reqBS = serializeMessage (AuthRequestMsgWrapper (AuthRequestMsg authReq))

    -- Send auth request
    BS.hPut handle reqBS
    hFlush handle

    -- Read auth response
    respBS <- readMessageWithTimeout handle 5000000 -- 5 seconds timeout

    -- Parse and handle the response
    case deserializeMessage respBS of
        Left err -> do
            hClose handle
            close sock
            throwIO $ AuthError $ "Authentication failed: " <> err

        Right (AuthResponseMsgWrapper (AuthResponseMsg (AuthAccepted userId authToken))) -> do
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
                    csLastError = lastError
                }

            -- Return connection object
            return $ DaemonConnection sock userId authToken connState

        Right (AuthResponseMsgWrapper (AuthResponseMsg (AuthRejected reason))) -> do
            hClose handle
            close sock
            throwIO $ AuthError $ "Authentication rejected: " <> reason

        Right (AuthResponseMsgWrapper (AuthResponseMsg (AuthSuccess userId authToken))) -> do
            -- For backward compatibility with older daemons
            hSetBuffering handle (BlockBuffering Nothing)

            readerThread <- forkIO $ responseReaderThread handle requestMap shutdownFlag lastError

            let connState = ConnectionState {
                    csSocket = sock,
                    csHandle = handle,
                    csUserId = userId,
                    csToken = authToken,
                    csRequestMap = requestMap,
                    csNextReqId = nextReqId,
                    csReaderThread = readerThread,
                    csShutdown = shutdownFlag,
                    csLastError = lastError
                }

            return $ DaemonConnection sock userId authToken connState

        Right _ -> do
            hClose handle
            close sock
            throwIO $ DaemonError "Invalid authentication response from daemon"

-- | Disconnect from the Ten daemon
disconnectFromDaemon :: DaemonConnection -> IO ()
disconnectFromDaemon conn = do
    -- Extract connection state
    let state = connState conn

    -- Signal reader thread to shut down
    writeIORef (csShutdown state) True

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

-- | Send a request to the daemon
sendRequest :: DaemonConnection -> DaemonRequest -> IO Int
sendRequest conn request = do
    -- Extract connection state
    let state = connState conn

    -- Generate request ID
    reqId <- atomicModifyIORef' (csNextReqId state) (\id -> (id + 1, id))

    -- Create response MVar
    respVar <- newEmptyMVar

    -- Register request
    atomicModifyIORef' (csRequestMap state) (\m -> (Map.insert reqId respVar m, ()))

    -- Convert to Protocol.Request type
    let req = Request (requestTypeToTag request) (Aeson.toJSON request)

    -- Create protocol message
    let msg = RequestMsgWrapper (RequestMsg reqId req)

    -- Serialize message
    let serialized = serializeMessage msg

    -- Send message
    BS.hPut (csHandle state) serialized
    hFlush (csHandle state)

    -- Return request ID for tracking
    return reqId

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection -> Int -> Int -> IO (Either BuildError Response)
receiveResponse conn reqId timeoutMicros = do
    -- Extract connection state
    let state = connState conn

    -- Get response MVar
    respVarMap <- readIORef (csRequestMap state)
    case Map.lookup reqId respVarMap of
        Nothing ->
            -- No such request ID
            return $ Left $ DaemonError $ "Unknown request ID: " <> T.pack (show reqId)

        Just respVar -> do
            -- Wait for response with timeout
            result <- timeout timeoutMicros $ takeMVar respVar

            -- Clean up request map
            atomicModifyIORef' (csRequestMap state) (\m -> (Map.delete reqId m, ()))

            -- Check for error
            lastErr <- readIORef (csLastError state)

            -- Return response or error
            case (result, lastErr) of
                (Nothing, _) ->
                    return $ Left $ DaemonError "Timeout waiting for daemon response"
                (_, Just err) -> do
                    -- Clear the error
                    writeIORef (csLastError state) Nothing
                    return $ Left err
                (Just resp, _) ->
                    return $ Right resp

-- | Send a request and wait for response (synchronous)
sendRequestSync :: DaemonConnection -> DaemonRequest -> Int -> IO (Either BuildError DaemonResponse)
sendRequestSync conn request timeoutMicros = do
    -- Send request and get ID
    reqId <- sendRequest conn request

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
responseReaderThread :: Handle -> IORef (Map Int (MVar Response)) -> IORef Bool -> IORef (Maybe BuildError) -> IO ()
responseReaderThread handle requestMap shutdownFlag lastError = do
    let loop = do
            -- Check if we should shut down
            shutdown <- readIORef shutdownFlag
            unless shutdown $ do
                -- Try to read a message with error handling
                result <- try $ readMessage handle
                case result of
                    Left (e :: SomeException) -> do
                        -- Socket error, write to lastError and exit thread
                        writeIORef lastError $ Just $ DaemonError $ "Connection error: " <> T.pack (show e)
                        return ()

                    Right msgBS -> do
                        -- Process message if valid
                        case deserializeMessage msgBS of
                            Left err -> do
                                -- Parsing error, write to lastError and continue
                                writeIORef lastError $ Just $ DaemonError $ "Protocol error: " <> err
                                loop

                            Right (ResponseMsgWrapper (ResponseMsg reqId resp)) -> do
                                -- Look up request
                                reqMap <- readIORef requestMap
                                case Map.lookup reqId reqMap of
                                    Nothing ->
                                        -- Unknown request ID, ignore and continue
                                        loop

                                    Just respVar -> do
                                        -- Deliver response
                                        putMVar respVar resp
                                        loop

                            Right _ ->
                                -- Other message type, ignore and continue
                                loop

    -- Start the loop and handle exceptions
    loop `catch` (\(e :: SomeException) -> do
        -- Store the error
        writeIORef lastError $ Just $ DaemonError $ "Connection error: " <> T.pack (show e)
        -- Continue loop if the socket is still open
        unlessM (readIORef shutdownFlag) loop)

-- | Read a message from a handle with timeout
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

        Right ShutdownResponse ->
            -- For backward compatibility with older daemons
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

-- | Add a file to the store
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

-- | Store a derivation in the daemon store
storeDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError StorePath)
storeDerivation conn derivation = do
    -- Serialize the derivation
    let serialized = serializeDerivation derivation

    -- Create store derivation request
    let request = StoreDerivationCmd StoreDerivationRequest
            { derivationContent = serialized
            , registerOutputs = True
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
    let request = RetrieveDerivationCmd RetrieveDerivationRequest
            { derivationPath = path
            , includeOutputs = True
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
    let request = QueryDerivationCmd QueryDerivationRequest
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
getDerivationInfo :: DaemonConnection -> StorePath -> IO (Either BuildError DerivationInfoResponse)
getDerivationInfo conn path = do
    -- Create query derivation info request
    let request = QueryDerivationCmd QueryDerivationRequest
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

-- | Run garbage collection
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
getGCStatus :: DaemonConnection -> Bool -> IO (Either BuildError GCStatusResponseData)
getGCStatus conn forceCheck = do
    -- Create GC status request
    let request = GCStatusCmd GCStatusRequestData
            { forceCheck = forceCheck
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

-- | Add a GC root
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

-- | Remove a GC root
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

-- | Shutdown the daemon
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

-- | Map protocol request type to tag
requestTypeToTag :: DaemonRequest -> RequestTag
requestTypeToTag (AuthCmd _ _) = TagAuth
requestTypeToTag (BuildRequest _ _ _) = TagBuild
requestTypeToTag (EvalRequest _ _ _) = TagEval
requestTypeToTag (BuildDerivationRequest _ _) = TagBuildDerivation
requestTypeToTag (BuildStatusRequest _) = TagBuildStatus
requestTypeToTag (CancelBuildRequest _) = TagCancelBuild
requestTypeToTag (QueryBuildOutputRequest _) = TagQueryBuildOutput
requestTypeToTag (ListBuildsRequest _) = TagListBuilds
requestTypeToTag (StoreAddRequest _ _) = TagStoreAdd
requestTypeToTag (StoreVerifyRequest _) = TagStoreVerify
requestTypeToTag (StorePathRequest _ _) = TagStorePath
requestTypeToTag StoreListRequest = TagStoreList
requestTypeToTag (StoreDerivationCmd _) = TagStoreDerivation
requestTypeToTag (RetrieveDerivationCmd _) = TagRetrieveDerivation
requestTypeToTag (QueryDerivationCmd _) = TagQueryDerivation
requestTypeToTag (GetDerivationForOutputRequest _) = TagGetDerivationForOutput
requestTypeToTag (ListDerivationsRequest _) = TagListDerivations
requestTypeToTag (GCRequest _) = TagGC
requestTypeToTag (GCStatusCmd _) = TagGCStatus
requestTypeToTag (AddGCRootRequest _ _ _) = TagAddGCRoot
requestTypeToTag (RemoveGCRootRequest _) = TagRemoveGCRoot
requestTypeToTag ListGCRootsRequest = TagListGCRoots
requestTypeToTag PingRequest = TagPing
requestTypeToTag ShutdownRequest = TagShutdown
requestTypeToTag StatusRequest = TagStatus
requestTypeToTag ConfigRequest = TagConfig

-- | Bitwise operations for message length encoding/decoding
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b

-- | Helper function for control flow - unlessM
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM cond action = do
    result <- cond
    unless result action
