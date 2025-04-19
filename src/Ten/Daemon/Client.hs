{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

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

    -- Store operations
    addFileToStore,
    verifyStorePath,
    getStorePathForFile,
    listStore,

    -- GC operations
    collectGarbage,

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
import Control.Concurrent.STM (atomically, TVar, newTVarIO, readTVar, writeTVar, TQueue, newTQueueIO, writeTQueue, readTQueue)
import Control.Exception (catch, finally, bracketOnError, bracket, throwIO, SomeException, try)
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
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory, getXdgDirectory, XdgDirectory(..))
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), hClose, hFlush, hPutStrLn, stderr, hPutStr, BufferMode(..), hSetBuffering)
import System.Process (createProcess, proc, waitForProcess, CreateProcess(..), StdStream(..))
import System.Timeout (timeout)

import Ten.Core hiding (UserCredentials) -- Import Core, but not UserCredentials
import qualified Ten.Daemon.Protocol as Protocol -- Import Protocol qualified
import Ten.Daemon.Auth (authenticateUser) -- Import Auth functions directly
import Ten.Daemon.Config (getDefaultSocketPath)

-- | Re-export UserCredentials from Protocol
type UserCredentials = Protocol.UserCredentials

-- | Connection state for daemon communication
data ConnectionState = ConnectionState {
    csSocket :: Socket,                     -- ^ Socket connected to daemon
    csHandle :: Handle,                     -- ^ Handle for socket I/O
    csUserId :: UserId,                     -- ^ Authenticated user ID
    csToken :: AuthToken,                   -- ^ Authentication token
    csRequestMap :: IORef (Map Int (MVar Protocol.Response)), -- ^ Map of pending requests
    csNextReqId :: IORef Int,               -- ^ Next request ID
    csReaderThread :: ThreadId,             -- ^ Thread ID of the response reader
    csShutdown :: IORef Bool                -- ^ Flag to indicate connection shutdown
}

-- | Daemon connection type
data DaemonConnection = DaemonConnection {
    connSocket :: Socket,
    connUserId :: UserId,
    connAuthToken :: AuthToken,
    connState :: ConnectionState
}

-- | Connect to the Ten daemon
connectToDaemon :: FilePath -> Protocol.UserCredentials -> IO DaemonConnection
connectToDaemon socketPath credentials = do
    -- Check if daemon is running
    running <- isDaemonRunning socketPath

    -- If not running, try to start it if autostart is enabled
    unless running $ do
        startDaemonIfNeeded socketPath
        -- Brief pause to allow daemon to initialize
        threadDelay 500000 -- 0.5 seconds

    -- Create socket and connect
    (sock, handle) <- createSocketAndConnect socketPath

    -- Initialize request tracking
    requestMap <- newIORef Map.empty
    nextReqId <- newIORef 1
    shutdownFlag <- newIORef False

    -- Authenticate with the daemon
    let authReq = Protocol.AuthRequest {
            Protocol.version = Protocol.currentProtocolVersion,
            Protocol.credentials = credentials
        }

    -- Send auth request
    BS.hPut handle $ Protocol.serializeMessage $ Protocol.AuthRequestMsg authReq
    hFlush handle

    -- Read auth response
    respBS <- readMessageWithTimeout handle 5000000 -- 5 seconds timeout
    case Protocol.deserializeMessage respBS of
        Left err -> do
            hClose handle
            close sock
            throwIO $ AuthError $ "Authentication failed: " <> err

        Right (Protocol.AuthResponseMsg (Protocol.AuthAccepted userId authToken)) -> do
            -- Set up handle buffering
            hSetBuffering handle NoBuffering

            -- Start background thread to read responses
            readerThread <- forkIO $ responseReaderThread handle requestMap shutdownFlag

            -- Create connection state
            let connState = ConnectionState {
                    csSocket = sock,
                    csHandle = handle,
                    csUserId = userId,
                    csToken = authToken,
                    csRequestMap = requestMap,
                    csNextReqId = nextReqId,
                    csReaderThread = readerThread,
                    csShutdown = shutdownFlag
                }

            -- Return connection object
            return $ DaemonConnection sock userId authToken connState

        Right (Protocol.AuthResponseMsg (Protocol.AuthRejected reason)) -> do
            hClose handle
            close sock
            throwIO $ AuthError $ "Authentication failed: " <> reason

        Right _ -> do
            hClose handle
            close sock
            throwIO $ DaemonError "Invalid authentication response from daemon"

-- | Disconnect from the Ten daemon
disconnectFromDaemon :: DaemonConnection -> IO ()
disconnectFromDaemon conn = do
    -- Extract connection state
    let connState = connState conn

    -- Signal reader thread to shut down
    writeIORef (csShutdown connState) True

    -- Give reader thread time to clean up
    threadDelay 100000 -- 0.1 seconds

    -- Close the socket handle and socket
    hClose $ csHandle connState
    close $ csSocket connState

    -- Kill reader thread if still running
    killThread $ csReaderThread connState

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
withDaemonConnection :: FilePath -> Protocol.UserCredentials -> (DaemonConnection -> IO a) -> IO a
withDaemonConnection socketPath credentials action =
    bracket
        (connectToDaemon socketPath credentials)
        disconnectFromDaemon
        action

-- | Create a socket and connect to the daemon
createSocketAndConnect :: FilePath -> IO (Socket, Handle)
createSocketAndConnect socketPath = do
    -- Ensure parent directory exists
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Create socket
    sock <- socket AF_UNIX Stream 0

    -- Connect to socket (with error handling)
    bracketOnError
        (return sock)
        close
        (\s -> do
            connect s (SockAddrUnix socketPath)
            handle <- socketToHandle s ReadWriteMode
            return (s, handle)
        )

-- | Send a request to the daemon
sendRequest :: DaemonConnection -> Protocol.Request -> IO Int
sendRequest conn request = do
    -- Extract connection state
    let connState = connState conn

    -- Generate request ID
    reqId <- atomicModifyIORef' (csNextReqId connState) (\id -> (id + 1, id))

    -- Create response MVar
    respVar <- newEmptyMVar

    -- Register request
    atomicModifyIORef' (csRequestMap connState) (\m -> (Map.insert reqId respVar m, ()))

    -- Create message with request ID
    let msg = Protocol.RequestMsg reqId request

    -- Send message
    let serialized = Protocol.serializeMessage msg
    BS.hPut (csHandle connState) serialized
    hFlush (csHandle connState)

    -- Return request ID for tracking
    return reqId

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection -> Int -> Int -> IO (Maybe Protocol.Response)
receiveResponse conn reqId timeoutMicros = do
    -- Extract connection state
    let connState = connState conn

    -- Get response MVar
    respVarMap <- readIORef (csRequestMap connState)
    case Map.lookup reqId respVarMap of
        Nothing ->
            -- No such request ID
            return Nothing

        Just respVar -> do
            -- Wait for response with timeout
            result <- timeout timeoutMicros $ takeMVar respVar

            -- Clean up request map
            atomicModifyIORef' (csRequestMap connState) (\m -> (Map.delete reqId m, ()))

            -- Return response or Nothing on timeout
            return result

-- | Send a request and wait for response (synchronous)
sendRequestSync :: DaemonConnection -> Protocol.Request -> Int -> IO (Either BuildError Protocol.Response)
sendRequestSync conn request timeoutMicros = do
    -- Send request
    reqId <- sendRequest conn request

    -- Wait for response
    result <- receiveResponse conn reqId timeoutMicros
    case result of
        Nothing ->
            return $ Left $ DaemonError "Timeout waiting for daemon response"

        Just (Protocol.ErrorResponse err) ->
            return $ Left err

        Just response ->
            return $ Right response

-- | Background thread to read and dispatch responses
responseReaderThread :: Handle -> IORef (Map Int (MVar Protocol.Response)) -> IORef Bool -> IO ()
responseReaderThread handle requestMap shutdownFlag = do
    let loop = do
            -- Check if we should shut down
            shutdown <- readIORef shutdownFlag
            unless shutdown $ do
                -- Try to read a message (with error handling)
                result <- try $ readMessage handle
                case result of
                    Left (_ :: SomeException) -> do
                        -- Socket error, exit thread
                        return ()

                    Right msgBS -> do
                        -- Process message if valid
                        case Protocol.deserializeMessage msgBS of
                            Left _ ->
                                -- Parsing error, continue loop
                                loop

                            Right (Protocol.ResponseMsg reqId response) -> do
                                -- Look up request
                                reqMap <- readIORef requestMap
                                case Map.lookup reqId reqMap of
                                    Nothing ->
                                        -- Unknown request ID, ignore
                                        loop

                                    Just respVar -> do
                                        -- Deliver response
                                        putMVar respVar response
                                        loop

                            Right _ ->
                                -- Other message type, ignore
                                loop

    -- Start the loop
    loop `catch` (\(_ :: SomeException) -> return ())

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
    when (len > 100 * 1024 * 1024) $ -- 100 MB max
        throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

    -- Read message body
    msgBytes <- BS.hGet handle len
    when (BS.length msgBytes /= len) $
        throwIO $ DaemonError "Disconnected from daemon while reading message body"

    return msgBytes

-- | Encode a request for transmission
encodeRequest :: Protocol.Request -> BS.ByteString
encodeRequest req = Protocol.serializeRequest req

-- | Decode a response from bytes
decodeResponse :: BS.ByteString -> Either Text Protocol.Response
decodeResponse bs =
    case Protocol.deserializeResponse bs of
        Left err -> Left err
        Right resp -> Right resp

-- | Start the daemon if it's not running
startDaemonIfNeeded :: FilePath -> IO ()
startDaemonIfNeeded socketPath = do
    -- Check if daemon is already running
    running <- isDaemonRunning socketPath

    unless running $ do
        -- Find the ten-daemon executable
        tenDaemonPath <- findExecutable "ten-daemon"
        case tenDaemonPath of
            Nothing ->
                throwIO $ DaemonError "Could not find ten-daemon executable in PATH"

            Just daemonPath -> do
                -- Start daemon in background
                (_, _, _, processHandle) <- createProcess $
                    (proc daemonPath ["start", "--socket=" ++ socketPath])
                        { std_out = CreatePipe,
                          std_err = CreatePipe }

                -- Wait for daemon to start
                exitCode <- waitForProcess processHandle
                case exitCode of
                    ExitSuccess ->
                        -- Check that daemon is now running
                        do
                            -- Retry a few times with delay
                            let checkWithRetry retries = do
                                    when (retries > 0) $ do
                                        running <- isDaemonRunning socketPath
                                        unless running $ do
                                            threadDelay 500000 -- 0.5 seconds
                                            checkWithRetry (retries - 1)

                            checkWithRetry 10
                            running <- isDaemonRunning socketPath
                            unless running $
                                throwIO $ DaemonError "Daemon started but is not responding"

                    _ ->
                        throwIO $ DaemonError "Failed to start daemon"

-- | Find an executable in the PATH
findExecutable :: String -> IO (Maybe FilePath)
findExecutable name = do
    -- Get PATH environment variable
    pathEnv <- lookupEnv "PATH"
    case pathEnv of
        Nothing -> return Nothing
        Just path -> do
            -- Split PATH into components
            let pathComponents = splitPath path

            -- Try each component
            findInPaths pathComponents
  where
    splitPath path = case break (== ':') path of
        (p, "") -> [p]
        (p, _:rest) -> p : splitPath rest

    findInPaths [] = return Nothing
    findInPaths (p:ps) = do
        let fullPath = p </> name
        exists <- doesFileExist fullPath
        if exists
            then return $ Just fullPath
            else findInPaths ps

-- | Build a file using the daemon
buildFile :: DaemonConnection -> FilePath -> IO (Either BuildError BuildResult)
buildFile conn file = do
    -- Read file content
    content <- BS.readFile file

    -- Create build request
    let request = Protocol.BuildFileRequest {
            Protocol.filePath = T.pack file,
            Protocol.fileContent = content,
            Protocol.buildOptions = Protocol.defaultBuildOptions
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout
    case response of
        Right (Protocol.BuildResponse result) ->
            return $ Right result

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for build request"

        Left err ->
            return $ Left err

-- | Evaluate a file using the daemon
evalFile :: DaemonConnection -> FilePath -> IO (Either BuildError Derivation)
evalFile conn file = do
    -- Read file content
    content <- BS.readFile file

    -- Create eval request
    let request = Protocol.EvalFileRequest {
            Protocol.evalFilePath = T.pack file,
            Protocol.evalContent = content,
            Protocol.evalOptions = Protocol.defaultEvalOptions
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout
    case response of
        Right (Protocol.EvalResponse derivation) ->
            return $ Right derivation

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for eval request"

        Left err ->
            return $ Left err

-- | Build a derivation using the daemon
buildDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError BuildResult)
buildDerivation conn derivation = do
    -- Create build derivation request
    let request = Protocol.BuildDerivationRequest {
            Protocol.derivation = derivation,
            Protocol.buildOptions = Protocol.defaultBuildOptions
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (120 * 1000000) -- 120 seconds timeout
    case response of
        Right (Protocol.BuildResponse result) ->
            return $ Right result

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for build derivation request"

        Left err ->
            return $ Left err

-- | Cancel a build
cancelBuild :: DaemonConnection -> BuildId -> IO (Either BuildError ())
cancelBuild conn buildId = do
    -- Create cancel build request
    let request = Protocol.CancelBuildRequest {
            Protocol.buildId = buildId
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (Protocol.CancelBuildResponse success) ->
            if success
                then return $ Right ()
                else return $ Left $ BuildFailed "Failed to cancel build"

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for cancel build request"

        Left err ->
            return $ Left err

-- | Get status of a build
getBuildStatus :: DaemonConnection -> BuildId -> IO (Either BuildError BuildStatus)
getBuildStatus conn buildId = do
    -- Create build status request
    let request = Protocol.BuildStatusRequest {
            Protocol.statusBuildId = buildId
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (Protocol.BuildStatusResponse status) ->
            return $ Right status

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for build status request"

        Left err ->
            return $ Left err

-- | Add a file to the store
addFileToStore :: DaemonConnection -> FilePath -> IO (Either BuildError StorePath)
addFileToStore conn file = do
    -- Read file content
    content <- BS.readFile file

    -- Create store add request
    let request = Protocol.StoreAddRequest {
            Protocol.name = T.pack (takeFileName file),
            Protocol.content = content
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout
    case response of
        Right (Protocol.StoreAddResponse path) ->
            return $ Right path

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store add request"

        Left err ->
            return $ Left err

-- | Verify a store path
verifyStorePath :: DaemonConnection -> FilePath -> IO (Either BuildError Bool)
verifyStorePath conn path = do
    -- Create verify request
    let request = Protocol.StoreVerifyRequest {
            Protocol.storePath = T.pack path
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (Protocol.StoreVerifyResponse isValid) ->
            return $ Right isValid

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store verify request"

        Left err ->
            return $ Left err

-- | Get store path for a file
getStorePathForFile :: DaemonConnection -> FilePath -> IO (Either BuildError StorePath)
getStorePathForFile conn file = do
    -- Read file content
    content <- BS.readFile file

    -- Create path request
    let request = Protocol.StorePathRequest {
            Protocol.filePath = T.pack file,
            Protocol.content = content
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (Protocol.StorePathResponse path) ->
            return $ Right path

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store path request"

        Left err ->
            return $ Left err

-- | List store contents
listStore :: DaemonConnection -> IO (Either BuildError [StorePath])
listStore conn = do
    -- Create list request
    let request = Protocol.StoreListRequest

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (Protocol.StoreListResponse paths) ->
            return $ Right paths

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store list request"

        Left err ->
            return $ Left err

-- | Run garbage collection
collectGarbage :: DaemonConnection -> Bool -> IO (Either BuildError GCStats)
collectGarbage conn force = do
    -- Create GC request
    let request = Protocol.GCRequest {
            Protocol.force = force
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout
    case response of
        Right (Protocol.GCResponse stats) ->
            return $ Right stats

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for GC request"

        Left err ->
            return $ Left err

-- | Shutdown the daemon
shutdownDaemon :: DaemonConnection -> IO (Either BuildError ())
shutdownDaemon conn = do
    -- Create shutdown request
    let request = Protocol.ShutdownRequest

    -- Send request and expect connection to close
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right Protocol.ShutdownResponse ->
            return $ Right ()

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for shutdown request"

        Left err ->
            return $ Left err

-- | Get daemon status
getDaemonStatus :: DaemonConnection -> IO (Either BuildError Protocol.DaemonStatus)
getDaemonStatus conn = do
    -- Create status request
    let request = Protocol.StatusRequest

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (Protocol.StatusResponse status) ->
            return $ Right status

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for status request"

        Left err ->
            return $ Left err

-- | Get daemon configuration
getDaemonConfig :: DaemonConnection -> IO (Either BuildError Protocol.DaemonConfig)
getDaemonConfig conn = do
    -- Create config request
    let request = Protocol.ConfigRequest

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (Protocol.ConfigResponse config) ->
            return $ Right config

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for config request"

        Left err ->
            return $ Left err

-- | Bitwise operations for message length encoding/decoding
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
(.|.) = (Prelude.+)
