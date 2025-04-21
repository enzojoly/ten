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

    -- Derivation operations
    storeDerivation,
    retrieveDerivation,
    queryDerivationForOutput,
    queryOutputsForDerivation,
    listDerivations,
    getDerivationInfo,

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
import System.Process (createProcess, proc, waitForProcess, StdStream(..), CreateProcess(..))
import System.Timeout (timeout)

import Ten.Core
import Ten.Daemon.Protocol
import Ten.Daemon.Auth (authenticateUser)
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
connectToDaemon :: FilePath -> UserCredentials -> IO DaemonConnection
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
    let authReq = AuthRequest {
            version = currentProtocolVersion,
            authUser = username credentials,
            authToken = token credentials
        }

    -- Send auth request
    BS.hPut handle $ serializeMessage $ AuthRequestMsg authReq
    hFlush handle

    -- Read auth response
    respBS <- readMessageWithTimeout handle 5000000 -- 5 seconds timeout
    case deserializeMessage respBS of
        Left err -> do
            hClose handle
            close sock
            throwIO $ AuthError $ "Authentication failed: " <> err

        Right (AuthResponseMsg (AuthAccepted userId authToken)) -> do
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

        Right (AuthResponseMsg (AuthRejected reason)) -> do
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
withDaemonConnection :: FilePath -> UserCredentials -> (DaemonConnection -> IO a) -> IO a
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
sendRequest :: DaemonConnection -> Request -> IO Int
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
    let msg = RequestMsg reqId request

    -- Send message
    let serialized = serializeMessage msg
    BS.hPut (csHandle connState) serialized
    hFlush (csHandle connState)

    -- Return request ID for tracking
    return reqId

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection -> Int -> Int -> IO (Maybe Response)
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
sendRequestSync :: DaemonConnection -> Request -> Int -> IO (Either BuildError Response)
sendRequestSync conn request timeoutMicros = do
    -- Send request
    reqId <- sendRequest conn request

    -- Wait for response
    result <- receiveResponse conn reqId timeoutMicros
    case result of
        Nothing ->
            return $ Left $ DaemonError "Timeout waiting for daemon response"

        Just (ErrorResponse err) ->
            return $ Left err

        Just response ->
            return $ Right response

-- | Background thread to read and dispatch responses
responseReaderThread :: Handle -> IORef (Map Int (MVar Response)) -> IORef Bool -> IO ()
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
                        case deserializeMessage msgBS of
                            Left _ ->
                                -- Parsing error, continue loop
                                loop

                            Right (ResponseMsg reqId response) -> do
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
    when (len > 100 * 1024 * 1024) $ -- 100 MB limit
        throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

    -- Read message body
    msgBytes <- BS.hGet handle len
    when (BS.length msgBytes /= len) $
        throwIO $ DaemonError "Disconnected from daemon while reading message body"

    return msgBytes

-- | Encode a request for transmission
encodeRequest :: Request -> BS.ByteString
encodeRequest req = serializeRequest req

-- | Decode a response from bytes
decodeResponse :: BS.ByteString -> Either Text Response
decodeResponse bs = deserializeResponse bs

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
    let request = BuildFileRequest {
            filePath = T.pack file,
            fileContent = content,
            buildOptions = defaultBuildRequestInfo
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout
    case response of
        Right (BuildResponse result) ->
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
    let request = EvalFileRequest {
            evalFilePath = T.pack file,
            evalContent = content,
            evalOptions = defaultBuildRequestInfo
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout
    case response of
        Right (EvalResponse derivation) ->
            return $ Right derivation

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for eval request"

        Left err ->
            return $ Left err

-- | Build a derivation using the daemon
buildDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError BuildResult)
buildDerivation conn derivation = do
    -- Create build derivation request
    let request = BuildDerivationRequest {
            buildDerivation = derivation,
            buildDerivOptions = defaultBuildRequestInfo
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (120 * 1000000) -- 120 seconds timeout
    case response of
        Right (BuildResponse result) ->
            return $ Right result

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for build derivation request"

        Left err ->
            return $ Left err

-- | Cancel a build
cancelBuild :: DaemonConnection -> BuildId -> IO (Either BuildError ())
cancelBuild conn buildId = do
    -- Create cancel build request
    let request = CancelBuildRequest {
            cancelBuildId = buildId
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (CancelBuildResponse success) ->
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
    let request = BuildStatusRequest {
            statusBuildId = buildId
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (BuildStatusResponse status) ->
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
    let request = StoreAddRequest {
            storeAddPath = T.pack file,
            storeAddContent = content
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout
    case response of
        Right (StoreAddResponse path) ->
            return $ Right path

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store add request"

        Left err ->
            return $ Left err

-- | Verify a store path
verifyStorePath :: DaemonConnection -> StorePath -> IO (Either BuildError Bool)
verifyStorePath conn path = do
    -- Create verify request
    let request = StoreVerifyRequest {
            storeVerifyPath = storePathToText path
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (StoreVerifyResponse isValid) ->
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
    let request = StorePathRequest {
            storePathForFile = T.pack file,
            storePathContent = content
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (StorePathResponse path) ->
            return $ Right path

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store path request"

        Left err ->
            return $ Left err

-- | List store contents
listStore :: DaemonConnection -> IO (Either BuildError [StorePath])
listStore conn = do
    -- Create list request
    let request = StoreListRequest

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (StoreListResponse paths) ->
            return $ Right paths

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store list request"

        Left err ->
            return $ Left err

-- | Store a derivation in the store and database
storeDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError StorePath)
storeDerivation conn derivation = do
    -- Serialize the derivation
    let serialized = serializeDerivation derivation

    -- Create store derivation request
    let request = StoreDerivationCmd StoreDerivationRequest {
            derivationContent = serialized,
            registerOutputs = True
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout
    case response of
        Right (DerivationStoredResponse path) ->
            return $ Right path

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for store derivation request"

        Left err ->
            return $ Left err

-- | Retrieve a derivation from the store by path
retrieveDerivation :: DaemonConnection -> StorePath -> IO (Either BuildError (Maybe Derivation))
retrieveDerivation conn path = do
    -- Create retrieve derivation request
    let request = RetrieveDerivationCmd RetrieveDerivationRequest {
            derivationPath = path,
            includeOutputs = True
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (DerivationRetrievedResponse Nothing) ->
            return $ Right Nothing

        Right (DerivationRetrievedResponse (Just derivation)) ->
            return $ Right $ Just derivation

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for retrieve derivation request"

        Left err ->
            return $ Left err

-- | Query which derivation produced a particular output
queryDerivationForOutput :: DaemonConnection -> StorePath -> IO (Either BuildError (Maybe Derivation))
queryDerivationForOutput conn outputPath = do
    -- Create query derivation request
    let request = QueryDerivationCmd QueryDerivationRequest {
            queryType = "by-output",
            queryValue = storePathToText outputPath,
            queryLimit = Just 1
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (DerivationQueryResponse []) ->
            return $ Right Nothing

        Right (DerivationQueryResponse (drv:_)) ->
            return $ Right $ Just drv

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for query derivation request"

        Left err ->
            return $ Left err

-- | Query all outputs produced by a derivation
queryOutputsForDerivation :: DaemonConnection -> Derivation -> IO (Either BuildError (Set StorePath))
queryOutputsForDerivation conn derivation = do
    -- Create query outputs request
    let request = GetDerivationForOutputRequest {
            getDerivationForPath = derivHash derivation
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (DerivationOutputResponse outputs) ->
            return $ Right outputs

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for query derivation outputs request"

        Left err ->
            return $ Left err

-- | List all derivations in the store
listDerivations :: DaemonConnection -> IO (Either BuildError [StorePath])
listDerivations conn = do
    -- Create list derivations request
    let request = ListDerivationsRequest {
            listDerivLimit = Nothing
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout
    case response of
        Right (DerivationListResponse paths) ->
            return $ Right paths

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for list derivations request"

        Left err ->
            return $ Left err

-- | Get detailed information about a derivation
getDerivationInfo :: DaemonConnection -> StorePath -> IO (Either BuildError DerivationInfoResponse)
getDerivationInfo conn path = do
    -- Create get derivation info request
    let request = QueryDerivationCmd QueryDerivationRequest {
            queryType = "info",
            queryValue = storePathToText path,
            queryLimit = Just 1
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout
    case response of
        Right (DerivationInfoResponse info) ->
            return $ Right info

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for get derivation info request"

        Left err ->
            return $ Left err

-- | Run garbage collection
collectGarbage :: DaemonConnection -> Bool -> IO (Either BuildError GCStats)
collectGarbage conn force = do
    -- Create GC request
    let request = GCRequest {
            gcForce = force
        }

    -- Send request and wait for response
    response <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout
    case response of
        Right (GCResponse stats) ->
            return $ Right stats

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for GC request"

        Left err ->
            return $ Left err

-- | Shutdown the daemon
shutdownDaemon :: DaemonConnection -> IO (Either BuildError ())
shutdownDaemon conn = do
    -- Create shutdown request
    let request = ShutdownRequest

    -- Send request and expect connection to close
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right ShutdownResponse ->
            return $ Right ()

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for shutdown request"

        Left err ->
            return $ Left err

-- | Get daemon status
getDaemonStatus :: DaemonConnection -> IO (Either BuildError DaemonStatus)
getDaemonStatus conn = do
    -- Create status request
    let request = StatusRequest

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (StatusResponse status) ->
            return $ Right status

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for status request"

        Left err ->
            return $ Left err

-- | Get daemon configuration
getDaemonConfig :: DaemonConnection -> IO (Either BuildError DaemonConfig)
getDaemonConfig conn = do
    -- Create config request
    let request = ConfigRequest

    -- Send request and wait for response
    response <- sendRequestSync conn request (5 * 1000000) -- 5 seconds timeout
    case response of
        Right (ConfigResponse config) ->
            return $ Right config

        Right _ ->
            return $ Left $ DaemonError "Invalid response type for config request"

        Left err ->
            return $ Left err

-- | Bitwise operations for message length encoding/decoding
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b
