{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Ten.Daemon.Server (
    -- Server initialization and management
    startServer,
    stopServer,

    -- Connection handling
    acceptClients,
    handleClient,

    -- Request processing
    processRequest,
    dispatchRequest,

    -- Authentication
    authenticateClient,

    -- Specialized request handlers
    handleBuildRequest,
    handleEvalRequest,
    handleBuildDerivationRequest,
    handleCancelBuildRequest,
    handleBuildStatusRequest,
    handleStoreRequest,
    handleGCRequest,
    handleStatusRequest,
    handleConfigRequest,
    handleShutdownRequest,

    -- Socket management
    createServerSocket,
    closeServerSocket,

    -- Client management
    ClientInfo(..),
    ClientState(..),
    ActiveClients,
    addClient,
    removeClient,
    broadcastToClients,

    -- Utilities for testing
    decodeRequestMessage,
    encodeResponseMessage
) where

import Control.Concurrent (ThreadId, forkIO, forkFinally, myThreadId, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, try, catch, SomeException, throwIO, AsyncException(..))
import Control.Monad (forever, void, when, unless, forM_)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString, word32BE)
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust, isNothing, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Word (Word32)
import Network.Socket
import Network.Socket.ByteString (sendAll, recv)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory)
import System.IO (Handle, IOMode(..), hClose, hFlush, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)
import System.Exit (ExitCode(..))
import System.Process (createProcess, proc, waitForProcess)

import Ten.Core
import Ten.Daemon.Protocol
import Ten.Daemon.State
import Ten.Daemon.Auth
import Ten.Store (makeStorePath)
import Ten.Derivation (mkDerivation, hashDerivation)
import Ten.Build (buildDerivation, buildApplicativeStrategy, buildMonadicStrategy)

-- | Information about a connected client
data ClientInfo = ClientInfo {
    ciThreadId :: ThreadId,          -- ^ Thread ID handling the client
    ciSocket :: Socket,              -- ^ Client socket
    ciHandle :: Handle,              -- ^ Socket handle for I/O
    ciUserId :: UserId,              -- ^ Authenticated user ID
    ciAuthToken :: AuthToken,        -- ^ Authentication token
    ciConnectTime :: UTCTime,        -- ^ When the client connected
    ciLastActivity :: TVar UTCTime,  -- ^ Last client activity time
    ciState :: TVar ClientState      -- ^ Current client state
}

-- | Client state tracking
data ClientState =
    Authenticating |                 -- ^ Client is in authentication process
    Active |                         -- ^ Client is actively connected
    ShuttingDown                     -- ^ Client is being disconnected
    deriving (Eq, Show)

-- | Collection of active client connections
type ActiveClients = TVar (Map ThreadId ClientInfo)

-- | Server socket and control information
data ServerControl = ServerControl {
    scSocket :: Socket,              -- ^ Server listening socket
    scThread :: ThreadId,            -- ^ Main server thread
    scClients :: ActiveClients,      -- ^ Connected clients
    scShutdown :: TVar Bool,         -- ^ Shutdown flag
    scState :: DaemonState           -- ^ Daemon state
}

-- | Start the server
startServer :: FilePath -> DaemonState -> IO ServerControl
startServer socketPath state = do
    -- Create socket directory if it doesn't exist
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Clean up any existing socket file
    removeSocketIfExists socketPath

    -- Create and bind the server socket
    serverSocket <- createServerSocket socketPath

    -- Initialize client tracking
    clients <- newTVarIO Map.empty
    shutdownFlag <- newTVarIO False

    -- Start the accept thread
    serverThread <- forkIO $ acceptClients serverSocket clients shutdownFlag state

    -- Return the server control structure
    return ServerControl {
        scSocket = serverSocket,
        scThread = serverThread,
        scClients = clients,
        scShutdown = shutdownFlag,
        scState = state
    }

-- | Stop the server
stopServer :: ServerControl -> IO ()
stopServer ServerControl{..} = do
    -- Set shutdown flag
    atomically $ writeTVar scShutdown True

    -- Wait a moment for the flag to be noticed
    threadDelay 100000 -- 0.1 seconds

    -- Get all client info
    clientMap <- atomically $ readTVar scClients

    -- Close all client connections
    forM_ (Map.elems clientMap) $ \ClientInfo{..} -> do
        atomically $ writeTVar ciState ShuttingDown
        hClose ciHandle
        close ciSocket

    -- Close server socket
    close scSocket

    -- Kill server thread if still running
    killThread scThread

-- | Main loop to accept client connections
acceptClients :: Socket -> ActiveClients -> TVar Bool -> DaemonState -> IO ()
acceptClients serverSocket clients shutdownFlag state = do
    let acceptLoop = do
            -- Check if we should shut down
            shouldShutdown <- atomically $ readTVar shutdownFlag
            unless shouldShutdown $ do
                -- Wait for a client connection (with timeout)
                mClient <- waitForClientConnection serverSocket 1000000 -- 1 second timeout

                case mClient of
                    Nothing ->
                        -- Timeout, continue accepting
                        acceptLoop

                    Just (clientSocket, clientAddr) -> do
                        -- Convert to handle for convenience
                        clientHandle <- socketToHandle clientSocket ReadWriteMode

                        -- Initialize client state
                        now <- getCurrentTime
                        lastActivityVar <- newTVarIO now
                        clientStateVar <- newTVarIO Authenticating

                        -- Start client handler thread
                        clientThread <- forkFinally
                            (handleClient clientSocket clientHandle clients state lastActivityVar clientStateVar)
                            (\_ -> do
                                -- Clean up when client thread exits
                                hClose clientHandle
                                close clientSocket
                                tid <- myThreadId
                                atomically $ modifyTVar clients (Map.delete tid)
                            )

                        -- Store client info (without user ID yet)
                        let clientInfo = ClientInfo {
                                ciThreadId = clientThread,
                                ciSocket = clientSocket,
                                ciHandle = clientHandle,
                                ciUserId = UserId "unauthenticated",
                                ciAuthToken = AuthToken "",
                                ciConnectTime = now,
                                ciLastActivity = lastActivityVar,
                                ciState = clientStateVar
                            }

                        -- Add to client map
                        atomically $ modifyTVar clients $ Map.insert clientThread clientInfo

                        -- Continue accepting
                        acceptLoop

    -- Start the accept loop
    acceptLoop `catch` \(e :: SomeException) -> do
        -- Log error and continue if not intentional shutdown
        case fromException e of
            Just ThreadKilled -> return ()  -- Normal shutdown
            _ -> do
                hPutStrLn stderr $ "Error in accept loop: " ++ show e
                -- Try to restart accept loop unless we're shutting down
                shouldShutdown <- atomically $ readTVar shutdownFlag
                unless shouldShutdown $ acceptClients serverSocket clients shutdownFlag state

-- | Wait for a client connection with timeout
waitForClientConnection :: Socket -> Int -> IO (Maybe (Socket, SockAddr))
waitForClientConnection serverSocket timeoutMicros = do
    -- Set up file descriptor for select
    fdSet <- setFdSet serverSocket

    -- Wait for activity with timeout
    (hasActivity, _, _) <- select [serverSocket] [] [] (fromIntegral timeoutMicros `div` 1000000)

    if not hasActivity
        then return Nothing  -- Timeout
        else do
            -- Accept the connection
            result <- try $ accept serverSocket
            case result of
                Left (_ :: SomeException) -> return Nothing
                Right clientConn -> return $ Just clientConn

-- | Handle a client connection
handleClient :: Socket -> Handle -> ActiveClients -> DaemonState -> TVar UTCTime -> TVar ClientState -> IO ()
handleClient clientSocket clientHandle clients state lastActivityVar clientStateVar = do
    -- First, authenticate the client
    authResult <- authenticateClient clientHandle
    case authResult of
        Left errorMsg -> do
            -- Authentication failed, send error and close
            sendAuthFailure clientHandle errorMsg
            return ()

        Right (userId, authToken) -> do
            -- Update client info with authenticated user
            tid <- myThreadId
            updateClientAuth tid userId authToken

            -- Set client state to active
            atomically $ writeTVar clientStateVar Active

            -- Handle client requests
            handleClientRequests clientSocket clientHandle state lastActivityVar clientStateVar
  where
    -- Update client info after authentication
    updateClientAuth tid userId authToken = atomically $ do
        clientMap <- readTVar clients
        case Map.lookup tid clientMap of
            Nothing -> return ()  -- Client was removed
            Just clientInfo -> do
                let updatedInfo = clientInfo {
                        ciUserId = userId,
                        ciAuthToken = authToken
                    }
                writeTVar clients (Map.insert tid updatedInfo clientMap)

-- | Process client requests
handleClientRequests :: Socket -> Handle -> DaemonState -> TVar UTCTime -> TVar ClientState -> IO ()
handleClientRequests clientSocket clientHandle state lastActivityVar clientStateVar = do
    let processLoop = do
            -- Check if client is shutting down
            clientState <- atomically $ readTVar clientStateVar
            when (clientState == Active) $ do
                -- Try to read a message
                msgResult <- try $ readMessageWithTimeout clientHandle 30000000 -- 30 seconds timeout

                case msgResult of
                    Left (_ :: SomeException) ->
                        -- Read error, client disconnected or timeout
                        return ()

                    Right msgBytes -> do
                        -- Update last activity time
                        now <- getCurrentTime
                        atomically $ writeTVar lastActivityVar now

                        -- Process the message
                        case decodeRequestMessage msgBytes of
                            Nothing ->
                                -- Invalid message format, continue
                                processLoop

                            Just (RequestMsg reqId request) -> do
                                -- Process the request
                                response <- processRequest request state

                                -- Send the response
                                sendResponse clientHandle reqId response

                                -- Continue processing
                                processLoop

    -- Start processing loop
    processLoop `catch` \(e :: SomeException) -> do
        -- Log error unless it's a normal disconnect
        case fromException e of
            Just ThreadKilled -> return ()  -- Normal shutdown
            _ -> hPutStrLn stderr $ "Error handling client: " ++ show e

-- | Authenticate a client
authenticateClient :: Handle -> IO (Either Text (UserId, AuthToken))
authenticateClient handle = do
    -- Wait for auth message
    msgBytes <- readMessageWithTimeout handle 5000000 -- 5 seconds timeout

    -- Decode auth request
    case decodeMessage msgBytes of
        Just (AuthRequestMsg authReq) -> do
            -- Validate credentials
            case validateCredentials (authUser authReq) (authToken authReq) of
                Left err ->
                    return $ Left err

                Right (userId, token) -> do
                    -- Send success response
                    let response = AuthSuccess userId token
                    BS.hPut handle $ encodeMessage $ AuthResponseMsg response
                    hFlush handle

                    return $ Right (userId, token)

        _ ->
            return $ Left "Invalid authentication message"

-- | Send authentication failure response
sendAuthFailure :: Handle -> Text -> IO ()
sendAuthFailure handle reason = do
    let response = AuthFailure reason
    BS.hPut handle $ encodeMessage $ AuthResponseMsg response
    hFlush handle

-- | Send a response to a request
sendResponse :: Handle -> RequestId -> Response -> IO ()
sendResponse handle reqId response = do
    let msg = ResponseMsg reqId response
    BS.hPut handle $ encodeMessage msg
    hFlush handle

-- | Process a client request
processRequest :: Request -> DaemonState -> IO Response
processRequest request state =
    -- Dispatch to the appropriate handler based on request type
    dispatchRequest request state `catch` \(e :: SomeException) -> do
        -- Convert any errors to ErrorResponse
        case fromException e of
            Just ThreadKilled -> throwIO ThreadKilled  -- Re-throw ThreadKilled
            _ -> return $ ErrorResponse $ DaemonError $ "Request processing error: " <> T.pack (show e)

-- | Dispatch a request to the appropriate handler
dispatchRequest :: Request -> DaemonState -> IO Response
dispatchRequest request state = case request of
    BuildRequest{..} ->
        handleBuildRequest buildFilePath buildFileContent buildOptions state

    EvalRequest{..} ->
        handleEvalRequest evalFilePath evalFileContent evalOptions state

    BuildDerivationRequest{..} ->
        handleBuildDerivationRequest buildDerivation buildDerivOptions state

    BuildStatusRequest{..} ->
        handleBuildStatusRequest statusBuildId state

    CancelBuildRequest{..} ->
        handleCancelBuildRequest cancelBuildId state

    StoreAddRequest{..} ->
        handleStoreRequest (StoreAddCmd storeAddPath storeAddContent) state

    StoreVerifyRequest{..} ->
        handleStoreRequest (StoreVerifyCmd storeVerifyPath) state

    StorePathRequest{..} ->
        handleStoreRequest (StorePathCmd storePathForFile storePathContent) state

    StoreListRequest ->
        handleStoreRequest StoreListCmd state

    GCRequest{..} ->
        handleGCRequest gcForce state

    StatusRequest ->
        handleStatusRequest state

    ConfigRequest ->
        handleConfigRequest state

    ShutdownRequest ->
        handleShutdownRequest state

-- | Handle a build request
handleBuildRequest :: Text -> Maybe BS.ByteString -> BuildOptions -> DaemonState -> IO Response
handleBuildRequest filePath maybeContent options state = do
    -- Get the file content
    content <- case maybeContent of
        Just c -> return c
        Nothing -> do
            -- Try to read from filesystem if not provided
            let path = T.unpack filePath
            fileExists <- doesFileExist path
            if fileExists
                then BS.readFile path
                else return $ ErrorResponse $ InputNotFound path

    -- Create or get the build environment
    env <- getBuildEnv state

    -- Determine file type and build
    let ext = takeExtension $ T.unpack filePath
    result <- case ext of
        ".ten" -> buildTenFile env content options
        ".drv" -> buildDrvFile env content options
        _ -> do
            -- Try to auto-detect
            if isJsonContent content
                then buildTenFile env content options
                else buildDrvFile env content options

    -- Convert result to response
    case result of
        Left err -> return $ ErrorResponse err
        Right buildResult -> return $ BuildResponse buildResult

-- | Handle an evaluation request
handleEvalRequest :: Text -> Maybe BS.ByteString -> EvalOptions -> DaemonState -> IO Response
handleEvalRequest filePath maybeContent options state = do
    -- Get the file content
    content <- case maybeContent of
        Just c -> return c
        Nothing -> do
            -- Try to read from filesystem if not provided
            let path = T.unpack filePath
            fileExists <- doesFileExist path
            if fileExists
                then BS.readFile path
                else return $ ErrorResponse $ InputNotFound path

    -- Create evaluation environment
    env <- getEvalEnv state options

    -- Evaluate the content
    result <- evaluateContent env content options

    -- Convert result to response
    case result of
        Left err -> return $ ErrorResponse err
        Right derivation -> return $ EvalResponse derivation

-- | Handle a build derivation request
handleBuildDerivationRequest :: Derivation -> BuildOptions -> DaemonState -> IO Response
handleBuildDerivationRequest drv options state = do
    -- Create or get the build environment
    env <- getBuildEnv state

    -- Register the build in the daemon state
    buildId <- registerBuild state drv

    -- Create a build function that updates state as it progresses
    let buildWithProgress = do
            -- Update build status to running
            atomically $ updateBuildStatus state buildId (BuildRunning 0.0)

            -- Build the derivation
            result <- buildTen (buildDerivation drv) env

            -- Update final status
            case result of
                Left err -> do
                    atomically $ updateBuildStatus state buildId BuildFailed'
                    return $ Left err
                Right (buildResult, _) -> do
                    atomically $ updateBuildStatus state buildId BuildCompleted
                    return $ Right buildResult

    -- Start the build in a separate thread if async, or run immediately if sync
    if buildAsync options
        then do
            -- Start async build
            void $ forkIO $ do
                result <- buildWithProgress
                atomically $ storeBuildResult state buildId result

            -- Return immediate response with build ID
            return $ BuildStartedResponse buildId
        else do
            -- Run build synchronously
            result <- buildWithProgress

            -- Store result
            atomically $ storeBuildResult state buildId result

            -- Return result directly
            case result of
                Left err -> return $ ErrorResponse err
                Right buildResult -> return $ BuildResponse buildResult

-- | Handle a build status request
handleBuildStatusRequest :: BuildId -> DaemonState -> IO Response
handleBuildStatusRequest buildId state = do
    -- Get build status from state
    status <- atomically $ getBuildStatus state buildId

    -- Return status response
    return $ BuildStatusResponse status

-- | Handle a cancel build request
handleCancelBuildRequest :: BuildId -> DaemonState -> IO Response
handleCancelBuildRequest buildId state = do
    -- Try to cancel the build
    success <- cancelBuild state buildId

    -- Return result
    return $ CancelBuildResponse success

-- | Handle a store request
handleStoreRequest :: StoreCommand -> DaemonState -> IO Response
handleStoreRequest cmd state = case cmd of
    StoreAddCmd path content -> do
        -- Add file to store
        env <- getBuildEnv state
        result <- buildTen (addToStore (T.pack $ takeFileName $ T.unpack path) content) env
        case result of
            Left err -> return $ ErrorResponse err
            Right (storePath, _) -> return $ StoreAddResponse storePath

    StoreVerifyCmd path -> do
        -- Verify store path
        env <- getBuildEnv state
        let storePath = parseStorePath $ T.unpack path
        case storePath of
            Nothing -> return $ ErrorResponse $ StoreError "Invalid store path format"
            Just sp -> do
                result <- runTen (verifyStorePath sp) env (initBuildState Build)
                case result of
                    Left err -> return $ ErrorResponse err
                    Right (isValid, _) -> return $ StoreVerifyResponse isValid

    StorePathCmd file content -> do
        -- Get store path for file
        let name = T.pack $ takeFileName $ T.unpack file
        let hash = showHash $ hashByteString content
        let path = makeStorePath hash name
        return $ StorePathResponse path

    StoreListCmd -> do
        -- List store contents
        env <- getBuildEnv state
        paths <- getStorePaths env
        return $ StoreListResponse paths

-- | Handle a garbage collection request
handleGCRequest :: Bool -> DaemonState -> IO Response
handleGCRequest force state = do
    -- Check if we can run GC
    canGC <- atomically $ checkGCLock state

    if not canGC && not force
        then return $ ErrorResponse $ ResourceError "Garbage collection already in progress"
        else do
            -- Acquire GC lock
            atomically $ acquireGCLock state

            -- Run GC in a separate thread if async
            void $ forkIO $ do
                env <- getBuildEnv state
                result <- runTen collectGarbage env (initBuildState Build)
                -- Release GC lock when done
                atomically $ releaseGCLock state

            -- Return immediate response
            return $ GCResponse GCStats {
                gcTotal = 0,
                gcLive = 0,
                gcCollected = 0,
                gcBytes = 0,
                gcElapsedTime = 0
            }

-- | Handle a status request
handleStatusRequest :: DaemonState -> IO Response
handleStatusRequest state = do
    -- Get daemon status
    buildCount <- atomically $ countActiveBuilds state
    rootCount <- atomically $ countGCRoots state
    uptime <- getDaemonUptime state

    -- Create status response
    let status = DaemonStatus {
            daemonStatus = "running",
            daemonUptime = uptime,
            daemonActiveBuilds = buildCount,
            daemonCompletedBuilds = 0, -- Would track this in a real implementation
            daemonFailedBuilds = 0,
            daemonGcRoots = rootCount,
            daemonStoreSize = 0, -- Would calculate in a real implementation
            daemonStorePaths = 0
        }

    return $ StatusResponse status

-- | Handle a configuration request
handleConfigRequest :: DaemonState -> IO Response
handleConfigRequest state = do
    -- Get daemon configuration
    config <- getDaemonConfig state

    -- Return config response
    return $ ConfigResponse config

-- | Handle a shutdown request
handleShutdownRequest :: DaemonState -> IO Response
handleShutdownRequest state = do
    -- Signal shutdown (in a real implementation)
    atomically $ signalShutdown state

    -- Return success
    return ShutdownResponse

-- | Create a server socket
createServerSocket :: FilePath -> IO Socket
createServerSocket socketPath = do
    -- Create socket
    sock <- socket AF_UNIX Stream 0

    -- Bind to path
    bind sock (SockAddrUnix socketPath)

    -- Set permissions on socket file (666)
    setFileMode socketPath 0o666

    -- Listen for connections
    listen sock 5

    return sock

-- | Close a server socket
closeServerSocket :: Socket -> FilePath -> IO ()
closeServerSocket sock socketPath = do
    -- Close the socket
    close sock

    -- Remove the socket file
    removeSocketIfExists socketPath

-- | Remove socket file if it exists
removeSocketIfExists :: FilePath -> IO ()
removeSocketIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path

-- | Add a client to the active clients map
addClient :: ActiveClients -> ClientInfo -> IO ()
addClient clients clientInfo =
    atomically $ modifyTVar clients $ Map.insert (ciThreadId clientInfo) clientInfo

-- | Remove a client from the active clients map
removeClient :: ActiveClients -> ThreadId -> IO ()
removeClient clients tid =
    atomically $ modifyTVar clients $ Map.delete tid

-- | Broadcast a message to all clients
broadcastToClients :: ActiveClients -> BS.ByteString -> IO ()
broadcastToClients clients msg = do
    clientMap <- atomically $ readTVar clients
    forM_ (Map.elems clientMap) $ \ClientInfo{..} -> do
        -- Only send to active clients
        state <- atomically $ readTVar ciState
        when (state == Active) $ do
            -- Try to send, ignore errors
            void $ try $ BS.hPut ciHandle msg >> hFlush ciHandle

-- | Read a message with timeout
readMessageWithTimeout :: Handle -> Int -> IO BS.ByteString
readMessageWithTimeout handle timeoutMicros = do
    -- Try to read length header with timeout
    headerResult <- timeout timeoutMicros $ BS.hGet handle 4
    case headerResult of
        Nothing -> throwIO $ DaemonError "Timeout waiting for message header"
        Just headerBytes ->
            if BS.length headerBytes /= 4
                then throwIO $ DaemonError "Disconnected while reading message header"
                else do
                    -- Decode message length
                    let len = fromIntegral (BS.index headerBytes 0) `shiftL` 24 .|.
                              fromIntegral (BS.index headerBytes 1) `shiftL` 16 .|.
                              fromIntegral (BS.index headerBytes 2) `shiftL` 8 .|.
                              fromIntegral (BS.index headerBytes 3)

                    -- Sanity check on message length
                    when (len > 100 * 1024 * 1024) $ -- 100 MB max
                        throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

                    -- Try to read message body with timeout
                    bodyResult <- timeout timeoutMicros $ BS.hGet handle len
                    case bodyResult of
                        Nothing -> throwIO $ DaemonError "Timeout waiting for message body"
                        Just bodyBytes ->
                            if BS.length bodyBytes /= len
                                then throwIO $ DaemonError "Disconnected while reading message body"
                                else return bodyBytes

-- | Decode a request message
decodeRequestMessage :: BS.ByteString -> Maybe (RequestMsg)
decodeRequestMessage bs = Aeson.decode $ LBS.fromStrict bs

-- | Encode a response message
encodeResponseMessage :: ResponseMsg -> BS.ByteString
encodeResponseMessage msg =
    let msgBS = LBS.toStrict $ Aeson.encode msg
        len = fromIntegral $ BS.length msgBS
        header = LBS.toStrict $ toLazyByteString $ word32BE len
    in header <> msgBS

-- | Encode a message with length prefix
encodeMessage :: Message -> BS.ByteString
encodeMessage msg =
    let msgBS = LBS.toStrict $ Aeson.encode msg
        len = fromIntegral $ BS.length msgBS
        header = LBS.toStrict $ toLazyByteString $ word32BE len
    in header <> msgBS

-- | Decode a message
decodeMessage :: BS.ByteString -> Maybe Message
decodeMessage bs = Aeson.decode $ LBS.fromStrict bs

-- | Check if content is in JSON format
isJsonContent :: BS.ByteString -> Bool
isJsonContent bs =
    case BS.uncons bs of
        Just (c, _) -> c == 123 || c == 91  -- '{' or '['
        Nothing -> False

-- | Create build environment for request processing
getBuildEnv :: DaemonState -> IO BuildEnv
getBuildEnv state = do
    -- Get store path and other settings from daemon state
    config <- getDaemonConfig state
    let env = initBuildEnv
            (daemonTmpDir config)
            (daemonStorePath config)

    -- Set daemon mode
    return $ env { runMode = DaemonMode }

-- | Create evaluation environment for request processing
getEvalEnv :: DaemonState -> EvalOptions -> IO BuildEnv
getEvalEnv state options = do
    -- Start with basic build env
    env <- getBuildEnv state

    -- Set verbosity based on options
    return $ env { verbosity = evalVerbosity options }

-- | Parse a store path from string
parseStorePath :: FilePath -> Maybe StorePath
parseStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) -> Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

-- | Get store paths
getStorePaths :: BuildEnv -> IO [StorePath]
getStorePaths env = do
    -- In a real implementation, this would list paths in the store
    -- For now, return an empty list
    return []

-- | Build a Ten expression file
buildTenFile :: BuildEnv -> BS.ByteString -> BuildOptions -> IO (Either BuildError BuildResult)
buildTenFile env content options = do
    -- In a real implementation, this would:
    -- 1. Parse the Ten expression
    -- 2. Evaluate it to produce a derivation
    -- 3. Build the derivation

    -- For now, return a placeholder error
    return $ Left $ EvalError "Ten expression evaluation not implemented in this version"

-- | Build a derivation file
buildDrvFile :: BuildEnv -> BS.ByteString -> BuildOptions -> IO (Either BuildError BuildResult)
buildDrvFile env content options = do
    -- In a real implementation, this would:
    -- 1. Parse the derivation
    -- 2. Build it

    -- For now, return a placeholder error
    return $ Left $ BuildFailed "Derivation building not implemented in this version"

-- | Evaluate a Ten expression
evaluateContent :: BuildEnv -> BS.ByteString -> EvalOptions -> IO (Either BuildError Derivation)
evaluateContent env content options = do
    -- In a real implementation, this would evaluate the Ten expression

    -- For now, return a placeholder derivation
    let name = "placeholder"
        hash = "abcdef1234567890"
        builder = StorePath "placeholder-builder-hash" "bash"
        args = ["-c", "echo placeholder > $out"]
        inputs = Set.empty
        outputs = Set.singleton "out"
        derivEnv = Map.empty
        systemType = fromMaybe "x86_64-linux" (evalSystemType options)

    -- Create a placeholder derivation
    result <- evalTen (mkDerivation name builder args inputs outputs derivEnv systemType) env
    case result of
        Left err -> return $ Left err
        Right (drv, _) -> return $ Right drv

-- | Register a build in the daemon state
registerBuild :: DaemonState -> Derivation -> IO BuildId
registerBuild state drv = do
    -- Generate a build ID
    buildId <- newBuildId

    -- Register in state
    atomically $ do
        -- Add to active builds
        let status = BuildPending
        addActiveBuild state buildId drv status

    return buildId

-- | Cancel a build
cancelBuild :: DaemonState -> BuildId -> IO Bool
cancelBuild state buildId = do
    -- Get build info
    buildInfo <- atomically $ getBuildInfo state buildId

    case buildInfo of
        Nothing ->
            -- Build not found
            return False

        Just (_, status, _) ->
            if status == BuildCompleted || status == BuildFailed'
                then
                    -- Can't cancel completed/failed builds
                    return False
                else do
                    -- Mark as canceled and signal cancellation
                    atomically $ updateBuildStatus state buildId BuildFailed'
                    return True

-- | System call timeout function
timeout :: Int -> IO a -> IO (Maybe a)
timeout micros action = do
    -- In a real implementation, this would use System.Timeout.timeout
    -- For this code, we'll implement a simplified version

    -- Create MVar to hold result
    resultVar <- newEmptyMVar

    -- Fork thread to run action
    tid <- forkIO $ do
        result <- try action
        case result of
            Left (e :: SomeException) -> putMVar resultVar (Left e)
            Right val -> putMVar resultVar (Right val)

    -- Fork thread for timeout
    timeoutTid <- forkIO $ do
        threadDelay micros
        putMVar resultVar (Left (userError "timeout"))

    -- Wait for result or timeout
    result <- takeMVar resultVar

    -- Kill both threads if still running
    killThread tid `catch` \(_ :: SomeException) -> return ()
    killThread timeoutTid `catch` \(_ :: SomeException) -> return ()

    -- Return result
    case result of
        Left _ -> return Nothing
        Right val -> return (Just val)

-- Bit manipulation helpers
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
(.|.) = (Prelude.||)
