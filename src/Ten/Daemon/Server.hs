{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

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
    handleDerivationRequest,

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

    -- Security functions
    validateRequestPermissions,
    logSecurityEvent,

    -- Server control
    ServerControl(..)
) where

import Control.Concurrent (ThreadId, forkIO, forkFinally, myThreadId, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Exception (bracket, finally, try, catch, handle, throwIO, SomeException, AsyncException(..),
                          displayException, fromException, ErrorCall(..))
import Control.Monad (forever, void, when, unless, forM_, foldM, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.State (get, modify)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString, word32BE)
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust, isNothing, catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Word (Word32, Word64)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import Network.Socket.Options (setSendTimeout, setRecvTimeout, setReuseAddr)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, getModificationTime)
import System.Environment (getEnvironment, getArgs, lookupEnv, getProgName)
import System.Exit (ExitCode(..), exitSuccess, exitFailure, exitWith)
import System.FilePath ((</>), takeDirectory, takeFileName, takeExtension)
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush, hPutStrLn, stderr, stdout, stdin,
                 openFile, hGetLine, BufferMode(..), hSetBuffering)
import System.IO.Error (isDoesNotExistError, isPermissionError, catchIOError)
import System.Posix.Files (setFileMode, getFileStatus, accessTime, modificationTime)
import System.Posix.Types (FileMode, ProcessID)
import System.Posix.User (UserID, GroupID, setUserID, setGroupID, getEffectiveUserID, getRealUserID,
                          getUserEntryForName, groupID, userID, getUserEntryForID)
import System.Posix.Process (getProcessID)
import System.Process (readProcess, createProcess, proc, waitForProcess)
import System.Random (randomRIO)
import Crypto.Hash (hash, Digest, SHA256)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA

import Ten.Core
import Ten.Build (BuildResult(..), buildDerivation)
import Ten.Daemon.Protocol
import Ten.Daemon.State
import Ten.Daemon.Auth
import Ten.Daemon.Config (DaemonConfig(..), defaultDBPath)
import Ten.Derivation (serializeDerivation, deserializeDerivation, hashDerivation)
import Ten.Store (storePathToFilePath, makeStorePath, addToStore, verifyStorePath, listStorePaths)
import Ten.Hash (hashByteString, showHash)
import Ten.DB.Core
import Ten.DB.Derivations
import Ten.DB.References (registerReferences)
import Ten.GC (collectGarbage, GCStats(..))

-- | Information about a connected client
data ClientInfo = ClientInfo {
    ciThreadId :: ThreadId,          -- ^ Thread ID handling the client
    ciSocket :: Socket,              -- ^ Client socket
    ciHandle :: Handle,              -- ^ Socket handle for I/O
    ciUserId :: UserId,              -- ^ Authenticated user ID
    ciAuthToken :: AuthToken,        -- ^ Authentication token
    ciConnectTime :: UTCTime,        -- ^ When the client connected
    ciLastActivity :: TVar UTCTime,  -- ^ Last client activity time
    ciState :: TVar ClientState,     -- ^ Current client state
    ciAddress :: SockAddr,           -- ^ Client socket address
    ciRequestCount :: TVar Int,      -- ^ Number of requests processed
    ciPermissions :: TVar (Set Permission) -- ^ Client permissions
}

instance Show ClientInfo where
    show ci = "ClientInfo { userId = " ++ show (ciUserId ci) ++
              ", connectTime = " ++ show (ciConnectTime ci) ++
              ", address = " ++ show (ciAddress ci) ++ " }"

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
    scState :: DaemonState,          -- ^ Daemon state
    scConfig :: DaemonConfig,        -- ^ Daemon configuration
    scAuthDb :: TVar AuthDb,         -- ^ Authentication database
    scRateLimiter :: TVar (Map SockAddr (Int, UTCTime)), -- ^ Rate limiting
    scSecurityLog :: Handle,         -- ^ Security event log handle
    scAccessLog :: Handle,           -- ^ Access log handle
    scProcesses :: TVar (Map BuildId ProcessID)  -- ^ Active build processes
}

-- | Type for timeout handle
data TimerHandle = TimerHandle (IORef (Maybe ThreadId))

-- | Security constants
maxRequestSize :: Int
maxRequestSize = 50 * 1024 * 1024  -- 50MB max request size

maxRequestsPerMinute :: Int
maxRequestsPerMinute = 300  -- 5 requests per second

maxAuthAttemptsPerMinute :: Int
maxAuthAttemptsPerMinute = 10

clientTimeoutSeconds :: Int
clientTimeoutSeconds = 300  -- 5 minutes

maxConcurrentClientsPerIP :: Int
maxConcurrentClientsPerIP = 10

-- | Start the server
startServer :: Socket -> DaemonState -> DaemonConfig -> IO ServerControl
startServer serverSocket state config = do
    -- Initialize client tracking
    clients <- newTVarIO Map.empty
    shutdownFlag <- newTVarIO False
    buildProcesses <- newTVarIO Map.empty

    -- Initialize rate limiter
    rateLimiter <- newTVarIO Map.empty

    -- Initialize authentication database
    authDbPath <- getAuthDbPath config
    authDb <- loadAuthDb authDbPath
    authDbVar <- newTVarIO authDb

    -- Setup log files
    securityLog <- openSecurityLog config
    accessLog <- openAccessLog config

    -- Set log buffering mode to line buffering for more immediate output
    hSetBuffering securityLog LineBuffering
    hSetBuffering accessLog LineBuffering

    -- Log server startup
    now <- getCurrentTime
    pid <- getProcessID
    hPutStrLn securityLog $ formatLogEntry now "SERVER" $
           "Ten daemon starting (PID: " ++ show pid ++ ") with socket: " ++
           daemonSocketPath config

    -- Set socket options
    setSocketOptions serverSocket

    -- Start the accept thread with proper privileged context
    serverThread <- forkIO $ do
        -- Create build environment with daemon context
        let env = initDaemonEnv (daemonTmpDir config) (daemonStorePath config) (daemonUser config)
            buildState = initBuildState Build (BuildIdFromInt 0)

        -- Run the accept loop with daemon privileges
        result <- runTen @'Build @'Privileged
                  (acceptClients serverSocket clients shutdownFlag state config
                                authDbVar rateLimiter securityLog accessLog)
                  env buildState

        case result of
            Left err ->
                hPutStrLn stderr $ "Server error: " ++ show err
            Right _ ->
                return ()

    -- Return the server control structure
    return ServerControl {
        scSocket = serverSocket,
        scThread = serverThread,
        scClients = clients,
        scShutdown = shutdownFlag,
        scState = state,
        scConfig = config,
        scAuthDb = authDbVar,
        scRateLimiter = rateLimiter,
        scSecurityLog = securityLog,
        scAccessLog = accessLog,
        scProcesses = buildProcesses
    }

-- | Set socket options for performance and security
setSocketOptions :: Socket -> IO ()
setSocketOptions sock = do
    -- Set socket reuse option
    setReuseAddr sock 1

    -- Set timeout options (5 seconds)
    setSendTimeout sock 5000
    setRecvTimeout sock 5000

    -- Set socket to close-on-exec
    setCloseOnExecIfNeeded sock

-- | Stop the server
stopServer :: ServerControl -> IO ()
stopServer ServerControl{..} = do
    -- Set shutdown flag
    atomically $ writeTVar scShutdown True

    -- Wait a moment for the flag to be noticed
    threadDelay 100000 -- 0.1 seconds

    -- Get all client info
    clientMap <- atomically $ readTVar scClients

    -- Log server shutdown
    now <- getCurrentTime
    hPutStrLn scSecurityLog $ formatLogEntry now "SERVER" "Server shutting down, closing connections"

    -- Terminate all build processes
    buildProcs <- atomically $ readTVar scProcesses
    forM_ (Map.assocs buildProcs) $ \(buildId, pid) -> do
        -- Try to terminate the process
        terminateProcess pid
        hPutStrLn scSecurityLog $ formatLogEntry now "BUILD" $
            "Terminated build process " ++ show pid ++ " for build " ++ show buildId

    -- Close all client connections
    forM_ (Map.elems clientMap) $ \ClientInfo{..} -> do
        atomically $ writeTVar ciState ShuttingDown
        -- Attempt to send a shutdown notice to the client
        catch (sendShutdownNotice ciHandle) (\(_ :: SomeException) -> return ())
        -- Close the socket and handle
        catch (hClose ciHandle) (\(_ :: SomeException) -> return ())
        catch (close ciSocket) (\(_ :: SomeException) -> return ())

    -- Close server socket
    close scSocket

    -- Kill server thread if still running
    killThread scThread `catch` \(_ :: SomeException) -> return ()

    -- Close log files
    hClose scSecurityLog
    hClose scAccessLog

    -- Save authentication database
    authDb <- atomically $ readTVar scAuthDb
    authDbPath <- getAuthDbPath scConfig
    saveAuthDb authDbPath authDb

    -- Save daemon state
    saveDaemonState scConfig scState

    -- Log final shutdown
    hPutStrLn stderr "Ten daemon server shutdown complete"

-- | Terminate a process by PID
terminateProcess :: ProcessID -> IO ()
terminateProcess pid = do
    -- Try to send SIGTERM
    void $ try $ signalProcess sigTERM pid

    -- Give it a moment to shut down
    threadDelay 1000000  -- 1 second

    -- If still running, force kill with SIGKILL
    stillRunning <- isProcessRunning pid
    when stillRunning $ do
        void $ try $ signalProcess sigKILL pid

-- | Check if a process is running
isProcessRunning :: ProcessID -> IO Bool
isProcessRunning pid = do
    -- Try to signal with signal 0 (does not actually send a signal)
    result <- try $ signalProcess 0 pid
    case result of
        Left (_ :: SomeException) -> return False
        Right () -> return True

-- | Send a shutdown notice to a client
sendShutdownNotice :: Handle -> IO ()
sendShutdownNotice handle = do
    let shutdownMsg = encodeMessage $ ResponseMsgWrapper $ ResponseMsg 0 $
                     Response TagShutdownResponse (Aeson.toJSON ShutdownResponse)
    BS.hPut handle shutdownMsg
    hFlush handle

-- | Main loop to accept client connections
acceptClients :: Socket -> ActiveClients -> TVar Bool -> DaemonState -> DaemonConfig
              -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
              -> Handle -> Handle -> TenM 'Build 'Privileged ()
acceptClients serverSocket clients shutdownFlag state config
             authDbVar rateLimiter securityLog accessLog = do
    env <- ask

    let acceptLoop = do
            -- Check if we should shut down
            shouldShutdown <- liftIO $ atomically $ readTVar shutdownFlag
            unless shouldShutdown $ do
                -- Wait for a client connection (with timeout)
                mClient <- liftIO $ waitForClientConnection serverSocket 1000000 -- 1 second timeout

                case mClient of
                    Nothing ->
                        -- Timeout, continue accepting
                        acceptLoop

                    Just (clientSocket, clientAddr) -> do
                        -- Log the connection attempt
                        now <- liftIO getCurrentTime
                        liftIO $ hPutStrLn accessLog $ formatLogEntry now "CONNECT" $
                                 "New connection from " ++ show clientAddr

                        -- Check rate limiting for this address
                        allowed <- liftIO $ checkConnectionRateLimit rateLimiter clientAddr

                        if not allowed
                            then do
                                -- Log and close the connection
                                liftIO $ hPutStrLn securityLog $ formatLogEntry now "RATELIMIT" $
                                         "Connection rate limit exceeded for " ++ show clientAddr
                                liftIO $ close clientSocket
                                acceptLoop
                            else do
                                -- Check for too many connections from same address
                                clientCount <- liftIO $ countClientsFromAddress clients clientAddr
                                if clientCount >= maxConcurrentClientsPerIP
                                    then do
                                        -- Log and close the connection
                                        liftIO $ hPutStrLn securityLog $ formatLogEntry now "TOOMANYCONN" $
                                                 "Too many connections from " ++ show clientAddr
                                        liftIO $ close clientSocket
                                        acceptLoop
                                    else do
                                        -- Convert to handle for convenience
                                        clientHandle <- liftIO $ socketToHandle clientSocket ReadWriteMode
                                        liftIO $ hSetBuffering clientHandle LineBuffering

                                        -- Initialize client state
                                        lastActivityVar <- liftIO $ newTVarIO now
                                        clientStateVar <- liftIO $ newTVarIO Authenticating
                                        requestCountVar <- liftIO $ newTVarIO 0
                                        permissionsVar <- liftIO $ newTVarIO Set.empty

                                        -- Start client handler thread
                                        clientThread <- liftIO $ forkFinally
                                            (handleClientIO clientSocket clientHandle clients state config
                                                         authDbVar rateLimiter securityLog accessLog
                                                         lastActivityVar clientStateVar requestCountVar
                                                         permissionsVar clientAddr env)
                                            (\result -> do
                                                -- Clean up when client thread exits
                                                case result of
                                                    Left ex ->
                                                        hPutStrLn securityLog $ formatLogEntry now "ERROR" $
                                                             "Client handler error: " ++ displayException ex
                                                    Right _ -> return ()

                                                -- Close the handle and socket
                                                catch (hClose clientHandle) (\(_ :: SomeException) -> return ())
                                                catch (close clientSocket) (\(_ :: SomeException) -> return ())

                                                -- Remove from clients map
                                                tid <- myThreadId
                                                atomically $ modifyTVar clients (Map.delete tid)

                                                -- Log the disconnection
                                                disconnectTime <- getCurrentTime
                                                hPutStrLn accessLog $ formatLogEntry disconnectTime "DISCONNECT" $
                                                         "Client " ++ show clientAddr ++ " disconnected"
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
                                                ciState = clientStateVar,
                                                ciAddress = clientAddr,
                                                ciRequestCount = requestCountVar,
                                                ciPermissions = permissionsVar
                                            }

                                        -- Add to client map
                                        liftIO $ atomically $ modifyTVar clients $ Map.insert clientThread clientInfo

                                        -- Continue accepting
                                        acceptLoop

    -- Start the accept loop with error handling
    acceptLoop `catch` \(e :: SomeException) -> do
        -- Log error and continue if not intentional shutdown
        case fromException e of
            Just ThreadKilled -> return ()  -- Normal shutdown
            _ -> do
                now <- liftIO getCurrentTime
                liftIO $ hPutStrLn securityLog $ formatLogEntry now "ERROR" $
                         "Error in accept loop: " ++ displayException e

                -- Try to restart accept loop unless we're shutting down
                shouldShutdown <- liftIO $ atomically $ readTVar shutdownFlag
                unless shouldShutdown $
                    acceptClients serverSocket clients shutdownFlag state config
                                  authDbVar rateLimiter securityLog accessLog

-- Helper to launch client handler with proper environment
handleClientIO :: Socket -> Handle -> ActiveClients -> DaemonState -> DaemonConfig
               -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
               -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
               -> TVar Int -> TVar (Set Permission) -> SockAddr -> BuildEnv -> IO ()
handleClientIO clientSocket clientHandle clients state config
              authDbVar rateLimiter securityLog accessLog
              lastActivityVar clientStateVar requestCountVar permissionsVar clientAddr env = do
    -- Create a new build state for this client
    buildId <- BuildId <$> newUnique
    let buildState = initBuildState Build buildId

    -- Run with privileged context since we're in the daemon
    result <- runTen @'Build @'Privileged
              (handleClient clientSocket clientHandle clients state config
                            authDbVar rateLimiter securityLog accessLog
                            lastActivityVar clientStateVar requestCountVar
                            permissionsVar clientAddr)
              env buildState

    case result of
        Left err ->
            hPutStrLn stderr $ "Client handler error: " ++ show err
        Right _ ->
            return ()

-- | Handle a client connection
handleClient :: Socket -> Handle -> ActiveClients -> DaemonState -> DaemonConfig
             -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
             -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
             -> TVar Int -> TVar (Set Permission) -> SockAddr
             -> TenM 'Build 'Privileged ()
handleClient clientSocket clientHandle clients state config
            authDbVar rateLimiter securityLog accessLog
            lastActivityVar clientStateVar requestCountVar permissionsVar clientAddr = do

    -- Set timeout for authentication
    authTimeout <- liftIO $ registerTimeout 30 $ do
        -- Authentication timeout
        now <- getCurrentTime
        hPutStrLn securityLog $ formatLogEntry now "TIMEOUT" $
                 "Authentication timeout for client " ++ show clientAddr

        -- Force disconnect
        atomically $ writeTVar clientStateVar ShuttingDown
        hClose clientHandle
        close clientSocket

    -- First, authenticate the client
    authResult <- liftIO $ authenticateClient clientHandle authDbVar rateLimiter clientAddr securityLog

    -- Clear the authentication timeout
    liftIO $ cancelTimeout authTimeout

    case authResult of
        Left errorMsg -> do
            -- Authentication failed, send error and close
            liftIO $ sendAuthFailure clientHandle errorMsg
            now <- liftIO getCurrentTime
            liftIO $ hPutStrLn securityLog $ formatLogEntry now "AUTH_FAIL" $
                     "Authentication failed for " ++ show clientAddr ++ ": " ++ T.unpack errorMsg
            return ()

        Right (userId, authToken, permissions) -> do
            -- Log successful authentication
            now <- liftIO getCurrentTime
            liftIO $ hPutStrLn accessLog $ formatLogEntry now "AUTH_SUCCESS" $
                     "Client " ++ show clientAddr ++ " authenticated as " ++
                     case userId of UserId uid -> T.unpack uid

            -- Store permissions
            liftIO $ atomically $ writeTVar permissionsVar permissions

            -- Update client info with authenticated user
            tid <- liftIO myThreadId
            updateClientAuth tid userId authToken

            -- Register idle timeout handler
            idleTimeout <- liftIO $ registerTimeout clientTimeoutSeconds $ do
                -- Idle timeout
                idleTime <- getCurrentTime
                hPutStrLn securityLog $ formatLogEntry idleTime "TIMEOUT" $
                         "Idle timeout for client " ++ show clientAddr

                -- Force disconnect
                atomically $ writeTVar clientStateVar ShuttingDown
                hClose clientHandle
                close clientSocket

            -- Set client state to active
            liftIO $ atomically $ writeTVar clientStateVar Active

            -- Handle client requests
            handleClientRequests clientSocket clientHandle state config clientAddr
                                 securityLog accessLog lastActivityVar clientStateVar
                                 requestCountVar permissionsVar idleTimeout

  where
    -- Update client info after authentication
    updateClientAuth tid userId authToken = liftIO $ atomically $ do
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
handleClientRequests :: Socket -> Handle -> DaemonState -> DaemonConfig -> SockAddr
                     -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
                     -> TVar Int -> TVar (Set Permission) -> TimerHandle
                     -> TenM 'Build 'Privileged ()
handleClientRequests clientSocket clientHandle state config clientAddr
                    securityLog accessLog lastActivityVar clientStateVar
                    requestCountVar permissionsVar idleTimeout = do

    env <- ask  -- Get the BuildEnv with privileged context

    let processLoop = do
            -- Check if client is shutting down
            clientState <- liftIO $ atomically $ readTVar clientStateVar
            when (clientState == Active) $ do
                -- Try to read a message
                msgResult <- liftIO $ try $ readMessageWithTimeout clientHandle 30000000 -- 30 seconds

                case msgResult of
                    Left (e :: SomeException) -> do
                        -- Read error, client disconnected or timeout
                        now <- liftIO getCurrentTime
                        liftIO $ hPutStrLn accessLog $ formatLogEntry now "ERROR" $
                                 "Read error from client " ++ show clientAddr ++ ": " ++ displayException e
                        return ()

                    Right msgBytes -> do
                        -- Update last activity time
                        now <- liftIO getCurrentTime
                        liftIO $ atomically $ writeTVar lastActivityVar now

                        -- Reset idle timeout
                        liftIO $ resetTimeout idleTimeout

                        -- Update request count for rate limiting
                        reqCount <- liftIO $ atomically $ do
                            count <- readTVar requestCountVar
                            writeTVar requestCountVar (count + 1)
                            return (count + 1)

                        -- Apply rate limiting
                        allowed <- liftIO $ checkRequestRateLimit rateLimiter clientAddr

                        if not allowed then do
                            -- Rate limit exceeded
                            liftIO $ hPutStrLn securityLog $ formatLogEntry now "RATELIMIT" $
                                     "Request rate limit exceeded for " ++ show clientAddr

                            -- Send rate limit error
                            liftIO $ sendResponse clientHandle 0 $
                                ErrorResponse $ DaemonError "Request rate limit exceeded"

                            -- Continue processing
                            processLoop
                        else do
                            -- Log the request
                            liftIO $ hPutStrLn accessLog $ formatLogEntry now "REQUEST" $
                                     "Client " ++ show clientAddr ++ " request #" ++ show reqCount

                            -- Process the message
                            case decodeRequestMessage msgBytes of
                                Nothing -> do
                                    -- Invalid message format
                                    liftIO $ hPutStrLn securityLog $ formatLogEntry now "MALFORMED" $
                                             "Malformed request from client " ++ show clientAddr

                                    -- Send error response
                                    liftIO $ sendResponse clientHandle 0 $
                                        ErrorResponse $ DaemonError "Malformed request"

                                    -- Continue processing
                                    processLoop

                                Just (RequestMsg reqId request) -> do
                                    -- Get client permissions
                                    permissions <- liftIO $ atomically $ readTVar permissionsVar

                                    -- Validate permissions for this request
                                    permissionValid <- validateRequestPermissions request permissions securityLog clientAddr

                                    if not permissionValid
                                        then do
                                            -- Permission denied
                                            liftIO $ sendResponse clientHandle reqId $
                                                ErrorResponse $ AuthError "Permission denied"

                                            -- Continue processing
                                            processLoop
                                        else do
                                            -- Process the request in the daemon's privileged context
                                            response <- processRequest request state config permissions

                                            -- Send the response
                                            liftIO $ sendResponse clientHandle reqId response

                                            -- Continue processing
                                            processLoop

    -- Start processing loop
    processLoop `catch` \(e :: SomeException) -> do
        -- Log error unless it's a normal disconnect
        now <- liftIO getCurrentTime
        case fromException e of
            Just ThreadKilled -> return ()  -- Normal shutdown
            _ -> liftIO $ hPutStrLn securityLog $ formatLogEntry now "ERROR" $
                     "Error handling client " ++ show clientAddr ++ ": " ++ displayException e

        -- Cancel the idle timeout
        liftIO $ cancelTimeout idleTimeout

-- | Process a client request in the privileged daemon context
processRequest :: Request -> DaemonState -> DaemonConfig -> Set Permission
               -> TenM 'Build 'Privileged Response
processRequest request state config permissions = do
    -- Dispatch to the appropriate handler
    dispatchRequest request state config permissions `catch` \(e :: SomeException) -> do
        -- Convert any errors to ErrorResponse
        case fromException e of
            Just ThreadKilled -> throwError $ DaemonError "Thread killed"
            _ -> return $ ErrorResponse $ DaemonError $ "Request processing error: " <> T.pack (displayException e)

-- | Dispatch a request to the appropriate handler
dispatchRequest :: Request -> DaemonState -> DaemonConfig -> Set Permission
                -> TenM 'Build 'Privileged Response
dispatchRequest request state config permissions = case request of
    BuildRequest{..} ->
        handleBuildRequest buildFilePath buildFileContent buildOptions state permissions

    EvalRequest{..} ->
        handleEvalRequest evalFilePath evalFileContent evalOptions state permissions

    BuildDerivationRequest{..} ->
        handleBuildDerivationRequest buildDerivation buildDerivOptions state permissions

    BuildStatusRequest{..} ->
        handleBuildStatusRequest statusBuildId state permissions

    CancelBuildRequest{..} ->
        handleCancelBuildRequest cancelBuildId state permissions

    StoreAddRequest{..} ->
        handleStoreRequest (StoreAddCmd storeAddPath storeAddContent) state config permissions

    StoreVerifyRequest{..} ->
        handleStoreRequest (StoreVerifyCmd storeVerifyPath) state config permissions

    StorePathRequest{..} ->
        handleStoreRequest (StorePathCmd storePathForFile storePathContent) state config permissions

    StoreListRequest ->
        handleStoreRequest StoreListCmd state config permissions

    GCRequest{..} ->
        handleGCRequest gcForce state config permissions

    StatusRequest ->
        handleStatusRequest state config permissions

    ConfigRequest ->
        handleConfigRequest state config permissions

    ShutdownRequest ->
        handleShutdownRequest state config permissions

    DerivationStoreRequest{..} ->
        handleDerivationRequest (StoreDerivationCmd derivationContent) state config permissions

    DerivationQueryRequest{..} ->
        handleDerivationRequest (QueryDerivationCmd derivationQueryHash) state config permissions

    GetDerivationForOutputRequest{..} ->
        handleDerivationRequest (GetDerivationForOutputCmd getDerivationForPath) state config permissions

    ListDerivationsRequest{..} ->
        handleDerivationRequest ListDerivationsCmd state config permissions

    PingRequest ->
        return PongResponse

-- | Validate permissions for a request
validateRequestPermissions :: Request -> Set Permission -> Handle -> SockAddr -> TenM 'Build 'Privileged Bool
validateRequestPermissions request permissions securityLog clientAddr = do
    -- Determine which permission is required for this request
    let requiredPermission = getRequiredPermission request
    let hasPermission = requiredPermission `Set.member` permissions

    -- Log permission issues
    unless hasPermission $ do
        now <- liftIO getCurrentTime
        liftIO $ hPutStrLn securityLog $ formatLogEntry now "PERMISSION_DENIED" $
                 "Client " ++ show clientAddr ++ " lacks required permission: " ++
                 T.unpack (permissionToText requiredPermission)

    return hasPermission

-- | Get the required permission for a request
getRequiredPermission :: Request -> Permission
getRequiredPermission request = case request of
    -- Build-related requests
    BuildRequest{} -> PermBuild
    EvalRequest{} -> PermBuild
    BuildDerivationRequest{} -> PermBuild
    BuildStatusRequest{} -> PermQueryBuild
    CancelBuildRequest{} -> PermCancelBuild

    -- Store-related requests
    StoreAddRequest{} -> PermModifyStore
    StoreVerifyRequest{} -> PermQueryStore
    StorePathRequest{} -> PermQueryStore
    StoreListRequest -> PermQueryStore

    -- Derivation-related requests
    DerivationStoreRequest{} -> PermStoreDerivation
    DerivationQueryRequest{} -> PermQueryDerivation
    GetDerivationForOutputRequest{} -> PermQueryDerivation
    ListDerivationsRequest{} -> PermQueryDerivation

    -- GC-related requests
    GCRequest{} -> PermRunGC

    -- Admin requests
    StatusRequest -> PermQueryStatus
    ConfigRequest -> PermAdmin
    ShutdownRequest -> PermShutdown

    -- Default to admin permission for anything else
    _ -> PermAdmin

-- | Permission to text mapping
permissionToText :: Permission -> Text
permissionToText PermBuild = "build"
permissionToText PermCancelBuild = "cancel-build"
permissionToText PermQueryBuild = "query-build"
permissionToText PermQueryStore = "query-store"
permissionToText PermModifyStore = "modify-store"
permissionToText PermRunGC = "run-gc"
permissionToText PermShutdown = "shutdown"
permissionToText PermManageUsers = "manage-users"
permissionToText PermQueryDerivation = "query-derivation"
permissionToText PermStoreDerivation = "store-derivation"
permissionToText PermQueryStatus = "query-status"
permissionToText PermAdmin = "admin"

-- | Handle a build request
handleBuildRequest :: Text -> Maybe BS.ByteString -> BuildOptions -> DaemonState
                   -> Set Permission -> TenM 'Build 'Privileged Response
handleBuildRequest filePath maybeContent options state permissions = do
    -- Verify build permission
    unless (PermBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: build"

    env <- ask

    -- Get the file content
    content <- case maybeContent of
        Just c -> return c
        Nothing -> do
            -- Try to read from filesystem if not provided
            let path = T.unpack filePath
            fileExists <- liftIO $ doesFileExist path
            if fileExists
                then liftIO $ BS.readFile path
                else return $ TE.encodeUtf8 "File not found"

    -- Parse the build file
    derivResult <- parseBuildFile filePath content `catchError` \err -> do
        return $ Left err

    case derivResult of
        Left err ->
            return $ ErrorResponse err

        Right derivation -> do
            -- Register the build with the daemon state
            buildId <- liftIO $ registerBuild state derivation (UserId "daemon")
                              (buildPriority options) (buildTimeout options)

            -- Update build status to pending
            liftIO $ atomically $ updateBuildStatus state buildId BuildPending

            -- If async, run in background thread
            if buildAsync options
                then do
                    -- Start build in background
                    void $ liftIO $ forkIO $ do
                        -- Run the build with the daemon's privileges
                        buildResult <- runTen @'Build @'Privileged
                                      (buildDerivation derivation) env
                                      (initBuildState Build buildId)

                        -- Update build status based on result
                        case buildResult of
                            Left err -> do
                                atomically $ updateBuildStatus state buildId BuildFailed'
                                atomically $ storeBuildResult state buildId (Left err)
                            Right (result, _) -> do
                                atomically $ updateBuildStatus state buildId BuildCompleted
                                atomically $ storeBuildResult state buildId (Right result)

                    -- Return immediate response with build ID
                    return $ BuildStartedResponse buildId
                else do
                    -- Run build synchronously
                    liftIO $ atomically $ updateBuildStatus state buildId (BuildRunning 0.0)

                    buildResult <- buildDerivation derivation `catchError` \err -> do
                        liftIO $ atomically $ updateBuildStatus state buildId BuildFailed'
                        liftIO $ atomically $ storeBuildResult state buildId (Left err)
                        throwError err

                    -- Update build status and store result
                    liftIO $ atomically $ updateBuildStatus state buildId BuildCompleted
                    liftIO $ atomically $ storeBuildResult state buildId (Right buildResult)

                    -- Return complete result
                    return $ BuildResponse buildResult

-- | Handle an evaluation request
handleEvalRequest :: Text -> Maybe BS.ByteString -> EvalOptions -> DaemonState
                  -> Set Permission -> TenM 'Build 'Privileged Response
handleEvalRequest filePath maybeContent options state permissions = do
    -- Verify build permission
    unless (PermBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: build"

    env <- ask

    -- Get the file content
    content <- case maybeContent of
        Just c -> return c
        Nothing -> do
            -- Try to read from filesystem if not provided
            let path = T.unpack filePath
            fileExists <- liftIO $ doesFileExist path
            if fileExists
                then liftIO $ BS.readFile path
                else return $ TE.encodeUtf8 "File not found"

    -- Evaluate the content to a derivation
    derivResult <- evaluateContent filePath content options `catchError` \err -> do
        return $ Left err

    case derivResult of
        Left err ->
            return $ ErrorResponse err

        Right derivation ->
            return $ EvalResponse derivation

-- | Evaluate content to derivation
evaluateContent :: Text -> BS.ByteString -> EvalOptions -> TenM 'Build 'Privileged (Either BuildError Derivation)
evaluateContent filePath content options = do
    env <- ask

    -- Examine file extension to determine evaluation approach
    let ext = T.pack $ takeExtension $ T.unpack filePath

    if ext == ".ten" || ext == ".nix" then do
        -- Parse Ten/Nix expression
        parseTenExpression filePath content
    else if ext == ".drv" then do
        -- Parse derivation directly
        case deserializeDerivation content of
            Left err -> return $ Left $ SerializationError err
            Right drv -> return $ Right drv
    else do
        -- Try to auto-detect format
        if isJsonContent content
            then case deserializeDerivation content of
                Left _ -> parseTenExpression filePath content
                Right drv -> return $ Right drv
            else parseTenExpression filePath content

-- | Parse Ten expression to derivation
parseTenExpression :: Text -> BS.ByteString -> TenM 'Build 'Privileged (Either BuildError Derivation)
parseTenExpression filePath content = do
    -- This is where Ten expression parsing would happen
    -- For now, create a placeholder derivation for testing
    let name = T.pack $ takeBaseName $ T.unpack filePath
        hash = hashByteString content
        hashText = showHash hash

    -- This is a stub - real implementation would parse and evaluate the Ten expression
    -- Return error as Ten language parser isn't implemented yet
    return $ Left $ ParseError "Ten expression parsing not yet implemented"

-- | Handle a build derivation request
handleBuildDerivationRequest :: Derivation -> BuildOptions -> DaemonState
                             -> Set Permission -> TenM 'Build 'Privileged Response
handleBuildDerivationRequest drv options state permissions = do
    -- Verify build permission
    unless (PermBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: build"

    env <- ask

    -- Register the build in the daemon state
    buildId <- liftIO $ registerBuild state drv (UserId "daemon")
                      (buildPriority options) (buildTimeout options)

    -- Update build status to pending
    liftIO $ atomically $ updateBuildStatus state buildId BuildPending

    -- If async, run in background thread
    if buildAsync options
        then do
            -- Start build in background
            void $ liftIO $ forkIO $ do
                -- Run the build with the daemon's privileges
                buildResult <- runTen @'Build @'Privileged
                              (buildDerivation drv) env
                              (initBuildState Build buildId)

                -- Update build status based on result
                case buildResult of
                    Left err -> do
                        atomically $ updateBuildStatus state buildId BuildFailed'
                        atomically $ storeBuildResult state buildId (Left err)
                    Right (result, _) -> do
                        atomically $ updateBuildStatus state buildId BuildCompleted
                        atomically $ storeBuildResult state buildId (Right result)

            -- Return immediate response with build ID
            return $ BuildStartedResponse buildId
        else do
            -- Run build synchronously
            liftIO $ atomically $ updateBuildStatus state buildId (BuildRunning 0.0)

            buildResult <- buildDerivation drv `catchError` \err -> do
                liftIO $ atomically $ updateBuildStatus state buildId BuildFailed'
                liftIO $ atomically $ storeBuildResult state buildId (Left err)
                throwError err

            -- Update build status and store result
            liftIO $ atomically $ updateBuildStatus state buildId BuildCompleted
            liftIO $ atomically $ storeBuildResult state buildId (Right buildResult)

            -- Return complete result
            return $ BuildResponse buildResult

-- | Handle a build status request
handleBuildStatusRequest :: BuildId -> DaemonState -> Set Permission
                         -> TenM 'Build 'Privileged Response
handleBuildStatusRequest buildId state permissions = do
    -- Verify query permission
    unless (PermQueryBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: query-build"

    -- Get build status from state
    status <- liftIO $ getBuildStatus state buildId `catch` \(e :: SomeException) -> do
        return $ Left $ "Failed to get build status: " <> T.pack (displayException e)

    case status of
        Left errMsg ->
            return $ ErrorResponse $ DaemonError errMsg

        Right buildStatus -> do
            -- Get build info
            buildInfo <- liftIO $ getBuildInfo state buildId `catch` \(e :: SomeException) -> do
                return $ Left $ "Failed to get build info: " <> T.pack (displayException e)

            -- Try to get build result if completed
            buildResult <- if buildStatus == BuildCompleted
                then liftIO $ getBuildResult state buildId `catch` \(e :: SomeException) -> do
                    return $ Left $ "Failed to get build result: " <> T.pack (displayException e)
                else return $ Right Nothing

            -- Get build log (last 100 lines)
            buildLog <- liftIO $ getBuildLog state buildId 100 `catch` \(e :: SomeException) -> do
                return $ "Error retrieving log: " <> T.pack (displayException e)

            -- Calculate elapsed time
            now <- liftIO getCurrentTime
            elapsedTime <- case buildInfo of
                Left _ -> return 0.0
                Right info -> return $ realToFrac $ diffUTCTime now (buildInfoStartTime info)

            -- Create status update
            let update = BuildStatusUpdate {
                    buildId = buildId,
                    buildStatus = buildStatus,
                    buildTimeElapsed = elapsedTime,
                    buildTimeRemaining = Nothing,  -- Can't estimate remaining time yet
                    buildLogUpdate = Just buildLog,
                    buildResourceUsage = Map.empty
                }

            -- Return status response
            return $ BuildStatusResponse update

-- | Handle a cancel build request
handleCancelBuildRequest :: BuildId -> DaemonState -> Set Permission
                         -> TenM 'Build 'Privileged Response
handleCancelBuildRequest buildId state permissions = do
    -- Verify cancel permission
    unless (PermCancelBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: cancel-build"

    -- Try to cancel the build
    success <- liftIO $ cancelBuild state buildId

    -- If successful, update status
    when success $ do
        liftIO $ atomically $ updateBuildStatus state buildId BuildFailed'
        liftIO $ atomically $ storeBuildResult state buildId $
            Left $ BuildFailed "Build cancelled by user request"

    return $ CancelBuildResponse success

-- | Store command type
data StoreCommand
    = StoreAddCmd Text BS.ByteString
    | StoreVerifyCmd Text
    | StorePathCmd Text BS.ByteString
    | StoreListCmd

-- | Derivation command type
data DerivationCommand
    = StoreDerivationCmd BS.ByteString
    | QueryDerivationCmd Text
    | GetDerivationForOutputCmd Text
    | ListDerivationsCmd

-- | Handle a store request
handleStoreRequest :: StoreCommand -> DaemonState -> DaemonConfig -> Set Permission
                   -> TenM 'Build 'Privileged Response
handleStoreRequest cmd state config permissions = case cmd of
    StoreAddCmd path content -> do
        -- Verify store modification permission
        unless (PermModifyStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: modify-store"

        -- Add to store with privileged context
        storePath <- addToStore (storeFilename path) content `catchError` \err -> do
            return $ ErrorResponse $ StoreError $ "Failed to add to store: " <>
                       case err of
                           StoreError msg -> msg
                           _ -> T.pack $ show err

        case storePath of
            ErrorResponse{} -> return storePath
            _ -> return $ StoreAddResponse storePath

    StoreVerifyCmd path -> do
        -- Verify store query permission
        unless (PermQueryStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-store"

        -- Parse the store path
        case parseStorePath path of
            Nothing ->
                return $ ErrorResponse $ StoreError "Invalid store path format"

            Just sp -> do
                -- Verify the path exists in store
                exists <- verifyStorePath sp `catchError` \err -> do
                    return $ ErrorResponse $ StoreError $ "Error verifying path: " <>
                               case err of
                                   StoreError msg -> msg
                                   _ -> T.pack $ show err

                case exists of
                    ErrorResponse{} -> return exists
                    _ -> return $ StoreVerifyResponse sp exists

    StorePathCmd file content -> do
        -- Verify store query permission
        unless (PermQueryStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-store"

        -- Create a store path for this content
        let name = T.pack $ takeFileName $ T.unpack file
        let hash = showHash $ hashByteString content
        let storePath = makeStorePath hash name

        return $ StorePathResponse storePath

    StoreListCmd -> do
        -- Verify store query permission
        unless (PermQueryStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-store"

        env <- ask

        -- List store contents
        storePaths <- listStorePaths (storeLocation env) `catchError` \err -> do
            return $ ErrorResponse $ StoreError $ "Failed to list store: " <>
                       case err of
                           StoreError msg -> msg
                           _ -> T.pack $ show err

        case storePaths of
            ErrorResponse{} -> return storePaths
            _ -> return $ StoreContentsResponse storePaths

-- | Handle a garbage collection request
handleGCRequest :: Bool -> DaemonState -> DaemonConfig -> Set Permission
                -> TenM 'Build 'Privileged Response
handleGCRequest force state config permissions = do
    -- Verify GC permission
    unless (PermRunGC `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: run-gc"

    -- Check if we can run GC
    canGC <- liftIO $ atomically $ checkGCLock state

    if not canGC && not force
        then return $ ErrorResponse $ GCError "Garbage collection already in progress"
        else do
            env <- ask

            -- Acquire GC lock
            liftIO $ atomically $ acquireGCLock state

            -- Run GC in a separate thread if async
            gcThread <- liftIO $ forkIO $ do
                -- Mark GC as running in state
                atomically $ updateGCStatus state (GCRunning 0.0)

                startTime <- getCurrentTime
                result <- runTen @'Build @'Privileged collectGarbage env (initBuildState Build (BuildIdFromInt 0))
                endTime <- getCurrentTime

                -- Release GC lock
                atomically $ releaseGCLock state

                -- Update last GC time and status
                atomically $ do
                    updateLastGC state endTime
                    updateGCStatus state GCIdle

                -- Process result for logging
                case result of
                    Left err ->
                        putStrLn $ "GC error: " ++ show err

                    Right (stats, _) -> do
                        putStrLn $ "GC completed: " ++ show (gcCollected stats) ++ " paths collected"
                        -- Store GC stats in state
                        atomically $ storeGCStats state stats

            -- Return immediate response
            return $ GCStartedResponse

-- | Handle a derivation request
handleDerivationRequest :: DerivationCommand -> DaemonState -> DaemonConfig -> Set Permission
                        -> TenM 'Build 'Privileged Response
handleDerivationRequest cmd state config permissions = case cmd of
    StoreDerivationCmd derivationContent -> do
        -- Verify store derivation permission
        unless (PermStoreDerivation `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: store-derivation"

        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- Parse the derivation data
        case deserializeDerivation derivationContent of
            Left err ->
                return $ ErrorResponse $ SerializationError $ "Failed to deserialize derivation: " <> err

            Right drv -> do
                -- Store the derivation in the store
                storePath <- storeDerivation drv `catchError` \err ->
                    return $ Left err

                case storePath of
                    Left err ->
                        return $ ErrorResponse err

                    Right path -> do
                        -- Register in database
                        result <- withDatabase dbPath 5000 $ \db -> do
                            liftIO $ registerDerivationFile db drv path
                            return $ Right path

                        case result of
                            Left err ->
                                return $ ErrorResponse $ DBError $ "Database error: " <> T.pack (show err)

                            Right regPath ->
                                return $ DerivationStoredResponse regPath

    QueryDerivationCmd hash -> do
        -- Verify query derivation permission
        unless (PermQueryDerivation `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-derivation"

        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- Query the database for the derivation
        result <- withDatabase dbPath 5000 $ \db -> do
            derivResult <- liftIO $ retrieveDerivation db hash
            return derivResult

        case result of
            Nothing ->
                return $ ErrorResponse $ StoreError $ "Derivation not found: " <> hash

            Just drv ->
                return $ DerivationResponse drv

    GetDerivationForOutputCmd outputPath -> do
        -- Verify query derivation permission
        unless (PermQueryDerivation `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-derivation"

        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- Parse the output path
        case parseStorePath outputPath of
            Nothing ->
                return $ ErrorResponse $ StoreError $ "Invalid store path format: " <> outputPath

            Just sp -> do
                -- Query the database for the derivation that produced this output
                result <- withDatabase dbPath 5000 $ \db -> do
                    derivResult <- liftIO $ getDerivationForOutput db sp

                    case derivResult of
                        Nothing ->
                            return $ Left $ StoreError $ "No derivation found for output: " <> outputPath

                        Just derivInfo -> do
                            -- Get the actual derivation from the store path
                            derivContent <- liftIO $ retrieveDerivation db (derivInfoHash derivInfo)

                            case derivContent of
                                Nothing ->
                                    return $ Left $ StoreError "Derivation file not found in store"

                                Just drv ->
                                    return $ Right drv

                case result of
                    Left err ->
                        return $ ErrorResponse err

                    Right drv ->
                        return $ DerivationResponse drv

    ListDerivationsCmd -> do
        -- Verify query derivation permission
        unless (PermQueryDerivation `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-derivation"

        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- List all registered derivations from the database
        result <- withDatabase dbPath 5000 $ \db -> do
            liftIO $ listRegisteredDerivations db

        return $ DerivationListResponse result

-- | Handle a status request
handleStatusRequest :: DaemonState -> DaemonConfig -> Set Permission
                    -> TenM 'Build 'Privileged Response
handleStatusRequest state config permissions = do
    -- Verify query permission
    unless (PermQueryStatus `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: query-status"

    env <- ask

    -- Get daemon stats
    now <- liftIO getCurrentTime
    startTime <- liftIO $ getStateStartTime state
    let uptime = diffUTCTime now startTime

    -- Get build statistics
    buildCount <- liftIO $ atomically $ countActiveBuilds state
    completedCount <- liftIO $ atomically $ countCompletedBuilds state
    failedCount <- liftIO $ atomically $ countFailedBuilds state

    -- Get store statistics
    storePaths <- listStorePaths (storeLocation env) `catchError` \_ -> return []
    storeSize <- getStoreSize (storeLocation env)
    rootCount <- countGCRoots (storeLocation env)

    -- Create status response
    let status = DaemonStatus {
            daemonStatus = "running",
            daemonUptime = realToFrac uptime,
            daemonActiveBuilds = buildCount,
            daemonCompletedBuilds = completedCount,
            daemonFailedBuilds = failedCount,
            daemonGcRoots = rootCount,
            daemonStoreSize = storeSize,
            daemonStorePaths = length storePaths
        }

    return $ StatusResponse status

-- | Get store statistics
getStoreSize :: FilePath -> TenM 'Build 'Privileged Integer
getStoreSize storePath = do
    -- Calculate total size of store
    -- This would be optimized in a real implementation
    paths <- listStorePaths storePath `catchError` \_ -> return []
    env <- ask

    totalSize <- liftIO $ foldM (\acc path -> do
        let fullPath = storePathToFilePath path env
        fileExists <- doesFileExist fullPath
        if fileExists
            then do
                stat <- getFileStatus fullPath
                return $! acc + fromIntegral (fileSize stat)
            else
                return acc
        ) 0 paths

    return totalSize

-- | Count GC roots
countGCRoots :: FilePath -> TenM 'Build 'Privileged Int
countGCRoots storePath = do
    -- Count GC roots in the store
    let rootsDir = storePath </> "gc-roots"
    rootsExist <- liftIO $ doesDirectoryExist rootsDir

    if not rootsExist
        then return 0
        else do
            roots <- liftIO $ listDirectory rootsDir `catch` \(_ :: SomeException) -> return []
            return $ length roots

-- | Handle a configuration request
handleConfigRequest :: DaemonState -> DaemonConfig -> Set Permission
                    -> TenM 'Build 'Privileged Response
handleConfigRequest state config permissions = do
    -- Verify admin permission
    unless (PermAdmin `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: admin"

    -- Sanitize config to remove sensitive information
    let sanitizedConfig = sanitizeConfig config

    -- Return config response
    return $ ConfigResponse sanitizedConfig

-- | Sanitize config for user display (remove sensitive data)
sanitizeConfig :: DaemonConfig -> DaemonConfig
sanitizeConfig config = config {
    daemonAllowedUsers = Set.empty  -- Don't expose user list
}

-- | Handle a shutdown request
handleShutdownRequest :: DaemonState -> DaemonConfig -> Set Permission
                      -> TenM 'Build 'Privileged Response
handleShutdownRequest state config permissions = do
    -- Verify shutdown permission
    unless (PermShutdown `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: shutdown"

    -- Signal shutdown (in a real implementation, this would trigger a proper shutdown)
    liftIO $ forkIO $ do
        -- Give time for the response to be sent
        threadDelay 1000000  -- 1 second
        atomically $ signalShutdown state

    -- Return success
    return ShutdownResponse

-- | Signal shutdown in state
signalShutdown :: DaemonState -> STM ()
signalShutdown state =
    modifyTVar' (daemonShutdown state) $ const True

-- | Authenticate a client
authenticateClient :: Handle -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
                  -> SockAddr -> Handle -> IO (Either Text (UserId, AuthToken, Set Permission))
authenticateClient handle authDbVar rateLimiter clientAddr securityLog = do
    -- Check rate limiting for auth attempts
    allowed <- checkAuthRateLimit rateLimiter clientAddr

    if not allowed
        then do
            -- Log and reject
            now <- getCurrentTime
            hPutStrLn securityLog $ formatLogEntry now "AUTH_RATELIMIT" $
                     "Authentication rate limit exceeded for " ++ show clientAddr
            return $ Left "Authentication rate limit exceeded"
        else do
            -- Wait for auth message
            msgBytes <- try $ readMessageWithTimeout handle 5000000 -- 5 seconds timeout

            case msgBytes of
                Left (e :: SomeException) -> do
                    now <- getCurrentTime
                    hPutStrLn securityLog $ formatLogEntry now "AUTH_ERROR" $
                             "Error reading auth message: " ++ displayException e
                    return $ Left "Failed to read authentication message"

                Right bytes -> do
                    -- Decode auth request
                    case decodeMessage bytes of
                        Just (AuthRequestMsgWrapper (AuthRequestMsg authReq)) -> do
                            -- Validate credentials
                            now <- getCurrentTime
                            authDb <- atomically $ readTVar authDbVar

                            -- Check protocol version
                            if not (isCompatibleVersion (authVersion authReq))
                                then do
                                    hPutStrLn securityLog $ formatLogEntry now "AUTH_PROTOCOL" $
                                             "Incompatible protocol version from " ++ show clientAddr
                                    return $ Left "Incompatible protocol version"
                                else do
                                    -- Authenticate user
                                    result <- authenticateUser authDb (UserCredentials (authUser authReq) (authToken authReq)) clientAddr

                                    case result of
                                        Left err -> do
                                            hPutStrLn securityLog $ formatLogEntry now "AUTH_INVALID" $
                                                     "Invalid credentials from " ++ show clientAddr ++
                                                     ": " ++ T.unpack err
                                            return $ Left err

                                        Right (userId, token, permissions) -> do
                                            -- Send success response
                                            let response = AuthAccepted userId token
                                            BS.hPut handle $ encodeMessage $ AuthResponseMsgWrapper $ AuthResponseMsg response
                                            hFlush handle

                                            -- Update auth db
                                            atomically $ writeTVar authDbVar authDb

                                            return $ Right (userId, token, permissions)

                        _ -> do
                            now <- getCurrentTime
                            hPutStrLn securityLog $ formatLogEntry now "AUTH_MALFORMED" $
                                     "Malformed auth message from " ++ show clientAddr
                            return $ Left "Invalid authentication message"

-- | Check rate limiting for authentication attempts
checkAuthRateLimit :: TVar (Map SockAddr (Int, UTCTime)) -> SockAddr -> IO Bool
checkAuthRateLimit rateLimiter addr = atomically $ do
    now <- getCurrentTime
    rateMap <- readTVar rateLimiter

    case Map.lookup addr rateMap of
        Nothing -> do
            -- First authentication attempt from this address
            writeTVar rateLimiter (Map.insert addr (1, now) rateMap)
            return True

        Just (count, time) -> do
            -- Check if the window has expired
            let timeWindow = 60 -- 1 minute in seconds
            let windowExpired = diffUTCTime now time > fromIntegral timeWindow

            if windowExpired then do
                -- Reset counter if the window has expired
                writeTVar rateLimiter (Map.insert addr (1, now) rateMap)
                return True
            else if count >= maxAuthAttemptsPerMinute then do
                -- Rate limit exceeded
                return False
            else do
                -- Increment counter
                writeTVar rateLimiter (Map.insert addr (count + 1, time) rateMap)
                return True

-- | Check rate limiting for requests
checkRequestRateLimit :: TVar (Map SockAddr (Int, UTCTime)) -> SockAddr -> IO Bool
checkRequestRateLimit rateLimiter addr = atomically $ do
    now <- getCurrentTime
    rateMap <- readTVar rateLimiter

    case Map.lookup addr rateMap of
        Nothing -> do
            -- First request from this address
            writeTVar rateLimiter (Map.insert addr (1, now) rateMap)
            return True

        Just (count, time) -> do
            -- Check if the window has expired
            let timeWindow = 60 -- 1 minute in seconds
            let windowExpired = diffUTCTime now time > fromIntegral timeWindow

            if windowExpired then do
                -- Reset counter if the window has expired
                writeTVar rateLimiter (Map.insert addr (1, now) rateMap)
                return True
            else if count >= maxRequestsPerMinute then do
                -- Rate limit exceeded
                return False
            else do
                -- Increment counter
                writeTVar rateLimiter (Map.insert addr (count + 1, time) rateMap)
                return True

-- | Wait for a client connection with timeout
waitForClientConnection :: Socket -> Int -> IO (Maybe (Socket, SockAddr))
waitForClientConnection serverSocket timeoutMicros = do
    result <- timeout' timeoutMicros $ try $ accept serverSocket
    case result of
        Nothing -> return Nothing
        Just (Left (_ :: SomeException)) -> return Nothing
        Just (Right conn) -> return $ Just conn

-- | Count clients from a specific address
countClientsFromAddress :: ActiveClients -> SockAddr -> IO Int
countClientsFromAddress clients addr = do
    clientMap <- atomically $ readTVar clients
    return $ length $ filter (\ci -> ciAddress ci == addr) $ Map.elems clientMap

-- | Send authentication failure response
sendAuthFailure :: Handle -> Text -> IO ()
sendAuthFailure handle reason = do
    let response = AuthRejected reason
    BS.hPut handle $ encodeMessage $ AuthResponseMsgWrapper $ AuthResponseMsg response
    hFlush handle

-- | Send a response to a request
sendResponse :: Handle -> RequestId -> Response -> IO ()
sendResponse handle reqId response = do
    let respTag = responseTypeToTag response
    let msg = ResponseMsg reqId $ Response respTag (Aeson.toJSON response)
    BS.hPut handle $ encodeMessage $ ResponseMsgWrapper msg
    hFlush handle

-- | Register a timeout handler
registerTimeout :: Int -> IO () -> IO TimerHandle
registerTimeout seconds action = do
    timerRef <- newIORef Nothing

    -- Create the timer thread
    timerThread <- forkIO $ do
        threadDelay (seconds * 1000000)
        -- Check if we've been canceled
        active <- readIORef timerRef
        when (isJust active) $ do
            -- Execute the timeout action
            action

    -- Store the thread ID
    writeIORef timerRef (Just timerThread)

    return $ TimerHandle timerRef

-- | Cancel a timeout
cancelTimeout :: TimerHandle -> IO ()
cancelTimeout (TimerHandle timerRef) = do
    -- Get the timer thread
    mThread <- readIORef timerRef

    -- Cancel the thread if it exists
    case mThread of
        Just thread -> do
            killThread thread `catch` \(_ :: SomeException) -> return ()
            writeIORef timerRef Nothing
        Nothing -> return ()

-- | Reset a timeout
resetTimeout :: TimerHandle -> IO ()
resetTimeout handle@(TimerHandle timerRef) = do
    -- Cancel the current timeout
    cancelTimeout handle

    -- Thread ID will be set by the next action that uses this handle

-- | Format a log entry
formatLogEntry :: UTCTime -> String -> String -> String
formatLogEntry time tag msg =
    formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" time ++ " [" ++ tag ++ "] " ++ msg

-- | Timeout function implementation
timeout' :: Int -> IO a -> IO (Maybe a)
timeout' micros action = do
    -- Create MVar to hold result
    resultVar <- newEmptyMVar

    -- Fork thread to run action
    actionThread <- forkIO $ do
        result <- try action
        case result of
            Left (e :: SomeException) -> putMVar resultVar (Left e)
            Right val -> putMVar resultVar (Right val)

    -- Fork thread for timeout
    timeoutThread <- forkIO $ do
        threadDelay micros
        -- Try to put, in case action already completed
        tryPutMVar resultVar (Left $ toException $ userError "Timeout")

    -- Wait for result or timeout
    result <- takeMVar resultVar

    -- Kill both threads if still running
    killThread actionThread `catch` \(_ :: SomeException) -> return ()
    killThread timeoutThread `catch` \(_ :: SomeException) -> return ()

    -- Return result
    case result of
        Left e -> case fromException e of
            Just (_ :: IOError) -> return Nothing
            Nothing -> return Nothing
        Right val -> return $ Just val

-- | Try to put a value in an MVar without blocking
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar mvar val = do
    empty <- isEmptyMVar mvar
    if empty
        then do
            putMVar mvar val
            return True
        else return False

-- | Read a message with timeout
readMessageWithTimeout :: Handle -> Int -> IO BS.ByteString
readMessageWithTimeout handle timeoutMicros = do
    -- Try to read length header with timeout
    headerResult <- timeout' timeoutMicros $ BS.hGet handle 4
    case headerResult of
        Nothing -> throwIO $ userError "Timeout waiting for message header"
        Just headerBytes ->
            if BS.length headerBytes /= 4
                then throwIO $ userError "Disconnected while reading message header"
                else do
                    -- Decode message length
                    let len = fromIntegral (fromEnum (BS.index headerBytes 0)) `shiftL` 24 .|.
                              fromIntegral (fromEnum (BS.index headerBytes 1)) `shiftL` 16 .|.
                              fromIntegral (fromEnum (BS.index headerBytes 2)) `shiftL` 8 .|.
                              fromIntegral (fromEnum (BS.index headerBytes 3))

                    -- Sanity check on message length
                    when (len > maxRequestSize) $ -- enforce size limit
                        throwIO $ userError $ "Message too large: " ++ show len ++ " bytes"

                    -- Try to read message body with timeout
                    bodyResult <- timeout' timeoutMicros $ BS.hGet handle len
                    case bodyResult of
                        Nothing -> throwIO $ userError "Timeout waiting for message body"
                        Just bodyBytes ->
                            if BS.length bodyBytes /= len
                                then throwIO $ userError "Disconnected while reading message body"
                                else return bodyBytes

-- | Create a server socket
createServerSocket :: FilePath -> IO Socket
createServerSocket socketPath = do
    -- Remove existing socket if it exists
    removeSocketIfExists socketPath

    -- Create socket directory if it doesn't exist
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Create the socket
    sock <- socket AF_UNIX Stream 0

    -- Set socket options
    setReuseAddr sock 1
    setCloseOnExecIfNeeded sock

    -- Bind to path
    bind sock (SockAddrUnix socketPath)

    -- Start listening
    listen sock 16  -- Queue up to 16 connections

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
    when exists $ removeFile path `catch` \(_ :: SomeException) -> return ()

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

-- | Decode a request message
decodeRequestMessage :: BS.ByteString -> Maybe RequestMsg
decodeRequestMessage bs =
    case decodeMessage bs of
        Just (RequestMsgWrapper msg) -> Just msg
        _ -> Nothing

-- | Decode a message
decodeMessage :: BS.ByteString -> Maybe Message
decodeMessage bs = Aeson.decodeStrict bs

-- | Helper function to get exception from SomeException
fromException :: Exception e => SomeException -> Maybe e
fromException = Control.Exception.fromException

-- | Convert response type to tag
responseTypeToTag :: Response -> ResponseTag
responseTypeToTag (BuildStartedResponse _) = TagBuildStartedResponse
responseTypeToTag (BuildResponse _) = TagBuildResponse
responseTypeToTag (BuildStatusResponse _) = TagBuildStatusResponse
responseTypeToTag (CancelBuildResponse _) = TagBuildCancelledResponse
responseTypeToTag (StoreAddResponse _) = TagStoreAddResponse
responseTypeToTag (StoreVerifyResponse _ _) = TagStoreVerifyResponse
responseTypeToTag (StorePathResponse _) = TagStorePathResponse
responseTypeToTag (StoreContentsResponse _) = TagStoreListResponse
responseTypeToTag (DerivationResponse _) = TagDerivationResponse
responseTypeToTag (DerivationStoredResponse _) = TagDerivationStoredResponse
responseTypeToTag (DerivationListResponse _) = TagDerivationListResponse
responseTypeToTag GCStartedResponse = TagGCResponse
responseTypeToTag (StatusResponse _) = TagStatusResponse
responseTypeToTag (ConfigResponse _) = TagConfigResponse
responseTypeToTag ShutdownResponse = TagShutdownResponse
responseTypeToTag (ErrorResponse _) = TagErrorResponse
responseTypeToTag PongResponse = TagPongResponse
responseTypeToTag (EvalResponse _) = TagEvalResponse

-- | Check if protocol version is compatible
isCompatibleVersion :: ProtocolVersion -> Bool
isCompatibleVersion version =
    version `elem` compatibleVersions || version == currentProtocolVersion

-- | Get path for authentication database
getAuthDbPath :: DaemonConfig -> IO FilePath
getAuthDbPath config = do
    -- Use the state file directory with a different name
    let statePath = daemonStateFile config
    let authPath = takeDirectory statePath </> "auth.db"
    return authPath

-- | Open security log
openSecurityLog :: DaemonConfig -> IO Handle
openSecurityLog config = case daemonLogFile config of
    Just logPath -> do
        -- Create directory if needed
        createDirectoryIfMissing True (takeDirectory logPath)
        -- Open log file in append mode
        openFile (takeDirectory logPath </> "security.log") AppendMode
    Nothing -> return stderr

-- | Open access log
openAccessLog :: DaemonConfig -> IO Handle
openAccessLog config = case daemonLogFile config of
    Just logPath -> do
        -- Create directory if needed
        createDirectoryIfMissing True (takeDirectory logPath)
        -- Open log file in append mode
        openFile (takeDirectory logPath </> "access.log") AppendMode
    Nothing -> return stdout

-- | Save daemon state
saveDaemonState :: DaemonConfig -> DaemonState -> IO ()
saveDaemonState config state = do
    -- Get the state file path
    let statePath = daemonStateFile config

    -- Create directory if needed
    createDirectoryIfMissing True (takeDirectory statePath)

    -- Save to a temporary file first
    let tempPath = statePath ++ ".tmp"

    -- Serialize the state
    saveResult <- try $ saveStateToFile state

    case saveResult of
        Left (e :: SomeException) ->
            hPutStrLn stderr $ "Warning: Failed to save daemon state: " ++ displayException e

        Right _ -> do
            -- Move temporary file to final location
            renameFile tempPath statePath `catch` \(_ :: SomeException) -> return ()

-- | Parse a build file
parseBuildFile :: Text -> BS.ByteString -> TenM 'Build 'Privileged (Either BuildError Derivation)
parseBuildFile filePath content = do
    -- Determine file type based on extension or content
    let ext = T.pack $ takeExtension $ T.unpack filePath

    if ext == ".drv" then do
        -- Parse derivation directly
        case deserializeDerivation content of
            Left err -> return $ Left $ SerializationError err
            Right drv -> return $ Right drv
    else if ext == ".ten" || ext == ".nix" then do
        -- This would be where the Ten expression parser would be called
        -- For now, return error
        return $ Left $ ParseError "Ten expression parsing not yet implemented"
    else do
        -- Try to detect format from content
        if isJsonContent content
            then case deserializeDerivation content of
                Left err -> return $ Left $ SerializationError err
                Right drv -> return $ Right drv
            else return $ Left $ ParseError "Unknown file format"

-- | Check if content is JSON-formatted
isJsonContent :: BS.ByteString -> Bool
isJsonContent content =
    case BS.uncons content of
        Just (c, _) | c == 123 || c == 91 -> True  -- '{' or '['
        _ -> False

-- | Take the base name of a file (without extension)
takeBaseName :: FilePath -> String
takeBaseName path =
    let filename = takeFileName path
        extension = takeExtension path
    in take (length filename - length extension) filename

-- | Store filename helper
storeFilename :: Text -> Text
storeFilename path = T.pack $ takeFileName $ T.unpack path

-- | Log a security event
logSecurityEvent :: ServerControl -> Text -> IO ()
logSecurityEvent control msg = do
    now <- getCurrentTime
    hPutStrLn (scSecurityLog control) $ formatLogEntry now "SECURITY" $ T.unpack msg

-- | Bit manipulation helpers
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b

-- | Load authentication database
loadAuthDb :: FilePath -> IO AuthDb
loadAuthDb path = do
    -- Check if the file exists
    exists <- doesFileExist path

    if not exists
        then do
            -- Create a new empty database
            now <- getCurrentTime
            let db = AuthDb {
                    adUsers = Map.empty,
                    adTokens = Map.empty,
                    adUserPermissions = Map.empty,
                    adLastModified = now
                }
            saveAuthDb path db
            return db
        else do
            -- Read the existing database
            content <- BS.readFile path `catch` \(_ :: SomeException) -> return BS.empty

            -- Parse the database
            case Aeson.eitherDecodeStrict content of
                Left err -> do
                    -- Log error and return empty database
                    hPutStrLn stderr $ "Error loading auth database: " ++ err
                    now <- getCurrentTime
                    let db = AuthDb {
                            adUsers = Map.empty,
                            adTokens = Map.empty,
                            adUserPermissions = Map.empty,
                            adLastModified = now
                        }
                    return db

                Right db -> return db

-- | Save authentication database
saveAuthDb :: FilePath -> AuthDb -> IO ()
saveAuthDb path db = do
    -- Create directory if needed
    createDirectoryIfMissing True (takeDirectory path)

    -- Update last modified time
    now <- getCurrentTime
    let db' = db { adLastModified = now }

    -- Write to a temporary file first
    let tempPath = path ++ ".tmp"
    BS.writeFile tempPath (LBS.toStrict $ Aeson.encode db')

    -- Rename to the final path
    renameFile tempPath path `catch` \(_ :: SomeException) -> return ()

-- | Rename file with fallback if atomic rename fails
renameFile :: FilePath -> FilePath -> IO ()
renameFile old new = do
    exists <- doesFileExist old
    when exists $ do
        -- Try to rename atomically
        renameResult <- try $ System.Directory.renameFile old new

        case renameResult of
            Right _ -> return ()
            Left (_ :: SomeException) -> do
                -- Fallback: read old file and write to new file
                content <- BS.readFile old
                BS.writeFile new content
                removeFile old `catch` \(_ :: SomeException) -> return ()

-- | Send a signal to a process
signalProcess :: Signal -> ProcessID -> IO ()
signalProcess signal pid = do
    -- This would use POSIX signal functions
    -- For now, we'll use a stub implementation
    if signal == 0
        then return ()  -- Just checking process existence
        else process <- System.Process.readProcess "kill" [
            case signal of
                0 -> "-0"
                s -> "-" ++ show s,
            show pid
            ] ""
    return ()

-- | POSIX signals
sigTERM, sigKILL :: Signal
sigTERM = 15
sigKILL = 9

-- | Signal type
type Signal = Int
