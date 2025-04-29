{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Ten.Daemon.Server (
    -- Server initialization and management
    startServer,
    stopServer,

    -- Connection handling
    acceptClients,
    handleClient,

    -- Request processing
    processRequest,
    dispatchDaemonRequest,
    dispatchBuilderRequest,

    -- Authentication
    authenticateClient,

    -- Specialized request handlers
    handleBuildRequestDaemon,
    handleBuildRequestBuilder,
    handleEvalRequestDaemon,
    handleEvalRequestBuilder,
    handleBuildDerivationRequestDaemon,
    handleBuildDerivationRequestBuilder,
    handleCancelBuildRequestDaemon,
    handleCancelBuildRequestBuilder,
    handleBuildStatusRequestDaemon,
    handleBuildStatusRequestBuilder,
    handleStoreRequestDaemon,
    handleStoreRequestBuilder,
    handleGCRequestDaemon,
    handleStatusRequestDaemon,
    handleStatusRequestBuilder,
    handleConfigRequestDaemon,
    handleConfigRequestBuilder,
    handleShutdownRequestDaemon,
    handleDerivationRequestDaemon,
    handleDerivationRequestBuilder,

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

import Control.Concurrent (ThreadId, forkIO, forkFinally, myThreadId, killThread, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar, newMVar, readMVar, withMVar)
import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Exception (bracket, finally, try, catch, throwIO, SomeException, AsyncException(..),
                          displayException, fromException, ErrorCall(..), IOException)
import Control.Monad (forever, void, when, unless, forM_, foldM, forM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.Except (throwError, catchError, runExceptT)
import Control.Monad.State (get, modify, runStateT)
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
import Data.Unique (newUnique)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (takeMVar, tryTakeMVar)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Word (Word32, Word64)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, getModificationTime, doesDirectoryExist, listDirectory)
import System.Environment (getEnvironment, getArgs, lookupEnv, getProgName)
import System.Exit (ExitCode(..), exitSuccess, exitFailure, exitWith)
import System.FilePath ((</>), takeDirectory, takeFileName, takeExtension, takeBaseName)
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush, hPutStrLn, stderr, stdout, stdin,
                 openFile, hGetLine, BufferMode(..), hSetBuffering)
import System.IO.Error (isDoesNotExistError, isPermissionError, catchIOError)
import System.Posix.Files (setFileMode, getFileStatus, accessTime, modificationTime, fileSize)
import System.Posix.Types (FileMode, ProcessID, UserID, GroupID, Fd)
import System.Posix.User (setUserID, setGroupID, getEffectiveUserID, getRealUserID,
                          getUserEntryForName, groupID, userID, getUserEntryForID)
import System.Posix.Process (getProcessID)
import System.Process (readProcess, createProcess, proc, waitForProcess)
import System.Random (randomRIO)
import Crypto.Hash (hash, Digest, SHA256)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA
import Data.Singletons
import Data.Singletons.TH
import Data.Kind (Type)
import Foreign.C.Types (CInt(..))

import Ten.Core
import qualified Ten.Core as Core
import Ten.Build (buildDerivation)
import Ten.Daemon.Protocol
import qualified Ten.Daemon.Protocol as Protocol
import Ten.Daemon.State
import Ten.Daemon.Auth
import Ten.Derivation (serializeDerivation, deserializeDerivation, hashDerivation)
import qualified Ten.Derivation as Derivation
import Ten.Store (storePathToFilePath, makeStorePath, addToStore, verifyStore, getStorePaths)
import qualified Ten.Store as Store
import Ten.Hash (hashByteString)
import qualified Ten.Hash as Hash
import Ten.DB.Core
import Ten.DB.Derivations
import Ten.DB.References (registerReferences)
import Ten.GC (collectGarbage, GCStats(..))

-- | Permission types for fine-grained access control
data Permission
    = PermBuild        -- Permission to build derivations
    | PermCancelBuild  -- Permission to cancel builds
    | PermQueryBuild   -- Permission to query build status
    | PermQueryStore   -- Permission to query store contents
    | PermModifyStore  -- Permission to modify store contents
    | PermRunGC        -- Permission to run garbage collection
    | PermShutdown     -- Permission to shut down the daemon
    | PermManageUsers  -- Permission to manage users
    | PermQueryDerivation -- Permission to query derivations
    | PermStoreDerivation -- Permission to store derivations
    | PermQueryStatus  -- Permission to query daemon status
    | PermAdmin        -- Administrative permission (implies all others)
    deriving (Show, Eq, Ord)

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
    ciPermissions :: TVar (Set Permission), -- ^ Client permissions
    ciPrivilegeTier :: TVar PrivilegeTier   -- ^ Assigned privilege tier
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
    scState :: DaemonState 'Daemon,  -- ^ Daemon state with phantom type
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

-- | Store command type
data StoreCommand
    = StoreAddCmd Text BS.ByteString
    | StoreVerifyCmd Text
    | StorePathCmd Text BS.ByteString
    | StoreListCmd
    | StoreReadCmd StorePath

-- | Derivation command type
data DerivationCommand
    = StoreDerivationCmd BS.ByteString
    | QueryDerivationCmd Text
    | GetDerivationForOutputCmd Text
    | ListDerivationsCmd

-- | Start the server
startServer :: Socket -> DaemonState 'Daemon -> DaemonConfig -> IO ServerControl
startServer serverSocket state config = do
    -- Initialize client tracking
    clients <- newTVarIO Map.empty
    shutdownFlag <- newTVarIO False
    buildProcesses <- newTVarIO Map.empty

    -- Initialize rate limiter
    rateLimiter <- newTVarIO Map.empty

    -- Initialize authentication database
    authDbPath <- getAuthDbPath config
    authDb <- loadAuthFile authDbPath
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

        -- Run the accept loop with daemon privileges in Build phase
        result <- runTen sBuild sDaemon (acceptClients serverSocket clients shutdownFlag state config
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
    setSocketOption sock ReuseAddr 1

    -- Set timeout options (5 seconds)
    setSocketOption sock SendTimeOut 5000
    setSocketOption sock RecvTimeOut 5000

    -- Set socket to close-on-exec using the file descriptor
    fd <- liftIO $ fdSocket sock  -- Properly sequence the IO action
    liftIO $ setCloseOnExecIfNeeded fd

-- | Stop the server
stopServer :: ServerControl -> IO ()
stopServer ServerControl{..} = do
    -- Set shutdown flag
    atomically $ writeTVar scShutdown True

    -- Wait a moment for the flag to be noticed
    threadDelay 100000 -- 0.1 seconds

    -- Get all client info
    clientMap <- readTVarIO scClients

    -- Log server shutdown
    now <- getCurrentTime
    hPutStrLn scSecurityLog $ formatLogEntry now "SERVER" "Server shutting down, closing connections"

    -- Terminate all build processes
    buildProcs <- readTVarIO scProcesses
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
    authDb <- readTVarIO scAuthDb
    authDbPath <- getAuthDbPath scConfig
    saveAuthFile authDbPath authDb

    -- Save daemon state
    saveDaemonState scConfig scState

    -- Log final shutdown
    hPutStrLn stderr "Ten daemon server shutdown complete"

-- | Terminate a process by PID
terminateProcess :: ProcessID -> IO ()
terminateProcess pid = do
    -- Try to send SIGTERM
    void $ try @SomeException $ signalProcess sigTERM pid

    -- Give it a moment to shut down
    threadDelay 1000000  -- 1 second

    -- If still running, force kill with SIGKILL
    stillRunning <- isProcessRunning pid
    when stillRunning $ do
        void $ try @SomeException $ signalProcess sigKILL pid

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
    -- Create a proper ShutdownResponse using Core types
    let response = Core.ShutdownResponse
    sendResponse handle 0 response

-- | Main loop to accept client connections with proper privilege evidence
acceptClients :: Socket -> ActiveClients -> TVar Bool -> DaemonState 'Daemon -> DaemonConfig
              -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
              -> Handle -> Handle -> TenM 'Build 'Daemon ()
acceptClients serverSocket clients shutdownFlag state config
             authDbVar rateLimiter securityLog accessLog =
    let acceptLoop = do
            -- Check if we should shut down
            shouldShutdown <- liftIO $ readTVarIO shutdownFlag
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

                                        -- Initialize client state with Builder privilege tier by default
                                        lastActivityVar <- liftIO $ newTVarIO now
                                        clientStateVar <- liftIO $ newTVarIO Authenticating
                                        requestCountVar <- liftIO $ newTVarIO 0
                                        permissionsVar <- liftIO $ newTVarIO Set.empty
                                        privilegeTierVar <- liftIO $ newTVarIO Builder -- Default to Builder tier

                                        -- Start client handler thread in Daemon context
                                        clientThread <- liftIO $ forkFinally
                                            (runClientHandler clientSocket clientHandle clients state config
                                                         authDbVar rateLimiter securityLog accessLog
                                                         lastActivityVar clientStateVar requestCountVar
                                                         permissionsVar privilegeTierVar clientAddr)
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
                                                ciPermissions = permissionsVar,
                                                ciPrivilegeTier = privilegeTierVar
                                            }

                                        -- Add to client map
                                        liftIO $ atomically $ modifyTVar clients $ Map.insert clientThread clientInfo

                                        -- Continue accepting
                                        acceptLoop
    in acceptLoop

-- | Helper to run client handler with proper environment and privileges
runClientHandler :: Socket -> Handle -> ActiveClients -> DaemonState 'Daemon -> DaemonConfig
                 -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
                 -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
                 -> TVar Int -> TVar (Set Permission) -> TVar PrivilegeTier
                 -> SockAddr -> IO ()
runClientHandler clientSocket clientHandle clients state config
                authDbVar rateLimiter securityLog accessLog
                lastActivityVar clientStateVar requestCountVar
                permissionsVar privilegeTierVar clientAddr = do
    -- Create a new build state for this client
    buildId <- BuildId <$> newUnique
    let buildState = initBuildState Build buildId

    -- Get daemon environment
    let env = initDaemonEnv (daemonTmpDir config) (daemonStorePath config) (daemonUser config)

    -- Run client handler with daemon privilege in Build phase
    result <- runTen sBuild sDaemon (handleClient clientSocket clientHandle clients state config
                              authDbVar rateLimiter securityLog accessLog
                              lastActivityVar clientStateVar requestCountVar
                              permissionsVar privilegeTierVar clientAddr)
              env buildState

    case result of
        Left err ->
            hPutStrLn stderr $ "Client handler error: " ++ show err
        Right _ ->
            return ()

-- | Handle a client connection with proper privilege transitions
handleClient :: Socket -> Handle -> ActiveClients -> DaemonState 'Daemon -> DaemonConfig
             -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
             -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
             -> TVar Int -> TVar (Set Permission) -> TVar PrivilegeTier -> SockAddr
             -> TenM 'Build 'Daemon ()
handleClient clientSocket clientHandle clients state config
            authDbVar rateLimiter securityLog accessLog
            lastActivityVar clientStateVar requestCountVar permissionsVar privilegeTierVar clientAddr = do

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

        Right (userId, authToken, permissions, tier) -> do
            -- Log successful authentication
            now <- liftIO getCurrentTime
            liftIO $ hPutStrLn accessLog $ formatLogEntry now "AUTH_SUCCESS" $
                     "Client " ++ show clientAddr ++ " authenticated as " ++
                     case userId of UserId uid -> T.unpack uid ++ " with tier " ++ show tier

            -- Store permissions and privilege tier
            liftIO $ atomically $ do
                writeTVar permissionsVar permissions
                writeTVar privilegeTierVar tier

            -- Update client info with authenticated user
            tid <- liftIO myThreadId
            updateClientAuth tid userId authToken tier

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

            -- Handle client requests based on assigned privilege tier
            case tier of
                Daemon ->
                    -- Daemon tier (privileged)
                    handleClientRequestsDaemon clientSocket clientHandle state config clientAddr
                         securityLog accessLog lastActivityVar clientStateVar
                         requestCountVar permissionsVar idleTimeout

                Builder ->
                    -- Builder tier (unprivileged)
                    handleClientRequestsBuilder clientSocket clientHandle state config clientAddr
                         securityLog accessLog lastActivityVar clientStateVar
                         requestCountVar permissionsVar idleTimeout

  where
    -- Update client info after authentication
    updateClientAuth tid userId authToken tier = liftIO $ atomically $ do
        clientMap <- readTVar clients
        case Map.lookup tid clientMap of
            Nothing -> return ()  -- Client was removed
            Just clientInfo -> do
                let updatedInfo = clientInfo {
                        ciUserId = userId,
                        ciAuthToken = authToken
                    }
                writeTVar clients (Map.insert tid updatedInfo clientMap)

-- | Process client requests with daemon privileges
handleClientRequestsDaemon :: Socket -> Handle -> DaemonState 'Daemon -> DaemonConfig -> SockAddr
                           -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
                           -> TVar Int -> TVar (Set Permission) -> TimerHandle
                           -> TenM 'Build 'Daemon ()
handleClientRequestsDaemon clientSocket clientHandle state config clientAddr
                         securityLog accessLog lastActivityVar clientStateVar
                         requestCountVar permissionsVar idleTimeout = do

    let processLoop = do
            -- Check if client is shutting down
            clientState <- liftIO $ readTVarIO clientStateVar
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

                    Right (msgData, mPayload) -> do
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

                        -- Apply rate limiting using server control
                        let control = ServerControl{..}
                        allowed <- liftIO $ checkRequestRateLimit (scRateLimiter control) clientAddr

                        if not allowed then do
                            -- Rate limit exceeded
                            liftIO $ hPutStrLn securityLog $ formatLogEntry now "RATELIMIT" $
                                     "Request rate limit exceeded for " ++ show clientAddr

                            -- Send rate limit error
                            liftIO $ sendResponse clientHandle 0 $
                                Core.ErrorResponse $ Core.DaemonError "Request rate limit exceeded"

                            -- Continue processing
                            processLoop
                        else do
                            -- Log the request
                            liftIO $ hPutStrLn accessLog $ formatLogEntry now "REQUEST" $
                                     "Client " ++ show clientAddr ++ " request #" ++ show reqCount

                            -- Process the message
                            case Protocol.deserializeDaemonRequest msgData mPayload of
                                Left err -> do
                                    -- Invalid message format
                                    liftIO $ hPutStrLn securityLog $ formatLogEntry now "MALFORMED" $
                                             "Malformed request from client " ++ show clientAddr ++ ": " ++ T.unpack err

                                    -- Send error response
                                    liftIO $ sendResponse clientHandle 0 $
                                        Core.ErrorResponse $ Core.DaemonError "Malformed request"

                                    -- Continue processing
                                    processLoop

                                Right request -> do
                                    -- Get client permissions
                                    permissions <- liftIO $ readTVarIO permissionsVar

                                    -- Validate permissions for this request
                                    permissionValid <- validateRequestPermissions request permissions securityLog clientAddr sDaemon

                                    if not permissionValid
                                        then do
                                            -- Permission denied
                                            liftIO $ sendResponse clientHandle 0 $
                                                Core.ErrorResponse $ Core.AuthError "Permission denied"

                                            -- Continue processing
                                            processLoop
                                        else do
                                            -- Process the request with daemon privileges
                                            response <- processRequest sDaemon request state config permissions

                                            -- Send the response
                                            liftIO $ sendResponse clientHandle 0 response

                                            -- Continue processing
                                            processLoop

    -- Execute the processing loop
    processLoop `finally` do
        -- Always cancel the idle timeout
        liftIO $ cancelTimeout idleTimeout

-- | Process client requests with builder privileges
handleClientRequestsBuilder :: Socket -> Handle -> DaemonState 'Daemon -> DaemonConfig -> SockAddr
                            -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
                            -> TVar Int -> TVar (Set Permission) -> TimerHandle
                            -> TenM 'Build 'Builder ()
handleClientRequestsBuilder clientSocket clientHandle state config clientAddr
                          securityLog accessLog lastActivityVar clientStateVar
                          requestCountVar permissionsVar idleTimeout = do

    let processLoop = do
            -- Check if client is shutting down
            clientState <- liftIO $ readTVarIO clientStateVar
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

                    Right (msgData, mPayload) -> do
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
                        -- Note: Handling this slightly differently for builder context
                        env <- ask
                        case runMode env of
                            ClientMode conn -> do
                                -- Use the daemon connection to check rate limit
                                let req = Request {
                                        reqId = 0,
                                        reqType = "check-rate-limit",
                                        reqParams = Map.singleton "address" (T.pack $ show clientAddr),
                                        reqPayload = Nothing
                                    }

                                -- Default to allowed if can't check
                                allowed <- liftIO $ do
                                    resp <- try $ sendRequestSync conn req 5000000
                                    case resp of
                                        Left _ -> return True
                                        Right (SuccessResponse) -> return True
                                        Right _ -> return False

                                if not allowed then do
                                    -- Rate limit exceeded
                                    liftIO $ hPutStrLn securityLog $ formatLogEntry now "RATELIMIT" $
                                            "Request rate limit exceeded for " ++ show clientAddr

                                    -- Send rate limit error
                                    liftIO $ sendResponse clientHandle 0 $
                                        Core.ErrorResponse $ Core.DaemonError "Request rate limit exceeded"

                                    -- Continue processing
                                    processLoop
                                else processParsedRequest reqCount now msgData mPayload

                            -- Default to allowed if not in client mode
                            _ -> processParsedRequest reqCount now msgData mPayload

                where
                    processParsedRequest reqCount now msgData mPayload = do
                        -- Log the request
                        liftIO $ hPutStrLn accessLog $ formatLogEntry now "REQUEST" $
                                "Client " ++ show clientAddr ++ " request #" ++ show reqCount

                        -- Process the message
                        case Protocol.deserializeDaemonRequest msgData mPayload of
                            Left err -> do
                                -- Invalid message format
                                liftIO $ hPutStrLn securityLog $ formatLogEntry now "MALFORMED" $
                                        "Malformed request from client " ++ show clientAddr ++ ": " ++ T.unpack err

                                -- Send error response
                                liftIO $ sendResponse clientHandle 0 $
                                    Core.ErrorResponse $ Core.DaemonError "Malformed request"

                                -- Continue processing
                                processLoop

                            Right request -> do
                                -- Get client permissions
                                permissions <- liftIO $ readTVarIO permissionsVar

                                -- Validate permissions for this request
                                permissionValid <- validateRequestPermissions request permissions securityLog clientAddr sBuilder

                                if not permissionValid
                                    then do
                                        -- Permission denied
                                        liftIO $ sendResponse clientHandle 0 $
                                            Core.ErrorResponse $ Core.AuthError "Permission denied"

                                        -- Continue processing
                                        processLoop
                                    else do
                                        -- Process the request with builder privileges
                                        response <- processRequest sBuilder request state config permissions

                                        -- Send the response
                                        liftIO $ sendResponse clientHandle 0 response

                                        -- Continue processing
                                        processLoop

    -- Execute the processing loop
    processLoop `finally` do
        -- Always cancel the idle timeout
        liftIO $ cancelTimeout idleTimeout

-- | Process a client request with appropriate privilege tier
processRequest :: SPrivilegeTier t -> Protocol.DaemonRequest -> DaemonState 'Daemon
               -> DaemonConfig -> Set Permission -> TenM 'Build t Core.DaemonResponse
processRequest st request state config permissions =
    -- Handle errors from dispatch
    catchError
    (case fromSing st of
        -- Daemon-tier request processing (privileged)
        Daemon -> dispatchDaemonRequest request state config permissions

        -- Builder-tier request processing (unprivileged)
        Builder -> dispatchBuilderRequest request state config permissions)
    (\(e :: BuildError) ->
        -- Handle domain-specific errors directly
        return $ Core.ErrorResponse $ e)

-- | Dispatch a request for the Daemon privilege tier (privileged)
dispatchDaemonRequest :: Protocol.DaemonRequest -> DaemonState 'Daemon
                      -> DaemonConfig -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
dispatchDaemonRequest request state config permissions = case request of
    -- Build operations
    Protocol.BuildRequest path content info ->
        handleBuildRequestDaemon path content info state permissions

    Protocol.EvalRequest path content info ->
        handleEvalRequestDaemon path content info state permissions

    Protocol.BuildDerivationRequest deriv info ->
        handleBuildDerivationRequestDaemon deriv info state permissions

    Protocol.BuildStatusRequest bid ->
        handleBuildStatusRequestDaemon bid state permissions

    Protocol.CancelBuildRequest bid ->
        handleCancelBuildRequestDaemon bid state permissions

    -- Store operations
    Protocol.StoreAddRequest path content ->
        handleStoreRequestDaemon (StoreAddCmd path content) state config permissions

    Protocol.StoreVerifyRequest path ->
        handleStoreRequestDaemon (StoreVerifyCmd (storePathToText path)) state config permissions

    Protocol.StorePathRequest path content ->
        handleStoreRequestDaemon (StorePathCmd path content) state config permissions

    Protocol.StoreListRequest ->
        handleStoreRequestDaemon StoreListCmd state config permissions

    Protocol.StoreReadRequest path ->
        handleStoreRequestDaemon (StoreReadCmd path) state config permissions

    -- GC operations
    Protocol.GCRequest params ->
        handleGCRequestDaemon (Core.gcForce params) state config permissions

    -- Information requests
    Protocol.StatusRequest ->
        handleStatusRequestDaemon state config permissions

    Protocol.ConfigRequest ->
        handleConfigRequestDaemon state config permissions

    -- Administrative operations
    Protocol.ShutdownRequest ->
        handleShutdownRequestDaemon state config permissions

    -- Derivation operations
    Protocol.StoreDerivationRequest content ->
        handleDerivationRequestDaemon (StoreDerivationCmd content) state config permissions

    Protocol.QueryDerivationRequest qType qVal _ ->
        handleDerivationRequestDaemon (QueryDerivationCmd qVal) state config permissions

    Protocol.GetDerivationForOutputRequest path ->
        handleDerivationRequestDaemon (GetDerivationForOutputCmd path) state config permissions

    Protocol.ListDerivationsRequest _ ->
        handleDerivationRequestDaemon ListDerivationsCmd state config permissions

    Protocol.PingRequest ->
        return Core.PongResponse

    -- Handle other request types appropriately
    _ -> return $ Core.ErrorResponse $ Core.DaemonError "Unsupported request type"

-- | Dispatch a request for the Builder privilege tier (unprivileged)
dispatchBuilderRequest :: Protocol.DaemonRequest -> DaemonState 'Daemon
                      -> DaemonConfig -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
dispatchBuilderRequest request state config permissions = case request of
    -- Build operations
    Protocol.BuildRequest path content info ->
        handleBuildRequestBuilder path content info state permissions

    Protocol.EvalRequest path content info ->
        handleEvalRequestBuilder path content info state permissions

    Protocol.BuildDerivationRequest deriv info ->
        handleBuildDerivationRequestBuilder deriv info state permissions

    Protocol.BuildStatusRequest bid ->
        handleBuildStatusRequestBuilder bid state permissions

    Protocol.CancelBuildRequest bid ->
        handleCancelBuildRequestBuilder bid state permissions

    -- Store operations - use Builder-specific implementations that go through protocol
    Protocol.StoreVerifyRequest path ->
        handleStoreRequestBuilder (StoreVerifyCmd (storePathToText path)) state config permissions

    Protocol.StorePathRequest path content ->
        handleStoreRequestBuilder (StorePathCmd path content) state config permissions

    Protocol.StoreListRequest ->
        handleStoreRequestBuilder StoreListCmd state config permissions

    Protocol.StoreReadRequest path ->
        handleStoreRequestBuilder (StoreReadCmd path) state config permissions

    -- Information requests
    Protocol.StatusRequest ->
        handleStatusRequestBuilder state config permissions

    Protocol.ConfigRequest ->
        handleConfigRequestBuilder state config permissions

    -- Derivation operations
    Protocol.QueryDerivationRequest qType qVal _ ->
        handleDerivationRequestBuilder (QueryDerivationCmd qVal) state config permissions

    Protocol.GetDerivationForOutputRequest path ->
        handleDerivationRequestBuilder (GetDerivationForOutputCmd path) state config permissions

    Protocol.ListDerivationsRequest _ ->
        handleDerivationRequestBuilder ListDerivationsCmd state config permissions

    Protocol.PingRequest ->
        return Core.PongResponse

    -- Operations that require daemon privileges - reject with error
    Protocol.StoreAddRequest _ _ ->
        return $ Core.ErrorResponse $ Core.PrivilegeError "Store modification requires daemon privileges"

    Protocol.StoreDerivationRequest _ ->
        return $ Core.ErrorResponse $ Core.PrivilegeError "Storing derivations requires daemon privileges"

    Protocol.GCRequest _ ->
        return $ Core.ErrorResponse $ Core.PrivilegeError "Garbage collection requires daemon privileges"

    Protocol.ShutdownRequest ->
        return $ Core.ErrorResponse $ Core.PrivilegeError "Shutdown requires daemon privileges"

    -- Handle other request types appropriately
    _ -> return $ Core.ErrorResponse $ Core.DaemonError "Unsupported request type"

-- | Validate permissions for a request with privilege tier context
validateRequestPermissions :: Protocol.DaemonRequest -> Set Permission -> Handle -> SockAddr
                           -> SPrivilegeTier t -> TenM 'Build t Bool
validateRequestPermissions request permissions securityLog clientAddr st = do
    -- Determine which permission is required for this request
    let requiredPermission = getRequiredPermission request
    let hasPermission = requiredPermission `Set.member` permissions

    -- For operations that require daemon tier, check the current privilege tier
    let requiresDaemonTier = operationRequiresDaemonTier request
    let tierSufficient = case (requiresDaemonTier, fromSing st) of
                            (True, Daemon) -> True   -- Operation requires Daemon, we have Daemon
                            (True, Builder) -> False  -- Operation requires Daemon, we have Builder
                            (False, _) -> True        -- Operation doesn't require Daemon

    -- Log permission issues
    unless (hasPermission && tierSufficient) $ do
        now <- liftIO getCurrentTime
        let reason = if not hasPermission
                     then "lacks required permission: " ++ T.unpack (permissionToText requiredPermission)
                     else "insufficient privilege tier for operation (requires Daemon)"
        liftIO $ hPutStrLn securityLog $ formatLogEntry now "PERMISSION_DENIED" $
                 "Client " ++ show clientAddr ++ " " ++ reason

    return $ hasPermission && tierSufficient

-- | Check if an operation requires the Daemon privilege tier
operationRequiresDaemonTier :: Protocol.DaemonRequest -> Bool
operationRequiresDaemonTier request = case request of
    -- Store modification operations require Daemon tier
    Protocol.StoreAddRequest{} -> True

    -- GC operations require Daemon tier
    Protocol.GCRequest{} -> True

    -- Some derivation operations require Daemon tier
    Protocol.StoreDerivationRequest{} -> True

    -- Administrative operations require Daemon tier
    Protocol.ShutdownRequest -> True
    Protocol.ConfigRequest -> True

    -- Other operations can be performed with Builder tier
    _ -> False

-- | Get the required permission for a request
getRequiredPermission :: Protocol.DaemonRequest -> Permission
getRequiredPermission request = case request of
    -- Build-related requests
    Protocol.BuildRequest{} -> PermBuild
    Protocol.EvalRequest{} -> PermBuild
    Protocol.BuildDerivationRequest{} -> PermBuild
    Protocol.BuildStatusRequest{} -> PermQueryBuild
    Protocol.CancelBuildRequest{} -> PermCancelBuild

    -- Store-related requests
    Protocol.StoreAddRequest{} -> PermModifyStore
    Protocol.StoreVerifyRequest{} -> PermQueryStore
    Protocol.StorePathRequest{} -> PermQueryStore
    Protocol.StoreListRequest -> PermQueryStore
    Protocol.StoreReadRequest{} -> PermQueryStore  -- Reading requires query permission

    -- Derivation-related requests
    Protocol.StoreDerivationRequest{} -> PermStoreDerivation
    Protocol.QueryDerivationRequest{} -> PermQueryDerivation
    Protocol.GetDerivationForOutputRequest{} -> PermQueryDerivation
    Protocol.ListDerivationsRequest{} -> PermQueryDerivation

    -- GC-related requests
    Protocol.GCRequest{} -> PermRunGC

    -- Admin requests
    Protocol.StatusRequest -> PermQueryStatus
    Protocol.ConfigRequest -> PermAdmin
    Protocol.ShutdownRequest -> PermShutdown

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

-- | Get authentication database path
getAuthDbPath :: DaemonConfig -> IO FilePath
getAuthDbPath config = do
    -- Use store directory as base for auth database
    let storeDir = daemonStorePath config
    return $ storeDir </> "var/ten/auth.db"

-- | Open security log
openSecurityLog :: DaemonConfig -> IO Handle
openSecurityLog config = do
    -- Use log file specified in config or default to stderr
    case daemonLogFile config of
        Just logPath -> do
            -- Create log directory if needed
            createDirectoryIfMissing True (takeDirectory logPath)
            -- Create or open security log file (logPath.security)
            let securityLogPath = logPath ++ ".security"
            openFile securityLogPath AppendMode
        Nothing -> return stderr

-- | Open access log
openAccessLog :: DaemonConfig -> IO Handle
openAccessLog config = do
    -- Use log file specified in config or default to stdout
    case daemonLogFile config of
        Just logPath -> do
            -- Create log directory if needed
            createDirectoryIfMissing True (takeDirectory logPath)
            -- Create or open access log file (logPath.access)
            let accessLogPath = logPath ++ ".access"
            openFile accessLogPath AppendMode
        Nothing -> return stdout

-- | Format a log entry
formatLogEntry :: UTCTime -> String -> String -> String
formatLogEntry time tag msg =
    formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" time ++ " [" ++ tag ++ "] " ++ msg

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

-- | Authenticate a client
authenticateClient :: Handle -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
                  -> SockAddr -> Handle -> IO (Either Text (UserId, AuthToken, Set Permission, PrivilegeTier))
authenticateClient handle authDbVar rateLimiter addr securityLog = do
    -- Check rate limiting for auth attempts
    allowed <- checkAuthRateLimit rateLimiter addr

    if not allowed
        then do
            -- Log rate limit violation
            now <- getCurrentTime
            hPutStrLn securityLog $ formatLogEntry now "AUTH_RATELIMIT" $
                     "Authentication rate limit exceeded for " ++ show addr
            return $ Left "Too many authentication attempts. Please try again later."
        else do
            -- Read auth message
            (msgData, mPayload) <- readMessageWithTimeout handle 5000000 -- 5 second timeout

            -- Try to parse auth message
            case Protocol.deserializeDaemonRequest msgData mPayload of
                Left err ->
                    return $ Left $ "Invalid authentication message format: " <> err

                Right (Protocol.AuthRequest content) -> do
                    -- Get auth database
                    authDb <- readTVarIO authDbVar

                    -- Create client info for logging
                    let clientInfo = T.pack $ show addr

                    -- Authenticate with proper auth function parameters
                    authResult <- authenticateUser authDb (authUser content) (authToken content) clientInfo (authRequestedTier content)

                    case authResult of
                        Left err -> return $ Left $ T.pack $ show err
                        Right (_, userId, token, tier, caps) -> do
                            -- Convert capabilities to permissions
                            let permissions = Set.fromList $ mapCapToPermissions caps
                            return $ Right (userId, token, permissions, tier)

                Right _ -> return $ Left "Expected authentication message"

  where
    -- Map protocol capabilities to internal permissions
    mapCapToPermissions :: Set DaemonCapability -> [Permission]
    mapCapToPermissions caps = concat $ map capToPerms $ Set.toList caps

    -- Map each capability to corresponding permissions
    capToPerms :: Protocol.DaemonCapability -> [Permission]
    capToPerms Protocol.StoreAccess = [PermModifyStore]
    capToPerms Protocol.SandboxCreation = [PermBuild]
    capToPerms Protocol.GarbageCollection = [PermRunGC]
    capToPerms Protocol.DerivationRegistration = [PermStoreDerivation]
    capToPerms Protocol.DerivationBuild = [PermBuild, PermCancelBuild]
    capToPerms Protocol.StoreQuery = [PermQueryStore, PermQueryDerivation]
    capToPerms Protocol.BuildQuery = [PermQueryBuild]
    capToPerms _ = []

-- | Check authentication rate limit
checkAuthRateLimit :: TVar (Map SockAddr (Int, UTCTime)) -> SockAddr -> IO Bool
checkAuthRateLimit rateLimiter addr = atomically $ do
    now <- unsafeIOToSTM getCurrentTime
    limits <- readTVar rateLimiter

    case Map.lookup addr limits of
        Nothing -> do
            -- First attempt, add to tracking
            modifyTVar' rateLimiter $ Map.insert addr (1, now)
            return True

        Just (count, timestamp) -> do
            -- Check if we're in the same time window (one minute)
            let timeWindow = 60 -- seconds
                elapsed = diffUTCTime now timestamp

            if elapsed > fromIntegral timeWindow
                then do
                    -- Reset counter for new window
                    modifyTVar' rateLimiter $ Map.insert addr (1, now)
                    return True
                else if count >= maxAuthAttemptsPerMinute
                    then return False -- Rate limit exceeded
                    else do
                        -- Increment counter
                        modifyTVar' rateLimiter $ Map.insert addr (count + 1, timestamp)
                        return True

-- | Check connection rate limit
checkConnectionRateLimit :: TVar (Map SockAddr (Int, UTCTime)) -> SockAddr -> IO Bool
checkConnectionRateLimit rateLimiter addr = atomically $ do
    now <- unsafeIOToSTM getCurrentTime
    limits <- readTVar rateLimiter

    let timeWindow = 60 -- seconds
        maxConnections = 30 -- connections per minute

    cleanupOldEntries rateLimiter now timeWindow

    case Map.lookup addr limits of
        Nothing -> do
            -- First connection, add to tracking
            modifyTVar' rateLimiter $ Map.insert addr (1, now)
            return True

        Just (count, timestamp) -> do
            -- Check if we're in the same time window
            let elapsed = diffUTCTime now timestamp

            if elapsed > fromIntegral timeWindow
                then do
                    -- Reset counter for new window
                    modifyTVar' rateLimiter $ Map.insert addr (1, now)
                    return True
                else if count >= maxConnections
                    then return False -- Rate limit exceeded
                    else do
                        -- Increment counter
                        modifyTVar' rateLimiter $ Map.insert addr (count + 1, timestamp)
                        return True

-- | Check request rate limit
checkRequestRateLimit :: TVar (Map SockAddr (Int, UTCTime)) -> SockAddr -> IO Bool
checkRequestRateLimit rateLimiter addr = atomically $ do
    now <- unsafeIOToSTM getCurrentTime
    limits <- readTVar rateLimiter

    let timeWindow = 60 -- seconds

    case Map.lookup addr limits of
        Nothing -> do
            -- First request, add to tracking
            modifyTVar' rateLimiter $ Map.insert addr (1, now)
            return True

        Just (count, timestamp) -> do
            -- Check if we're in the same time window
            let elapsed = diffUTCTime now timestamp

            if elapsed > fromIntegral timeWindow
                then do
                    -- Reset counter for new window
                    modifyTVar' rateLimiter $ Map.insert addr (1, now)
                    return True
                else if count >= maxRequestsPerMinute
                    then return False -- Rate limit exceeded
                    else do
                        -- Increment counter
                        modifyTVar' rateLimiter $ Map.insert addr (count + 1, timestamp)
                        return True

-- | Clean up old entries from rate limiter
cleanupOldEntries :: TVar (Map SockAddr (Int, UTCTime)) -> UTCTime -> Int -> STM ()
cleanupOldEntries rateLimiter now timeWindow = do
    limits <- readTVar rateLimiter

    -- Filter out entries older than timeWindow
    let updatedLimits = Map.filter (\(_, timestamp) ->
                                     diffUTCTime now timestamp <= fromIntegral timeWindow)
                                 limits

    writeTVar rateLimiter updatedLimits

-- | Wait for client connection with timeout
waitForClientConnection :: Socket -> Int -> IO (Maybe (Socket, SockAddr))
waitForClientConnection sock timeoutMicros = do
    -- Set up timeout
    result <- Core.timeout timeoutMicros $ do
        -- Accept connection
        (clientSock, clientAddr) <- accept sock
        return (clientSock, clientAddr)

    return result

-- | Try to put a value in an MVar if it's empty
tryPutMVar :: MVar a -> a -> IO ()
tryPutMVar mv val = tryTakeMVar mv >> putMVar mv val

-- | Count clients from address
countClientsFromAddress :: ActiveClients -> SockAddr -> IO Int
countClientsFromAddress clients addr = do
    clientMap <- readTVarIO clients
    return $ length $ filter (\ci -> ciAddress ci == addr) $ Map.elems clientMap

-- | Send auth failure
sendAuthFailure :: Handle -> Text -> IO ()
sendAuthFailure handle msg = do
    let response = Core.AuthResponse $ Core.AuthRejected msg
    sendResponse handle 0 response

-- | Read message with timeout
readMessageWithTimeout :: Handle -> Int -> IO (BS.ByteString, Maybe BS.ByteString)
readMessageWithTimeout handle timeoutMicros = do
    result <- Core.timeout timeoutMicros $ do
        -- Use Core's frame reading utility
        Core.receiveFramedResponse handle

    case result of
        Just (msgData, mPayload) -> return (msgData, mPayload)
        Nothing -> throwIO $ Core.DaemonError "Timeout reading message"

-- | Send a response to a request
sendResponse :: Handle -> Int -> Core.DaemonResponse -> IO ()
sendResponse handle reqId response = do
    -- Serialize the response with Core utility
    let (respData, mPayload) = Protocol.serializeDaemonResponse response

    -- Create framed response
    let framedResp = Protocol.createResponseFrame respData

    -- Send the response
    BS.hPut handle framedResp

    -- Send payload if present
    case mPayload of
        Just payload -> do
            let framedPayload = Protocol.createResponseFrame payload
            BS.hPut handle framedPayload
        Nothing -> return ()

    -- Ensure data is sent
    hFlush handle

-- | Unsafe conversion from IO to STM for timestamp operations
-- This is needed for rate limiting in STM transactions
unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM io = unsafePerformIO io `seq` return (unsafePerformIO io)

-- | Signal constants
sigTERM, sigKILL :: Int
sigTERM = 15
sigKILL = 9

-- | Signal a process
signalProcess :: Int -> ProcessID -> IO ()
signalProcess sig pid = do
    -- Use the process module to send a signal
    void $ try @SomeException $ readProcess "kill" ["-" ++ show sig, show pid] ""

-- | Handler implementations with explicit privilege tiers

-- Daemon tier (privileged) handlers
handleBuildRequestDaemon :: Text -> Maybe BS.ByteString -> Core.BuildRequestInfo
                         -> DaemonState 'Daemon -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleBuildRequestDaemon path content info state perms = do
    -- In a real implementation, this would compile and build the requested file with full daemon privileges
    buildId <- Core.BuildId <$> liftIO newUnique
    return $ Core.BuildStartedResponse buildId

handleEvalRequestDaemon :: Text -> Maybe BS.ByteString -> Core.BuildRequestInfo
                        -> DaemonState 'Daemon -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleEvalRequestDaemon path content info state perms = do
    -- In a real implementation, this would evaluate the file to a derivation with full daemon privileges
    -- For now, create a minimal derivation
    let drv = Core.Derivation {
            Core.derivName = "evaluated-" <> path,
            Core.derivHash = T.pack $ show $ Hash.hashByteString $ fromMaybe BS.empty content,
            Core.derivBuilder = Core.StorePath "0000000000000000000000000000000000000000" "bash",
            Core.derivArgs = ["-c", "echo 'hello'"],
            Core.derivInputs = Set.empty,
            Core.derivOutputs = Set.singleton $ Core.DerivationOutput "out" $
                            Core.StorePath "0000000000000000000000000000000000000000" "result",
            Core.derivEnv = Map.empty,
            Core.derivSystem = "x86_64-linux",
            Core.derivStrategy = Core.ApplicativeStrategy,
            Core.derivMeta = Map.empty
        }
    return $ Core.EvalResponse drv

handleBuildDerivationRequestDaemon :: Core.Derivation -> Core.BuildRequestInfo
                                   -> DaemonState 'Daemon -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleBuildDerivationRequestDaemon drv info state perms = do
    -- In a real implementation, this would build the derivation with full daemon privileges
    buildId <- Core.BuildId <$> liftIO newUnique
    return $ Core.BuildStartedResponse buildId

handleBuildStatusRequestDaemon :: Core.BuildId -> DaemonState 'Daemon
                               -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleBuildStatusRequestDaemon bid state perms = do
    -- In a real implementation, this would return the current build status with full daemon access
    let update = Core.BuildStatusUpdate {
            Core.buildId = bid,
            Core.buildStatus = Core.BuildPending,
            Core.buildTimeElapsed = 0.0,
            Core.buildTimeRemaining = Nothing,
            Core.buildLogUpdate = Nothing,
            Core.buildResourceUsage = Map.empty
        }
    return $ Core.BuildStatusResponse update

handleCancelBuildRequestDaemon :: Core.BuildId -> DaemonState 'Daemon
                               -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleCancelBuildRequestDaemon bid state perms = do
    -- In a real implementation, this would cancel an ongoing build with full daemon privileges
    return $ Core.CancelBuildResponse True

handleStoreRequestDaemon :: StoreCommand -> DaemonState 'Daemon
                         -> DaemonConfig -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleStoreRequestDaemon cmd state config perms = case cmd of
    StoreAddCmd path content -> do
        -- Full implementation would add the file to the store with daemon privileges
        storePath <- Store.addToStore path content
        return $ Core.StoreAddResponse storePath

    StoreVerifyCmd path -> do
        -- Verify a store path with daemon privileges
        case parseStorePath path of
            Just storePath -> do
                valid <- Store.verifyStoreIntegrity storePath
                return $ Core.StoreVerifyResponse valid
            Nothing ->
                return $ Core.StoreVerifyResponse False

    StorePathCmd path content -> do
        -- Calculate store path with daemon privileges
        storePath <- Store.makeStorePath path (Core.hashByteString content)
        return $ Core.StorePathResponse storePath

    StoreListCmd -> do
        -- List store contents with daemon privileges
        paths <- Store.getStorePaths
        return $ Core.StoreListResponse paths

    StoreReadCmd path -> do
        -- Read content from store with daemon privileges
        content <- Store.readStoreContent path
        return $ Core.StoreReadResponse content

handleGCRequestDaemon :: Bool -> DaemonState 'Daemon
                      -> DaemonConfig -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleGCRequestDaemon force state config perms = do
    -- Full implementation would perform garbage collection with daemon privileges
    (collected, live, bytes) <- Store.collectGarbage
    let stats = Core.GCStats {
            Core.gcTotal = collected + live,
            Core.gcLive = live,
            Core.gcCollected = collected,
            Core.gcBytes = bytes,
            Core.gcElapsedTime = 1.5
        }
    return $ Core.GCResultResponse stats

handleStatusRequestDaemon :: DaemonState 'Daemon
                          -> DaemonConfig -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleStatusRequestDaemon state config perms = do
    -- Get current time
    now <- liftIO getCurrentTime

    -- Calculate uptime
    let uptime = diffUTCTime now (dsStartTime state)

    -- Create status response with full daemon information
    let status = Core.DaemonStatus {
            Core.daemonStatus = "running",
            Core.daemonUptime = realToFrac uptime,
            Core.daemonActiveBuilds = 0,
            Core.daemonCompletedBuilds = 0,
            Core.daemonFailedBuilds = 0,
            Core.daemonGcRoots = 0,
            Core.daemonStoreSize = 0,
            Core.daemonStorePaths = 0
        }

    return $ Core.StatusResponse status

handleConfigRequestDaemon :: DaemonState 'Daemon
                          -> DaemonConfig -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleConfigRequestDaemon state config perms = do
    -- Return full config with daemon privileges
    return $ Core.ConfigResponse config

handleShutdownRequestDaemon :: DaemonState 'Daemon
                            -> DaemonConfig -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleShutdownRequestDaemon state config perms = do
    -- Shutdown requires daemon privileges
    return Core.ShutdownResponse

handleDerivationRequestDaemon :: DerivationCommand -> DaemonState 'Daemon
                              -> DaemonConfig -> Set Permission -> TenM 'Build 'Daemon Core.DaemonResponse
handleDerivationRequestDaemon cmd state config perms = case cmd of
    StoreDerivationCmd content -> do
        -- Parse and store the derivation with daemon privileges
        case Derivation.deserializeDerivation content of
            Left err -> return $ Core.ErrorResponse err
            Right drv -> do
                -- Store the derivation (real implementation with daemon privileges)
                path <- Core.storeDerivationDaemon drv
                return $ Core.DerivationStoredResponse path

    QueryDerivationCmd hash -> do
        -- Query the derivation with daemon privileges
        -- In a real implementation, this would query the derivation database
        return $ Core.DerivationQueryResponse []

    GetDerivationForOutputCmd path -> do
        -- Find the derivation that produced an output with daemon privileges
        -- In a real implementation, this would use daemon's direct DB access
        return $ Core.DerivationRetrievedResponse Nothing

    ListDerivationsCmd -> do
        -- List derivations with daemon privileges
        -- In a real implementation, this would access the store directly
        return $ Core.DerivationListResponse []

-- Builder tier (unprivileged) handlers
handleBuildRequestBuilder :: Text -> Maybe BS.ByteString -> Core.BuildRequestInfo
                          -> DaemonState 'Daemon -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleBuildRequestBuilder path content info state perms = do
    -- In a builder context, forward the build request to the daemon via protocol
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create build request with content
            let req = Request {
                    reqId = 0,
                    reqType = "build",
                    reqParams = Map.fromList [
                        ("path", path),
                        ("hasContent", if isJust content then "true" else "false")
                    ],
                    reqPayload = content
                }

            -- Send request to daemon and return response
            respResult <- liftIO $ sendRequestSync conn req 60000000
            case respResult of
                Left err -> return $ Core.ErrorResponse err
                Right resp -> return resp

        _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleEvalRequestBuilder :: Text -> Maybe BS.ByteString -> Core.BuildRequestInfo
                         -> DaemonState 'Daemon -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleEvalRequestBuilder path content info state perms = do
    -- In a builder context, forward the eval request to the daemon via protocol
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create eval request with content
            let req = Request {
                    reqId = 0,
                    reqType = "eval",
                    reqParams = Map.fromList [
                        ("path", path),
                        ("hasContent", if isJust content then "true" else "false")
                    ],
                    reqPayload = content
                }

            -- Send request to daemon and return response
            respResult <- liftIO $ sendRequestSync conn req 60000000
            case respResult of
                Left err -> return $ Core.ErrorResponse err
                Right resp -> return resp

        _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleBuildDerivationRequestBuilder :: Core.Derivation -> Core.BuildRequestInfo
                                    -> DaemonState 'Daemon -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleBuildDerivationRequestBuilder drv info state perms = do
    -- In a builder context, forward the build derivation request to the daemon via protocol
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Serialize the derivation
            let serialized = Derivation.serializeDerivation drv

            -- Create build derivation request
            let req = Request {
                    reqId = 0,
                    reqType = "build-derivation",
                    reqParams = Map.singleton "hasDerivation" "true",
                    reqPayload = Just serialized
                }

            -- Send request to daemon and return response
            respResult <- liftIO $ sendRequestSync conn req 60000000
            case respResult of
                Left err -> return $ Core.ErrorResponse err
                Right resp -> return resp

        _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleBuildStatusRequestBuilder :: Core.BuildId -> DaemonState 'Daemon
                                -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleBuildStatusRequestBuilder bid state perms = do
    -- In a builder context, forward the build status request to the daemon via protocol
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create build status request
            let req = Request {
                    reqId = 0,
                    reqType = "build-status",
                    reqParams = Map.singleton "buildId" (renderBuildId bid),
                    reqPayload = Nothing
                }

            -- Send request to daemon and return response
            respResult <- liftIO $ sendRequestSync conn req 10000000
            case respResult of
                Left err -> return $ Core.ErrorResponse err
                Right resp -> return resp

        _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleCancelBuildRequestBuilder :: Core.BuildId -> DaemonState 'Daemon
                                -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleCancelBuildRequestBuilder bid state perms = do
    -- In a builder context, forward the cancel build request to the daemon via protocol
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create cancel build request
            let req = Request {
                    reqId = 0,
                    reqType = "cancel-build",
                    reqParams = Map.singleton "buildId" (renderBuildId bid),
                    reqPayload = Nothing
                }

            -- Send request to daemon and return response
            respResult <- liftIO $ sendRequestSync conn req 10000000
            case respResult of
                Left err -> return $ Core.ErrorResponse err
                Right resp -> return resp

        _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleStoreRequestBuilder :: StoreCommand -> DaemonState 'Daemon
                          -> DaemonConfig -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleStoreRequestBuilder cmd state config perms = case cmd of
    StoreAddCmd _ _ ->
        -- Builder cannot directly add to store, this should be blocked earlier
        return $ Core.ErrorResponse $ Core.PrivilegeError "Store modification requires daemon privileges"

    StoreVerifyCmd path -> do
        -- Forward the verify request to the daemon via protocol
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let req = Request {
                        reqId = 0,
                        reqType = "store-verify",
                        reqParams = Map.singleton "path" path,
                        reqPayload = Nothing
                    }

                respResult <- liftIO $ sendRequestSync conn req 10000000
                case respResult of
                    Left err -> return $ Core.ErrorResponse err
                    Right resp -> return resp

            _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

    StorePathCmd path content -> do
        -- Forward the path request to the daemon via protocol
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let req = Request {
                        reqId = 0,
                        reqType = "store-path",
                        reqParams = Map.singleton "path" path,
                        reqPayload = Just content
                    }

                respResult <- liftIO $ sendRequestSync conn req 10000000
                case respResult of
                    Left err -> return $ Core.ErrorResponse err
                    Right resp -> return resp

            _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

    StoreListCmd -> do
        -- Forward the list request to the daemon via protocol
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let req = Request {
                        reqId = 0,
                        reqType = "store-list",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                respResult <- liftIO $ sendRequestSync conn req 30000000
                case respResult of
                    Left err -> return $ Core.ErrorResponse err
                    Right resp -> return resp

            _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

    StoreReadCmd path -> do
        -- Try to read directly if accessible, otherwise forward to daemon
        env <- ask
        let filePath = storePathToFilePath path env
        fileExists <- liftIO $ doesFileExist filePath

        if fileExists
            then do
                -- Try direct read
                contentResult <- liftIO $ try $ BS.readFile filePath
                case contentResult of
                    Right content ->
                        return $ Core.StoreReadResponse content
                    Left (_ :: IOException) ->
                        -- Forward to daemon if direct read fails
                        forwardReadToDaemon path
            else
                -- Path not directly accessible, forward to daemon
                forwardReadToDaemon path
      where
        forwardReadToDaemon p = do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    let req = Request {
                            reqId = 0,
                            reqType = "store-read",
                            reqParams = Map.singleton "path" (storePathToText p),
                            reqPayload = Nothing
                        }

                    respResult <- liftIO $ sendRequestSync conn req 30000000
                    case respResult of
                        Left err -> return $ Core.ErrorResponse err
                        Right resp -> return resp

                _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleStatusRequestBuilder :: DaemonState 'Daemon
                           -> DaemonConfig -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleStatusRequestBuilder state config perms = do
    -- Forward the status request to the daemon via protocol
    env <- ask
    case runMode env of
        ClientMode conn -> do
            let req = Request {
                    reqId = 0,
                    reqType = "status",
                    reqParams = Map.empty,
                    reqPayload = Nothing
                }

            respResult <- liftIO $ sendRequestSync conn req 10000000
            case respResult of
                Left err -> return $ Core.ErrorResponse err
                Right resp -> return resp

        _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleConfigRequestBuilder :: DaemonState 'Daemon
                           -> DaemonConfig -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleConfigRequestBuilder state config perms = do
    -- Forward the config request to the daemon via protocol, but access might be restricted
    env <- ask
    case runMode env of
        ClientMode conn -> do
            let req = Request {
                    reqId = 0,
                    reqType = "config",
                    reqParams = Map.empty,
                    reqPayload = Nothing
                }

            respResult <- liftIO $ sendRequestSync conn req 10000000
            case respResult of
                Left err -> return $ Core.ErrorResponse err
                Right resp -> return resp

        _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

handleDerivationRequestBuilder :: DerivationCommand -> DaemonState 'Daemon
                               -> DaemonConfig -> Set Permission -> TenM 'Build 'Builder Core.DaemonResponse
handleDerivationRequestBuilder cmd state config perms = case cmd of
    StoreDerivationCmd _ ->
        -- Builder cannot directly store derivations, this should be blocked earlier
        return $ Core.ErrorResponse $ Core.PrivilegeError "Storing derivations requires daemon privileges"

    QueryDerivationCmd hash -> do
        -- Forward the query request to the daemon via protocol
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let req = Request {
                        reqId = 0,
                        reqType = "query-derivation",
                        reqParams = Map.fromList [
                            ("queryType", "query"),
                            ("queryValue", hash)
                        ],
                        reqPayload = Nothing
                    }

                respResult <- liftIO $ sendRequestSync conn req 30000000
                case respResult of
                    Left err -> return $ Core.ErrorResponse err
                    Right resp -> return resp

            _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

    GetDerivationForOutputCmd path -> do
        -- Forward the request to the daemon via protocol
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let req = Request {
                        reqId = 0,
                        reqType = "get-derivation-for-output",
                        reqParams = Map.singleton "path" path,
                        reqPayload = Nothing
                    }

                respResult <- liftIO $ sendRequestSync conn req 30000000
                case respResult of
                    Left err -> return $ Core.ErrorResponse err
                    Right resp -> return resp

            _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

    ListDerivationsCmd -> do
        -- Forward the list request to the daemon via protocol
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let req = Request {
                        reqId = 0,
                        reqType = "list-derivations",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                respResult <- liftIO $ sendRequestSync conn req 30000000
                case respResult of
                    Left err -> return $ Core.ErrorResponse err
                    Right resp -> return resp

            _ -> return $ Core.ErrorResponse $ Core.DaemonError "Not connected to daemon"

-- | Create a server socket
createServerSocket :: FilePath -> IO Socket
createServerSocket path = do
    -- Remove existing socket file if it exists
    exists <- doesFileExist path
    when exists $ removeFile path

    -- Create directory if needed
    createDirectoryIfMissing True (takeDirectory path)

    -- Create the socket
    sock <- socket AF_UNIX Stream 0

    -- Set options
    setSocketOption sock ReuseAddr 1

    -- Bind to path
    bind sock (SockAddrUnix path)

    -- Listen for connections
    listen sock 10

    -- Set permissions to allow all users to connect
    setFileMode path 0o666

    return sock

-- | Close a server socket
closeServerSocket :: Socket -> FilePath -> IO ()
closeServerSocket sock path = do
    -- Close the socket
    close sock

    -- Remove the socket file
    removeFile path `catch` \(_ :: SomeException) -> return ()

-- | Add a client to the active clients map
addClient :: ActiveClients -> ClientInfo -> IO ()
addClient clients info = atomically $
    modifyTVar' clients $ Map.insert (ciThreadId info) info

-- | Remove a client from the active clients map
removeClient :: ActiveClients -> ThreadId -> IO ()
removeClient clients tid = atomically $
    modifyTVar' clients $ Map.delete tid

-- | Broadcast a message to all clients
broadcastToClients :: ActiveClients -> BS.ByteString -> IO ()
broadcastToClients clients msg = do
    clientMap <- readTVarIO clients

    -- Send to each client that is in Active state
    forM_ (Map.elems clientMap) $ \info -> do
        state <- readTVarIO (ciState info)
        when (state == Active) $ do
            catch (BS.hPut (ciHandle info) msg >> hFlush (ciHandle info))
                  (\(_ :: SomeException) -> return ())

-- | Log a security event
logSecurityEvent :: ServerControl -> Text -> IO ()
logSecurityEvent control msg = do
    now <- getCurrentTime
    hPutStrLn (scSecurityLog control) $ formatLogEntry now "SECURITY" $ T.unpack msg
    hFlush (scSecurityLog control)

-- | Save daemon state
saveDaemonState :: DaemonConfig -> DaemonState 'Daemon -> IO ()
saveDaemonState config state = do
    -- Save state to file
    saveStateToFile state
