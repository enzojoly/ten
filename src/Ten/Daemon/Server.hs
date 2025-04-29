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

import Control.Concurrent (ThreadId, forkIO, forkFinally, myThreadId, killThread, threadDelay, MVar, newEmptyMVar, putMVar, takeMVar, newMVar, readMVar, withMVar)
import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async, cancel, waitCatch)
import Control.Exception (bracket, finally, try, catch, handle, throwIO, SomeException, AsyncException(..),
                          displayException, fromException, ErrorCall(..))
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
import System.Posix.Types (FileMode, ProcessID, UserID, GroupID)
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

import Ten.Core
import qualified Ten.Core as Core
import Ten.Build (buildDerivation)
import Ten.Daemon.Protocol
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

-- | Message wrapper for requests
data RequestWrapper = RequestWrapper RequestMessage

-- | Request message structure
data RequestMessage = RequestMessage
    { reqTag :: RequestTag
    , reqPayload :: Aeson.Value
    , reqRequiresAuth :: Bool
    }

-- | Response tag types for categorizing responses
data ResponseTag
    = TagAuthResponse
    | TagBuildStartedResponse
    | TagBuildResultResponse
    | TagBuildStatusResponse
    | TagBuildOutputResponse
    | TagBuildListResponse
    | TagCancelBuildResponse
    | TagStoreAddResponse
    | TagStoreVerifyResponse
    | TagStorePathResponse
    | TagStoreListResponse
    | TagStoreReadResponse
    | TagDerivationResponse
    | TagDerivationStoredResponse
    | TagDerivationRetrievedResponse
    | TagDerivationQueryResponse
    | TagDerivationOutputResponse
    | TagDerivationListResponse
    | TagGCResultResponse
    | TagGCStartedResponse
    | TagGCStatusResponse
    | TagGCRootAddedResponse
    | TagGCRootRemovedResponse
    | TagGCRootListResponse
    | TagPongResponse
    | TagShutdownResponse
    | TagStatusResponse
    | TagConfigResponse
    | TagEvalResponse
    | TagErrorResponse
    | TagSuccessResponse

-- | Response message structure
data ResponseMessage = ResponseMessage
    { respTag :: ResponseTag
    , respPayload :: Aeson.Value
    , respRequiresAuth :: Bool
    }

-- | Request tag type
data RequestTag
    = TagAuthRequest
    | TagBuildRequest
    | TagEvalRequest
    | TagBuildDerivationRequest
    | TagBuildStatusRequest
    | TagCancelBuildRequest
    | TagQueryBuildOutputRequest
    | TagListBuildsRequest
    | TagStoreAddRequest
    | TagStoreVerifyRequest
    | TagStorePathRequest
    | TagStoreListRequest
    | TagStoreReadRequest
    | TagStoreDerivationRequest
    | TagRetrieveDerivationRequest
    | TagQueryDerivationRequest
    | TagGetDerivationForOutputRequest
    | TagListDerivationsRequest
    | TagGCRequest
    | TagGCStatusRequest
    | TagAddGCRootRequest
    | TagRemoveGCRootRequest
    | TagListGCRootsRequest
    | TagPingRequest
    | TagShutdownRequest
    | TagStatusRequest
    | TagConfigRequest
    deriving (Show, Eq)

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
        result <- runTen sDaemon (acceptClients serverSocket clients shutdownFlag state config
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
-- | Send a shutdown notice to a client
sendShutdownNotice :: Handle -> IO ()
sendShutdownNotice handle = do
    -- Use the DaemonResponse directly
    let response = ShutdownResponse
    -- Use Protocol's serialization method to convert to bytes
    let (respData, mPayload) = serializeDaemonResponse response
    -- Use Protocol's frame creation function
    let framedResp = createResponseFrame respData
    -- Send the frame
    BS.hPut handle framedResp
    -- Send payload if present
    case mPayload of
        Just payload -> do
            let framedPayload = createResponseFrame payload
            BS.hPut handle framedPayload
        Nothing -> return ()
    -- Flush to ensure delivery
    hFlush handle

-- | Main loop to accept client connections with proper privilege evidence
acceptClients :: Socket -> ActiveClients -> TVar Bool -> DaemonState 'Daemon -> DaemonConfig
              -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
              -> Handle -> Handle -> TenM 'Build 'Daemon ()
acceptClients serverSocket clients shutdownFlag state config
             authDbVar rateLimiter securityLog accessLog = do
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
                shouldShutdown <- liftIO $ readTVarIO shutdownFlag
                unless shouldShutdown $
                    acceptClients serverSocket clients shutdownFlag state config
                                  authDbVar rateLimiter securityLog accessLog

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

    -- Run client handler with daemon privilege - will transition to builder privilege after auth
    result <- runTen sDaemon (handleClient clientSocket clientHandle clients state config
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

            -- Handle client requests with proper privilege transition
            case tier of
                Daemon ->
                    -- Continue processing with Daemon privileges
                    handleClientRequests clientSocket clientHandle state config clientAddr
                                        securityLog accessLog lastActivityVar clientStateVar
                                        requestCountVar permissionsVar idleTimeout sDaemon

                Builder ->
                    -- Drop privileges to Builder tier with a transition
                    withPrivilegeTransition DropPrivilege id $
                        handleClientRequests clientSocket clientHandle state config clientAddr
                                           securityLog accessLog lastActivityVar clientStateVar
                                           requestCountVar permissionsVar idleTimeout sBuilder

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

-- | Process client requests with privilege-aware dispatch
handleClientRequests :: Socket -> Handle -> DaemonState 'Daemon -> DaemonConfig -> SockAddr
                     -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
                     -> TVar Int -> TVar (Set Permission) -> TimerHandle
                     -> SPrivilegeTier t  -- Explicit privilege singleton evidence
                     -> TenM 'Build t ()  -- Result type has same privilege tier
handleClientRequests clientSocket clientHandle state config clientAddr
                    securityLog accessLog lastActivityVar clientStateVar
                    requestCountVar permissionsVar idleTimeout st = do

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
                            case decodeMessage msgBytes of
                                Nothing -> do
                                    -- Invalid message format
                                    liftIO $ hPutStrLn securityLog $ formatLogEntry now "MALFORMED" $
                                             "Malformed request from client " ++ show clientAddr

                                    -- Send error response
                                    liftIO $ sendResponse clientHandle 0 $
                                        ErrorResponse $ DaemonError "Malformed request"

                                    -- Continue processing
                                    processLoop

                                Just (RequestWrapper (RequestMessage reqTag reqPayload _)) -> do
                                    -- Get client permissions
                                    permissions <- liftIO $ readTVarIO permissionsVar

                                    -- Try to parse the request
                                    case Aeson.fromJSON reqPayload of
                                        Aeson.Error err -> do
                                            -- Invalid request format
                                            liftIO $ hPutStrLn securityLog $ formatLogEntry now "INVALID" $
                                                      "Invalid request format: " ++ err

                                            -- Send error response
                                            liftIO $ sendResponse clientHandle 0 $
                                                ErrorResponse $ DaemonError $ "Invalid request format: " <> T.pack err

                                            -- Continue processing
                                            processLoop

                                        Aeson.Success request -> do
                                            -- Validate permissions for this request
                                            permissionValid <- validateRequestPermissions request permissions securityLog clientAddr st

                                            if not permissionValid
                                                then do
                                                    -- Permission denied
                                                    liftIO $ sendResponse clientHandle 0 $
                                                        ErrorResponse $ AuthError "Permission denied"

                                                    -- Continue processing
                                                    processLoop
                                                else do
                                                    -- Process the request with proper privilege context
                                                    response <- processRequest st request state config permissions

                                                    -- Send the response
                                                    liftIO $ sendResponse clientHandle 0 response

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

-- | Process a client request with proper privilege tier
processRequest :: SPrivilegeTier t -> DaemonRequest -> DaemonState 'Daemon
               -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
processRequest st request state config permissions = do
    -- Dispatch to the appropriate handler based on privilege tier
    dispatchRequest st request state config permissions `catch` \(e :: SomeException) -> do
        -- Convert any errors to ErrorResponse
        case fromException e of
            Just ThreadKilled -> return $ ErrorResponse $ DaemonError "Thread killed"
            _ -> return $ ErrorResponse $ DaemonError $ "Request processing error: " <> T.pack (displayException e)

-- | Dispatch a request to the appropriate handler based on privilege tier
dispatchRequest :: SPrivilegeTier t -> DaemonRequest -> DaemonState 'Daemon
                -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
dispatchRequest st request state config permissions = case request of
    -- Build operations (allowed for both privilege tiers)
    BuildRequest path content info ->
        handleBuildRequest st path content info state permissions

    EvalRequest path content info ->
        handleEvalRequest st path content info state permissions

    BuildDerivationRequest deriv info ->
        handleBuildDerivationRequest st deriv info state permissions

    BuildStatusRequest bid ->
        handleBuildStatusRequest st bid state permissions

    CancelBuildRequest bid ->
        handleCancelBuildRequest st bid state permissions

    -- Store operations (some require Daemon tier)
    StoreAddRequest path content ->
        handleStoreRequest st (StoreAddCmd path content) state config permissions

    StoreVerifyRequest path ->
        handleStoreRequest st (StoreVerifyCmd (storePathToText path)) state config permissions

    StorePathRequest path content ->
        handleStoreRequest st (StorePathCmd path content) state config permissions

    StoreListRequest ->
        handleStoreRequest st StoreListCmd state config permissions

    StoreReadRequest path ->
        handleStoreRequest st (StoreReadCmd path) state config permissions

    -- GC operations (require Daemon tier)
    GCRequest params ->
        handleGCRequest st (gcForce params) state config permissions

    -- Information requests
    StatusRequest ->
        handleStatusRequest st state config permissions

    ConfigRequest ->
        handleConfigRequest st state config permissions

    -- Administrative operations
    ShutdownRequest ->
        handleShutdownRequest st state config permissions

    -- Derivation operations
    StoreDerivationRequest content ->
        handleDerivationRequest st (StoreDerivationCmd content) state config permissions

    QueryDerivationRequest qType qVal _ ->
        handleDerivationRequest st (QueryDerivationCmd qVal) state config permissions

    GetDerivationForOutputRequest path ->
        handleDerivationRequest st (GetDerivationForOutputCmd path) state config permissions

    ListDerivationsRequest _ ->
        handleDerivationRequest st ListDerivationsCmd state config permissions

    PingRequest ->
        return PongResponse

    -- Handle other request types appropriately
    _ -> return $ ErrorResponse $ DaemonError "Unsupported request type"

-- | Validate permissions for a request with privilege tier context
validateRequestPermissions :: DaemonRequest -> Set Permission -> Handle -> SockAddr
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
operationRequiresDaemonTier :: DaemonRequest -> Bool
operationRequiresDaemonTier request = case request of
    -- Store modification operations require Daemon tier
    StoreAddRequest{} -> True

    -- GC operations require Daemon tier
    GCRequest{} -> True

    -- Some derivation operations require Daemon tier
    StoreDerivationRequest{} -> True

    -- Administrative operations require Daemon tier
    ShutdownRequest -> True
    ConfigRequest -> True

    -- Other operations can be performed with Builder tier
    _ -> False

-- | Get the required permission for a request
getRequiredPermission :: DaemonRequest -> Permission
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
    StoreReadRequest{} -> PermQueryStore  -- Reading requires query permission

    -- Derivation-related requests
    StoreDerivationRequest{} -> PermStoreDerivation
    QueryDerivationRequest{} -> PermQueryDerivation
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
            msgBytes <- readMessageWithTimeout handle 5000000 -- 5 second timeout

            -- Try to parse auth message
            case decodeMessage msgBytes of
                Nothing ->
                    return $ Left "Invalid authentication message format"

                Just (RequestWrapper (RequestMessage _ payload _)) -> do
                    -- Try to parse as auth request
                    case Aeson.fromJSON payload of
                        Aeson.Error err ->
                            return $ Left $ "Invalid authentication payload: " <> T.pack err

                        Aeson.Success (AuthRequest content) -> do
                            -- Get auth database
                            authDb <- readTVarIO authDbVar

                            -- Authenticate
                            authResult <- authenticateUser authDb (authUser content) (authToken content)

                            case authResult of
                                Left err -> return $ Left err
                                Right (userId, token, permissions) -> do
                                    -- Determine privilege tier based on permissions
                                    let tier = if PermAdmin `Set.member` permissions
                                              then Daemon
                                              else Builder

                                    return $ Right (userId, token, permissions, tier)

                _ -> return $ Left "Expected authentication message"

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
    let response = ResponseWrapper $ ResponseMessage {
            respTag = TagAuthResponse,
            respPayload = Aeson.toJSON $ AuthResponse $ AuthRejected msg,
            respRequiresAuth = False
        }

    BS.hPut handle $ serializeMessage response
    hFlush handle

-- | Read message with timeout
readMessageWithTimeout :: Handle -> Int -> IO BS.ByteString
readMessageWithTimeout handle timeoutMicros = do
    result <- Core.timeout timeoutMicros $ do
        -- Read message header (4 bytes for length)
        lenBytes <- BS.hGet handle 4

        -- Parse message length
        let len = fromIntegral (BS.index lenBytes 0) `shiftL` 24 .|.
                  fromIntegral (BS.index lenBytes 1) `shiftL` 16 .|.
                  fromIntegral (BS.index lenBytes 2) `shiftL` 8 .|.
                  fromIntegral (BS.index lenBytes 3)

        -- Sanity check message size
        when (len > maxRequestSize) $
            throwIO $ DaemonError $ "Message too large: " <> T.pack (show len)

        -- Read message body
        msgBody <- BS.hGet handle len

        return $ BS.append lenBytes msgBody

    case result of
        Just msg -> return msg
        Nothing -> throwIO $ DaemonError "Timeout reading message"

-- | Decode message
decodeMessage :: BS.ByteString -> Maybe Message
decodeMessage bs | BS.length bs < 4 = Nothing
                 | otherwise = do
    -- Extract length
    let len = fromIntegral (BS.index bs 0) `shiftL` 24 .|.
              fromIntegral (BS.index bs 1) `shiftL` 16 .|.
              fromIntegral (BS.index bs 2) `shiftL` 8 .|.
              fromIntegral (BS.index bs 3)

    -- Check if we have enough data
    if BS.length bs < 4 + len
        then Nothing
        else do
            -- Extract message body
            let body = BS.drop 4 $ BS.take (4 + len) bs

            -- Parse JSON
            case Aeson.eitherDecodeStrict body of
                Left _ -> Nothing
                Right val -> Just val

-- | Send a response to a request
sendResponse :: Handle -> Int -> DaemonResponse -> IO ()
sendResponse handle reqId response = do
    let message = createResponseMessage reqId response
    BS.hPut handle $ serializeMessage message
    hFlush handle

-- | Create a response message
createResponseMessage :: Int -> DaemonResponse -> Message
createResponseMessage reqId resp =
    let tag = responseTypeToTag resp
        payload = Aeson.toJSON resp
        requiresAuth = requiresAuthentication resp
    in ResponseWrapper $ ResponseMessage tag payload requiresAuth

-- | Convert response type to tag
responseTypeToTag :: DaemonResponse -> ResponseTag
responseTypeToTag (AuthResponse _) = TagAuthResponse
responseTypeToTag (BuildStartedResponse _) = TagBuildStartedResponse
responseTypeToTag (BuildResultResponse _) = TagBuildResultResponse
responseTypeToTag (BuildStatusResponse _) = TagBuildStatusResponse
responseTypeToTag (BuildOutputResponse _) = TagBuildOutputResponse
responseTypeToTag (BuildListResponse _) = TagBuildListResponse
responseTypeToTag (CancelBuildResponse _) = TagCancelBuildResponse
responseTypeToTag (StoreAddResponse _) = TagStoreAddResponse
responseTypeToTag (StoreVerifyResponse _) = TagStoreVerifyResponse
responseTypeToTag (StorePathResponse _) = TagStorePathResponse
responseTypeToTag (StoreListResponse _) = TagStoreListResponse
responseTypeToTag (StoreReadResponse _) = TagStoreReadResponse
responseTypeToTag (DerivationResponse _) = TagDerivationResponse
responseTypeToTag (DerivationStoredResponse _) = TagDerivationStoredResponse
responseTypeToTag (DerivationRetrievedResponse _) = TagDerivationRetrievedResponse
responseTypeToTag (DerivationQueryResponse _) = TagDerivationQueryResponse
responseTypeToTag (DerivationOutputResponse _) = TagDerivationOutputResponse
responseTypeToTag (DerivationListResponse _) = TagDerivationListResponse
responseTypeToTag (GCResultResponse _) = TagGCResultResponse
responseTypeToTag (GCStartedResponse) = TagGCStartedResponse
responseTypeToTag (GCStatusResponse _) = TagGCStatusResponse
responseTypeToTag (GCRootAddedResponse _) = TagGCRootAddedResponse
responseTypeToTag (GCRootRemovedResponse _) = TagGCRootRemovedResponse
responseTypeToTag (GCRootListResponse _) = TagGCRootListResponse
responseTypeToTag (PongResponse) = TagPongResponse
responseTypeToTag (ShutdownResponse) = TagShutdownResponse
responseTypeToTag (StatusResponse _) = TagStatusResponse
responseTypeToTag (ConfigResponse _) = TagConfigResponse
responseTypeToTag (EvalResponse _) = TagEvalResponse
responseTypeToTag (ErrorResponse _) = TagErrorResponse
responseTypeToTag (SuccessResponse) = TagSuccessResponse

-- | Check if response requires authentication
requiresAuthentication :: DaemonResponse -> Bool
requiresAuthentication (AuthResponse _) = False
requiresAuthentication (ErrorResponse _) = False
requiresAuthentication _ = True

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
    void $ try $ readProcess "kill" ["-" ++ show sig, show pid] ""

-- | Serialize a message for transmission
serializeMessage :: Message -> BS.ByteString
serializeMessage message =
    let encoded = case message of
            RequestWrapper req ->
                -- Serialize request header and payload
                let header = Aeson.encode $ Aeson.object [
                        "type" Aeson..= ("request" :: Text),
                        "tag" Aeson..= show (reqTag req),
                        "requiresAuth" Aeson..= reqRequiresAuth req,
                        "payload" Aeson..= reqPayload req
                        ]
                    headerLen = BS.length $ LBS.toStrict header
                    lenBytes = BS.pack [
                        fromIntegral (headerLen `shiftR` 24) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 16) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 8) .&. 0xFF,
                        fromIntegral headerLen .&. 0xFF
                        ]
                in BS.concat [lenBytes, LBS.toStrict header]

            ResponseWrapper resp ->
                -- Serialize response header and payload
                let header = Aeson.encode $ Aeson.object [
                        "type" Aeson..= ("response" :: Text),
                        "tag" Aeson..= show (respTag resp),
                        "requiresAuth" Aeson..= respRequiresAuth resp,
                        "payload" Aeson..= respPayload resp
                        ]
                    headerLen = BS.length $ LBS.toStrict header
                    lenBytes = BS.pack [
                        fromIntegral (headerLen `shiftR` 24) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 16) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 8) .&. 0xFF,
                        fromIntegral headerLen .&. 0xFF
                        ]
                in BS.concat [lenBytes, LBS.toStrict header]
    in encoded

-- | Handler implementations
handleBuildRequest :: SPrivilegeTier t -> Text -> Maybe BS.ByteString -> BuildRequestInfo
                   -> DaemonState 'Daemon -> Set Permission -> TenM 'Build t DaemonResponse
handleBuildRequest st path content info state perms = do
    -- In a real implementation, this would compile and build the requested file
    buildId <- BuildId <$> liftIO newUnique
    return $ BuildStartedResponse buildId

handleEvalRequest :: SPrivilegeTier t -> Text -> Maybe BS.ByteString -> BuildRequestInfo
                  -> DaemonState 'Daemon -> Set Permission -> TenM 'Build t DaemonResponse
handleEvalRequest st path content info state perms = do
    -- In a real implementation, this would evaluate the file to a derivation
    -- For now, create a minimal derivation
    let drv = Derivation {
            derivName = "evaluated-" <> path,
            derivHash = Hash.hashByteString $ fromMaybe BS.empty content,
            derivBuilder = StorePath "0000000000000000000000000000000000000000" "bash",
            derivArgs = ["-c", "echo 'hello'"],
            derivInputs = Set.empty,
            derivOutputs = Set.singleton $ DerivationOutput "out" $
                            StorePath "0000000000000000000000000000000000000000" "result",
            derivEnv = Map.empty,
            derivSystem = "x86_64-linux",
            derivStrategy = ApplicativeStrategy,
            derivMeta = Map.empty
        }
    return $ EvalResponse drv

handleBuildDerivationRequest :: SPrivilegeTier t -> Derivation -> BuildRequestInfo
                             -> DaemonState 'Daemon -> Set Permission -> TenM 'Build t DaemonResponse
handleBuildDerivationRequest st drv info state perms = do
    -- In a real implementation, this would build the derivation
    buildId <- BuildId <$> liftIO newUnique
    return $ BuildStartedResponse buildId

handleBuildStatusRequest :: SPrivilegeTier t -> BuildId -> DaemonState 'Daemon
                         -> Set Permission -> TenM 'Build t DaemonResponse
handleBuildStatusRequest st bid state perms = do
    -- In a real implementation, this would return the current build status
    let update = BuildStatusUpdate {
            buildId = bid,
            buildStatus = BuildPending,
            buildTimeElapsed = 0.0,
            buildTimeRemaining = Nothing,
            buildLogUpdate = Nothing,
            buildResourceUsage = Map.empty
        }
    return $ BuildStatusResponse update

handleCancelBuildRequest :: SPrivilegeTier t -> BuildId -> DaemonState 'Daemon
                         -> Set Permission -> TenM 'Build t DaemonResponse
handleCancelBuildRequest st bid state perms = do
    -- In a real implementation, this would cancel an ongoing build
    return $ CancelBuildResponse True

handleStoreRequest :: SPrivilegeTier t -> StoreCommand -> DaemonState 'Daemon
                   -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
handleStoreRequest st cmd state config perms = case cmd of
    StoreAddCmd path content ->
        case fromSing st of
            Daemon -> do
                -- Full implementation would add the file to the store
                storePath <- addToStore path content
                return $ StoreAddResponse storePath
            Builder ->
                return $ ErrorResponse $ PrivilegeError "Store modification requires daemon privileges"

    StoreVerifyCmd path ->
        return $ StoreVerifyResponse True

    StorePathCmd path content ->
        return $ StorePathResponse $ makeStorePath "0000000000000000000000000000000000000000" path

    StoreListCmd ->
        return $ StoreListResponse []

    StoreReadCmd path ->
        -- In a real implementation, this would read content from the store
        return $ StoreReadResponse BS.empty

handleGCRequest :: SPrivilegeTier t -> Bool -> DaemonState 'Daemon
                -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
handleGCRequest st force state config perms =
    case fromSing st of
        Daemon -> do
            -- Full implementation would perform garbage collection
            let stats = GCStats {
                    gcTotal = 100,
                    gcLive = 90,
                    gcCollected = 10,
                    gcBytes = 1024 * 1024,
                    gcElapsedTime = 1.5
                }
            return $ GCResultResponse stats
        Builder ->
            return $ ErrorResponse $ PrivilegeError "Garbage collection requires daemon privileges"

handleStatusRequest :: SPrivilegeTier t -> DaemonState 'Daemon
                     -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
handleStatusRequest st state config perms = do
    -- Get current time
    now <- liftIO getCurrentTime

    -- Calculate uptime
    let uptime = diffUTCTime now (dsStartTime state)

    -- Create status response
    let status = DaemonStatus {
            daemonStatus = "running",
            daemonUptime = realToFrac uptime,
            daemonActiveBuilds = 0,
            daemonCompletedBuilds = 0,
            daemonFailedBuilds = 0,
            daemonGcRoots = 0,
            daemonStoreSize = 0,
            daemonStorePaths = 0
        }

    return $ StatusResponse status

handleConfigRequest :: SPrivilegeTier t -> DaemonState 'Daemon
                    -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
handleConfigRequest st state config perms =
    case fromSing st of
        Daemon -> return $ ConfigResponse config
        Builder -> return $ ErrorResponse $ PrivilegeError "Config access requires daemon privileges"

handleShutdownRequest :: SPrivilegeTier t -> DaemonState 'Daemon
                      -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
handleShutdownRequest st state config perms =
    case fromSing st of
        Daemon -> return ShutdownResponse
        Builder -> return $ ErrorResponse $ PrivilegeError "Shutdown requires daemon privileges"

handleDerivationRequest :: SPrivilegeTier t -> DerivationCommand -> DaemonState 'Daemon
                        -> DaemonConfig -> Set Permission -> TenM 'Build t DaemonResponse
handleDerivationRequest st cmd state config perms = case cmd of
    StoreDerivationCmd content ->
        case fromSing st of
            Daemon -> do
                -- Parse the derivation
                case Derivation.deserializeDerivation content of
                    Left err -> return $ ErrorResponse err
                    Right drv -> do
                        -- Store the derivation (real implementation)
                        let path = StorePath "0000000000000000000000000000000000000000" (derivName drv <> ".drv")
                        return $ DerivationStoredResponse path
            Builder ->
                return $ ErrorResponse $ PrivilegeError "Storing derivations requires daemon privileges"

    QueryDerivationCmd hash ->
        -- In a real implementation, this would query the derivation database
        return $ DerivationQueryResponse []

    GetDerivationForOutputCmd path ->
        -- In a real implementation, this would find the derivation that produced an output
        return $ DerivationRetrievedResponse Nothing

    ListDerivationsCmd ->
        -- In a real implementation, this would list derivations in the store
        return $ DerivationListResponse []

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
