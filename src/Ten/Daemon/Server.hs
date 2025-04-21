{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

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

    -- Server control
    ServerControl(..)
) where

import Control.Concurrent (ThreadId, forkIO, forkFinally, myThreadId, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, try, catch, handle, throwIO, SomeException)
import Control.Monad (forever, void, when, unless, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (runStateT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (toLazyByteString, word32BE)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush, hPutStrLn, stderr, socketToHandle,
                 BufferMode(..), hSetBuffering)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)
import System.Posix.User (UserEntry(..), GroupEntry(..), setUserID, setGroupID, getEffectiveUserID)
import System.Random (randomRIO)

import Ten.Core
import Ten.Build (buildDerivation)
import Ten.DB.Core (Database, withDatabase, tenQuery, tenExecute_, withTenTransaction, TransactionMode(..))
import Ten.DB.Derivations (storeDerivation, retrieveDerivation, registerDerivationFile)
import Ten.Store (addToStore, verifyStorePath, listStorePaths)
import Ten.GC (collectGarbage)
import Ten.Hash (hashByteString)
import Ten.Daemon.Protocol
import Ten.Daemon.State
import Ten.Daemon.Auth
import Ten.Daemon.Config (DaemonConfig(..), defaultDBPath)

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
    scAccessLog :: Handle            -- ^ Access log handle
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
    hPutStrLn securityLog $ formatLogEntry now "SERVER" "Server starting"
    hPutStrLn accessLog $ formatLogEntry now "SERVER" "Access log initialized"

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
        scAccessLog = accessLog
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

    -- Log server shutdown
    now <- getCurrentTime
    hPutStrLn scSecurityLog $ formatLogEntry now "SERVER" "Server shutting down, closing connections"

    -- Close all client connections
    forM_ (Map.elems clientMap) $ \ClientInfo{..} -> do
        atomically $ writeTVar ciState ShuttingDown
        -- Send a shutdown notice to the client
        catch (sendShutdownNotice ciHandle) (\(_ :: SomeException) -> return ())
        -- Close the socket and handle
        catch (hClose ciHandle) (\(_ :: SomeException) -> return ())
        catch (close ciSocket) (\(_ :: SomeException) -> return ())

    -- Kill server thread if still running
    killThread scThread

    -- Close log files
    hClose scSecurityLog
    hClose scAccessLog

    -- Save authentication database
    authDb <- atomically $ readTVar scAuthDb
    authDbPath <- getAuthDbPath scConfig
    saveAuthDb authDbPath authDb

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
                                                             "Client handler error: " ++ show ex
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
                         "Error in accept loop: " ++ show e

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
                                 "Read error from client " ++ show clientAddr ++ ": " ++ show e
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
                     "Error handling client " ++ show clientAddr ++ ": " ++ show e

        -- Cancel the idle timeout
        liftIO $ cancelTimeout idleTimeout

-- | Process a client request in the privileged daemon context
processRequest :: Request -> DaemonState -> DaemonConfig -> Set Permission
               -> TenM 'Build 'Privileged Response
processRequest request state config permissions = do
    env <- ask

    -- Dispatch to the appropriate handler based on request type
    case request of
        BuildRequest{..} -> do
            -- Get the file content
            content <- case buildFileContent of
                Just c -> return c
                Nothing -> do
                    -- Try to read from filesystem if not provided
                    let path = T.unpack buildFilePath
                    fileExists <- liftIO $ doesFileExist path
                    if fileExists
                        then liftIO $ BS.readFile path
                        else return $ TE.encodeUtf8 "File not found"

            -- Parse and build the derivation
            deriv <- parseBuildFile buildFilePath content

            -- Register the build with the daemon state
            buildId <- liftIO $ registerBuild state deriv (UserId "daemon") 50 Nothing

            -- Run the build in a separate thread to avoid blocking
            void $ liftIO $ forkIO $ do
                -- Run the build with the daemon's privileges
                buildResult <- runTen @'Build @'Privileged (buildDerivation deriv) env (initBuildState Build buildId)

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

        StoreAddRequest{..} -> do
            -- Add to store with privileged context
            storePath <- addToStore (storeFilename storeAddPath) storeAddContent
            return $ StoreAddResponse storePath

        GCRequest{..} -> do
            -- Check if we can run GC
            canGC <- liftIO $ atomically $ checkGCLock state

            if not canGC && not gcForce
                then return $ ErrorResponse $ GCError "Garbage collection already in progress"
                else do
                    -- Acquire GC lock
                    liftIO $ atomically $ acquireGCLock state

                    -- Run GC in a separate thread
                    void $ liftIO $ forkIO $ do
                        stats <- runTen @'Build @'Privileged collectGarbage env (initBuildState Build (BuildIdFromInt 0))

                        -- Release GC lock
                        atomically $ releaseGCLock state

                        -- Update last GC time
                        now <- getCurrentTime
                        atomically $ updateLastGC state now

                    -- Return immediate response
                    return $ GCStartedResponse

        DerivationStoreRequest{..} -> do
            -- Use the database to store derivation with privileged context
            let dbPath = defaultDBPath (storeLocation env)

            -- Run within database transaction
            result <- withDatabase dbPath 5000 $ \db -> do
                -- Parse the derivation
                case deserializeDerivation derivationContent of
                    Left err -> return $ Left $ SerializationError err
                    Right drv -> do
                        -- Store in content-addressable store
                        storePath <- storeDerivation drv

                        -- Register in database
                        _ <- liftIO $ registerDerivationFile db drv storePath

                        return $ Right storePath

            case result of
                Left err -> return $ ErrorResponse err
                Right storePath -> return $ DerivationStoredResponse storePath

        DerivationQueryRequest{..} -> do
            -- Query database for derivation with privileged context
            let dbPath = defaultDBPath (storeLocation env)

            result <- withDatabase dbPath 5000 $ \db -> do
                retrieveDerivation db derivationQueryHash

            case result of
                Nothing -> return $ ErrorResponse $ StoreError $ "Derivation not found: " <> derivationQueryHash
                Just drv -> return $ DerivationResponse drv

        StatusRequest -> do
            -- Get daemon stats with privileged access
            now <- liftIO getCurrentTime
            startTime <- liftIO $ getStateStartTime state
            let uptime = diffUTCTime now startTime

            buildCount <- liftIO $ atomically $ countActiveBuilds state
            completedCount <- liftIO $ atomically $ countCompletedBuilds state
            failedCount <- liftIO $ atomically $ countFailedBuilds state

            -- Get store stats (privileged operation)
            storePaths <- listStorePaths (storeLocation env)
            storeSize <- getStoreSize (storeLocation env)
            rootCount <- countGCRoots (storeLocation env)

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

        -- Handle other requests similarly, with proper privilege context
        _ -> return $ ErrorResponse $ DaemonError "Unimplemented request type"

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

-- | Check rate limiting for new connections
checkConnectionRateLimit :: TVar (Map SockAddr (Int, UTCTime)) -> SockAddr -> IO Bool
checkConnectionRateLimit rateLimiter addr = atomically $ do
    now <- getCurrentTime
    rateMap <- readTVar rateLimiter

    case Map.lookup addr rateMap of
        Nothing -> do
            -- First connection from this address
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
            killThread thread
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
                             "Error reading auth message: " ++ show e
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
responseTypeToTag (GCStartedResponse) = TagGCResponse
responseTypeToTag (StatusResponse _) = TagStatusResponse
responseTypeToTag (ConfigResponse _) = TagConfigResponse
responseTypeToTag ShutdownResponse = TagShutdownResponse
responseTypeToTag (ErrorResponse _) = TagErrorResponse
responseTypeToTag _ = TagErrorResponse

-- | Check if protocol version is compatible
isCompatibleVersion :: ProtocolVersion -> Bool
isCompatibleVersion version =
    version `elem` compatibleVersions || version == currentProtocolVersion

-- | Remove socket file if it exists
removeSocketIfExists :: FilePath -> IO ()
removeSocketIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path

-- | Decode a request message
decodeRequestMessage :: BS.ByteString -> Maybe RequestMsg
decodeRequestMessage bs =
    case decodeMessage bs of
        Just (RequestMsgWrapper msg) -> Just msg
        _ -> Nothing

-- | Decode a message
decodeMessage :: BS.ByteString -> Maybe Message
decodeMessage bs = Aeson.decodeStrict bs

-- | Bit manipulation helpers
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b

-- | Helper function to get exception from SomeException
fromException :: Exception e => SomeException -> Maybe e
fromException = Control.Exception.fromException

-- | Helper functions for parsing and error handling
parseBuildFile :: Text -> BS.ByteString -> TenM 'Build 'Privileged Derivation
parseBuildFile path content = do
    -- Implementation depends on the file format
    -- This would parse the content into a Derivation based on file extension
    let ext = takeExtension $ T.unpack path
    case ext of
        ".drv" ->
            case deserializeDerivation content of
                Left err -> throwError $ SerializationError err
                Right drv -> return drv
        ".ten" ->
            -- This would parse a Ten expression and evaluate it to a derivation
            -- For now, returning a placeholder
            throwError $ ParseError "Ten file format not implemented"
        _ ->
            throwError $ ParseError $ "Unknown file format: " <> T.pack ext

-- | Additional functions for store operations in privileged context
storeFilename :: Text -> Text
storeFilename path = T.pack $ takeFileName $ T.unpack path

-- | Get store statistics
getStoreSize :: FilePath -> TenM 'Build 'Privileged Integer
getStoreSize storePath = do
    -- This would calculate the total size of all store objects
    -- Placeholder implementation
    return 0

countGCRoots :: FilePath -> TenM 'Build 'Privileged Int
countGCRoots storePath = do
    -- Count GC roots in the store
    -- Placeholder implementation
    return 0

-- | These functions would be implemented in Ten.Daemon.State
getStateStartTime :: DaemonState -> IO UTCTime
getStateStartTime _ = getCurrentTime

countActiveBuilds :: DaemonState -> STM Int
countActiveBuilds _ = return 0

countCompletedBuilds :: DaemonState -> STM Int
countCompletedBuilds _ = return 0

countFailedBuilds :: DaemonState -> STM Int
countFailedBuilds _ = return 0

updateLastGC :: DaemonState -> UTCTime -> STM ()
updateLastGC _ _ = return ()
