{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
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

    -- Utilities for testing
    decodeRequestMessage,
    encodeResponseMessage,

    -- Server control
    ServerControl(..)
) where

import Control.Concurrent (ThreadId, forkIO, forkFinally, myThreadId, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, try, catch, handle, throwIO, SomeException, AsyncException(..), displayException)
import Control.Monad (forever, void, when, unless, forM_, foldM, forM)
import Control.Monad.IO.Class (liftIO)
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
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Word (Word32)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (sendAll, recv)
import System.Directory (doesFileExist, removeFile, createDirectoryIfMissing, getModificationTime)
import System.Environment (getEnvironment)
import System.FilePath (takeDirectory)
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush, hPutStrLn, stderr, hSetBuffering, BufferMode(..))
import System.IO.Error (isDoesNotExistError, isPermissionError, catchIOError)
import System.Posix.Files (setFileMode, getFileStatus, accessTime, modificationTime)
import System.Posix.Types (FileMode)
import System.Posix.User (UserID, GroupID, setUserID, setGroupID, getEffectiveUserID,
                          getUserEntryForName, groupID, userID, getUserEntryForID)
import System.Exit (ExitCode(..))
import System.Process (createProcess, proc, waitForProcess, StdStream(..))
import System.Random (randomRIO)
import Crypto.Hash (hash, Digest, SHA256)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteArray as BA

import Ten.Core
import Ten.Build (BuildResult(..), buildDerivation)
import Ten.Daemon.Protocol
import Ten.Daemon.State
import Ten.Daemon.Auth
import Ten.Daemon.Config (DaemonConfig(..))
import Ten.Derivation (serializeDerivation, deserializeDerivation, hashDerivation)
import Ten.Store (storePathToFilePath, makeStorePath, addToStore)
import Ten.Hash (hashByteString, showHash)

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

-- | Rate limiting configuration
maxRequestsPerMinute :: Int
maxRequestsPerMinute = 300  -- 5 requests per second

maxAuthAttemptsPerMinute :: Int
maxAuthAttemptsPerMinute = 10

-- | Security constants
maxRequestSize :: Int
maxRequestSize = 50 * 1024 * 1024  -- 50MB max request size

clientTimeoutSeconds :: Int
clientTimeoutSeconds = 300  -- 5 minutes

maxConcurrentClientsPerIP :: Int
maxConcurrentClientsPerIP = 10

-- | Start the server
startServer :: FilePath -> DaemonState -> DaemonConfig -> IO ServerControl
startServer socketPath state config = do
    -- Create socket directory if it doesn't exist
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Clean up any existing socket file
    removeSocketIfExists socketPath

    -- Create and bind the server socket
    serverSocket <- createServerSocket socketPath

    -- Set the socket permissions
    setFileMode socketPath 0o666

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
    hPutStrLn securityLog $ formatLogEntry now "SERVER" "Server starting with socket: " socketPath
    hPutStrLn accessLog $ formatLogEntry now "SERVER" "Access log initialized"

    -- Start the accept thread
    serverThread <- forkIO $ acceptClients serverSocket clients shutdownFlag state config
                                           authDbVar rateLimiter securityLog accessLog

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
        -- Attempt to send a shutdown notice to the client
        catch (sendShutdownNotice ciHandle) (\(_ :: SomeException) -> return ())
        -- Close the socket and handle
        catch (hClose ciHandle) (\(_ :: SomeException) -> return ())
        catch (close ciSocket) (\(_ :: SomeException) -> return ())

    -- Close server socket
    close scSocket

    -- Kill server thread if still running
    killThread scThread

    -- Close log files
    hClose scSecurityLog
    hClose scAccessLog

    -- Save authentication database
    authDb <- atomically $ readTVar scAuthDb
    authDbPath <- getAuthDbPath scConfig
    saveAuthDb authDbPath authDb

    -- Log final shutdown message to stderr
    hPutStrLn stderr "Ten daemon server shutdown complete"

-- | Send a shutdown notice to a client
sendShutdownNotice :: Handle -> IO ()
sendShutdownNotice handle = do
    let shutdownMsg = encodeMessage $ ResponseMsg 0 ShutdownResponse
    BS.hPut handle shutdownMsg
    hFlush handle

-- | Main loop to accept client connections
acceptClients :: Socket -> ActiveClients -> TVar Bool -> DaemonState -> DaemonConfig
              -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
              -> Handle -> Handle -> IO ()
acceptClients serverSocket clients shutdownFlag state config
             authDbVar rateLimiter securityLog accessLog = do
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
                        -- Log the connection attempt
                        now <- getCurrentTime
                        hPutStrLn accessLog $ formatLogEntry now "CONNECT" $
                                 "New connection from " ++ show clientAddr

                        -- Check rate limiting for this address
                        allowed <- checkConnectionRateLimit rateLimiter clientAddr

                        if not allowed
                            then do
                                -- Log and close the connection
                                hPutStrLn securityLog $ formatLogEntry now "RATELIMIT" $
                                         "Connection rate limit exceeded for " ++ show clientAddr
                                close clientSocket
                                acceptLoop
                            else do
                                -- Check for too many connections from same address
                                clientCount <- countClientsFromAddress clients clientAddr
                                if clientCount >= maxConcurrentClientsPerIP
                                    then do
                                        -- Log and close the connection
                                        hPutStrLn securityLog $ formatLogEntry now "TOOMANYCONN" $
                                                 "Too many connections from " ++ show clientAddr
                                        close clientSocket
                                        acceptLoop
                                    else do
                                        -- Convert to handle for convenience
                                        clientHandle <- socketToHandle clientSocket ReadWriteMode
                                        hSetBuffering clientHandle LineBuffering

                                        -- Initialize client state
                                        lastActivityVar <- newTVarIO now
                                        clientStateVar <- newTVarIO Authenticating
                                        requestCountVar <- newTVarIO 0
                                        permissionsVar <- newTVarIO Set.empty

                                        -- Start client handler thread
                                        clientThread <- forkFinally
                                            (handleClient clientSocket clientHandle clients state config
                                                         authDbVar rateLimiter securityLog accessLog
                                                         lastActivityVar clientStateVar requestCountVar
                                                         permissionsVar clientAddr)
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
                                        atomically $ modifyTVar clients $ Map.insert clientThread clientInfo

                                        -- Continue accepting
                                        acceptLoop

    -- Start the accept loop
    acceptLoop `catch` \(e :: SomeException) -> do
        -- Log error and continue if not intentional shutdown
        case fromException e of
            Just ThreadKilled -> return ()  -- Normal shutdown
            _ -> do
                now <- getCurrentTime
                hPutStrLn securityLog $ formatLogEntry now "ERROR" $
                         "Error in accept loop: " ++ displayException e

                -- Try to restart accept loop unless we're shutting down
                shouldShutdown <- atomically $ readTVar shutdownFlag
                unless shouldShutdown $
                    acceptClients serverSocket clients shutdownFlag state config
                                  authDbVar rateLimiter securityLog accessLog

-- | Wait for a client connection with timeout
waitForClientConnection :: Socket -> Int -> IO (Maybe (Socket, SockAddr))
waitForClientConnection serverSocket timeoutMicros = do
    -- Set up file descriptor for select
    mfd <- try $ fdSocket serverSocket

    case mfd of
        Left (_ :: SomeException) -> return Nothing
        Right fd -> do
            -- Set up timeout
            let timeoutSec = timeoutMicros `div` 1000000
            let timeoutUsec = timeoutMicros `mod` 1000000

            -- Create an fd_set with just our socket
            ready <- try $ threadDelay timeoutMicros

            case ready of
                Left (_ :: SomeException) -> return Nothing
                Right _ -> do
                    -- Try to accept a connection (non-blocking)
                    result <- try $ accept serverSocket
                    case result of
                        Left (_ :: SomeException) -> return Nothing
                        Right clientConn -> return $ Just clientConn

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

-- | Handle a client connection
handleClient :: Socket -> Handle -> ActiveClients -> DaemonState -> DaemonConfig
             -> TVar AuthDb -> TVar (Map SockAddr (Int, UTCTime))
             -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
             -> TVar Int -> TVar (Set Permission) -> SockAddr -> IO ()
handleClient clientSocket clientHandle clients state config
            authDbVar rateLimiter securityLog accessLog
            lastActivityVar clientStateVar requestCountVar permissionsVar clientAddr = do
    -- Set timeout for authentication
    authTimeout <- registerTimeout 30 $ do
        -- Authentication timeout
        now <- getCurrentTime
        hPutStrLn securityLog $ formatLogEntry now "TIMEOUT" $
                 "Authentication timeout for client " ++ show clientAddr

        -- Force disconnect
        atomically $ writeTVar clientStateVar ShuttingDown
        hClose clientHandle
        close clientSocket

    -- First, authenticate the client
    authResult <- authenticateClient clientHandle authDbVar rateLimiter clientAddr securityLog

    -- Clear the authentication timeout
    cancelTimeout authTimeout

    case authResult of
        Left errorMsg -> do
            -- Authentication failed, send error and close
            sendAuthFailure clientHandle errorMsg
            now <- getCurrentTime
            hPutStrLn securityLog $ formatLogEntry now "AUTH_FAIL" $
                     "Authentication failed for " ++ show clientAddr ++ ": " ++ T.unpack errorMsg
            return ()

        Right (userId, authToken, permissions) -> do
            -- Log successful authentication
            now <- getCurrentTime
            hPutStrLn accessLog $ formatLogEntry now "AUTH_SUCCESS" $
                     "Client " ++ show clientAddr ++ " authenticated as " ++
                     case userId of UserId uid -> T.unpack uid

            -- Store permissions
            atomically $ writeTVar permissionsVar permissions

            -- Update client info with authenticated user
            tid <- myThreadId
            updateClientAuth tid userId authToken

            -- Register idle timeout handler
            idleTimeout <- registerTimeout clientTimeoutSeconds $ do
                -- Idle timeout
                idleTime <- getCurrentTime
                hPutStrLn securityLog $ formatLogEntry idleTime "TIMEOUT" $
                         "Idle timeout for client " ++ show clientAddr

                -- Force disconnect
                atomically $ writeTVar clientStateVar ShuttingDown
                hClose clientHandle
                close clientSocket

            -- Set client state to active
            atomically $ writeTVar clientStateVar Active

            -- Handle client requests
            handleClientRequests clientSocket clientHandle state config clientAddr
                                 securityLog accessLog lastActivityVar clientStateVar
                                 requestCountVar permissionsVar idleTimeout

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
handleClientRequests :: Socket -> Handle -> DaemonState -> DaemonConfig -> SockAddr
                     -> Handle -> Handle -> TVar UTCTime -> TVar ClientState
                     -> TVar Int -> TVar (Set Permission) -> TimerHandle -> IO ()
handleClientRequests clientSocket clientHandle state config clientAddr
                    securityLog accessLog lastActivityVar clientStateVar
                    requestCountVar permissionsVar idleTimeout = do
    let processLoop = do
            -- Check if client is shutting down
            clientState <- atomically $ readTVar clientStateVar
            when (clientState == Active) $ do
                -- Try to read a message
                msgResult <- try $ readMessageWithTimeout clientHandle 30000000 -- 30 seconds timeout

                case msgResult of
                    Left (e :: SomeException) -> do
                        -- Read error, client disconnected or timeout
                        now <- getCurrentTime
                        hPutStrLn accessLog $ formatLogEntry now "ERROR" $
                                 "Read error from client " ++ show clientAddr ++ ": " ++ displayException e
                        return ()

                    Right msgBytes -> do
                        -- Update last activity time
                        now <- getCurrentTime
                        atomically $ writeTVar lastActivityVar now

                        -- Reset idle timeout
                        resetTimeout idleTimeout

                        -- Update request count for rate limiting
                        reqCount <- atomically $ do
                            count <- readTVar requestCountVar
                            writeTVar requestCountVar (count + 1)
                            return (count + 1)

                        -- Log the request
                        hPutStrLn accessLog $ formatLogEntry now "REQUEST" $
                                 "Client " ++ show clientAddr ++ " request #" ++ show reqCount

                        -- Process the message
                        case decodeRequestMessage msgBytes of
                            Nothing -> do
                                -- Invalid message format
                                hPutStrLn securityLog $ formatLogEntry now "MALFORMED" $
                                         "Malformed request from client " ++ show clientAddr

                                -- Send error response
                                sendResponse clientHandle 0 $
                                    ErrorResponse $ DaemonError "Malformed request"

                                -- Continue processing
                                processLoop

                            Just (RequestMsg reqId request) -> do
                                -- Get client permissions
                                permissions <- atomically $ readTVar permissionsVar

                                -- Validate permissions for this request
                                permissionValid <- validateRequestPermissions request permissions securityLog clientAddr

                                if not permissionValid
                                    then do
                                        -- Permission denied
                                        sendResponse clientHandle reqId $
                                            ErrorResponse $ AuthError "Permission denied"

                                        -- Continue processing
                                        processLoop
                                    else do
                                        -- Process the request
                                        response <- processRequest request state config permissions

                                        -- Send the response
                                        sendResponse clientHandle reqId response

                                        -- Continue processing
                                        processLoop

    -- Start processing loop
    processLoop `catch` \(e :: SomeException) -> do
        -- Log error unless it's a normal disconnect
        now <- getCurrentTime
        case fromException e of
            Just ThreadKilled -> return ()  -- Normal shutdown
            _ -> hPutStrLn securityLog $ formatLogEntry now "ERROR" $
                     "Error handling client " ++ show clientAddr ++ ": " ++ displayException e

        -- Cancel the idle timeout
        cancelTimeout idleTimeout

-- | Type for timeout handle
data TimerHandle = TimerHandle (IORef (Maybe ThreadId))

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
resetTimeout (TimerHandle timerRef) = do
    -- Get the timer thread
    mThread <- readIORef timerRef

    -- Cancel the old thread if it exists
    case mThread of
        Just thread -> killThread thread
        Nothing -> return ()

    -- Create a new timer thread (in a real implementation this would
    -- reuse the existing timer instead of creating a new one)
    writeIORef timerRef Nothing

-- | Format a log entry
formatLogEntry :: UTCTime -> String -> String -> String
formatLogEntry time tag msg =
    formatTime time ++ " [" ++ tag ++ "] " ++ msg
  where
    formatTime :: UTCTime -> String
    formatTime = formatRFC3339

-- | Format time in RFC3339 format
formatRFC3339 :: UTCTime -> String
formatRFC3339 time =
    formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" time

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
                        Just (AuthRequestMsg authReq) -> do
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
                                    result <- authenticateUser' authDb (authUser authReq) (authToken authReq) clientAddr

                                    case result of
                                        Left err -> do
                                            hPutStrLn securityLog $ formatLogEntry now "AUTH_INVALID" $
                                                     "Invalid credentials from " ++ show clientAddr ++
                                                     ": " ++ T.unpack err
                                            return $ Left err

                                        Right (userId, token, permissions) -> do
                                            -- Send success response
                                            let response = AuthSuccess userId token
                                            BS.hPut handle $ encodeMessage $ AuthResponseMsg response
                                            hFlush handle

                                            -- Update auth db
                                            atomically $ writeTVar authDbVar authDb

                                            return $ Right (userId, token, permissions)

                        _ -> do
                            now <- getCurrentTime
                            hPutStrLn securityLog $ formatLogEntry now "AUTH_MALFORMED" $
                                     "Malformed auth message from " ++ show clientAddr
                            return $ Left "Invalid authentication message"

-- | Check if protocol version is compatible
isCompatibleVersion :: ProtocolVersion -> Bool
isCompatibleVersion version =
    version `elem` compatibleVersions || version == currentProtocolVersion

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

-- | Authenticate a user (stub implementation)
authenticateUser' :: AuthDb -> Text -> Text -> SockAddr
                 -> IO (Either Text (UserId, AuthToken, Set Permission))
authenticateUser' authDb username token clientAddr = do
    -- In a real implementation, this would validate credentials against the AuthDb
    -- and retrieve the user's permissions

    -- For now, a simplified version that accepts the token if it's valid
    let credentials = UserCredentials username token
    result <- validateCredentials' authDb credentials clientAddr

    case result of
        Left err -> return $ Left err
        Right (userId, authToken, permissions) -> return $ Right (userId, authToken, permissions)

-- | Validate user credentials
validateCredentials' :: AuthDb -> UserCredentials -> SockAddr
                    -> IO (Either Text (UserId, AuthToken, Set Permission))
validateCredentials' authDb creds clientAddr = do
    -- Validate the token
    result <- validateToken authDb (AuthToken (token creds))

    case result of
        Nothing -> do
            -- Invalid token, try to authenticate with username/password
            -- For this example, we just generate a new token
            now <- getCurrentTime
            newToken <- generateToken

            -- In a real implementation, this would authenticate against a database
            -- For now, just check if the username is valid
            let userId = UserId (username creds)

            -- Grant basic permissions for this example
            let permissions = Set.fromList [PermQueryBuild, PermQueryStore]

            return $ Right (userId, AuthToken newToken, permissions)

        Just (userId, permissions) ->
            -- Token is valid
            return $ Right (userId, AuthToken (token creds), permissions)

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
processRequest :: Request -> DaemonState -> DaemonConfig -> Set Permission -> IO Response
processRequest request state config permissions =
    -- Dispatch to the appropriate handler based on request type
    dispatchRequest request state config permissions `catch` \(e :: SomeException) -> do
        -- Convert any errors to ErrorResponse
        case fromException e of
            Just ThreadKilled -> throwIO ThreadKilled  -- Re-throw ThreadKilled
            _ -> return $ ErrorResponse $ DaemonError $ "Request processing error: " <> T.pack (displayException e)

-- | Validate permissions for a request
validateRequestPermissions :: Request -> Set Permission -> Handle -> SockAddr -> IO Bool
validateRequestPermissions request permissions securityLog clientAddr = do
    -- Determine which permission is required for this request
    let requiredPermission = getRequiredPermission request
    let hasPermission = requiredPermission `Set.member` permissions

    -- Log permission issues
    unless hasPermission $ do
        now <- getCurrentTime
        hPutStrLn securityLog $ formatLogEntry now "PERMISSION_DENIED" $
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

    -- GC-related requests
    GCRequest{} -> PermRunGC

    -- Admin requests
    StatusRequest -> PermQueryBuild
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
permissionToText PermAdmin = "admin"

-- | Dispatch a request to the appropriate handler
dispatchRequest :: Request -> DaemonState -> DaemonConfig -> Set Permission -> IO Response
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
        handleStoreRequest (StoreAddCmd storeAddPath storeAddContent) state permissions

    StoreVerifyRequest{..} ->
        handleStoreRequest (StoreVerifyCmd storeVerifyPath) state permissions

    StorePathRequest{..} ->
        handleStoreRequest (StorePathCmd storePathForFile storePathContent) state permissions

    StoreListRequest ->
        handleStoreRequest StoreListCmd state permissions

    GCRequest{..} ->
        handleGCRequest gcForce state permissions

    StatusRequest ->
        handleStatusRequest state config permissions

    ConfigRequest ->
        handleConfigRequest state config permissions

    ShutdownRequest ->
        handleShutdownRequest state config permissions

    PingRequest ->
        return Pong

-- | Handle a build request
handleBuildRequest :: Text -> Maybe BS.ByteString -> BuildOptions -> DaemonState
                   -> Set Permission -> IO Response
handleBuildRequest filePath maybeContent options state permissions = do
    -- Verify build permission
    unless (PermBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: build"

    -- Get the file content
    content <- case maybeContent of
        Just c -> return c
        Nothing -> do
            -- Try to read from filesystem if not provided
            let path = T.unpack filePath
            fileExists <- doesFileExist path
            if fileExists
                then BS.readFile path
                else return $ BS8.pack "File not found"

    -- Create or get the build environment
    env <- getBuildEnv state options

    -- Determine file type and build
    let ext = takeExtension $ T.unpack filePath
    result <- if BS.length content > 0
        then case ext of
            ".ten" -> buildTenFile env content options
            ".drv" -> buildDrvFile env content options
            _ -> do
                -- Try to auto-detect
                if isJsonContent content
                    then buildTenFile env content options
                    else buildDrvFile env content options
        else return $ Left $ InputNotFound $ T.unpack filePath

    -- Convert result to response
    case result of
        Left err -> return $ ErrorResponse err
        Right (buildId, _) -> return $ BuildStartedResponse buildId

-- | Handle an evaluation request
handleEvalRequest :: Text -> Maybe BS.ByteString -> EvalOptions -> DaemonState
                  -> Set Permission -> IO Response
handleEvalRequest filePath maybeContent options state permissions = do
    -- Verify build permission (eval requires same permissions as build)
    unless (PermBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: eval"

    -- Get the file content
    content <- case maybeContent of
        Just c -> return c
        Nothing -> do
            -- Try to read from filesystem if not provided
            let path = T.unpack filePath
            fileExists <- doesFileExist path
            if fileExists
                then BS.readFile path
                else return $ BS8.pack "File not found"

    -- Create evaluation environment
    env <- getEvalEnv state

    -- Evaluate the content
    result <- if BS.length content > 0
        then evaluateContent env content options
        else return $ Left $ InputNotFound $ T.unpack filePath

    -- Convert result to response
    case result of
        Left err -> return $ ErrorResponse err
        Right derivation -> return $ EvalResponse derivation

-- | Handle a build derivation request
handleBuildDerivationRequest :: Derivation -> BuildOptions -> DaemonState
                             -> Set Permission -> IO Response
handleBuildDerivationRequest drv options state permissions = do
    -- Verify build permission
    unless (PermBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: build"

    -- Create or get the build environment
    env <- getBuildEnv state options

    -- Register the build in the daemon state
    buildId <- registerBuild state drv (UserId "daemon") 50 Nothing  -- Default priority, no timeout

    -- Create a build function that updates state as it progresses
    let buildWithProgress = do
            -- Update build status to running
            atomically $ updateBuildStatus state buildId (BuildRunning 0.0)

            -- Build the derivation
            result <- buildDerivation' env drv

            -- Update final status based on result
            case result of
                Left err -> do
                    atomically $ updateBuildStatus state buildId BuildFailed'
                    -- Return error
                    return $ Left err
                Right buildResult -> do
                    atomically $ updateBuildStatus state buildId BuildCompleted
                    -- Store result
                    atomically $ storeBuildResult state buildId (Right buildResult)
                    -- Return result
                    return $ Right buildResult

    -- Start the build in a separate thread if async, or run immediately if sync
    if buildAsync options
        then do
            -- Start async build
            void $ forkIO $ void buildWithProgress

            -- Return immediate response with build ID
            return $ BuildStartedResponse buildId
        else do
            -- Run build synchronously
            result <- buildWithProgress

            -- Return result directly
            case result of
                Left err -> return $ ErrorResponse err
                Right buildResult -> return $ BuildResponse buildResult

-- | Simplified build function for Derivation
buildDerivation' :: BuildEnv -> Derivation -> IO (Either BuildError BuildResult)
buildDerivation' env drv = do
    -- In a real implementation, this would use Ten.Build.buildDerivation
    -- For this example, we'll simulate a build with placeholder results

    -- Simulate build time
    threadDelay 500000  -- 0.5 second

    -- Create a result with the expected outputs
    let outputs = Set.map outputPath (derivOutputs drv)
    let result = BuildResult {
            resultOutputs = outputs,
            resultExitCode = ExitSuccess,
            resultLog = "Simulated build of " <> derivName drv <> " completed successfully",
            resultMetadata = Map.empty
        }

    return $ Right result

-- | Handle a build status request
handleBuildStatusRequest :: BuildId -> DaemonState -> Set Permission -> IO Response
handleBuildStatusRequest buildId state permissions = do
    -- Verify query permission
    unless (PermQueryBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: query-build"

    -- Get build status from state
    result <- try $ getBuildStatus state buildId

    case result of
        Left (e :: SomeException) ->
            return $ ErrorResponse $ DaemonError $ "Failed to get build status: " <> T.pack (displayException e)

        Right status -> do
            -- Create status update
            now <- getCurrentTime
            let update = BuildStatusUpdate {
                    buildId = buildId,
                    buildStatus = status,
                    buildTimeElapsed = 0.0,  -- Would be computed from state
                    buildTimeRemaining = Nothing,
                    buildLogUpdate = Nothing,
                    buildResourceUsage = Map.empty
                }

            -- Return status response
            return $ BuildStatusResponse update

-- | Handle a cancel build request
handleCancelBuildRequest :: BuildId -> DaemonState -> Set Permission -> IO Response
handleCancelBuildRequest buildId state permissions = do
    -- Verify cancel permission
    unless (PermCancelBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: cancel-build"

    -- Try to cancel the build
    result <- try $ cancelBuild state buildId

    case result of
        Left (e :: SomeException) ->
            return $ ErrorResponse $ DaemonError $ "Failed to cancel build: " <> T.pack (displayException e)

        Right success ->
            return $ CancelBuildResponse success

-- | Store command type
data StoreCommand
    = StoreAddCmd Text BS.ByteString
    | StoreVerifyCmd Text
    | StorePathCmd Text BS.ByteString
    | StoreListCmd

-- | Handle a store request
handleStoreRequest :: StoreCommand -> DaemonState -> Set Permission -> IO Response
handleStoreRequest cmd state permissions = case cmd of
    StoreAddCmd path content -> do
        -- Verify store modification permission
        unless (PermModifyStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: modify-store"

        -- Add file to store
        env <- getBuildEnv state defaultBuildOptions

        result <- try $ do
            drv <- evalTen (addToStore (T.pack $ takeFileName $ T.unpack path) content) env
            case drv of
                Left err -> throwIO err
                Right (storePath, _) -> return storePath

        case result of
            Left (e :: SomeException) ->
                return $ ErrorResponse $ StoreError $ "Failed to add to store: " <> T.pack (displayException e)

            Right storePath ->
                return $ PathAddedResponse storePath

    StoreVerifyCmd path -> do
        -- Verify store query permission
        unless (PermQueryStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-store"

        -- Verify store path
        env <- getBuildEnv state defaultBuildOptions

        -- Parse the store path
        let storePath = parseStorePath $ T.unpack path
        case storePath of
            Nothing ->
                return $ ErrorResponse $ StoreError "Invalid store path format"

            Just sp -> do
                result <- try $ runTen (verifyStorePath' sp) env (initBuildState Build)

                case result of
                    Left (e :: SomeException) ->
                        return $ ErrorResponse $ StoreError $ "Failed to verify path: " <> T.pack (displayException e)

                    Right (isValid, _) ->
                        return $ PathVerifiedResponse sp isValid

    StorePathCmd file content -> do
        -- Verify store query permission
        unless (PermQueryStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-store"

        -- Get store path for file
        let name = T.pack $ takeFileName $ T.unpack file
        let hash = showHash $ hashByteString content
        let path = makeStorePath hash name

        return $ StorePathResponse path

    StoreListCmd -> do
        -- Verify store query permission
        unless (PermQueryStore `Set.member` permissions) $
            return $ ErrorResponse $ AuthError "Permission denied: query-store"

        -- List store contents
        env <- getBuildEnv state defaultBuildOptions

        result <- try $ listStorePaths' (storePath env)

        case result of
            Left (e :: SomeException) ->
                return $ ErrorResponse $ StoreError $ "Failed to list store: " <> T.pack (displayException e)

            Right paths ->
                return $ StoreContentsResponse paths

-- | Stub for verify store path
verifyStorePath' :: StorePath -> TenM 'Build Bool
verifyStorePath' _ = return True

-- | Stub for listing store paths
listStorePaths' :: FilePath -> IO [StorePath]
listStorePaths' _ = return []

-- | Handle a garbage collection request
handleGCRequest :: Bool -> DaemonState -> Set Permission -> IO Response
handleGCRequest force state permissions = do
    -- Verify GC permission
    unless (PermRunGC `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: run-gc"

    -- Check if we can run GC
    canGC <- atomically $ checkGCLock state

    if not canGC && not force
        then return $ ErrorResponse $ ResourceError "Garbage collection already in progress"
        else do
            -- Acquire GC lock
            atomically $ acquireGCLock state

            -- Run GC in a separate thread
            void $ forkIO $ do
                env <- getBuildEnv state defaultBuildOptions
                result <- try $ runTen collectGarbage' env (initBuildState Build)

                -- Release GC lock when done
                atomically $ releaseGCLock state

                -- Update last GC time in state
                now <- getCurrentTime
                atomically $ updateLastGC state now

                -- Process result for logging
                case result of
                    Left (e :: SomeException) ->
                        putStrLn $ "GC error: " ++ displayException e

                    Right _ ->
                        putStrLn "GC completed successfully"

            -- Return immediate response
            return $ GCResponse GCStats {
                gcTotal = 0,
                gcLive = 0,
                gcCollected = 0,
                gcBytes = 0,
                gcElapsedTime = 0
            }

-- | Stub for garbage collection
collectGarbage' :: TenM 'Build GCStats
collectGarbage' = do
    -- In a real implementation, this would use Ten.GC.collectGarbage
    return GCStats {
        gcTotal = 0,
        gcLive = 0,
        gcCollected = 0,
        gcBytes = 0,
        gcElapsedTime = 0
    }

-- | Handle a status request
handleStatusRequest :: DaemonState -> DaemonConfig -> Set Permission -> IO Response
handleStatusRequest state config permissions = do
    -- Verify query permission
    unless (PermQueryBuild `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: query-build"

    -- Get daemon stats
    now <- getCurrentTime
    startTime <- getStateStartTime state
    let uptime = diffUTCTime now startTime

    buildCount <- atomically $ countActiveBuilds' state

    -- Create status response
    let status = DaemonStatus {
            daemonStatus = "running",
            daemonUptime = realToFrac uptime,
            daemonActiveBuilds = buildCount,
            daemonCompletedBuilds = 0,  -- Would be tracked in state
            daemonFailedBuilds = 0,     -- Would be tracked in state
            daemonGcRoots = 0,          -- Would be computed from state
            daemonStoreSize = 0,        -- Would be computed from storage
            daemonStorePaths = 0        -- Would be computed from storage
        }

    return $ StatusResponse status

-- | Stub for getting state start time
getStateStartTime :: DaemonState -> IO UTCTime
getStateStartTime _ = getCurrentTime

-- | Stub for counting active builds
countActiveBuilds' :: DaemonState -> STM Int
countActiveBuilds' _ = return 0

-- | Handle a configuration request
handleConfigRequest :: DaemonState -> DaemonConfig -> Set Permission -> IO Response
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
handleShutdownRequest :: DaemonState -> DaemonConfig -> Set Permission -> IO Response
handleShutdownRequest state config permissions = do
    -- Verify shutdown permission
    unless (PermShutdown `Set.member` permissions) $
        return $ ErrorResponse $ AuthError "Permission denied: shutdown"

    -- Signal shutdown (in a real implementation, this would trigger a proper shutdown)
    _ <- forkIO $ do
        -- Give time for the response to be sent
        threadDelay 1000000  -- 1 second
        atomically $ signalShutdown state

    -- Return success
    return ShutdownResponse

-- | Signal shutdown in state
signalShutdown :: DaemonState -> STM ()
signalShutdown _ = return ()

-- | Update last GC time in state
updateLastGC :: DaemonState -> UTCTime -> STM ()
updateLastGC _ _ = return ()

-- | Get the build environment
getBuildEnv :: DaemonState -> BuildOptions -> IO BuildEnv
getBuildEnv _ _ = do
    -- Create a minimal build environment for this example
    return BuildEnv {
        workDir = "/tmp/ten/work",
        storePath = "/var/lib/ten/store",
        verbosity = 1,
        allowedPaths = Set.empty,
        runMode = DaemonMode,
        userName = Just "nobody",
        buildStrategy = MonadicStrategy,
        maxRecursionDepth = 100
    }

-- | Get the evaluation environment
getEvalEnv :: DaemonState -> IO BuildEnv
getEvalEnv state = getBuildEnv state defaultBuildOptions

-- | Build a Ten expression file
buildTenFile :: BuildEnv -> BS.ByteString -> BuildOptions -> IO (Either BuildError (BuildId, BuildResult))
buildTenFile env content options = do
    -- In a real implementation, this would:
    -- 1. Parse the Ten expression
    -- 2. Evaluate it to produce a derivation
    -- 3. Build the derivation

    -- For now, create a placeholder build ID and result
    buildId <- BuildId <$> newUnique

    let result = BuildResult {
            resultOutputs = Set.empty,
            resultExitCode = ExitSuccess,
            resultLog = "Simulated Ten file build",
            resultMetadata = Map.empty
        }

    return $ Right (buildId, result)

-- | Build a derivation file
buildDrvFile :: BuildEnv -> BS.ByteString -> BuildOptions -> IO (Either BuildError (BuildId, BuildResult))
buildDrvFile env content options = do
    -- Parse the derivation
    case deserializeDerivation content of
        Left err ->
            return $ Left $ SerializationError err

        Right derivation -> do
            -- Create a build ID
            buildId <- BuildId <$> newUnique

            -- In a real implementation, this would build the derivation
            -- For now, return a placeholder result
            let result = BuildResult {
                    resultOutputs = Set.map outputPath (derivOutputs derivation),
                    resultExitCode = ExitSuccess,
                    resultLog = "Simulated derivation build of " <> derivName derivation,
                    resultMetadata = Map.empty
                }

            return $ Right (buildId, result)

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
        outputs = Set.singleton (DerivationOutput "out" (StorePath "outputhash" "output"))
        derivEnv = Map.empty
        derivSystem = fromMaybe "x86_64-linux" (evalSystemType options)
        derivStrategy = MonadicStrategy
        derivMeta = Map.empty

    return $ Right $ Derivation {
        derivName = name,
        derivHash = hash,
        derivBuilder = builder,
        derivArgs = args,
        derivInputs = inputs,
        derivOutputs = outputs,
        derivEnv = derivEnv,
        derivSystem = derivSystem,
        derivStrategy = derivStrategy,
        derivMeta = derivMeta
    }

-- | Check if content is in JSON format
isJsonContent :: BS.ByteString -> Bool
isJsonContent bs =
    case BS.uncons bs of
        Just (c, _) -> c == 123 || c == 91  -- '{' or '['
        Nothing -> False

-- | Register a build
storeBuildResult :: DaemonState -> BuildId -> Either BuildError BuildResult -> STM ()
storeBuildResult _ _ _ = return ()

-- | Default build options
defaultBuildOptions :: BuildOptions
defaultBuildOptions = BuildOptions {
    buildVerbosity = 1,
    buildMaxJobs = Nothing,
    buildTimeout = Nothing,
    buildSystemType = Nothing,
    buildArgs = Map.empty,
    buildAsync = True,
    buildKeepTemp = False
}

-- | Create a server socket
createServerSocket :: FilePath -> IO Socket
createServerSocket socketPath = do
    -- Remove existing socket if it exists
    catch (removeFile socketPath) $ \(e :: IOError) ->
        unless (isDoesNotExistError e) $ throwIO e

    -- Create socket
    sock <- socket AF_UNIX Stream 0

    -- Set socket options
    setSocketOption sock ReuseAddr 1

    -- Bind to path
    bind sock (SockAddrUnix socketPath)

    -- Listen for connections
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
    headerResult <- timeout' timeoutMicros $ BS.hGet handle 4
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
                    when (len > maxRequestSize) $ -- enforce size limit
                        throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

                    -- Try to read message body with timeout
                    bodyResult <- timeout' timeoutMicros $ BS.hGet handle len
                    case bodyResult of
                        Nothing -> throwIO $ DaemonError "Timeout waiting for message body"
                        Just bodyBytes ->
                            if BS.length bodyBytes /= len
                                then throwIO $ DaemonError "Disconnected while reading message body"
                                else return bodyBytes

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
        tryPutMVar resultVar (Left $ DaemonError "Timeout")

    -- Wait for result or timeout
    result <- takeMVar resultVar

    -- Kill both threads if still running
    killThread actionThread `catch` \(_ :: SomeException) -> return ()
    killThread timeoutThread `catch` \(_ :: SomeException) -> return ()

    -- Return result
    case result of
        Left e -> case fromException e of
            Just err -> throwIO err
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

-- | Decode a request message
decodeRequestMessage :: BS.ByteString -> Maybe RequestMsg
decodeRequestMessage bs =
    case Aeson.eitherDecodeStrict bs of
        Left _ -> Nothing
        Right msg -> Just msg

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
decodeMessage bs = Aeson.decodeStrict bs

-- | Log a security event
logSecurityEvent :: ServerControl -> Text -> IO ()
logSecurityEvent control msg = do
    now <- getCurrentTime
    hPutStrLn (scSecurityLog control) $ formatLogEntry now "SECURITY" $ T.unpack msg

-- | Get path for authentication database
getAuthDbPath :: DaemonConfig -> IO FilePath
getAuthDbPath config = do
    -- Use the state file directory with a different name
    let statePath = daemonStateFile config
    let authPath = takeDirectory statePath </> "auth.db"
    return authPath

-- | Open security log
openSecurityLog :: DaemonConfig -> IO Handle
openSecurityLog config = do
    -- Use the log file if specified, otherwise stderr
    case daemonLogFile config of
        Just logPath -> do
            -- Create directory if needed
            createDirectoryIfMissing True (takeDirectory logPath)

            -- Open log file in append mode
            let securityLogPath = takeDirectory logPath </> "security.log"

            -- Create or open the file
            openFile securityLogPath AppendMode

        Nothing -> return stderr

-- | Open access log
openAccessLog :: DaemonConfig -> IO Handle
openAccessLog config = do
    -- Use the log file if specified, otherwise stderr
    case daemonLogFile config of
        Just logPath -> do
            -- Create directory if needed
            createDirectoryIfMissing True (takeDirectory logPath)

            -- Open log file in append mode
            let accessLogPath = takeDirectory logPath </> "access.log"

            -- Create or open the file
            openFile accessLogPath AppendMode

        Nothing -> return stderr

-- | Parse a store path from a string
parseStorePath :: FilePath -> Maybe StorePath
parseStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) -> Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

-- | Load authentication database
loadAuthDb :: FilePath -> IO AuthDb
loadAuthDb path = do
    -- Check if the file exists
    exists <- doesFileExist path

    if not exists
        then do
            -- Create a new empty database
            now <- getCurrentTime
            return $ AuthDb Map.empty Map.empty Map.empty now
        else do
            -- Read the existing database
            content <- BS.readFile path

            -- Parse the database
            case Aeson.eitherDecodeStrict content of
                Left err -> do
                    -- Log error and return empty database
                    hPutStrLn stderr $ "Error loading auth database: " ++ err
                    now <- getCurrentTime
                    return $ AuthDb Map.empty Map.empty Map.empty now

                Right db -> return db

-- | Save authentication database
saveAuthDb :: FilePath -> AuthDb -> IO ()
saveAuthDb path db = do
    -- Create directory if needed
    createDirectoryIfMissing True (takeDirectory path)

    -- Write to a temporary file first
    let tempPath = path ++ ".tmp"
    BS.writeFile tempPath (LBS.toStrict $ Aeson.encode db)

    -- Rename to the final path
    renameFile tempPath path

-- | Generate a unique identifier
newUnique :: IO Integer
newUnique = do
    -- Get the current time as microseconds
    now <- getCurrentTime
    let micros = floor $ 1000000 * realToFrac (diffUTCTime now (read "1970-01-01 00:00:00 UTC"))

    -- Add some randomness
    r <- randomRIO (0, 999) :: IO Int

    -- Combine time and random number
    return $ micros * 1000 + fromIntegral r

-- | Bit manipulation helpers
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b

-- FormatTime for logs
formatTime :: UTCTime -> String
formatTime = error "Function imported from Data.Time.Format"

defaultTimeLocale :: ()
defaultTimeLocale = error "Imported from Data.Time.Format"

-- A stub AuthDb type
data AuthDb = AuthDb {
    adUsers :: Map Text UserInfo,  -- Keyed by username
    adSystemUserMap :: Map Text Text,  -- System username to Ten username
    adTokenMap :: Map Text Text,  -- Token to username
    adLastModified :: UTCTime
} deriving (Show, Eq)

-- A stub UserInfo type for authentication
data UserInfo = UserInfo {
    uiUserId :: UserId,
    uiUsername :: Text,
    uiPasswordHash :: Maybe PasswordHash,
    uiPermissionLevel :: UserPermissionLevel,
    uiSpecificPermissions :: Set Permission,
    uiSystemUser :: Maybe Text,  -- Associated system user, if any
    uiTokens :: Map Text TokenInfo,
    uiLastLogin :: Maybe UTCTime,
    uiCreated :: UTCTime
} deriving (Show, Eq)

-- A stub for PasswordHash
data PasswordHash = PasswordHash {
    phAlgorithm :: Text,      -- Hash algorithm (e.g., "pbkdf2-sha512")
    phSalt :: BS.ByteString,  -- Salt value
    phHash :: BS.ByteString,  -- Actual hash value
    phIterations :: Int,      -- Number of iterations
    phCreated :: UTCTime      -- When this hash was created
} deriving (Show, Eq)

-- A stub for UserPermissionLevel
data UserPermissionLevel
    = UserLevelNone       -- No access
    | UserLevelBasic      -- Basic access (read-only)
    | UserLevelStandard   -- Standard user (can build)
    | UserLevelAdvanced   -- Advanced user (can manage own builds)
    | UserLevelAdmin      -- Administrator (full access)
    deriving (Show, Eq, Ord, Enum, Bounded)

-- A stub for Permission
data Permission
    = PermBuild              -- Can build derivations
    | PermCancelBuild        -- Can cancel builds
    | PermQueryBuild         -- Can query build status
    | PermQueryStore         -- Can query store contents
    | PermModifyStore        -- Can add to store
    | PermRunGC              -- Can run garbage collection
    | PermShutdown           -- Can shut down daemon
    | PermManageUsers        -- Can manage users
    | PermAdmin              -- Administrative access
    deriving (Show, Eq, Ord, Enum, Bounded)

-- A stub for TokenInfo
data TokenInfo = TokenInfo {
    tiToken :: Text,           -- The actual token value
    tiCreated :: UTCTime,      -- When the token was created
    tiExpires :: Maybe UTCTime, -- When the token expires (Nothing = no expiry)
    tiClientInfo :: Text,      -- Information about the client
    tiLastUsed :: UTCTime,     -- When the token was last used
    tiPermissions :: Set Permission -- Specific permissions for this token
} deriving (Show, Eq)
