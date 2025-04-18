{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Ten.Daemon.Core (
    -- Main daemon functionality
    startDaemon,
    stopDaemon,
    restartDaemon,
    isDaemonRunning,

    -- Daemon lifecycle
    daemonize,
    runDaemon,

    -- Daemon control
    cleanShutdown,
    signalDaemon,

    -- Configuration
    setupDaemonConfig,
    loadDaemonState,
    saveDaemonState,

    -- Process management
    createPidFile,
    removePidFile,
    readPidFile,

    -- Signal handling
    setupSignalHandlers,

    -- Log management
    setupLogging,
    closeLogs,
    logMessage,

    -- Privilege management
    dropPrivileges,

    -- Main daemon context
    DaemonContext(..),

    -- Internal utilities for testing
    ensureDirectories,
    checkStoreAccess,
    acquireGlobalLock
) where

import Control.Concurrent (forkIO, killThread, threadDelay, ThreadId, myThreadId)
import Control.Concurrent.STM
import Control.Exception (bracket, finally, try, catch, handle, throwIO, SomeException, Exception, ErrorCall(..))
import Control.Monad (forever, void, when, unless, forM_)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
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
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Network.Socket (Socket, SockAddr(..), socketToHandle)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, getHomeDirectory, removeFile)
import System.Environment (getEnvironment, getArgs, lookupEnv, getProgName)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), hClose, hFlush, hPutStrLn, stderr, stdout, stdin, openFile, hGetLine)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Daemon (daemonize)
import System.Posix.Files (fileExist, getFileStatus, isRegularFile, setFileMode)
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import System.Posix.Signals
import System.Posix.Types (ProcessID)
import System.Posix.User (getUserEntryForName, UserEntry(..), GroupEntry(..), getGroupEntryForName)
import System.Process (readProcess, createProcess, proc, waitForProcess)
import Text.Read (readMaybe)

import Ten.Core
import Ten.Build
import Ten.Daemon.Config
import Ten.Daemon.Protocol
import Ten.Daemon.Auth
import Ten.Daemon.State
import Ten.Daemon.Client
import Ten.Daemon.Server

-- | Main daemon context
data DaemonContext = DaemonContext {
    ctxConfig :: DaemonConfig,        -- ^ Daemon configuration
    ctxState :: DaemonState,          -- ^ Daemon state
    ctxServer :: ServerControl,       -- ^ Server control
    ctxLogHandle :: Maybe Handle,     -- ^ Log file handle
    ctxShutdownFlag :: TVar Bool,     -- ^ Shutdown flag
    ctxMainThread :: ThreadId,        -- ^ Main thread ID
    ctxPid :: ProcessID,              -- ^ Process ID
    ctxStartTime :: UTCTime,          -- ^ Daemon start time
    ctxBackgroundThreads :: IORef [ThreadId] -- ^ Background worker threads
}

-- | Start the daemon
startDaemon :: DaemonConfig -> IO ()
startDaemon config = do
    -- Check if daemon is already running
    running <- isDaemonRunning (daemonSocketPath config)
    when running $ do
        putStrLn "Daemon is already running."
        exitSuccess

    -- Ensure required directories exist
    ensureDirectories config

    -- Check store access
    checkStoreAccess config

    if daemonForeground config
        then do
            -- Run in foreground
            putStrLn "Starting daemon in foreground mode..."
            runDaemon config
        else do
            -- Prepare for daemonization
            putStrLn "Starting daemon in background mode..."

            -- Close standard file descriptors (will be redirected by daemonize)
            hClose stdin

            -- Daemonize the process
            daemonize $ do
                -- Run the daemon
                runDaemon config

-- | Stop the daemon
stopDaemon :: IO ()
stopDaemon = do
    -- Get default socket path
    socketPath <- getDefaultSocketPath

    -- Check if daemon is running
    running <- isDaemonRunning socketPath
    unless running $ do
        putStrLn "Daemon is not running."
        exitSuccess

    -- Read PID file
    pidFile <- getPidFilePath socketPath
    mPid <- readPidFile pidFile

    case mPid of
        Nothing -> do
            putStrLn "Could not determine daemon PID."
            exitFailure

        Just pid -> do
            -- Send SIGTERM to daemon
            putStrLn $ "Sending shutdown signal to daemon (PID: " ++ show pid ++ ")..."
            signalProcess sigTERM pid

            -- Wait for daemon to exit
            putStrLn "Waiting for daemon to shut down..."
            waitForDaemonExit pid socketPath 30

            putStrLn "Daemon stopped."

-- | Restart the daemon
restartDaemon :: IO ()
restartDaemon = do
    -- Stop the daemon if running
    stopDaemon `catch` \(_ :: SomeException) -> return ()

    -- Start the daemon with default config
    config <- getDefaultConfig
    startDaemon config

-- | Check if daemon is running
isDaemonRunning :: FilePath -> IO Bool
isDaemonRunning socketPath = do
    -- Check if socket file exists
    socketExists <- doesFileExist socketPath

    if not socketExists
        then return False
        else do
            -- Check if PID file exists and process is running
            pidFile <- getPidFilePath socketPath
            mPid <- readPidFile pidFile

            case mPid of
                Nothing -> return False
                Just pid -> isProcessRunning pid

-- | Main daemon execution
runDaemon :: DaemonConfig -> IO ()
runDaemon config = do
    -- Initialize daemon context
    context <- initDaemonContext config

    -- Set up signal handlers
    setupSignalHandlers context

    -- Create PID file
    createPidFile (daemonSocketPath config) (ctxPid context)

    -- Drop privileges if configured and running as root
    when (isJust (daemonUser config)) $
        dropPrivileges config

    -- Set up logging
    logHandle <- setupLogging config

    -- Log daemon startup
    logMessage logHandle LogNormal $ "Ten daemon starting (PID: " ++ show (ctxPid context) ++ ")"
    logMessage logHandle LogNormal $ "Using store: " ++ daemonStorePath config

    -- Initialize daemon state
    state <- do
        -- Try to load existing state
        result <- loadDaemonState config
        case result of
            Left err -> do
                logMessage logHandle LogNormal $ "Failed to load daemon state: " ++ err
                logMessage logHandle LogNormal "Initializing new daemon state..."
                initializeDaemonState
            Right loadedState -> do
                logMessage logHandle LogNormal "Daemon state loaded successfully."
                return loadedState

    -- Start the server
    server <- startServer (daemonSocketPath config) state

    -- Update context with state and server
    let context' = context {
            ctxState = state,
            ctxServer = server,
            ctxLogHandle = logHandle
        }

    -- Start background workers
    startBackgroundWorkers context'

    -- Run main daemon loop
    runDaemonLoop context'

    -- Performed on shutdown
    finally (return ()) $ do
        -- Log shutdown
        logMessage logHandle LogNormal "Ten daemon shutting down..."

        -- Stop server
        stopServer server

        -- Kill background workers
        killBackgroundWorkers context'

        -- Save state
        saveDaemonState config state

        -- Remove PID file
        removePidFile (daemonSocketPath config)

        -- Close logs
        closeLogs logHandle

        logMessage logHandle LogNormal "Ten daemon shutdown complete."

-- | Initialize daemon context
initDaemonContext :: DaemonConfig -> IO DaemonContext
initDaemonContext config = do
    -- Get process ID
    pid <- getProcessID

    -- Record start time
    startTime <- getCurrentTime

    -- Create shutdown flag
    shutdownFlag <- newTVarIO False

    -- Get main thread ID
    mainThread <- myThreadId

    -- Create background thread tracker
    backgroundThreads <- newIORef []

    return DaemonContext {
        ctxConfig = config,
        ctxState = error "State not initialized",  -- Will be set later
        ctxServer = error "Server not initialized",  -- Will be set later
        ctxLogHandle = Nothing,
        ctxShutdownFlag = shutdownFlag,
        ctxMainThread = mainThread,
        ctxPid = pid,
        ctxStartTime = startTime,
        ctxBackgroundThreads = backgroundThreads
    }

-- | Main daemon loop
runDaemonLoop :: DaemonContext -> IO ()
runDaemonLoop context = do
    -- Set up thread that checks shutdown flag
    let loop = do
            -- Check if we should shut down
            shouldShutdown <- atomically $ readTVar (ctxShutdownFlag context)
            if shouldShutdown
                then return ()  -- Exit loop to start shutdown
                else do
                    -- Sleep for a bit
                    threadDelay 1000000  -- 1 second
                    loop

    -- Run the loop
    loop

-- | Start background worker threads
startBackgroundWorkers :: DaemonContext -> IO ()
startBackgroundWorkers context = do
    -- Set up periodic state saving
    stateSaverThread <- forkIO $ stateSaverWorker context

    -- Set up garbage collection if configured
    gcThread <- case daemonGcInterval (ctxConfig context) of
        Nothing -> return Nothing
        Just interval -> do
            tid <- forkIO $ gcWorker context interval
            return $ Just tid

    -- Track threads
    let threads = catMaybes [Just stateSaverThread, gcThread]
    writeIORef (ctxBackgroundThreads context) threads

-- | Kill background worker threads
killBackgroundWorkers :: DaemonContext -> IO ()
killBackgroundWorkers context = do
    -- Get thread IDs
    threads <- readIORef (ctxBackgroundThreads context)

    -- Kill each thread
    forM_ threads $ \tid ->
        killThread tid `catch` \(_ :: SomeException) -> return ()

-- | Background thread to save state periodically
stateSaverWorker :: DaemonContext -> IO ()
stateSaverWorker context = do
    let config = ctxConfig context
        state = ctxState context
        logHandle = ctxLogHandle context

        -- Save every 5 minutes
        loop = do
            -- Check if we should shut down
            shouldShutdown <- atomically $ readTVar (ctxShutdownFlag context)
            unless shouldShutdown $ do
                -- Save state
                result <- try $ saveDaemonState config state
                case result of
                    Left (e :: SomeException) ->
                        logMessage logHandle LogNormal $
                            "Error saving daemon state: " ++ show e
                    Right _ ->
                        logMessage logHandle LogDebug "Daemon state saved."

                -- Sleep
                threadDelay (5 * 60 * 1000000)  -- 5 minutes
                loop

    -- Run the loop
    loop `catch` \(e :: SomeException) -> do
        -- Log error unless it's ThreadKilled
        case fromException e of
            Just ThreadKilled -> return ()
            _ -> logMessage logHandle LogNormal $
                    "State saver worker terminated: " ++ show e

-- | Background thread for garbage collection
gcWorker :: DaemonContext -> Int -> IO ()
gcWorker context interval = do
    let config = ctxConfig context
        state = ctxState context
        logHandle = ctxLogHandle context

        loop = do
            -- Check if we should shut down
            shouldShutdown <- atomically $ readTVar (ctxShutdownFlag context)
            unless shouldShutdown $ do
                -- Check if we can run GC
                canGC <- atomically $ checkGCLock state

                when canGC $ do
                    -- Log GC start
                    logMessage logHandle LogNormal "Starting automatic garbage collection..."

                    -- Acquire GC lock
                    atomically $ acquireGCLock state

                    -- Create build environment
                    env <- mkBuildEnv config

                    -- Run GC
                    result <- runTen collectGarbage env (initBuildState Build)

                    -- Process result
                    case result of
                        Left err ->
                            logMessage logHandle LogNormal $
                                "Garbage collection failed: " ++ show err
                        Right (stats, _) ->
                            logMessage logHandle LogNormal $
                                "Garbage collection completed: " ++
                                show (gcCollected stats) ++ " paths collected, " ++
                                show (gcBytes stats) ++ " bytes freed."

                    -- Release GC lock
                    atomically $ releaseGCLock state

                -- Sleep
                threadDelay (interval * 1000000)
                loop

    -- Run the loop
    loop `catch` \(e :: SomeException) -> do
        -- Log error unless it's ThreadKilled
        case fromException e of
            Just ThreadKilled -> return ()
            _ -> logMessage logHandle LogNormal $
                    "GC worker terminated: " ++ show e

-- | Set up signal handlers
setupSignalHandlers :: DaemonContext -> IO ()
setupSignalHandlers context = do
    -- Set up SIGTERM handler for clean shutdown
    installHandler sigTERM (Catch $ handleSigTerm context) Nothing

    -- Set up SIGHUP handler for config reload
    installHandler sigHUP (Catch $ handleSigHup context) Nothing

    -- Set up SIGUSR1 handler for manual GC
    installHandler sigUSR1 (Catch $ handleSigUsr1 context) Nothing

    return ()

-- | Handle SIGTERM (graceful shutdown)
handleSigTerm :: DaemonContext -> IO ()
handleSigTerm context = do
    -- Log shutdown signal
    logMessage (ctxLogHandle context) LogNormal "Received SIGTERM, initiating shutdown..."

    -- Signal shutdown
    atomically $ writeTVar (ctxShutdownFlag context) True

-- | Handle SIGHUP (reload configuration)
handleSigHup :: DaemonContext -> IO ()
handleSigHup context = do
    -- Log reload signal
    logMessage (ctxLogHandle context) LogNormal "Received SIGHUP, reloading configuration..."

    -- In a real implementation, we would reload configuration here
    -- For now, just log that we received the signal
    logMessage (ctxLogHandle context) LogNormal "Configuration reload not implemented."

-- | Handle SIGUSR1 (manual garbage collection)
handleSigUsr1 :: DaemonContext -> IO ()
handleSigUsr1 context = do
    -- Log GC signal
    logMessage (ctxLogHandle context) LogNormal "Received SIGUSR1, triggering manual garbage collection..."

    -- Start GC in a separate thread
    void $ forkIO $ do
        -- Check if we can run GC
        canGC <- atomically $ checkGCLock (ctxState context)

        if canGC
            then do
                -- Acquire GC lock
                atomically $ acquireGCLock (ctxState context)

                -- Create build environment
                env <- mkBuildEnv (ctxConfig context)

                -- Run GC
                result <- runTen collectGarbage env (initBuildState Build)

                -- Process result
                case result of
                    Left err ->
                        logMessage (ctxLogHandle context) LogNormal $
                            "Manual garbage collection failed: " ++ show err
                    Right (stats, _) ->
                        logMessage (ctxLogHandle context) LogNormal $
                            "Manual garbage collection completed: " ++
                            show (gcCollected stats) ++ " paths collected, " ++
                            show (gcBytes stats) ++ " bytes freed."

                -- Release GC lock
                atomically $ releaseGCLock (ctxState context)
            else
                logMessage (ctxLogHandle context) LogNormal
                    "Cannot run garbage collection: already in progress."

-- | Create a build environment for daemon operations
mkBuildEnv :: DaemonConfig -> IO BuildEnv
mkBuildEnv config = do
    -- Create a build environment with appropriate paths
    let env = initBuildEnv
            (daemonTmpDir config)
            (daemonStorePath config)

    -- Set daemon mode
    return $ env { runMode = DaemonMode }

-- | Set up logging
setupLogging :: DaemonConfig -> IO (Maybe Handle)
setupLogging config = case daemonLogFile config of
    Nothing ->
        -- Log to stdout/stderr
        return Nothing

    Just path -> do
        -- Ensure log directory exists
        createDirectoryIfMissing True (takeDirectory path)

        -- Open log file
        handle <- openFile path AppendMode

        -- Set buffer mode
        hSetBuffering handle LineBuffering

        return $ Just handle

-- | Close log handles
closeLogs :: Maybe Handle -> IO ()
closeLogs Nothing = return ()
closeLogs (Just handle) = hClose handle

-- | Log a message
logMessage :: Maybe Handle -> LogLevel -> String -> IO ()
logMessage mHandle level msg = do
    -- Get current time for timestamp
    now <- getCurrentTime
    let timestamp = formatTime now

        -- Format log message
        logMsg = timestamp ++ " [" ++ show level ++ "] " ++ msg

    -- Write to appropriate destination
    case mHandle of
        Nothing ->
            -- Log to stdout/stderr based on level
            if level == LogQuiet || level == LogNormal
                then putStrLn logMsg
                else hPutStrLn stderr logMsg

        Just handle -> do
            -- Log to file
            hPutStrLn handle logMsg
            hFlush handle

-- | Format time for log messages
formatTime :: UTCTime -> String
formatTime = formatRFC3339

-- | RFC3339 time format (simplified version)
formatRFC3339 :: UTCTime -> String
formatRFC3339 time =
    formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" time

-- | Drop privileges if running as root
dropPrivileges :: DaemonConfig -> IO ()
dropPrivileges config = do
    -- Check if we're root
    uid <- getRealUserID

    when (uid == 0) $ do
        -- Get target user and group from config
        case (daemonUser config, daemonGroup config) of
            (Just userName, Just groupName) -> do
                -- Get user entry
                userEntry <- getUserEntryForName (T.unpack userName)

                -- Get group entry
                groupEntry <- getGroupEntryForName (T.unpack groupName)

                -- Set group ID first
                setGroupID (groupID groupEntry)

                -- Then set user ID
                setUserID (userID userEntry)

                -- Verify the change
                newUid <- getEffectiveUserID
                when (newUid == 0) $
                    error "Failed to drop privileges"

            (Just userName, Nothing) -> do
                -- Get user entry
                userEntry <- getUserEntryForName (T.unpack userName)

                -- Set user ID (group is set automatically to primary group)
                setUserID (userID userEntry)

                -- Verify the change
                newUid <- getEffectiveUserID
                when (newUid == 0) $
                    error "Failed to drop privileges"

            _ ->
                return ()  -- No user/group specified, continue as root

-- | Create PID file
createPidFile :: FilePath -> ProcessID -> IO ()
createPidFile socketPath pid = do
    -- Get PID file path
    pidFile <- getPidFilePath socketPath

    -- Create directory if needed
    createDirectoryIfMissing True (takeDirectory pidFile)

    -- Write PID to file
    writeFile pidFile (show pid)

    -- Set permissions (644)
    setFileMode pidFile 0o644

-- | Remove PID file
removePidFile :: FilePath -> IO ()
removePidFile socketPath = do
    -- Get PID file path
    pidFile <- getPidFilePath socketPath

    -- Remove file if it exists
    removeFileIfExists pidFile

-- | Read PID from PID file
readPidFile :: FilePath -> IO (Maybe ProcessID)
readPidFile path = do
    -- Check if file exists
    exists <- doesFileExist path

    if not exists
        then return Nothing
        else do
            -- Read content
            content <- readFile path

            -- Parse as integer
            case readMaybe content of
                Nothing -> return Nothing
                Just pid -> return $ Just pid

-- | Get PID file path
getPidFilePath :: FilePath -> IO FilePath
getPidFilePath socketPath = do
    -- Get XDG runtime directory
    runtimeDir <- getXdgDirectory XdgRuntime "ten"

    -- Create directory if needed
    createDirectoryIfMissing True runtimeDir

    -- PID file path
    return $ runtimeDir </> "ten-daemon.pid"

-- | Check if a process is running
isProcessRunning :: ProcessID -> IO Bool
isProcessRunning pid = do
    -- Check if process exists by sending signal 0
    result <- try $ signalProcess 0 pid
    case result of
        Left _ -> return False  -- Process not running
        Right _ -> return True  -- Process exists

-- | Wait for daemon to exit
waitForDaemonExit :: ProcessID -> FilePath -> Int -> IO ()
waitForDaemonExit pid socketPath maxSeconds = do
    let loop remaining = do
            -- Check if process still exists
            running <- isProcessRunning pid

            -- Check if socket file still exists
            socketExists <- doesFileExist socketPath

            if not running && not socketExists
                then return ()  -- Daemon has exited
                else if remaining <= 0
                    then do
                        -- Timeout reached
                        putStrLn "Timed out waiting for daemon to exit."
                        exitFailure
                    else do
                        -- Wait and check again
                        threadDelay 1000000  -- 1 second
                        loop (remaining - 1)

    -- Start waiting
    loop maxSeconds

-- | Send signal to daemon process
signalDaemon :: Signal -> FilePath -> IO ()
signalDaemon signal socketPath = do
    -- Get PID file path
    pidFile <- getPidFilePath socketPath

    -- Read PID
    mPid <- readPidFile pidFile

    case mPid of
        Nothing -> do
            putStrLn "Could not determine daemon PID."
            exitFailure

        Just pid -> do
            -- Send signal
            signalProcess signal pid

            putStrLn $ "Signal " ++ show signal ++ " sent to daemon (PID: " ++ show pid ++ ")."

-- | Ensure all required directories exist
ensureDirectories :: DaemonConfig -> IO ()
ensureDirectories config = do
    -- Create directories with appropriate permissions
    let directories = [
            takeDirectory (daemonSocketPath config),
            daemonStorePath config,
            daemonTmpDir config,
            takeDirectory (daemonStateFile config),
            maybe "" takeDirectory (daemonLogFile config)
            ]

    -- Create each directory
    forM_ directories $ \dir ->
        unless (null dir) $ createDirectoryIfMissing True dir

-- | Check store access
checkStoreAccess :: DaemonConfig -> IO ()
checkStoreAccess config = do
    -- Check if store directory exists
    storeExists <- doesDirectoryExist (daemonStorePath config)

    unless storeExists $ do
        -- Create store directory
        createDirectoryIfMissing True (daemonStorePath config)

        -- Create subdirectories
        forM_ ["tmp", "gc-roots"] $ \subdir -> do
            let path = daemonStorePath config </> subdir
            createDirectoryIfMissing True path

-- | Load daemon state from file
loadDaemonState :: DaemonConfig -> IO (Either String DaemonState)
loadDaemonState config = do
    -- Check if state file exists
    let stateFile = daemonStateFile config
    exists <- doesFileExist stateFile

    if not exists
        then do
            -- No state file, start fresh
            state <- initializeDaemonState
            return $ Right state
        else do
            -- Read and parse state file
            result <- try $ BS.readFile stateFile
            case result of
                Left (e :: SomeException) ->
                    return $ Left $ "Error reading state file: " ++ show e

                Right content ->
                    case Aeson.eitherDecodeStrict content of
                        Left err ->
                            return $ Left $ "Error parsing state file: " ++ err

                        Right state ->
                            return $ Right state

-- | Save daemon state to file
saveDaemonState :: DaemonConfig -> DaemonState -> IO ()
saveDaemonState config state = do
    -- Ensure directory exists
    createDirectoryIfMissing True (takeDirectory (daemonStateFile config))

    -- Serialize state to JSON
    let stateJSON = Aeson.encode state

    -- Write to file (atomically)
    let tempFile = daemonStateFile config ++ ".tmp"
    LBS.writeFile tempFile stateJSON
    renameFile tempFile (daemonStateFile config)

-- | Acquire global daemon lock
acquireGlobalLock :: FilePath -> IO ()
acquireGlobalLock lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath

    when exists $ do
        -- Read PID from lock file
        mPid <- readPidFile lockPath

        case mPid of
            Nothing ->
                -- Invalid lock file, remove it
                removeFile lockPath

            Just pid -> do
                -- Check if process is still running
                running <- isProcessRunning pid

                when running $
                    error $ "Another daemon instance is already running (PID: " ++ show pid ++ ")"

    -- Create lock file with our PID
    pid <- getProcessID
    writeFile lockPath (show pid)

    -- Clean up on exit
    finally (return ()) $
        removeFileIfExists lockPath

-- | Set up daemon configuration
setupDaemonConfig :: [String] -> IO DaemonConfig
setupDaemonConfig args = do
    -- Parse command line arguments
    result <- parseConfigFromArgs args

    case result of
        Left err -> do
            putStrLn $ "Error in configuration: " ++ err
            exitFailure

        Right config ->
            return config

-- | Clean shutdown of daemon
cleanShutdown :: DaemonContext -> IO ()
cleanShutdown context = do
    -- Log shutdown
    logMessage (ctxLogHandle context) LogNormal "Initiating clean shutdown..."

    -- Signal shutdown
    atomically $ writeTVar (ctxShutdownFlag context) True

    -- Let main loop handle the rest

-- | Helper function to remove a file if it exists
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
    exists <- doesFileExist path
    when exists $ removeFile path

-- | Helper function to rename a file
renameFile :: FilePath -> FilePath -> IO ()
renameFile oldPath newPath = do
    -- In a real implementation, use atomic rename
    -- For now, just copy and remove
    BS.readFile oldPath >>= BS.writeFile newPath
    removeFile oldPath

-- Standard time formatting imports that were missing
import Data.Time.Format (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)

-- Missing POSIX functions
import System.Posix.User (setUserID, setGroupID, getRealUserID, getEffectiveUserID)
import System.Posix.Process (getProcessID, signalProcess)
import System.Posix.Signals (installHandler, Handler(..), Signal, sigTERM, sigHUP, sigUSR1)
import System.Posix.IO (setFdOption, FdOption(CloseOnExec))
import qualified System.Posix.Files as Posix

-- Missing exception utils
import Control.Exception (fromException)

-- FD set operations for select
setFdSet :: Socket -> IO [Socket]
setFdSet sock = return [sock]

select :: [Socket] -> [Socket] -> [Socket] -> Int -> IO (Bool, [Socket], [Socket])
select readSocks writeSocks exceptSocks timeout = do
    -- Simple implementation; in real code use Network.Socket.select
    threadDelay (timeout * 1000000)
    return (False, [], [])

-- | Set buffering mode
hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering h mode = System.IO.hSetBuffering h mode

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
