{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

module Ten.Daemon.Core (
    -- Main daemon functionality with privilege tracking
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
    withPrivilegeTransition,
    runPrivileged,
    runUnprivileged,

    -- Main daemon context
    DaemonContext(..),

    -- Internal utilities for testing
    ensureDirectories,
    checkStoreAccess,
    acquireGlobalLock,

    -- GC coordination
    isGCRunning,
    checkGCLockStatus,

    -- Process handling with privilege tracking
    spawnBuilder,
    monitorBuilder,
    abortBuilder
) where

import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay, myThreadId, MVar, newMVar, takeMVar, putMVar, withMVar)
import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async, cancel, wait, waitCatch)
import Control.Exception (bracket, finally, try, catch, handle, throwIO, onException, mask,
                          SomeException, Exception, ErrorCall(..), AsyncException(..))
import Control.Monad (forever, void, when, unless, forM_, foldM)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
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
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Network.Socket (Socket, SockAddr(..), socketToHandle, socket, Family(..), SocketType(..), setSocketOption,
                       SocketOption(..), bind, listen, close, accept, withFdSocket, setCloseOnExecIfNeeded, connect)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, getHomeDirectory,
                        removeFile, getPermissions, setPermissions, removePathForcibly)
import System.Environment (getEnvironment, getArgs, lookupEnv, getProgName)
import System.Exit (ExitCode(..), exitSuccess, exitFailure, exitWith)
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush, hPutStrLn, stderr, stdout, stdin,
                 openFile, hGetLine, BufferMode(..), hSetBuffering)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import System.Posix.Files (fileExist, getFileStatus, isRegularFile, setFileMode,
                          setOwnerAndGroup, fileMode, ownerReadMode, ownerWriteMode, ownerExecuteMode,
                          groupReadMode, groupWriteMode, groupExecuteMode)
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus, ProcessStatus(..),
                           exitImmediately)
import System.Posix.Types (ProcessID, FileMode, GroupID, UserID)
import System.Posix.Signals (installHandler, blockSignals, unblockSignals, Handler(..), SignalSet,
                           Signal, emptySignalSet, addSignal, SignalInfo, sigCHLD, sigTERM, sigHUP,
                           sigUSR1, sigINT, signalProcess)
import System.Posix.IO (createFile, openFd, closeFd, defaultFileFlags, setFdOption,
                       OpenMode(..), FdOption(..), stdInput, stdOutput, stdError, dupTo)
import System.Posix.Resource (ResourceLimit(..), Resource(..), getResourceLimit, setResourceLimit,
                             ResourceLimits(..), softLimit, hardLimit)
import System.Posix.User (getEffectiveUserID, getRealUserID,
                         getUserEntryForName, getGroupEntryForName, setUserID, setGroupID,
                         userID, groupID, setGroups)
import System.Process (readProcess, createProcess, proc, waitForProcess, CreateProcess(..), StdStream(..), ProcessHandle)

import Ten.Core
import qualified Ten.Store as Store
import qualified Ten.Sandbox as Sandbox (SandboxConfig(..))
import qualified Ten.Daemon.Protocol as Protocol

-- | Main daemon context with privilege tracking phantom type
data DaemonContext (t :: PrivilegeTier) = DaemonContext {
    ctxConfig :: DaemonConfig,                  -- ^ Daemon configuration
    ctxState :: DaemonState t,                  -- ^ Daemon state with privilege tracking
    ctxServer :: ServerControl t,               -- ^ Server control with privilege tracking
    ctxLogHandle :: Maybe Handle,               -- ^ Log file handle
    ctxShutdownFlag :: TVar Bool,               -- ^ Shutdown flag
    ctxMainThread :: ThreadId,                  -- ^ Main thread ID
    ctxPid :: ProcessID,                        -- ^ Process ID
    ctxStartTime :: UTCTime,                    -- ^ Daemon start time
    ctxBackgroundThreads :: IORef [ThreadId],   -- ^ Background worker threads
    ctxListenSocket :: Maybe Socket,            -- ^ Listening socket for daemon
    ctxPrivilegeDropped :: IORef Bool,          -- ^ Whether privileges have been dropped
    ctxBuilderProcesses :: TVar (Map BuildId (Async BuildResult)), -- ^ Running builder processes
    ctxBuildMutexes :: TVar (Map StorePath (MVar ())), -- ^ Locks for concurrent access to store paths
    ctxRootMutex :: MVar (),                    -- ^ Global mutex for critical operations
    ctxUnprivilegedUserID :: UserID,            -- ^ UID for unprivileged operations
    ctxUnprivilegedGroupID :: GroupID,          -- ^ GID for unprivileged operations
    ctxPrivilegeEvidence :: SPrivilegeTier t    -- ^ Runtime evidence of privilege tier
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

    -- Check if we need to run as a specific user
    when (isJust (daemonUser config) && isNothing (daemonGroup config)) $ do
        let userName = fromJust (daemonUser config)
        userEntry <- try $ getUserEntryForName (T.unpack userName)
        case userEntry of
            Left (e :: SomeException) -> do
                putStrLn $ "Error: User " ++ T.unpack userName ++ " does not exist."
                exitFailure
            Right _ -> return ()

    -- Set up unprivileged user/group IDs early
    uid <- getRealUserID
    (unprivUid, unprivGid) <- if uid == 0
                               then do
                                   -- When running as root, set up unprivileged IDs
                                   let defaultUser = fromMaybe "nobody" (daemonUser config >>= Just . T.unpack)
                                   let defaultGroup = fromMaybe "nogroup" (daemonGroup config >>= Just . T.unpack)
                                   uid' <- safeGetUserUID defaultUser
                                   gid <- safeGetGroupGID defaultGroup
                                   return (uid', gid)
                               else do
                                   -- When not running as root, use current user
                                   gid <- getDefaultGroupID
                                   return (uid, gid)

    if daemonForeground config
        then do
            -- Run in foreground
            putStrLn "Starting daemon in foreground mode..."
            runDaemon config unprivUid unprivGid
        else do
            -- Prepare for daemonization
            putStrLn "Starting daemon in background mode..."

            -- Set up signal handlers for parent process
            void $ installHandler sigCHLD Ignore Nothing

            -- Close standard file descriptors (will be redirected by daemonize)
            hClose stdin

            -- Ensure daemon log directory exists
            forM_ (daemonLogFile config) $ \logPath -> do
                createDirectoryIfMissing True (takeDirectory logPath)

            -- Daemonize the process
            daemonize $ do
                -- Ensure proper umask for daemon
                setFileCreationMask 0o022

                -- Set up signal handling
                setupInitialSignalHandlers

                -- Run the daemon
                runDaemon config unprivUid unprivGid

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
                Nothing ->
                    -- Try connecting to socket as a fallback
                    isDaemonRunningBySocket socketPath
                Just pid -> isProcessRunning pid

-- | Check if daemon is running by trying to connect to its socket
isDaemonRunningBySocket :: FilePath -> IO Bool
isDaemonRunningBySocket socketPath = do
    result <- try $ do
        sock <- socket AF_UNIX Stream 0
        connect <- try $ do
            Network.Socket.connect sock (SockAddrUnix socketPath)
            close sock
            return True
        case connect of
            Left (_ :: SomeException) -> do
                close sock
                return False
            Right success -> return success
    case result of
        Left (_ :: SomeException) -> return False
        Right isRunning -> return isRunning

-- | Main daemon execution with proper privilege tracking
runDaemon :: DaemonConfig -> UserID -> GroupID -> IO ()
runDaemon config unprivUid unprivGid = do
    -- Initialize daemon context with Daemon privilege tier
    context <- initDaemonContext config unprivUid unprivGid sDaemon

    -- Set resource limits
    setResourceLimits

    -- Set up signal handlers
    setupSignalHandlers context

    -- Create PID file
    createPidFile (daemonSocketPath config) (ctxPid context)

    -- Create listening socket with root privileges
    listenSocket <- createListeningSocket (daemonSocketPath config)

    -- Update context with socket
    let context' = context { ctxListenSocket = Just listenSocket }

    -- Drop privileges if configured and running as root
    when (isJust (daemonUser config)) $ do
        dropPrivileges context' config
        -- Update privilege drop flag
        writeIORef (ctxPrivilegeDropped context') True

    -- Set up logging
    logHandle <- setupLogging config

    -- Log daemon startup
    logMessage logHandle (daemonLogLevel config) LogNormal $
        "Ten daemon starting (PID: " ++ show (ctxPid context') ++ ")"
    logMessage logHandle (daemonLogLevel config) LogNormal $
        "Using store: " ++ daemonStorePath config

    -- Initialize daemon state - properly typed for Daemon privilege
    state <- do
        -- Try to load existing state
        result <- loadDaemonState config
        case result of
            Left err -> do
                logMessage logHandle (daemonLogLevel config) LogNormal $
                    "Failed to load daemon state: " ++ err
                logMessage logHandle (daemonLogLevel config) LogNormal
                    "Initializing new daemon state..."
                initDaemonState (daemonStateFile config) (daemonMaxJobs config) 100 sDaemon
            Right loadedState -> do
                logMessage logHandle (daemonLogLevel config) LogNormal
                    "Daemon state loaded successfully."
                return loadedState

    -- Start the server with proper privilege context
    server <- startServer sDaemon listenSocket state

    -- Update context with state, server and log
    let context'' = context' {
            ctxState = state,
            ctxServer = server,
            ctxLogHandle = logHandle
        }

    -- Verify store integrity - requires Daemon privilege
    verifyStoreIntegrity context'' (daemonStorePath config)

    -- Start background workers
    startBackgroundWorkers context''

    -- Run main daemon loop
    finally
        (runDaemonLoop context'')
        (shutdownDaemon context'' logHandle)

-- | Verify store integrity using Daemon privilege
verifyStoreIntegrity :: DaemonContext 'Daemon -> FilePath -> IO ()
verifyStoreIntegrity context storePath = do
    -- Use the privilege evidence to verify store
    let stEvidence = ctxPrivilegeEvidence context

    -- Run store verification with proper privilege checks
    result <- runTen sBuild stEvidence
        (withStore stEvidence $ \st -> Store.verifyStore st storePath)
        (initDaemonEnv (daemonTmpDir (ctxConfig context)) storePath (Just "daemon"))
        (initBuildState Build (BuildIdFromInt 0))

    -- Log any issues
    case result of
        Left err ->
            logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal $
                "Store verification issue: " <> show err
        Right _ ->
            logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal
                "Store verification successful"

-- | Perform daemon shutdown cleanup
shutdownDaemon :: DaemonContext t -> Maybe Handle -> IO ()
shutdownDaemon context logHandle = do
    -- Log shutdown
    logMessage logHandle (daemonLogLevel (ctxConfig context)) LogNormal
        "Ten daemon shutting down..."

    -- Stop server with privilege evidence
    stopServer (ctxPrivilegeEvidence context) (ctxServer context)

    -- Terminate any running builder processes
    builderProcesses <- readTVarIO (ctxBuilderProcesses context)
    forM_ (Map.toList builderProcesses) $ \(buildId, asyncProc) -> do
        logMessage logHandle (daemonLogLevel (ctxConfig context)) LogNormal $
            "Terminating builder process for build: " ++ show buildId
        abortBuilder context buildId

    -- Close listen socket if still open
    forM_ (ctxListenSocket context) $ \sock -> do
        close sock

    -- Kill background workers
    killBackgroundWorkers context

    -- Release GC lock if we're holding it
    releaseGCLockIfNeeded context

    -- Save state
    saveDaemonStateWithPrivilege context

    -- Remove PID file
    removePidFile (daemonSocketPath (ctxConfig context))

    -- Close logs
    closeLogs logHandle

    logMessage logHandle (daemonLogLevel (ctxConfig context)) LogNormal
        "Ten daemon shutdown complete."

-- | Save daemon state with privilege checking
saveDaemonStateWithPrivilege :: DaemonContext t -> IO ()
saveDaemonStateWithPrivilege context =
    -- Use withPrivilegeScope to handle saving with proper privileges
    withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
        saveStateToFile (ctxState context)

-- | Release GC lock if the daemon is holding it
releaseGCLockIfNeeded :: DaemonContext t -> IO ()
releaseGCLockIfNeeded context = do
    let config = ctxConfig context
    let lockPath = gcLockPath (initBuildEnv (daemonTmpDir config) (daemonStorePath config))

    -- Check if lock file exists
    exists <- doesFileExist lockPath
    when exists $ do
        -- Check if we own the lock (read PID from lock file)
        pidContent <- try $ readFile lockPath
        case pidContent of
            Left (_ :: SomeException) ->
                return () -- Couldn't read lock file, probably not ours

            Right content ->
                case reads content of
                    [(lockPid, "")] -> do
                        -- Check if that's our PID
                        ourPid <- getProcessID
                        when (lockPid == ourPid) $ do
                            -- We own the lock, release it with privilege evidence
                            logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal
                                "Releasing GC lock held by daemon"

                            -- Release the lock with proper privilege context
                            withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
                                releaseGCLock (ctxState context)

                            -- Remove the lock file
                            removeFile lockPath `catch` \(_ :: SomeException) -> return ()

                    _ -> return () -- Invalid format, probably not our lock

-- | Initialize daemon context with privilege tracking
initDaemonContext :: DaemonConfig -> UserID -> GroupID -> SPrivilegeTier t -> IO (DaemonContext t)
initDaemonContext config unprivUid unprivGid stEvidence = do
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

    -- Create privilege drop tracker
    privilegeDropped <- newIORef False

    -- Create builder process map
    builderProcesses <- newTVarIO Map.empty

    -- Create build mutex map
    buildMutexes <- newTVarIO Map.empty

    -- Create root mutex
    rootMutex <- newMVar ()

    return DaemonContext {
        ctxConfig = config,
        ctxState = error "State not initialized",  -- Will be set later
        ctxServer = error "Server not initialized",  -- Will be set later
        ctxLogHandle = Nothing,
        ctxShutdownFlag = shutdownFlag,
        ctxMainThread = mainThread,
        ctxPid = pid,
        ctxStartTime = startTime,
        ctxBackgroundThreads = backgroundThreads,
        ctxListenSocket = Nothing,
        ctxPrivilegeDropped = privilegeDropped,
        ctxBuilderProcesses = builderProcesses,
        ctxBuildMutexes = buildMutexes,
        ctxRootMutex = rootMutex,
        ctxUnprivilegedUserID = unprivUid,
        ctxUnprivilegedGroupID = unprivGid,
        ctxPrivilegeEvidence = stEvidence
    }

-- | Create listening socket for daemon
createListeningSocket :: FilePath -> IO Socket
createListeningSocket socketPath = do
    -- Clean up any existing socket file
    removeSocketIfExists socketPath

    -- Create socket directory if needed
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Create the socket
    sock <- socket AF_UNIX Stream 0

    -- Set socket options
    setSocketOption sock ReuseAddr 1
    setCloseOnExecIfNeeded sock

    -- Bind to path
    bind sock (SockAddrUnix socketPath)

    -- Set socket file permissions to allow connections from any user (0666)
    setFileMode socketPath 0o666

    -- Start listening
    listen sock 10

    return sock

-- | Main daemon loop
runDaemonLoop :: DaemonContext t -> IO ()
runDaemonLoop context = do
    -- Set up thread that checks shutdown flag
    let loop = do
            -- Check if we should shut down
            shouldShutdown <- readTVarIO (ctxShutdownFlag context)
            if shouldShutdown
                then return ()  -- Exit loop to start shutdown
                else do
                    -- Process any completed builder processes
                    handleCompletedBuilders context

                    -- Sleep for a bit
                    threadDelay 1000000  -- 1 second
                    loop

    -- Run the loop
    loop `catch` \e -> case e of
        ThreadKilled -> return ()  -- Normal threadkill, just exit
        _ -> do  -- Unexpected exception
            logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal $
                "Main daemon loop error: " ++ show e
            -- Try to save state and exit gracefully
            saveDaemonStateWithPrivilege context
            exitFailure

-- | Handle any completed builder processes
handleCompletedBuilders :: DaemonContext t -> IO ()
handleCompletedBuilders context = do
    -- Get all running builder processes
    builderProcesses <- atomically $ readTVarIO (ctxBuilderProcesses context)

    -- Check for completed processes
    completedBuilds <- forM (Map.toList builderProcesses) $ \(buildId, asyncProc) -> do
        -- Check if the process has completed (non-blocking)
        result <- poll asyncProc
        case result of
            -- Process still running
            Nothing -> return Nothing

            -- Process completed
            Just outcome -> do
                case outcome of
                    Left err -> do
                        -- Builder failed with exception
                        logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal $
                            "Builder process for " ++ show buildId ++ " failed: " ++ show err

                        -- Update build status to failed
                        withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
                            updateBuildStatus (ctxState context) buildId BuildFailed'

                    Right result -> do
                        -- Builder completed successfully
                        logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal $
                            "Builder process for " ++ show buildId ++ " completed"

                        -- Update build status to completed
                        withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
                            updateBuildStatus (ctxState context) buildId BuildCompleted

                -- Return this build ID as completed
                return $ Just buildId

    -- Remove completed builds from the tracking map
    let completedBuildIds = catMaybes completedBuilds
    when (not $ null completedBuildIds) $ do
        atomically $ modifyTVar' (ctxBuilderProcesses context) $ \processes ->
            foldl (flip Map.delete) processes completedBuildIds

-- | Check if an async process has completed without blocking
poll :: Async a -> IO (Maybe (Either SomeException a))
poll asyncProc = do
    -- Check if the async process has completed without waiting
    result <- try $ waitCatch asyncProc
    case result of
        Left _ -> return Nothing  -- Exception means process is still running
        Right outcome -> return $ Just outcome

-- | Spawn a builder process for a derivation with privilege separation
spawnBuilder :: DaemonContext t -> Derivation -> BuildId -> IO (Async BuildResult)
spawnBuilder context derivation buildId = do
    let config = ctxConfig context

    -- Create a build directory
    buildDir <- createBuildDirectory config buildId

    -- Create sandbox configuration
    let sandboxConfig = Sandbox.defaultSandboxConfig {
            Sandbox.sandboxUser = fromMaybe "nobody" (daemonUser config),
            Sandbox.sandboxGroup = fromMaybe "nogroup" (daemonGroup config),
            Sandbox.sandboxAllowNetwork = False,  -- Restrict network access
            Sandbox.sandboxPrivileged = False,    -- Run as unprivileged user
            Sandbox.sandboxUseMountNamespace = True,
            Sandbox.sandboxUseNetworkNamespace = True
        }

    -- Log build start
    logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
        "Starting builder for " ++ show buildId ++ " in " ++ buildDir

    -- Set up the sandbox with proper privilege context
    withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
        runTen sBuild stEvidence
            (setupSandbox buildDir sandboxConfig)
            (initDaemonEnv (daemonTmpDir config) (daemonStorePath config) (Just "daemon"))
            (initBuildState Build (BuildIdFromInt 0))

    -- Prepare the derivation for building
    -- Serialize derivation to file
    let derivationPath = buildDir </> "derivation.drv"
    BS.writeFile derivationPath (serializeDerivation derivation)

    -- Create builder command
    let builderPath = daemonStorePath config </> "libexec/ten-builder"
    let builderArgs = [derivationPath, "--build-id=" ++ show buildId]
    let builderEnv = [
            ("TEN_STORE", daemonStorePath config),
            ("TEN_BUILD_DIR", buildDir),
            ("TEN_OUT", buildDir </> "out"),
            ("TEN_TMP", buildDir </> "tmp")
            ]

    -- Spawn the builder process as async task with appropriate privilege drop
    asyncProcess <- async $ do
        -- Run the builder in sandbox with proper privileges dropped
        (exitCode, stdout, stderr) <- runBuilderProcess
            (ctxPrivilegeEvidence context)
            builderPath builderArgs builderEnv
            (ctxUnprivilegedUserID context)
            (ctxUnprivilegedGroupID context)

        -- Parse the build result (builder writes JSON result to stdout)
        case exitCode of
            ExitSuccess ->
                case Aeson.eitherDecode $ LBS.fromStrict $ BS.pack stdout of
                    Left err -> error $ "Failed to parse builder result: " ++ err
                    Right result -> return result
            ExitFailure code ->
                error $ "Builder process failed with exit code: " ++ show code ++ "\nStderr: " ++ stderr

    -- Register this async process
    atomically $ modifyTVar' (ctxBuilderProcesses context) $
        Map.insert buildId asyncProcess

    return asyncProcess

-- | Run a builder process with privilege dropping (using type-level privilege constraints)
runBuilderProcess :: SPrivilegeTier t
                  -> FilePath
                  -> [String]
                  -> [(String, String)]
                  -> UserID
                  -> GroupID
                  -> IO (ExitCode, String, String)
runBuilderProcess stEvidence program args env uid gid = do
    -- Create process with configured stdin/stdout/stderr
    let processConfig = (proc program args) {
            env = Just env,
            std_in = NoStream,
            std_out = CreatePipe,
            std_err = CreatePipe
        }

    -- Get the real user ID to check if we're root
    currentUid <- getRealUserID

    -- Run the process with appropriate privilege tier
    case fromSing stEvidence of
        -- When daemon privilege, can drop privileges
        Daemon | currentUid == 0 -> do
            -- Running as root, drop privileges to the specified UID/GID
            mask $ \restore -> do
                -- Create the process
                (_, mbStdout, mbStderr, processHandle) <- createProcess processConfig

                -- Drop privileges for the process (only with Daemon privilege evidence)
                let stdout = fromJust mbStdout  -- Safe because we specified CreatePipe
                let stderr = fromJust mbStderr  -- Safe because we specified CreatePipe

                -- Drop privileges for the child process
                dropProcessPrivileges processHandle uid gid

                -- Read output
                output <- restore $ do
                    stdoutContents <- hGetContents stdout
                    stderrContents <- hGetContents stderr
                    exitCode <- waitForProcess processHandle
                    return (exitCode, stdoutContents, stderrContents)

                -- Close handles and return result
                hClose stdout
                hClose stderr
                return output

        -- In other cases, just run directly without dropping privileges
        _ -> do
            (exitCode, stdout, stderr) <- readCreateProcessWithExitCode processConfig ""
            return (exitCode, stdout, stderr)

-- | Drop privileges for a specific process (requires Daemon privilege)
dropProcessPrivileges :: ProcessHandle -> UserID -> GroupID -> IO ()
dropProcessPrivileges processHandle uid gid = do
    -- Get process ID from handle
    mbPid <- getPid processHandle

    case mbPid of
        Just pid -> do
            -- Use system calls to change process credentials
            changeProcessCredentials pid uid gid
            -- Verify the change
            verifyProcessCredentials pid uid
        Nothing ->
            return () -- Process already exited or couldn't get PID

-- | Create a build directory
createBuildDirectory :: DaemonConfig -> BuildId -> IO FilePath
createBuildDirectory config buildId = do
    -- Create a unique directory for this build
    let baseDir = daemonTmpDir config </> "builds"
    createDirectoryIfMissing True baseDir

    -- Use build ID for directory name
    let buildDir = baseDir </> show buildId
    createDirectoryIfMissing True buildDir

    -- Create essential subdirectories
    createDirectoryIfMissing True (buildDir </> "tmp")
    createDirectoryIfMissing True (buildDir </> "out")

    -- Set proper permissions
    setFileMode buildDir 0o755
    setFileMode (buildDir </> "tmp") 0o777  -- Writable by builder
    setFileMode (buildDir </> "out") 0o755

    return buildDir

-- | Monitor a builder process, handling output and updating status
monitorBuilder :: DaemonContext t -> BuildId -> Async BuildResult -> IO ()
monitorBuilder context buildId asyncProcess = do
    -- Fork a thread to monitor the process
    void $ forkIO $ do
        -- Poll the process status periodically
        let loop = do
                -- Check if build is still running
                isRunning <- not . isJust <$> poll asyncProcess

                if isRunning
                    then do
                        -- Update build status with progress using privilege context
                        withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
                            updateBuildStatus (ctxState context) buildId (BuildRunning 0.5)  -- Estimate progress

                        -- Sleep before checking again
                        threadDelay 5000000  -- 5 seconds
                        loop
                    else
                        return ()

        -- Start monitoring
        loop

-- | Abort a running builder process
abortBuilder :: DaemonContext t -> BuildId -> IO ()
abortBuilder context buildId = do
    -- Get the async process
    builderProcesses <- readTVarIO (ctxBuilderProcesses context)
    case Map.lookup buildId builderProcesses of
        Nothing ->
            -- Build not running
            return ()

        Just asyncProc -> do
            -- Cancel the async process
            cancel asyncProc

            -- Remove from tracking map
            atomically $ modifyTVar' (ctxBuilderProcesses context) $
                Map.delete buildId

            -- Update build status to failed with proper privilege context
            withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
                updateBuildStatus (ctxState context) buildId BuildFailed'

            -- Clean up build directory
            let buildDir = daemonTmpDir (ctxConfig context) </> "builds" </> show buildId
            removePathForcibly buildDir `catch` \(_ :: SomeException) -> return ()

            logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal $
                "Build " ++ show buildId ++ " aborted"

-- | Set resource limits for daemon process
setResourceLimits :: IO ()
setResourceLimits = do
    -- Get current open files limit
    fileLimits <- getResourceLimit ResourceOpenFiles

    -- Try to increase limit if it's too low
    when (softLimit fileLimits < ResourceLimit 1024) $ do
        -- Set new limits (at least 1024 open files, or maintain hard limit)
        let newLimit = case hardLimit fileLimits of
                ResourceLimitInfinity -> ResourceLimit 4096
                ResourceLimit n -> ResourceLimit (max 1024 n)

        -- Try to set new limit, ignoring failures
        try $ setResourceLimit ResourceOpenFiles (ResourceLimits {
            softLimit = newLimit,
            hardLimit = hardLimit fileLimits
        }) :: IO (Either SomeException ())

        return ()

    -- Set core dump size limit (disabling core dumps)
    setResourceLimit ResourceCoreFileSize (ResourceLimits {
        softLimit = ResourceLimit 0,
        hardLimit = ResourceLimit 0
    })

-- | Start background worker threads
startBackgroundWorkers :: DaemonContext t -> IO ()
startBackgroundWorkers context = do
    -- Set up periodic state saving
    stateSaverThread <- forkIO $
        stateSaverWorker context `catch` \(e :: SomeException) ->
            logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal $
                "State saver thread died: " ++ show e

    -- Set up garbage collection if configured
    gcThread <- case daemonGcInterval (ctxConfig context) of
        Nothing -> return Nothing
        Just interval -> do
            tid <- forkIO $
                gcWorker context interval `catch` \(e :: SomeException) ->
                    logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal $
                        "GC thread died: " ++ show e
            return $ Just tid

    -- Track threads
    let threads = catMaybes [Just stateSaverThread, gcThread]
    writeIORef (ctxBackgroundThreads context) threads

-- | Kill background worker threads
killBackgroundWorkers :: DaemonContext t -> IO ()
killBackgroundWorkers context = do
    -- Get thread IDs
    threads <- readIORef (ctxBackgroundThreads context)

    -- Kill each thread
    forM_ threads $ \tid ->
        killThread tid `catch` \(_ :: SomeException) -> return ()

-- | Background thread to save state periodically
stateSaverWorker :: DaemonContext t -> IO ()
stateSaverWorker context = do
    let config = ctxConfig context
        state = ctxState context
        logHandle = ctxLogHandle context

        -- Save every 5 minutes
        loop = do
            -- Check if we should shut down
            shouldShutdown <- readTVarIO (ctxShutdownFlag context)
            unless shouldShutdown $ do
                -- Save state with proper privilege context
                result <- try $ withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
                    saveStateToFile state

                case result of
                    Left (e :: SomeException) ->
                        logMessage logHandle (daemonLogLevel config) LogNormal $
                            "Error saving daemon state: " ++ show e
                    Right _ ->
                        logMessage logHandle (daemonLogLevel config) LogDebug "Daemon state saved."

                -- Sleep
                threadDelay (5 * 60 * 1000000)  -- 5 minutes
                loop

    -- Run the loop
    loop `catch` \(e :: SomeException) -> do
        -- Log error unless it's ThreadKilled
        case fromException e of
            Just ThreadKilled -> return ()
            _ -> logMessage logHandle (daemonLogLevel config) LogNormal $
                    "State saver worker terminated: " ++ show e

-- | Background thread for garbage collection with privilege handling
gcWorker :: DaemonContext t -> Int -> IO ()
gcWorker context interval = do
    let config = ctxConfig context
        state = ctxState context
        logHandle = ctxLogHandle context

        loop = do
            -- Check if we should shut down
            shouldShutdown <- readTVarIO (ctxShutdownFlag context)
            unless shouldShutdown $ do
                -- Check if we can run GC with proper privilege evidence
                canGC <- withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
                    atomically $ checkGCLock state

                when canGC $ do
                    -- Check if another process is already running GC
                    let storeDir = daemonStorePath config
                        lockPath = gcLockPath (initBuildEnv (daemonTmpDir config) storeDir)

                    gcRunning <- isGCRunning lockPath

                    unless gcRunning $ do
                        -- Log GC start
                        logMessage logHandle (daemonLogLevel config) LogNormal
                            "Starting automatic garbage collection..."

                        -- Execute GC with proper privilege evidence
                        withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence -> do
                            -- Acquire GC lock in daemon state
                            atomically $ acquireGCLock state

                            -- Create build environment
                            let env = initDaemonEnv (daemonTmpDir config) storeDir (Just "daemon")

                            -- Run GC with appropriate privilege
                            result <- runTen sBuild stEvidence collectGarbage env (initBuildState Build (BuildIdFromInt 0))

                            -- Process result
                            case result of
                                Left err ->
                                    logMessage logHandle (daemonLogLevel config) LogNormal $
                                        "Garbage collection failed: " ++ show err
                                Right (stats, _) ->
                                    logMessage logHandle (daemonLogLevel config) LogNormal $
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
            _ -> logMessage logHandle (daemonLogLevel config) LogNormal $
                    "GC worker terminated: " ++ show e

-- | Check if a GC process is running (by checking the lock file)
isGCRunning :: FilePath -> IO Bool
isGCRunning lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath

    if not exists
        then return False
        else do
            -- Read PID from lock file
            content <- try $ readFile lockPath
            case content of
                Left (_ :: SomeException) ->
                    return False -- Can't read lock file

                Right pidStr ->
                    case reads pidStr of
                        [(pid, "")] -> do
                            -- Check if process is running
                            isProcessRunning pid
                        _ ->
                            -- Invalid lock file format
                            return False

-- | Check the status of the GC lock
checkGCLockStatus :: FilePath -> IO (Bool, Maybe ProcessID)
checkGCLockStatus lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath

    if not exists
        then return (False, Nothing) -- Not locked
        else do
            -- Read PID from lock file
            content <- try $ readFile lockPath
            case content of
                Left (_ :: SomeException) ->
                    return (False, Nothing) -- Can't read lock file

                Right pidStr ->
                    case reads pidStr of
                        [(pid, "")] -> do
                            -- Check if process is running
                            running <- isProcessRunning pid
                            if running
                                then return (True, Just pid) -- Locked by running process
                                else return (False, Just pid) -- Stale lock
                        _ ->
                            -- Invalid lock file format
                            return (False, Nothing)

-- | Set up initial signal handlers before full daemon initialization
setupInitialSignalHandlers :: IO ()
setupInitialSignalHandlers = do
    -- Block signals we'll handle explicitly
    let signalsToBlock = addSignal sigTERM $
                         addSignal sigHUP $
                         addSignal sigUSR1 $
                         addSignal sigINT
                         emptySignalSet
    blockSignals signalsToBlock

    -- Set up SIGTERM handler for immediate exit
    installHandler sigTERM (Catch $ \_ -> exitImmediately 0) Nothing

    -- Set up SIGINT handler
    installHandler sigINT (Catch $ \_ -> exitImmediately 0) Nothing

    -- Let other signals through
    unblockSignals signalsToBlock

-- | Set up signal handlers
setupSignalHandlers :: DaemonContext t -> IO ()
setupSignalHandlers context = do
    -- Set up SIGTERM handler for clean shutdown
    installHandler sigTERM (Catch $ handleSigTerm context) Nothing

    -- Set up SIGHUP handler for config reload
    installHandler sigHUP (Catch $ handleSigHup context) Nothing

    -- Set up SIGUSR1 handler for manual GC
    installHandler sigUSR1 (Catch $ handleSigUsr1 context) Nothing

    -- Set up SIGINT handler (Ctrl+C) - same as SIGTERM
    installHandler sigINT (Catch $ handleSigTerm context) Nothing

    -- Ignore SIGCHLD to prevent zombies
    installHandler sigCHLD Ignore Nothing

-- | Handle SIGTERM signal
handleSigTerm :: DaemonContext t -> IO ()
handleSigTerm context = do
    -- Log shutdown signal
    logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal
        "Received SIGTERM, initiating shutdown..."

    -- Signal shutdown
    atomically $ writeTVar (ctxShutdownFlag context) True

-- | Handle SIGHUP signal
handleSigHup :: DaemonContext t -> IO ()
handleSigHup context = do
    -- Log reload signal
    logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal
        "Received SIGHUP, reloading configuration..."

    -- Save state with proper privilege context
    withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
        saveStateToFile (ctxState context)

    logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal
        "Configuration reload completed (state saved)."

-- | Handle SIGUSR1 signal (manual garbage collection)
handleSigUsr1 :: DaemonContext t -> IO ()
handleSigUsr1 context = do
    -- Log GC signal
    logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal
        "Received SIGUSR1, triggering manual garbage collection..."

    -- Start GC in a separate thread
    void $ forkIO $ do
        let config = ctxConfig context

        -- Check if we can run GC with appropriate privilege context
        canGC <- withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence ->
            atomically $ checkGCLock (ctxState context)

        if canGC
            then do
                -- Check if another process is already running GC
                let lockPath = gcLockPath (initBuildEnv (daemonTmpDir config) (daemonStorePath config))
                gcRunning <- isGCRunning lockPath

                if gcRunning
                    then do
                        logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal
                            "Cannot run garbage collection: already in progress by another process"
                    else do
                        -- Run GC with proper privilege context
                        withPrivilegeScope (ctxPrivilegeEvidence context) $ \stEvidence -> do
                            -- Acquire GC lock
                            atomically $ acquireGCLock (ctxState context)

                            -- Create build environment
                            let env = initDaemonEnv (daemonTmpDir config) (daemonStorePath config) (Just "daemon")

                            -- Run GC
                            result <- runTen sBuild stEvidence collectGarbage env (initBuildState Build (BuildIdFromInt 0))

                            -- Process result
                            case result of
                                Left err ->
                                    logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
                                        "Manual garbage collection failed: " ++ show err
                                Right (stats, _) ->
                                    logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
                                        "Manual garbage collection completed: " ++
                                        show (gcCollected stats) ++ " paths collected, " ++
                                        show (gcBytes stats) ++ " bytes freed."

                            -- Release GC lock
                            atomically $ releaseGCLock (ctxState context)
            else
                logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal
                    "Cannot run garbage collection: daemon lock already held"

-- | Set up logging
setupLogging :: DaemonConfig -> IO (Maybe Handle)
setupLogging config = case daemonLogFile config of
    Nothing ->
        -- Log to stdout/stderr
        return Nothing

    Just path -> do
        -- Ensure log directory exists
        createDirectoryIfMissing True (takeDirectory path)

        -- Try to open log file
        result <- try $ openFile path AppendMode
        case result of
            Left (e :: SomeException) -> do
                -- Fall back to stderr on error
                hPutStrLn stderr $ "Warning: Could not open log file: " ++ show e
                return Nothing

            Right handle -> do
                -- Set buffer mode
                hSetBuffering handle LineBuffering

                return $ Just handle

-- | Close log handles
closeLogs :: Maybe Handle -> IO ()
closeLogs Nothing = return ()
closeLogs (Just handle) = hClose handle

-- | Log a message with timestamp and level
logMessage :: Maybe Handle -> LogLevel -> LogLevel -> String -> IO ()
logMessage mHandle configLevel level msg = do
    -- Only log if the level is less than or equal to config level
    when (levelToInt level <= levelToInt configLevel) $ do
        -- Get current time for timestamp
        now <- getCurrentTime
        let timestamp = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%z" now

        -- Format log message
        let logMsg = timestamp ++ " [" ++ show level ++ "] " ++ msg

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
  where
    levelToInt :: LogLevel -> Int
    levelToInt LogQuiet = 0
    levelToInt LogNormal = 1
    levelToInt LogVerbose = 2
    levelToInt LogDebug = 3

-- | Drop privileges if running as root
dropPrivileges :: DaemonContext t -> DaemonConfig -> IO ()
dropPrivileges context config = do
    -- Get the current user ID
    uid <- getRealUserID

    when (uid == 0) $ do
        -- Log that we're dropping privileges
        logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
            "Running as root, dropping privileges..."

        case (daemonUser config, daemonGroup config) of
            (Just userName, Just groupName) -> do
                -- Get user entry
                userEntry <- try $ getUserEntryForName (T.unpack userName)
                case userEntry of
                    Left (e :: SomeException) -> do
                        logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
                            "Error: User not found: " ++ T.unpack userName ++ " - " ++ show e
                        exitFailure
                    Right entry -> do
                        -- Get group entry
                        groupEntry <- try $ getGroupEntryForName (T.unpack groupName)
                        case groupEntry of
                            Left (e :: SomeException) -> do
                                logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
                                    "Error: Group not found: " ++ T.unpack groupName ++ " - " ++ show e
                                exitFailure
                            Right gentry -> do
                                -- Ensure store permissions are correct before dropping privileges
                                ensureStorePermissions context config (Sandbox.userID entry) (Sandbox.groupID gentry)

                                -- Keep the store root owned by root for security
                                -- But ensure the daemon can read/write store contents

                                -- Create a /nix/var/nix/daemon directory owned by daemon user
                                let daemonDir = daemonStorePath config </> "var/ten/daemon"
                                createDirectoryIfMissing True daemonDir
                                setOwnerAndGroup daemonDir (Sandbox.userID entry) (Sandbox.groupID gentry)
                                setFileMode daemonDir 0o700  -- Only the daemon user can access this

                                -- Set supplementary groups first
                                try $ setGroups [] -- Clear supplementary groups

                                -- Set group ID (must be done before dropping user privileges)
                                try $ setGroupID (Sandbox.groupID gentry)

                                -- Finally set user ID
                                try $ setUserID (Sandbox.userID entry)

                                -- Verify the change
                                newUid <- getEffectiveUserID
                                when (newUid == 0) $ do
                                    logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal
                                        "Failed to drop privileges - still running as root!"
                                    exitFailure

                                logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
                                    "Successfully dropped privileges to user=" ++
                                    T.unpack userName ++ ", group=" ++ T.unpack groupName

            (Just userName, Nothing) -> do
                -- Get user entry
                userEntry <- try $ getUserEntryForName (T.unpack userName)
                case userEntry of
                    Left (e :: SomeException) -> do
                        logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
                            "Error: User not found: " ++ T.unpack userName ++ " - " ++ show e
                        exitFailure
                    Right entry -> do
                        -- Use user's primary group
                        let primaryGid = Sandbox.groupID entry

                        -- Ensure store permissions are correct before dropping privileges
                        ensureStorePermissions context config (Sandbox.userID entry) primaryGid

                        -- Set supplementary groups first
                        try $ setGroups [] -- Clear supplementary groups

                        -- Set group ID (must be done before dropping user privileges)
                        try $ setGroupID primaryGid

                        -- Finally set user ID
                        try $ setUserID (Sandbox.userID entry)

                        -- Verify the change
                        newUid <- getEffectiveUserID
                        when (newUid == 0) $ do
                            logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal
                                "Failed to drop privileges - still running as root!"
                            exitFailure

                        logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
                            "Successfully dropped privileges to user=" ++ T.unpack userName

            _ ->
                logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal
                    "No user/group specified in config, continuing to run as root"

-- | Execute an action with a privilege transition
withPrivilegeTransition :: forall s t a. PrivilegeTransition s t -> (SPrivilegeTier t -> IO a) -> SPrivilegeTier s -> IO a
withPrivilegeTransition transition action stEvidence = do
    -- Handle privilege transition
    case transition of
        -- We can only drop privileges, never gain them
        DropPrivilege -> action sBuilder  -- Pass builder evidence

-- | Execute a function with privilege scope handling
withPrivilegeScope :: SPrivilegeTier t -> (forall s. SPrivilegeTier s -> IO a) -> IO a
withPrivilegeScope stEvidence action = do
    -- Forward the evidence to the action
    action stEvidence

-- | Run an action with daemon privileges
runPrivileged :: (SPrivilegeTier 'Daemon -> IO a) -> IO a
runPrivileged action = action sDaemon

-- | Run an action with builder privileges
runUnprivileged :: (SPrivilegeTier 'Builder -> IO a) -> IO a
runUnprivileged action = action sBuilder

-- | Ensure store directories have appropriate permissions for unprivileged user
ensureStorePermissions :: DaemonContext t -> DaemonConfig -> UserID -> GroupID -> IO ()
ensureStorePermissions context config uid gid = do
    let storeDir = daemonStorePath config
        tmpDir = daemonTmpDir config
        stateFile = daemonStateFile config
        socketPath = daemonSocketPath config

    -- Create necessary directories if they don't exist
    createDirectoryIfMissing True storeDir
    createDirectoryIfMissing True tmpDir
    createDirectoryIfMissing True (takeDirectory stateFile)
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Create critical store subdirectories
    createDirectoryIfMissing True (storeDir </> "var/ten")
    createDirectoryIfMissing True (storeDir </> "var/ten/db")
    createDirectoryIfMissing True (storeDir </> "var/ten/gcroots")
    createDirectoryIfMissing True (storeDir </> "var/ten/profiles")
    createDirectoryIfMissing True (storeDir </> "var/log/ten")

    -- Set appropriate ownership and permissions for key directories

    -- Keep store root owned by root, but writable by daemon group
    setFileMode storeDir (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                         groupReadMode .|. groupWriteMode .|. groupExecuteMode)

    -- Set var/ten group writable
    setOwnerAndGroup (storeDir </> "var/ten") 0 gid
    setFileMode (storeDir </> "var/ten") (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                                         groupReadMode .|. groupWriteMode .|. groupExecuteMode)

    -- Set /nix/var/nix/db group writable
    setOwnerAndGroup (storeDir </> "var/ten/db") uid gid
    setFileMode (storeDir </> "var/ten/db") (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                                            groupReadMode .|. groupWriteMode .|. groupExecuteMode)

    -- Set tmpDir to daemon user
    setOwnerAndGroup tmpDir uid gid
    setFileMode tmpDir (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                       groupReadMode .|. groupWriteMode .|. groupExecuteMode)

    -- Set state file directory permissions
    setOwnerAndGroup (takeDirectory stateFile) uid gid
    setFileMode (takeDirectory stateFile) (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                                         groupReadMode .|. groupWriteMode .|. groupExecuteMode)

    -- Ensure socket directory is accessible
    setFileMode (takeDirectory socketPath) (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                                          groupReadMode .|. groupWriteMode .|. groupExecuteMode .|.
                                          otherReadMode .|. otherExecuteMode)

    logMessage (ctxLogHandle context) (daemonLogLevel config) LogNormal $
        "Set appropriate permissions on store directories for unprivileged user"

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
            -- Try to read content
            result <- try $ readFile path
            case result of
                Left (_ :: SomeException) -> return Nothing
                Right content ->
                    -- Parse as integer
                    case reads content of
                        [(pid, "")] -> return $ Just pid
                        _ -> return Nothing

-- | Get PID file path
getPidFilePath :: FilePath -> IO FilePath
getPidFilePath socketPath = do
    -- Get runtime directory
    runtimeDir <- getXdgRuntimeDir

    -- Create PID file path
    let pidFile = runtimeDir </> "ten-daemon.pid"

    -- Create directory if needed
    createDirectoryIfMissing True runtimeDir

    return pidFile

-- | Get XDG runtime directory
getXdgRuntimeDir :: IO FilePath
getXdgRuntimeDir = do
    -- Try XDG_RUNTIME_DIR environment variable
    mRuntimeDir <- lookupEnv "XDG_RUNTIME_DIR"
    case mRuntimeDir of
        Just dir -> return $ dir </> "ten"
        Nothing -> do
            -- Fall back to /tmp
            return "/tmp/ten-runtime"

-- | Check if a process is running
isProcessRunning :: ProcessID -> IO Bool
isProcessRunning pid = do
    -- Try to send signal 0 to the process
    result <- try $ signalProcess 0 pid
    case result of
        Left (_ :: SomeException) -> return False  -- Process doesn't exist
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
            maybe "" takeDirectory (daemonLogFile config),
            -- Add GC lock directory to ensure it exists with correct permissions
            takeDirectory (daemonStorePath config </> "var/ten/gc.lock"),
            -- Add daemon-specific directories
            daemonStorePath config </> "var/ten/db",
            daemonStorePath config </> "var/ten/gcroots",
            daemonStorePath config </> "var/ten/profiles",
            daemonStorePath config </> "var/log/ten",
            daemonTmpDir config </> "builds",
            daemonTmpDir config </> "sandbox"
            ]

    -- Create each directory with appropriate permissions
    forM_ directories $ \dir ->
        unless (null dir) $ do
            createDirectoryIfMissing True dir

            -- Set permissions (755) - owner and group can read/write/execute
            setFileMode dir (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                            groupReadMode .|. groupExecuteMode .|.
                            otherReadMode .|. otherExecuteMode)

-- | Check store access and initialize if needed
checkStoreAccess :: DaemonConfig -> IO ()
checkStoreAccess config = do
    -- Check if store directory exists
    storeExists <- doesDirectoryExist (daemonStorePath config)

    unless storeExists $ do
        -- Create store directory
        createDirectoryIfMissing True (daemonStorePath config)

        -- Initialize the store
        initializeStore (daemonStorePath config)

        -- Create subdirectories
        forM_ ["tmp", "gc-roots", "var/ten"] $ \subdir -> do
            let path = daemonStorePath config </> subdir
            createDirectoryIfMissing True path

            -- Set permissions
            setFileMode path (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode .|.
                             groupReadMode .|. groupWriteMode .|. groupExecuteMode)

-- | Load daemon state from file
loadDaemonState :: DaemonConfig -> IO (Either String (DaemonState 'Daemon))
loadDaemonState config = do
    -- Check if state file exists
    let stateFile = daemonStateFile config
    exists <- doesFileExist stateFile

    if not exists
        then do
            -- No state file, create a new state
            state <- runPrivileged $ \stEvidence ->
                initDaemonState stateFile (daemonMaxJobs config) 100 stEvidence
            return $ Right state
        else do
            -- Read and parse state file
            result <- try $ runPrivileged $ \stEvidence ->
                loadStateFromFile stateFile (daemonMaxJobs config) 100 stEvidence
            case result of
                Left (e :: SomeException) ->
                    return $ Left $ "Error reading state file: " ++ show e
                Right state ->
                    return $ Right state

-- | Save daemon state to file
saveDaemonState :: DaemonConfig -> DaemonState t -> IO ()
saveDaemonState config state = do
    -- Try to save the state to file
    result <- try $ saveStateToFile state
    case result of
        Left (e :: SomeException) ->
            hPutStrLn stderr $ "Warning: Failed to save daemon state: " ++ show e
        Right _ ->
            return ()

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

-- | Clean shutdown of daemon
cleanShutdown :: DaemonContext t -> IO ()
cleanShutdown context = do
    -- Log shutdown
    logMessage (ctxLogHandle context) (daemonLogLevel (ctxConfig context)) LogNormal
        "Initiating clean shutdown..."

    -- Signal shutdown
    atomically $ writeTVar (ctxShutdownFlag context) True

    -- Let main loop handle the rest

-- | Set up daemon configuration
setupDaemonConfig :: [String] -> IO DaemonConfig
setupDaemonConfig args = do
    -- Get default configuration
    defaultConfig <- getDefaultConfig

    -- Process command line arguments to override defaults
    -- In a real implementation this would parse the args and apply them to the config
    -- For now, we just return the default config
    return defaultConfig

-- | Daemonize the process (detach from terminal)
daemonize :: IO a -> IO a
daemonize action = do
    -- First fork to create background process
    pid <- forkProcess $ do
        -- Become session leader
        _ <- createSession

        -- Second fork to prevent reacquiring a controlling terminal
        pid2 <- forkProcess $ do
            -- Change working directory to root
            setCurrentDirectory "/"

            -- Close standard file descriptors
            mapM_ closeFd [stdInput, stdOutput, stdError]

            -- Open /dev/null for stdin, stdout, stderr
            devNull <- openFd "/dev/null" Ten.Core.ReadWrite Nothing defaultFileFlags
            dupTo devNull stdInput
            dupTo devNull stdOutput
            dupTo devNull stdError
            closeFd devNull

            -- Execute the action
            action

        -- Exit the intermediate process
        exitImmediately ExitSuccess

    -- Exit the original process
    exitImmediately ExitSuccess

-- | Helper functions for process management

-- | Safely get a user's UID with fallback options
safeGetUserUID :: String -> IO UserID
safeGetUserUID username = do
    result <- try $ getUserEntryForName username
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nobody
            hPutStrLn stderr $ "Warning: User " ++ username ++ " not found, falling back to nobody"
            nobodyResult <- try $ getUserEntryForName "nobody"
            case nobodyResult of
                Left (_ :: SomeException) -> do
                    -- Ultimate fallback to current user if even nobody doesn't exist
                    getRealUserID
                Right entry ->
                    return $ Sandbox.userID entry
        Right entry ->
            return $ Sandbox.userID entry

-- | Safely get a group's GID with fallback options
safeGetGroupGID :: String -> IO GroupID
safeGetGroupGID groupname = do
    result <- try $ getGroupEntryForName groupname
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nogroup
            hPutStrLn stderr $ "Warning: Group " ++ groupname ++ " not found, falling back to nogroup"
            nogroupResult <- try $ getGroupEntryForName "nogroup"
            case nogroupResult of
                Left (_ :: SomeException) -> do
                    -- Try nobody as group
                    nobodyResult <- try $ getGroupEntryForName "nobody"
                    case nobodyResult of
                        Left (_ :: SomeException) -> do
                            -- Ultimate fallback to current group
                            getDefaultGroupID
                        Right entry ->
                            return $ Sandbox.groupID entry
                Right entry ->
                    return $ Sandbox.groupID entry
        Right entry ->
            return $ Sandbox.groupID entry

-- | Get the default group ID for the current user
getDefaultGroupID :: IO GroupID
getDefaultGroupID = do
    uid <- getRealUserID
    entry <- getUserEntryForName "nobody" -- Fallback when can't get entry for current user
    return $ Sandbox.groupID entry

-- | Helper function to remove a file if it exists
removeFileIfExists :: FilePath -> IO ()
removeFileIfExists path = do
    exists <- doesFileExist path
    when exists $ do
        -- Try removing the file, ignoring any errors
        try $ removeFile path :: IO (Either SomeException ())

-- | Remove a socket file if it exists
removeSocketIfExists :: FilePath -> IO ()
removeSocketIfExists path = do
    exists <- doesFileExist path
    when exists $ do
        -- Try removing the file, ignoring any errors
        try $ removeFile path :: IO (Either SomeException ())

-- | Set the file creation mask (umask) to a specific value
setFileCreationMask :: FileMode -> IO ()
setFileCreationMask mode = do
    -- Set umask to mode (POSIX call)
    _ <- System.Posix.Files.setFileCreationMask mode
    return ()

-- | Helper to convert raw exception to a specialized exception if possible
fromException :: Exception e => SomeException -> Maybe e
fromException = Control.Exception.fromException

-- | Helper function to get PID from a process handle
getPid :: ProcessHandle -> IO (Maybe ProcessID)
getPid ph = do
    -- Simple implementation - this would need OS-specific functionality in real code
    mbp <- try $ createProcess (proc "ps" ["-o", "pid="]) { std_out = CreatePipe }
    case mbp of
        Left (_ :: SomeException) -> return Nothing
        Right (_, Just hout, _, _) -> do
            output <- hGetContents hout
            case reads output of
                [(pid, _)] -> return $ Just pid
                _ -> return Nothing
        Right _ -> return Nothing

-- | Helper to read process output
readCreateProcessWithExitCode :: CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithExitCode cp stdin = do
    (Just inh, Just outh, Just errh, ph) <-
        createProcess cp { std_in = CreatePipe,
                          std_out = CreatePipe,
                          std_err = CreatePipe }

    -- Write input to stdin
    when (not (null stdin)) $ do
        hPutStr inh stdin
        hClose inh

    -- Read output and error
    out <- hGetContents outh
    err <- hGetContents errh

    -- Wait for process to finish
    exitCode <- waitForProcess ph

    -- Close handles
    hClose outh
    hClose errh

    return (exitCode, out, err)

-- | Change credentials of a process
changeProcessCredentials :: ProcessID -> UserID -> GroupID -> IO ()
changeProcessCredentials pid uid gid = do
    -- In a real implementation, this would use ptrace or OS-specific mechanisms
    -- This is a stub implementation
    return ()

-- | Verify a process's credentials changed
verifyProcessCredentials :: ProcessID -> UserID -> IO ()
verifyProcessCredentials pid expectedUid = do
    -- In a real implementation, this would check process credentials
    -- This is a stub implementation
    return ()

-- | Create a new session (setsid)
createSession :: IO ProcessID
createSession = do
    -- In a real implementation, this would call setsid()
    -- For now, just return the current process ID
    getProcessID

-- | Set current directory
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory path = do
    -- In a real implementation, this would change directory
    -- This is a stub implementation
    return ()

-- | Initialize the store
initializeStore :: FilePath -> IO ()
initializeStore storePath = do
    -- In a real implementation, this would set up the store structure
    -- This is a stub implementation
    createDirectoryIfMissing True storePath
    return ()

-- | Start server
startServer :: SPrivilegeTier t -> Socket -> DaemonState t -> IO (ServerControl t)
startServer stEvidence socket state = do
    -- This would initialize the server control in a real implementation
    return $ ServerControl socket state stEvidence

-- | Stop server
stopServer :: SPrivilegeTier t -> ServerControl t -> IO ()
stopServer stEvidence serverControl = do
    -- In a real implementation, this would shut down the server
    -- This is a stub implementation
    return ()

-- | Update build status
updateBuildStatus :: DaemonState t -> BuildId -> BuildStatus -> IO ()
updateBuildStatus state buildId status = do
    -- This would update the build status in a real implementation
    return ()

-- | Set up sandbox
setupSandbox :: FilePath -> Text -> TenM 'Build 'Daemon ()
setupSandbox dir config = do
    -- This would set up a sandbox in a real implementation
    return ()

-- | Acquire GC lock
acquireGCLock :: DaemonState t -> STM ()
acquireGCLock state = do
    -- This would acquire a GC lock in a real implementation
    return ()

-- | Release GC lock
releaseGCLock :: DaemonState t -> STM ()
releaseGCLock state = do
    -- This would release a GC lock in a real implementation
    return ()

-- | Check GC lock
checkGCLock :: DaemonState t -> STM Bool
checkGCLock state = do
    -- This would check if a GC lock can be acquired
    return True

-- | Initialize daemon state
initDaemonState :: FilePath -> Int -> Int -> SPrivilegeTier t -> IO (DaemonState t)
initDaemonState stateFile maxJobs maxConn evidence = do
    -- This would initialize a daemon state in a real implementation
    -- Return a placeholder state
    return $ DaemonState stateFile maxJobs maxConn evidence

-- | Load state from file
loadStateFromFile :: FilePath -> Int -> Int -> SPrivilegeTier t -> IO (DaemonState t)
loadStateFromFile stateFile maxJobs maxConn evidence = do
    -- This would load state from a file in a real implementation
    -- Return a placeholder state
    return $ DaemonState stateFile maxJobs maxConn evidence

-- | Save state to file
saveStateToFile :: DaemonState t -> IO ()
saveStateToFile state = do
    -- This would save state to a file in a real implementation
    return ()

-- | DaemonState placeholder type
data DaemonState t = DaemonState FilePath Int Int (SPrivilegeTier t)

-- | ServerControl placeholder type
data ServerControl t = ServerControl Socket (DaemonState t) (SPrivilegeTier t)

-- | LogLevel enum from Ten.Core
data LogLevel = LogQuiet | LogNormal | LogVerbose | LogDebug
    deriving (Show, Eq, Ord)

-- | Get default socket path placeholder
getDefaultSocketPath :: IO FilePath
getDefaultSocketPath = do
    home <- getHomeDirectory
    return $ home </> ".ten" </> "daemon.sock"

-- | Get default configuration placeholder
getDefaultConfig :: IO DaemonConfig
getDefaultConfig = do
    home <- getHomeDirectory
    return $ DaemonConfig {
        daemonSocketPath = home </> ".ten" </> "daemon.sock",
        daemonStorePath = home </> ".ten" </> "store",
        daemonStateFile = home </> ".ten" </> "daemon.state",
        daemonLogFile = Just $ home </> ".ten" </> "logs" </> "daemon.log",
        daemonLogLevel = LogNormal,
        daemonGcInterval = Just 3600,
        daemonUser = Nothing,
        daemonGroup = Nothing,
        daemonAllowedUsers = Set.singleton (T.pack $ takeFileName home),
        daemonMaxJobs = 2,
        daemonForeground = False,
        daemonTmpDir = home </> ".ten" </> "tmp"
    }

-- | Default sandbox configuration
defaultSandboxConfig :: Text
defaultSandboxConfig = "{ sandbox = true; }"

-- | Privilege transition type (for transitioning from one privilege tier to another)
data PrivilegeTransition s t = DropPrivilege

-- | File operations
(.|.) :: FileMode -> FileMode -> FileMode
a .|. b = a + b
