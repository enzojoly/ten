{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent (threadDelay, myThreadId)
import Control.Exception (bracket, try, catch, SomeException, IOException, finally)
import Control.Monad (when, unless, forever, void, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.GetOpt -- Needed for CLI parsing
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import System.Environment (getArgs, getProgName, lookupEnv, getEnvironment)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode(..), withFile, hPutStrLn, stdout, stderr, hSetBuffering, BufferMode(..))
import System.Posix.Files (fileExist, setFileMode)
import System.Posix.Process (getProcessID, exitImmediately)
import System.Posix.Signals hiding (SignalSet, Handler) -- Avoid conflict with Control.Exception
import qualified System.Posix.Signals as PosixSignals
import System.Posix.Types (UserID, GroupID) -- Import necessary types
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName, UserEntry, GroupEntry,
                         getRealUserID, setUserID, setGroupID, getUserEntryForName, getGroupEntryForName,
                         userID, groupID)
import qualified Network.Socket as Net -- For socket operations

-- Import Ten modules (ensure exposed by ten.cabal library)
import Ten (
    -- Core
    TenM, BuildEnv(..), BuildState(..), BuildError(..), DaemonConfig(..), RunMode(..),
    PrivilegeTier(..), SPrivilegeTier(..), sDaemon, sBuild,
    runTen, runTenDaemon, initDaemonEnv, initBuildState, BuildId(..),
    -- Store
    initializeStore, createStoreDirectories, verifyStore,
    -- DB
    ensureDBDirectories,
    -- Protocol/Client (for status/shutdown checks)
    isDaemonRunning -- Needs to be exposed or reimplemented locally
    )
-- Direct imports if needed and exposed
import qualified Ten.Daemon.State as DaemonState
import qualified Ten.Daemon.Server as DaemonServer
import qualified Ten.Daemon.Protocol as Protocol -- If Protocol types are needed directly
import qualified Ten.DB.Core as DBCore -- If needed for DB initialization detail

-- LogLevel definition (must be defined if Ten.Daemon.Config is removed)
data LogLevel = LogQuiet | LogNormal | LogVerbose | LogDebug
    deriving (Show, Eq, Ord)

-- Daemon Options/Commands (moved from TenDaemon.hs CLI section)
-- | Command line options
data DaemonOption
    = OptHelp                       -- Show help
    | OptVersion                    -- Show version
    | OptConfig FilePath            -- Path to config file
    | OptSocketPath FilePath        -- Override socket path
    | OptStorePath FilePath         -- Override store path
    | OptStateFile FilePath         -- Override state file
    | OptUser String                -- Override user
    | OptGroup String               -- Override group
    | OptLogFile FilePath           -- Override log file
    | OptLogLevel LogLevel          -- Override log level
    | OptForeground                 -- Run in foreground
    | OptPidFile FilePath           -- PID file path
    | OptReload                     -- Reload configuration (Signal based)
    | OptDebug                      -- Enable debug mode
    | OptMaxJobs Int                -- Maximum concurrent jobs
    deriving (Show, Eq)

-- | Command to execute
data DaemonCommand
    = CmdStart                      -- Start the daemon
    | CmdStop                       -- Stop the daemon (Signal based)
    | CmdRestart                    -- Restart the daemon (Signal based)
    | CmdStatus                     -- Show daemon status (External command)
    | CmdHelp                       -- Show help
    | CmdVersion                    -- Show version
    deriving (Show, Eq)

-- | Parse command line options
options :: [OptDescr DaemonOption]
options =
    [ Option ['h'] ["help"]
        (NoArg OptHelp)
        "Show this help message"
    , Option ['v'] ["version"]
        (NoArg OptVersion)
        "Show version information"
    , Option ['c'] ["config"]
        (ReqArg OptConfig "FILE")
        "Path to configuration file"
    , Option ['s'] ["socket"]
        (ReqArg OptSocketPath "PATH")
        "Path to daemon socket"
    , Option [] ["store"]
        (ReqArg OptStorePath "PATH")
        "Path to store"
    , Option [] ["state-file"]
        (ReqArg OptStateFile "PATH")
        "Path to daemon state file"
    , Option ['u'] ["user"]
        (ReqArg OptUser "USER")
        "User to run daemon as"
    , Option ['g'] ["group"]
        (ReqArg OptGroup "GROUP")
        "Group to run daemon as"
    , Option ['l'] ["log-file"]
        (ReqArg OptLogFile "FILE")
        "Path to log file (use 'stdout' for console)"
    , Option [] ["log-level"]
        (ReqArg (\s -> case s of
                "quiet"   -> OptLogLevel LogQuiet
                "normal"  -> OptLogLevel LogNormal
                "verbose" -> OptLogLevel LogVerbose
                "debug"   -> OptLogLevel LogDebug
                _ -> error $ "Invalid log level: " ++ s)
            "LEVEL")
        "Log level (quiet, normal, verbose, debug)"
    , Option ['f'] ["foreground"]
        (NoArg OptForeground)
        "Run in foreground (don't daemonize)"
    , Option ['p'] ["pid-file"]
        (ReqArg OptPidFile "FILE")
        "Path to PID file"
    -- Reload is handled by SIGHUP, not a command line arg for starting
    -- , Option ['r'] ["reload"] (NoArg OptReload) "Reload configuration"
    , Option ['d'] ["debug"]
        (NoArg OptDebug)
        "Enable debug mode"
    , Option ['j'] ["max-jobs"]
        (ReqArg (OptMaxJobs . read) "NUM")
        "Maximum number of concurrent build jobs"
    ]

-- | Parse command line arguments for the daemon executable
parseDaemonArgs :: [String] -> IO (DaemonCommand, [DaemonOption])
parseDaemonArgs args =
    -- Daemon typically only takes 'start' implicitly or options
    case getOpt Permute options args of
        (opts, [], []) -> return (CmdStart, opts) -- Assume start if no command given
        (opts, nonOpts, []) ->
            -- Check if nonOpts is a valid command (like 'help' or 'version')
            case nonOpts of
                ["help"]    -> return (CmdHelp, opts)
                ["version"] -> return (CmdVersion, opts)
                _           -> error $ "Unexpected arguments: " ++ unwords nonOpts ++ "\n" ++ usageInfo usageHeader options
        (_, _, errs) ->
            error $ concat errs ++ usageInfo usageHeader options

-- | Show usage information
usageHeader :: String
usageHeader = "Usage: ten-daemon [OPTIONS]\n\n" ++
              "Starts the Ten build daemon.\n\n" ++
              "Control Commands (use 'ten' client):\n" ++
              "  ten daemon start\n" ++
              "  ten daemon stop\n" ++
              "  ten daemon restart\n" ++
              "  ten daemon status\n\n" ++
              "Options:"

-- | Show help information
showHelp :: IO ()
showHelp = putStrLn $ usageInfo usageHeader options

-- | Show version information
showVersion :: IO ()
showVersion = putStrLn "Ten Daemon version 0.1.0" -- Adjust version as needed

-- | Main entry point for the daemon
main :: IO ()
main = do
    args <- getArgs
    (cmd, opts) <- parseDaemonArgs args

    -- Handle help and version commands early
    case cmd of
        CmdHelp -> showHelp >> exitSuccess
        CmdVersion -> showVersion >> exitSuccess
        CmdStart -> return () -- Proceed to start
        _ -> error $ "Invalid command for ten-daemon executable: " ++ show cmd -- Should not happen

    -- Extract options using the corrected getOpt'
    let getOpt' constructor = foldr (\opt acc -> case opt of x -> Just x; _ -> acc) Nothing opts
    let hasOpt constructor = any (\case { x | x == constructor -> True; _ -> False }) opts -- Simplified check

    let configFile = getOpt' OptConfig
    let socketPath = getOpt' OptSocketPath
    let storePath = getOpt' OptStorePath
    let stateFile = getOpt' OptStateFile
    let userOverride = getOpt' OptUser
    let groupOverride = getOpt' OptGroup
    let logFile = getOpt' OptLogFile
    let logLevel = getOpt' OptLogLevel
    let foreground = hasOpt OptForeground
    let pidFile = getOpt' OptPidFile
    let debug = hasOpt OptDebug
    let maxJobs = getOpt' OptMaxJobs

    -- Load configuration (Implement loadConfigFromFile and loadConfigFromEnv)
    config <- case configFile of
        Just path -> loadConfigFromFile path >>= \case
                        Left err -> hPutStrLn stderr ("Error loading config: " ++ err) >> loadConfigFromEnv
                        Right cfg -> return cfg
        Nothing -> loadConfigFromEnv

    -- Apply command line overrides to config
    let config' = config
            { daemonSocketPath = fromMaybe (daemonSocketPath config) socketPath
            , daemonStorePath = fromMaybe (daemonStorePath config) storePath
            , daemonStateFile = fromMaybe (daemonStateFile config) stateFile
            , daemonUser = T.pack <$> userOverride `orElse` daemonUser config
            , daemonGroup = T.pack <$> groupOverride `orElse` daemonGroup config
            , daemonLogFile = (\f -> if f == "stdout" || f == "stderr" then Nothing else Just f) <$> logFile `orElse` daemonLogFile config
            , daemonLogLevel = fromMaybe (daemonLogLevel config) logLevel
            , daemonForeground = foreground || daemonForeground config
            , daemonMaxJobs = fromMaybe (daemonMaxJobs config) maxJobs
            }

    -- Enable debug mode if requested
    let config'' = if debug
                  then config' { daemonLogLevel = LogDebug }
                  else config'

    -- Make directories as needed (ensure permissions later if dropping privs)
    createDirectoryIfMissing True (takeDirectory (daemonSocketPath config''))
    createDirectoryIfMissing True (daemonStorePath config'')
    createDirectoryIfMissing True (takeDirectory (daemonStateFile config''))
    case daemonLogFile config'' of
        Just logPath -> createDirectoryIfMissing True (takeDirectory logPath)
        Nothing -> return ()

    -- Start the daemon process logic
    startDaemonProcess config'' pidFile

-- Maybe Or Else operator
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing  y = y

-- | Start the daemon process logic (run after parsing args)
startDaemonProcess :: DaemonConfig -> Maybe FilePath -> IO ()
startDaemonProcess config pidFilePath = do
    -- Check if the daemon is already running using the configured socket path
    running <- isDaemonRunning (daemonSocketPath config)
    when running $ do
        hPutStrLn stderr $ "Error: Daemon appears to be already running (socket file exists: " ++ daemonSocketPath config ++ ")"
        exitFailure

    -- Acquire global lock early
    lockHandle <- acquireGlobalLockSafe config

    -- Fork and daemonize if not running in foreground
    if daemonForeground config
        then do
            putStrLn $ "Starting Ten daemon in foreground (PID: " ++ show =<< getProcessID ++ ")"
            -- Write PID file even in foreground mode if requested
            forM_ pidFilePath writePidFile
            runDaemonSafely config lockHandle `finally` (releaseGlobalLock lockHandle >> forM_ pidFilePath removeFileSafe)
        else do
            putStrLn $ "Starting Ten daemon in background (socket: " ++ daemonSocketPath config ++ ")"
            let pidFile = fromMaybe (takeDirectory (daemonSocketPath config) </> "ten-daemon.pid") pidFilePath
            createDirectoryIfMissing True (takeDirectory pidFile) -- Ensure PID directory exists

            -- Daemonize the process
            daemonize $ do
                -- Now running in the background process
                -- Write PID file *after* daemonizing
                writePidFile pidFile

                -- Run the main daemon logic, ensuring cleanup
                runDaemonSafely config lockHandle `finally` (releaseGlobalLock lockHandle >> removeFileSafe pidFile)

-- | Run the daemon core logic with exception handling
runDaemonSafely :: DaemonConfig -> LockHandle -> IO ()
runDaemonSafely config lockHandle = do
    -- Set up signal handlers within the daemon process
    installSignalHandlers config

    -- Initialize store structure (permissions might be adjusted later)
    setupStoreStructure config

    -- Initialize secure store (DB, etc.) - might need root initially
    initializeSecureStore config

    -- Drop privileges *after* initial setup requiring root (like socket binding, store setup)
    dropPrivilegesIfNeeded config

    -- Run the main daemon server logic
    runDaemonWithConfig config `catch` \(e :: SomeException) -> do
        logToFile (daemonLogFile config) $ "[CRITICAL] Daemon crashed: " ++ displayException e
        -- Release lock before exiting abnormally
        releaseGlobalLock lockHandle
        exitFailure -- Ensure daemon exits on unhandled exception

-- | Setup store directory structure
setupStoreStructure :: DaemonConfig -> IO ()
setupStoreStructure config = do
    let storeDir = daemonStorePath config
    createDirectoryIfMissing True storeDir
    -- Create essential subdirectories
    let subdirs = ["tmp", "gc-roots", "var/ten", "var/log", "var/nix/db"]
    forM_ subdirs $ \dir -> createDirectoryIfMissing True (storeDir </> dir)
    -- Set base store permissions (may be overridden later if dropping privs)
    catch (setFileMode storeDir 0o755) (\(_ :: IOException) -> hPutStrLn stderr $ "Warning: could not set permissions on " ++ storeDir)
    -- Create DB directories (permissions set inside ensureDBDirectories)
    handleDBErrorAsWarning $ ensureDBDirectories storeDir

-- | Acquire global lock safely
type LockHandle = Handle -- Or a custom type wrapping the handle/FD

acquireGlobalLockSafe :: DaemonConfig -> IO LockHandle
acquireGlobalLockSafe config = do
    let lockPath = daemonStorePath config </> "var/ten/daemon.lock"
    createDirectoryIfMissing True (takeDirectory lockPath)
    -- Attempt to create and lock the file
    catch (acquireGlobalLock lockPath) $ \(e :: SomeException) -> do
        hPutStrLn stderr $ "Error: Could not acquire global daemon lock at " ++ lockPath
        hPutStrLn stderr $ "Ensure no other daemon instance is running and you have permissions."
        hPutStrLn stderr $ "Details: " ++ show e
        exitFailure

-- Placeholder: Needs actual implementation for file locking
acquireGlobalLock :: FilePath -> IO LockHandle
acquireGlobalLock lockPath = do
    -- Example using simple file creation, replace with proper locking (e.g., flock)
    h <- openFile lockPath WriteMode
    hSetBuffering h NoBuffering
    -- Try to lock (this is a placeholder for actual file locking)
    hPutStrLn h =<< show <$> getProcessID
    hFlush h
    putStrLn $ "Acquired lock: " ++ lockPath
    return h

releaseGlobalLock :: LockHandle -> IO ()
releaseGlobalLock h = do
    putStrLn "Releasing global lock."
    hClose h `catch` \(_ :: IOException) -> return ()
    -- Remove the lock file if possible (best effort)
    -- Need the path here, ideally LockHandle would store it
    -- removeFileSafe lockPath


-- | Initialize secure store
initializeSecureStore :: DaemonConfig -> IO ()
initializeSecureStore config = do
    let storeDir = daemonStorePath config
    -- Initialize store (might create files/dirs)
    handleDBErrorAsWarning $ initializeStore storeDir
    -- Ensure DB directories exist again after potential store init
    handleDBErrorAsWarning $ ensureDBDirectories storeDir
    -- Verify store structure and permissions
    handleDBErrorAsWarning $ verifyStore storeDir

handleDBErrorAsWarning :: IO () -> IO ()
handleDBErrorAsWarning action = catch action $ \(e :: SomeException) ->
    hPutStrLn stderr $ "Warning during store/DB initialization: " ++ show e

-- | Write PID file
writePidFile :: FilePath -> IO ()
writePidFile path = do
    pid <- getProcessID
    catch (writeFile path (show pid)) $ \(e :: IOException) ->
        hPutStrLn stderr $ "Warning: Could not write PID file to " ++ path ++ ": " ++ show e

-- | Remove file safely (ignoring errors)
removeFileSafe :: FilePath -> IO ()
removeFileSafe path = catch (removeFile path) (\(_ :: IOException) -> return ())

-- | Wait for socket file to disappear (placeholder)
waitForSocketToDisappear :: FilePath -> IO ()
waitForSocketToDisappear path = do
    -- In a real implementation, poll for file existence
    threadDelay 500000 -- Wait 0.5s

-- | Run the daemon with given configuration
runDaemonWithConfig :: DaemonConfig -> IO ()
runDaemonWithConfig config = do
    setupLogging config

    -- Initialize database connection pool (or single connection)
    dbConn <- catch (DBCore.initializeDatabase (daemonDbPath config) 5000) $
        \(e :: SomeException) -> logToFile (daemonLogFile config) ("[CRITICAL] Database init failed: " ++ show e) >> exitFailure

    -- Load or initialize daemon state
    daemonState <- loadOrInitializeState config dbConn

    -- Initialize secure daemon socket
    serverSocket <- catch (createServerSocket (daemonSocketPath config)) $
        \(e :: SomeException) -> logToFile (daemonLogFile config) ("[CRITICAL] Socket creation failed: " ++ show e) >> exitFailure

    -- Initialize protocol handlers (if needed, maybe part of server?)
    -- protocolHandlers <- initializeProtocolHandlers

    logToFile (daemonLogFile config) "[INFO] Daemon initialized successfully. Starting server loop."

    -- Run the main server loop (needs DaemonState, Socket, Config)
    -- Assuming runDaemonServer is the main loop accepting connections
    DaemonServer.startServer serverSocket daemonState config
        `finally` (logToFile (daemonLogFile config) "[INFO] Server loop terminated." >> DBCore.closeDatabase dbConn)


-- | Set up logging based on configuration
setupLogging :: DaemonConfig -> IO (Handle, Handle) -- Returns (accessLog, securityLog)
setupLogging config = do
    accessLogH <- setupLogHandle (daemonLogFile config) stdout
    -- Security logs might go to a separate file or stderr even if access logs go to stdout
    let secLogPath = fmap (++ ".security") (daemonLogFile config)
    securityLogH <- setupLogHandle secLogPath stderr

    -- Set line buffering for immediate output
    hSetBuffering accessLogH LineBuffering
    hSetBuffering securityLogH LineBuffering

    return (accessLogH, securityLogH)

setupLogHandle :: Maybe FilePath -> Handle -> IO Handle
setupLogHandle Nothing fallbackH = return fallbackH
setupLogHandle (Just path) fallbackH =
    catch (openFile path AppendMode) $ \(e :: IOException) -> do
        hPutStrLn stderr $ "Warning: Could not open log file " ++ path ++ ": " ++ show e
        return fallbackH

-- | Load existing state or initialize new state
loadOrInitializeState :: DaemonConfig -> DBCore.Database 'Daemon -> IO (DaemonState.DaemonState 'Daemon)
loadOrInitializeState config db = do
    let stateFile = daemonStateFile config
    stateExists <- doesFileExist stateFile
    if stateExists then do
        result <- DaemonState.loadStateFromFile sDaemon stateFile (daemonMaxJobs config) 100 -- Max history = 100
        case result of
            -- Need to handle loading errors properly, perhaps by initializing fresh state
             Right state -> logInfo "Loaded daemon state." >> return state
             Left err -> logWarning ("Failed to load state: " ++ show err ++ ". Initializing fresh state.") >> initState
    else do
        logInfo "Initializing new daemon state."
        initState
  where
      initState = DaemonState.initDaemonState sDaemon (daemonStateFile config) (daemonMaxJobs config) 100
      logInfo msg = logToFile (daemonLogFile config) ("[INFO] " ++ msg)
      logWarning msg = logToFile (daemonLogFile config) ("[WARNING] " ++ msg)


-- | Install signal handlers
installSignalHandlers :: DaemonConfig -> IO ()
installSignalHandlers config = do
    let logH = fromMaybe stderr -- Log signal handling to stderr if no log file
    installHandler PosixSignals.sigTERM (PosixSignals.Catch $ cleanupAndExit config logH) Nothing
    installHandler PosixSignals.sigINT (PosixSignals.Catch $ cleanupAndExit config logH) Nothing
    installHandler PosixSignals.sigHUP (PosixSignals.Catch $ reloadConfig config logH) Nothing
    installHandler PosixSignals.sigUSR1 (PosixSignals.Catch $ dumpState config logH) Nothing
    installHandler PosixSignals.sigUSR2 (PosixSignals.Catch $ triggerGC config logH) Nothing
    return ()

-- | Clean up and exit
cleanupAndExit :: DaemonConfig -> Handle -> IO ()
cleanupAndExit config logH = do
    hPutStrLn logH "[INFO] Received termination signal, shutting down..."
    cleanupAtExit config logH -- Pass log handle for cleanup logging
    -- Don't exit here, let the main thread finish or be killed gracefully
    -- exitImmediately (ExitSuccess) -- Avoid immediate exit

-- | Reload configuration (placeholder)
reloadConfig :: DaemonConfig -> Handle -> IO ()
reloadConfig config logH = hPutStrLn logH "[INFO] Received SIGHUP, configuration reload requested (not implemented)."

-- | Dump state (placeholder)
dumpState :: DaemonConfig -> Handle -> IO ()
dumpState config logH = hPutStrLn logH "[INFO] Received SIGUSR1, state dump requested (not implemented)."

-- | Trigger garbage collection (placeholder)
triggerGC :: DaemonConfig -> Handle -> IO ()
triggerGC config logH = hPutStrLn logH "[INFO] Received SIGUSR2, garbage collection trigger requested (not implemented)."

-- | Clean up resources at exit
cleanupAtExit :: DaemonConfig -> Handle -> IO ()
cleanupAtExit config logH = do
    hPutStrLn logH "[INFO] Cleaning up resources..."
    removeFileSafe (daemonSocketPath config)
    removeFileSafe (daemonStorePath config </> "var/ten/daemon.lock")
    removeFileSafe (daemonStorePath config </> "var/ten/gc.lock")
    -- Persist state (needs access to the running state, difficult from signal handler)
    -- This should ideally happen in the main loop's finally block
    -- catch (saveCurrentState config) (\(_ :: SomeException) -> hPutStrLn logH "[ERROR] Failed to save state on exit.")
    hPutStrLn logH "[INFO] Cleanup finished."

-- | Save current daemon state (placeholder)
saveCurrentState :: DaemonConfig -> IO ()
saveCurrentState config = logToFile (daemonLogFile config) "[INFO] Saving daemon state (placeholder)."


-- | Drop privileges if needed
dropPrivilegesIfNeeded :: DaemonConfig -> IO ()
dropPrivilegesIfNeeded config = do
    uid <- getRealUserID
    when (uid == 0) $ case (daemonUser config, daemonGroup config) of
        (Just user, Just group) -> dropPrivs uid (T.unpack user) (Just $ T.unpack group)
        (Just user, Nothing)    -> dropPrivs uid (T.unpack user) Nothing
        _                       -> return () -- No user specified, keep root
  where
    dropPrivs :: UserID -> String -> Maybe String -> IO ()
    dropPrivs currentUid user mGroup = do
        targetUserEntry <- getUserEntryForName user `catch` userNotFound user
        let targetUid = userID targetUserEntry
        targetGid <- case mGroup of
            Just groupName -> groupID <$> (getGroupEntryForName groupName `catch` groupNotFound groupName)
            Nothing        -> return $ userGroupID targetUserEntry -- Use user's primary group

        -- Ensure store ownership *before* dropping privileges
        ensureStoreOwnership (daemonStorePath config) targetUid targetGid

        -- Set group first
        setGroupID targetGid `catch` setGidFailed targetGid

        -- Set user
        setUserID targetUid `catch` setUidFailed targetUid

        -- Verify
        newUid <- getEffectiveUserID
        when (newUid == 0) $ criticalError "Failed to drop root privileges!"
        logToFile (daemonLogFile config) $ "[INFO] Dropped privileges to user=" ++ user ++ ", group=" ++ show targetGid

    userNotFound name e = criticalError $ "User '" ++ name ++ "' not found: " ++ show (e :: SomeException)
    groupNotFound name e = criticalError $ "Group '" ++ name ++ "' not found: " ++ show (e :: SomeException)
    setGidFailed gid e = criticalError $ "Failed to set group ID to " ++ show gid ++ ": " ++ show (e :: SomeException)
    setUidFailed uid e = criticalError $ "Failed to set user ID to " ++ show uid ++ ": " ++ show (e :: SomeException)
    criticalError msg = logToFile (daemonLogFile config) ("[CRITICAL] " ++ msg) >> exitFailure


-- | Ensure store has correct ownership
ensureStoreOwnership :: FilePath -> UserID -> GroupID -> IO ()
ensureStoreOwnership storePath uid gid = do
    logToFile Nothing $ "[INFO] Ensuring store ownership for UID=" ++ show uid ++ ", GID=" ++ show gid ++ " at " ++ storePath
    -- Recursively set ownership on essential directories
    forM_ [storePath, storePath </> "var", storePath </> "tmp"] $ \p ->
        setOwnershipRecursive p uid gid

-- | Set ownership recursively
setOwnershipRecursive :: FilePath -> UserID -> GroupID -> IO ()
setOwnershipRecursive path uid gid = do
    exists <- doesPathExist path
    when exists $ do
        catch (System.Posix.Files.setOwnerAndGroup path uid gid) $ \(e :: SomeException) ->
             logToFile Nothing $ "[WARNING] Failed set owner/group on " ++ path ++ ": " ++ show e
        isDir <- System.Directory.doesDirectoryExist path
        when isDir $
            listDirectory path >>= mapM_ (\entry -> setOwnershipRecursive (path </> entry) uid gid)


-- | Log message to file or stderr
logToFile :: Maybe FilePath -> String -> IO ()
logToFile Nothing msg = hPutStrLn stderr msg
logToFile (Just path) msg = appendFile path (msg ++ "\n") `catch` \(e::IOException) ->
    hPutStrLn stderr ("Failed to write to log file " ++ path ++ ": " ++ show e)

-- Placeholder Stubs for Missing Implementations

getDefaultConfig :: IO DaemonConfig
getDefaultConfig = return defaultDaemonConfig -- Use the one from Ten.Core if exported

loadConfigFromFile :: FilePath -> IO (Either String DaemonConfig)
loadConfigFromFile _ = return $ Right defaultDaemonConfig -- Stub

loadConfigFromEnv :: IO DaemonConfig
loadConfigFromEnv = return defaultDaemonConfig -- Stub

stopDaemon :: FilePath -> IO (Either Text ())
stopDaemon _ = return $ Right () -- Stub

initSecureSocket :: FilePath -> IO Net.Socket
initSecureSocket path = createServerSocket path -- Use the function defined below

createServerSocket :: FilePath -> IO Net.Socket
createServerSocket path = do
    exists <- doesFileExist path
    when exists $ removeFile path
    createDirectoryIfMissing True (takeDirectory path)
    sock <- Net.socket Net.AF_UNIX Net.Stream 0
    Net.setSocketOption sock Net.ReuseAddr 1
    Net.bind sock (Net.SockAddrUnix path)
    Net.listen sock 10
    setFileMode path 0o666 -- Or 0o777 depending on desired access
    return sock

initializeProtocolHandlers :: IO () -- Placeholder type
initializeProtocolHandlers = return () -- Stub

-- Helper functions potentially needed from Ten.Core or defined locally
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    f <- doesFileExist path
    d <- System.Directory.doesDirectoryExist path
    return $ f || d

-- Assume LogLevel was defined somewhere, if not:
-- data LogLevel = LogQuiet | LogNormal | LogVerbose | LogDebug deriving (Show, Eq, Ord)

-- Assume DaemonConfig has necessary fields (adjust based on Ten.Core)
-- defaultDaemonConfig = DaemonConfig { ... } -- Define this based on Ten.Core

-- Assume DaemonState constructor exists (adjust based on Ten.Daemon.State)
-- initializeDaemonState = ... -- Define this based on Ten.Daemon.State
-- persistStateToFile = ...
-- recoverStateFromFile = ...

-- Helper to run TenM actions for store init (needs proper env/state)
runInitAction :: TenM 'Eval 'Daemon a -> IO (Either BuildError a)
runInitAction action = do
    -- Create minimal environment/state needed for the action
    -- This is tricky outside the main server context
    let env = initDaemonEnv "/tmp/ten-init-work" "/tmp/ten-init-store" Nothing -- Example paths
    state <- DaemonState.initDaemonState sDaemon "/tmp/ten-init-state" 1 100 -- Example state
    initialState <- initBuildState Eval (BuildIdFromInt 0) -- Example BuildState
    runTen sEval sDaemon action env initialState >>= \case
         Left err -> return $ Left err
         Right (res, _) -> return $ Right res

-- Adapt store initialization calls
initializeDatabase :: FilePath -> Int -> IO (DBCore.Database 'Daemon)
initializeDatabase path timeout = DBCore.initDatabaseDaemon sDaemon path timeout -- Use the DB core function

ensureDBDirectories :: FilePath -> IO ()
ensureDBDirectories storeDir = runInitAction (DBCore.ensureDBDirectories storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed ensureDBDirectories: " ++ show e

initializeStore :: FilePath -> IO ()
initializeStore storeDir = runInitAction (Store.initializeStore storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed initializeStore: " ++ show e

createStoreDirectories :: FilePath -> IO ()
createStoreDirectories storeDir = runInitAction (Store.createStoreDirectories storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed createStoreDirectories: " ++ show e

verifyStore :: FilePath -> IO ()
verifyStore storeDir = runInitAction (Store.verifyStore storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed verifyStore: " ++ show e

-- We need the `isDaemonRunning` function
isDaemonRunning :: FilePath -> IO Bool
isDaemonRunning socketPath = do
    exists <- doesFileExist socketPath
    if not exists then return False else do
        result <- try $ bracket (Net.socket Net.AF_UNIX Net.Stream 0) Net.close $ \sock ->
             Net.connect sock (Net.SockAddrUnix socketPath)
        case result of
            Left (_ :: IOException) -> return False
            Right ()                -> return True
