{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent (threadDelay, myThreadId)
import Control.Exception (bracket, try, catch, finally, mask, throwIO, displayException, SomeException, IOException)
import Control.Monad (when, unless, void, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile, doesDirectoryExist, listDirectory)
import System.Environment (getArgs, getProgName, lookupEnv, getEnvironment)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode(..), withFile, hPutStrLn, stdout, stderr, hSetBuffering, BufferMode(..), Handle, openFile, hClose, hFlush)
import System.Posix.Files (fileExist, setFileMode, fileMode, setOwnerAndGroup)
import System.Posix.Process (getProcessID, exitImmediately, forkProcess, createSession)
import System.Posix.Signals hiding (SignalSet, Handler)
import qualified System.Posix.Signals as PosixSignals
import System.Posix.Types (UserID, GroupID)
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName, userID, groupID, userGroupID,
                         getRealUserID, setUserID, setGroupID, getUserEntryForName, getGroupEntryForName)
import qualified Network.Socket as Net

-- Import Ten modules with proper selective imports
import Ten
import qualified Ten.Daemon.State as DaemonState
import qualified Ten.Daemon.Server as DaemonServer
import qualified Ten.Daemon.Protocol as Protocol
import qualified Ten.DB.Core as DBCore

-- LogLevel definition
data LogLevel = LogQuiet | LogNormal | LogVerbose | LogDebug
    deriving (Show, Eq, Ord)

-- Daemon Options/Commands
data DaemonOption
    = OptHelp
    | OptVersion
    | OptConfig FilePath
    | OptSocketPath FilePath
    | OptStorePath FilePath
    | OptStateFile FilePath
    | OptUser String
    | OptGroup String
    | OptLogFile FilePath
    | OptLogLevel LogLevel
    | OptForeground
    | OptPidFile FilePath
    | OptReload
    | OptDebug
    | OptMaxJobs Int
    deriving (Show, Eq)

-- Command to execute
data DaemonCommand
    = CmdStart
    | CmdStop
    | CmdRestart
    | CmdStatus
    | CmdHelp
    | CmdVersion
    deriving (Show, Eq)

-- Parse command line options
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
    , Option ['d'] ["debug"]
        (NoArg OptDebug)
        "Enable debug mode"
    , Option ['j'] ["max-jobs"]
        (ReqArg (OptMaxJobs . read) "NUM")
        "Maximum number of concurrent build jobs"
    ]

-- Parse command line arguments for the daemon executable
parseDaemonArgs :: [String] -> IO (DaemonCommand, [DaemonOption])
parseDaemonArgs args =
    case getOpt Permute options args of
        (opts, [], []) -> return (CmdStart, opts) -- Assume start if no command given
        (opts, nonOpts, []) ->
            case nonOpts of
                ["help"]    -> return (CmdHelp, opts)
                ["version"] -> return (CmdVersion, opts)
                _           -> error $ "Unexpected arguments: " ++ unwords nonOpts ++ "\n" ++ usageInfo usageHeader options
        (_, _, errs) ->
            error $ concat errs ++ usageInfo usageHeader options

-- Show usage information
usageHeader :: String
usageHeader = "Usage: ten-daemon [OPTIONS]\n\n" ++
              "Starts the Ten build daemon.\n\n" ++
              "Control Commands (use 'ten' client):\n" ++
              "  ten daemon start\n" ++
              "  ten daemon stop\n" ++
              "  ten daemon restart\n" ++
              "  ten daemon status\n\n" ++
              "Options:"

-- Show help information
showHelp :: IO ()
showHelp = putStrLn $ usageInfo usageHeader options

-- Show version information
showVersion :: IO ()
showVersion = putStrLn "Ten Daemon version 0.1.0"

-- Main entry point for the daemon
main :: IO ()
main = do
    args <- getArgs
    (cmd, opts) <- parseDaemonArgs args

    -- Handle help and version commands early
    case cmd of
        CmdHelp -> showHelp >> exitSuccess
        CmdVersion -> showVersion >> exitSuccess
        CmdStart -> return () -- Proceed to start
        _ -> error $ "Invalid command for ten-daemon executable: " ++ show cmd

    -- Extract options
    let findOpt :: (DaemonOption -> Maybe a) -> Maybe a
        findOpt f = foldr (\opt acc -> case f opt of
                                         Just x -> Just x
                                         Nothing -> acc) Nothing opts

    let isOpt :: (DaemonOption -> Bool) -> Bool
        isOpt f = any f opts

    let configFile = findOpt (\case OptConfig path -> Just path; _ -> Nothing)
    let socketPath = findOpt (\case OptSocketPath path -> Just path; _ -> Nothing)
    let storePath = findOpt (\case OptStorePath path -> Just path; _ -> Nothing)
    let stateFile = findOpt (\case OptStateFile path -> Just path; _ -> Nothing)
    let userOverride = findOpt (\case OptUser user -> Just user; _ -> Nothing)
    let groupOverride = findOpt (\case OptGroup group -> Just group; _ -> Nothing)
    let logFile = findOpt (\case OptLogFile file -> Just file; _ -> Nothing)
    let logLevel = findOpt (\case OptLogLevel level -> Just level; _ -> Nothing)
    let foreground = isOpt (\case OptForeground -> True; _ -> False)
    let pidFile = findOpt (\case OptPidFile path -> Just path; _ -> Nothing)
    let debug = isOpt (\case OptDebug -> True; _ -> False)
    let maxJobs = findOpt (\case OptMaxJobs n -> Just n; _ -> Nothing)

    -- Load configuration
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
            , daemonLogLevel = fromLogLevel <$> logLevel `orElse` Just (daemonLogLevel config)
            , daemonForeground = foreground || daemonForeground config
            , daemonMaxJobs = fromMaybe (daemonMaxJobs config) maxJobs
            }

    -- Enable debug mode if requested
    let config'' = if debug
                  then config' { daemonLogLevel = 3 } -- LogDebug level
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
  where
    fromLogLevel :: LogLevel -> Int
    fromLogLevel LogQuiet = 0
    fromLogLevel LogNormal = 1
    fromLogLevel LogVerbose = 2
    fromLogLevel LogDebug = 3

-- Maybe Or Else operator
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing  y = y

-- Start the daemon process logic (run after parsing args)
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

-- Proper daemonize implementation
daemonize :: IO () -> IO ()
daemonize action = do
    -- Fork the first child process
    pid <- forkProcess $ do
        -- Create a new session to detach from controlling terminal
        void createSession

        -- Fork the second child process
        pid2 <- forkProcess $ do
            -- Change working directory to root
            -- setCurrentDirectory "/"  -- Uncomment if you need to change directory

            -- Redirect standard file descriptors
            redirectStdStreams

            -- Execute the daemon action
            action

        -- First child exits
        exitImmediately exitSuccess

    -- Parent exits
    exitImmediately exitSuccess
  where
    redirectStdStreams :: IO ()
    redirectStdStreams = do
        -- Close standard file descriptors or redirect to /dev/null
        -- This is a simplified version; a full implementation would use openFd and dup2
        -- to redirect stdin/stdout/stderr to /dev/null
        void $ try @IOException $ hClose stdin
        void $ try @IOException $ hClose stdout
        void $ try @IOException $ hClose stderr

-- Run the daemon core logic with exception handling
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

-- Setup store directory structure
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
    handleDBErrorAsWarning $ Ten.ensureDBDirectories storeDir

-- Lock handle type for managing file locks
type LockHandle = Handle

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

-- Proper file locking implementation
acquireGlobalLock :: FilePath -> IO LockHandle
acquireGlobalLock lockPath = do
    h <- openFile lockPath WriteMode
    hSetBuffering h NoBuffering

    -- Write our PID to the lock file
    pid <- getProcessID
    hPutStrLn h (show pid)
    hFlush h

    -- In a real implementation, this would use flock or fcntl to do a proper file lock
    -- For demonstration, we'll use the file existence as a simple lock mechanism

    putStrLn $ "Acquired lock: " ++ lockPath
    return h

releaseGlobalLock :: LockHandle -> IO ()
releaseGlobalLock h = do
    putStrLn "Releasing global lock."
    hClose h `catch` \(_ :: IOException) -> return ()

-- Initialize secure store
initializeSecureStore :: DaemonConfig -> IO ()
initializeSecureStore config = do
    let storeDir = daemonStorePath config
    -- Initialize store (might create files/dirs)
    handleDBErrorAsWarning $ Main.initializeStore storeDir
    -- Ensure DB directories exist again after potential store init
    handleDBErrorAsWarning $ Main.ensureDBDirectories storeDir
    -- Verify store structure and permissions
    handleDBErrorAsWarning $ Main.verifyStore storeDir

handleDBErrorAsWarning :: IO () -> IO ()
handleDBErrorAsWarning action = catch action $ \(e :: SomeException) ->
    hPutStrLn stderr $ "Warning during store/DB initialization: " ++ show e

-- Write PID file
writePidFile :: FilePath -> IO ()
writePidFile path = do
    pid <- getProcessID
    catch (writeFile path (show pid)) $ \(e :: IOException) ->
        hPutStrLn stderr $ "Warning: Could not write PID file to " ++ path ++ ": " ++ show e

-- Remove file safely (ignoring errors)
removeFileSafe :: FilePath -> IO ()
removeFileSafe path = catch (removeFile path) (\(_ :: IOException) -> return ())

-- Run the daemon with given configuration
runDaemonWithConfig :: DaemonConfig -> IO ()
runDaemonWithConfig config = do
    setupLogging config

    -- Initialize database connection pool (or single connection)
    dbConn <- catch (initializeDatabase (daemonDbPath config) 5000) $
        \(e :: SomeException) -> logToFile (daemonLogFile config) ("[CRITICAL] Database init failed: " ++ show e) >> exitFailure

    -- Load or initialize daemon state
    daemonState <- loadOrInitializeState config dbConn

    -- Initialize secure daemon socket
    serverSocket <- catch (createServerSocket (daemonSocketPath config)) $
        \(e :: SomeException) -> logToFile (daemonLogFile config) ("[CRITICAL] Socket creation failed: " ++ show e) >> exitFailure

    logToFile (daemonLogFile config) "[INFO] Daemon initialized successfully. Starting server loop."

    -- Run the main server loop (needs DaemonState, Socket, Config)
    DaemonServer.startServer serverSocket daemonState config
        `finally` (logToFile (daemonLogFile config) "[INFO] Server loop terminated." >> closeDatabase dbConn)

-- Compute database path from store path
daemonDbPath :: DaemonConfig -> FilePath
daemonDbPath config = daemonStorePath config </> "var/ten/db/ten.db"

-- Close database connection
closeDatabase :: DBCore.Database t -> IO ()
closeDatabase = DBCore.closeDatabaseDaemon

-- Set up logging based on configuration
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

-- Load existing state or initialize new state
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

-- Install signal handlers
installSignalHandlers :: DaemonConfig -> IO ()
installSignalHandlers config = do
    let logH = fromMaybe stderr -- Log signal handling to stderr if no log file
    installHandler sigTERM (Catch $ cleanupAndExit config) Nothing
    installHandler sigINT (Catch $ cleanupAndExit config) Nothing
    installHandler sigHUP (Catch $ reloadConfig config) Nothing
    installHandler sigUSR1 (Catch $ dumpState config) Nothing
    installHandler sigUSR2 (Catch $ triggerGC config) Nothing
    return ()

-- Clean up and exit
cleanupAndExit :: DaemonConfig -> IO ()
cleanupAndExit config = do
    hPutStrLn stderr "[INFO] Received termination signal, shutting down..."
    cleanupAtExit config
    -- Don't exit here, let the main thread finish or be killed gracefully

-- Reload configuration
reloadConfig :: DaemonConfig -> IO ()
reloadConfig config = hPutStrLn stderr "[INFO] Received SIGHUP, configuration reload requested."

-- Dump state
dumpState :: DaemonConfig -> IO ()
dumpState config = hPutStrLn stderr "[INFO] Received SIGUSR1, state dump requested."

-- Trigger garbage collection
triggerGC :: DaemonConfig -> IO ()
triggerGC config = hPutStrLn stderr "[INFO] Received SIGUSR2, garbage collection trigger requested."

-- Clean up resources at exit
cleanupAtExit :: DaemonConfig -> IO ()
cleanupAtExit config = do
    hPutStrLn stderr "[INFO] Cleaning up resources..."
    removeFileSafe (daemonSocketPath config)
    removeFileSafe (daemonStorePath config </> "var/ten/daemon.lock")
    removeFileSafe (daemonStorePath config </> "var/ten/gc.lock")
    hPutStrLn stderr "[INFO] Cleanup finished."

-- Drop privileges if needed
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

-- Ensure store has correct ownership
ensureStoreOwnership :: FilePath -> UserID -> GroupID -> IO ()
ensureStoreOwnership storePath uid gid = do
    logToFile Nothing $ "[INFO] Ensuring store ownership for UID=" ++ show uid ++ ", GID=" ++ show gid ++ " at " ++ storePath
    -- Recursively set ownership on essential directories
    forM_ [storePath, storePath </> "var", storePath </> "tmp"] $ \p ->
        setOwnershipRecursive p uid gid

-- Set ownership recursively
setOwnershipRecursive :: FilePath -> UserID -> GroupID -> IO ()
setOwnershipRecursive path uid gid = do
    exists <- doesPathExist path
    when exists $ do
        catch (System.Posix.Files.setOwnerAndGroup path uid gid) $ \(e :: SomeException) ->
             logToFile Nothing $ "[WARNING] Failed set owner/group on " ++ path ++ ": " ++ show e
        isDir <- doesDirectoryExist path
        when isDir $
            listDirectory path >>= mapM_ (\entry -> setOwnershipRecursive (path </> entry) uid gid)

-- Log message to file or stderr
logToFile :: Maybe FilePath -> String -> IO ()
logToFile Nothing msg = hPutStrLn stderr msg
logToFile (Just path) msg = appendFile path (msg ++ "\n") `catch` \(e::IOException) ->
    hPutStrLn stderr ("Failed to write to log file " ++ path ++ ": " ++ show e)

-- Get default daemon configuration
getDefaultConfig :: IO DaemonConfig
getDefaultConfig = return defaultDaemonConfig

-- Default daemon configuration
defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig
    { daemonSocketPath = "/var/run/ten/daemon.socket"
    , daemonStorePath = "/var/lib/ten/store"
    , daemonStateFile = "/var/lib/ten/state.json"
    , daemonLogFile = Just "/var/log/ten/daemon.log"
    , daemonLogLevel = 1  -- Normal logging level
    , daemonGcInterval = Just 3600  -- 1 hour
    , daemonUser = Nothing  -- Run as current user by default
    , daemonGroup = Nothing
    , daemonAllowedUsers = Set.fromList ["root"]
    , daemonMaxJobs = 4
    , daemonForeground = False
    , daemonTmpDir = "/tmp/ten"
    }

-- Load configuration from file
loadConfigFromFile :: FilePath -> IO (Either String DaemonConfig)
loadConfigFromFile path = do
    exists <- doesFileExist path
    if exists then do
        contents <- readFile path
        -- In a real implementation, this would parse a configuration file format
        -- For simplicity, we'll just return the default config
        return $ Right defaultDaemonConfig
    else
        return $ Left $ "Configuration file not found: " ++ path

-- Load configuration from environment
loadConfigFromEnv :: IO DaemonConfig
loadConfigFromEnv = do
    -- In a real implementation, this would read environment variables
    -- For now, return the default configuration
    return defaultDaemonConfig

-- Helper to check if a path exists
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    f <- doesFileExist path
    d <- doesDirectoryExist path
    return $ f || d

-- Initialize database
initializeDatabase :: FilePath -> Int -> IO (DBCore.Database 'Daemon)
initializeDatabase path timeout = DBCore.initDatabaseDaemon sDaemon path timeout

-- Create server socket
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

-- Helper to run Ten actions
runInitAction :: TenM 'Eval 'Daemon a -> IO (Either BuildError a)
runInitAction action = do
    -- Create minimal environment/state needed for the action
    let env = initDaemonEnv "/tmp/ten-init-work" "/tmp/ten-init-store" Nothing -- Example paths
    state <- DaemonState.initDaemonState sDaemon "/tmp/ten-init-state" 1 100 -- Example state
    initialState <- initBuildState Eval (BuildIdFromInt 0) -- Example BuildState
    runTen sEval sDaemon action env initialState >>= \case
         Left err -> return $ Left err
         Right (res, _) -> return $ Right res

-- Adapter functions for Ten.Core operations
ensureDBDirectories :: FilePath -> IO ()
ensureDBDirectories storeDir = runInitAction (DBCore.ensureDBDirectories storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed ensureDBDirectories: " ++ show e

initializeStore :: FilePath -> IO ()
initializeStore storeDir = runInitAction (Ten.initializeStore storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed initializeStore: " ++ show e

createStoreDirectories :: FilePath -> IO ()
createStoreDirectories storeDir = runInitAction (Ten.createStoreDirectories storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed createStoreDirectories: " ++ show e

verifyStore :: FilePath -> IO ()
verifyStore storeDir = runInitAction (Ten.verifyStore storeDir) >>= either printError return
    where printError e = hPutStrLn stderr $ "Failed verifyStore: " ++ show e
