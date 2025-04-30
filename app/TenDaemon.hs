{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-} -- Needed for privilege constraints

module Main where

import Control.Concurrent (threadDelay, myThreadId)
import Control.Exception (bracket, try, catch, finally, mask, throwIO, displayException, SomeException, IOException)
import Control.Monad (when, unless, void, forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import System.Console.GetOpt
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile, doesDirectoryExist, listDirectory, canonicalizePath)
import System.Environment (getArgs, getProgName, lookupEnv, getEnvironment)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath (takeDirectory, (</>), normalise)
import System.IO (IOMode(..), withFile, hPutStrLn, stdout, stderr, hSetBuffering, BufferMode(..), Handle, openFile, hClose, hFlush, stdin)
import System.Posix.Files (fileExist, setFileMode, fileMode, setOwnerAndGroup, getFileStatus, isRegularFile)
import System.Posix.IO (stdInput, stdOutput, stdError, closeFd, createFile, openFd, dupTo, OpenMode(..), defaultFileFlags)
import System.Posix.Process (getProcessID, exitImmediately, forkProcess, createSession, getProcessStatus, ProcessStatus(..))
import System.Posix.Signals hiding (SignalSet, Handler)
import qualified System.Posix.Signals as PosixSignals
import System.Posix.Types (UserID, GroupID, ProcessID)
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName, userID, groupID, userGroupID,
                         getRealUserID, setUserID, setGroupID, getUserEntryForName, getGroupEntryForName)
import Network.Socket (Socket)
import qualified Network.Socket as Net
import qualified Data.Aeson as Aeson -- For config loading (if implemented)
import Control.Concurrent.STM (newTVarIO)
import Control.Concurrent.Async (wait) -- Import wait

import Ten -- Import the main facade

-- LogLevel definition (redefined locally as it's specific to daemon CLI)
data LogLevel = LogQuiet | LogNormal | LogVerbose | LogDebug
    deriving (Show, Eq, Ord)

-- Daemon Options/Commands (redefined locally for daemon CLI)
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
    | OptMaxJobs Int
    deriving (Show, Eq)

-- Command to execute (redefined locally for daemon CLI)
data DaemonCommand
    = CmdStart
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
        "Path to log file (use 'stdout' or 'stderr')"
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
            case listToMaybe nonOpts of
                Just cmd | cmd `elem` ["start"] -> return (CmdStart, opts)
                         | cmd `elem` ["help", "--help"] -> return (CmdHelp, opts)
                         | cmd `elem` ["version", "--version"] -> return (CmdVersion, opts)
                _ -> error $ "Unexpected arguments: " ++ unwords nonOpts ++ "\n" ++ usageInfo usageHeader options
        (_, _, errs) ->
            error $ concat errs ++ usageInfo usageHeader options

-- Show usage information
usageHeader :: String
usageHeader = "Usage: ten-daemon [OPTIONS] [COMMAND]\n\n" ++
              "Starts the Ten build daemon.\n\n" ++
              "Commands:\n" ++
              "  start           Start the daemon (default)\n" ++
              "  help            Show this help message\n" ++
              "  version         Show version information\n\n" ++
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

    -- Extract options
    let findOpt :: (DaemonOption -> Maybe a) -> Maybe a
        findOpt f = listToMaybe $ mapMaybe f opts

    let isOpt :: (DaemonOption -> Bool) -> Bool
        isOpt f = any f opts

    let configFile = findOpt (\case OptConfig path -> Just path; _ -> Nothing)
    let socketPathOpt = findOpt (\case OptSocketPath path -> Just path; _ -> Nothing)
    let storePathOpt = findOpt (\case OptStorePath path -> Just path; _ -> Nothing)
    let stateFileOpt = findOpt (\case OptStateFile path -> Just path; _ -> Nothing)
    let userOverride = findOpt (\case OptUser user -> Just user; _ -> Nothing)
    let groupOverride = findOpt (\case OptGroup group -> Just group; _ -> Nothing)
    let logFileOpt = findOpt (\case OptLogFile file -> Just file; _ -> Nothing)
    let logLevelOpt = findOpt (\case OptLogLevel level -> Just level; _ -> Nothing)
    let foreground = isOpt (\case OptForeground -> True; _ -> False)
    let pidFileOpt = findOpt (\case OptPidFile path -> Just path; _ -> Nothing)
    let maxJobsOpt = findOpt (\case OptMaxJobs n -> Just n; _ -> Nothing)

    -- Load configuration (using default or from file/env - simplified)
    config <- loadDaemonConfig configFile

    -- Apply command line overrides to config
    let config' = config
            { daemonSocketPath = fromMaybe (daemonSocketPath config) socketPathOpt
            , daemonStorePath = fromMaybe (daemonStorePath config) storePathOpt
            , daemonStateFile = fromMaybe (daemonStateFile config) stateFileOpt
            , daemonUser = T.pack <$> (userOverride `orElse` (T.unpack <$> daemonUser config))
            , daemonGroup = T.pack <$> (groupOverride `orElse` (T.unpack <$> daemonGroup config))
            , daemonLogFile = logFileOpt `orElse` daemonLogFile config
            , daemonLogLevel = fromLogLevel (fromMaybe (toLogLevel $ daemonLogLevel config) logLevelOpt)
            , daemonForeground = foreground || daemonForeground config
            , daemonMaxJobs = fromMaybe (daemonMaxJobs config) maxJobsOpt
            }

    -- Create necessary directories early (permissions adjusted later)
    createDirectoryIfMissing True (takeDirectory (daemonSocketPath config'))
    createDirectoryIfMissing True (daemonStorePath config')
    createDirectoryIfMissing True (takeDirectory (daemonStateFile config'))
    case daemonLogFile config' of
        Just logPath | logPath /= "stdout" && logPath /= "stderr" ->
            createDirectoryIfMissing True (takeDirectory logPath)
        _ -> return ()

    -- Start the daemon process logic
    startDaemonProcess config' pidFileOpt
  where
    fromLogLevel :: LogLevel -> Int
    fromLogLevel LogQuiet = 0
    fromLogLevel LogNormal = 1
    fromLogLevel LogVerbose = 2
    fromLogLevel LogDebug = 3

    toLogLevel :: Int -> LogLevel
    toLogLevel 0 = LogQuiet
    toLogLevel 1 = LogNormal
    toLogLevel 2 = LogVerbose
    toLogLevel _ = LogDebug

-- Maybe Or Else operator
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing  y = y

-- Start the daemon process logic (run after parsing args)
startDaemonProcess :: DaemonConfig -> Maybe FilePath -> IO ()
startDaemonProcess config pidFilePath = do
    -- Check if the daemon is already running using the configured socket path
    -- Note: isDaemonRunning is a client function, we check differently inside daemon
    lockPath <- canonicalizePath $ daemonStorePath config </> "var/ten/daemon.lock"
    lockHandleResult <- try @IOException $ tryAcquireGlobalLock lockPath
    case lockHandleResult of
        Left e -> do -- Error acquiring lock (permissions?)
            hPutStrLn stderr $ "Error: Could not acquire global daemon lock at " ++ lockPath
            hPutStrLn stderr $ "Ensure no other daemon instance is running and you have permissions."
            hPutStrLn stderr $ "Details: " ++ show e
            exitFailure
        Right Nothing -> do -- Lock exists and is held
            hPutStrLn stderr $ "Error: Daemon appears to be already running (lock file exists: " ++ lockPath ++ ")"
            exitFailure
        Right (Just lockHandle) -> do -- Lock acquired successfully
            -- Fork and daemonize if not running in foreground
            if daemonForeground config
                then do
                    pid <- getProcessID
                    putStrLn $ "Starting Ten daemon in foreground (PID: " ++ show pid ++ ", lock: " ++ lockPath ++ ")"
                    -- Write PID file even in foreground mode if requested
                    forM_ pidFilePath writePidFile
                    runDaemonSafely config lockHandle lockPath
                        `finally` (releaseGlobalLock lockHandle lockPath >> forM_ pidFilePath removeFileSafe)
                else do
                    putStrLn $ "Starting Ten daemon in background (socket: " ++ daemonSocketPath config ++ ")"
                    let pidFile = fromMaybe (takeDirectory lockPath </> "ten-daemon.pid") pidFilePath
                    createDirectoryIfMissing True (takeDirectory pidFile) -- Ensure PID directory exists

                    -- Daemonize the process
                    daemonize $ do
                        -- Now running in the background process
                        -- Write PID file *after* daemonizing
                        writePidFile pidFile

                        -- Run the main daemon logic, ensuring cleanup
                        runDaemonSafely config lockHandle lockPath
                            `finally` (releaseGlobalLock lockHandle lockPath >> removeFileSafe pidFile)

-- Proper daemonize implementation
daemonize :: IO () -> IO ()
daemonize action = do
    -- Fork the first child process
    pid1 <- forkProcess $ do
        -- Create a new session to detach from controlling terminal
        void createSession

        -- Fork the second child process
        pid2 <- forkProcess $ do
            -- Change working directory to root to avoid holding mount points
            -- setCurrentDirectory "/" -- Consider uncommenting if necessary

            -- Redirect standard file descriptors
            redirectStdStreams

            -- Execute the daemon action
            action `catch` \(e :: SomeException) -> do
                -- Log crash to stderr or a fallback log
                hPutStrLn stderr $ "[CRITICAL] Daemon crashed: " ++ displayException e
                exitFailure

        -- First child exits immediately after second fork
        exitImmediately ExitSuccess

    -- Wait for the first child to exit (ensures session leader terminates)
    status <- getProcessStatus True False pid1
    case status of
      Just (Exited ExitSuccess) -> return ()
      _ -> hPutStrLn stderr "Warning: First daemon fork did not exit cleanly."

    -- Original parent exits
    exitImmediately ExitSuccess
  where
    redirectStdStreams :: IO ()
    redirectStdStreams = do
        nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
        void $ dupTo nullFd stdInput
        void $ dupTo nullFd stdOutput
        void $ dupTo nullFd stdError
        closeFd nullFd

-- Run the daemon core logic with exception handling
runDaemonSafely :: DaemonConfig -> Handle -> FilePath -> IO ()
runDaemonSafely config lockHandle lockPath = do
    -- Set up signal handlers within the daemon process
    -- Setup requires ServerControl, which isn't available yet.
    -- Signals handled later inside startServer or via simpler handlers here.
    installSimpleSignalHandlers config lockPath lockHandle

    -- Initialize store structure (permissions might be adjusted later)
    liftIO $ setupStoreStructure config

    -- Initialize database connection pool (Daemon privilege required)
    db <- catch (initDatabaseDaemon sDaemon (daemonDbPath config) 5000) $
        \(e :: SomeException) -> logToFile config ("[CRITICAL] Database init failed: " ++ show e) >> exitFailure

    -- Load or initialize authentication database
    authDbPath <- getAuthDbPath config
    authDb <- loadAuthFile authDbPath
    authDbVar <- newTVarIO authDb

    -- Load or initialize daemon state
    daemonState <- loadStateFromFile sDaemon (daemonStateFile config) (daemonMaxJobs config) 1000 -- Use config values

    -- Drop privileges *after* initial setup requiring root
    dropPrivilegesIfNeeded config

    -- Initialize secure daemon socket (after dropping privs if possible, or before if binding restricted port)
    serverSocket <- catch (createServerSocket (daemonSocketPath config)) $
        \(e :: SomeException) -> logToFile config ("[CRITICAL] Socket creation failed: " ++ show e) >> exitFailure

    -- Run the main server loop
    serverControl <- startServer serverSocket daemonState config authDbVar -- startServer handles the accept loop
    logToFile config "[INFO] Ten daemon server started successfully."

    -- Wait for server thread to finish (e.g., on shutdown signal)
    -- The accept loop runs in startServer's thread. We might need to wait on it or a shutdown signal.
    -- The simple signal handler now calls stopServer. We just need to keep the main thread alive.
    waitForShutdown serverControl -- This function needs implementation, e.g., wait on scShutdown TVar

  `finally` (do
    logToFile config "[INFO] Daemon shutting down..."
    -- Cleanup happens in stopServer called by signal handler or elsewhere
    -- Ensure DB is closed if not handled by stopServer
    -- closeDatabaseDaemon db -- Assuming stopServer handles this
    logToFile config "[INFO] Daemon shutdown complete."
    )

-- Wait for the shutdown signal
waitForShutdown :: ServerControl -> IO ()
waitForShutdown sc = atomically $ do
    shutdown <- readTVar (scShutdown sc)
    unless shutdown retry -- retry is STM's way to wait for a change

-- Setup store directory structure
setupStoreStructure :: DaemonConfig -> IO ()
setupStoreStructure config = do
    let storeDir = normalise $ daemonStorePath config
    createDirectoryIfMissing True storeDir
    -- Create essential subdirectories
    let subdirs = ["tmp", "gc-roots", "var/ten", "var/ten/db", "var/log", "var/nix/db"]
    forM_ subdirs $ \dir -> createDirectoryIfMissing True (storeDir </> dir)
    -- Set base store permissions (may be overridden later if dropping privs)
    catch (setFileMode storeDir 0o755) (\(_ :: IOException) -> hPutStrLn stderr $ "Warning: could not set permissions on " ++ storeDir)

    -- Ensure DB directories exist and have correct permissions (before DB init)
    let dbDir = takeDirectory $ daemonDbPath config
    createDirectoryIfMissing True dbDir
    catch (setFileMode dbDir 0o700) (\(_ :: IOException) -> hPutStrLn stderr $ "Warning: could not set permissions on " ++ dbDir)

    -- Initialize store components (delegated to Ten library)
    handleDBErrorAsWarning $ initializeStore storeDir
    -- Verify store structure and permissions after initialization
    handleDBErrorAsWarning $ verifyStore storeDir

-- Lock handle type for managing file locks
type LockHandle = Handle

-- Acquire global lock using POSIX file locking
tryAcquireGlobalLock :: FilePath -> IO (Maybe Handle)
tryAcquireGlobalLock lockPath = do
    createDirectoryIfMissing True (takeDirectory lockPath)
    -- Open the file, creating if necessary
    fd <- openFd lockPath ReadWrite (Just 0o644) (defaultFileFlags { creat = Just 0o644 })
    -- Attempt to acquire an exclusive, non-blocking lock
    lockResult <- try @IOException $ System.Posix.IO.setLock fd (System.Posix.IO.WriteLock, AbsoluteSeek, 0, 0)
    case lockResult of
        Left e -> do -- Lock failed (likely already held)
            closeFd fd
            return Nothing
        Right _ -> do -- Lock acquired
            -- Write PID to the lock file
            pid <- getProcessID
            handle <- fdToHandle fd -- Get handle AFTER locking
            hPutStrLn handle (show pid)
            hFlush handle
            -- Keep handle open (holds the lock)
            return (Just handle)

-- Release global lock
releaseGlobalLock :: Handle -> FilePath -> IO ()
releaseGlobalLock h lockPath = do
    putStrLn $ "Releasing global lock: " ++ lockPath
    -- Close the handle, which releases the POSIX lock
    hClose h `catch` \(_ :: IOException) -> return ()
    -- Attempt to remove the lock file
    removeFileSafe lockPath

handleDBErrorAsWarning :: IO () -> IO ()
handleDBErrorAsWarning action = catch action $ \(e :: SomeException) ->
    hPutStrLn stderr $ "Warning during store/DB initialization: " ++ displayException e

-- Write PID file
writePidFile :: FilePath -> IO ()
writePidFile path = do
    pid <- getProcessID
    catch (writeFile path (show pid ++ "\n")) $ \(e :: IOException) ->
        hPutStrLn stderr $ "Warning: Could not write PID file to " ++ path ++ ": " ++ displayException e

-- Remove file safely (ignoring errors)
removeFileSafe :: FilePath -> IO ()
removeFileSafe path = catch (removeFile path) (\(_ :: IOException) -> return ())

-- Compute database path from store path
daemonDbPath :: DaemonConfig -> FilePath
daemonDbPath config = normalise $ daemonStorePath config </> "var/ten/db/ten.db"

-- Setup logging based on configuration
setupLogging :: DaemonConfig -> IO (Handle, Handle) -- Returns (accessLog, securityLog)
setupLogging config = do
    accessLogH <- setupLogHandle (daemonLogFile config) stdout
    -- Security logs might go to a separate file or stderr
    let secLogPath = fmap (++ ".security") (daemonLogFile config)
    securityLogH <- setupLogHandle secLogPath stderr

    -- Set line buffering for immediate output
    hSetBuffering accessLogH LineBuffering
    hSetBuffering securityLogH LineBuffering

    return (accessLogH, securityLogH)

setupLogHandle :: Maybe FilePath -> Handle -> IO Handle
setupLogHandle Nothing fallbackH = return fallbackH
setupLogHandle (Just path) fallbackH
    | path == "stdout" = return stdout
    | path == "stderr" = return stderr
    | otherwise =
        catch (openFile path AppendMode) $ \(e :: IOException) -> do
            hPutStrLn stderr $ "Warning: Could not open log file " ++ path ++ ": " ++ displayException e
            return fallbackH

-- Install simplified signal handlers before ServerControl exists
installSimpleSignalHandlers :: DaemonConfig -> FilePath -> Handle -> IO ()
installSimpleSignalHandlers config lockPath lockHandle = do
    let logH = stderr -- Log basic signal info to stderr
    installHandler sigTERM (Catch $ simpleCleanupAndExit config lockPath lockHandle logH) Nothing
    installHandler sigINT (Catch $ simpleCleanupAndExit config lockPath lockHandle logH) Nothing
    -- Other signals (HUP, USR1, USR2) require ServerControl to be useful
    installHandler sigHUP Ignore Nothing -- Ignore for now
    installHandler sigUSR1 Ignore Nothing
    installHandler sigUSR2 Ignore Nothing
    return ()

-- Simplified cleanup for early exit or before ServerControl is ready
simpleCleanupAndExit :: DaemonConfig -> FilePath -> Handle -> Handle -> IO ()
simpleCleanupAndExit config lockPath lockHandle logH = do
    hPutStrLn logH "[INFO] Received termination signal during startup, shutting down..."
    releaseGlobalLock lockHandle lockPath
    removeFileSafe (daemonSocketPath config)
    hPutStrLn logH "[INFO] Basic cleanup finished."
    exitImmediately ExitSuccess -- Use immediate exit as server loop isn't running

-- Drop privileges if needed
dropPrivilegesIfNeeded :: DaemonConfig -> IO ()
dropPrivilegesIfNeeded config = do
    realUid <- getRealUserID
    effUid <- getEffectiveUserID
    when (realUid == 0 || effUid == 0) $ case (daemonUser config, daemonGroup config) of
        (Just user, mGroupText) -> do
            logToFile config $ "[INFO] Dropping privileges to user=" ++ T.unpack user ++
                                maybe "" (\g -> ", group=" ++ T.unpack g) mGroupText
            dropPrivs (T.unpack user) (T.unpack <$> mGroupText)
        _ -> logToFile config "[INFO] Running as root or current user (no user/group specified)."
  where
    dropPrivs :: String -> Maybe String -> IO ()
    dropPrivs user mGroup = do
        targetUserEntry <- getUserEntryForName user `catch` userNotFound user
        let targetUid = userID targetUserEntry
        targetGid <- case mGroup of
            Just groupName -> groupID <$> (getGroupEntryForName groupName `catch` groupNotFound groupName)
            Nothing        -> return $ userGroupID targetUserEntry -- Use user's primary group

        -- Ensure store ownership *before* dropping privileges
        logToFile config $ "[INFO] Ensuring store ownership for UID=" ++ show targetUid ++ ", GID=" ++ show targetGid
        ensureStoreOwnership (daemonStorePath config) targetUid targetGid `catch`
            (\(e::SomeException) -> logToFile config $ "[WARNING] Failed to set store ownership: " ++ displayException e)

        -- Ensure DB directory ownership
        let dbDir = takeDirectory $ daemonDbPath config
        ensurePathOwnership dbDir targetUid targetGid `catch`
            (\(e::SomeException) -> logToFile config $ "[WARNING] Failed to set DB directory ownership: " ++ displayException e)

        -- Set group first (must be done while root)
        setGroupID targetGid `catch` setGidFailed targetGid

        -- Set user (this drops root permanently)
        setUserID targetUid `catch` setUidFailed targetUid

        -- Verify
        newEffUid <- getEffectiveUserID
        when (newEffUid == 0) $ criticalError config "Failed to drop root privileges!"
        logToFile config $ "[INFO] Privileges dropped successfully."

    userNotFound name e = criticalError config $ "User '" ++ name ++ "' not found: " ++ displayException e
    groupNotFound name e = criticalError config $ "Group '" ++ name ++ "' not found: " ++ displayException e
    setGidFailed gid e = criticalError config $ "Failed to set group ID to " ++ show gid ++ ": " ++ displayException e
    setUidFailed uid e = criticalError config $ "Failed to set user ID to " ++ show uid ++ ": " ++ displayException e

-- Ensure a specific path has correct ownership
ensurePathOwnership :: FilePath -> UserID -> GroupID -> IO ()
ensurePathOwnership path uid gid = do
    exists <- doesPathExist path
    when exists $
        System.Posix.Files.setOwnerAndGroup path uid gid

-- Ensure store has correct ownership recursively (use cautiously)
ensureStoreOwnership :: FilePath -> UserID -> GroupID -> IO ()
ensureStoreOwnership storePath uid gid = do
    -- Only set ownership on top-level and essential subdirs for performance and safety
    forM_ [storePath, storePath </> "var", storePath </> "tmp", storePath </> "var/ten", storePath </> "var/ten/db"] $ \p -> do
         exists <- doesPathExist p
         when exists $ catch (System.Posix.Files.setOwnerAndGroup p uid gid) $ \(e :: SomeException) ->
              logToFile Nothing $ "[WARNING] Failed set owner/group on " ++ p ++ ": " ++ displayException e
    -- Avoid recursive chown unless absolutely necessary due to performance/security risks
    -- For full recursive ownership:
    -- setOwnershipRecursive storePath uid gid

-- Set ownership recursively (use with caution)
setOwnershipRecursive :: FilePath -> UserID -> GroupID -> IO ()
setOwnershipRecursive path uid gid = do
    exists <- doesPathExist path
    when exists $ do
        catch (System.Posix.Files.setOwnerAndGroup path uid gid) $ \(e :: SomeException) ->
             logToFile Nothing $ "[WARNING] Failed set owner/group on " ++ path ++ ": " ++ displayException e
        isDir <- doesDirectoryExist path
        when isDir $ do
            entries <- catch (listDirectory path) (\(_::IOException) -> return []) -- Ignore permission errors listing
            forM_ entries $ \entry -> setOwnershipRecursive (path </> entry) uid gid

-- Log message to file or stderr
logToFile :: Maybe DaemonConfig -> String -> IO ()
logToFile Nothing msg = hPutStrLn stderr msg -- Fallback if config not available
logToFile (Just config) msg =
    case daemonLogFile config of
        Nothing -> hPutStrLn stderr msg -- Default to stderr if no log file configured
        Just "stdout" -> hPutStrLn stdout msg
        Just "stderr" -> hPutStrLn stderr msg
        Just path -> appendFile path (msg ++ "\n") `catch` \(e::IOException) ->
            hPutStrLn stderr ("Failed to write to log file " ++ path ++ ": " ++ displayException e)

-- Default daemon configuration (used if loading fails)
defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig
    { daemonSocketPath = "/var/run/ten/daemon.socket" -- Standard FHS location
    , daemonStorePath = "/var/lib/ten/store"          -- Standard FHS location
    , daemonStateFile = "/var/lib/ten/state.json"     -- Standard FHS location
    , daemonLogFile = Just "/var/log/ten/daemon.log"  -- Standard FHS location
    , daemonLogLevel = 1  -- Normal logging level
    , daemonGcInterval = Just 3600  -- 1 hour
    , daemonUser = Just "ten-daemon" -- Dedicated user is best practice
    , daemonGroup = Just "ten-daemon" -- Dedicated group is best practice
    , daemonAllowedUsers = Set.empty -- Empty means allow all authenticated? Needs clarification.
    , daemonMaxJobs = 4
    , daemonForeground = False
    , daemonTmpDir = "/tmp/ten"
    }

-- Load configuration from file or environment (simplified)
loadDaemonConfig :: Maybe FilePath -> IO DaemonConfig
loadDaemonConfig mPath = do
    -- In a real implementation, parse file (e.g., JSON, YAML) or read env vars
    -- For now, just return the default configuration
    -- If a path is given, we could try reading it as JSON
    case mPath of
        Just path -> do
            exists <- doesFileExist path
            if exists then do
                content <- BS.readFile path
                case Aeson.eitherDecodeStrict content of
                    Left err -> do
                        hPutStrLn stderr $ "Warning: Failed to parse config file " ++ path ++ ": " ++ err
                        hPutStrLn stderr "Using default configuration."
                        return defaultDaemonConfig
                    Right cfg -> return cfg
            else do
                hPutStrLn stderr $ "Warning: Config file not found: " ++ path
                hPutStrLn stderr "Using default configuration."
                return defaultDaemonConfig
        Nothing -> return defaultDaemonConfig

-- Helper to check if a path exists (file or directory)
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    f <- doesFileExist path
    d <- doesDirectoryExist path
    return $ f || d

-- Helper function for critical errors that should terminate the daemon
criticalError :: Maybe DaemonConfig -> String -> IO a
criticalError mConfig msg = do
    let fullMsg = "[CRITICAL] " ++ msg
    logToFile mConfig fullMsg
    hPutStrLn stderr fullMsg -- Ensure it goes to stderr too
    exitFailure

-- JSON instances for DaemonConfig need to be available
-- Add them if not already defined elsewhere (e.g., in Ten.Core or Protocol)
instance Aeson.ToJSON DaemonConfig where
    toJSON DaemonConfig{..} = Aeson.object [
        "socketPath" .= daemonSocketPath,
        "storePath" .= daemonStorePath,
        "stateFile" .= daemonStateFile,
        "logFile" .= daemonLogFile,
        "logLevel" .= daemonLogLevel,
        "gcInterval" .= daemonGcInterval,
        "user" .= daemonUser,
        "group" .= daemonGroup,
        "allowedUsers" .= Set.map T.unpack (daemonAllowedUsers), -- Convert Set Text to [String]
        "maxJobs" .= daemonMaxJobs,
        "foreground" .= daemonForeground,
        "tmpDir" .= daemonTmpDir
        ]

instance Aeson.FromJSON DaemonConfig where
    parseJSON = Aeson.withObject "DaemonConfig" $ \v -> do
        daemonSocketPath <- v Aeson..: "socketPath"
        daemonStorePath <- v Aeson..: "storePath"
        daemonStateFile <- v Aeson..: "stateFile"
        daemonLogFile <- v Aeson..:? "logFile"
        daemonLogLevel <- v Aeson..:? "logLevel" .!= daemonLogLevel defaultDaemonConfig
        daemonGcInterval <- v Aeson..:? "gcInterval"
        daemonUser <- v Aeson..:? "user"
        daemonGroup <- v Aeson..:? "group"
        allowedUsersList <- v Aeson..:? "allowedUsers" .!= [] -- Expect list of strings
        daemonMaxJobs <- v Aeson..:? "maxJobs" .!= daemonMaxJobs defaultDaemonConfig
        daemonForeground <- v Aeson..:? "foreground" .!= daemonForeground defaultDaemonConfig
        daemonTmpDir <- v Aeson..:? "tmpDir" .!= daemonTmpDir defaultDaemonConfig
        let daemonAllowedUsers = Set.fromList (map T.pack allowedUsersList)
        return DaemonConfig{..}

-- MapMaybe utility
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) = case f x of
    Just y -> y : mapMaybe f xs
    Nothing -> mapMaybe f xs
