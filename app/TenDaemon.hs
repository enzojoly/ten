{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Control.Concurrent (threadDelay, myThreadId)
import Control.Exception (bracket, try, catch, SomeException, IOException, finally)
import Control.Monad (when, unless, forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.GetOpt
import System.Directory (doesFileExist, createDirectoryIfMissing, removeFile)
import System.Environment (getArgs, getProgName, lookupEnv, getEnvironment)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode(..), withFile, hPutStrLn, stdout, stderr, hSetBuffering, BufferMode(..))
import System.Posix.Daemon (daemonize)
import System.Posix.Files (fileExist, setFileMode)
import System.Posix.Process (getProcessID, exitImmediately)
import System.Posix.Signals
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName, UserEntry(..), GroupEntry(..),
                         getRealUserID, setUserID, setGroupID, getUserEntryForName, getGroupEntryForName,
                         userID, groupID)

-- Import privilege-aware modules
import Ten.Core
       (BuildError(..), LogMsg(..), TenM, BuildEnv(..), BuildState, PrivCtx(..),
        runTenDaemon, initDaemonEnv, initStoreEnv, initDaemonState,
        withPrivilegedContext, dropPrivileges, setupPrivilegedStore)
import Ten.Daemon.Config (DaemonConfig(..), LogLevel(..), defaultDaemonConfig, getDefaultConfig, parseConfigArgs,
                          loadConfigFromEnv, loadConfigFromFile, saveConfigToFile)
import Ten.Daemon.Core (startDaemon, stopDaemon, isDaemonRunning, initSecureSocket, acquireGlobalLock)
import Ten.Daemon.State (DaemonState(..), initializeDaemonState, persistStateToFile, recoverStateFromFile)
import Ten.Daemon.Server (runDaemonServer)
import Ten.Daemon.Protocol (initializeProtocolHandlers)
import Ten.Store (initializeStore, createStoreDirectories, verifyStore)
import Ten.DB.Core (initializeDatabase, ensureDBDirectories)

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
    | OptReload                     -- Reload configuration
    | OptDebug                      -- Enable debug mode
    | OptMaxJobs Int                -- Maximum concurrent jobs
    deriving (Show, Eq)

-- | Command to execute
data DaemonCommand
    = CmdStart                      -- Start the daemon
    | CmdStop                       -- Stop the daemon
    | CmdRestart                    -- Restart the daemon
    | CmdStatus                     -- Show daemon status
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
    , Option ['r'] ["reload"]
        (NoArg OptReload)
        "Reload configuration"
    , Option ['d'] ["debug"]
        (NoArg OptDebug)
        "Enable debug mode"
    , Option ['j'] ["max-jobs"]
        (ReqArg (OptMaxJobs . read) "NUM")
        "Maximum number of concurrent build jobs"
    ]

-- | Parse command line arguments
parseArgs :: [String] -> IO (DaemonCommand, [DaemonOption])
parseArgs args = do
    -- Get the command
    let (cmdArg, remainingArgs) = case args of
            [] -> (CmdHelp, [])
            (cmd:rest) -> (parseCommand cmd, rest)

    -- Parse remaining options
    case getOpt Permute options remainingArgs of
        (opts, [], []) -> return (cmdArg, opts)
        (_, nonOpts, []) ->
            error $ "Unexpected arguments: " ++ unwords nonOpts
        (_, _, errs) ->
            error $ concat errs ++ usageInfo usageHeader options
  where
    parseCommand :: String -> DaemonCommand
    parseCommand cmd = case cmd of
        "start"   -> CmdStart
        "stop"    -> CmdStop
        "restart" -> CmdRestart
        "status"  -> CmdStatus
        "help"    -> CmdHelp
        "version" -> CmdVersion
        _ -> error $ "Unknown command: " ++ cmd

-- | Show usage information
usageHeader :: String
usageHeader = "Usage: ten-daemon COMMAND [OPTIONS]\n\nCommands:\n" ++
              "  start      Start the daemon\n" ++
              "  stop       Stop the daemon\n" ++
              "  restart    Restart the daemon\n" ++
              "  status     Show daemon status\n" ++
              "  help       Show this help message\n" ++
              "  version    Show version information\n\n" ++
              "Options:"

-- | Show help information
showHelp :: IO ()
showHelp = putStrLn $ usageInfo usageHeader options

-- | Show version information
showVersion :: IO ()
showVersion = do
    putStrLn "Ten Daemon version 0.1.0"
    putStrLn "Copyright (C) 2025"
    putStrLn "License: MIT"

-- | Main entry point
main :: IO ()
main = do
    -- Parse command line arguments
    args <- getArgs
    (cmd, opts) <- parseArgs args

    -- Process help and version commands early
    case cmd of
        CmdHelp -> showHelp >> exitSuccess
        CmdVersion -> showVersion >> exitSuccess
        _ -> return ()

    -- Extract options
    let getOpt' f = foldr (\opt acc -> case opt of f x -> Just x; _ -> acc) Nothing opts
    let hasOpt f = any (\case { f -> True; _ -> False }) opts

    let configFile = getOpt' (\case OptConfig x -> Just x; _ -> Nothing)
    let socketPath = getOpt' (\case OptSocketPath x -> Just x; _ -> Nothing)
    let storePath = getOpt' (\case OptStorePath x -> Just x; _ -> Nothing)
    let stateFile = getOpt' (\case OptStateFile x -> Just x; _ -> Nothing)
    let userOverride = getOpt' (\case OptUser x -> Just x; _ -> Nothing)
    let groupOverride = getOpt' (\case OptGroup x -> Just x; _ -> Nothing)
    let logFile = getOpt' (\case OptLogFile x -> Just x; _ -> Nothing)
    let logLevel = getOpt' (\case OptLogLevel x -> Just x; _ -> Nothing)
    let foreground = hasOpt OptForeground
    let pidFile = getOpt' (\case OptPidFile x -> Just x; _ -> Nothing)
    let reload = hasOpt OptReload
    let debug = hasOpt OptDebug
    let maxJobs = getOpt' (\case OptMaxJobs x -> Just x; _ -> Nothing)

    -- Load configuration
    config <- case configFile of
        Just path -> do
            result <- loadConfigFromFile path
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Error loading config file: " ++ err
                    -- Fall back to environment/default config
                    loadConfigFromEnv
                Right cfg -> return cfg
        Nothing -> loadConfigFromEnv

    -- Apply command line overrides to config
    let config' = config
            { daemonSocketPath = fromMaybe (daemonSocketPath config) socketPath
            , daemonStorePath = fromMaybe (daemonStorePath config) storePath
            , daemonStateFile = fromMaybe (daemonStateFile config) stateFile
            , daemonUser = T.pack <$> userOverride <|> daemonUser config
            , daemonGroup = T.pack <$> groupOverride <|> daemonGroup config
            , daemonLogFile = (\f -> if f == "stdout" then Nothing else Just f) <$> logFile <|> daemonLogFile config
            , daemonLogLevel = fromMaybe (daemonLogLevel config) logLevel
            , daemonForeground = foreground || daemonForeground config
            , daemonMaxJobs = fromMaybe (daemonMaxJobs config) maxJobs
            }

    -- Enable debug mode if requested
    let config'' = if debug
                  then config' { daemonLogLevel = LogDebug }
                  else config'

    -- Make directories as needed
    createDirectoryIfMissing True (takeDirectory (daemonSocketPath config''))
    createDirectoryIfMissing True (daemonStorePath config'')
    createDirectoryIfMissing True (takeDirectory (daemonStateFile config''))
    case daemonLogFile config'' of
        Just logPath -> createDirectoryIfMissing True (takeDirectory logPath)
        Nothing -> return ()

    -- Process daemon commands
    case cmd of
        CmdStart -> startDaemonProcess config'' pidFile
        CmdStop -> stopDaemonProcess config''
        CmdRestart -> restartDaemonProcess config'' pidFile
        CmdStatus -> checkDaemonStatus config''
        _ -> error "Unexpected command" -- Should never happen due to early handling
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> y = y
    x <|> _ = x

-- | Start the daemon process
startDaemonProcess :: DaemonConfig -> Maybe FilePath -> IO ()
startDaemonProcess config pidFilePath = do
    -- Check if the daemon is already running
    running <- isDaemonRunning (daemonSocketPath config)
    when running $ do
        hPutStrLn stderr "Daemon is already running"
        exitFailure

    -- Ensure store directory structure exists
    setupStoreStructure config

    -- Acquire global lock to ensure only one daemon is running
    acquireGlobalLockSafe config

    -- Check if we should daemonize or run in foreground
    if daemonForeground config
        then do
            -- Run in foreground
            putStrLn $ "Starting Ten daemon in foreground mode (socket: " ++ daemonSocketPath config ++ ")"
            runDaemonWithConfig config
        else do
            -- Daemonize
            putStrLn $ "Starting Ten daemon in background mode (socket: " ++ daemonSocketPath config ++ ")"

            -- Determine PID file path
            let pidFile = fromMaybe (takeDirectory (daemonSocketPath config) </> "ten-daemon.pid") pidFilePath

            -- Create parent directories for PID file
            createDirectoryIfMissing True (takeDirectory pidFile)

            -- Daemonize the process
            daemonize $ do
                -- Set up signal handlers
                installSignalHandlers config

                -- Write PID file
                writePidFile pidFile

                -- Initialize store directories with correct permissions
                initializeSecureStore config

                -- Drop privileges if running as root and user/group specified
                dropPrivilegesIfNeeded config

                -- Run the daemon
                runDaemonWithConfig config `finally` (do
                    -- Clean up on exit
                    cleanupAtExit config
                    -- Remove PID file
                    removeFileSafe pidFile
                    )

-- | Setup store directory structure
setupStoreStructure :: DaemonConfig -> IO ()
setupStoreStructure config = do
    -- Create base store directory
    createDirectoryIfMissing True (daemonStorePath config)

    -- Create essential subdirectories
    let subdirs = ["tmp", "gc-roots", "var/ten", "var/log", "var/nix/db"]
    forM_ subdirs $ \dir ->
        createDirectoryIfMissing True (daemonStorePath config </> dir)

    -- Create database directories
    ensureDBDirectories (daemonStorePath config)

    -- Set proper permissions on store directories
    -- Root needs read/write/execute, everyone else needs read/execute
    setFileMode (daemonStorePath config) 0o755

-- | Acquire global lock safely
acquireGlobalLockSafe :: DaemonConfig -> IO ()
acquireGlobalLockSafe config = do
    let lockPath = daemonStorePath config </> "var/ten/daemon.lock"
    createDirectoryIfMissing True (takeDirectory lockPath)
    result <- try $ acquireGlobalLock lockPath
    case result of
        Left (e :: SomeException) -> do
            hPutStrLn stderr $ "Error acquiring global lock: " ++ show e
            exitFailure
        Right _ -> return ()

-- | Initialize secure store
initializeSecureStore :: DaemonConfig -> IO ()
initializeSecureStore config = do
    -- Create store directories
    createStoreDirectories (daemonStorePath config)

    -- Initialize store database
    result <- try $ initializeStore (daemonStorePath config)
    case result of
        Left (e :: SomeException) -> do
            logError $ "Failed to initialize store: " ++ show e
            throw e
        Right _ -> return ()
  where
    logError :: String -> IO ()
    logError msg = case daemonLogFile config of
        Just path -> appendFile path (msg ++ "\n")
        Nothing -> hPutStrLn stderr msg

    throw :: SomeException -> IO ()
    throw = throwIO

-- | Stop the daemon process
stopDaemonProcess :: DaemonConfig -> IO ()
stopDaemonProcess config = do
    -- Check if the daemon is running
    running <- isDaemonRunning (daemonSocketPath config)
    unless running $ do
        hPutStrLn stderr "Daemon is not running"
        exitFailure

    -- Stop the daemon
    putStrLn "Stopping Ten daemon..."
    result <- stopDaemon (daemonSocketPath config)
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error stopping daemon: " ++ T.unpack err
            exitFailure
        Right _ -> do
            putStrLn "Daemon stopped successfully"
            exitSuccess

-- | Restart the daemon process
restartDaemonProcess :: DaemonConfig -> Maybe FilePath -> IO ()
restartDaemonProcess config pidFilePath = do
    -- Check if the daemon is running
    running <- isDaemonRunning (daemonSocketPath config)

    -- Stop the daemon if it's running
    when running $ do
        putStrLn "Stopping Ten daemon..."
        result <- stopDaemon (daemonSocketPath config)
        case result of
            Left err -> do
                hPutStrLn stderr $ "Error stopping daemon: " ++ T.unpack err
                exitFailure
            Right _ -> do
                putStrLn "Daemon stopped successfully"

                -- Wait for the socket to disappear
                waitForSocketToDisappear (daemonSocketPath config)

    -- Start the daemon
    startDaemonProcess config pidFilePath

-- | Check daemon status
checkDaemonStatus :: DaemonConfig -> IO ()
checkDaemonStatus config = do
    -- Check if the daemon is running
    running <- isDaemonRunning (daemonSocketPath config)
    if running
        then do
            putStrLn "Ten daemon is running"
            exitSuccess
        else do
            putStrLn "Ten daemon is not running"
            exitFailure

-- | Write PID file
writePidFile :: FilePath -> IO ()
writePidFile path = do
    pid <- getProcessID
    writeFile path (show pid)

-- | Remove file safely (ignoring errors)
removeFileSafe :: FilePath -> IO ()
removeFileSafe path = do
    exists <- doesFileExist path
    when exists $ catch (removeFile path) (\(_ :: IOException) -> return ())

-- | Wait for socket file to disappear
waitForSocketToDisappear :: FilePath -> IO ()
waitForSocketToDisappear path = do
    exists <- fileExist path
    when exists $ do
        threadDelay 100000  -- 100ms
        waitForSocketToDisappear path

-- | Run the daemon with given configuration
runDaemonWithConfig :: DaemonConfig -> IO ()
runDaemonWithConfig config = do
    -- Set up logging
    setupLogging config

    -- Initialize database
    dbPath <- initializeDatabase (daemonStorePath config </> "var/ten/db") 5000

    -- Initialize secure daemon socket
    socket <- initSecureSocket (daemonSocketPath config)

    -- Initialize protocol handlers
    protocolHandlers <- initializeProtocolHandlers

    -- Load or initialize daemon state
    state <- loadOrInitializeState config

    -- Initialize the privileged store environment
    storeEnv <- initStoreEnv (daemonStorePath config)

    -- Initialize daemon environment with privilege awareness
    let env = initDaemonEnv
                (daemonStorePath config </> "tmp")
                (daemonStorePath config)
                (daemonUser config)

    -- Initialize daemon state with proper privilege context
    dState <- initDaemonState 'Daemon state

    -- Setup the privileged store with proper access controls
    void $ runTenDaemon (setupPrivilegedStore (daemonSocketPath config)) env dState

    -- Run the daemon server in privileged context
    result <- try $ withPrivilegedContext $
                runDaemonServer config state socket protocolHandlers
    case result of
        Left (e :: SomeException) -> do
            logError $ "Daemon crashed: " ++ show e
            exitFailure
        Right _ -> return ()
  where
    logError :: String -> IO ()
    logError msg = case daemonLogFile config of
        Just path -> appendFile path (msg ++ "\n")
        Nothing -> hPutStrLn stderr msg

-- | Set up logging based on configuration
setupLogging :: DaemonConfig -> IO ()
setupLogging config = do
    -- Set up log file or stdout
    case daemonLogFile config of
        Just path -> do
            -- Create directory if needed
            createDirectoryIfMissing True (takeDirectory path)
            -- Make sure we can write to the log file
            withFile path AppendMode $ \_ -> return ()
        Nothing -> do
            -- Log to stdout
            hSetBuffering stdout LineBuffering
            hSetBuffering stderr LineBuffering

-- | Load existing state or initialize new state
loadOrInitializeState :: DaemonConfig -> IO DaemonState
loadOrInitializeState config = do
    -- Check if state file exists
    stateFileExists <- doesFileExist (daemonStateFile config)

    if stateFileExists
        then do
            -- Try to load existing state
            result <- recoverStateFromFile (daemonStateFile config)
            case result of
                Left err -> do
                    logWarning $ "Failed to load state file, initializing new state: " ++ err
                    initializeDaemonState (daemonMaxJobs config)
                Right state -> do
                    logInfo "Loaded daemon state from file"
                    return state
        else do
            -- Initialize new state
            logInfo "Initializing new daemon state"
            initializeDaemonState (daemonMaxJobs config)
  where
    logWarning, logInfo :: String -> IO ()
    logWarning msg = case daemonLogFile config of
        Just path -> appendFile path ("[WARNING] " ++ msg ++ "\n")
        Nothing -> hPutStrLn stderr ("[WARNING] " ++ msg)

    logInfo msg = case daemonLogFile config of
        Just path -> appendFile path ("[INFO] " ++ msg ++ "\n")
        Nothing -> hPutStrLn stdout ("[INFO] " ++ msg)

-- | Install signal handlers
installSignalHandlers :: DaemonConfig -> IO ()
installSignalHandlers config = do
    -- Handle SIGTERM (terminate)
    installHandler sigTERM (Catch $ cleanupAndExit config) Nothing

    -- Handle SIGINT (interrupt)
    installHandler sigINT (Catch $ cleanupAndExit config) Nothing

    -- Handle SIGHUP (hangup/reload)
    installHandler sigHUP (Catch $ reloadConfig config) Nothing

    -- Handle SIGUSR1 (custom action, e.g., dump state)
    installHandler sigUSR1 (Catch $ dumpState config) Nothing

    -- Handle SIGUSR2 (trigger garbage collection)
    installHandler sigUSR2 (Catch $ triggerGC config) Nothing

    return ()

-- | Clean up and exit
cleanupAndExit :: DaemonConfig -> IO ()
cleanupAndExit config = do
    -- Clean up resources
    cleanupAtExit config

    -- Exit
    exitImmediately (fromIntegral 0)

-- | Reload configuration
reloadConfig :: DaemonConfig -> IO ()
reloadConfig config = do
    logInfo "Received SIGHUP, reloading configuration"
    -- In a real implementation, this would reload the config
    -- and reconfigure the running daemon
    return ()
  where
    logInfo msg = case daemonLogFile config of
        Just path -> appendFile path ("[INFO] " ++ msg ++ "\n")
        Nothing -> hPutStrLn stdout ("[INFO] " ++ msg)

-- | Dump state
dumpState :: DaemonConfig -> IO ()
dumpState config = do
    logInfo "Received SIGUSR1, dumping state"
    -- In a real implementation, this would dump the current state
    -- to a file or the log for debugging
    return ()
  where
    logInfo msg = case daemonLogFile config of
        Just path -> appendFile path ("[INFO] " ++ msg ++ "\n")
        Nothing -> hPutStrLn stdout ("[INFO] " ++ msg)

-- | Trigger garbage collection
triggerGC :: DaemonConfig -> IO ()
triggerGC config = do
    logInfo "Received SIGUSR2, triggering garbage collection"
    -- In a real implementation, this would trigger the GC process
    return ()
  where
    logInfo msg = case daemonLogFile config of
        Just path -> appendFile path ("[INFO] " ++ msg ++ "\n")
        Nothing -> hPutStrLn stdout ("[INFO] " ++ msg)

-- | Clean up resources at exit
cleanupAtExit :: DaemonConfig -> IO ()
cleanupAtExit config = do
    -- Remove socket file
    removeFileSafe (daemonSocketPath config)

    -- Release any locks
    removeFileSafe (daemonStorePath config </> "var/ten/daemon.lock")
    removeFileSafe (daemonStorePath config </> "var/ten/gc.lock")

    -- Persist state if possible
    catch (saveCurrentState config) (\(_ :: SomeException) -> return ())

-- | Save current daemon state
saveCurrentState :: DaemonConfig -> IO ()
saveCurrentState config = do
    -- In a real implementation, this would save the full daemon state
    logInfo "Saving daemon state before exit"
  where
    logInfo msg = case daemonLogFile config of
        Just path -> appendFile path ("[INFO] " ++ msg ++ "\n")
        Nothing -> hPutStrLn stdout ("[INFO] " ++ msg)

-- | Drop privileges if needed with enhanced security
dropPrivilegesIfNeeded :: DaemonConfig -> IO ()
dropPrivilegesIfNeeded config = do
    -- Check if we're running as root
    uid <- getRealUserID
    when (uid == 0) $ do
        -- Check if we should drop privileges
        case (daemonUser config, daemonGroup config) of
            (Just user, Just group) -> do
                -- Get user and group entries
                userEntry <- getUserEntryForName (T.unpack user)
                groupEntry <- getGroupEntryForName (T.unpack group)

                -- Set up store directories with correct ownership
                ensureStoreOwnership (daemonStorePath config) (userID userEntry) (groupID groupEntry)

                -- Set group first (needed before dropping user privileges)
                setGroupID (groupID groupEntry)

                -- Set user (dropping root privileges)
                setUserID (userID userEntry)

                -- Verify privilege drop worked
                newUid <- getEffectiveUserID
                when (newUid == 0) $
                    error "Failed to drop privileges - still running as root!"

                logInfo $ "Dropped privileges to user=" ++ T.unpack user ++ ", group=" ++ T.unpack group

            (Just user, Nothing) -> do
                -- Get user entry
                userEntry <- getUserEntryForName (T.unpack user)

                -- Use user's primary group
                let gid = userGroupID userEntry

                -- Set up store directories with correct ownership
                ensureStoreOwnership (daemonStorePath config) (userID userEntry) gid

                -- Set user (dropping root privileges)
                setUserID (userID userEntry)

                -- Verify privilege drop worked
                newUid <- getEffectiveUserID
                when (newUid == 0) $
                    error "Failed to drop privileges - still running as root!"

                logInfo $ "Dropped privileges to user=" ++ T.unpack user

            _ -> return () -- No privilege dropping requested
  where
    logInfo msg = case daemonLogFile config of
        Just path -> appendFile path ("[INFO] " ++ msg ++ "\n")
        Nothing -> hPutStrLn stdout ("[INFO] " ++ msg)

-- | Ensure store has correct ownership for privilege drop
ensureStoreOwnership :: FilePath -> UserID -> GroupID -> IO ()
ensureStoreOwnership storePath uid gid = do
    -- Set ownership on key directories
    setOwnershipRecursive storePath uid gid
    setOwnershipRecursive (storePath </> "var") uid gid
    setOwnershipRecursive (storePath </> "tmp") uid gid

    -- Log that we're setting ownership
    putStrLn $ "Setting store ownership to UID=" ++ show uid ++ ", GID=" ++ show gid

-- | Set ownership recursively on a directory
setOwnershipRecursive :: FilePath -> UserID -> GroupID -> IO ()
setOwnershipRecursive path uid gid = do
    -- Check if path exists
    exists <- doesPathExist path
    when exists $ do
        -- Set ownership on this path
        setOwnerAndGroup path uid gid `catch` \(e :: SomeException) ->
            hPutStrLn stderr $ "Warning: Could not set ownership on " ++ path ++ ": " ++ show e

        -- Check if it's a directory
        isDir <- doesDirectoryExist path
        when isDir $ do
            -- Get directory contents
            contents <- listDirectory path
            -- Set ownership on each entry
            forM_ contents $ \entry ->
                setOwnershipRecursive (path </> entry) uid gid

-- Missing helper functions for compatibility
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    fileExists <- doesFileExist path
    if fileExists
        then return True
        else doesDirectoryExist path

setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup path uid gid = do
    setOwnership path uid gid

-- Import compatibility functions
import System.Posix.Files (setOwnerAndGroup)
throwIO :: Exception e => e -> IO a
throwIO = Control.Exception.throwIO
listDirectory :: FilePath -> IO [FilePath]
listDirectory = System.Directory.listDirectory
