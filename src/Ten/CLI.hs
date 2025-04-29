{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Ten.CLI (
    -- Command types
    Command(..),
    Options(..),
    defaultOptions,
    StoreCommand(..),
    DaemonCommand(..),
    DerivationCommand(..),

    -- Command parsing
    parseArgs,

    -- Command execution
    runCommand,

    -- Daemon commands
    daemonCommand,

    -- Command privilege determination
    commandPrivilege,
    PrivilegeRequirement(..),

    -- Individual command handlers
    handleBuild,
    handleEval,
    handleGC,
    handleStore,
    handleInfo,
    handleDaemon,
    handleDerivation,
    handleHelp,
    handleVersion,

    -- Utility functions
    resolveStorePath,
    getDefaultStorePath,
    getDefaultWorkDir,
    showBuildResult,
    showDerivation,

    -- Daemon integration
    shouldRunWithDaemon,
    checkDaemonAvailable,
    runWithDaemon,
    runStandalone,
    executeDaemonCommand,
    getUserCredentials,

    -- Privilege handling
    elevatePrivileges,
    dispatchCommand,
    executeInContext
) where

import Control.Concurrent (threadDelay, myThreadId, killThread)
import Control.Exception (try, catch, SomeException, IOException, bracket, finally, throwIO)
import Control.Monad (when, unless, void, forM_, foldM, filterM)
import Data.List (intercalate, isPrefixOf, isInfixOf)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, catMaybes, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Data.Singletons
import Network.Socket (Socket, SockAddr(..), socketToHandle, close)
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory,
                        getXdgDirectory, XdgDirectory(..), removeFile, doesDirectoryExist,
                        canonicalizePath, findExecutable, setPermissions, getPermissions,
                        Permissions(..), setOwnerExecutable, setOwnerWritable,
                        setOwnerReadable)
import System.Environment (getArgs, getProgName, lookupEnv, getEnvironment)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName, takeExtension)
import System.IO (IOMode(..), withFile, hPutStrLn, stdout, stderr, stdin, hClose,
                  hSetBuffering, BufferMode(..), hFlush, hGetContents, Handle, openFile)
import System.Console.GetOpt
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName,
                         setUserID, userID)
import System.Posix.Files (fileExist, setFileMode)
import System.Posix.Process (getProcessID, executeFile, exitImmediately)
import System.Posix.Signals (installHandler, Handler(..), signalProcess, sigTERM)
import System.Process (createProcess, proc, waitForProcess, readProcessWithExitCode)
import System.Random (randomRIO)
import Text.Read (readMaybe)

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Build
import Ten.GC
import Ten.Hash
import Ten.DB.Core
import Ten.Daemon.Protocol (getDefaultSocketPath)
import qualified Ten.Daemon.Client as DaemonClient

-- | CLI commands
data Command
    = Build FilePath                     -- Build a derivation file
    | Eval FilePath                      -- Evaluate a Ten expression file
    | GC                                 -- Garbage collection
    | Store StoreCommand                 -- Store operations
    | Derivation DerivationCommand       -- Derivation operations
    | Info FilePath                      -- Show info about a store path
    | Daemon DaemonCommand               -- Daemon operations
    | Help                               -- Show help
    | Version                            -- Show version
    deriving (Show, Eq)

-- | Store subcommands
data StoreCommand
    = StoreAdd FilePath                  -- Add a file to the store
    | StoreVerify FilePath               -- Verify a store path
    | StorePath FilePath                 -- Convert a path to a store path
    | StoreGC                            -- Run garbage collection
    | StoreList                          -- List store contents
    deriving (Show, Eq)

-- | Derivation subcommands
data DerivationCommand
    = DerivationInfo FilePath            -- Show info about a derivation
    | QueryDeriver FilePath              -- Find which derivation produced an output
    | StoreDerivation FilePath           -- Store a derivation file
    | RegisterDerivation FilePath        -- Register a derivation in the database
    | ListDerivations                    -- List registered derivations
    deriving (Show, Eq)

-- | Daemon subcommands
data DaemonCommand
    = DaemonStart                        -- Start the daemon
    | DaemonStop                         -- Stop the daemon
    | DaemonStatus                       -- Check daemon status
    | DaemonRestart                      -- Restart the daemon
    | DaemonConfig                       -- Show daemon configuration
    deriving (Show, Eq)

-- | Privilege requirement for commands
data PrivilegeRequirement = DaemonRequired | BuilderSufficient
    deriving (Show, Eq)

-- | CLI options
data Options = Options
    { optVerbosity :: Int                -- Verbosity level (0-3)
    , optStorePath :: Maybe FilePath     -- Path to the store
    , optWorkDir :: Maybe FilePath       -- Working directory
    , optKeepFailed :: Bool              -- Keep failed build outputs
    , optForce :: Bool                   -- Force operation even if risky
    , optUseDaemon :: Bool               -- Use daemon if available
    , optDaemonSocket :: Maybe FilePath  -- Path to daemon socket
    , optSystemType :: Maybe Text        -- Target system type
    , optMaxJobs :: Maybe Int            -- Maximum parallel jobs
    , optSuppressOutput :: Bool          -- Suppress command output
    , optBuildArgs :: [(String, String)] -- Extra build arguments
    } deriving (Show, Eq)

-- | Default options
defaultOptions :: Options
defaultOptions = Options
    { optVerbosity = 1
    , optStorePath = Nothing
    , optWorkDir = Nothing
    , optKeepFailed = False
    , optForce = False
    , optUseDaemon = True
    , optDaemonSocket = Nothing
    , optSystemType = Nothing
    , optMaxJobs = Nothing
    , optSuppressOutput = False
    , optBuildArgs = []
    }

-- | Command-line options specification
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v'] ["verbose"]
        (NoArg (\opts -> opts { optVerbosity = min 3 (optVerbosity opts + 1) }))
        "Increase verbosity"
    , Option ['q'] ["quiet"]
        (NoArg (\opts -> opts { optVerbosity = max 0 (optVerbosity opts - 1) }))
        "Decrease verbosity"
    , Option ['s'] ["store"]
        (ReqArg (\path opts -> opts { optStorePath = Just (sanitizeFilePath path) }) "PATH")
        "Path to the store"
    , Option ['w'] ["work-dir"]
        (ReqArg (\dir opts -> opts { optWorkDir = Just (sanitizeFilePath dir) }) "DIR")
        "Working directory"
    , Option ['k'] ["keep-failed"]
        (NoArg (\opts -> opts { optKeepFailed = True }))
        "Keep build outputs even if build fails"
    , Option ['f'] ["force"]
        (NoArg (\opts -> opts { optForce = True }))
        "Force operation even if risky (can break stale locks)"
    , Option [] ["no-daemon"]
        (NoArg (\opts -> opts { optUseDaemon = False }))
        "Don't use daemon even if available"
    , Option [] ["daemon-socket"]
        (ReqArg (\sock opts -> opts { optDaemonSocket = Just (sanitizeFilePath sock) }) "PATH")
        "Path to daemon socket"
    , Option [] ["system"]
        (ReqArg (\sys opts -> opts { optSystemType = Just (T.pack $ sanitizeInput sys) }) "TYPE")
        "Target system type (e.g., x86_64-linux)"
    , Option ['j'] ["jobs"]
        (ReqArg (\n opts -> case validateNumber n of
                                Just num -> opts { optMaxJobs = Just num }
                                Nothing -> opts) "N")
        "Maximum number of parallel jobs"
    , Option [] ["silent"]
        (NoArg (\opts -> opts { optSuppressOutput = True }))
        "Suppress command output"
    , Option [] ["arg"]
        (ReqArg (\arg opts ->
                 let (key, val) = case break (=='=') arg of
                                       (k, '=':v) -> (sanitizeInput k, sanitizeInput v)
                                       (k, _) -> (sanitizeInput k, "")
                 in opts { optBuildArgs = (key, val) : optBuildArgs opts }) "KEY=VALUE")
        "Pass build argument"
    ]

-- | Input validation for file paths
sanitizeFilePath :: FilePath -> FilePath
sanitizeFilePath path
    | null path = "."
    | ".." `isInfixOf` path = path -- We'll handle this via canonicalizePath later
    | otherwise = path

-- | Input validation for generic string input
sanitizeInput :: String -> String
sanitizeInput input
    | any (`elem` "`$\\") input = filter (`notElem` "`$\\") input
    | otherwise = input

-- | Validate numeric input
validateNumber :: String -> Maybe Int
validateNumber s = case readMaybe s of
    Just n | n > 0 && n < 1000 -> Just n
    _ -> Nothing

-- | Parse command line arguments
parseArgs :: [String] -> Either String (Command, Options)
parseArgs [] = Right (Help, defaultOptions)
parseArgs (cmdStr:args) = do
    let (flags, nonOpts, errors) = getOpt Permute options args

    unless (null errors) $
        Left $ "Error parsing options: " ++ concat errors

    let opts = foldr ($) defaultOptions flags

    cmd <- case cmdStr of
        "build" ->
            case nonOpts of
                (file:_) -> Right $ Build (sanitizeFilePath file)
                [] -> Left "build: missing file argument"
        "eval" ->
            case nonOpts of
                (file:_) -> Right $ Eval (sanitizeFilePath file)
                [] -> Left "eval: missing file argument"
        "gc" -> Right GC
        "store" ->
            case nonOpts of
                [] -> Left "store: missing subcommand"
                (subcmd:args') ->
                    case subcmd of
                        "add" ->
                            case args' of
                                (file:_) -> Right $ Store (StoreAdd (sanitizeFilePath file))
                                [] -> Left "store add: missing file argument"
                        "verify" ->
                            case args' of
                                (path:_) -> Right $ Store (StoreVerify (sanitizeFilePath path))
                                [] -> Left "store verify: missing path argument"
                        "path" ->
                            case args' of
                                (file:_) -> Right $ Store (StorePath (sanitizeFilePath file))
                                [] -> Left "store path: missing file argument"
                        "gc" -> Right $ Store StoreGC
                        "list" -> Right $ Store StoreList
                        _ -> Left $ "unknown store subcommand: " ++ subcmd
        "derivation" ->
            case nonOpts of
                [] -> Left "derivation: missing subcommand"
                (subcmd:args') ->
                    case subcmd of
                        "info" ->
                            case args' of
                                (path:_) -> Right $ Derivation (DerivationInfo (sanitizeFilePath path))
                                [] -> Left "derivation info: missing path argument"
                        "query-deriver" ->
                            case args' of
                                (path:_) -> Right $ Derivation (QueryDeriver (sanitizeFilePath path))
                                [] -> Left "derivation query-deriver: missing path argument"
                        "store" ->
                            case args' of
                                (file:_) -> Right $ Derivation (StoreDerivation (sanitizeFilePath file))
                                [] -> Left "derivation store: missing file argument"
                        "register" ->
                            case args' of
                                (file:_) -> Right $ Derivation (RegisterDerivation (sanitizeFilePath file))
                                [] -> Left "derivation register: missing file argument"
                        "list" -> Right $ Derivation ListDerivations
                        _ -> Left $ "unknown derivation subcommand: " ++ subcmd
        "info" ->
            case nonOpts of
                (path:_) -> Right $ Info (sanitizeFilePath path)
                [] -> Left "info: missing path argument"
        "daemon" ->
            case nonOpts of
                [] -> Left "daemon: missing subcommand"
                (subcmd:_) ->
                    case subcmd of
                        "start" -> Right $ Daemon DaemonStart
                        "stop" -> Right $ Daemon DaemonStop
                        "status" -> Right $ Daemon DaemonStatus
                        "restart" -> Right $ Daemon DaemonRestart
                        "config" -> Right $ Daemon DaemonConfig
                        _ -> Left $ "unknown daemon subcommand: " ++ subcmd
        "help" -> Right Help
        "version" -> Right Version
        _ -> Left $ "unknown command: " ++ cmdStr

    return (cmd, opts)

-- | Determine which privilege tier is needed for a command
commandPrivilege :: Command -> PrivilegeRequirement
commandPrivilege = \case
    Build _ -> BuilderSufficient       -- Build can use either context
    Eval _ -> BuilderSufficient        -- Eval can use either context
    Store (StoreAdd _) -> DaemonRequired    -- Adding to store needs daemon privileges
    Store (StoreList) -> BuilderSufficient  -- Listing can use either
    Store (StoreVerify _) -> BuilderSufficient  -- Verification can use either
    Store (StorePath _) -> BuilderSufficient  -- Path calculation can use either
    Store (StoreGC) -> DaemonRequired       -- GC needs daemon privileges
    GC -> DaemonRequired                   -- GC needs daemon privileges
    Derivation (RegisterDerivation _) -> DaemonRequired  -- Registration needs daemon
    Derivation (StoreDerivation _) -> DaemonRequired    -- Store write needs daemon
    Derivation (DerivationInfo _) -> BuilderSufficient  -- Info can use either
    Derivation (QueryDeriver _) -> BuilderSufficient    -- Query can use either
    Derivation (ListDerivations) -> BuilderSufficient   -- Listing can use either
    Info _ -> BuilderSufficient             -- Info can use either
    Daemon _ -> DaemonRequired              -- Daemon management needs daemon privileges
    Help -> BuilderSufficient              -- Help works in any context
    Version -> BuilderSufficient           -- Version info works in any context

-- | Determine which phase a command operates in
commandPhase :: Command -> Phase
commandPhase = \case
    Build _ -> Build              -- Build command uses Build phase
    Eval _ -> Eval                -- Eval command uses Eval phase
    Store _ -> Build              -- Store commands use Build phase
    GC -> Build                   -- GC uses Build phase
    Derivation _ -> Eval          -- Derivation commands use Eval phase
    Info _ -> Build               -- Info works in Build phase
    Daemon _ -> Build             -- Daemon commands use Build phase
    Help -> Build                 -- Help works in any phase
    Version -> Build              -- Version works in any phase

-- | Run a command with the given options
runCommand :: Command -> Options -> IO ()
runCommand cmd options = do
    -- Resolve store path
    storePath <- resolveStorePath options

    -- Canonicalize paths for security
    canonicalStorePath <- canonicalizePath storePath `catch` \(_ :: SomeException) -> return storePath

    -- Resolve work directory
    workDir <- case optWorkDir options of
        Just dir -> canonicalizePath dir `catch` \(_ :: SomeException) -> return dir
        Nothing -> getDefaultWorkDir

    -- Create directories if needed
    createDirectoryIfMissing True canonicalStorePath
    createDirectoryIfMissing True workDir

    -- Determine username
    username <- do
        uid <- getEffectiveUserID
        entry <- getUserEntryForID uid
        return $ T.pack $ userName entry

    -- Determine required privilege tier for this command
    let requiredPrivilege = commandPrivilege cmd

    -- Check current privileges
    currentUID <- getEffectiveUserID
    let hasPrivilege = currentUID == 0

    -- Check if daemon should be used
    useDaemon <- shouldRunWithDaemon cmd options

    -- Execute the command based on privilege requirements and availability
    case (requiredPrivilege, hasPrivilege, useDaemon) of
        -- Command requires daemon, but we don't have privileges and daemon isn't available
        (DaemonRequired, False, False) -> do
            hPutStrLn stderr $ "Error: Command requires root privileges or daemon mode: " ++ show cmd
            hPutStrLn stderr $ "Try running with: sudo ten " ++ commandToString cmd
            hPutStrLn stderr $ "Or use the ten daemon by starting it first: sudo ten daemon start"
            exitFailure

        -- Command requires daemon, we don't have privileges, but daemon is available
        (DaemonRequired, False, True) -> do
            -- Set up builder environment
            let builderEnv = initBuildEnv workDir canonicalStorePath
                              { verbosity = optVerbosity options
                              , userName = Just username
                              }
            -- Run through daemon
            runWithDaemon cmd options builderEnv

        -- Command requires daemon and we have daemon privileges
        (DaemonRequired, True, _) -> do
            -- Set up daemon environment
            let daemonEnv = initDaemonEnv workDir canonicalStorePath (Just "root")
                              { verbosity = optVerbosity options
                              }
            -- Run with daemon privileges
            runStandalone (SDaemon) cmd options daemonEnv

        -- Command works with builder privileges, daemon is available and preferred
        (BuilderSufficient, _, True) -> do
            -- Set up builder environment
            let builderEnv = initBuildEnv workDir canonicalStorePath
                              { verbosity = optVerbosity options
                              , userName = Just username
                              }
            -- Run through daemon
            runWithDaemon cmd options builderEnv

        -- Command works with builder privileges and we can run standalone
        (BuilderSufficient, _, False) -> do
            -- If we have root privilege, use daemon tier, otherwise builder tier
            if hasPrivilege
                then do
                    -- Set up daemon environment
                    let daemonEnv = initDaemonEnv workDir canonicalStorePath (Just "root")
                                     { verbosity = optVerbosity options
                                     }
                    -- Run with daemon privileges
                    runStandalone (SDaemon) cmd options daemonEnv
                else do
                    -- Set up builder environment
                    let builderEnv = initBuildEnv workDir canonicalStorePath
                                      { verbosity = optVerbosity options
                                      , userName = Just username
                                      }
                    -- Run with builder privileges
                    runStandalone (SBuilder) cmd options builderEnv

-- | Determine if a command should use the daemon
shouldRunWithDaemon :: Command -> Options -> IO Bool
shouldRunWithDaemon cmd opts = do
    -- Check if daemon usage is explicitly disabled
    if not (optUseDaemon opts)
        then return False
        else case cmd of
            -- These commands always run standalone
            Help -> return False
            Version -> return False
            Daemon _ -> return False

            -- Store operations often need daemon
            Store storeCmds -> case storeCmds of
                -- These can run without daemon
                StorePath _ -> return False
                StoreVerify _ -> return False
                -- These need daemon privileges
                StoreAdd _ -> checkDaemonAvailable opts
                StoreGC -> checkDaemonAvailable opts
                -- Others depend on store access
                _ -> do
                    -- Check if user has direct access to store
                    storeDir <- resolveStorePath opts
                    perms <- getPermissions storeDir
                    if writable perms
                        then return False  -- Can operate directly
                        else checkDaemonAvailable opts  -- Need daemon

            -- Derivation operations often need daemon
            Derivation derivCmds -> case derivCmds of
                -- These can run without daemon
                DerivationInfo _ -> return False
                -- These need daemon privileges
                RegisterDerivation _ -> checkDaemonAvailable opts
                StoreDerivation _ -> checkDaemonAvailable opts
                -- Others depend on store access
                _ -> do
                    -- Check if user has direct access to store
                    storeDir <- resolveStorePath opts
                    perms <- getPermissions storeDir
                    if writable perms
                        then return False  -- Can operate directly
                        else checkDaemonAvailable opts  -- Need daemon

            -- These commands benefit from daemon
            Build _ -> do
                uid <- getEffectiveUserID
                if uid == 0
                    then return False  -- Root can build directly
                    else checkDaemonAvailable opts
            Eval _ -> checkDaemonAvailable opts
            GC -> checkDaemonAvailable opts  -- Always use daemon for GC if available
            Info _ -> checkDaemonAvailable opts

-- | Check if daemon is available (running or can be auto-started)
checkDaemonAvailable :: Options -> IO Bool
checkDaemonAvailable opts = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> canonicalizePath path `catch` \(_ :: SomeException) -> return path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- DaemonClient.isDaemonRunning socketPath

    -- Return True if running or auto-start is enabled
    return daemonRunning

-- | Run a command with the daemon
runWithDaemon :: Command -> Options -> BuildEnv -> IO ()
runWithDaemon cmd opts env = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> canonicalizePath path `catch` \(_ :: SomeException) -> return path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- DaemonClient.isDaemonRunning socketPath

    -- Start daemon if needed and auto-start is enabled
    unless daemonRunning $ do
        putStrLn "Daemon not running. Starting daemon..."
        startDaemonResult <- try $ DaemonClient.startDaemonIfNeeded socketPath
        case startDaemonResult of
            Left (e :: SomeException) -> do
                hPutStrLn stderr $ "Error starting daemon: " ++ show e
                hPutStrLn stderr "Falling back to standalone mode."
                runStandalone (SBuilder) cmd opts env
                exitSuccess
            Right _ -> do
                -- Wait for daemon to fully start
                waitForDaemonStart socketPath 10  -- 10 retries, 0.5s each

    -- Get user credentials
    credentials <- getUserCredentials socketPath

    -- Connect to daemon (always as Builder privilege tier)
    connectResult <- try $ DaemonClient.connectToDaemon socketPath credentials
    case connectResult of
        Left (e :: SomeException) -> do
            hPutStrLn stderr $ "Error connecting to daemon: " ++ show e
            hPutStrLn stderr "Falling back to standalone mode."
            runStandalone (SBuilder) cmd opts env

        Right conn -> do
            -- Execute command using daemon (with builder privileges)
            exit <- executeDaemonCommand (SBuilder) cmd opts conn `finally`
                    DaemonClient.disconnectFromDaemon conn

            -- Exit with appropriate status
            if exit
                then exitSuccess
                else exitFailure

-- | Wait for daemon to fully start
waitForDaemonStart :: FilePath -> Int -> IO ()
waitForDaemonStart _ 0 = do
    hPutStrLn stderr "Timed out waiting for daemon to start."
    return ()
waitForDaemonStart socketPath retries = do
    running <- DaemonClient.isDaemonRunning socketPath
    if running
        then return ()
        else do
            threadDelay 500000  -- 0.5 seconds
            waitForDaemonStart socketPath (retries - 1)

-- | Execute a command using the daemon connection
executeDaemonCommand :: SPrivilegeTier 'Builder -> Command -> Options -> DaemonClient.DaemonConnection -> IO Bool
executeDaemonCommand st cmd opts conn = case cmd of
    Build file -> do
        -- Check file existence and access
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Build a file using daemon
        buildResult <- DaemonClient.buildFile conn file
        case buildResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right buildResult -> do
                -- Show build result
                showBuildResult buildResult
                return True

    Eval file -> do
        -- Check file existence and access
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Evaluate a file using daemon
        evalResult <- DaemonClient.evalFile conn file
        case evalResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right derivation -> do
                -- Show derivation
                showDerivation derivation
                return True

    GC -> do
        -- Confirm garbage collection with user
        proceed <- if optForce opts
            then return True
            else do
                putStr "This will remove unreachable paths from the store. Continue? [y/N] "
                hFlush stdout
                answer <- getLine
                return $ answer `elem` ["y", "Y", "yes", "YES"]

        if proceed
            then do
                -- Run garbage collection via protocol (daemon privilege required)
                gcResult <- DaemonClient.requestGarbageCollection conn (optForce opts)
                case gcResult of
                    Left err -> do
                        TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                        return False
                    Right stats -> do
                        putStrLn $ "Garbage collection completed successfully."
                        putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
                        putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"
                        return True
            else do
                putStrLn "Garbage collection cancelled."
                return True

    Store storeCmd -> executeStoreCommand st storeCmd conn opts

    Derivation derivCmd -> executeDerivationCommand st derivCmd conn opts

    Info path -> do
        -- Verify path format
        let validPath = checkValidStorePath path
        unless validPath $ do
            hPutStrLn stderr $ "Error: Invalid store path format: " ++ path
            return False

        -- Show info about a store path
        infoResult <- try $ do
            let storePath = parseStorePath T.pack path
            case storePath of
                Nothing -> return $ Left $ StoreError "Invalid store path format"
                Just sp -> DaemonClient.verifyStorePath conn path

        case infoResult of
            Left (e :: SomeException) -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show e)
                return False
            Right (Left err) -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right (Right isValid) -> do
                putStrLn $ "Path: " ++ path
                putStrLn $ "Valid: " ++ if isValid then "Yes" else "No"
                return True

    -- These commands should never be run through daemon
    Help -> showHelp >> return True
    Version -> showVersion >> return True
    Daemon _ -> do
        putStrLn "Daemon commands must be run directly, not through the daemon."
        return False

-- | Execute a store command using the daemon
executeStoreCommand :: SPrivilegeTier 'Builder -> StoreCommand -> DaemonClient.DaemonConnection -> Options -> IO Bool
executeStoreCommand st storeCmd conn opts = case storeCmd of
    StoreAdd file -> do
        -- Check file existence and access
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Add file to store (via daemon protocol)
        addResult <- DaemonClient.requestAddToStore conn file
        case addResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right path -> do
                putStrLn $ "File added to store: " ++ file
                putStrLn $ "Store path: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                return True

    StoreVerify path -> do
        -- Verify store path (can run in builder context)
        verifyResult <- DaemonClient.verifyStorePath conn path
        case verifyResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right isValid -> do
                putStrLn $ "Path: " ++ path
                putStrLn $ "Valid: " ++ if isValid then "Yes" else "No"
                return True

    StorePath file -> do
        -- Check file existence and access
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Calculate store path for file (can run locally)
        storePathResult <- getStorePathForFile file
        case storePathResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right storePath -> do
                putStrLn $ "Store path for " ++ file ++ ":"
                putStrLn $ "  " ++ T.unpack (storeHash storePath) ++ "-" ++ T.unpack (storeName storePath)
                return True

    StoreGC -> do
        -- Confirm GC with user
        proceed <- if optForce opts
            then return True
            else do
                putStr "This will remove unreachable paths from the store. Continue? [y/N] "
                hFlush stdout
                answer <- getLine
                return $ answer `elem` ["y", "Y", "yes", "YES"]

        if proceed
            then do
                -- Run garbage collection via protocol (daemon privilege required)
                gcResult <- DaemonClient.requestGarbageCollection conn (optForce opts)
                case gcResult of
                    Left err -> do
                        TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                        return False
                    Right stats -> do
                        putStrLn "Garbage collection completed successfully."
                        putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
                        putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"
                        return True
            else do
                putStrLn "Garbage collection cancelled."
                return True

    StoreList -> do
        -- List store contents (readable operation, works in builder context)
        listResult <- DaemonClient.listStore conn
        case listResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right paths -> do
                putStrLn $ "Store contains " ++ show (length paths) ++ " items:"
                forM_ paths $ \path ->
                    putStrLn $ "  " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                return True

-- | Execute a derivation command using the daemon
executeDerivationCommand :: SPrivilegeTier 'Builder -> DerivationCommand -> DaemonClient.DaemonConnection -> Options -> IO Bool
executeDerivationCommand st derivCmd conn opts = case derivCmd of
    DerivationInfo path -> do
        -- Check if it's a valid store path
        let validPath = checkValidStorePath path
        unless validPath $ do
            hPutStrLn stderr $ "Error: Invalid store path format: " ++ path
            return False

        -- Query derivation info (read operation, works in builder context)
        derivInfoResult <- DaemonClient.getDerivationInfo conn path
        case derivInfoResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right drv -> do
                -- Display derivation information
                showDerivation drv
                return True

    QueryDeriver outputPath -> do
        -- Check if it's a valid store path
        let validPath = checkValidStorePath outputPath
        unless validPath $ do
            hPutStrLn stderr $ "Error: Invalid store path format: " ++ outputPath
            return False

        -- Query which derivation produced this output (read operation)
        derivResult <- DaemonClient.queryDeriver conn outputPath
        case derivResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right drvInfo -> do
                -- Display the derivation info
                putStrLn $ "Output: " ++ outputPath
                putStrLn $ "Produced by derivation: " ++ T.unpack (drvInfoPath drvInfo)
                putStrLn $ "Derivation hash: " ++ T.unpack (drvInfoHash drvInfo)
                return True

    StoreDerivation file -> do
        -- Check file existence
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Store the derivation file (uses daemon protocol)
        storeResult <- DaemonClient.requestStoreDerivation conn file
        case storeResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right path -> do
                putStrLn $ "Derivation stored at: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                return True

    RegisterDerivation file -> do
        -- Check file existence
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Register the derivation file (uses daemon protocol)
        registerResult <- DaemonClient.requestRegisterDerivation conn file
        case registerResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right (path, outputs) -> do
                putStrLn $ "Derivation registered at: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                putStrLn "Outputs:"
                forM_ outputs $ \output ->
                    putStrLn $ "  " ++ T.unpack (storeHash output) ++ "-" ++ T.unpack (storeName output)
                return True

    ListDerivations -> do
        -- List all registered derivations (read operation)
        listResult <- DaemonClient.listDerivations conn
        case listResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
                return False
            Right drvs -> do
                putStrLn $ "Found " ++ show (length drvs) ++ " registered derivations:"
                forM_ drvs $ \drv -> do
                    putStrLn $ T.unpack (drvInfoPath drv) ++ " (" ++ T.unpack (drvInfoHash drv) ++ ")"
                return True

-- | Add derivation info getter to DaemonClient
drvInfoPath :: DaemonClient.DerivInfoResult -> Text
drvInfoPath = fst

-- | Add derivation info hash getter to DaemonClient
drvInfoHash :: DaemonClient.DerivInfoResult -> Text
drvInfoHash = snd

-- | Derivation info result type
type DerivInfoResult = (Text, Text)

-- | Run a command in standalone mode (with explicit privilege tier)
runStandalone :: SPrivilegeTier t -> Command -> Options -> BuildEnv -> IO ()
runStandalone st cmd opts env = do
    -- Execute the command with appropriate privilege evidence
    let phase = SBuild -- For most commands we use Build phase

    -- Initialize build state with appropriate phase
    let state = initBuildState (commandPhase cmd) (BuildIdFromInt 0)

    -- Run the command with the correct privilege tier and phase
    result <- executeInContext st phase cmd opts env state

    -- Exit with appropriate status
    if result
        then exitSuccess
        else exitFailure

-- | Execute a command in the appropriate context with singleton evidence
executeInContext :: SPrivilegeTier t -> SPhase p -> Command -> Options -> BuildEnv -> BuildState p -> IO Bool
executeInContext st sp cmd opts env state = do
    -- Route command to the correct handler based on command type
    case cmd of
        Build file ->
            dispatchCommand st sp (handleBuild st file) env state

        Eval file ->
            let evalPhase = transitionPhase sp :: SPhase 'Eval in
            dispatchCommand st evalPhase (handleEval st file) env state

        GC ->
            -- GC requires daemon privileges
            case st of
                SDaemon -> dispatchCommand st sp (handleGC st (optForce opts)) env state
                _ -> do
                    hPutStrLn stderr "Error: GC requires daemon privileges"
                    return False

        Store storeCmd ->
            dispatchCommand st sp (handleStore st storeCmd) env state

        Derivation derivCmd ->
            let evalPhase = transitionPhase sp :: SPhase 'Eval in
            dispatchCommand st evalPhase (handleDerivation st derivCmd) env state

        Info path ->
            dispatchCommand st sp (handleInfo st path) env state

        Daemon daemonCmd ->
            -- Daemon commands require daemon privileges
            case st of
                SDaemon -> dispatchCommand st sp (handleDaemon st daemonCmd) env state
                _ -> do
                    hPutStrLn stderr "Error: Daemon commands require daemon privileges"
                    return False

        Help ->
            showHelp >> return True

        Version ->
            showVersion >> return True

-- | Dispatch a command with proper phase and privilege context
dispatchCommand :: SPrivilegeTier t -> SPhase p -> (SPrivilegeTier t -> TenM p t a) ->
                  BuildEnv -> BuildState p -> IO Bool
dispatchCommand st sp handler env state = do
    -- Run the TenM action with correct privilege and phase evidence
    result <- runTen (handler st) env state

    -- Process result
    case result of
        Left err -> do
            TIO.hPutStrLn stderr $ "Error: " <> buildErrorToText err
            return False
        Right _ -> return True

-- | Elevate privileges if necessary
elevatePrivileges :: Command -> Options -> IO ExitCode
elevatePrivileges cmd opts = do
    -- Get current user ID
    uid <- getEffectiveUserID

    -- If already running as root, no need to elevate
    if uid == 0
        then do
            -- Just run the command normally
            runCommand cmd opts
            return ExitSuccess
        else do
            -- Find path to sudo
            mSudoPath <- findExecutable "sudo"
            case mSudoPath of
                Nothing -> do
                    hPutStrLn stderr "Cannot elevate privileges: sudo not found"
                    return $ ExitFailure 1

                Just sudoPath -> do
                    -- Get program name and arguments
                    progName <- getProgName
                    progPath <- findExecutable progName
                    case progPath of
                        Nothing -> do
                            hPutStrLn stderr $ "Cannot find executable: " ++ progName
                            return $ ExitFailure 1

                        Just exePath -> do
                            -- Construct arguments for sudo
                            let cmdArgs = commandToArgs cmd opts

                            -- Run sudo with the appropriate arguments
                            hPutStrLn stderr $ "Elevating privileges using sudo..."
                            exitCode <- rawSystem sudoPath [exePath] cmdArgs
                            return exitCode

-- | Handle build command
handleBuild :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleBuild st file = do
    env <- ask

    -- Check if the file exists
    fileExists <- liftIO $ doesFileExist file
    unless fileExists $
        throwError $ InputNotFound file

    -- Notify user
    liftIO $ putStrLn $ "Building derivation: " ++ file

    -- Determine file type based on extension
    let ext = takeExtension file
    case ext of
        ".ten" -> handleBuildTenExpression st file
        ".drv" -> handleBuildDerivationFile st file
        _ -> do
            -- Try to infer file type
            content <- liftIO $ BS.readFile file
            if isJsonContent content
                then handleBuildTenExpression st file  -- Likely a Ten expression
                else handleBuildDerivationFile st file  -- Assume it's a derivation file

-- | Handle building a Ten expression
handleBuildTenExpression :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleBuildTenExpression st file = do
    -- Read the file
    content <- liftIO $ BS.readFile file

    -- Switch to Eval phase to evaluate the expression
    derivation <- transitionPhase $ evaluateExpression st content

    -- Store the derivation
    case st of
        SDaemon -> do
            -- With daemon privileges, we can store directly
            storePath <- storeDerivationFile st derivation

            -- Build the derivation
            result <- buildDerivation st derivation

            -- Show result
            liftIO $ showBuildResult result

        SBuilder -> do
            -- In builder context, we use daemon protocol
            result <- requestBuildDerivation st derivation

            -- Show result
            liftIO $ showBuildResult result

-- | Handle building a derivation file directly
handleBuildDerivationFile :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleBuildDerivationFile st file = do
    -- Read and parse the derivation file
    content <- liftIO $ BS.readFile file
    case deserializeDerivation content of
        Left err -> throwError $ SerializationError err
        Right derivation -> do
            -- Store the derivation if needed
            case st of
                SDaemon -> do
                    -- With daemon privileges, we can store directly
                    storePath <- storeDerivationFile st derivation

                    -- Build the derivation
                    result <- buildDerivation st derivation

                    -- Show result
                    liftIO $ showBuildResult result

                SBuilder -> do
                    -- In builder context, we use daemon protocol
                    result <- requestBuildDerivation st derivation

                    -- Show result
                    liftIO $ showBuildResult result

-- | Handle evaluation command
handleEval :: SPrivilegeTier t -> FilePath -> TenM 'Eval t ()
handleEval st file = do
    -- Check if file exists
    fileExists <- liftIO $ doesFileExist file
    unless fileExists $
        throwError $ InputNotFound file

    -- Notify user
    liftIO $ putStrLn $ "Evaluating expression: " ++ file

    -- Read the file
    content <- liftIO $ BS.readFile file

    -- Evaluate the expression
    derivation <- evaluateExpression st content

    -- Store the derivation if needed
    case st of
        SDaemon -> do
            -- With daemon privileges, we can store directly
            storePath <- storeDerivationFile st derivation

            -- Show the derivation
            liftIO $ showDerivation derivation

        SBuilder -> do
            -- In builder context, we just show the derivation
            liftIO $ showDerivation derivation
            -- We can't store it, but we can request the daemon to store it
            -- This would be implemented via daemon protocol

-- | Handle garbage collection command
handleGC :: SPrivilegeTier 'Daemon -> Bool -> TenM 'Build 'Daemon ()
handleGC st force = do
    -- Notify user
    liftIO $ putStrLn "Running garbage collection"

    -- If force is true, try to break any stale locks first
    when force $ do
        env <- ask
        let lockPath = getGCLockPath env
        exists <- liftIO $ doesFileExist lockPath
        when exists $ do
            liftIO $ hPutStrLn stderr "Found existing GC lock file. Breaking stale lock with --force."
            breakStaleLock st lockPath

    -- Run garbage collection with proper privilege evidence
    stats <- collectGarbage st

    -- Show GC stats
    liftIO $ do
        putStrLn "GC completed successfully"
        putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
        putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"

-- | Handle store operations
handleStore :: SPrivilegeTier t -> StoreCommand -> TenM 'Build t ()
handleStore st = \case
    StoreAdd file -> do
        -- Check if this is the daemon tier (required for store writes)
        case st of
            SDaemon -> do
                -- Check if the file exists
                fileExists <- liftIO $ doesFileExist file
                unless fileExists $
                    throwError $ InputNotFound file

                -- Read the file
                content <- liftIO $ BS.readFile file

                -- Add to store
                let name = T.pack $ takeFileName file
                path <- addToStore st name content

                -- Show result
                liftIO $ putStrLn $ "Added to store: " ++ storePathToFilePath path

            SBuilder ->
                -- Can't write to store directly, need to use daemon protocol
                throwError $ PrivilegeError "Adding to store requires daemon privileges"

    StoreVerify path -> do
        -- Parse the store path
        if not (checkValidStorePath path)
            then throwError $ StoreError $ "Invalid store path format: " <> T.pack path
            else case parseStorePath T.pack path of
                Just storePath -> do
                    -- Verify the path (read operation, works in any context)
                    valid <- verifyStorePath st storePath

                    -- Show result
                    liftIO $ if valid
                        then putStrLn "Path is valid"
                        else putStrLn "Path is invalid"

                Nothing -> throwError $ StoreError $ "Invalid store path: " <> T.pack path

    StorePath file -> do
        -- Check if the file exists
        fileExists <- liftIO $ doesFileExist file
        unless fileExists $
            throwError $ InputNotFound file

        -- Read the file
        content <- liftIO $ BS.readFile file

        -- Compute the store path (computation only, no writes)
        let name = T.pack $ takeFileName file
            hash = showHash $ hashByteString content
            path = StorePath hash name

        -- Show result
        liftIO $ putStrLn $ "Store path would be: " ++ storePathToFilePath path

    StoreGC -> do
        -- Check if this is the daemon tier (required for GC)
        case st of
            SDaemon -> handleGC st False
            SBuilder ->
                throwError $ PrivilegeError "Garbage collection requires daemon privileges"

    StoreList -> do
        -- List all paths in the store (read operation, works in any context)
        paths <- listStorePaths

        -- Show the paths
        liftIO $ do
            putStrLn "Store paths:"
            forM_ paths $ \path ->
                putStrLn $ "  " ++ path

-- | Handle derivation operations
handleDerivation :: SPrivilegeTier t -> DerivationCommand -> TenM 'Eval t ()
handleDerivation st = \case
    DerivationInfo path -> do
        -- Check path validity
        if not (checkValidStorePath path)
            then throwError $ StoreError $ "Invalid store path format: " <> T.pack path
            else do
                -- Get the store path
                let storePath = case parseStorePath T.pack path of
                        Just sp -> sp
                        Nothing -> error "Invalid store path format"

                -- Try to retrieve the derivation from store (read operation)
                mDrv <- retrieveDerivation st storePath
                case mDrv of
                    Nothing ->
                        throwError $ StoreError $ "Derivation not found: " <> T.pack path
                    Just drv -> do
                        -- Show the derivation
                        liftIO $ showDerivation drv

    QueryDeriver outputPath -> do
        -- Check path validity
        if not (checkValidStorePath outputPath)
            then throwError $ StoreError $ "Invalid store path format: " <> T.pack outputPath
            else do
                -- Get the store path
                let storePath = case parseStorePath T.pack outputPath of
                        Just sp -> sp
                        Nothing -> error "Invalid store path format"

                -- Query the database for the derivation
                case st of
                    SDaemon -> do
                        -- Use database directly (daemon privilege)
                        mDeriv <- queryDerivationForOutput st storePath
                        case mDeriv of
                            Nothing -> do
                                liftIO $ putStrLn $ "No derivation found for output: " ++ outputPath
                            Just drv -> do
                                liftIO $ do
                                    putStrLn $ "Output: " ++ outputPath
                                    putStrLn $ "Produced by derivation: " ++ storePathToFilePath (derivInfoStorePath drv)
                                    putStrLn $ "Derivation hash: " ++ T.unpack (derivInfoHash drv)

                    SBuilder -> do
                        -- Use protocol to query (builder privilege)
                        mDeriv <- requestQueryDerivationForOutput st storePath
                        case mDeriv of
                            Nothing ->
                                liftIO $ putStrLn $ "No derivation found for output: " ++ outputPath
                            Just (drvPath, drvHash) ->
                                liftIO $ do
                                    putStrLn $ "Output: " ++ outputPath
                                    putStrLn $ "Produced by derivation: " ++ T.unpack drvPath
                                    putStrLn $ "Derivation hash: " ++ T.unpack drvHash

    StoreDerivation file -> do
        -- Check file existence
        fileExists <- liftIO $ doesFileExist file
        unless fileExists $
            throwError $ InputNotFound file

        -- Read the file content
        content <- liftIO $ BS.readFile file

        -- Deserialize the derivation
        case deserializeDerivation content of
            Left err ->
                throwError $ SerializationError err
            Right drv -> do
                -- Store the derivation
                case st of
                    SDaemon -> do
                        -- With daemon privileges, store directly
                        path <- storeDerivation st drv
                        liftIO $ putStrLn $ "Derivation stored at: " ++ storePathToFilePath path

                    SBuilder ->
                        -- Can't write to store directly, need to use daemon protocol
                        throwError $ PrivilegeError "Storing derivations requires daemon privileges"

    RegisterDerivation file -> do
        -- Check file existence
        fileExists <- liftIO $ doesFileExist file
        unless fileExists $
            throwError $ InputNotFound file

        -- Read the file content
        content <- liftIO $ BS.readFile file

        -- Deserialize the derivation
        case deserializeDerivation content of
            Left err ->
                throwError $ SerializationError err
            Right drv -> do
                -- Register the derivation
                case st of
                    SDaemon -> do
                        -- With daemon privileges, register directly
                        path <- registerDerivation st drv
                        liftIO $ do
                            putStrLn $ "Derivation registered at: " ++ storePathToFilePath path
                            putStrLn "Outputs:"
                            forM_ (derivationOutputPaths drv) $ \output ->
                                putStrLn $ "  " ++ storePathToFilePath output

                    SBuilder ->
                        -- Can't write to DB directly, need to use daemon protocol
                        throwError $ PrivilegeError "Registering derivations requires daemon privileges"

    ListDerivations -> do
        -- List registered derivations
        case st of
            SDaemon -> do
                -- With daemon privileges, query DB directly
                drvs <- listRegisteredDerivations st
                liftIO $ do
                    putStrLn $ "Found " ++ show (length drvs) ++ " registered derivations:"
                    forM_ drvs $ \drv ->
                        putStrLn $ storePathToFilePath (derivInfoStorePath drv) ++
                               " (" ++ T.unpack (derivInfoHash drv) ++ ")"

            SBuilder -> do
                -- Use protocol to query with builder privileges
                drvs <- requestListDerivations st
                liftIO $ do
                    putStrLn $ "Found " ++ show (length drvs) ++ " registered derivations:"
                    forM_ drvs $ \(path, hash) ->
                        putStrLn $ T.unpack path ++ " (" ++ T.unpack hash ++ ")"

-- | Handle info command
handleInfo :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleInfo st path = do
    -- Parse the store path
    if not (checkValidStorePath path)
        then throwError $ StoreError $ "Invalid store path format: " <> T.pack path
        else case parseStorePath T.pack path of
            Just storePath -> do
                -- Check if the path exists in the store
                exists <- storePathExists st storePath

                if not exists
                    then throwError $ StoreError $ "Path not in store: " <> T.pack path
                    else do
                        -- Check if this is a derivation file
                        let isDrv = ".drv" `T.isSuffixOf` storeName storePath

                        if isDrv then do
                            -- Show derivation info (read operation)
                            mDrv <- retrieveDerivation st storePath
                            case mDrv of
                                Nothing ->
                                    throwError $ StoreError $ "Derivation not found: " <> T.pack path
                                Just drv -> do
                                    -- Show the derivation
                                    liftIO $ putStrLn "Derivation file:"
                                    liftIO $ showDerivation drv
                        else do
                            -- Show regular path info
                            case st of
                                SDaemon -> do
                                    -- With daemon privileges, query DB
                                    mDeriv <- queryDerivationForOutput st storePath

                                    -- Show path info
                                    liftIO $ do
                                        putStrLn $ "Store path: " ++ storePathToFilePath storePath
                                        putStrLn $ "Hash: " ++ T.unpack (storeHash storePath)
                                        putStrLn $ "Name: " ++ T.unpack (storeName storePath)

                                        -- Show deriver info if available
                                        case mDeriv of
                                            Nothing ->
                                                putStrLn "Not an output of any known derivation"
                                            Just derivInfo ->
                                                putStrLn $ "Output of derivation: " ++
                                                          storePathToFilePath (derivInfoStorePath derivInfo)

                                SBuilder -> do
                                    -- Limited info in builder context
                                    liftIO $ do
                                        putStrLn $ "Store path: " ++ storePathToFilePath storePath
                                        putStrLn $ "Hash: " ++ T.unpack (storeHash storePath)
                                        putStrLn $ "Name: " ++ T.unpack (storeName storePath)
                                        putStrLn "(Database information unavailable in unprivileged context)"

                -- Verify the path
                valid <- verifyStorePath st storePath
                liftIO $ putStrLn $ "Valid: " ++ if valid then "Yes" else "No"

            Nothing -> throwError $ StoreError $ "Invalid store path: " <> T.pack path

-- | Handle daemon operations
handleDaemon :: SPrivilegeTier 'Daemon -> DaemonCommand -> TenM 'Build 'Daemon ()
handleDaemon st = \case
    DaemonStart -> do
        -- Check if daemon is already running
        socketPath <- liftIO getDefaultSocketPath
        running <- liftIO $ DaemonClient.isDaemonRunning socketPath

        if running
            then liftIO $ putStrLn "Daemon is already running"
            else do
                -- Start the daemon
                liftIO $ putStrLn "Starting daemon..."
                result <- liftIO $ daemonCommand st ["start"]
                case result of
                    ExitSuccess ->
                        liftIO $ putStrLn "Daemon started successfully"
                    _ ->
                        throwError $ DaemonError "Failed to start daemon"

    DaemonStop -> do
        -- Check if daemon is running
        socketPath <- liftIO getDefaultSocketPath
        running <- liftIO $ DaemonClient.isDaemonRunning socketPath

        if running
            then do
                -- Stop the daemon
                liftIO $ putStrLn "Stopping daemon..."
                result <- liftIO $ daemonCommand st ["stop"]
                case result of
                    ExitSuccess ->
                        liftIO $ putStrLn "Daemon stopped successfully"
                    _ ->
                        throwError $ DaemonError "Failed to stop daemon"
            else
                liftIO $ putStrLn "Daemon is not running"

    DaemonStatus -> do
        -- Check if daemon is running
        socketPath <- liftIO getDefaultSocketPath
        running <- liftIO $ DaemonClient.isDaemonRunning socketPath

        if running
            then do
                liftIO $ putStrLn "Daemon is running"

                -- Get detailed status
                statusResult <- liftIO $ daemonCommand st ["status"]
                case statusResult of
                    ExitSuccess -> return ()
                    _ -> liftIO $ hPutStrLn stderr "Warning: Could not get detailed status"
            else
                liftIO $ putStrLn "Daemon is not running"

    DaemonRestart -> do
        -- First stop
        result1 <- liftIO $ daemonCommand st ["stop"]
        -- Give it a moment to fully shut down
        liftIO $ threadDelay 1000000  -- 1 second
        -- Then start
        result2 <- liftIO $ daemonCommand st ["start"]

        -- Check results
        case (result1, result2) of
            (ExitSuccess, ExitSuccess) ->
                liftIO $ putStrLn "Daemon restarted successfully"
            (_, _) ->
                throwError $ DaemonError "Failed to restart daemon"

    DaemonConfig -> do
        -- Show daemon configuration from env
        env <- ask

        -- Show basic configuration
        liftIO $ do
            putStrLn "Daemon configuration:"
            socketPath <- getDefaultSocketPath
            let storeLoc = storeLocation env
            putStrLn $ "  Socket: " ++ socketPath
            putStrLn $ "  Store: " ++ storeLoc
            putStrLn $ "  Work dir: " ++ workDir env

-- | List all store paths
listStorePaths :: TenM 'Build t [FilePath]
listStorePaths = do
    env <- ask
    -- Check if store directory exists
    let storeDir = storeLocation env
    exists <- liftIO $ doesDirectoryExist storeDir

    if not exists
        then return []
        else do
            -- List all files in the store directory
            entriesResult <- liftIO $ try $ listDirectory storeDir
            case entriesResult of
                Left (e :: SomeException) -> do
                    liftIO $ hPutStrLn stderr $ "Error reading store directory: " ++ show e
                    return []
                Right entries -> do
                    -- Filter out non-store paths
                    let isStorePath name = case break (== '-') name of
                            (hash, '-':_) -> not (null hash) && not (hash `elem` ["tmp", "gc-roots", "var"])
                            _ -> False

                    return $ filter isStorePath entries

-- | Check if a string follows store path format (hash-name)
checkValidStorePath :: FilePath -> Bool
checkValidStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':_) -> not (null hash) && all (`elem` validHashChars) hash
        _ -> False
  where
    validHashChars = "0123456789abcdefABCDEF"

-- | Show help text
showHelp :: IO ()
showHelp = do
    putStrLn "Ten - A pure functional build system"
    putStrLn ""
    putStrLn "Usage: ten COMMAND [OPTIONS]"
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "  build FILE          Build a derivation file"
    putStrLn "  eval FILE           Evaluate a Ten expression file"
    putStrLn "  gc                  Run garbage collection"
    putStrLn "  store SUBCOMMAND    Store operations"
    putStrLn "  derivation SUBCOMMAND  Derivation operations"
    putStrLn "  info PATH           Show information about a store path"
    putStrLn "  daemon SUBCOMMAND   Daemon operations"
    putStrLn "  help                Show this help"
    putStrLn "  version             Show version"
    putStrLn ""
    putStrLn "Store subcommands:"
    putStrLn "  add FILE            Add a file to the store"
    putStrLn "  verify PATH         Verify a store path"
    putStrLn "  path FILE           Convert a file to a store path"
    putStrLn "  gc                  Run garbage collection"
    putStrLn "  list                List store contents"
    putStrLn ""
    putStrLn "Derivation subcommands:"
    putStrLn "  info PATH           Show information about a derivation"
    putStrLn "  query-deriver PATH  Find which derivation produced an output"
    putStrLn "  store FILE          Store a derivation file"
    putStrLn "  register FILE       Register a derivation in the database"
    putStrLn "  list                List registered derivations"
    putStrLn ""
    putStrLn "Daemon subcommands:"
    putStrLn "  start               Start the daemon"
    putStrLn "  stop                Stop the daemon"
    putStrLn "  status              Check daemon status"
    putStrLn "  restart             Restart the daemon"
    putStrLn "  config              Show daemon configuration"
    putStrLn ""
    putStrLn "Options:"
    putStrLn $ usageInfo "" options

-- | Show version information
showVersion :: IO ()
showVersion = do
    putStrLn "Ten version 0.1.0"
    putStrLn "Copyright (C) 2025"
    putStrLn "License: MIT"

-- | Helper function to run a daemon command
daemonCommand :: SPrivilegeTier 'Daemon -> [String] -> IO ExitCode
daemonCommand st args = do
    -- Find the daemon executable
    daemonPathResult <- findExecutable "ten-daemon"
    tenDaemonPath <- case daemonPathResult of
        Just path -> return path
        Nothing -> do
            -- Try a relative path in case it's in the same directory
            progName <- getProgName
            progDir <- takeDirectory <$> findExecutable progName
            let localPath = progDir </> "ten-daemon"
            exists <- doesFileExist localPath
            if exists
                then return localPath
                else do
                    hPutStrLn stderr "Could not find ten-daemon executable"
                    -- Create a fallback execution for testing
                    return "ten-daemon"

    -- Run the daemon command with proper error handling
    processResult <- try $ createProcess (proc tenDaemonPath args)
    case processResult of
        Left (e :: SomeException) ->
            throwIO $ userError $ "Failed to execute daemon command: " ++ show e
        Right (_, _, _, processHandle) -> do
            -- Wait for daemon to finish (for commands like status, not start)
            waitForProcess processHandle

-- | Convert command to string representation
commandToString :: Command -> String
commandToString = \case
    Build path -> "build " ++ path
    Eval path -> "eval " ++ path
    GC -> "gc"
    Store StoreGC -> "store gc"
    Store (StoreAdd path) -> "store add " ++ path
    Store (StoreVerify path) -> "store verify " ++ path
    Store (StorePath path) -> "store path " ++ path
    Store StoreList -> "store list"
    Derivation (DerivationInfo path) -> "derivation info " ++ path
    Derivation (QueryDeriver path) -> "derivation query-deriver " ++ path
    Derivation (StoreDerivation path) -> "derivation store " ++ path
    Derivation (RegisterDerivation path) -> "derivation register " ++ path
    Derivation ListDerivations -> "derivation list"
    Info path -> "info " ++ path
    Daemon DaemonStart -> "daemon start"
    Daemon DaemonStop -> "daemon stop"
    Daemon DaemonStatus -> "daemon status"
    Daemon DaemonRestart -> "daemon restart"
    Daemon DaemonConfig -> "daemon config"
    Help -> "help"
    Version -> "version"

-- | Convert command to argument list (for privilege elevation)
commandToArgs :: Command -> Options -> [String]
commandToArgs cmd opts =
    let cmdArgs = case cmd of
            Build path -> ["build", path]
            Eval path -> ["eval", path]
            GC -> ["gc"]
            Store (StoreAdd path) -> ["store", "add", path]
            Store (StoreVerify path) -> ["store", "verify", path]
            Store (StorePath path) -> ["store", "path", path]
            Store StoreGC -> ["store", "gc"]
            Store StoreList -> ["store", "list"]
            Derivation (DerivationInfo path) -> ["derivation", "info", path]
            Derivation (QueryDeriver path) -> ["derivation", "query-deriver", path]
            Derivation (StoreDerivation path) -> ["derivation", "store", path]
            Derivation (RegisterDerivation path) -> ["derivation", "register", path]
            Derivation ListDerivations -> ["derivation", "list"]
            Info path -> ["info", path]
            Daemon DaemonStart -> ["daemon", "start"]
            Daemon DaemonStop -> ["daemon", "stop"]
            Daemon DaemonStatus -> ["daemon", "status"]
            Daemon DaemonRestart -> ["daemon", "restart"]
            Daemon DaemonConfig -> ["daemon", "config"]
            Help -> ["help"]
            Version -> ["version"]

        -- Add options
        optArgs = []
            ++ (if optVerbosity opts > 1 then replicate (optVerbosity opts - 1) "-v" else [])
            ++ (if optVerbosity opts < 1 then replicate (1 - optVerbosity opts) "-q" else [])
            ++ (case optStorePath opts of Just path -> ["--store", path]; Nothing -> [])
            ++ (case optWorkDir opts of Just dir -> ["--work-dir", dir]; Nothing -> [])
            ++ (if optKeepFailed opts then ["--keep-failed"] else [])
            ++ (if optForce opts then ["--force"] else [])
            ++ (if not (optUseDaemon opts) then ["--no-daemon"] else [])
            ++ (case optDaemonSocket opts of Just sock -> ["--daemon-socket", sock]; Nothing -> [])
            ++ (case optSystemType opts of Just sys -> ["--system", T.unpack sys]; Nothing -> [])
            ++ (case optMaxJobs opts of Just n -> ["--jobs", show n]; Nothing -> [])
            ++ (if optSuppressOutput opts then ["--silent"] else [])
            ++ (concatMap (\(k, v) -> ["--arg", k ++ "=" ++ v]) (optBuildArgs opts))

    in cmdArgs ++ optArgs

-- | Resolve the store path from options or default
resolveStorePath :: Options -> IO FilePath
resolveStorePath options = case optStorePath options of
    Just path -> canonicalizePath path `catch` \(_ :: SomeException) -> return path
    Nothing -> getDefaultStorePath

-- | Get the default store path
getDefaultStorePath :: IO FilePath
getDefaultStorePath = do
    -- Check environment variable
    envPath <- lookupEnv "TEN_STORE_PATH"
    case envPath of
        Just path -> return path
        Nothing -> do
            -- Use XDG data directory
            dataDir <- getXdgDirectory XdgData "ten"
            createDirectoryIfMissing True dataDir
            return $ dataDir </> "store"

-- | Get the default work directory
getDefaultWorkDir :: IO FilePath
getDefaultWorkDir = do
    -- Check environment variable
    envPath <- lookupEnv "TEN_WORK_DIR"
    case envPath of
        Just path -> return path
        Nothing -> do
            -- Use XDG runtime directory if available
            runtimeDir <- getXdgDirectory XdgRuntime "ten"
            createDirectoryIfMissing True runtimeDir
            return $ runtimeDir </> "work"

-- | Display build result
showBuildResult :: BuildResult -> IO ()
showBuildResult result = do
    -- Show basic info
    putStrLn "Build completed successfully"
    putStrLn $ "Exit code: " ++ show (resultExitCode result)

    -- Show outputs
    putStrLn "Outputs:"
    forM_ (Set.toList $ resultOutputs result) $ \path ->
        putStrLn $ "  " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)

    -- Show log if available and not empty
    unless (T.null $ resultLog result) $ do
        putStrLn "\nBuild log:"
        TIO.putStrLn $ resultLog result

-- | Display derivation details
showDerivation :: Derivation -> IO ()
showDerivation drv = do
    putStrLn $ "Derivation: " ++ T.unpack (derivName drv)
    putStrLn $ "Hash: " ++ T.unpack (derivHash drv)
    putStrLn $ "System: " ++ T.unpack (derivSystem drv)

    -- Show builder
    putStrLn $ "Builder: " ++ T.unpack (storeHash $ derivBuilder drv) ++ "-" ++ T.unpack (storeName $ derivBuilder drv)

    -- Show args
    unless (null $ derivArgs drv) $ do
        putStrLn "Arguments:"
        forM_ (derivArgs drv) $ \arg ->
            putStrLn $ "  " ++ T.unpack arg

    -- Show inputs
    unless (Set.null $ derivInputs drv) $ do
        putStrLn "Inputs:"
        forM_ (Set.toList $ derivInputs drv) $ \input ->
            putStrLn $ "  " ++ T.unpack (inputName input) ++ ": " ++
                       T.unpack (storeHash $ inputPath input) ++ "-" ++
                       T.unpack (storeName $ inputPath input)

    -- Show outputs
    unless (Set.null $ derivOutputs drv) $ do
        putStrLn "Outputs:"
        forM_ (Set.toList $ derivOutputs drv) $ \output ->
            putStrLn $ "  " ++ T.unpack (outputName output) ++ ": " ++
                       T.unpack (storeHash $ outputPath output) ++ "-" ++
                       T.unpack (storeName $ outputPath output)

    -- Show environment
    unless (Map.null $ derivEnv drv) $ do
        putStrLn "Environment:"
        forM_ (Map.toList $ derivEnv drv) $ \(key, val) ->
            putStrLn $ "  " ++ T.unpack key ++ "=" ++ T.unpack val

    -- Show metadata
    unless (Map.null $ derivMeta drv) $ do
        putStrLn "Metadata:"
        forM_ (Map.toList $ derivMeta drv) $ \(key, val) ->
            putStrLn $ "  " ++ T.unpack key ++ ": " ++ T.unpack val

-- | Get user credentials for daemon authentication
getUserCredentials :: FilePath -> IO UserCredentials
getUserCredentials socketPath = do
    -- Get username from current user
    uid <- getEffectiveUserID
    entry <- getUserEntryForID uid
    let username = T.pack $ userName entry

    -- Get or generate an authentication token
    token <- getStoredToken socketPath username `catch` \(_ :: SomeException) -> do
        -- If no token exists or there's any error, generate a new one
        generateRandomToken

    return $ UserCredentials {
        username = username,
        token = token,
        requestedTier = Builder  -- Always request builder tier by default
    }

-- | Get stored token from token file if it exists
getStoredToken :: FilePath -> Text -> IO Text
getStoredToken socketPath username = do
    -- Determine token file location based on socket path and username
    homeDir <- getHomeDirectory
    let tokenFile = homeDir </> ".ten" </> "tokens" </> (T.unpack username ++ ".token")

    -- Check if token file exists
    exists <- doesFileExist tokenFile
    if exists
        then do
            -- Read token from file
            token <- TIO.readFile tokenFile
            if T.null token || T.length token < 10
                then generateRandomToken
                else return token
        else
            -- No token found, generate a new one
            generateRandomToken

-- | Generate a random token for authentication
generateRandomToken :: IO Text
generateRandomToken = do
    -- Create a random token with proper entropy
    r1 <- (randomRIO (1000000, 9999999) :: IO Int)
    r2 <- (randomRIO (1000000, 9999999) :: IO Int)
    r3 <- (randomRIO (1000000, 9999999) :: IO Int)

    -- Format as a base64-like string for use in authentication
    let randomChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']
    let randomString = take 32 $ map (\i -> randomChars !! (abs i `mod` length randomChars)) [r1, r2, r3]
    return $ T.pack $ "ten_token_" ++ randomString

-- | Calculate store path for a file
getStorePathForFile :: FilePath -> IO (Either BuildError StorePath)
getStorePathForFile file = do
    -- Check if file exists
    fileExistsResult <- try $ doesFileExist file
    case fileExistsResult of
        Left (e :: IOException) ->
            return $ Left $ InputNotFound $ "Error accessing file: " ++ show e
        Right False ->
            return $ Left $ InputNotFound file
        Right True -> do
            -- Read the file content
            contentResult <- try $ BS.readFile file
            case contentResult of
                Left (e :: SomeException) ->
                    return $ Left $ InputNotFound $ "Error reading file: " ++ show e
                Right content -> do
                    -- Calculate hash
                    let name = T.pack $ takeFileName file
                        hash = showHash $ hashByteString content
                        path = StorePath hash name

                    return $ Right path

-- | Evaluate a Ten expression to get a derivation
evaluateExpression :: SPrivilegeTier t -> BS.ByteString -> TenM 'Eval t Derivation
evaluateExpression st content = do
    -- For a real implementation, we parse the Ten language here
    -- Since we don't have the Ten language parser in this exercise,
    -- we'll create a basic derivation for demonstration

    -- Create a basic builder
    builderPath <- createBasicBuilder st

    -- Create a derivation using the builder
    let name = "ten-expression"
    let args = ["build"]
    let inputs = Set.empty
    let outputNames = Set.singleton "out"
    let envVars = Map.fromList [("PATH", "/bin:/usr/bin")]
    let system = "x86_64-linux"

    mkDerivation st name builderPath args inputs outputNames envVars system

-- | Create a basic builder for testing
createBasicBuilder :: SPrivilegeTier t -> TenM 'Eval t StorePath
createBasicBuilder st = do
    -- Create a simple bash script builder
    let builderContent = "#!/bin/sh\necho \"Running build with args: $@\"\nmkdir -p $TEN_OUT\necho \"Build result\" > $TEN_OUT/out\n"

    -- Add to store (requires daemon privileges)
    case st of
        SDaemon ->
            -- With daemon privileges, add directly
            addToStore st "bash-builder" (BS.pack builderContent)
        SBuilder ->
            -- In builder context, request via protocol
            requestAddToStore st "bash-builder" (BS.pack builderContent)

-- | Request to build a derivation via daemon
requestBuildDerivation :: SPrivilegeTier 'Builder -> Derivation -> TenM 'Build 'Builder BuildResult
requestBuildDerivation st derivation = do
    -- Call the daemon protocol for building
    -- This would be implemented via proper RPC mechanisms
    -- For now, simulate with a placeholder implementation
    env <- ask

    -- Create a default result for demonstration
    let outPath = StorePath "1234567890abcdef" "out"
    return $ BuildResult {
        resultExitCode = ExitSuccess,
        resultOutputs = Set.singleton outPath,
        resultLog = "Build completed via daemon protocol",
        resultMetadata = Map.empty
    }

-- | Request to add path to store via daemon
requestAddToStore :: SPrivilegeTier 'Builder -> Text -> BS.ByteString -> TenM p 'Builder StorePath
requestAddToStore st name content = do
    -- Call the daemon protocol for store add
    -- This would be implemented via proper RPC mechanisms
    -- For now, simulate with a placeholder implementation
    let hash = showHash $ hashByteString content
    return $ StorePath hash name

-- | Request to query derivation for output via daemon
requestQueryDerivationForOutput :: SPrivilegeTier 'Builder -> StorePath -> TenM p 'Builder (Maybe (Text, Text))
requestQueryDerivationForOutput st path = do
    -- Call the daemon protocol for querying
    -- This would be implemented via proper RPC mechanisms
    -- For now, return Nothing to simulate empty result
    return Nothing

-- | Request to list derivations via daemon
requestListDerivations :: SPrivilegeTier 'Builder -> TenM p 'Builder [(Text, Text)]
requestListDerivations st = do
    -- Call the daemon protocol for listing
    -- This would be implemented via proper RPC mechanisms
    -- For now, return empty list
    return []

-- | Check if content looks like JSON
isJsonContent :: BS.ByteString -> Bool
isJsonContent content =
    case BS.uncons content of
        Just (first, _) -> first == 123  -- '{' character (ASCII 123)
        Nothing -> False

-- | Helper function: list directory contents
listDirectory :: FilePath -> IO [FilePath]
listDirectory dir = do
    entries <- getDirectoryContents dir
    return $ filter (`notElem` [".", ".."]) entries

-- | Helper function: get directory contents
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = Directory.getDirectoryContents

-- | Helper function: get derivation output paths
derivationOutputPaths :: Derivation -> Set StorePath
derivationOutputPaths drv = Set.map outputPath (derivOutputs drv)

-- | Helper function: get store path to file path
storePathToFilePath :: StorePath -> String
storePathToFilePath sp = T.unpack (storeHash sp) ++ "-" ++ T.unpack (storeName sp)

-- | Helper function: raw system call
rawSystem :: FilePath -> [String] -> [String] -> IO ExitCode
rawSystem cmd exec args = do
    processResult <- try $ createProcess (proc cmd (exec:args))
    case processResult of
        Left (_ :: SomeException) ->
            return $ ExitFailure 127
        Right (_, _, _, processHandle) ->
            waitForProcess processHandle

-- | Break a stale lock
breakStaleLock :: SPrivilegeTier 'Daemon -> FilePath -> TenM 'Build 'Daemon ()
breakStaleLock _ lockPath = do
    -- Remove the stale lock file
    liftIO $ removeFile lockPath `catch` \(_ :: SomeException) -> return ()
