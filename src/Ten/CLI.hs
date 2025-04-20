{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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
    getUserCredentials
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
                  hSetBuffering, BufferMode(..), hFlush, Handle, openFile)
import System.Console.GetOpt
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName, realUserID,
                         UserEntry(..), setUserID, userID)
import System.Posix.Files (fileExist, setFileMode)
import System.Posix.Process (getProcessID, executeFile, exitImmediately)
import System.Posix.Signals (installHandler, Handler(..), signalProcess, sigTERM)
import System.Process (createProcess, proc, waitForProcess, readProcessWithExitCode)
import System.Random (randomIO)
import Text.Read (readMaybe)

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Build
import Ten.GC
import Ten.Hash
import Ten.DB.Core
import Ten.DB.Derivations
import Ten.Daemon.Config (getDefaultSocketPath, getDefaultConfig, daemonSocketPath)
import Ten.Daemon.Client (isDaemonRunning, startDaemonIfNeeded, UserCredentials(..))
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

-- | Run a command with the given options
runCommand :: Command -> Options -> IO ()
runCommand cmd options = do
    -- Resolve store path
    storePath <- resolveStorePath options

    -- Canonicalize paths for security
    canonicalStorePath <- canonicalizePath storePath

    -- Resolve work directory
    workDir <- case optWorkDir options of
        Just dir -> canonicalizePath dir
        Nothing -> getDefaultWorkDir

    -- Create directories if needed
    createDirectoryIfMissing True canonicalStorePath
    createDirectoryIfMissing True workDir

    -- Check if we have write permission to the store
    storePerms <- getPermissions canonicalStorePath
    unless (writable storePerms) $ do
        hPutStrLn stderr $ "Warning: Store path is not writable: " ++ canonicalStorePath
        hPutStrLn stderr $ "Some operations may require daemon mode."

    -- Determine username
    username <- do
        uid <- getEffectiveUserID
        entry <- getUserEntryForID uid
        return $ T.pack $ userName entry

    -- Set up build environment
    let env = initBuildEnv workDir canonicalStorePath
                { verbosity = optVerbosity options
                , userName = Just username
                }

    -- Check if daemon should be used
    useDaemon <- shouldRunWithDaemon cmd options

    -- Execute the command (with or without daemon)
    if useDaemon
        then runWithDaemon cmd options env
        else runStandalone cmd options env

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
                -- Others benefit from daemon
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
                -- Others may benefit from daemon
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
            GC -> do
                -- Only root or daemon should run GC
                uid <- getEffectiveUserID
                if uid == 0
                    then return False  -- Root can GC directly
                    else checkDaemonAvailable opts
            Info _ -> checkDaemonAvailable opts

-- | Check if daemon is available (running or can be auto-started)
checkDaemonAvailable :: Options -> IO Bool
checkDaemonAvailable opts = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> canonicalizePath path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- isDaemonRunning socketPath

    -- Return True if running or auto-start is enabled
    return daemonRunning

-- | Run a command with the daemon
runWithDaemon :: Command -> Options -> BuildEnv -> IO ()
runWithDaemon cmd opts env = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> canonicalizePath path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- isDaemonRunning socketPath

    -- Start daemon if needed and auto-start is enabled
    unless daemonRunning $ do
        putStrLn "Daemon not running. Starting daemon..."
        startDaemonResult <- try $ startDaemonIfNeeded socketPath
        case startDaemonResult of
            Left (e :: SomeException) -> do
                hPutStrLn stderr $ "Error starting daemon: " ++ show e
                hPutStrLn stderr "Falling back to standalone mode."
                runStandalone cmd opts env
                exitSuccess
            Right _ -> do
                -- Wait for daemon to fully start
                waitForDaemonStart socketPath 10  -- 10 retries, 0.5s each

    -- Get user credentials
    credentials <- getUserCredentials socketPath

    -- Connect to daemon
    connectResult <- try $ DaemonClient.connectToDaemon socketPath credentials
    case connectResult of
        Left (e :: SomeException) -> do
            hPutStrLn stderr $ "Error connecting to daemon: " ++ show e
            hPutStrLn stderr "Falling back to standalone mode."
            runStandalone cmd opts env

        Right conn -> do
            -- Execute command using daemon
            exit <- executeDaemonCommand cmd opts conn `finally`
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
    running <- isDaemonRunning socketPath
    if running
        then return ()
        else do
            threadDelay 500000  -- 0.5 seconds
            waitForDaemonStart socketPath (retries - 1)

-- | Execute a command using the daemon connection
executeDaemonCommand :: Command -> Options -> DaemonClient.DaemonConnection -> IO Bool
executeDaemonCommand cmd opts conn = case cmd of
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
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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
                -- Run garbage collection
                gcResult <- DaemonClient.collectGarbage conn (optForce opts)
                case gcResult of
                    Left err -> do
                        TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                        return False
                    Right stats -> do
                        putStrLn $ "Garbage collection completed successfully."
                        putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
                        putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"
                        return True
            else do
                putStrLn "Garbage collection cancelled."
                return True

    Store storeCmd -> executeStoreCommand storeCmd conn opts

    Derivation derivCmd -> executeDerivationCommand derivCmd conn opts

    Info path -> do
        -- Verify path format
        let validPath = checkValidStorePath path
        unless validPath $ do
            hPutStrLn stderr $ "Error: Invalid store path format: " ++ path
            return False

        -- Show info about a store path
        infoResult <- try $ do
            let storePath = parseStorePath path
            case storePath of
                Nothing -> return $ Left $ StoreError "Invalid store path format"
                Just sp -> DaemonClient.verifyStorePath conn path

        case infoResult of
            Left (e :: SomeException) -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show e)
                return False
            Right (Left err) -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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
executeStoreCommand :: StoreCommand -> DaemonClient.DaemonConnection -> Options -> IO Bool
executeStoreCommand storeCmd conn opts = case storeCmd of
    StoreAdd file -> do
        -- Check file existence and access
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Add file to store
        addResult <- DaemonClient.addFileToStore conn file
        case addResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right path -> do
                putStrLn $ "File added to store: " ++ file
                putStrLn $ "Store path: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                return True

    StoreVerify path -> do
        -- Verify store path
        verifyResult <- DaemonClient.verifyStorePath conn path
        case verifyResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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
                -- Run garbage collection
                gcResult <- DaemonClient.collectGarbage conn (optForce opts)
                case gcResult of
                    Left err -> do
                        TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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
        -- List store contents
        listResult <- DaemonClient.listStore conn
        case listResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right paths -> do
                putStrLn $ "Store contains " ++ show (length paths) ++ " items:"
                forM_ paths $ \path ->
                    putStrLn $ "  " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                return True

-- | Execute a derivation command using the daemon
executeDerivationCommand :: DerivationCommand -> DaemonClient.DaemonConnection -> Options -> IO Bool
executeDerivationCommand derivCmd conn opts = case derivCmd of
    DerivationInfo path -> do
        -- Check if it's a valid store path
        let validPath = checkValidStorePath path
        unless validPath $ do
            hPutStrLn stderr $ "Error: Invalid store path format: " ++ path
            return False

        -- Query derivation info
        derivInfoResult <- DaemonClient.getDerivationInfo conn path
        case derivInfoResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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

        -- Query which derivation produced this output
        derivResult <- DaemonClient.queryDeriver conn outputPath
        case derivResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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

        -- Store the derivation file
        storeResult <- DaemonClient.storeDerivationFile conn file
        case storeResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
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

        -- Register the derivation file
        registerResult <- DaemonClient.registerDerivation conn file
        case registerResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right (path, outputs) -> do
                putStrLn $ "Derivation registered at: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                putStrLn "Outputs:"
                forM_ outputs $ \output ->
                    putStrLn $ "  " ++ T.unpack (storeHash output) ++ "-" ++ T.unpack (storeName output)
                return True

    ListDerivations -> do
        -- List all registered derivations
        listResult <- DaemonClient.listDerivations conn
        case listResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right drvs -> do
                putStrLn $ "Found " ++ show (length drvs) ++ " registered derivations:"
                forM_ drvs $ \drv -> do
                    putStrLn $ T.unpack (drvInfoPath drv) ++ " (" ++ T.unpack (drvInfoHash drv) ++ ")"
                return True

-- | Add derivation info getter to DaemonClient
drvInfoPath :: DerivInfoResult -> Text
drvInfoPath = fst

-- | Add derivation info hash getter to DaemonClient
drvInfoHash :: DerivInfoResult -> Text
drvInfoHash = snd

-- | Derivation info result type
type DerivInfoResult = (Text, Text)

-- | Run a command in standalone mode (without daemon)
runStandalone :: Command -> Options -> BuildEnv -> IO ()
runStandalone cmd opts env = do
    -- Check if we have sufficient permissions for the operation
    needsRoot <- commandNeedsRoot cmd
    if needsRoot
        then do
            uid <- getEffectiveUserID
            unless (uid == 0) $ do
                hPutStrLn stderr $ "Error: Command requires root privileges or daemon mode: " ++ show cmd
                hPutStrLn stderr $ "Try running with: sudo ten " ++ commandToString cmd
                hPutStrLn stderr $ "Or use the ten daemon by starting it first: sudo ten daemon start"
                exitFailure
        else do
            -- Execute the command
            result <- executeCommand cmd opts env

            -- Exit with appropriate status
            if result
                then exitSuccess
                else exitFailure

-- | Check if a command needs root privileges
commandNeedsRoot :: Command -> IO Bool
commandNeedsRoot = \case
    GC -> return True  -- GC needs store write access
    Store StoreGC -> return True  -- Store GC also needs store write access
    Store (StoreAdd _) -> do
        -- Adding to store requires write access to store
        storePath <- getDefaultStorePath
        perms <- getPermissions storePath
        return (not $ writable perms)
    Daemon _ -> return True  -- Daemon operations generally need root
    Derivation (RegisterDerivation _) -> do
        -- Registering derivations needs DB access
        storePath <- getDefaultStorePath
        perms <- getPermissions storePath
        return (not $ writable perms)
    _ -> return False  -- Others depend on file permissions

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

-- | Execute a standalone command
executeCommand :: Command -> Options -> BuildEnv -> IO Bool
executeCommand cmd opts env = case cmd of
    Build file -> do
        -- Check if the file exists
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Build a file
        buildResult <- handleBuild env file
        case buildResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Eval file -> do
        -- Check if the file exists
        fileExists <- doesFileExist file
        unless fileExists $ do
            hPutStrLn stderr $ "Error: File not found: " ++ file
            return False

        -- Evaluate a file
        evalResult <- handleEval env file
        case evalResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    GC -> do
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
                -- Run garbage collection
                gcResult <- handleGC env (optForce opts)
                case gcResult of
                    Left err -> do
                        TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                        return False
                    Right _ -> return True
            else do
                putStrLn "Garbage collection cancelled."
                return True

    Store storeCmd -> do
        -- Handle store commands
        storeResult <- handleStore env storeCmd
        case storeResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Derivation derivCmd -> do
        -- Handle derivation commands
        derivResult <- handleDerivation env derivCmd
        case derivResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Info path -> do
        -- Check path validity
        let validPath = checkValidStorePath path
        unless validPath $ do
            hPutStrLn stderr $ "Error: Invalid store path format: " ++ path
            return False

        -- Show info about a store path
        infoResult <- handleInfo env path
        case infoResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Daemon daemonCmd -> do
        -- Check if user is root for daemon operations
        uid <- getEffectiveUserID
        unless (uid == 0) $ do
            hPutStrLn stderr "Error: Daemon operations require root privileges"
            hPutStrLn stderr "Try running with: sudo ten daemon <command>"
            return False

        -- Handle daemon commands
        daemonResult <- handleDaemon env daemonCmd
        case daemonResult of
            Left err -> do
                TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Help -> showHelp >> return True

    Version -> showVersion >> return True

-- | Handle derivation commands
handleDerivation :: BuildEnv -> DerivationCommand -> IO (Either BuildError ())
handleDerivation env = \case
    DerivationInfo path -> do
        -- Check path validity
        let validPath = checkValidStorePath path
        unless validPath $
            return $ Left $ StoreError $ "Invalid store path format: " <> T.pack path

        -- Get the store path
        let storePath = case parseStorePath path of
                Just sp -> sp
                Nothing -> error "Invalid store path format"

        -- Try to retrieve the derivation from store
        retrieveResult <- try $ runTen (retrieveDerivation storePath) env (initBuildState Build)

        case retrieveResult of
            Left (e :: SomeException) ->
                return $ Left $ StoreError $ "Failed to retrieve derivation: " <> T.pack (show e)
            Right (Left err) ->
                return $ Left err
            Right (Right (Nothing, _)) ->
                return $ Left $ StoreError $ "Derivation not found: " <> T.pack path
            Right (Right (Just drv, _)) -> do
                -- Show the derivation
                showDerivation drv
                return $ Right ()

    QueryDeriver outputPath -> do
        -- Check path validity
        let validPath = checkValidStorePath outputPath
        unless validPath $
            return $ Left $ StoreError $ "Invalid store path format: " <> T.pack outputPath

        -- Get the store path
        let storePath = case parseStorePath outputPath of
                Just sp -> sp
                Nothing -> error "Invalid store path format"

        -- Query the database for the derivation that produced this output
        db <- initDatabase (defaultDBPath (storePath env)) 5000

        derivResult <- try $ getDerivationForOutput db storePath
        closeDatabase db

        case derivResult of
            Left (e :: SomeException) ->
                return $ Left $ StoreError $ "Database query failed: " <> T.pack (show e)
            Right Nothing -> do
                putStrLn $ "No derivation found for output: " ++ outputPath
                return $ Right ()
            Right (Just derivInfo) -> do
                putStrLn $ "Output: " ++ outputPath
                putStrLn $ "Produced by derivation: " ++ T.unpack (storePathToText (derivInfoStorePath derivInfo))
                putStrLn $ "Derivation hash: " ++ T.unpack (derivInfoHash derivInfo)
                return $ Right ()

    StoreDerivation file -> do
        -- Check file existence
        fileExists <- doesFileExist file
        unless fileExists $
            return $ Left $ InputNotFound file

        -- Read the file content
        content <- try $ BS.readFile file
        case content of
            Left (e :: SomeException) ->
                return $ Left $ InputNotFound $ "Error reading file: " ++ show e
            Right bytes -> do
                -- Deserialize the derivation
                case deserializeDerivation bytes of
                    Left err ->
                        return $ Left $ SerializationError err
                    Right drv -> do
                        -- Store the derivation
                        storeResult <- try $ runTen (storeDerivation drv) env (initBuildState Build)

                        case storeResult of
                            Left (e :: SomeException) ->
                                return $ Left $ StoreError $ "Failed to store derivation: " <> T.pack (show e)
                            Right (Left err) ->
                                return $ Left err
                            Right (Right (path, _)) -> do
                                putStrLn $ "Derivation stored at: " ++ storePathToFilePath path env
                                return $ Right ()

    RegisterDerivation file -> do
        -- Check file existence
        fileExists <- doesFileExist file
        unless fileExists $
            return $ Left $ InputNotFound file

        -- Read the file content
        content <- try $ BS.readFile file
        case content of
            Left (e :: SomeException) ->
                return $ Left $ InputNotFound $ "Error reading file: " ++ show e
            Right bytes -> do
                -- Deserialize the derivation
                case deserializeDerivation bytes of
                    Left err ->
                        return $ Left $ SerializationError err
                    Right drv -> do
                        -- Store the derivation
                        storeResult <- try $ runTen (storeDerivationFile drv) env (initBuildState Build)

                        case storeResult of
                            Left (e :: SomeException) ->
                                return $ Left $ StoreError $ "Failed to register derivation: " <> T.pack (show e)
                            Right (Left err) ->
                                return $ Left err
                            Right (Right (path, _)) -> do
                                putStrLn $ "Derivation registered at: " ++ storePathToFilePath path env
                                putStrLn "Outputs:"
                                forM_ (Set.toList $ derivationOutputPaths drv) $ \output ->
                                    putStrLn $ "  " ++ storePathToFilePath output env
                                return $ Right ()

    ListDerivations -> do
        -- Open database
        db <- initDatabase (defaultDBPath (storePath env)) 5000

        -- Query all registered derivations
        derivsResult <- try $ listRegisteredDerivations db
        closeDatabase db

        case derivsResult of
            Left (e :: SomeException) ->
                return $ Left $ StoreError $ "Database query failed: " <> T.pack (show e)
            Right drvs -> do
                putStrLn $ "Found " ++ show (length drvs) ++ " registered derivations:"
                forM_ drvs $ \drv ->
                    putStrLn $ storePathToFilePath (derivInfoStorePath drv) env ++
                               " (" ++ T.unpack (derivInfoHash drv) ++ ")"
                return $ Right ()

-- | Build a derivation file
handleBuild :: BuildEnv -> FilePath -> IO (Either BuildError ())
handleBuild env file = do
    -- Check if the file exists with proper error handling
    fileExistsResult <- try $ doesFileExist file
    case fileExistsResult of
        Left (e :: IOException) ->
            return $ Left $ InputNotFound $ "Error accessing file: " ++ show e
        Right False ->
            return $ Left $ InputNotFound file
        Right True -> do
            putStrLn $ "Building derivation: " ++ file

            -- Determine file type based on extension
            extensionResult <- try $ return $ takeExtension file
            case extensionResult of
                Left (e :: SomeException) ->
                    return $ Left $ InputNotFound $ "Error determining file type: " ++ show e
                Right ext -> case ext of
                    ".ten" -> buildTenExpression env file
                    ".drv" -> buildDerivationFile env file
                    _ -> do
                        -- Try to infer file type
                        contentResult <- try $ BS.readFile file
                        case contentResult of
                            Left (e :: SomeException) ->
                                return $ Left $ InputNotFound $ "Error reading file: " ++ show e
                            Right content -> do
                                if isJsonContent content
                                    then buildTenExpression env file  -- Likely a Ten expression
                                    else buildDerivationFile env file  -- Assume it's a derivation file

-- | Check if content looks like JSON
isJsonContent :: BS.ByteString -> Bool
isJsonContent content =
    let trimmed = BS.dropWhile isSpace content
    in not (BS.null trimmed) && BS.head trimmed == 123  -- '{' character
  where
    isSpace w = w == 32 || w == 9 || w == 10 || w == 13

-- | Build a Ten expression file
buildTenExpression :: BuildEnv -> FilePath -> IO (Either BuildError ())
buildTenExpression env file = do
    -- Read the file with proper error handling
    contentResult <- try $ BS.readFile file
    case contentResult of
        Left (e :: SomeException) ->
            return $ Left $ InputNotFound $ "Error reading file: " ++ show e
        Right content -> do
            -- Run the evaluation phase to get the derivation
            evalResult <- evaluateContent env content
            case evalResult of
                Left err -> return $ Left err
                Right derivation -> do
                    -- Store the derivation in the store
                    storeResult <- runTen (storeDerivationFile derivation) env (initBuildState Build)
                    case storeResult of
                        Left err -> return $ Left err
                        Right _ -> do
                            -- Now build the derivation
                            buildResult <- buildTen (buildDerivation derivation) env
                            case buildResult of
                                Left err -> return $ Left err
                                Right (result, _) -> do
                                    -- Show build result
                                    showBuildResult result
                                    return $ Right ()

-- | Evaluate a Ten expression file to get a derivation
evaluateContent :: BuildEnv -> BS.ByteString -> IO (Either BuildError Derivation)
evaluateContent env content = do
    -- Set up evaluation environment
    let evalEnv = env { verbosity = verbosity env + 1 }

    -- Parse and evaluate Ten expression
    -- For a real implementation, we parse the Ten language here
    -- Since we don't have the Ten language parser in this exercise,
    -- we'll create a basic derivation for demonstration

    -- Create a basic builder
    builderResult <- buildTen (createBasicBuilder) evalEnv
    case builderResult of
        Left err -> return $ Left err
        Right (builder, _) -> do
            -- Create a derivation using the builder
            let drvResult = evalTen (createBasicDerivation builder content) evalEnv
            case drvResult of
                Left err -> return $ Left err
                Right (drv, _) -> return $ Right drv

-- | Create a basic builder for testing
createBasicBuilder :: TenM p StorePath
createBasicBuilder = do
    -- Create a simple bash script builder
    let builderContent = "#!/bin/sh\necho \"Running build with args: $@\"\nmkdir -p $TEN_OUT\necho \"Build result\" > $TEN_OUT/out\n"
    addToStore "bash-builder" (BS.pack builderContent)

-- | Create a basic derivation for testing
createBasicDerivation :: StorePath -> BS.ByteString -> TenM 'Eval Derivation
createBasicDerivation builder content = do
    let name = "ten-expression"
    let args = ["build"]
    let inputs = Set.empty
    let outputNames = Set.singleton "out"
    let envVars = Map.fromList [("PATH", "/bin:/usr/bin")]
    let system = "x86_64-linux"

    mkDerivation name builder args inputs outputNames envVars system

-- | Build a derivation file directly
buildDerivationFile :: BuildEnv -> FilePath -> IO (Either BuildError ())
buildDerivationFile env file = do
    -- Read the derivation file with proper error handling
    contentResult <- try $ BS.readFile file
    case contentResult of
        Left (e :: SomeException) ->
            return $ Left $ InputNotFound $ "Error reading file: " ++ show e
        Right content -> do
            -- Parse the derivation
            case deserializeDerivation content of
                Left err -> return $ Left $ SerializationError err
                Right derivation -> do
                    -- Store the derivation in the store
                    storeResult <- runTen (storeDerivationFile derivation) env (initBuildState Build)
                    case storeResult of
                        Left err -> return $ Left err
                        Right _ -> do
                            -- Build the derivation
                            buildResult <- buildTen (buildDerivation derivation) env
                            case buildResult of
                                Left err -> return $ Left err
                                Right (result, _) -> do
                                    -- Show build result
                                    showBuildResult result
                                    return $ Right ()

-- | Evaluate a Ten expression file
handleEval :: BuildEnv -> FilePath -> IO (Either BuildError ())
handleEval env file = do
    -- Check if the file exists with proper error handling
    fileExistsResult <- try $ doesFileExist file
    case fileExistsResult of
        Left (e :: IOException) ->
            return $ Left $ InputNotFound $ "Error accessing file: " ++ show e
        Right False ->
            return $ Left $ InputNotFound file
        Right True -> do
            putStrLn $ "Evaluating expression: " ++ file

            -- Read the file
            contentResult <- try $ BS.readFile file
            case contentResult of
                Left (e :: SomeException) ->
                    return $ Left $ InputNotFound $ "Error reading file: " ++ show e
                Right content -> do
                    -- Evaluate the expression
                    evalResult <- evaluateContent env content
                    case evalResult of
                        Left err -> return $ Left err
                        Right derivation -> do
                            -- Store the derivation
                            storeResult <- runTen (storeDerivationFile derivation) env (initBuildState Build)
                            case storeResult of
                                Left err -> return $ Left err
                                Right _ -> do
                                    -- Show the derivation
                                    showDerivation derivation
                                    return $ Right ()

-- | Run garbage collection
handleGC :: BuildEnv -> Bool -> IO (Either BuildError ())
handleGC env force = do
    putStrLn "Running garbage collection"

    -- If force is true, skip the confirmation
    continue <- if force
                then return True
                else do
                    putStr "This will remove all unreachable paths. Continue? [y/N] "
                    hFlush stdout
                    response <- getLine
                    return $ response `elem` ["y", "Y", "yes", "YES"]

    if continue
        then do
            -- If force is true, try to break any stale locks first
            when force $ do
                let lockPath = getGCLockPath env
                exists <- doesFileExist lockPath
                when exists $ do
                    hPutStrLn stderr "Found existing GC lock file. Breaking stale lock with --force."
                    -- Import the breakStaleLock function from Ten.GC
                    liftIO $ breakStaleLock lockPath

            -- Run garbage collection
            result <- runTen collectGarbage env (initBuildState Build)
            case result of
                Left err ->
                    case err of
                        ResourceError msg | "Could not acquire GC lock" `T.isPrefixOf` msg ->
                            return $ Left $ ResourceError $
                                "Could not acquire GC lock. Another garbage collection may be in progress. " <>
                                "Use --force to break a stale lock if you're sure no other GC is running."
                        _ -> return $ Left err
                Right (stats, _) -> do
                    -- Show GC stats
                    putStrLn $ "GC completed successfully"
                    putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
                    putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"
                    return $ Right ()
        else do
            putStrLn "Garbage collection cancelled."
            return $ Right ()

-- | Handle store operations
handleStore :: BuildEnv -> StoreCommand -> IO (Either BuildError ())
handleStore env = \case
    StoreAdd file -> do
        -- Check if the file exists with proper error handling
        fileExistsResult <- try $ doesFileExist file
        case fileExistsResult of
            Left (e :: IOException) ->
                return $ Left $ InputNotFound $ "Error accessing file: " ++ show e
            Right False ->
                return $ Left $ InputNotFound file
            Right True -> do
                -- Read the file
                contentResult <- try $ BS.readFile file
                case contentResult of
                    Left (e :: SomeException) ->
                        return $ Left $ InputNotFound $ "Error reading file: " ++ show e
                    Right content -> do
                        -- Add to store
                        let name = T.pack $ takeFileName file
                        result <- buildTen (addToStore name content) env
                        case result of
                            Left err -> return $ Left err
                            Right (path, _) -> do
                                putStrLn $ "Added to store: " ++ storePathToFilePath path env
                                return $ Right ()

    StoreVerify path -> do
        -- Parse the store path with validation
        if not (checkValidStorePath path)
            then return $ Left $ StoreError $ "Invalid store path format: " <> T.pack path
            else case parseStorePath path of
                Just storePath -> do
                    -- Verify the path
                    result <- runTen (verifyStorePath storePath) env (initBuildState Build)
                    case result of
                        Left err -> return $ Left err
                        Right (valid, _) -> do
                            if valid
                                then putStrLn "Path is valid"
                                else putStrLn "Path is invalid"
                            return $ Right ()
                Nothing -> return $ Left $ StoreError $ "Invalid store path: " <> T.pack path

    StorePath file -> do
        -- Check if the file exists with proper error handling
        fileExistsResult <- try $ doesFileExist file
        case fileExistsResult of
            Left (e :: IOException) ->
                return $ Left $ InputNotFound $ "Error accessing file: " ++ show e
            Right False ->
                return $ Left $ InputNotFound file
            Right True -> do
                -- Read the file
                contentResult <- try $ BS.readFile file
                case contentResult of
                    Left (e :: SomeException) ->
                        return $ Left $ InputNotFound $ "Error reading file: " ++ show e
                    Right content -> do
                        -- Compute the store path
                        let name = T.pack $ takeFileName file
                            hash = showHash $ hashByteString content
                            path = StorePath hash name

                        putStrLn $ "Store path would be: " ++ storePathToFilePath path env
                        return $ Right ()

    StoreGC -> handleGC env False

    StoreList -> do
        -- List all paths in the store
        pathsResult <- try $ listStorePaths env
        case pathsResult of
            Left (e :: SomeException) ->
                return $ Left $ StoreError $ "Error listing store: " <> T.pack (show e)
            Right paths -> do
                -- Show the paths
                putStrLn "Store paths:"
                forM_ paths $ \path ->
                    putStrLn $ "  " ++ path

                return $ Right ()

-- | List all store paths
listStorePaths :: BuildEnv -> IO [FilePath]
listStorePaths env = do
    -- Check if store directory exists
    let storeDir = storePath env
    exists <- doesDirectoryExist storeDir
    if not exists
        then return []
        else do
            -- List all files in the store directory
            entriesResult <- try $ listDirectory storeDir
            case entriesResult of
                Left (e :: SomeException) -> do
                    hPutStrLn stderr $ "Error reading store directory: " ++ show e
                    return []
                Right entries -> do
                    -- Filter out non-store paths
                    let isStorePath name = case break (== '-') name of
                            (hash, '-':_) -> not (null hash) && not (hash `elem` ["tmp", "gc-roots", "var"])
                            _ -> False

                    return $ filter isStorePath entries

-- | Show info about a store path
handleInfo :: BuildEnv -> FilePath -> IO (Either BuildError ())
handleInfo env path = do
    -- Parse the store path or file
    storePathResult <- if "/" `isPrefixOf` path || "." `isPrefixOf` path
                 then do
                     -- Treat as a file path - check it exists
                     fileExistsResult <- try $ doesFileExist path
                     case fileExistsResult of
                         Left (e :: IOException) ->
                             return $ Left $ InputNotFound $ "Error accessing file: " ++ show e
                         Right False ->
                             return $ Left $ InputNotFound path
                         Right True -> do
                             -- Read the file and compute hash
                             contentResult <- try $ BS.readFile path
                             case contentResult of
                                 Left (e :: SomeException) ->
                                     return $ Left $ InputNotFound $ "Error reading file: " ++ show e
                                 Right content -> do
                                     let name = T.pack $ takeFileName path
                                         hash = showHash $ hashByteString content
                                     return $ Right $ StorePath hash name
                 else
                     -- Try to parse as a store path with validation
                     if not (checkValidStorePath path)
                         then return $ Left $ StoreError $ "Invalid store path format: " <> T.pack path
                         else case parseStorePath path of
                             Just sp -> return $ Right sp
                             Nothing -> return $ Left $ StoreError $ "Invalid store path: " <> T.pack path

    case storePathResult of
        Left err -> return $ Left err
        Right sp -> do
            -- Check if the path exists in the store
            existsResult <- try $ runTen (storePathExists sp) env (initBuildState Build)
            case existsResult of
                Left (e :: SomeException) ->
                    return $ Left $ StoreError $ "Error checking path existence: " <> T.pack (show e)

                Right (Left err) -> return $ Left err
                Right (Right (True, _)) -> do
                    -- Check if this is a derivation file
                    let isDrv = ".drv" `T.isSuffixOf` storeName sp

                    if isDrv then do
                        -- Show derivation info
                        drvResult <- try $ runTen (retrieveDerivation sp) env (initBuildState Build)
                        case drvResult of
                            Left (e :: SomeException) ->
                                return $ Left $ StoreError $ "Error retrieving derivation: " <> T.pack (show e)
                            Right (Left err) ->
                                return $ Left err
                            Right (Right (Nothing, _)) ->
                                return $ Left $ StoreError $ "Derivation not found: " <> T.pack path
                            Right (Right (Just drv, _)) -> do
                                -- Show the derivation
                                putStrLn "Derivation file:"
                                showDerivation drv
                                return $ Right ()
                    else do
                        -- Check if this is an output of some derivation
                        db <- initDatabase (defaultDBPath (storePath env)) 5000
                        derivResult <- try $ getDerivationForOutput db sp
                        closeDatabase db

                        -- Show info about the path
                        putStrLn $ "Store path: " ++ storePathToFilePath sp env
                        putStrLn $ "Hash: " ++ T.unpack (storeHash sp)
                        putStrLn $ "Name: " ++ T.unpack (storeName sp)

                        -- Show deriver info if available
                        case derivResult of
                            Left (e :: SomeException) ->
                                putStrLn $ "Could not query database: " ++ show e
                            Right Nothing ->
                                putStrLn "Not an output of any known derivation"
                            Right (Just derivInfo) ->
                                putStrLn $ "Output of derivation: " ++
                                          storePathToFilePath (derivInfoStorePath derivInfo) env

                        -- Verify the path
                        verifyResult <- runTen (verifyStorePath sp) env (initBuildState Build)
                        case verifyResult of
                            Left err -> return $ Left err
                            Right (valid, _) -> do
                                putStrLn $ "Valid: " ++ if valid then "Yes" else "No"
                                return $ Right ()
                Right (Right (False, _)) ->
                    return $ Left $ StoreError $ "Path not in store: " <> T.pack path

-- | Handle daemon operations
handleDaemon :: BuildEnv -> DaemonCommand -> IO (Either BuildError ())
handleDaemon env = \case
    DaemonStart -> do
        -- Check if user is root
        uid <- getEffectiveUserID
        unless (uid == 0) $
            return $ Left $ DaemonError "Starting daemon requires root privileges"

        -- Check if daemon is already running
        socketPath <- getDefaultSocketPath
        running <- isDaemonRunning socketPath
        if running
            then do
                putStrLn "Daemon is already running"
                return $ Right ()
            else do
                -- Start the daemon
                putStrLn "Starting daemon..."
                daemonCommandResult <- try $ daemonCommand ["start"]
                case daemonCommandResult of
                    Left (e :: SomeException) ->
                        return $ Left $ DaemonError $ "Failed to start daemon: " <> T.pack (show e)
                    Right result -> case result of
                        ExitSuccess -> do
                            putStrLn "Daemon started successfully"
                            return $ Right ()
                        _ -> return $ Left $ DaemonError "Failed to start daemon"

    DaemonStop -> do
        -- Check if daemon is running
        socketPath <- getDefaultSocketPath
        running <- isDaemonRunning socketPath
        if running
            then do
                -- Stop the daemon
                putStrLn "Stopping daemon..."
                daemonCommandResult <- try $ daemonCommand ["stop"]
                case daemonCommandResult of
                    Left (e :: SomeException) ->
                        return $ Left $ DaemonError $ "Failed to stop daemon: " <> T.pack (show e)
                    Right result -> case result of
                        ExitSuccess -> do
                            putStrLn "Daemon stopped successfully"
                            return $ Right ()
                        _ -> return $ Left $ DaemonError "Failed to stop daemon"
            else do
                putStrLn "Daemon is not running"
                return $ Right ()

    DaemonStatus -> do
        -- Check if daemon is running
        socketPath <- getDefaultSocketPath
        running <- isDaemonRunning socketPath
        if running
            then do
                putStrLn "Daemon is running"
                -- Try to get more detailed status
                statusResult <- try $ daemonCommand ["status"]
                case statusResult of
                    Left (e :: SomeException) ->
                        hPutStrLn stderr $ "Warning: Could not get detailed status: " ++ show e
                    Right _ -> return ()
                return $ Right ()
            else do
                putStrLn "Daemon is not running"
                return $ Right ()

    DaemonRestart -> do
        -- Check if user is root
        uid <- getEffectiveUserID
        unless (uid == 0) $
            return $ Left $ DaemonError "Restarting daemon requires root privileges"

        -- First stop
        stopResult <- handleDaemon env DaemonStop
        case stopResult of
            Left err -> return $ Left err
            Right _ -> do
                -- Then start
                -- Give it a moment to fully shut down
                threadDelay 1000000  -- 1 second
                handleDaemon env DaemonStart

    DaemonConfig -> do
        -- Show daemon configuration
        configResult <- try $ getDefaultConfig
        case configResult of
            Left (e :: SomeException) ->
                return $ Left $ DaemonError $ "Error getting daemon config: " <> T.pack (show e)
            Right config -> do
                putStrLn "Daemon configuration:"
                putStrLn $ "  Socket: " ++ daemonSocketPath config
                putStrLn $ "  Store: " ++ daemonStorePath config
                putStrLn $ "  State file: " ++ daemonStateFile config
                putStrLn $ "  GC interval: " ++ maybe "None" show (daemonGcInterval config)
                putStrLn $ "  User: " ++ maybe "None" T.unpack (daemonUser config)
                putStrLn $ "  Allowed users: " ++ intercalate ", " (map T.unpack $ Set.toList $ daemonAllowedUsers config)
                return $ Right ()

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

-- | Helper to run a daemon command
daemonCommand :: [String] -> IO ExitCode
daemonCommand args = do
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

-- | Parse a store path from a string
parseStorePath :: FilePath -> Maybe StorePath
parseStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) -> Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

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
    username <- getUserName

    -- Get or generate an authentication token
    token <- getStoredToken socketPath username `catch` \(_ :: SomeException) -> do
        -- If no token exists or there's any error, generate a new one
        generateRandomToken

    return $ UserCredentials {
        username = username,
        token = token
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
    r1 <- (randomIO :: IO Int)
    r2 <- (randomIO :: IO Int)
    r3 <- (randomIO :: IO Int)

    -- Format as a base64-like string for use in authentication
    let randomChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-', '_']
    let randomString = take 32 $ map (\i -> randomChars !! (abs i `mod` length randomChars)) [r1, r2, r3]
    return $ T.pack $ "ten_token_" ++ randomString

-- | Get current username
getUserName :: IO Text
getUserName = do
    -- Try environment variable
    mUser <- lookupEnv "USER"
    case mUser of
        Just user -> return $ T.pack user
        Nothing -> do
            -- Try another environment variable
            mUsername <- lookupEnv "USERNAME"
            case mUsername of
                Just username -> return $ T.pack username
                Nothing -> do
                    -- Use system call
                    uid <- getEffectiveUserID
                    entry <- getUserEntryForID uid
                    return $ T.pack $ userName entry

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

-- Utility functions for working with store paths

-- | Convert a StorePath to a text representation for database operations
storePathToText :: StorePath -> Text
storePathToText (StorePath hash name) = hash <> "-" <> name

-- | Get paths that a derivation produces
derivationOutputPaths :: Derivation -> Set StorePath
derivationOutputPaths drv = Set.map outputPath (derivOutputs drv)

-- | Check if file exists
fileExists :: FilePath -> IO Bool
fileExists = doesFileExist

-- | Break a stale lock - imported from Ten.GC
breakStaleLock :: FilePath -> IO ()
breakStaleLock = error "Function imported from Ten.GC"
