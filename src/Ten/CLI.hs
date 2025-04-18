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
import Control.Exception (try, catch, SomeException, IOException, bracket, finally)
import Control.Monad (when, unless, void, forM_)
import Data.List (intercalate)
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
import Network.Socket
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory,
                        getXdgDirectory, XdgDirectory(..), removeFile)
import System.Environment (getArgs, getProgName, lookupEnv, getEnvironment)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (IOMode(..), withFile, hPutStrLn, stdout, stderr, hSetBuffering,
                BufferMode(..), hClose, hFlush)
import System.Console.GetOpt
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName)
import System.Process (createProcess, proc, waitForProcess, readProcessWithExitCode)

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Build
import Ten.GC
import Ten.Hash
import Ten.Daemon.Config (getDefaultSocketPath, getDefaultConfig)
import Ten.Daemon.Client (isDaemonRunning, startDaemonIfNeeded, UserCredentials(..))
import qualified Ten.Daemon.Client as DaemonClient

-- | CLI commands
data Command
    = Build FilePath                     -- Build a derivation file
    | Eval FilePath                      -- Evaluate a Ten expression file
    | GC                                 -- Garbage collection
    | Store StoreCommand                 -- Store operations
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
        (ReqArg (\path opts -> opts { optStorePath = Just path }) "PATH")
        "Path to the store"
    , Option ['w'] ["work-dir"]
        (ReqArg (\dir opts -> opts { optWorkDir = Just dir }) "DIR")
        "Working directory"
    , Option ['k'] ["keep-failed"]
        (NoArg (\opts -> opts { optKeepFailed = True }))
        "Keep build outputs even if build fails"
    , Option ['f'] ["force"]
        (NoArg (\opts -> opts { optForce = True }))
        "Force operation even if risky"
    , Option [] ["no-daemon"]
        (NoArg (\opts -> opts { optUseDaemon = False }))
        "Don't use daemon even if available"
    , Option [] ["daemon-socket"]
        (ReqArg (\sock opts -> opts { optDaemonSocket = Just sock }) "PATH")
        "Path to daemon socket"
    , Option [] ["system"]
        (ReqArg (\sys opts -> opts { optSystemType = Just (T.pack sys) }) "TYPE")
        "Target system type (e.g., x86_64-linux)"
    , Option ['j'] ["jobs"]
        (ReqArg (\n opts -> opts { optMaxJobs = Just (read n) }) "N")
        "Maximum number of parallel jobs"
    , Option [] ["silent"]
        (NoArg (\opts -> opts { optSuppressOutput = True }))
        "Suppress command output"
    , Option [] ["arg"]
        (ReqArg (\arg opts ->
                 let (key, val) = case break (=='=') arg of
                                       (k, '=':v) -> (k, v)
                                       (k, _) -> (k, "")
                 in opts { optBuildArgs = (key, val) : optBuildArgs opts }) "KEY=VALUE")
        "Pass build argument"
    ]

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
                (file:_) -> Right $ Build file
                [] -> Left "build: missing file argument"
        "eval" ->
            case nonOpts of
                (file:_) -> Right $ Eval file
                [] -> Left "eval: missing file argument"
        "gc" -> Right GC
        "store" ->
            case nonOpts of
                [] -> Left "store: missing subcommand"
                (subcmd:args') ->
                    case subcmd of
                        "add" ->
                            case args' of
                                (file:_) -> Right $ Store (StoreAdd file)
                                [] -> Left "store add: missing file argument"
                        "verify" ->
                            case args' of
                                (path:_) -> Right $ Store (StoreVerify path)
                                [] -> Left "store verify: missing path argument"
                        "path" ->
                            case args' of
                                (file:_) -> Right $ Store (StorePath file)
                                [] -> Left "store path: missing file argument"
                        "gc" -> Right $ Store StoreGC
                        "list" -> Right $ Store StoreList
                        _ -> Left $ "unknown store subcommand: " ++ subcmd
        "info" ->
            case nonOpts of
                (path:_) -> Right $ Info path
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

    -- Resolve work directory
    workDir <- case optWorkDir options of
        Just dir -> return dir
        Nothing -> getDefaultWorkDir

    -- Create directories if needed
    createDirectoryIfMissing True storePath
    createDirectoryIfMissing True workDir

    -- Determine username
    username <- do
        uid <- getEffectiveUserID
        entry <- getUserEntryForID uid
        return $ T.pack $ userName entry

    -- Set up build environment
    let env = initBuildEnv workDir storePath
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
                _ -> checkDaemonAvailable opts

            -- These commands benefit from daemon
            Build _ -> checkDaemonAvailable opts
            Eval _ -> checkDaemonAvailable opts
            GC -> checkDaemonAvailable opts
            Info _ -> checkDaemonAvailable opts

-- | Check if daemon is available (running or can be auto-started)
checkDaemonAvailable :: Options -> IO Bool
checkDaemonAvailable opts = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> return path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- isDaemonRunning socketPath

    -- Return True if running or auto-start is enabled (assuming auto-start is always enabled)
    return $ daemonRunning || True  -- Always allow auto-start

-- | Run a command with the daemon
runWithDaemon :: Command -> Options -> BuildEnv -> IO ()
runWithDaemon cmd opts env = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> return path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- isDaemonRunning socketPath

    -- Start daemon if needed and auto-start is enabled
    unless daemonRunning $ do
        putStrLn "Daemon not running. Starting daemon..."
        startDaemonIfNeeded socketPath `catch` \(e :: SomeException) -> do
            hPutStrLn stderr $ "Error starting daemon: " ++ show e
            hPutStrLn stderr "Falling back to standalone mode."
            runStandalone cmd opts env
            exitSuccess

    -- Get user credentials
    credentials <- getUserCredentials

    -- Connect to daemon
    result <- try $ DaemonClient.connectToDaemon socketPath credentials
    case result of
        Left (e :: SomeException) -> do
            hPutStrLn stderr $ "Error connecting to daemon: " ++ show e
            hPutStrLn stderr "Falling back to standalone mode."
            runStandalone cmd opts env

        Right conn -> do
            -- Execute command using daemon
            exit <- executeDaemonCommand cmd opts conn

            -- Disconnect from daemon
            DaemonClient.disconnectFromDaemon conn

            -- Exit with appropriate status
            if exit
                then exitSuccess
                else exitFailure

-- | Execute a command using the daemon connection
executeDaemonCommand :: Command -> Options -> DaemonClient.DaemonConnection -> IO Bool
executeDaemonCommand cmd opts conn = case cmd of
    Build file -> do
        -- Build a file using daemon
        result <- DaemonClient.buildFile conn file
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right buildResult -> do
                -- Show build result
                showBuildResult buildResult
                return True

    Eval file -> do
        -- Evaluate a file using daemon
        result <- DaemonClient.evalFile conn file
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right derivation -> do
                -- Show derivation
                showDerivation derivation
                return True

    GC -> do
        -- Run garbage collection
        result <- DaemonClient.collectGarbage conn (optForce opts)
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right stats -> do
                putStrLn $ "Garbage collection completed successfully."
                putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
                putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"
                return True

    Store storeCmd -> executeStoreCommand storeCmd conn opts

    Info path -> do
        -- Show info about a store path
        result <- try $ do
            let storePath = parseStorePath path
            case storePath of
                Nothing -> return $ Left $ StoreError "Invalid store path format"
                Just sp -> DaemonClient.verifyStorePath conn path

        case result of
            Left (e :: SomeException) -> do
                TIO.putStrLn $ "Error: " <> T.pack (show e)
                return False
            Right (Left err) -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
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
        -- Add file to store
        result <- DaemonClient.addFileToStore conn file
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right path -> do
                putStrLn $ "File added to store: " ++ file
                putStrLn $ "Store path: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                return True

    StoreVerify path -> do
        -- Verify store path
        result <- DaemonClient.verifyStorePath conn path
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right isValid -> do
                putStrLn $ "Path: " ++ path
                putStrLn $ "Valid: " ++ if isValid then "Yes" else "No"
                return True

    StorePath file -> do
        -- Calculate store path for file (can run locally)
        result <- getStorePathForFile file
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right storePath -> do
                putStrLn $ "Store path for " ++ file ++ ":"
                putStrLn $ "  " ++ T.unpack (storeHash storePath) ++ "-" ++ T.unpack (storeName storePath)
                return True

    StoreGC -> do
        -- Run garbage collection (same as GC command)
        result <- DaemonClient.collectGarbage conn (optForce opts)
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right stats -> do
                putStrLn "Garbage collection completed successfully."
                putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
                putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"
                return True

    StoreList -> do
        -- List store contents
        result <- DaemonClient.listStore conn
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right paths -> do
                putStrLn $ "Store contains " ++ show (length paths) ++ " items:"
                forM_ paths $ \path ->
                    putStrLn $ "  " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)
                return True

-- | Run a command in standalone mode (without daemon)
runStandalone :: Command -> Options -> BuildEnv -> IO ()
runStandalone cmd opts env = do
    -- Execute the command
    result <- executeCommand cmd opts env

    -- Exit with appropriate status
    if result
        then exitSuccess
        else exitFailure

-- | Execute a standalone command
executeCommand :: Command -> Options -> BuildEnv -> IO Bool
executeCommand cmd opts env = case cmd of
    Build file -> do
        -- Build a file
        result <- handleBuild env file
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Eval file -> do
        -- Evaluate a file
        result <- handleEval env file
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    GC -> do
        -- Run garbage collection
        result <- handleGC env (optForce opts)
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Store storeCmd -> do
        -- Handle store commands
        result <- handleStore env storeCmd
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Info path -> do
        -- Show info about a store path
        result <- handleInfo env path
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Daemon daemonCmd -> do
        -- Handle daemon commands
        result <- handleDaemon env daemonCmd
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> return True

    Help -> showHelp >> return True

    Version -> showVersion >> return True

-- | Build a derivation file
handleBuild :: BuildEnv -> FilePath -> IO (Either BuildError ())
handleBuild env file = do
    -- Check if the file exists
    exists <- doesFileExist file
    unless exists $
        return $ Left $ InputNotFound file

    putStrLn $ "Building derivation: " ++ file

    -- Determine file type based on extension
    case takeExtension file of
        ".ten" -> buildTenExpression env file
        ".drv" -> buildDerivationFile env file
        _ -> do
            -- Try to infer file type
            content <- BS.readFile file
            if BS.isPrefixOf "{" $ BS.dropWhile isSpace content
                then buildTenExpression env file  -- Likely a Ten expression
                else buildDerivationFile env file  -- Assume it's a derivation file
  where
    isSpace w = w == 32 || w == 9 || w == 10 || w == 13

-- | Build a Ten expression file
buildTenExpression :: BuildEnv -> FilePath -> IO (Either BuildError ())
buildTenExpression env file = do
    -- Read the file
    content <- BS.readFile file

    -- Run the evaluation phase to get the derivation
    evalResult <- evalExpression env content
    case evalResult of
        Left err -> return $ Left err
        Right derivation -> do
            -- Now build the derivation
            buildResult <- buildTen (buildDerivation derivation) env
            case buildResult of
                Left err -> return $ Left err
                Right (result, _) -> do
                    -- Show build result
                    showBuildResult result
                    return $ Right ()

-- | Evaluate a Ten expression file to get a derivation
evalExpression :: BuildEnv -> BS.ByteString -> IO (Either BuildError Derivation)
evalExpression env content = do
    -- Set up evaluation environment
    let evalEnv = env { verbosity = verbosity env + 1 }

    -- Real implementation would parse and evaluate Ten expression here
    -- For now, we'll create a placeholder derivation since Ten language
    -- parsing is beyond our scope in this implementation
    builder <- placeholderBuilder env
    let drvResult = evalTen (placeholderDerivation builder) evalEnv
    case drvResult of
        Left err -> return $ Left err
        Right (drv, _) -> return $ Right drv

-- | Create a placeholder builder for testing
placeholderBuilder :: BuildEnv -> IO StorePath
placeholderBuilder env = do
    -- Create a simple bash script builder
    let builderContent = "#!/bin/sh\necho \"$@\" > $TEN_OUT/out"
    builderResult <- buildTen (addToStore "placeholder-builder" (BS.pack builderContent)) env
    case builderResult of
        Left _ -> error "Failed to create placeholder builder"
        Right (path, _) -> return path

-- | Create a placeholder derivation for testing
placeholderDerivation :: StorePath -> TenM 'Eval Derivation
placeholderDerivation builder = do
    let name = "placeholder"
    let args = ["Hello from Ten!"]
    let inputs = Set.empty
    let outputNames = Set.singleton "out"
    let envVars = Map.fromList [("PATH", "/bin:/usr/bin")]
    let system = "x86_64-linux"

    mkDerivation name builder args inputs outputNames envVars system

-- | Build a derivation file directly
buildDerivationFile :: BuildEnv -> FilePath -> IO (Either BuildError ())
buildDerivationFile env file = do
    -- Read the derivation file
    content <- BS.readFile file

    -- Parse the derivation
    case deserializeDerivation content of
        Left err -> return $ Left $ SerializationError err
        Right derivation -> do
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
    -- Check if the file exists
    exists <- doesFileExist file
    unless exists $
        return $ Left $ InputNotFound file

    putStrLn $ "Evaluating expression: " ++ file

    -- Read the file
    content <- BS.readFile file

    -- Evaluate the expression
    evalResult <- evalExpression env content
    case evalResult of
        Left err -> return $ Left err
        Right derivation -> do
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
                    return $ response `elem` ["y", "Y", "yes", "Yes"]

    if continue
        then do
            -- Run garbage collection
            result <- runTen collectGarbage env (initBuildState Build)
            case result of
                Left err -> return $ Left err
                Right (stats, _) -> do
                    -- Show GC stats
                    putStrLn $ "GC completed successfully"
                    putStrLn $ "Collected " ++ show (gcCollected stats) ++ " paths"
                    putStrLn $ "Freed " ++ show (gcBytes stats) ++ " bytes"
                    return $ Right ()
        else do
            putStrLn "Garbage collection cancelled"
            return $ Right ()

-- | Handle store operations
handleStore :: BuildEnv -> StoreCommand -> IO (Either BuildError ())
handleStore env = \case
    StoreAdd file -> do
        -- Check if the file exists
        exists <- doesFileExist file
        unless exists $
            return $ Left $ InputNotFound file

        -- Read the file
        content <- BS.readFile file

        -- Add to store
        let name = T.pack $ takeFileName file
        result <- buildTen (addToStore name content) env
        case result of
            Left err -> return $ Left err
            Right (path, _) -> do
                putStrLn $ "Added to store: " ++ storePathToFilePath path env
                return $ Right ()

    StoreVerify path -> do
        -- Parse the store path
        case parseStorePath path of
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
        -- Check if the file exists
        exists <- doesFileExist file
        unless exists $
            return $ Left $ InputNotFound file

        -- Read the file
        content <- BS.readFile file

        -- Compute the store path
        let name = T.pack $ takeFileName file
            hash = showHash $ hashByteString content
            path = StorePath hash name

        putStrLn $ "Store path would be: " ++ storePathToFilePath path env
        return $ Right ()

    StoreGC -> handleGC env False

    StoreList -> do
        -- List all paths in the store
        paths <- listStorePaths env

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
            entries <- listDirectory storeDir

            -- Filter out non-store paths
            let isStorePath name = case break (== '-') name of
                    (hash, '-':_) -> not (null hash) && not (hash `elem` ["tmp", "gc-roots"])
                    _ -> False

            return $ filter isStorePath entries

-- | Show info about a store path
handleInfo :: BuildEnv -> FilePath -> IO (Either BuildError ())
handleInfo env path = do
    -- Parse the store path or file
    storePath <- if "/" `isPrefixOf` path || "." `isPrefixOf` path
                 then do
                     -- Treat as a file path
                     exists <- doesFileExist path
                     unless exists $
                         return $ Left $ InputNotFound path

                     -- Read the file and compute hash
                     content <- BS.readFile path
                     let name = T.pack $ takeFileName path
                         hash = showHash $ hashByteString content
                     return $ Right $ StorePath hash name
                 else
                     -- Try to parse as a store path
                     case parseStorePath path of
                         Just sp -> return $ Right sp
                         Nothing -> return $ Left $ StoreError $ "Invalid store path: " <> T.pack path

    case storePath of
        Left err -> return $ Left err
        Right sp -> do
            -- Check if the path exists in the store
            exists <- runTen (storePathExists sp) env (initBuildState Build)
            case exists of
                Left err -> return $ Left err
                Right (True, _) -> do
                    -- Show info about the path
                    putStrLn $ "Store path: " ++ storePathToFilePath sp env
                    putStrLn $ "Hash: " ++ T.unpack (storeHash sp)
                    putStrLn $ "Name: " ++ T.unpack (storeName sp)

                    -- Verify the path
                    verifyResult <- runTen (verifyStorePath sp) env (initBuildState Build)
                    case verifyResult of
                        Left err -> return $ Left err
                        Right (valid, _) -> do
                            putStrLn $ "Valid: " ++ if valid then "Yes" else "No"
                            return $ Right ()
                Right (False, _) ->
                    return $ Left $ StoreError $ "Path not in store: " <> T.pack path

-- | Handle daemon operations
handleDaemon :: BuildEnv -> DaemonCommand -> IO (Either BuildError ())
handleDaemon env = \case
    DaemonStart -> do
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
                result <- daemonCommand ["start"]
                case result of
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
                result <- daemonCommand ["stop"]
                case result of
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
                result <- daemonCommand ["status"]
                return $ Right ()
            else do
                putStrLn "Daemon is not running"
                return $ Right ()

    DaemonRestart -> do
        -- First stop
        _ <- handleDaemon env DaemonStop
        -- Then start
        handleDaemon env DaemonStart

    DaemonConfig -> do
        -- Show daemon configuration
        config <- getDefaultConfig
        putStrLn "Daemon configuration:"
        putStrLn $ "  Socket: " ++ daemonSocketPath config
        putStrLn $ "  Store: " ++ daemonStorePath config
        putStrLn $ "  State file: " ++ daemonStateFile config
        putStrLn $ "  GC interval: " ++ maybe "None" show (daemonGcInterval config)
        putStrLn $ "  User: " ++ maybe "None" T.unpack (daemonUser config)
        putStrLn $ "  Allowed users: " ++ intercalate ", " (map T.unpack $ Set.toList $ daemonAllowedUsers config)
        return $ Right ()

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
    let tenDaemonPath = "ten-daemon"

    -- Run the daemon command
    (_, _, _, ph) <- createProcess (proc tenDaemonPath args)
    waitForProcess ph

-- | Parse a store path from a string
parseStorePath :: FilePath -> Maybe StorePath
parseStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) -> Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

-- | Resolve the store path from options or default
resolveStorePath :: Options -> IO FilePath
resolveStorePath options = case optStorePath options of
    Just path -> return path
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
getUserCredentials :: IO UserCredentials
getUserCredentials = do
    -- Get username from environment or system
    username <- getUserName

    -- In a real implementation, we would have proper token management
    -- For now, just use a placeholder
    let token = "placeholder-token"

    return $ UserCredentials {
        username = username,
        token = token
    }

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
    exists <- doesFileExist file
    if not exists
        then return $ Left $ InputNotFound file
        else do
            -- Read the file content
            content <- BS.readFile file

            -- Calculate hash
            let name = T.pack $ takeFileName file
                hash = showHash $ hashByteString content
                path = StorePath hash name

            return $ Right path
