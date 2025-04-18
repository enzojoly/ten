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

    -- Utility functions
    resolveStorePath,
    getDefaultStorePath,
    getDefaultWorkDir,
    showBuildResult,
    showDerivation
) where

import Control.Concurrent
import Control.Exception (try, SomeException, bracket)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError, catchError)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Network.Socket
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Console.GetOpt
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName)

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Build
import Ten.GC
import Ten.Hash

-- Optional imports for daemon functionality
-- These will be conditional based on whether the daemon modules are available
import qualified Ten.Daemon.Client as DaemonClient
import qualified Ten.Daemon.Protocol as Protocol
import qualified Ten.Daemon.Config as DaemonConfig

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
    useDaemon <- if optUseDaemon options
                  then checkDaemonAvailable (optDaemonSocket options)
                  else return False

    -- Execute the command (with or without daemon)
    if useDaemon
        then executeDaemonCommand cmd options env
        else executeLocalCommand cmd options env

-- | Execute a command using the daemon
executeDaemonCommand :: Command -> Options -> BuildEnv -> IO ()
executeDaemonCommand cmd options env = do
    -- Connect to daemon
    daemonSocket <- case optDaemonSocket options of
        Just sock -> return sock
        Nothing -> DaemonClient.getDefaultSocketPath

    -- Get username for auth
    username <- do
        uid <- getEffectiveUserID
        entry <- getUserEntryForID uid
        return $ T.pack $ userName entry

    let credentials = UserCredentials
            { username = username
              -- In a real implementation, we would use a proper token system
              -- For now, just use a placeholder
            , token = "placeholder-token"
            }

    -- Run the command using daemon client
    withDaemonClient daemonSocket credentials $ \conn -> do
        -- Create client environment
        let clientEnv = initClientEnv (workDir env) (storePath env) conn

        -- Execute the command through the daemon
        result <- case cmd of
            Build file -> DaemonClient.buildFile conn file
            Eval file -> DaemonClient.evalFile conn file
            GC -> DaemonClient.collectGarbage conn (optForce options)
            Store storeCmd -> executeDaemonStoreCommand conn storeCmd
            Info path -> DaemonClient.getPathInfo conn path
            Daemon daemonCmd -> executeDaemonDaemonCommand conn daemonCmd
            Help -> do
                showHelp
                return $ Right ()
            Version -> do
                showVersion
                return $ Right ()

        -- Handle the result
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                exitFailure
            Right _ -> exitSuccess

-- | Execute a store command through the daemon
executeDaemonStoreCommand :: DaemonConnection -> StoreCommand -> IO (Either BuildError ())
executeDaemonStoreCommand conn = \case
    StoreAdd file -> DaemonClient.addFileToStore conn file
    StoreVerify path -> DaemonClient.verifyStorePath conn path
    StorePath file -> DaemonClient.getStorePathForFile conn file
    StoreGC -> DaemonClient.collectGarbage conn False
    StoreList -> DaemonClient.listStore conn

-- | Execute a daemon command through the daemon client
executeDaemonDaemonCommand :: DaemonConnection -> DaemonCommand -> IO (Either BuildError ())
executeDaemonDaemonCommand conn = \case
    DaemonStatus -> DaemonClient.getDaemonStatus conn
    DaemonConfig -> DaemonClient.getDaemonConfig conn
    -- These commands should be run directly, not through an existing daemon connection
    _ -> return $ Left $ DaemonError "This command cannot be executed through an existing daemon"

-- | Execute a command locally (without daemon)
executeLocalCommand :: Command -> Options -> BuildEnv -> IO ()
executeLocalCommand cmd options env = do
    -- Execute the command directly
    result <- case cmd of
        Build file -> handleBuild env file
        Eval file -> handleEval env file
        GC -> handleGC env (optForce options)
        Store storeCmd -> handleStore env storeCmd
        Info path -> handleInfo env path
        Daemon daemonCmd -> handleDaemon env daemonCmd
        Help -> showHelp >> return (Right ())
        Version -> showVersion >> return (Right ())

    -- Handle the result
    case result of
        Left err -> do
            TIO.putStrLn $ "Error: " <> T.pack (show err)
            exitFailure
        Right _ -> exitSuccess

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
            content <- readFile file
            if "{" `isPrefixOf` (dropWhile isSpace content)
                then buildTenExpression env file  -- Likely a Ten expression
                else buildDerivationFile env file  -- Assume it's a derivation file

-- | Build a Ten expression file
buildTenExpression :: BuildEnv -> FilePath -> IO (Either BuildError ())
buildTenExpression env file = do
    -- Read the file
    content <- TIO.readFile file

    -- Run the evaluation phase to get the derivation
    evalResult <- evalTen (evalExpression content) env
    case evalResult of
        Left err -> return $ Left err
        Right (derivation, _) -> do
            -- Now build the derivation
            buildResult <- buildTen (buildDerivation derivation) env
            case buildResult of
                Left err -> return $ Left err
                Right (result, _) -> do
                    -- Show build result
                    showBuildResult result
                    return $ Right ()

-- | Evaluate a Ten expression
evalExpression :: Text -> TenM 'Eval Derivation
evalExpression expr = do
    logMsg 1 $ "Evaluating expression: " <> expr
    -- In a real implementation, this would parse and evaluate the expression
    -- For now, we'll throw an error as this needs to be implemented
    throwError $ EvalError "Expression evaluation not implemented yet"

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
    content <- TIO.readFile file

    -- Run the evaluation phase
    evalResult <- evalTen (evalExpression content) env
    case evalResult of
        Left err -> return $ Left err
        Right (derivation, _) -> do
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
                    putStrLn $ "GC stats: " ++ show stats
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
        running <- isDaemonRunning (optDaemonSocket Nothing)
        if running
            then do
                putStrLn "Daemon is already running"
                return $ Right ()
            else do
                -- Start the daemon
                putStrLn "Starting daemon..."
                daemonCommand ["start"]
                return $ Right ()

    DaemonStop -> do
        -- Check if daemon is running
        running <- isDaemonRunning (optDaemonSocket Nothing)
        if running
            then do
                -- Stop the daemon
                putStrLn "Stopping daemon..."
                daemonCommand ["stop"]
                return $ Right ()
            else do
                putStrLn "Daemon is not running"
                return $ Right ()

    DaemonStatus -> do
        -- Check if daemon is running
        running <- isDaemonRunning (optDaemonSocket Nothing)
        if running
            then do
                putStrLn "Daemon is running"
                -- Get more status information
                _ <- daemonCommand ["status"]
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
        config <- DaemonConfig.getDefaultConfig
        putStrLn "Daemon configuration:"
        putStrLn $ "  Socket: " ++ DaemonConfig.daemonSocketPath config
        putStrLn $ "  Store: " ++ DaemonConfig.daemonStorePath config
        putStrLn $ "  State file: " ++ DaemonConfig.daemonStateFile config
        putStrLn $ "  Log level: " ++ show (DaemonConfig.daemonLogLevel config)
        putStrLn $ "  GC interval: " ++ maybe "None" show (DaemonConfig.daemonGcInterval config)
        putStrLn $ "  User: " ++ maybe "None" T.unpack (DaemonConfig.daemonUser config)
        putStrLn $ "  Allowed users: " ++ intercalate ", " (map T.unpack $ Set.toList $ DaemonConfig.daemonAllowedUsers config)
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

-- | Check if daemon is available
checkDaemonAvailable :: Maybe FilePath -> IO Bool
checkDaemonAvailable socketPath = do
    -- Get the socket path
    sock <- case socketPath of
        Just path -> return path
        Nothing -> DaemonClient.getDefaultSocketPath

    -- Check if socket exists and is valid
    exists <- doesFileExist sock
    if exists
        then isDaemonRunning sock
        else return False

-- | Check if daemon is running
isDaemonRunning :: FilePath -> IO Bool
isDaemonRunning socketPath = do
    -- Try to connect to the socket
    result <- try $ do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock (SockAddrUnix socketPath)
        return sock

    case result of
        Left (_ :: SomeException) -> return False
        Right sock -> do
            -- Successfully connected, daemon is running
            close sock
            return True

-- | Connect to the daemon and run an action
withDaemonClient :: FilePath -> UserCredentials -> (DaemonConnection -> IO a) -> IO a
withDaemonClient socketPath credentials action = do
    -- Connect to the daemon
    bracket
        (DaemonClient.connectToDaemon socketPath credentials)
        DaemonClient.disconnectFromDaemon
        action

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
            -- Use default path
            home <- getHomeDirectory
            return $ home </> ".ten/store"

-- | Get the default work directory
getDefaultWorkDir :: IO FilePath
getDefaultWorkDir = do
    -- Check environment variable
    envPath <- lookupEnv "TEN_WORK_DIR"
    case envPath of
        Just path -> return path
        Nothing -> do
            -- Use default path
            home <- getHomeDirectory
            return $ home </> ".ten/work"

-- | List all paths in the store
listStorePaths :: BuildEnv -> IO [FilePath]
listStorePaths env = do
    -- Get the store directory
    let storeDir = storePath env

    -- Check if the directory exists
    exists <- doesDirectoryExist storeDir
    if exists
        then do
            -- List all files in the directory
            entries <- listDirectory storeDir

            -- Filter out non-store paths
            return $ filter isStorePath entries
        else return []
  where
    -- Check if a path looks like a store path
    isStorePath path = case break (== '-') path of
        (hash, '-':_) -> length hash > 0
        _ -> False

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

    -- Show log if available
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
