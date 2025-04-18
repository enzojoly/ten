{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (try, catch, SomeException, IOException)
import Control.Monad (when, unless, void)
import Data.Maybe (isJust, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.Environment (getArgs, getProgName, getEnv, lookupEnv)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hPutStrLn, stderr)
import System.Process (createProcess, proc, waitForProcess)
import System.Exit (ExitCode(..))

import Ten.Core
import Ten.CLI
import Ten.Daemon.Config (getDefaultSocketPath, getDefaultConfig)
import Ten.Daemon.Client (isDaemonRunning, startDaemonIfNeeded, UserCredentials(..))
import qualified Ten.Daemon.Client as DaemonClient

-- | Main entry point
main :: IO ()
main = do
    -- Parse command-line arguments
    args <- getArgs

    -- Handle special case for no arguments
    when (null args) $ do
        showUsage
        exitSuccess

    -- Parse commands
    case parseArgs args of
        Left err -> do
            -- Show error and usage
            hPutStrLn stderr $ "Error: " ++ err
            showUsage
            exitFailure

        Right (cmd, opts) -> do
            -- Check if we should use daemon for this command
            shouldUseDaemon <- shouldRunWithDaemon cmd opts

            if shouldUseDaemon
                then runWithDaemon cmd opts
                else runStandalone cmd opts

-- | Show usage information
showUsage :: IO ()
showUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " COMMAND [OPTIONS]"
    putStrLn ""
    putStrLn "For detailed help, run:"
    putStrLn $ "  " ++ progName ++ " help"

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

    -- Return True if running or auto-start is enabled
    return $ daemonRunning || optAutoStart opts

-- | Run a command with the daemon
runWithDaemon :: Command -> Options -> IO ()
runWithDaemon cmd opts = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> return path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- isDaemonRunning socketPath

    -- Start daemon if needed and auto-start is enabled
    unless daemonRunning $ do
        when (optAutoStart opts) $ do
            putStrLn "Daemon not running. Starting daemon..."
            startDaemonIfNeeded socketPath `catch` \(e :: SomeException) -> do
                hPutStrLn stderr $ "Error starting daemon: " ++ show e
                hPutStrLn stderr "Falling back to standalone mode."
                runStandalone cmd opts
                exitSuccess

    -- Get user credentials
    credentials <- getUserCredentials

    -- Connect to daemon
    result <- try $ DaemonClient.connectToDaemon socketPath credentials
    case result of
        Left (e :: SomeException) -> do
            hPutStrLn stderr $ "Error connecting to daemon: " ++ show e
            hPutStrLn stderr "Falling back to standalone mode."
            runStandalone cmd opts

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
executeDaemonCommand :: Command -> Options -> DaemonConnection -> IO Bool
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
            Right _ -> do
                putStrLn "Garbage collection completed successfully."
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
executeStoreCommand :: StoreCommand -> DaemonConnection -> Options -> IO Bool
executeStoreCommand storeCmd conn opts = case storeCmd of
    StoreAdd file -> do
        -- Add file to store
        result <- DaemonClient.addFileToStore conn file
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> do
                putStrLn $ "File added to store: " ++ file
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
            Right _ -> do
                putStrLn "Garbage collection completed successfully."
                return True

    StoreList -> do
        -- List store contents
        result <- DaemonClient.listStore conn
        case result of
            Left err -> do
                TIO.putStrLn $ "Error: " <> T.pack (show err)
                return False
            Right _ -> do
                -- Output would be handled by daemon
                return True

-- | Run a command in standalone mode (without daemon)
runStandalone :: Command -> Options -> IO ()
runStandalone cmd opts = do
    -- Resolve store path
    storePath <- resolveStorePath opts

    -- Resolve work directory
    workDir <- case optWorkDir opts of
        Just dir -> return dir
        Nothing -> getDefaultWorkDir

    -- Create directories if needed
    createDirectoryIfMissing True storePath
    createDirectoryIfMissing True workDir

    -- Set up build environment
    let env = initBuildEnv workDir storePath
                { verbosity = optVerbosity opts
                }

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
        executeDaemonCommand daemonCmd

    Help -> showHelp >> return True

    Version -> showVersion >> return True

-- | Execute a daemon control command
executeDaemonCommand :: DaemonCommand -> IO Bool
executeDaemonCommand cmd =
    -- Execute using the CLI module's daemon command handler
    daemonCommand [daemonCmdToString cmd] >>= \case
        ExitSuccess -> return True
        _ -> return False

-- | Convert daemon command to string
daemonCmdToString :: DaemonCommand -> String
daemonCmdToString = \case
    DaemonStart -> "start"
    DaemonStop -> "stop"
    DaemonStatus -> "status"
    DaemonRestart -> "restart"
    DaemonConfig -> "config"

-- | Get user credentials for daemon authentication
getUserCredentials :: IO UserCredentials
getUserCredentials = do
    -- Get username from environment or system
    username <- getUserName

    -- In a real implementation, we would have proper token management
    -- For now, just use a placeholder
    let token = "placeholder-token"

    return $ UserCredentials
        { username = username
        , token = token
        }

-- | Get current username
getUserName :: IO T.Text
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
                    -- Use a default
                    return "unknown"

-- | Calculate store path for a file
getStorePathForFile :: FilePath -> IO (Either BuildError StorePath)
getStorePathForFile file = do
    -- Check if file exists
    exists <- doesFileExist file
    if not exists
        then return $ Left $ InputNotFound file
        else do
            -- Read the file content
            content <- readFile file

            -- Calculate hash
            let name = T.pack $ takeFileName file
                hash = showHash $ hashText $ T.pack content

                -- Create store path
                path = StorePath hash name

            return $ Right path

-- Calculate file basename
takeFileName :: FilePath -> FilePath
takeFileName path =
    case reverse path of
        [] -> ""
        ('/':name) -> reverse name
        name ->
            case break (== '/') name of
                (file, _) -> reverse file

-- Parse a store path from string
parseStorePath :: FilePath -> Maybe StorePath
parseStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) -> Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

-- | Extend Options with autostart capability
optAutoStart :: Options -> Bool
optAutoStart opts = True  -- Default to auto-starting daemon
