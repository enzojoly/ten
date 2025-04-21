{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception (try, catch, SomeException, IOException)
import Control.Monad (when, unless, void, forM_)
import Data.Maybe (isJust, fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getArgs, getProgName, getEnv, lookupEnv)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr, IOMode(..), withFile)
import System.Posix.User (getUserEntryForName, getRealUserID, getLoginName)
import qualified System.Posix.User as User

import Ten.Core
import Ten.CLI
import Ten.Hash (hashText, showHash)
import Ten.Store (storePathExists, readFromStore)
import Ten.Daemon.Config (getDefaultSocketPath, getDefaultConfig, defaultDaemonSocketPath)
import Ten.Daemon.Client (isDaemonRunning, startDaemonIfNeeded, connectToDaemon, disconnectFromDaemon)
import Ten.Daemon.Auth (UserCredentials(..), createAuthToken)
import qualified Ten.Daemon.Client as DaemonClient
import qualified Ten.Daemon.Protocol as Protocol

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
            -- Determine if the command requires daemon privileges
            let requiresDaemon = commandRequiresDaemon cmd
            let prefersDaemon = commandPrefersDaemon cmd

            -- Check if daemon is explicitly disabled
            let forceDaemonOff = not (optUseDaemon opts)

            if requiresDaemon && forceDaemonOff then do
                -- Error: Command requires daemon but daemon is disabled
                hPutStrLn stderr $ "Error: Command '" ++ commandName cmd ++ "' requires the daemon, but daemon usage is disabled."
                exitFailure
            else if requiresDaemon || (prefersDaemon && not forceDaemonOff) then
                -- Run with daemon for required or preferred commands (unless disabled)
                runWithDaemon cmd opts
            else
                -- Run standalone for other commands
                runStandalone cmd opts

-- | Show usage information
showUsage :: IO ()
showUsage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " COMMAND [OPTIONS]"
    putStrLn ""
    putStrLn "Common commands:"
    putStrLn "  build FILE      Build a derivation from FILE"
    putStrLn "  eval FILE       Evaluate FILE to a derivation"
    putStrLn "  gc              Run garbage collection"
    putStrLn "  store add FILE  Add FILE to store"
    putStrLn "  store verify PATH  Verify store PATH integrity"
    putStrLn "  store path FILE    Calculate store path for FILE"
    putStrLn "  daemon start    Start the Ten daemon"
    putStrLn "  daemon stop     Stop the Ten daemon"
    putStrLn "  daemon status   Show daemon status"
    putStrLn ""
    putStrLn "For detailed help, run:"
    putStrLn $ "  " ++ progName ++ " help"

-- | Get command name for error messages
commandName :: Command -> String
commandName = \case
    Build _ -> "build"
    Eval _ -> "eval"
    GC -> "gc"
    Store cmd -> "store " ++ storeCommandName cmd
    Info _ -> "info"
    Daemon cmd -> "daemon " ++ daemonCommandName cmd
    Help -> "help"
    Version -> "version"

-- | Get store command name
storeCommandName :: StoreCommand -> String
storeCommandName = \case
    StoreAdd _ -> "add"
    StoreVerify _ -> "verify"
    StorePath _ -> "path"
    StoreGC -> "gc"
    StoreList -> "list"

-- | Get daemon command name
daemonCommandName :: DaemonCommand -> String
daemonCommandName = \case
    DaemonStart -> "start"
    DaemonStop -> "stop"
    DaemonStatus -> "status"
    DaemonRestart -> "restart"
    DaemonConfig -> "config"

-- | Determine if a command requires daemon privileges
commandRequiresDaemon :: Command -> Bool
commandRequiresDaemon = \case
    -- These commands modify the store and must run with daemon privileges
    Store (StoreAdd _) -> True
    Store StoreGC -> True
    Store StoreList -> True
    GC -> True
    -- Daemon commands to control the daemon itself
    Daemon _ -> True
    -- These commands don't strictly require the daemon
    _ -> False

-- | Determine if a command benefits from daemon but can run standalone
commandPrefersDaemon :: Command -> Bool
commandPrefersDaemon = \case
    -- Build and eval benefit from daemon's build management
    Build _ -> True
    Eval _ -> True
    -- Store verification benefits from daemon's validation
    Store (StoreVerify _) -> True
    -- Path and info operations can be local
    Store (StorePath _) -> False
    Info _ -> False
    -- Help and version are always local
    Help -> False
    Version -> False
    -- Already handled by commandRequiresDaemon
    _ -> False

-- | Run a command with the daemon
runWithDaemon :: Command -> Options -> IO ()
runWithDaemon cmd opts = do
    -- Determine socket path
    socketPath <- case optDaemonSocket opts of
        Just path -> return path
        Nothing -> getDefaultSocketPath

    -- Check if daemon is running
    daemonRunning <- isDaemonRunning socketPath

    -- Handle daemon commands specially
    case cmd of
        Daemon daemonCmd -> do
            -- These commands manage the daemon itself and don't need a connection
            executeDaemonCommand daemonCmd socketPath opts
            exitSuccess

        _ -> do
            -- For other commands, we need the daemon to be running
            unless daemonRunning $ do
                if optAutoStartDaemon opts then do
                    -- Try to start the daemon automatically
                    putStrLn "Daemon not running. Starting daemon..."
                    startResult <- try $ startDaemonIfNeeded socketPath
                    case startResult of
                        Left (e :: SomeException) -> do
                            hPutStrLn stderr $ "Error starting daemon: " ++ show e
                            if commandRequiresDaemon cmd then do
                                -- Command requires daemon, so we fail
                                hPutStrLn stderr $ "Command requires daemon but daemon could not be started."
                                exitFailure
                            else do
                                -- Try standalone mode for commands that don't require daemon
                                hPutStrLn stderr "Falling back to standalone mode."
                                runStandalone cmd opts
                                exitSuccess
                        Right _ -> do
                            -- Wait briefly for daemon to initialize
                            putStrLn "Daemon started successfully."
                else do
                    -- Auto-start disabled
                    hPutStrLn stderr "Daemon not running and auto-start is disabled."
                    if commandRequiresDaemon cmd then do
                        -- Command requires daemon, so we fail
                        hPutStrLn stderr $ "Command requires daemon but daemon is not running."
                        exitFailure
                    else do
                        -- Try standalone mode for commands that don't require daemon
                        hPutStrLn stderr "Falling back to standalone mode."
                        runStandalone cmd opts
                        exitSuccess

            -- Get user credentials for authentication
            credentials <- getUserCredentials

            -- Connect to daemon with authentication
            connectionResult <- try $ connectToDaemon socketPath credentials
            case connectionResult of
                Left (e :: SomeException) -> do
                    hPutStrLn stderr $ "Error connecting to daemon: " ++ show e
                    if commandRequiresDaemon cmd then do
                        -- Command requires daemon, so we fail
                        hPutStrLn stderr $ "Command requires daemon but connection failed."
                        exitFailure
                    else do
                        -- Try standalone mode for commands that don't require daemon
                        hPutStrLn stderr "Falling back to standalone mode."
                        runStandalone cmd opts

                Right conn -> do
                    -- Execute command using daemon connection
                    executeDaemonCommand' cmd opts conn

                    -- Disconnect from daemon
                    disconnectFromDaemon conn

-- | Execute a command using the daemon
executeDaemonCommand' :: Command -> Options -> DaemonClient.DaemonConnection -> IO ()
executeDaemonCommand' cmd opts conn = do
    let verbosity = optVerbosity opts

    -- Execute command with daemon
    result <- case cmd of
        Build filePath -> do
            when (verbosity > 0) $ putStrLn $ "Building " ++ filePath ++ " using daemon..."

            -- Check if file exists
            fileExists <- doesFileExist filePath
            unless fileExists $ do
                hPutStrLn stderr $ "Error: File not found: " ++ filePath
                exitFailure

            -- Read file content if it exists
            fileContent <- readFile filePath

            -- Request build from daemon
            DaemonClient.requestBuild conn filePath (Just $ T.pack fileContent) defaultBuildOptions

        Eval filePath -> do
            when (verbosity > 0) $ putStrLn $ "Evaluating " ++ filePath ++ " using daemon..."

            -- Check if file exists
            fileExists <- doesFileExist filePath
            unless fileExists $ do
                hPutStrLn stderr $ "Error: File not found: " ++ filePath
                exitFailure

            -- Read file content if it exists
            fileContent <- readFile filePath

            -- Request evaluation from daemon
            DaemonClient.requestEval conn filePath (Just $ T.pack fileContent) defaultBuildOptions

        Store storeCmd -> executeStoreCommand storeCmd opts conn

        GC -> do
            when (verbosity > 0) $ putStrLn "Running garbage collection with daemon..."
            DaemonClient.requestGC conn (optForce opts)

        Info path -> do
            when (verbosity > 0) $ putStrLn $ "Getting info for " ++ path ++ " using daemon..."
            DaemonClient.requestPathInfo conn path

        -- These should never be called here
        Daemon _ -> error "Daemon commands should be handled separately"
        Help -> error "Help command should be handled separately"
        Version -> error "Version command should be handled separately"

    -- Handle result
    case result of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ T.unpack (Protocol.errorMessage err)
            exitFailure

        Right response -> do
            -- Display response based on command type
            case cmd of
                Build _ ->
                    case response of
                        Protocol.BuildResultResponse buildResult -> displayBuildResult buildResult
                        _ -> putStrLn "Build completed successfully."

                Eval _ ->
                    case response of
                        Protocol.DerivationResponse deriv -> displayDerivation deriv
                        _ -> putStrLn "Evaluation completed successfully."

                Store _ ->
                    case response of
                        Protocol.StorePathResponse path -> putStrLn $ "Store path: " ++ T.unpack (storePathToText path)
                        Protocol.StoreVerifyResponse valid -> putStrLn $ "Path verification: " ++ if valid then "valid" else "invalid"
                        Protocol.StoreListResponse paths -> forM_ paths $ \p -> putStrLn $ T.unpack (storePathToText p)
                        _ -> putStrLn "Store operation completed successfully."

                GC ->
                    case response of
                        Protocol.GCResponse stats -> displayGCStats stats
                        _ -> putStrLn "Garbage collection completed successfully."

                Info _ ->
                    case response of
                        Protocol.PathInfoResponse info -> displayPathInfo info
                        _ -> putStrLn "Info command completed successfully."

                _ -> putStrLn "Command completed successfully."

            exitSuccess

-- | Execute a store command with daemon
executeStoreCommand :: StoreCommand -> Options -> DaemonClient.DaemonConnection -> IO (Either Protocol.ResponseError Protocol.DaemonResponse)
executeStoreCommand cmd opts conn =
    case cmd of
        StoreAdd filePath -> do
            -- Check if file exists
            fileExists <- doesFileExist filePath
            unless fileExists $ do
                hPutStrLn stderr $ "Error: File not found: " ++ filePath
                exitFailure

            -- Read file content
            fileContent <- readFile filePath

            -- Add to store via daemon
            DaemonClient.requestStoreAdd conn filePath (T.pack fileContent)

        StoreVerify path ->
            -- Verify path via daemon
            DaemonClient.requestStoreVerify conn path

        StorePath filePath ->
            -- Get store path for file
            DaemonClient.requestStorePath conn filePath

        StoreGC ->
            -- Run garbage collection
            DaemonClient.requestGC conn (optForce opts)

        StoreList ->
            -- List store contents
            DaemonClient.requestStoreList conn

-- | Execute a daemon management command
executeDaemonCommand :: DaemonCommand -> FilePath -> Options -> IO ()
executeDaemonCommand cmd socketPath opts =
    case cmd of
        DaemonStart -> do
            -- Check if daemon is already running
            daemonRunning <- isDaemonRunning socketPath
            if daemonRunning then
                putStrLn "Daemon is already running."
            else do
                putStrLn "Starting Ten daemon..."

                -- Start daemon process
                result <- try $ do
                    -- Get daemon executable path
                    daemonExe <- getDaemonExecutablePath

                    -- Build command line
                    let cmdLine = buildDaemonCommandLine socketPath opts

                    -- Start process
                    (_, _, _, ph) <- createProcess $ proc daemonExe cmdLine

                    -- Wait for process to initialize
                    exitCode <- waitForProcess ph
                    return exitCode

                case result of
                    Left (e :: SomeException) -> do
                        hPutStrLn stderr $ "Error starting daemon: " ++ show e
                        exitFailure

                    Right ExitSuccess -> do
                        putStrLn "Daemon started successfully."
                        exitSuccess

                    Right (ExitFailure code) -> do
                        hPutStrLn stderr $ "Daemon failed to start (exit code " ++ show code ++ ")."
                        exitFailure

        DaemonStop -> do
            -- Check if daemon is running
            daemonRunning <- isDaemonRunning socketPath
            if not daemonRunning then
                putStrLn "Daemon is not running."
            else do
                putStrLn "Stopping Ten daemon..."

                -- Get user credentials
                credentials <- getUserCredentials

                -- Connect to daemon
                result <- try $ do
                    conn <- connectToDaemon socketPath credentials

                    -- Send shutdown request
                    shutdownResult <- DaemonClient.requestShutdown conn

                    -- Disconnect (may fail if daemon is shutting down)
                    catch (disconnectFromDaemon conn) $ \(_ :: SomeException) -> return ()

                    return shutdownResult

                case result of
                    Left (e :: SomeException) -> do
                        hPutStrLn stderr $ "Error communicating with daemon: " ++ show e
                        exitFailure

                    Right (Left err) -> do
                        hPutStrLn stderr $ "Daemon shutdown error: " ++ T.unpack (Protocol.errorMessage err)
                        exitFailure

                    Right (Right _) -> do
                        putStrLn "Daemon shutdown requested successfully."
                        exitSuccess

        DaemonStatus -> do
            -- Check if daemon is running
            daemonRunning <- isDaemonRunning socketPath
            if not daemonRunning then do
                putStrLn "Daemon status: not running"
                exitSuccess
            else do
                -- Get user credentials
                credentials <- getUserCredentials

                -- Connect to daemon
                result <- try $ do
                    conn <- connectToDaemon socketPath credentials

                    -- Get status
                    statusResult <- DaemonClient.requestStatus conn

                    -- Disconnect
                    disconnectFromDaemon conn

                    return statusResult

                case result of
                    Left (e :: SomeException) -> do
                        hPutStrLn stderr $ "Error communicating with daemon: " ++ show e
                        putStrLn "Daemon status: running but not responding"
                        exitFailure

                    Right (Left err) -> do
                        hPutStrLn stderr $ "Daemon status error: " ++ T.unpack (Protocol.errorMessage err)
                        exitFailure

                    Right (Right response) ->
                        case response of
                            Protocol.StatusResponse status -> do
                                displayDaemonStatus status
                                exitSuccess
                            _ -> do
                                putStrLn "Daemon status: running"
                                exitSuccess

        DaemonRestart -> do
            -- Stop and start the daemon
            executeDaemonCommand DaemonStop socketPath opts
            executeDaemonCommand DaemonStart socketPath opts

        DaemonConfig -> do
            -- Show daemon configuration
            config <- getDefaultConfig
            displayDaemonConfig config
            exitSuccess

-- | Run a command in standalone mode
runStandalone :: Command -> Options -> IO ()
runStandalone cmd opts = do
    -- Resolve store path
    storePath <- resolveStorePath opts

    -- Create store directory if needed
    createDirectoryIfMissing True storePath

    -- Resolve work directory
    workDir <- resolveWorkDir opts
    createDirectoryIfMissing True workDir

    -- Create build environment
    let env = initBuildEnv workDir storePath
                { verbosity = optVerbosity opts
                , runMode = StandaloneMode  -- Ensure we're in standalone mode
                }

    -- Execute command
    case cmd of
        -- Commands that work in standalone mode
        Help -> do
            showHelp
            exitSuccess

        Version -> do
            showVersion
            exitSuccess

        Store (StorePath filePath) -> do
            -- Get store path for file
            result <- handleStorePathLocal env filePath
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ show err
                    exitFailure
                Right path -> do
                    putStrLn $ "Store path: " ++ T.unpack (storePathToText path)
                    exitSuccess

        Info path -> do
            -- Show path info (standalone version)
            result <- handleInfoLocal env path
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ show err
                    exitFailure
                Right info -> do
                    putStrLn $ "Path: " ++ path
                    putStrLn $ "Valid: " ++ if isJust info then "Yes" else "No"
                    exitSuccess

        -- Commands that shouldn't be run in standalone mode
        _ | commandRequiresDaemon cmd -> do
            hPutStrLn stderr $ "Error: Command '" ++ commandName cmd ++ "' requires the daemon."
            exitFailure

        -- Commands that can run in standalone but with limitations
        Build filePath -> do
            when (optVerbosity opts > 0) $
                putStrLn "Warning: Building in standalone mode has limitations."

            result <- handleBuildLocal env filePath
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ show err
                    exitFailure
                Right _ -> exitSuccess

        Eval filePath -> do
            when (optVerbosity opts > 0) $
                putStrLn "Warning: Evaluating in standalone mode has limitations."

            result <- handleEvalLocal env filePath
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ show err
                    exitFailure
                Right _ -> exitSuccess

        Store (StoreVerify path) -> do
            when (optVerbosity opts > 0) $
                putStrLn "Warning: Verification in standalone mode has limitations."

            result <- handleStoreVerifyLocal env path
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ show err
                    exitFailure
                Right valid -> do
                    putStrLn $ "Path: " ++ path
                    putStrLn $ "Valid: " ++ if valid then "Yes" else "No"
                    exitSuccess

        _ -> do
            hPutStrLn stderr "Error: Command not supported in standalone mode."
            exitFailure

-- | Get user credentials for daemon authentication
getUserCredentials :: IO UserCredentials
getUserCredentials = do
    -- Get username
    username <- getEffectiveUsername

    -- Get or create auth token
    tokenPath <- getAuthTokenPath
    createDirectoryIfMissing True (takeDirectory tokenPath)

    token <- if False then do  -- Token file handling logic would go here
        -- In a real implementation, we would read/create persistent tokens
        -- For now, generate a temporary token
        createAuthToken username
    else do
        -- Simple token for now
        createAuthToken username

    return $ UserCredentials
        { username = username
        , token = token
        }

-- | Get effective username
getEffectiveUsername :: IO Text
getEffectiveUsername = do
    -- Try to get login name
    eLoginName <- try getLoginName
    case eLoginName of
        Right name -> return $ T.pack name
        Left (_ :: SomeException) -> do
            -- Fallback to environment variables
            mUser <- lookupEnv "USER"
            case mUser of
                Just user -> return $ T.pack user
                Nothing -> do
                    -- Try USERNAME (Windows-style)
                    mUsername <- lookupEnv "USERNAME"
                    case mUsername of
                        Just username -> return $ T.pack username
                        Nothing -> do
                            -- Last resort: use UID
                            uid <- getRealUserID
                            return $ T.pack $ "user" ++ show uid

-- | Get path for auth token file
getAuthTokenPath :: IO FilePath
getAuthTokenPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".config/ten/auth_token"

-- | Build daemon command line
buildDaemonCommandLine :: FilePath -> Options -> [String]
buildDaemonCommandLine socketPath opts =
    ["daemon", "--socket", socketPath] ++
    (if optVerbosity opts > 0 then ["--verbose=" ++ show (optVerbosity opts)] else []) ++
    (case optStoreDir opts of
        Just dir -> ["--store", dir]
        Nothing -> [])

-- | Resolve store directory path
resolveStorePath :: Options -> IO FilePath
resolveStorePath opts = case optStoreDir opts of
    Just dir -> return dir
    Nothing -> do
        -- Try environment variable
        mStoreDir <- lookupEnv "TEN_STORE"
        case mStoreDir of
            Just dir -> return dir
            Nothing -> do
                -- Use default
                homeDir <- getHomeDirectory
                return $ homeDir </> ".ten/store"

-- | Resolve work directory path
resolveWorkDir :: Options -> IO FilePath
resolveWorkDir opts = case optWorkDir opts of
    Just dir -> return dir
    Nothing -> do
        -- Try environment variable
        mWorkDir <- lookupEnv "TEN_WORK_DIR"
        case mWorkDir of
            Just dir -> return dir
            Nothing -> do
                -- Use default
                homeDir <- getHomeDirectory
                return $ homeDir </> ".ten/work"

-- | Get daemon executable path
getDaemonExecutablePath :: IO FilePath
getDaemonExecutablePath = do
    -- Try environment variable
    mDaemonPath <- lookupEnv "TEN_DAEMON_PATH"
    case mDaemonPath of
        Just path -> return path
        Nothing -> do
            -- Use default relative path
            return "ten-daemon"  -- Assume it's in PATH

-- Local handler implementations for standalone mode

-- | Handle build command locally
handleBuildLocal :: BuildEnv -> FilePath -> IO (Either BuildError BuildResult)
handleBuildLocal env filePath = do
    -- Check if file exists
    fileExists <- doesFileExist filePath
    unless fileExists $
        return $ Left $ InputNotFound filePath

    -- In a real implementation, this would call into the build system
    -- For now, this is a placeholder
    putStrLn $ "Building " ++ filePath ++ " in standalone mode"

    -- Create a mock build result
    let result = BuildResult
            { resultOutputs = mempty  -- No outputs for mock
            , resultExitCode = ExitSuccess
            , resultLog = "Build completed (standalone mode)"
            , resultMetadata = mempty
            }

    return $ Right result

-- | Handle eval command locally
handleEvalLocal :: BuildEnv -> FilePath -> IO (Either BuildError Derivation)
handleEvalLocal env filePath = do
    -- Check if file exists
    fileExists <- doesFileExist filePath
    unless fileExists $
        return $ Left $ InputNotFound filePath

    -- In a real implementation, this would call into the evaluation system
    -- For now, this is a placeholder
    putStrLn $ "Evaluating " ++ filePath ++ " in standalone mode"

    -- Return a mock derivation
    return $ Right mockDerivation

-- | Handle store path command locally
handleStorePathLocal :: BuildEnv -> FilePath -> IO (Either BuildError StorePath)
handleStorePathLocal env filePath = do
    -- Check if file exists
    fileExists <- doesFileExist filePath
    unless fileExists $
        return $ Left $ InputNotFound filePath

    -- Read file content
    content <- readFile filePath
    let name = T.pack $ takeFileName filePath
    let hash = showHash $ hashText $ T.pack content

    -- Create store path
    return $ Right $ StorePath hash name

-- | Handle store verify command locally
handleStoreVerifyLocal :: BuildEnv -> FilePath -> IO (Either BuildError Bool)
handleStoreVerifyLocal env path = do
    -- Parse store path
    case parseStorePathFromString path of
        Nothing -> return $ Left $ StoreError "Invalid store path format"
        Just storePath -> do
            -- Check if path exists in store
            pathExists <- evalTen (storePathExists storePath) env
            case pathExists of
                Left err -> return $ Left err
                Right exists -> return $ Right exists

-- | Handle info command locally
handleInfoLocal :: BuildEnv -> FilePath -> IO (Either BuildError (Maybe StorePath))
handleInfoLocal env path = do
    -- Parse store path
    case parseStorePathFromString path of
        Nothing -> return $ Left $ StoreError "Invalid store path format"
        Just storePath -> do
            -- Check if path exists in store
            pathExists <- evalTen (storePathExists storePath) env
            case pathExists of
                Left err -> return $ Left err
                Right exists ->
                    if exists
                        then return $ Right $ Just storePath
                        else return $ Right Nothing

-- | Parse store path from string
parseStorePathFromString :: String -> Maybe StorePath
parseStorePathFromString path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) | not (null hash) && not (null name) ->
            Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

-- Helper utilities

-- | Display build result
displayBuildResult :: Protocol.BuildResult -> IO ()
displayBuildResult result = do
    putStrLn "Build completed successfully:"
    putStrLn $ "  Outputs: " ++ show (length $ Protocol.resultOutputs result) ++ " paths"
    putStrLn $ "  Exit code: " ++ case Protocol.resultExitCode result of
                                      ExitSuccess -> "success"
                                      ExitFailure code -> "failure (" ++ show code ++ ")"

    -- Show build log if present
    unless (T.null $ Protocol.resultLog result) $ do
        putStrLn "Build log:"
        TIO.putStrLn $ Protocol.resultLog result

-- | Display derivation
displayDerivation :: Protocol.Derivation -> IO ()
displayDerivation deriv = do
    putStrLn "Derivation:"
    putStrLn $ "  Name: " ++ T.unpack (Protocol.derivName deriv)
    putStrLn $ "  Hash: " ++ T.unpack (Protocol.derivHash deriv)
    putStrLn $ "  System: " ++ T.unpack (Protocol.derivSystem deriv)
    putStrLn $ "  Inputs: " ++ show (length $ Protocol.derivInputs deriv)
    putStrLn $ "  Outputs: " ++ show (length $ Protocol.derivOutputs deriv)

-- | Display garbage collection stats
displayGCStats :: Protocol.GCStats -> IO ()
displayGCStats stats = do
    putStrLn "Garbage collection completed:"
    putStrLn $ "  Total paths: " ++ show (Protocol.gcTotal stats)
    putStrLn $ "  Live paths: " ++ show (Protocol.gcLive stats)
    putStrLn $ "  Collected: " ++ show (Protocol.gcCollected stats)
    putStrLn $ "  Space freed: " ++ formatBytes (Protocol.gcBytes stats)
    putStrLn $ "  Elapsed time: " ++ formatElapsedTime (Protocol.gcElapsedTime stats)

-- | Display path info
displayPathInfo :: Protocol.PathInfo -> IO ()
displayPathInfo info = do
    putStrLn $ "Path: " ++ T.unpack (storePathToText $ Protocol.pathInfoPath info)
    putStrLn $ "Valid: " ++ if Protocol.pathInfoIsValid info then "Yes" else "No"
    putStrLn $ "Registration time: " ++ show (Protocol.pathInfoRegistrationTime info)
    case Protocol.pathInfoDeriver info of
        Just deriver -> putStrLn $ "Deriver: " ++ T.unpack (storePathToText deriver)
        Nothing -> putStrLn "Deriver: None"

-- | Display daemon status
displayDaemonStatus :: Protocol.DaemonStatus -> IO ()
displayDaemonStatus status = do
    putStrLn $ "Daemon status: " ++ T.unpack (Protocol.daemonStatus status)
    putStrLn $ "Uptime: " ++ formatElapsedTime (Protocol.daemonUptime status)
    putStrLn $ "Active builds: " ++ show (Protocol.daemonActiveBuilds status)
    putStrLn $ "Completed builds: " ++ show (Protocol.daemonCompletedBuilds status)
    putStrLn $ "Failed builds: " ++ show (Protocol.daemonFailedBuilds status)
    putStrLn $ "GC roots: " ++ show (Protocol.daemonGcRoots status)
    putStrLn $ "Store size: " ++ formatBytes (Protocol.daemonStoreSize status)
    putStrLn $ "Store paths: " ++ show (Protocol.daemonStorePaths status)

-- | Display daemon configuration
displayDaemonConfig :: Protocol.DaemonConfig -> IO ()
displayDaemonConfig config = do
    putStrLn "Daemon configuration:"
    putStrLn $ "  Version: " ++ T.unpack (Protocol.daemonVersion config)
    putStrLn $ "  Socket path: " ++ Protocol.daemonSocketPath config
    putStrLn $ "  Store path: " ++ Protocol.daemonStorePath config
    putStrLn $ "  Max jobs: " ++ show (Protocol.daemonMaxJobs config)
    case Protocol.daemonGcInterval config of
        Just interval -> putStrLn $ "  GC interval: " ++ show interval ++ " seconds"
        Nothing -> putStrLn "  GC interval: Manual only"
    putStrLn $ "  Allowed users: " ++ show (Protocol.daemonAllowedUsers config)

-- | Format bytes in human-readable format
formatBytes :: Integer -> String
formatBytes bytes
    | bytes < 1024 = show bytes ++ " B"
    | bytes < 1024*1024 = show (bytes `div` 1024) ++ " KiB"
    | bytes < 1024*1024*1024 = show (bytes `div` (1024*1024)) ++ " MiB"
    | otherwise = show (bytes `div` (1024*1024*1024)) ++ " GiB"

-- | Format elapsed time in human-readable format
formatElapsedTime :: Double -> String
formatElapsedTime seconds
    | seconds < 60 = show (round seconds) ++ " seconds"
    | seconds < 3600 = show (round $ seconds / 60) ++ " minutes"
    | otherwise = show (round $ seconds / 3600) ++ " hours"

-- Helpers for auto-starting daemon
optAutoStartDaemon :: Options -> Bool
optAutoStartDaemon _ = True  -- Default to auto-starting daemon

-- Convert StorePath to Text
storePathToText :: StorePath -> Text
storePathToText (StorePath hash name) = hash <> "-" <> name

-- Default build options
defaultBuildOptions :: Protocol.BuildRequestInfo
defaultBuildOptions = Protocol.BuildRequestInfo
    { Protocol.buildArgs = []
    , Protocol.buildTimeout = Nothing
    , Protocol.buildPriority = 50  -- Normal priority
    , Protocol.buildNotifyURL = Nothing
    , Protocol.buildAllowRecursive = True
    }

-- Mock derivation for testing
mockDerivation :: Derivation
mockDerivation = error "Mock derivation not implemented"  -- Would be replaced with actual implementation
