{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-} -- Added for privilege types if needed

module Main where

import Control.Exception (try, catch, SomeException, IOException)
import Control.Monad (when, unless, void, forM_)
import Data.Maybe (isJust, fromMaybe, isNothing, listToMaybe, catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS -- Needed for reading file content
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory)
import System.Environment (getArgs, getProgName, lookupEnv)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (hPutStrLn, stderr, IOMode(..), withFile)
import System.Posix.User (getUserEntryForName, getRealUserID, getLoginName)
import qualified System.Posix.User as User
import Data.List (partition, isPrefixOf) -- For simple arg parsing

-- Import from the Ten library (relies on ten.cabal exposing these)
import Ten (
    -- Core
    TenM, BuildEnv(..), BuildState, BuildError(..), StorePath(..), Derivation(..),
    BuildResult(..), RunMode(..), initBuildEnv, storePathToText, textToStorePath, parseStorePath,
    runTen, evalTen, buildTen, -- Make sure errors are exported
    -- Store
    StoreAccessOps(..), StoreContentOps(..), StoreQueryOps(..),
    -- GC
    GCStats(..),
    -- Protocol / Daemon Interaction related
    UserCredentials(..), DaemonCapability(..), DaemonConnection, DaemonConfig(..),
    ProtocolVersion(..),
    -- Specific client requests
    buildFile, evalFile, buildDerivation, requestGC, requestStoreAdd, requestStoreVerify,
    requestStorePath, requestStoreList, requestStatus, requestShutdown, requestPathInfo,
    -- Protocol types used in responses
    DaemonResponse(..), PathInfo(..)
    )
-- Direct imports if not exposed via Ten.hs or for clarity
import qualified Ten.Daemon.Client as DaemonClient -- Use qualified if direct access needed
import qualified Ten.Daemon.Protocol as Protocol -- For ResponseError etc.

-- | Command-line options structure
data Options = Options {
    optVerbosity      :: Int,
    optStoreDir       :: Maybe FilePath,
    optWorkDir        :: Maybe FilePath,
    optDaemonSocket   :: Maybe FilePath,
    optUseDaemon      :: Bool,
    optAutoStartDaemon:: Bool,
    optForce          :: Bool
    -- Add other options as needed
} deriving (Show, Eq)

-- | Default options
defaultOptions :: Options
defaultOptions = Options {
    optVerbosity = 1,
    optStoreDir = Nothing,
    optWorkDir = Nothing,
    optDaemonSocket = Nothing,
    optUseDaemon = True,
    optAutoStartDaemon = True,
    optForce = False
    -- Initialize other options
}

-- | Main command categories
data Command
    = Build FilePath
    | Eval FilePath
    | GC
    | Store StoreCommand
    | Info String -- StorePath or FilePath? Assuming String for now
    | Daemon DaemonCommand
    | Help
    | Version
    deriving (Show, Eq)

-- | Store sub-commands
data StoreCommand
    = StoreAdd FilePath
    | StoreVerify String -- Assuming String represents StorePath text
    | StorePath FilePath
    | StoreGC
    | StoreList
    deriving (Show, Eq)

-- | Daemon sub-commands
data DaemonCommand
    = DaemonStart
    | DaemonStop
    | DaemonStatus
    | DaemonRestart
    -- | DaemonConfig -- Removed as config module was removed
    deriving (Show, Eq)

-- | Simple argument parser (replace with GetOpt if needed)
parseArgs :: [String] -> Either String (Command, Options)
parseArgs [] = Left "No command specified."
parseArgs (cmd:args) =
    let (optsArgs, nonOptsArgs) = partition isOpt args
        opts = parseOptions optsArgs defaultOptions -- Implement parseOptions
    in case cmd of
        "build"   -> case nonOptsArgs of [fp] -> Right (Build fp, opts); _ -> Left "build requires exactly one FILE argument."
        "eval"    -> case nonOptsArgs of [fp] -> Right (Eval fp, opts); _ -> Left "eval requires exactly one FILE argument."
        "gc"      -> case nonOptsArgs of []   -> Right (GC, opts); _ -> Left "gc takes no arguments."
        "store"   -> parseStoreCmd nonOptsArgs opts
        "info"    -> case nonOptsArgs of [p]  -> Right (Info p, opts); _ -> Left "info requires exactly one PATH argument."
        "daemon"  -> parseDaemonCmd nonOptsArgs opts
        "help"    -> Right (Help, opts)
        "version" -> Right (Version, opts)
        _         -> Left $ "Unknown command: " ++ cmd
  where
    isOpt ('-':_) = True
    isOpt _       = False

    parseStoreCmd :: [String] -> Options -> Either String (Command, Options)
    parseStoreCmd [] _ = Left "store requires a sub-command (add, verify, path, gc, list)."
    parseStoreCmd (sub:rest) opts = case sub of
        "add"    -> case rest of [fp] -> Right (Store (StoreAdd fp), opts); _ -> Left "store add requires exactly one FILE argument."
        "verify" -> case rest of [p]  -> Right (Store (StoreVerify p), opts); _ -> Left "store verify requires exactly one PATH argument."
        "path"   -> case rest of [fp] -> Right (Store (StorePath fp), opts); _ -> Left "store path requires exactly one FILE argument."
        "gc"     -> case rest of []   -> Right (Store StoreGC, opts); _ -> Left "store gc takes no arguments."
        "list"   -> case rest of []   -> Right (Store StoreList, opts); _ -> Left "store list takes no arguments."
        _        -> Left $ "Unknown store sub-command: " ++ sub

    parseDaemonCmd :: [String] -> Options -> Either String (Command, Options)
    parseDaemonCmd [] _ = Left "daemon requires a sub-command (start, stop, status, restart)." -- Removed config
    parseDaemonCmd (sub:rest) opts = case sub of
        "start"   -> Right (Daemon DaemonStart, opts)
        "stop"    -> Right (Daemon DaemonStop, opts)
        "status"  -> Right (Daemon DaemonStatus, opts)
        "restart" -> Right (Daemon DaemonRestart, opts)
        -- "config"  -> Right (Daemon DaemonConfig, opts) -- Removed config
        _         -> Left $ "Unknown daemon sub-command: " ++ sub

    -- Implement a function to parse Options from the args starting with '-'
    parseOptions :: [String] -> Options -> Options
    parseOptions args initialOpts = foldl parseOpt initialOpts args
      where
        parseOpt acc arg
          | arg == "-v" || arg == "--verbose" = acc { optVerbosity = optVerbosity acc + 1 }
          | "--socket=" `isPrefixOf` arg = acc { optDaemonSocket = Just $ drop (length "--socket=") arg }
          | "--store=" `isPrefixOf` arg = acc { optStoreDir = Just $ drop (length "--store=") arg }
          | "--work=" `isPrefixOf` arg = acc { optWorkDir = Just $ drop (length "--work=") arg }
          | arg == "--no-daemon" = acc { optUseDaemon = False }
          | arg == "--no-autostart" = acc { optAutoStartDaemon = False }
          | arg == "-f" || arg == "--force" = acc { optForce = True }
          | otherwise = acc -- Ignore unknown options for simplicity

-- End of CLI Parsing Logic

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
            -- Handle help and version directly
            case cmd of
                Help -> showUsage >> exitSuccess
                Version -> showVersion >> exitSuccess
                _ -> do
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
    putStrLn "  store verify PATH Verify store PATH integrity"
    putStrLn "  store path FILE   Calculate store path for FILE"
    putStrLn "  store list      List store contents"
    putStrLn "  info PATH       Show information about a store path"
    putStrLn "  daemon start    Start the Ten daemon"
    putStrLn "  daemon stop     Stop the Ten daemon"
    putStrLn "  daemon status   Show daemon status"
    putStrLn "  daemon restart  Restart the Ten daemon"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  -v, --verbose      Increase verbosity"
    putStrLn "  --socket=PATH    Specify daemon socket path"
    putStrLn "  --store=PATH     Specify store directory path"
    putStrLn "  --work=PATH      Specify working directory path"
    putStrLn "  --no-daemon      Force standalone mode (may fail for some commands)"
    putStrLn "  --no-autostart   Disable automatic daemon starting"
    putStrLn "  -f, --force        Force operation (e.g., GC)"
    putStrLn ""
    putStrLn "For detailed help, run:"
    putStrLn $ "  " ++ progName ++ " help"

-- | Show version information
showVersion :: IO ()
showVersion = putStrLn "Ten Client version 0.1.0" -- Adjust version as needed

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
    -- DaemonConfig -> "config" -- Removed

-- | Determine if a command requires daemon privileges
commandRequiresDaemon :: Command -> Bool
commandRequiresDaemon = \case
    Store (StoreAdd _) -> True
    Store StoreGC -> True
    Store StoreList -> True -- Listing might require DB access via daemon
    GC -> True
    Daemon _ -> True
    _ -> False

-- | Determine if a command benefits from daemon but can run standalone
commandPrefersDaemon :: Command -> Bool
commandPrefersDaemon = \case
    Build _ -> True
    Eval _ -> True
    Store (StoreVerify _) -> True
    Store (StorePath _) -> False -- Can be done locally
    Info _ -> True -- Info might require DB access via daemon
    Help -> False
    Version -> False
    _ -> False

-- | Run a command with the daemon
runWithDaemon :: Command -> Options -> IO ()
runWithDaemon cmd opts = do
    socketPath <- resolveSocketPath opts

    daemonRunning <- isDaemonRunning socketPath

    case cmd of
        Daemon daemonCmd -> executeDaemonCommand daemonCmd socketPath opts >> exitSuccess
        _ -> do
            unless daemonRunning $ handleDaemonNotRunning cmd opts socketPath

            credentials <- getUserCredentials
            connectionResult <- try $ connectToDaemon socketPath credentials

            case connectionResult of
                Left err -> handleConnectionError cmd opts err
                Right conn -> bracket (pure conn) disconnectFromDaemon (executeDaemonCommand' cmd opts)

-- | Handle the case where the daemon isn't running when needed
handleDaemonNotRunning :: Command -> Options -> FilePath -> IO ()
handleDaemonNotRunning cmd opts socketPath = do
    if optAutoStartDaemon opts then do
        putStrLn "Daemon not running. Starting daemon..."
        startResult <- try $ startDaemonIfNeeded socketPath
        case startResult of
            Left (e :: SomeException) -> do
                hPutStrLn stderr $ "Error starting daemon: " ++ show e
                if commandRequiresDaemon cmd then do
                    hPutStrLn stderr "Command requires daemon but daemon could not be started."
                    exitFailure
                else fallbackToStandalone cmd opts
            Right _ -> putStrLn "Daemon started successfully." >> TIO.putStr "" -- Continue
    else do
        hPutStrLn stderr "Daemon not running and auto-start is disabled."
        if commandRequiresDaemon cmd then do
            hPutStrLn stderr "Command requires daemon but daemon is not running."
            exitFailure
        else fallbackToStandalone cmd opts

-- | Handle errors during daemon connection
handleConnectionError :: Command -> Options -> SomeException -> IO ()
handleConnectionError cmd opts err = do
    hPutStrLn stderr $ "Error connecting to daemon: " ++ show err
    if commandRequiresDaemon cmd then do
        hPutStrLn stderr "Command requires daemon but connection failed."
        exitFailure
    else fallbackToStandalone cmd opts

-- | Fallback to standalone mode with a message
fallbackToStandalone :: Command -> Options -> IO ()
fallbackToStandalone cmd opts = do
    hPutStrLn stderr "Falling back to standalone mode."
    runStandalone cmd opts
    exitSuccess

-- | Execute a command using the daemon connection
executeDaemonCommand' :: Command -> Options -> DaemonConnection 'Builder -> IO ()
executeDaemonCommand' cmd opts conn = do
    let verbosity = optVerbosity opts

    resultEither <- case cmd of
        Build filePath -> do
            when (verbosity > 0) $ putStrLn $ "Building " ++ filePath ++ " using daemon..."
            DaemonClient.buildFile conn filePath -- Use the function from Client

        Eval filePath -> do
            when (verbosity > 0) $ putStrLn $ "Evaluating " ++ filePath ++ " using daemon..."
            fmap DerivationResponse <$> DaemonClient.evalFile conn filePath -- Use the function from Client

        Store storeCmd -> executeStoreCommand storeCmd opts conn

        GC -> do
            when (verbosity > 0) $ putStrLn "Running garbage collection with daemon..."
            fmap GCResultResponse <$> DaemonClient.collectGarbage conn (optForce opts) -- Use the function from Client

        Info path -> do
            when (verbosity > 0) $ putStrLn $ "Getting info for " ++ path ++ " using daemon..."
            -- Assuming path is StorePath text representation
            case parseStorePath (T.pack path) of
                 Nothing -> return $ Left $ ProtocolError $ "Invalid path format for info: " <> T.pack path
                 Just sp -> fmap PathInfoResponse <$> DaemonClient.requestPathInfo conn sp -- Needs implementation in Client

        -- Should not happen due to earlier routing
        Daemon _ -> error "Daemon command routed incorrectly"
        Help -> error "Help command routed incorrectly"
        Version -> error "Version command routed incorrectly"

    -- Handle result
    case resultEither of
        Left err -> do
            hPutStrLn stderr $ "Error: " <> buildErrorToText err -- Use buildErrorToText
            exitFailure
        Right response -> do
            displayResponse cmd response
            exitSuccess

-- | Display the response from the daemon
displayResponse :: Command -> DaemonResponse -> IO ()
displayResponse cmd response = case (cmd, response) of
    (Build _, BuildResultResponse res) -> displayBuildResult res
    (Eval _, EvalResponse deriv) -> displayDerivation deriv
    (Eval _, DerivationResponse deriv) -> displayDerivation deriv -- Handle potential alternative
    (Store (StoreAdd _), StoreAddResponse path) -> putStrLn $ "Added: " ++ T.unpack (storePathToText path)
    (Store (StorePath _), StorePathResponse path) -> putStrLn $ "Path: " ++ T.unpack (storePathToText path)
    (Store (StoreVerify _), StoreVerifyResponse valid) -> putStrLn $ "Verification: " ++ if valid then "valid" else "invalid"
    (Store StoreList, StoreListResponse paths) -> mapM_ (putStrLn . T.unpack . storePathToText) paths
    (GC, GCResultResponse stats) -> displayGCStats stats
    (Store StoreGC, GCResultResponse stats) -> displayGCStats stats
    (Info _, PathInfoResponse info) -> displayPathInfo info
    (_, ErrorResponse err) -> hPutStrLn stderr $ "Daemon Error: " <> buildErrorToText err
    (_, SuccessResponse) -> putStrLn "Operation successful."
    (_, _) -> putStrLn $ "Received unexpected response: " ++ show response -- Generic fallback

-- | Execute a store command with daemon
executeStoreCommand :: StoreCommand -> Options -> DaemonConnection 'Builder -> IO (Either BuildError DaemonResponse)
executeStoreCommand cmd opts conn = case cmd of
    StoreAdd filePath -> DaemonClient.addFileToStore conn filePath >>= handleStoreResponse StoreAddResponse
    StoreVerify pathStr -> case parseStorePath (T.pack pathStr) of
        Nothing -> return $ Left $ StoreError "Invalid store path format"
        Just sp -> DaemonClient.verifyStorePath conn sp >>= handleStoreResponse StoreVerifyResponse
    StorePath filePath -> DaemonClient.getStorePathForFile conn filePath >>= handleStoreResponse StorePathResponse
    StoreGC -> fmap GCResultResponse <$> DaemonClient.collectGarbage conn (optForce opts)
    StoreList -> fmap StoreListResponse <$> DaemonClient.listStore conn

-- | Helper to wrap Store command results
handleStoreResponse :: (a -> DaemonResponse) -> Either BuildError a -> IO (Either BuildError DaemonResponse)
handleStoreResponse constructor result = return $ fmap constructor result


-- | Execute a daemon management command
executeDaemonCommand :: DaemonCommand -> FilePath -> Options -> IO ()
executeDaemonCommand cmd socketPath opts = case cmd of
    DaemonStart -> do
        daemonRunning <- isDaemonRunning socketPath
        if daemonRunning then
            putStrLn "Daemon is already running."
        else do
            putStrLn "Starting Ten daemon..."
            result <- try $ startDaemonProcess socketPath opts -- Changed function name
            case result of
                Left (e :: SomeException) -> hPutStrLn stderr $ "Error starting daemon: " ++ show e >> exitFailure
                Right _ -> putStrLn "Daemon started successfully." >> exitSuccess -- Assuming startDaemonProcess handles exit

    DaemonStop -> do
        daemonRunning <- isDaemonRunning socketPath
        if not daemonRunning then
            putStrLn "Daemon is not running."
        else do
            putStrLn "Stopping Ten daemon..."
            credentials <- getUserCredentials -- Needed to potentially send shutdown command
            result <- try $ connectToDaemon socketPath credentials
            case result of
                Left err -> hPutStrLn stderr ("Failed to connect to daemon to stop it: " ++ show err) >> exitFailure
                Right conn -> do
                    stopResult <- try $ DaemonClient.shutdownDaemon conn -- Use the client function
                    disconnectFromDaemon conn
                    case stopResult of
                        Left (e :: SomeException) -> hPutStrLn stderr ("Error sending shutdown command: " ++ show e) >> exitFailure
                        Right (Left err) -> hPutStrLn stderr ("Daemon shutdown error: " <> buildErrorToText err) >> exitFailure
                        Right (Right _) -> putStrLn "Daemon stopped successfully." >> exitSuccess

    DaemonStatus -> do
        daemonRunning <- isDaemonRunning socketPath
        if not daemonRunning then do
            putStrLn "Daemon status: not running"
            exitSuccess
        else do
            credentials <- getUserCredentials
            result <- try $ connectToDaemon socketPath credentials
            case result of
                Left err -> putStrLn "Daemon status: running but not responding" >> exitFailure
                Right conn -> do
                    statusResult <- try $ DaemonClient.getDaemonStatus conn
                    disconnectFromDaemon conn
                    case statusResult of
                         Left (e :: SomeException) -> putStrLn "Daemon status: running but failed to get status" >> exitFailure
                         Right (Left err) -> putStrLn ("Daemon status error: " <> buildErrorToText err) >> exitFailure
                         Right (Right status) -> displayDaemonStatus status >> exitSuccess

    DaemonRestart -> do
        putStrLn "Restarting daemon..."
        executeDaemonCommand DaemonStop socketPath opts -- Errors handled within
        -- Add a small delay to allow the socket to be released
        Control.Concurrent.threadDelay 500000 -- 0.5 seconds
        executeDaemonCommand DaemonStart socketPath opts -- Errors handled within

    -- DaemonConfig -> displayDaemonConfig defaultDaemonConfig >> exitSuccess -- Removed

-- Placeholder: Start the daemon process (implementation likely in TenDaemon.hs or similar)
startDaemonProcess :: FilePath -> Options -> IO ()
startDaemonProcess socketPath opts = do
    -- Find daemon executable
    mDaemonExe <- System.Process.findExecutable "ten-daemon"
    case mDaemonExe of
        Nothing -> hPutStrLn stderr "Error: ten-daemon executable not found in PATH." >> exitFailure
        Just daemonExe -> do
            -- Construct command line arguments for the daemon process
            let daemonArgs = ["start", "--socket=" ++ socketPath] ++ buildDaemonOpts opts
            putStrLn $ "Executing: " ++ daemonExe ++ " " ++ unwords daemonArgs
            -- Create the process (consider using System.Process.Typed for better control)
            (_, _, _, ph) <- System.Process.createProcess (System.Process.proc daemonExe daemonArgs)
            -- Optionally wait or check if it started correctly
            -- For now, just assume it starts if createProcess succeeds
            return ()

buildDaemonOpts :: Options -> [String]
buildDaemonOpts opts = catMaybes [
    fmap (\s -> "--store=" ++ s) (optStoreDir opts),
    fmap (\s -> "--work=" ++ s) (optWorkDir opts) -- Assuming work dir is a daemon option
    -- Add other daemon-specific options based on TenDaemon's CLI
    ]


-- | Run a command in standalone mode
runStandalone :: Command -> Options -> IO ()
runStandalone cmd opts = do
    storePath <- resolveStorePath opts
    workDir <- resolveWorkDir opts
    createDirectoryIfMissing True storePath
    createDirectoryIfMissing True workDir

    let env = initBuildEnv workDir storePath -- Removed verbosity here, handle logging differently
                { runMode = StandaloneMode }

    -- Setup minimal TenM environment for standalone execution
    let initialState = initBuildState Core.Eval (Core.BuildIdFromInt 0) -- Use Eval phase for local ops initially

    -- Execute command locally using runTen
    result <- runTen Core.sEval Core.sBuilder (handleStandaloneCommand cmd opts) env initialState

    case result of
        Left err -> hPutStrLn stderr ("Standalone Error: " <> buildErrorToText err) >> exitFailure
        Right (output, _) -> TIO.putStrLn output >> exitSuccess


-- | Handle standalone commands within TenM
handleStandaloneCommand :: Command -> Options -> TenM 'Eval 'Builder Text
handleStandaloneCommand cmd opts = case cmd of
    Help -> return $ T.pack usageString -- Assuming usageString is defined
    Version -> return $ T.pack versionString -- Assuming versionString is defined

    Store (StorePath filePath) -> do
        liftIO (doesFileExist filePath) >>= \exists -> unless exists (throwError $ InputNotFound filePath)
        contentBS <- liftIO $ BS.readFile filePath
        let content = TE.decodeUtf8 contentBS -- Simple decode, maybe handle errors
        let name = T.pack $ takeFileName filePath
        let hash = showHash $ hashText content
        let storePath = StorePath hash name
        return $ "Store path: " <> storePathToText storePath

    Info pathStr -> do
        case parseStorePath (T.pack pathStr) of
            Nothing -> throwError $ StoreError "Invalid store path format"
            Just sp -> do
                exists <- checkStorePathExists sp -- Use the typeclass method
                return $ "Path: " <> T.pack pathStr <> "\nValid: " <> (if exists then "Yes" else "No")

    Build filePath -> throwError $ BuildError "Build command not supported in standalone mode (yet)."
    Eval filePath -> throwError $ BuildError "Eval command not supported in standalone mode (yet)."
    Store (StoreVerify pathStr) -> throwError $ BuildError "Store verify not supported in standalone mode (yet)."

    -- Commands that require daemon
    _ | commandRequiresDaemon cmd ->
        throwError $ BuildError $ "Command '" <> T.pack (commandName cmd) <> "' requires the daemon."

    _ -> throwError $ BuildError $ "Command '" <> T.pack (commandName cmd) <> "' not supported in standalone mode."

-- Placeholder strings for usage/version
usageString :: String
usageString = "Usage: ... (full help text here)"

versionString :: String
versionString = "Ten Client version 0.1.0 (standalone)"


-- | Get user credentials for daemon authentication
getUserCredentials :: IO UserCredentials
getUserCredentials = do
    username <- getEffectiveUsername
    -- For simplicity in this example, we'll use a fixed/dummy token.
    -- A real implementation needs secure token generation/storage/retrieval.
    let token = "dummy-token-for-" <> username
    return $ UserCredentials username token Builder -- Client always requests Builder tier initially

-- | Get effective username (simplified)
getEffectiveUsername :: IO Text
getEffectiveUsername = T.pack <$> getLoginName `catch` \(_ :: IOException) -> return "unknown_user"

-- | Get path for auth token file (dummy implementation)
getAuthTokenPath :: IO FilePath
getAuthTokenPath = do
    homeDir <- getHomeDirectory
    return $ homeDir </> ".config/ten/auth_token" -- Example path

-- | Create a dummy auth token
createAuthToken :: Text -> IO Text
createAuthToken user = return $ "temp-token-" <> user

-- | Resolve socket path
resolveSocketPath :: Options -> IO FilePath
resolveSocketPath opts = case optDaemonSocket opts of
    Just path -> return path
    Nothing -> getDefaultSocketPath -- Use the function from the library

-- | Resolve store directory path
resolveStorePath :: Options -> IO FilePath
resolveStorePath opts = case optStoreDir opts of
    Just dir -> return dir
    Nothing -> lookupEnv "TEN_STORE" >>= \case
        Just path -> return path
        Nothing -> do
            home <- getHomeDirectory
            return $ home </> ".ten/store" -- Default path

-- | Resolve work directory path
resolveWorkDir :: Options -> IO FilePath
resolveWorkDir opts = case optWorkDir opts of
    Just dir -> return dir
    Nothing -> lookupEnv "TEN_WORK_DIR" >>= \case
        Just path -> return path
        Nothing -> do
            home <- getHomeDirectory
            return $ home </> ".ten/work" -- Default path

-- | Get daemon executable path (simplified)
getDaemonExecutablePath :: IO FilePath
getDaemonExecutablePath = fromMaybe "ten-daemon" <$> lookupEnv "TEN_DAEMON_PATH"

-- Local handler implementations for standalone mode

-- | Handle build command locally (Placeholder)
handleBuildLocal :: BuildEnv -> FilePath -> IO (Either BuildError BuildResult)
handleBuildLocal env filePath = return $ Left $ BuildError "Standalone build not implemented"

-- | Handle eval command locally (Placeholder)
handleEvalLocal :: BuildEnv -> FilePath -> IO (Either BuildError Derivation)
handleEvalLocal env filePath = return $ Left $ BuildError "Standalone eval not implemented"

-- | Handle store verify command locally (Placeholder)
handleStoreVerifyLocal :: BuildEnv -> String -> IO (Either BuildError Bool)
handleStoreVerifyLocal env pathStr = return $ Left $ BuildError "Standalone verify not implemented"


-- Display functions (stubs - replace with actual implementations)
displayBuildResult :: BuildResult -> IO ()
displayBuildResult res = putStrLn $ "Build Result: (Exit: " ++ show (resultExitCode res) ++ ")"

displayDerivation :: Derivation -> IO ()
displayDerivation deriv = putStrLn $ "Derivation: " ++ T.unpack (derivName deriv)

displayGCStats :: GCStats -> IO ()
displayGCStats stats = putStrLn $ "GC Stats: Collected " ++ show (gcCollected stats)

displayPathInfo :: PathInfo -> IO ()
displayPathInfo info = putStrLn $ "Path Info: " ++ T.unpack (storePathToText $ pathInfoPath info)

displayDaemonStatus :: DaemonStatus -> IO ()
displayDaemonStatus status = putStrLn $ "Daemon Status: " ++ T.unpack (daemonStatus status)

displayDaemonConfig :: DaemonConfig -> IO ()
displayDaemonConfig config = putStrLn $ "Daemon Config: Socket=" ++ daemonSocketPath config

-- Mock derivation for standalone testing
mockDerivation :: Derivation
mockDerivation = Derivation "mock-standalone" "mockhash" (StorePath "builderhash" "builder") [] Set.empty Set.empty Map.empty "mock-system" ApplicativeStrategy Map.empty
