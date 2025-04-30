{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Ten.CLI (
    -- Core CLI types
    Command(..),
    StoreCommand(..),
    DerivationCommand(..),
    DaemonCommand(..),
    Options(..),
    defaultOptions,
    PrivilegeRequirement(..),

    -- Command parsing
    parseArgs,
    commandPrivilege,
    commandPhase,

    -- Command execution
    runCommand,
    executeDaemonCommand,
    executeStoreCommand,
    executeDerivationCommand,

    -- Derivation information helpers
    drvInfoPath,
    drvInfoHash,

    -- Command handlers
    handleBuild,
    handleEval,
    handleGC,
    handleStore,
    handleDerivation,
    handleInfo,
    handleDaemon,

    -- Utility functions
    listStorePaths,
    getDaemonStatus,
    daemonCommand,
    getUserCredentials,
    evaluateExpression,
    createBasicBuilder,
    requestBuildDerivation,
    breakStaleLock
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Exception (try, catch, finally, mask, bracket, throwIO, onException, evaluate, ErrorCall(..), handle, SomeException)
import Control.Monad (forM, forM_, when, unless, void, foldM)
import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.State (get, modify, gets)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.List (nub, isPrefixOf, isInfixOf)
import Data.Unique (Unique, newUnique, hashUnique)
import System.Directory
import System.FilePath
import qualified System.Process as Process
import System.Exit
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr, stdin, IOMode(..), withFile, hClose, hGetContents, hSetBuffering, BufferMode(..))
import System.Posix.Files (setFileMode, getFileStatus, fileMode, fileOwner, fileGroup, setOwnerAndGroup, modificationTime, fileSize)
import qualified System.Posix.User as User
import qualified System.Posix.Process as Process
import System.Posix.Process (getProcessStatus, forkProcess, executeFile, getProcessID)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM, installHandler, Handler(..))
import System.Posix.Types (ProcessID, FileMode, UserID, GroupID, Fd)
import System.Posix.IO (openFd, createFile, closeFd, setLock, getLock,
                       defaultFileFlags, OpenMode(..), OpenFileFlags(..),
                       exclusive, fdToHandle, createPipe)
import Foreign.C.Error (Errno(..), getErrno, throwErrnoIfMinus1_, throwErrno)
import Foreign.C (CInt(..))
import GHC.IO.Handle.FD (handleToFd)
import qualified System.Timeout as SystemTimeout
import Control.Concurrent.Async (async, Async, wait, cancel, waitCatch, race, withAsync)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import System.FilePath.Posix (normalise, takeDirectory)
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import Database.SQLite.Simple (Connection)
import qualified Database.SQLite.Simple as SQL
import Data.Int (Int64)
import Data.Singletons

import Ten.Core
import Ten.Store
import Ten.Build
import qualified Ten.Core as Core
import qualified Ten.Store as Store
import Ten.Derivation
import qualified Ten.Derivation as Derivation
import Ten.Sandbox
import qualified Ten.Sandbox as Sandbox
import Ten.Graph
import qualified Ten.Graph as Graph
import Ten.DB.Core
import qualified Ten.DB.Core as DB
import Ten.DB.Derivations
import qualified Ten.DB.Derivations as DBDeriv
import qualified Ten.Daemon.Protocol as Protocol
import qualified Ten.Daemon.Client as DaemonClient

-- | Command-line options
data Options = Options {
    optVerbosity :: Int,
    optShowHelp :: Bool,
    optShowVersion :: Bool,
    optStoreDir :: Maybe FilePath,
    optWorkDir :: Maybe FilePath,
    optDaemonSocket :: Maybe FilePath,
    optUseDaemon :: Bool
} deriving (Show, Eq)

-- | Default options
defaultOptions :: Options
defaultOptions = Options {
    optVerbosity = 1,
    optShowHelp = False,
    optShowVersion = False,
    optStoreDir = Nothing,
    optWorkDir = Nothing,
    optDaemonSocket = Nothing,
    optUseDaemon = True
}

-- | Privilege requirements for commands
data PrivilegeRequirement =
    DaemonRequired    -- Requires daemon privileges
  | BuilderSufficient -- Can be done from builder context
  deriving (Show, Eq)

-- | Command types
data Command
    = BuildCmd FilePath                -- Build a file
    | EvalCmd FilePath                 -- Evaluate a file
    | GCCmd                           -- Run garbage collection
    | StoreCmd StoreCommand           -- Store operations
    | DerivCmd DerivationCommand      -- Derivation operations
    | InfoCmd FilePath                -- Show info about a path
    | DaemonCmd DaemonCommand         -- Daemon management
    | HelpCmd                         -- Show help
    | VersionCmd                      -- Show version
    deriving (Show, Eq)

-- | Daemon commands
data DaemonCommand
    = DaemonStartCmd                -- Start the daemon
    | DaemonStopCmd                 -- Stop the daemon
    | DaemonRestartCmd              -- Restart the daemon
    | DaemonStatusCmd               -- Get daemon status
    | DaemonConfigCmd               -- Get daemon configuration
    deriving (Show, Eq)

-- | Store commands
data StoreCommand
    = StoreAddCmd FilePath                -- Add a file to the store
    | StoreVerifyCmd FilePath             -- Verify a store path
    | StorePathCmd FilePath               -- Calculate path for a file
    | StoreGCCmd                         -- Run garbage collection
    | StoreListCmd                       -- List store contents
    deriving (Show, Eq)

-- | Derivation commands
data DerivationCommand
    = DerivInfo FilePath               -- Show derivation info
    | QueryDeriver FilePath            -- Query the deriver of a path
    | StoreDerivation FilePath         -- Store a derivation
    | RegisterDerivation FilePath      -- Register a derivation
    | ListDerivations                  -- List all derivations
    deriving (Show, Eq)

-- | Valid command-line options
options :: [OptDescr (Options -> Options)]
options =
    [ Option "v" ["verbose"] (NoArg (\opts -> opts { optVerbosity = optVerbosity opts + 1 }))
      "Increase verbosity level"
    , Option "q" ["quiet"] (NoArg (\opts -> opts { optVerbosity = optVerbosity opts - 1 }))
      "Decrease verbosity level"
    , Option "h" ["help"] (NoArg (\opts -> opts { optShowHelp = True }))
      "Show help"
    , Option "V" ["version"] (NoArg (\opts -> opts { optShowVersion = True }))
      "Show version"
    , Option "s" ["store"] (ReqArg (\dir opts -> opts { optStoreDir = Just dir }) "DIR")
      "Use DIR as store directory"
    , Option "w" ["work-dir"] (ReqArg (\dir opts -> opts { optWorkDir = Just dir }) "DIR")
      "Use DIR as work directory"
    , Option "d" ["daemon"] (ReqArg (\socket opts -> opts { optDaemonSocket = Just socket }) "SOCKET")
      "Connect to daemon at SOCKET"
    , Option "n" ["no-daemon"] (NoArg (\opts -> opts { optUseDaemon = False }))
      "Don't use daemon, even for operations that would benefit from it"
    ]

-- Helper function to sanitize file paths
sanitizeFilePath :: FilePath -> FilePath
sanitizeFilePath path = normalise path

-- | Parse command line arguments
parseArgs :: [String] -> Either String (Command, Options)
parseArgs [] = Right (HelpCmd, defaultOptions)
parseArgs (cmdStr:args) = do
    let (flags, nonOpts, errors) = getOpt Permute options args

    unless (null errors) $
        Left $ "Error parsing options: " ++ concat errors

    let opts = foldr ($) defaultOptions flags

    cmd <- case cmdStr of
        "build" ->
            case nonOpts of
                (file:_) -> Right $ BuildCmd (sanitizeFilePath file)
                [] -> Left "build: missing file argument"
        "eval" ->
            case nonOpts of
                (file:_) -> Right $ EvalCmd (sanitizeFilePath file)
                [] -> Left "eval: missing file argument"
        "gc" -> Right GCCmd
        "store" ->
            case nonOpts of
                [] -> Left "store: missing subcommand"
                (subcmd:args') ->
                    case subcmd of
                        "add" ->
                            case args' of
                                (file:_) -> Right $ StoreCmd (StoreAddCmd (sanitizeFilePath file))
                                [] -> Left "store add: missing file argument"
                        "verify" ->
                            case args' of
                                (path:_) -> Right $ StoreCmd (StoreVerifyCmd (sanitizeFilePath path))
                                [] -> Left "store verify: missing path argument"
                        "path" ->
                            case args' of
                                (file:_) -> Right $ StoreCmd (StorePathCmd (sanitizeFilePath file))
                                [] -> Left "store path: missing file argument"
                        "gc" -> Right $ StoreCmd StoreGCCmd
                        "list" -> Right $ StoreCmd StoreListCmd
                        _ -> Left $ "unknown store subcommand: " ++ subcmd
        "derivation" ->
            case nonOpts of
                [] -> Left "derivation: missing subcommand"
                (subcmd:args') ->
                    case subcmd of
                        "info" ->
                            case args' of
                                (path:_) -> Right $ DerivCmd (DerivInfo (sanitizeFilePath path))
                                [] -> Left "derivation info: missing path argument"
                        "query-deriver" ->
                            case args' of
                                (path:_) -> Right $ DerivCmd (QueryDeriver (sanitizeFilePath path))
                                [] -> Left "derivation query-deriver: missing path argument"
                        "store" ->
                            case args' of
                                (file:_) -> Right $ DerivCmd (StoreDerivation (sanitizeFilePath file))
                                [] -> Left "derivation store: missing file argument"
                        "register" ->
                            case args' of
                                (file:_) -> Right $ DerivCmd (RegisterDerivation (sanitizeFilePath file))
                                [] -> Left "derivation register: missing file argument"
                        "list" -> Right $ DerivCmd ListDerivations
                        _ -> Left $ "unknown derivation subcommand: " ++ subcmd
        "info" ->
            case nonOpts of
                (path:_) -> Right $ InfoCmd (sanitizeFilePath path)
                [] -> Left "info: missing path argument"
        "daemon" ->
            case nonOpts of
                [] -> Left "daemon: missing subcommand"
                (subcmd:_) ->
                    case subcmd of
                        "start" -> Right $ DaemonCmd DaemonStartCmd
                        "stop" -> Right $ DaemonCmd DaemonStopCmd
                        "restart" -> Right $ DaemonCmd DaemonRestartCmd
                        "status" -> Right $ DaemonCmd DaemonStatusCmd
                        "config" -> Right $ DaemonCmd DaemonConfigCmd
                        _ -> Left $ "unknown daemon subcommand: " ++ subcmd
        "help" -> Right HelpCmd
        "version" -> Right VersionCmd
        _ -> Left $ "unknown command: " ++ cmdStr

    return (cmd, opts)

-- | Determine the privilege requirement for a command
commandPrivilege :: Command -> PrivilegeRequirement
commandPrivilege = \case
    BuildCmd _ -> BuilderSufficient       -- Build can use either context
    EvalCmd _ -> BuilderSufficient        -- Eval can use either context
    StoreCmd (StoreAddCmd _) -> DaemonRequired    -- Adding to store needs daemon privileges
    StoreCmd (StoreListCmd) -> BuilderSufficient  -- Listing can use either
    StoreCmd (StoreVerifyCmd _) -> BuilderSufficient  -- Verification can use either
    StoreCmd (StorePathCmd _) -> BuilderSufficient  -- Path calculation can use either
    StoreCmd (StoreGCCmd) -> DaemonRequired       -- GC needs daemon privileges
    GCCmd -> DaemonRequired                   -- GC needs daemon privileges
    DerivCmd (RegisterDerivation _) -> DaemonRequired  -- Registration needs daemon
    DerivCmd (StoreDerivation _) -> DaemonRequired    -- Store write needs daemon
    DerivCmd (DerivInfo _) -> BuilderSufficient  -- Info can use either
    DerivCmd (QueryDeriver _) -> BuilderSufficient    -- Query can use either
    DerivCmd (ListDerivations) -> BuilderSufficient   -- Listing can use either
    InfoCmd _ -> BuilderSufficient             -- Info can use either
    DaemonCmd _ -> DaemonRequired              -- Daemon management needs daemon privileges
    HelpCmd -> BuilderSufficient              -- Help works in any context
    VersionCmd -> BuilderSufficient           -- Version info works in any context

-- | Determine which phase a command belongs to
commandPhase :: Command -> Phase
commandPhase = \case
    BuildCmd _ -> Core.Build              -- Build command uses Build phase
    EvalCmd _ -> Core.Eval                -- Eval command uses Eval phase
    StoreCmd _ -> Core.Build              -- Store commands use Build phase
    GCCmd -> Core.Build                   -- GC uses Build phase
    DerivCmd _ -> Core.Build              -- Derivation commands use Build phase for consistency
    InfoCmd _ -> Core.Build               -- Info works in Build phase
    DaemonCmd _ -> Core.Build             -- Daemon commands use Build phase
    HelpCmd -> Core.Build                 -- Help works in any phase
    VersionCmd -> Core.Build              -- Version works in any phase

-- | Main entry point to run a command
runCommand :: Command -> Options -> IO ()
runCommand cmd opts = case commandPrivilege cmd of
    -- Commands that can run in builder context (potentially via daemon)
    BuilderSufficient -> do
        let storeDir = fromMaybe "/var/lib/ten/store" (optStoreDir opts)
        let workDir = fromMaybe "/var/lib/ten/work" (optStoreDir opts)

        -- Check if daemon socket is specified, attempt to connect
        daemonResult <- case optDaemonSocket opts of
            Just socket -> do
                creds <- getUserCredentials socket
                client <- DaemonClient.connectToDaemon socket creds
                case client of
                    Left err -> do
                        hPutStrLn stderr $ "Warning: Could not connect to daemon: " ++ show err
                        return Nothing
                    Right conn -> return $ Just conn
            Nothing -> return Nothing

        -- Create appropriate environment based on daemon connection
        env <- case daemonResult of
            Just conn ->
                return $ initClientEnv workDir storeDir conn
            Nothing -> do
                putStrLn "Running in standalone mode (some operations may be unavailable)"
                return $ initBuildEnv workDir storeDir

        -- Set verbosity
        let env' = env { verbosity = optVerbosity opts }

        -- Run the command based on its phase
        case commandPhase cmd of
            Core.Eval -> do
                bid <- BuildId <$> newUnique
                result <- evalTen (handleEvalCommand cmd opts) env'
                case result of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        exitWith (ExitFailure 1)
                    Right _ -> return ()

            Core.Build -> do
                bid <- BuildId <$> newUnique
                result <- buildTen (handleBuildCommand cmd opts) env'
                case result of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        exitWith (ExitFailure 1)
                    Right _ -> return ()

    -- Commands that require daemon privileges
    DaemonRequired -> case optDaemonSocket opts of
        -- If socket specified, use the daemon
        Just socket -> void $ daemonCommand (commandToString cmd) opts socket

        -- Otherwise, attempt to get daemon privileges
        Nothing -> do
            uid <- User.getRealUserID
            if uid == 0
                then do
                    -- Running as root, use daemon privileges
                    let storeDir = fromMaybe "/var/lib/ten/store" (optStoreDir opts)
                    let workDir = fromMaybe "/var/lib/ten/work" (optStoreDir opts)
                    let env = initDaemonEnv workDir storeDir Nothing
                    let env' = env { verbosity = optVerbosity opts }

                    -- Run the command based on its phase
                    case commandPhase cmd of
                        Core.Eval -> do
                            bid <- BuildId <$> newUnique
                            result <- evalTen (handleEvalCommand cmd opts) env'
                            case result of
                                Left err -> do
                                    hPutStrLn stderr $ "Error: " ++ show err
                                    exitWith (ExitFailure 1)
                                Right _ -> return ()

                        Core.Build -> do
                            bid <- BuildId <$> newUnique
                            result <- buildTen (handleBuildCommand cmd opts) env'
                            case result of
                                Left err -> do
                                    hPutStrLn stderr $ "Error: " ++ show err
                                    exitWith (ExitFailure 1)
                                Right _ -> return ()
                else do
                    -- Not running as root, suggest using daemon
                    hPutStrLn stderr $ "This command requires daemon privileges. Either:"
                    hPutStrLn stderr $ "  1. Run with root privileges (sudo)"
                    hPutStrLn stderr $ "  2. Connect to a running daemon with --daemon option"
                    exitWith $ ExitFailure 1

-- | Handler for Build phase commands
handleBuildCommand :: Command -> Options -> TenM 'Build 'Builder ()
handleBuildCommand cmd opts = case cmd of
    BuildCmd file -> handleBuild sBuilder file
    GCCmd -> handleGC sBuilder True
    StoreCmd subcmd -> handleStore sBuilder subcmd
    DerivCmd subcmd -> handleDerivation sBuilder subcmd
    InfoCmd path -> handleInfo sBuilder path
    DaemonCmd subcmd -> handleDaemon sBuilder subcmd
    HelpCmd -> showHelp
    VersionCmd -> showVersion
    EvalCmd _ -> throwError $ PhaseError "Eval commands cannot be handled in Build phase"

-- | Handler for Eval phase commands
handleEvalCommand :: Command -> Options -> TenM 'Eval 'Builder ()
handleEvalCommand cmd opts = case cmd of
    EvalCmd file -> handleEval sBuilder file
    HelpCmd -> showHelp
    VersionCmd -> showVersion
    _ -> throwError $ PhaseError "Command requires Build phase"

-- | Show help text
showHelp :: TenM p t ()
showHelp = liftIO $ do
    putStrLn "Ten - Content-addressed build system"
    putStrLn ""
    putStrLn "Usage: ten COMMAND [OPTIONS]"
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "  build FILE      Build a Ten expression or derivation file"
    putStrLn "  eval FILE       Evaluate a Ten expression"
    putStrLn "  gc              Run garbage collection"
    putStrLn "  store           Store operations (see 'ten store help')"
    putStrLn "  derivation      Derivation operations (see 'ten derivation help')"
    putStrLn "  info PATH       Show information about a store path"
    putStrLn "  daemon          Daemon operations (see 'ten daemon help')"
    putStrLn "  help            Show this help"
    putStrLn "  version         Show version information"
    putStrLn ""
    putStrLn $ usageInfo "General options:" options

-- | Show version information
showVersion :: TenM p t ()
showVersion = liftIO $ do
    putStrLn "Ten build system v0.1.0"
    putStrLn "Copyright (C) 2023 Ten Contributors"
    putStrLn "License: MIT"

-- | Get a command string representation
commandToString :: Command -> String
commandToString = \case
    BuildCmd path -> "build " ++ path
    EvalCmd path -> "eval " ++ path
    GCCmd -> "gc"
    StoreCmd subcmd -> case subcmd of
        StoreAddCmd path -> "store add " ++ path
        StoreVerifyCmd path -> "store verify " ++ path
        StorePathCmd path -> "store path " ++ path
        StoreGCCmd -> "store gc"
        StoreListCmd -> "store list"
    DerivCmd subcmd -> case subcmd of
        DerivInfo path -> "derivation info " ++ path
        QueryDeriver path -> "derivation query-deriver " ++ path
        StoreDerivation path -> "derivation store " ++ path
        RegisterDerivation path -> "derivation register " ++ path
        ListDerivations -> "derivation list"
    InfoCmd path -> "info " ++ path
    DaemonCmd subcmd -> case subcmd of
        DaemonStartCmd -> "daemon start"
        DaemonStopCmd -> "daemon stop"
        DaemonRestartCmd -> "daemon restart"
        DaemonStatusCmd -> "daemon status"
        DaemonConfigCmd -> "daemon config"
    HelpCmd -> "help"
    VersionCmd -> "version"

-- | Convert command to argument list
commandToArgs :: Command -> [String]
commandToArgs cmd = words (commandToString cmd)

-- | Execute daemon command (via client protocol)
executeDaemonCommand :: SPrivilegeTier 'Builder -> Command -> Options -> DaemonConnection 'Builder -> IO Bool
executeDaemonCommand spt cmd opts conn = do
    -- Create request based on command
    let request = commandToRequest cmd

    -- Send request to daemon
    response <- DaemonClient.sendRequestSync conn request 300000000  -- 5 minute timeout

    -- Process response
    case response of
        Left err -> do
            hPutStrLn stderr $ "Error: " ++ show err
            return False
        Right resp -> do
            -- Display response based on command
            displayDaemonResponse cmd resp
            return True
  where
    -- Convert command to appropriate request
    commandToRequest :: Command -> Request
    commandToRequest = \case
        GCCmd -> Request 0 "gc" Map.empty Nothing
        StoreCmd StoreGCCmd -> Request 0 "gc" Map.empty Nothing
        StoreCmd StoreListCmd -> Request 0 "store-list" Map.empty Nothing
        StoreCmd (StoreVerifyCmd path) -> Request 0 "store-verify"
                                       (Map.singleton "path" (T.pack path)) Nothing
        StoreCmd (StorePathCmd path) -> do
            -- For getting a store path, we need to read the file and send it
            Request 0 "store-path"
                   (Map.singleton "path" (T.pack path))
                   Nothing  -- Would need file content here
        StoreCmd (StoreAddCmd path) -> do
            -- For adding to store, we need to read the file and send it
            Request 0 "store-add"
                   (Map.singleton "path" (T.pack path))
                   Nothing  -- Would need file content here
        DerivCmd (DerivInfo path) ->
            Request 0 "derivation-info" (Map.singleton "path" (T.pack path)) Nothing
        DerivCmd ListDerivations ->
            Request 0 "list-derivations" Map.empty Nothing
        _ -> error $ "Command not supported via daemon protocol: " ++ show cmd

    -- Display response from daemon based on command type
    displayDaemonResponse :: Command -> DaemonResponse -> IO ()
    displayDaemonResponse _ resp = case resp of
        GCResultResponse stats -> do
            putStrLn $ "GC completed: " ++ show (gcCollected stats) ++ " paths collected"
            putStrLn $ "  Total paths: " ++ show (gcTotal stats)
            putStrLn $ "  Live paths: " ++ show (gcLive stats)
            putStrLn $ "  Freed: " ++ formatSize (gcBytes stats)
            putStrLn $ "  Elapsed time: " ++ formatTime (gcElapsedTime stats)
        StoreListResponse paths -> do
            putStrLn $ "Store contains " ++ show (length paths) ++ " paths:"
            forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)
        StoreVerifyResponse valid ->
            putStrLn $ if valid then "Path is valid" else "Path is invalid"
        StorePathResponse path ->
            putStrLn $ T.unpack (storePathToText path)
        StoreAddResponse path ->
            putStrLn $ "Added to store: " ++ T.unpack (storePathToText path)
        DerivationResponse drv ->
            putStrLn $ "Derivation: " ++ T.unpack (derivName drv)
        DerivationListResponse paths -> do
            putStrLn $ "Found " ++ show (length paths) ++ " derivations:"
            forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)
        StatusResponse status -> do
            putStrLn $ "Daemon status: " ++ T.unpack (daemonStatus status)
            putStrLn $ "  Uptime: " ++ formatTime (daemonUptime status)
            putStrLn $ "  Active builds: " ++ show (daemonActiveBuilds status)
            putStrLn $ "  Completed builds: " ++ show (daemonCompletedBuilds status)
            putStrLn $ "  Failed builds: " ++ show (daemonFailedBuilds status)
            putStrLn $ "  Store size: " ++ formatSize (daemonStoreSize status)
            putStrLn $ "  Store paths: " ++ show (daemonStorePaths status)
        ConfigResponse config -> do
            putStrLn "Daemon configuration:"
            putStrLn $ "  Socket: " ++ daemonSocketPath config
            putStrLn $ "  Store: " ++ daemonStorePath config
            putStrLn $ "  State file: " ++ daemonStateFile config
            putStrLn $ "  Log level: " ++ show (daemonLogLevel config)
            putStrLn $ "  Max jobs: " ++ show (daemonMaxJobs config)
        ErrorResponse err ->
            hPutStrLn stderr $ "Error: " ++ T.unpack (buildErrorToText err)
        SuccessResponse ->
            putStrLn "Operation completed successfully."
        _ -> putStrLn $ "Received response: " ++ show resp

    -- Format file size in human-readable form
    formatSize :: Integer -> String
    formatSize size
        | size < 1024 = show size ++ " B"
        | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
        | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
        | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"

    -- Format time in human-readable form
    formatTime :: Double -> String
    formatTime seconds
        | seconds < 60 = show (round seconds) ++ " seconds"
        | seconds < 3600 = show (round $ seconds / 60) ++ " minutes"
        | otherwise = show (round $ seconds / 3600) ++ " hours"

-- | Execute store command
executeStoreCommand :: SPrivilegeTier 'Builder -> StoreCommand -> DaemonConnection 'Builder -> Options -> IO Bool
executeStoreCommand spt cmd conn opts = case cmd of
    StoreAddCmd file -> do
        exists <- doesFileExist file
        if not exists
            then do
                hPutStrLn stderr $ "Error: File does not exist: " ++ file
                return False
            else do
                -- Read file content
                content <- BS.readFile file

                -- Create store-add request
                let request = Request {
                        reqId = 0,
                        reqType = "store-add",
                        reqParams = Map.singleton "path" (T.pack $ takeFileName file),
                        reqPayload = Just content
                    }

                -- Send request
                response <- DaemonClient.sendRequestSync conn request 300000000

                -- Handle response
                case response of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        return False
                    Right (StoreAddResponse path) -> do
                        putStrLn $ "Added to store: " ++ T.unpack (storePathToText path)
                        return True
                    Right _ -> do
                        hPutStrLn stderr "Unexpected response from daemon"
                        return False

    StoreVerifyCmd path -> do
        -- Create store-verify request
        let request = Request {
                reqId = 0,
                reqType = "store-verify",
                reqParams = Map.singleton "path" (T.pack path),
                reqPayload = Nothing
            }

        -- Send request
        response <- DaemonClient.sendRequestSync conn request 30000000

        -- Handle response
        case response of
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ show err
                return False
            Right (StoreVerifyResponse valid) -> do
                putStrLn $ if valid then "Path is valid" else "Path is invalid"
                return True
            Right _ -> do
                hPutStrLn stderr "Unexpected response from daemon"
                return False

    StorePathCmd file -> do
        exists <- doesFileExist file
        if not exists
            then do
                hPutStrLn stderr $ "Error: File does not exist: " ++ file
                return False
            else do
                -- Read file content
                content <- BS.readFile file

                -- Create store-path request
                let request = Request {
                        reqId = 0,
                        reqType = "store-path",
                        reqParams = Map.singleton "path" (T.pack $ takeFileName file),
                        reqPayload = Just content
                    }

                -- Send request
                response <- DaemonClient.sendRequestSync conn request 30000000

                -- Handle response
                case response of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        return False
                    Right (StorePathResponse path) -> do
                        putStrLn $ T.unpack (storePathToText path)
                        return True
                    Right _ -> do
                        hPutStrLn stderr "Unexpected response from daemon"
                        return False

    StoreGCCmd -> do
        -- Create gc request
        let request = Request {
                reqId = 0,
                reqType = "gc",
                reqParams = Map.empty,
                reqPayload = Nothing
            }

        -- Send request
        response <- DaemonClient.sendRequestSync conn request 3600000000  -- 1 hour timeout

        -- Handle response
        case response of
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ show err
                return False
            Right (GCResultResponse stats) -> do
                putStrLn $ "GC completed: " ++ show (gcCollected stats) ++ " paths collected"
                putStrLn $ "  Total paths: " ++ show (gcTotal stats)
                putStrLn $ "  Live paths: " ++ show (gcLive stats)
                putStrLn $ "  Freed: " ++ formatBytes (gcBytes stats)
                return True
            Right _ -> do
                hPutStrLn stderr "Unexpected response from daemon"
                return False

    StoreListCmd -> do
        -- Create store-list request
        let request = Request {
                reqId = 0,
                reqType = "store-list",
                reqParams = Map.empty,
                reqPayload = Nothing
            }

        -- Send request
        response <- DaemonClient.sendRequestSync conn request 300000000

        -- Handle response
        case response of
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ show err
                return False
            Right (StoreListResponse paths) -> do
                putStrLn $ "Store contains " ++ show (length paths) ++ " paths:"
                forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)
                return True
            Right _ -> do
                hPutStrLn stderr "Unexpected response from daemon"
                return False
  where
    formatBytes :: Integer -> String
    formatBytes bytes
        | bytes < 1024 = show bytes ++ " B"
        | bytes < 1024 * 1024 = show (bytes `div` 1024) ++ " KB"
        | bytes < 1024 * 1024 * 1024 = show (bytes `div` (1024 * 1024)) ++ " MB"
        | otherwise = show (bytes `div` (1024 * 1024 * 1024)) ++ " GB"

-- | Execute derivation command
executeDerivationCommand :: SPrivilegeTier 'Builder -> DerivationCommand -> DaemonConnection 'Builder -> Options -> IO Bool
executeDerivationCommand spt cmd conn opts = case cmd of
    DerivInfo path -> do
        -- Create derivation-info request
        let request = Request {
                reqId = 0,
                reqType = "derivation-info",
                reqParams = Map.singleton "path" (T.pack path),
                reqPayload = Nothing
            }

        -- Send request
        response <- DaemonClient.sendRequestSync conn request 30000000

        -- Handle response
        case response of
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ show err
                return False
            Right (DerivationResponse drv) -> do
                -- Display derivation info
                putStrLn $ "Derivation: " ++ T.unpack (derivName drv)
                putStrLn $ "Hash: " ++ T.unpack (derivHash drv)
                putStrLn $ "Builder: " ++ T.unpack (storePathToText (derivBuilder drv))
                putStrLn "Arguments:"
                forM_ (derivArgs drv) $ \arg -> putStrLn $ "  " ++ T.unpack arg
                putStrLn $ "Inputs: " ++ show (Set.size (derivInputs drv))
                putStrLn $ "Outputs: " ++ show (Set.size (derivOutputs drv))
                putStrLn $ "System: " ++ T.unpack (derivSystem drv)
                putStrLn $ "Strategy: " ++ case derivStrategy drv of
                    ApplicativeStrategy -> "applicative"
                    MonadicStrategy -> "monadic"
                return True
            Right _ -> do
                hPutStrLn stderr "Unexpected response from daemon"
                return False

    QueryDeriver path -> do
        -- Create query-deriver request
        let request = Request {
                reqId = 0,
                reqType = "query-deriver",
                reqParams = Map.singleton "path" (T.pack path),
                reqPayload = Nothing
            }

        -- Send request
        response <- DaemonClient.sendRequestSync conn request 30000000

        -- Handle response
        case response of
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ show err
                return False
            Right (DerivationResponse drv) -> do
                putStrLn $ "Deriver: " ++ T.unpack (derivName drv)
                return True
            Right _ -> do
                hPutStrLn stderr "Unexpected response from daemon"
                return False

    StoreDerivation file -> do
        exists <- doesFileExist file
        if not exists
            then do
                hPutStrLn stderr $ "Error: File does not exist: " ++ file
                return False
            else do
                -- Read file content
                content <- BS.readFile file

                -- Create store-derivation request
                let request = Request {
                        reqId = 0,
                        reqType = "store-derivation",
                        reqParams = Map.empty,
                        reqPayload = Just content
                    }

                -- Send request
                response <- DaemonClient.sendRequestSync conn request 30000000

                -- Handle response
                case response of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        return False
                    Right (DerivationStoredResponse path) -> do
                        putStrLn $ "Derivation stored: " ++ T.unpack (storePathToText path)
                        return True
                    Right _ -> do
                        hPutStrLn stderr "Unexpected response from daemon"
                        return False

    RegisterDerivation file -> do
        exists <- doesFileExist file
        if not exists
            then do
                hPutStrLn stderr $ "Error: File does not exist: " ++ file
                return False
            else do
                -- Read file content
                content <- BS.readFile file

                -- Create register-derivation request
                let request = Request {
                        reqId = 0,
                        reqType = "register-derivation",
                        reqParams = Map.empty,
                        reqPayload = Just content
                    }

                -- Send request
                response <- DaemonClient.sendRequestSync conn request 30000000

                -- Handle response
                case response of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        return False
                    Right (DerivationStoredResponse path) -> do
                        putStrLn $ "Derivation registered: " ++ T.unpack (storePathToText path)
                        return True
                    Right _ -> do
                        hPutStrLn stderr "Unexpected response from daemon"
                        return False

    ListDerivations -> do
        -- Create list-derivations request
        let request = Request {
                reqId = 0,
                reqType = "list-derivations",
                reqParams = Map.empty,
                reqPayload = Nothing
            }

        -- Send request
        response <- DaemonClient.sendRequestSync conn request 300000000

        -- Handle response
        case response of
            Left err -> do
                hPutStrLn stderr $ "Error: " ++ show err
                return False
            Right (DerivationListResponse paths) -> do
                putStrLn $ "Found " ++ show (length paths) ++ " derivations:"
                forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)
                return True
            Right _ -> do
                hPutStrLn stderr "Unexpected response from daemon"
                return False

-- | Helper functions for derivation info
drvInfoPath :: DerivInfoResult -> Text
drvInfoPath (path, _) = path

drvInfoHash :: DerivInfoResult -> Text
drvInfoHash (_, hash) = hash

type DerivInfoResult = (Text, Text)  -- Simple definition for the CLI module's needs

-- | Handle build command
handleBuild :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleBuild spt file = do
    -- Check if file exists
    exists <- liftIO $ doesFileExist file
    unless exists $
        throwError $ InputNotFound file

    -- Determine file type
    content <- liftIO $ BS.readFile file
    let fileExt = takeExtension file

    if fileExt == ".drv"
        then handleBuildDerivationFile spt file
        else handleBuildTenExpression spt file

-- | Handle evaluation command
handleEval :: SPrivilegeTier t -> FilePath -> TenM 'Eval t ()
handleEval spt file = do
    -- Check if file exists
    exists <- liftIO $ doesFileExist file
    unless exists $
        throwError $ InputNotFound file

    -- Read file content
    content <- liftIO $ BS.readFile file

    -- Evaluate the expression based on privilege tier
    result <- case fromSing spt of
        Daemon -> evaluateExpression sDaemon content
        Builder -> evaluateExpression sBuilder content

    -- Print the result
    liftIO $ putStrLn $ "Evaluated to derivation: " ++ T.unpack (derivName result)

    -- Output details about the derivation
    liftIO $ do
        putStrLn $ "Builder: " ++ T.unpack (storePathToText (derivBuilder result))
        putStrLn $ "Hash: " ++ T.unpack (derivHash result)
        putStrLn $ "System: " ++ T.unpack (derivSystem result)
        putStrLn $ "Inputs: " ++ show (Set.size (derivInputs result))
        putStrLn $ "Outputs: " ++ show (Set.size (derivOutputs result))

-- | Type class for handling GC with privilege-specific implementations
class CanRunGC (t :: PrivilegeTier) where
    runGarbageCollection :: Bool -> TenM 'Build t (Int, Int, Integer)

-- Daemon implementation with direct access
instance CanRunGC 'Daemon where
    runGarbageCollection dryRun = do
        env <- ask
        let storeDir = storeLocation env

        -- Find all store paths
        allPaths <- Store.listAllStorePaths

        -- Find all roots
        rootPaths <- Store.findGCRoots

        -- Find reachable paths
        reachable <- Graph.computeReachablePaths rootPaths

        -- Determine unreachable paths
        let unreachable = filter (\p -> not $ p `Set.member` reachable) allPaths

        -- Get size of unreachable paths
        sizes <- forM unreachable $ \path -> do
            let fullPath = storePathToFilePath path env
            fileExists <- liftIO $ doesFileExist fullPath
            if fileExists
                then do
                    stat <- liftIO $ getFileStatus fullPath
                    return $ fromIntegral $ fileSize stat
                else return 0

        let totalSize = sum sizes

        -- If not dry run, actually delete the unreachable paths
        unless dryRun $ forM_ unreachable $ \path -> do
            let fullPath = storePathToFilePath path env
            liftIO $ removePathForcibly fullPath

        -- Return stats
        return (length unreachable, length allPaths - length unreachable, totalSize)

-- Builder implementation uses protocol
instance CanRunGC 'Builder where
    runGarbageCollection dryRun = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create gc request
                let request = Request {
                        reqId = 0,
                        reqType = "gc",
                        reqParams = Map.singleton "dryRun" (if dryRun then "true" else "false"),
                        reqPayload = Nothing
                    }

                -- Send request and wait for response
                response <- liftIO $ sendRequestSync conn request 3600000000  -- 1 hour timeout

                -- Process response
                case response of
                    Left err -> throwError err
                    Right (GCResultResponse stats) -> return (
                        gcCollected stats,
                        gcLive stats,
                        gcBytes stats)
                    Right _ -> throwError $ DaemonError "Unexpected response type for GC request"

            _ -> throwError $ PrivilegeError "Garbage collection requires daemon connection"

-- | Handle GC command
handleGC :: SPrivilegeTier t -> Bool -> TenM 'Build t ()
handleGC spt dryRun = do
    -- Acquire GC lock
    env <- ask
    let lockPath = gcLockPath (storeLocation env)

    -- Check for stale lock
    liftIO $ breakStaleLock' lockPath

    -- Run garbage collection using type class for tier-specific implementation
    logMsg 1 "Starting garbage collection"
    when dryRun $
        logMsg 1 "Dry run mode - no paths will be deleted"

    -- Run the GC operation based on privilege tier
    (collected, live, bytes) <- withGCLock (runGarbageCollection dryRun)

    -- Output results
    liftIO $ do
        putStrLn $ "GC completed: " ++ show collected ++ " paths collected"
        putStrLn $ "Live paths: " ++ show live
        when (not dryRun) $
            putStrLn $ "Freed " ++ formatBytes bytes ++ " of disk space"
  where
    formatBytes :: Integer -> String
    formatBytes bytes
        | bytes < 1024 = show bytes ++ " B"
        | bytes < 1024 * 1024 = show (bytes `div` 1024) ++ " KB"
        | bytes < 1024 * 1024 * 1024 = show (bytes `div` (1024 * 1024)) ++ " MB"
        | otherwise = show (bytes `div` (1024 * 1024 * 1024)) ++ " GB"

    withGCLock :: TenM 'Build t (Int, Int, Integer) -> TenM 'Build t (Int, Int, Integer)
    withGCLock action = do
        env <- ask
        let lockPath = gcLockPath (storeLocation env)
        -- In a real implementation, this would acquire a lock file
        action

-- Type class for store operations with privilege-specific implementations
class StoreAccessOps (t :: PrivilegeTier) where
    addToStore :: Text -> ByteString -> TenM p t StorePath
    verifyStoreIntegrity :: StorePath -> TenM p t Bool
    listAllStorePaths :: TenM p t [StorePath]

instance StoreAccessOps 'Daemon where
    addToStore nameHint content = do
        env <- ask

        -- Calculate hash of content
        let contentHashDigest = hashByteString content
        let contentHash = T.pack $ show contentHashDigest

        -- Create store path
        let path = Store.StorePath contentHash nameHint

        -- Create full file path
        let fullPath = storePathToFilePath path env

        -- Check if the path already exists
        exists <- liftIO $ doesFileExist fullPath
        if exists
            then do
                -- Verify hash of existing file matches
                existingContent <- liftIO $ BS.readFile fullPath
                let existingHashDigest = hashByteString existingContent
                let existingHash = T.pack $ show existingHashDigest
                if existingHash == contentHash
                    then do
                        -- Path exists with correct content, register it
                        Store.registerValidPath path Nothing
                        return path
                    else throwError $ HashError $ "Hash collision in store for: " <> T.pack fullPath
            else do
                -- Create directory structure if needed
                liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)

                -- Write content to temporary file
                let tempFile = fullPath <> ".tmp"
                liftIO $ BS.writeFile tempFile content

                -- Set correct permissions (read-only for all, no execute)
                liftIO $ setFileMode tempFile 0o444

                -- Move to final location atomically
                liftIO $ renameFile tempFile fullPath

                -- Register as valid path in database
                Store.registerValidPath path Nothing

                -- Return the store path
                return path

    verifyStoreIntegrity path = do
        env <- ask
        let filePath = storePathToFilePath path env
        exists <- liftIO $ doesFileExist filePath
        if not exists
            then return False
            else do
                content <- liftIO $ BS.readFile filePath
                let contentHashDigest = hashByteString content
                let contentHash = T.pack $ show contentHashDigest
                return $ contentHash == storeHash path

    listAllStorePaths = Store.getStorePaths

instance StoreAccessOps 'Builder where
    addToStore nameHint content = do
        conn <- getDaemonConnection
        let req = Request {
                reqId = 0,
                reqType = "store-add",
                reqParams = Map.singleton "name" nameHint,
                reqPayload = Just content
            }
        response <- liftIO $ sendRequestSync conn req 60000000
        case response of
            Left err -> throwError err
            Right (StoreAddResponse path) -> return path
            Right _ -> throwError $ StoreError "Unexpected response type"

    verifyStoreIntegrity path = do
        conn <- getDaemonConnection
        let req = Request {
                reqId = 0,
                reqType = "store-verify",
                reqParams = Map.singleton "path" (storePathToText path),
                reqPayload = Nothing
            }
        response <- liftIO $ sendRequestSync conn req 30000000
        case response of
            Left _ -> return False
            Right (StoreVerifyResponse valid) -> return valid
            Right _ -> return False

    listAllStorePaths = do
        conn <- getDaemonConnection
        let req = Request {
                reqId = 0,
                reqType = "store-list",
                reqParams = Map.empty,
                reqPayload = Nothing
            }
        response <- liftIO $ sendRequestSync conn req 60000000
        case response of
            Left _ -> return []
            Right (StoreListResponse paths) -> return paths
            Right _ -> return []

-- | Handle store commands
handleStore :: SPrivilegeTier t -> StoreCommand -> TenM 'Build t ()
handleStore spt cmd = case cmd of
    StoreAddCmd file -> do
        -- Check if file exists
        exists <- liftIO $ doesFileExist file
        unless exists $
            throwError $ InputNotFound file

        -- Determine if it's a file or directory (for now, just handle files)
        isDir <- liftIO $ doesDirectoryExist file
        when isDir $
            throwError $ StoreError "Directory storage not implemented yet"

        -- Read file content
        content <- liftIO $ BS.readFile file
        let nameHint = T.pack $ takeFileName file

        -- Add to store with appropriate implementation for privilege tier
        result <- addToStore nameHint content

        -- Output the result
        liftIO $ putStrLn $ "Added to store: " ++ T.unpack (storePathToText result)

    StoreVerifyCmd path -> do
        -- Try to parse as store path first
        case textToStorePath (T.pack path) of
            Just storePath -> do
                -- Verify the path
                valid <- verifyStoreIntegrity storePath
                liftIO $ putStrLn $ if valid
                                     then "Path is valid"
                                     else "Path is invalid or corrupted"

            Nothing -> do
                -- Not a valid store path format
                liftIO $ putStrLn $ "Invalid store path format: " ++ path

    StorePathCmd file -> do
        -- Check if file exists
        exists <- liftIO $ doesFileExist file
        unless exists $
            throwError $ InputNotFound file

        -- Calculate hash and display store path
        content <- liftIO $ BS.readFile file
        let nameHint = T.pack $ takeFileName file
        storePathForContent nameHint content

    StoreGCCmd -> handleGC spt False

    StoreListCmd -> do
        -- List store paths
        paths <- listAllStorePaths
        liftIO $ do
            putStrLn $ "Found " ++ show (length paths) ++ " paths in store:"
            forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)

-- | Calculate store path for content without storing it
storePathForContent :: Text -> BS.ByteString -> TenM 'Build t ()
storePathForContent nameHint content = do
    env <- ask

    -- Calculate hash of content
    let contentHashDigest = hashByteString content
    let contentHash = T.pack $ show contentHashDigest

    -- Create store path
    let path = Store.StorePath contentHash nameHint

    -- Display the path
    liftIO $ putStrLn $ T.unpack (storePathToText path)

-- Type class for derivation operations with privilege-specific implementations
class CanManageDerivations (t :: PrivilegeTier) where
    readDerivation :: StorePath -> TenM p t (Either BuildError Derivation)
    storeDerivation :: Derivation -> TenM p t StorePath
    registerDerivation :: Derivation -> TenM p t StorePath
    queryDeriverForPath :: StorePath -> TenM p t (Maybe StorePath)
    listDerivations :: TenM p t [StorePath]

instance CanManageDerivations 'Daemon where
    readDerivation path = readDerivationDaemon path

    storeDerivation drv = storeDerivationDaemon drv

    registerDerivation drv = do
        -- First store the derivation
        path <- storeDerivation drv

        -- Then register it in the database
        env <- ask
        db <- getDatabaseConn

        -- Insert into database
        withTenWriteTransaction db $ \conn -> do
            tenExecute conn
                "INSERT INTO Derivations (path, name, hash, builder, system, strategy) VALUES (?, ?, ?, ?, ?, ?)"
                (storePathToText path, derivName drv, derivHash drv,
                 storePathToText (derivBuilder drv), derivSystem drv,
                 if derivStrategy drv == ApplicativeStrategy then "applicative" else "monadic")

        return path

    queryDeriverForPath path = do
        env <- ask
        db <- getDatabaseConn
        -- Query using path
        results <- withTenReadTransaction db $ \conn -> do
            tenQuery conn
                "SELECT d.path FROM Derivations d JOIN DerivationOutputs o ON d.id = o.derivation_id WHERE o.path = ?"
                (Only (storePathToText path)) :: TenM 'Build 'Daemon [Only Text]

        case results of
            (Only derivPath):_ -> return $ parseStorePath derivPath
            [] -> return Nothing

    listDerivations = do
        env <- ask
        db <- getDatabaseConn

        results <- withTenReadTransaction db $ \conn -> do
            rows <- tenQuery conn
                "SELECT path FROM Derivations LIMIT 100"
                () :: TenM 'Build 'Daemon [Only Text]
            return $ catMaybes $ map (parseStorePath . fromOnly) rows

        return results

instance CanManageDerivations 'Builder where
    readDerivation path = readDerivationBuilder path

    storeDerivation drv = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create store request with derivation payload
                let serialized = Derivation.serializeDerivation drv
                let request = Request {
                        reqId = 0,
                        reqType = "store-derivation",
                        reqParams = Map.singleton "name" (derivName drv <> ".drv"),
                        reqPayload = Just serialized
                    }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000

                case response of
                    Left err -> throwError err
                    Right (DerivationStoredResponse path) -> return path
                    Right _ -> throwError $ StoreError "Unexpected response type"

            _ -> throwError $ PrivilegeError "Cannot store derivation without daemon connection"

    registerDerivation drv = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create register request with derivation payload
                let serialized = Derivation.serializeDerivation drv
                let request = Request {
                        reqId = 0,
                        reqType = "register-derivation",
                        reqParams = Map.empty,
                        reqPayload = Just serialized
                    }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000

                case response of
                    Left err -> throwError err
                    Right (DerivationStoredResponse path) -> return path
                    Right _ -> throwError $ StoreError "Unexpected response type"

            _ -> throwError $ PrivilegeError "Cannot register derivation without daemon connection"

    queryDeriverForPath path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create query request
                let request = Request {
                        reqId = 0,
                        reqType = "query-deriver",
                        reqParams = Map.singleton "path" (storePathToText path),
                        reqPayload = Nothing
                    }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000

                case response of
                    Left _ -> return Nothing
                    Right (DerivationResponse drv) -> do
                        let drvPath = Store.StorePath (derivHash drv) (derivName drv <> ".drv")
                        return $ Just drvPath
                    Right _ -> return Nothing

            _ -> return Nothing

    listDerivations = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create list request
                let request = Request {
                        reqId = 0,
                        reqType = "list-derivations",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000

                case response of
                    Left _ -> return []
                    Right (DerivationListResponse paths) -> return paths
                    Right _ -> return []

            _ -> return []

-- | Handle derivation commands
handleDerivation :: SPrivilegeTier t -> DerivationCommand -> TenM 'Build t ()
handleDerivation spt cmd = case cmd of
    DerivInfo path -> do
        -- Parse storage path
        case textToStorePath (T.pack path) of
            Just storePath -> do
                -- Read the derivation
                result <- readDerivation storePath

                -- Process the result
                case result of
                    Left err -> throwError err
                    Right drv -> displayDerivationInfo drv

            Nothing -> throwError $ StoreError $ "Invalid store path format: " <> T.pack path

    QueryDeriver path -> do
        -- Parse storage path
        case textToStorePath (T.pack path) of
            Just storePath -> do
                -- Query for deriver
                mDeriver <- queryDeriverForPath storePath
                case mDeriver of
                    Just derivPath ->
                        liftIO $ putStrLn $ "Deriver: " ++ T.unpack (storePathToText derivPath)
                    Nothing ->
                        liftIO $ putStrLn "No deriver found for this path"

            Nothing -> throwError $ StoreError $ "Invalid store path format: " <> T.pack path

    StoreDerivation file -> do
        -- Check if file exists
        exists <- liftIO $ doesFileExist file
        unless exists $
            throwError $ InputNotFound file

        -- Read and deserialize the derivation
        content <- liftIO $ BS.readFile file
        drv <- case Derivation.deserializeDerivation content of
            Left err -> throwError err
            Right drv -> return drv

        -- Store the derivation
        path <- storeDerivation drv
        liftIO $ putStrLn $ "Stored derivation at: " ++ T.unpack (storePathToText path)

    RegisterDerivation file -> do
        -- Check if file exists
        exists <- liftIO $ doesFileExist file
        unless exists $
            throwError $ InputNotFound file

        -- Read and deserialize the derivation
        content <- liftIO $ BS.readFile file
        drv <- case Derivation.deserializeDerivation content of
            Left err -> throwError err
            Right drv -> return drv

        -- Register the derivation
        path <- registerDerivation drv
        liftIO $ putStrLn $ "Registered derivation at: " ++ T.unpack (storePathToText path)

    ListDerivations -> do
        -- List all derivations
        paths <- listDerivations
        liftIO $ do
            putStrLn $ "Found " ++ show (length paths) ++ " derivations:"
            forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)

-- | Display derivation info
displayDerivationInfo :: Derivation -> TenM 'Build t ()
displayDerivationInfo drv = liftIO $ do
    putStrLn $ "Derivation: " ++ T.unpack (derivName drv)
    putStrLn $ "Hash: " ++ T.unpack (derivHash drv)
    putStrLn $ "Builder: " ++ T.unpack (storePathToText (derivBuilder drv))
    putStrLn "Arguments:"
    forM_ (derivArgs drv) $ \arg -> putStrLn $ "  " ++ T.unpack arg
    putStrLn $ "Inputs: " ++ show (Set.size (derivInputs drv))
    forM_ (Set.toList (derivInputs drv)) $ \input ->
        putStrLn $ "  " ++ T.unpack (storePathToText (inputPath input)) ++
                  " as " ++ T.unpack (inputName input)
    putStrLn $ "Outputs: " ++ show (Set.size (derivOutputs drv))
    forM_ (Set.toList (derivOutputs drv)) $ \output ->
        putStrLn $ "  " ++ T.unpack (outputName output) ++
                  " -> " ++ T.unpack (storePathToText (outputPath output))
    putStrLn $ "System: " ++ T.unpack (derivSystem drv)
    putStrLn $ "Strategy: " ++ case derivStrategy drv of
        ApplicativeStrategy -> "applicative"
        MonadicStrategy -> "monadic"

-- Type class for handling path information with privilege-specific implementations
class CanGetPathInfo (t :: PrivilegeTier) where
    checkStorePathExists :: StorePath -> TenM p t Bool
    getReferencesToPath :: StorePath -> TenM p t (Set StorePath)
    getReferencesFromPath :: StorePath -> TenM p t (Set StorePath)
    isGCRoot :: StorePath -> TenM p t Bool

instance CanGetPathInfo 'Daemon where
    checkStorePathExists path = do
        env <- ask
        let fullPath = storePathToFilePath path env
        liftIO $ doesFileExist fullPath

    getReferencesToPath path = do
        env <- ask
        db <- getDatabaseConn

        -- Query references to this path
        refs <- withTenReadTransaction db $ \conn -> do
            rows <- tenQuery conn
                "SELECT referrer FROM References WHERE reference = ?"
                (Only (storePathToText path)) :: TenM p 'Daemon [Only Text]
            return $ catMaybes $ map (parseStorePath . fromOnly) rows

        return $ Set.fromList refs

    getReferencesFromPath path = do
        env <- ask
        db <- getDatabaseConn

        -- Query references from this path
        refs <- withTenReadTransaction db $ \conn -> do
            rows <- tenQuery conn
                "SELECT reference FROM References WHERE referrer = ?"
                (Only (storePathToText path)) :: TenM p 'Daemon [Only Text]
            return $ catMaybes $ map (parseStorePath . fromOnly) rows

        return $ Set.fromList refs

    isGCRoot path = do
        env <- ask
        let rootsDir = storeLocation env </> "gc-roots"

        -- Check if path is a GC root
        roots <- liftIO $ listDirectory rootsDir
        let pathText = storePathToText path

        -- Check through root files
        isRoot <- liftIO $ foldM (\found root -> do
            let rootPath = rootsDir </> root
            isLink <- doesFileExist rootPath
            if isLink
                then do
                    target <- readFile rootPath
                    return $ found || target == T.unpack pathText
                else return found
            ) False roots

        return isRoot

instance CanGetPathInfo 'Builder where
    checkStorePathExists path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create verify request
                let request = Request {
                        reqId = 0,
                        reqType = "store-verify",
                        reqParams = Map.singleton "path" (storePathToText path),
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left _ -> return False
                    Right (StoreVerifyResponse valid) -> return valid
                    Right _ -> return False

            _ -> return False

    getReferencesToPath path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create references-to request
                let request = Request {
                        reqId = 0,
                        reqType = "references-to",
                        reqParams = Map.singleton "path" (storePathToText path),
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left _ -> return Set.empty
                    Right (StoreListResponse paths) -> return $ Set.fromList paths
                    Right _ -> return Set.empty

            _ -> return Set.empty

    getReferencesFromPath path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create references-from request
                let request = Request {
                        reqId = 0,
                        reqType = "references-from",
                        reqParams = Map.singleton "path" (storePathToText path),
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left _ -> return Set.empty
                    Right (StoreListResponse paths) -> return $ Set.fromList paths
                    Right _ -> return Set.empty

            _ -> return Set.empty

    isGCRoot _ = return False  -- Builder can't check GC roots directly

-- | Handle info command
handleInfo :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleInfo spt path = do
    -- Check if the path is a file or a store path
    fileExists <- liftIO $ doesFileExist path

    if fileExists
        then do
            -- Get file info
            size <- liftIO $ fromIntegral . fileSize <$> getFileStatus path
            perms <- liftIO $ getPermissions path

            -- Calculate hash for the file
            content <- liftIO $ BS.readFile path
            let hashDigest = hashByteString content
            let hashStr = show hashDigest

            -- Display file info
            liftIO $ do
                putStrLn $ "File: " ++ path
                putStrLn $ "Size: " ++ formatSize size
                putStrLn $ "Hash: " ++ hashStr
                putStrLn $ "Permissions: " ++ showPermissions perms
        else case textToStorePath (T.pack path) of
            Just storePath -> handleStorePathInfo spt storePath
            Nothing -> liftIO $ putStrLn $ "Not a valid store path format: " ++ path
  where
    formatSize :: Integer -> String
    formatSize size
        | size < 1024 = show size ++ " B"
        | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
        | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
        | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"

    showPermissions :: Permissions -> String
    showPermissions perms = concat [
        if readable perms then "r" else "-",
        if writable perms then "w" else "-",
        if executable perms then "x" else "-"
        ]

-- | Handle info for store path
handleStorePathInfo :: SPrivilegeTier t -> StorePath -> TenM 'Build t ()
handleStorePathInfo spt storePath = do
    -- Check if the path exists
    exists <- checkStorePathExists storePath

    if exists
        then do
            -- Get references to and from this path
            env <- ask
            let fullPath = storePathToFilePath storePath env

            -- Get file info
            size <- liftIO $ fromIntegral . fileSize <$> getFileStatus fullPath

            -- Get references
            refsTo <- getReferencesToPath storePath
            refsFrom <- getReferencesFromPath storePath

            -- Check if it's a GC root
            isRoot <- isGCRoot storePath

            -- Display store path info
            liftIO $ do
                putStrLn $ "Store path: " ++ T.unpack (storePathToText storePath)
                putStrLn $ "Size: " ++ formatSize size
                putStrLn $ "Hash: " ++ T.unpack (storeHash storePath)
                putStrLn $ "GC root: " ++ if isRoot then "Yes" else "No"
                putStrLn $ "Referenced by: " ++ show (Set.size refsTo) ++ " paths"
                putStrLn $ "References: " ++ show (Set.size refsFrom) ++ " paths"
        else liftIO $ putStrLn $ "Path does not exist in store: " ++ T.unpack (storePathToText storePath)
  where
    formatSize :: Integer -> String
    formatSize size
        | size < 1024 = show size ++ " B"
        | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
        | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
        | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"

-- Type class for daemon operations with privilege-specific implementations
class CanManageDaemon (t :: PrivilegeTier) where
    startDaemon :: TenM 'Build t ()
    stopDaemon :: TenM 'Build t ()
    restartDaemon :: TenM 'Build t ()
    getDaemonStatus :: TenM 'Build t DaemonStatus
    getDaemonConfig :: TenM 'Build t DaemonConfig

instance CanManageDaemon 'Daemon where
    startDaemon = do
        -- Check if daemon is already running
        isRunning <- checkDaemonRunning
        if isRunning
            then liftIO $ putStrLn "Daemon is already running"
            else do
                -- Start the daemon process
                env <- ask
                let storeDir = storeLocation env
                let workDir' = workDir env

                -- Create necessary directories
                liftIO $ do
                    createDirectoryIfMissing True storeDir
                    createDirectoryIfMissing True workDir'
                    createDirectoryIfMissing True (storeDir </> "var/ten")

                -- Start the daemon process
                liftIO $ putStrLn "Starting daemon..."
                -- Actual daemon starting code would go here

                -- Wait a bit and check if it started
                liftIO $ threadDelay 1000000  -- 1 second
                isRunning' <- checkDaemonRunning
                if isRunning'
                    then liftIO $ putStrLn "Daemon started successfully"
                    else liftIO $ putStrLn "Failed to start daemon"
      where
        checkDaemonRunning :: TenM 'Build 'Daemon Bool
        checkDaemonRunning = do
            -- Check if the daemon is running by checking for its PID file
            env <- ask
            let pidFile = storeLocation env </> "var/ten/daemon.pid"

            exists <- liftIO $ doesFileExist pidFile
            if not exists
                then return False
                else do
                    -- Read PID from file
                    pidStr <- liftIO $ readFile pidFile
                    case reads pidStr of
                        [(pid, "")] -> do
                            -- Check if process is running
                            isRunning <- liftIO $ checkProcessRunning pid
                            return isRunning
                        _ -> return False
          where
            checkProcessRunning :: ProcessID -> IO Bool
            checkProcessRunning pid = do
                -- Send signal 0 to check if process exists
                result <- try $ signalProcess 0 pid
                case result of
                    Left _ -> return False  -- Process doesn't exist
                    Right _ -> return True  -- Process exists

    stopDaemon = do
        -- Check if daemon is running
        isRunning <- checkDaemonRunning
        if not isRunning
            then liftIO $ putStrLn "Daemon is not running"
            else do
                -- Stop the daemon
                liftIO $ putStrLn "Stopping daemon..."
                -- Actual daemon stopping code would go here

                -- Wait a bit and check if it stopped
                liftIO $ threadDelay 1000000  -- 1 second
                isRunning' <- checkDaemonRunning
                if not isRunning'
                    then liftIO $ putStrLn "Daemon stopped successfully"
                    else liftIO $ putStrLn "Failed to stop daemon"
      where
        checkDaemonRunning :: TenM 'Build 'Daemon Bool
        checkDaemonRunning = do
            -- Check if the daemon is running by checking for its PID file
            env <- ask
            let pidFile = storeLocation env </> "var/ten/daemon.pid"

            exists <- liftIO $ doesFileExist pidFile
            if not exists
                then return False
                else do
                    -- Read PID from file
                    pidStr <- liftIO $ readFile pidFile
                    case reads pidStr of
                        [(pid, "")] -> do
                            -- Check if process is running
                            isRunning <- liftIO $ checkProcessRunning pid
                            return isRunning
                        _ -> return False
          where
            checkProcessRunning :: ProcessID -> IO Bool
            checkProcessRunning pid = do
                -- Send signal 0 to check if process exists
                result <- try $ signalProcess 0 pid
                case result of
                    Left _ -> return False  -- Process doesn't exist
                    Right _ -> return True  -- Process exists

    restartDaemon = do
        -- Simple implementation that just stops and starts
        stopDaemon
        liftIO $ threadDelay 1000000  -- 1 second
        startDaemon

    getDaemonStatus = do
        env <- ask

        -- Get store size and path count
        storeDir <- asks storeLocation
        allPaths <- listAllStorePaths

        -- Calculate total store size
        size <- foldM (\total path -> do
            let fullPath = storePathToFilePath path env
            exists <- liftIO $ doesFileExist fullPath
            if exists
                then do
                    stat <- liftIO $ getFileStatus fullPath
                    return $ total + fromIntegral (fileSize stat)
                else return total
            ) 0 allPaths

        -- Get database connection
        db <- getDatabaseConn

        -- Get build statistics from database
        (activeBuilds, completedBuilds, failedBuilds) <- withTenReadTransaction db $ \conn -> do
            activeRows <- tenQuery conn
                "SELECT COUNT(*) FROM ActiveBuilds WHERE status = 'running'"
                () :: TenM 'Build 'Daemon [Only Int]

            completedRows <- tenQuery conn
                "SELECT COUNT(*) FROM ActiveBuilds WHERE status = 'completed'"
                () :: TenM 'Build 'Daemon [Only Int]

            failedRows <- tenQuery conn
                "SELECT COUNT(*) FROM ActiveBuilds WHERE status = 'failed'"
                () :: TenM 'Build 'Daemon [Only Int]

            let active = case activeRows of
                    [Only count] -> count
                    _ -> 0

            let completed = case completedRows of
                    [Only count] -> count
                    _ -> 0

            let failed = case failedRows of
                    [Only count] -> count
                    _ -> 0

            return (active, completed, failed)

        -- Calculate uptime
        -- In a real implementation, we'd get this from the daemon process start time
        uptime <- liftIO $ do
            let pidFile = storeDir </> "var/ten/daemon.pid"
            exists <- doesFileExist pidFile
            if exists
                then do
                    stat <- getFileStatus pidFile
                    now <- getCurrentTime
                    let modTime = posixSecondsToUTCTime (fromIntegral $ modificationTime stat)
                    return $ realToFrac $ diffUTCTime now modTime
                else return 0

        -- Construct status
        return DaemonStatus {
            daemonStatus = "running",
            daemonUptime = uptime,
            daemonActiveBuilds = activeBuilds,
            daemonCompletedBuilds = completedBuilds,
            daemonFailedBuilds = failedBuilds,
            daemonGcRoots = 0,  -- We'd get this from GC.findGCRoots in a real impl
            daemonStoreSize = size,
            daemonStorePaths = length allPaths
        }

    getDaemonConfig = do
        -- Get daemon configuration
        env <- ask

        -- Simple placeholder - would actually read from config
        return DaemonConfig {
            daemonSocketPath = storeLocation env </> "var/ten/daemon.sock",
            daemonStorePath = storeLocation env,
            daemonStateFile = storeLocation env </> "var/ten/daemon.state",
            daemonLogFile = Just $ storeLocation env </> "var/log/ten.log",
            daemonLogLevel = 1,
            daemonGcInterval = Just 3600,
            daemonUser = Nothing,
            daemonGroup = Nothing,
            daemonAllowedUsers = Set.empty,
            daemonMaxJobs = 4,
            daemonForeground = False,
            daemonTmpDir = storeLocation env </> "tmp"
        }

instance CanManageDaemon 'Builder where
    startDaemon = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let request = Request {
                        reqId = 0,
                        reqType = "daemon-start",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right SuccessResponse -> liftIO $ putStrLn "Daemon started successfully"
                    Right _ -> throwError $ DaemonError "Unexpected response type"

            _ -> throwError $ PrivilegeError "Cannot start daemon without daemon connection"

    stopDaemon = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let request = Request {
                        reqId = 0,
                        reqType = "daemon-stop",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right SuccessResponse -> liftIO $ putStrLn "Daemon stopped successfully"
                    Right _ -> throwError $ DaemonError "Unexpected response type"

            _ -> throwError $ PrivilegeError "Cannot stop daemon without daemon connection"

    restartDaemon = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let request = Request {
                        reqId = 0,
                        reqType = "daemon-restart",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right SuccessResponse -> liftIO $ putStrLn "Daemon restarted successfully"
                    Right _ -> throwError $ DaemonError "Unexpected response type"

            _ -> throwError $ PrivilegeError "Cannot restart daemon without daemon connection"

    getDaemonStatus = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let request = Request {
                        reqId = 0,
                        reqType = "daemon-status",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (StatusResponse status) -> return status
                    Right _ -> throwError $ DaemonError "Unexpected response type"

            _ -> throwError $ PrivilegeError "Cannot get daemon status without daemon connection"

    getDaemonConfig = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let request = Request {
                        reqId = 0,
                        reqType = "daemon-config",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (ConfigResponse config) -> return config
                    Right _ -> throwError $ DaemonError "Unexpected response type"

            _ -> throwError $ PrivilegeError "Cannot get daemon config without daemon connection"

-- | Handle daemon commands
handleDaemon :: SPrivilegeTier t -> DaemonCommand -> TenM 'Build t ()
handleDaemon spt cmd = case fromSing spt of
    Daemon -> handleDaemonDaemon cmd
    Builder -> handleDaemonBuilder cmd

-- | Handle daemon commands in Daemon context
handleDaemonDaemon :: DaemonCommand -> TenM 'Build 'Daemon ()
handleDaemonDaemon cmd = do
    -- Check if running as daemon
    env <- ask
    unless (isRunningAsDaemon env) $
        throwError $ PrivilegeError "Daemon commands must be run in daemon mode"

    case cmd of
        DaemonStartCmd -> startDaemon
        DaemonStopCmd -> stopDaemon
        DaemonRestartCmd -> restartDaemon
        DaemonStatusCmd -> showDaemonStatus
        DaemonConfigCmd -> showDaemonConfig
  where
    isRunningAsDaemon :: BuildEnv -> Bool
    isRunningAsDaemon env = runMode env == DaemonMode

    showDaemonStatus :: TenM 'Build 'Daemon ()
    showDaemonStatus = do
        -- Get and display daemon status
        status <- getDaemonStatus
        liftIO $ do
            putStrLn $ "Daemon status: " ++ T.unpack (daemonStatus status)
            putStrLn $ "Uptime: " ++ formatTime (daemonUptime status)
            putStrLn $ "Active builds: " ++ show (daemonActiveBuilds status)
            putStrLn $ "Completed builds: " ++ show (daemonCompletedBuilds status)
            putStrLn $ "Failed builds: " ++ show (daemonFailedBuilds status)
            putStrLn $ "Store size: " ++ formatSize (daemonStoreSize status)
            putStrLn $ "Store paths: " ++ show (daemonStorePaths status)
      where
        formatTime :: Double -> String
        formatTime seconds
            | seconds < 60 = show (round seconds) ++ " seconds"
            | seconds < 3600 = show (round $ seconds / 60) ++ " minutes"
            | otherwise = show (round $ seconds / 3600) ++ " hours"

        formatSize :: Integer -> String
        formatSize size
            | size < 1024 = show size ++ " B"
            | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
            | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
            | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"

    showDaemonConfig :: TenM 'Build 'Daemon ()
    showDaemonConfig = do
        -- Get and display daemon configuration
        config <- getDaemonConfig
        liftIO $ do
            putStrLn "Daemon configuration:"
            putStrLn $ "  Socket: " ++ daemonSocketPath config
            putStrLn $ "  Store: " ++ daemonStorePath config
            putStrLn $ "  State file: " ++ daemonStateFile config
            putStrLn $ "  Log level: " ++ show (daemonLogLevel config)
            putStrLn $ "  Max jobs: " ++ show (daemonMaxJobs config)

-- | Handle daemon commands in Builder context
handleDaemonBuilder :: DaemonCommand -> TenM 'Build 'Builder ()
handleDaemonBuilder cmd = do
    env <- ask
    case runMode env of
        ClientMode conn -> do
            let request = case cmd of
                    DaemonStartCmd -> Request 0 "daemon-start" Map.empty Nothing
                    DaemonStopCmd -> Request 0 "daemon-stop" Map.empty Nothing
                    DaemonRestartCmd -> Request 0 "daemon-restart" Map.empty Nothing
                    DaemonStatusCmd -> Request 0 "daemon-status" Map.empty Nothing
                    DaemonConfigCmd -> Request 0 "daemon-config" Map.empty Nothing

            response <- liftIO $ sendRequestSync conn request 30000000
            case response of
                Left err ->
                    throwError err
                Right (StatusResponse status) -> liftIO $ do
                    putStrLn $ "Daemon status: " ++ T.unpack (daemonStatus status)
                    -- Display other status information
                Right (ConfigResponse config) -> liftIO $ do
                    putStrLn "Daemon configuration:"
                    putStrLn $ "  Socket: " ++ daemonSocketPath config
                    -- Display other config information
                Right SuccessResponse ->
                    liftIO $ putStrLn "Command executed successfully"
                Right _ ->
                    throwError $ DaemonError "Unexpected response type"
        _ ->
            throwError $ PrivilegeError "Daemon operations require daemon connection"

-- | List all paths in the store
listStorePaths :: TenM 'Build t [String]
listStorePaths = do
    env <- ask
    paths <- listAllStorePaths
    return $ map (T.unpack . storePathToText) paths

-- | Execute daemon command via protocol
daemonCommand :: String -> Options -> FilePath -> IO ()
daemonCommand args opts socket = do
    -- Connect to daemon
    creds <- getUserCredentials socket
    client <- DaemonClient.connectToDaemon socket creds
    case client of
        Left err -> do
            hPutStrLn stderr $ "Error connecting to daemon: " ++ show err
            exitWith $ ExitFailure 1

        Right conn -> do
            -- Parse as command
            let cmdStr:cmdArgs = words args
            let command = parseCommandString cmdStr cmdArgs

            -- Execute command through daemon
            case command of
                Left err -> do
                    hPutStrLn stderr $ "Error: " ++ err
                    exitWith $ ExitFailure 1

                Right cmd -> do
                    -- Create a request based on the command
                    let request = commandToRequest cmd

                    -- Send request to daemon
                    response <- DaemonClient.sendRequestSync conn request 300000000

                    -- Process response
                    case response of
                        Left err -> do
                            hPutStrLn stderr $ "Error: " ++ show err
                            exitWith $ ExitFailure 1

                        Right resp -> do
                            -- Display response
                            displayDaemonResponse cmd resp
  where
    -- Parse command string and arguments
    parseCommandString :: String -> [String] -> Either String Command
    parseCommandString "gc" _ = Right GCCmd
    parseCommandString "store" ("gc":_) = Right $ StoreCmd StoreGCCmd
    parseCommandString "store" ("list":_) = Right $ StoreCmd StoreListCmd
    parseCommandString "store" ("verify":path:_) = Right $ StoreCmd (StoreVerifyCmd path)
    parseCommandString "store" ("path":file:_) = Right $ StoreCmd (StorePathCmd file)
    parseCommandString "store" ("add":file:_) = Right $ StoreCmd (StoreAddCmd file)
    parseCommandString "daemon" ("status":_) = Right $ DaemonCmd DaemonStatusCmd
    parseCommandString "daemon" ("config":_) = Right $ DaemonCmd DaemonConfigCmd
    parseCommandString "daemon" ("start":_) = Right $ DaemonCmd DaemonStartCmd
    parseCommandString "daemon" ("stop":_) = Right $ DaemonCmd DaemonStopCmd
    parseCommandString "daemon" ("restart":_) = Right $ DaemonCmd DaemonRestartCmd
    parseCommandString cmd _ = Left $ "Unknown command: " ++ cmd

    -- Convert command to request
    commandToRequest :: Command -> Request
    commandToRequest GCCmd = Request 0 "gc" Map.empty Nothing
    commandToRequest (StoreCmd StoreGCCmd) = Request 0 "gc" Map.empty Nothing
    commandToRequest (StoreCmd StoreListCmd) = Request 0 "store-list" Map.empty Nothing
    commandToRequest (StoreCmd (StoreVerifyCmd path)) =
        Request 0 "store-verify" (Map.singleton "path" (T.pack path)) Nothing
    commandToRequest (StoreCmd (StorePathCmd path)) =
        Request 0 "store-path" (Map.singleton "path" (T.pack path)) Nothing
    commandToRequest (StoreCmd (StoreAddCmd path)) =
        Request 0 "store-add" (Map.singleton "path" (T.pack path)) Nothing
    commandToRequest (DaemonCmd DaemonStartCmd) = Request 0 "daemon-start" Map.empty Nothing
    commandToRequest (DaemonCmd DaemonStopCmd) = Request 0 "daemon-stop" Map.empty Nothing
    commandToRequest (DaemonCmd DaemonRestartCmd) = Request 0 "daemon-restart" Map.empty Nothing
    commandToRequest (DaemonCmd DaemonStatusCmd) = Request 0 "daemon-status" Map.empty Nothing
    commandToRequest (DaemonCmd DaemonConfigCmd) = Request 0 "daemon-config" Map.empty Nothing
    commandToRequest _ = Request 0 "unknown" Map.empty Nothing

    -- Display response
    displayDaemonResponse :: Command -> DaemonResponse -> IO ()
    displayDaemonResponse _ resp = case resp of
        GCResultResponse stats -> do
            putStrLn $ "GC completed: " ++ show (gcCollected stats) ++ " paths collected"
            putStrLn $ "  Total paths: " ++ show (gcTotal stats)
            putStrLn $ "  Live paths: " ++ show (gcLive stats)
            putStrLn $ "  Freed: " ++ formatSize (gcBytes stats)

        StoreListResponse paths -> do
            putStrLn $ "Store contains " ++ show (length paths) ++ " paths:"
            forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)

        StoreVerifyResponse valid ->
            putStrLn $ if valid then "Path is valid" else "Path is invalid"

        StorePathResponse path ->
            putStrLn $ T.unpack (storePathToText path)

        StoreAddResponse path ->
            putStrLn $ "Added to store: " ++ T.unpack (storePathToText path)

        StatusResponse status -> do
            putStrLn $ "Daemon status: " ++ T.unpack (daemonStatus status)
            putStrLn $ "  Uptime: " ++ formatTime (daemonUptime status)
            putStrLn $ "  Active builds: " ++ show (daemonActiveBuilds status)
            putStrLn $ "  Completed builds: " ++ show (daemonCompletedBuilds status)
            putStrLn $ "  Failed builds: " ++ show (daemonFailedBuilds status)

        ConfigResponse config -> do
            putStrLn "Daemon configuration:"
            putStrLn $ "  Socket: " ++ daemonSocketPath config
            putStrLn $ "  Store: " ++ daemonStorePath config
            putStrLn $ "  Log level: " ++ show (daemonLogLevel config)
            putStrLn $ "  Max jobs: " ++ show (daemonMaxJobs config)

        SuccessResponse ->
            putStrLn "Operation completed successfully"

        ErrorResponse err ->
            hPutStrLn stderr $ "Error: " ++ T.unpack (buildErrorToText err)

        _ -> putStrLn $ "Received response: " ++ show resp

    formatSize :: Integer -> String
    formatSize size
        | size < 1024 = show size ++ " B"
        | size < 1024 * 1024 = show (size `div` 1024) ++ " KB"
        | size < 1024 * 1024 * 1024 = show (size `div` (1024 * 1024)) ++ " MB"
        | otherwise = show (size `div` (1024 * 1024 * 1024)) ++ " GB"

    formatTime :: Double -> String
    formatTime seconds
        | seconds < 60 = show (round seconds) ++ " seconds"
        | seconds < 3600 = show (round $ seconds / 60) ++ " minutes"
        | otherwise = show (round $ seconds / 3600) ++ " hours"

-- | Get user credentials for daemon authentication
getUserCredentials :: FilePath -> IO DaemonClient.UserCredentials
getUserCredentials socket = do
    -- Check for credential file in home directory
    home <- getHomeDirectory
    let credFile = home </> ".ten/credentials"

    exists <- doesFileExist credFile
    if exists
        then do
            -- Read credentials from file
            content <- readFile credFile
            let lines' = filter (not . null) $ lines content

            case lines' of
                (user:token:_) -> return $ DaemonClient.UserCredentials {
                    DaemonClient.username = T.pack (strip user),
                    DaemonClient.token = T.pack (strip token),
                    DaemonClient.requestedTier = Builder  -- Default to Builder tier
                }
                _ -> fallbackCredentials
        else fallbackCredentials
  where
    strip :: String -> String
    strip = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

    fallbackCredentials :: IO DaemonClient.UserCredentials
    fallbackCredentials = do
        -- Default to current user
        username <- User.getLoginName
        -- Generate a random token (in reality this would be properly authenticated)
        token <- show <$> getProcessID
        return $ DaemonClient.UserCredentials {
            DaemonClient.username = T.pack username,
            DaemonClient.token = T.pack token,
            DaemonClient.requestedTier = Builder
        }

-- | Evaluate a Ten expression to a derivation
evaluateExpression :: SPrivilegeTier t -> BS.ByteString -> TenM 'Eval t Derivation
evaluateExpression spt content = do
    env <- ask

    -- In a real implementation, this would actually evaluate the expression
    -- For now, we'll just create a dummy derivation for demonstration
    result <- case fromSing spt of
        Daemon -> evaluateExpressionDaemon content
        Builder -> evaluateExpressionBuilder content

    return result
  where
    evaluateExpressionDaemon :: BS.ByteString -> TenM 'Eval 'Daemon Derivation
    evaluateExpressionDaemon content = do
        -- Create a basic builder
        builderPath <- createBasicBuilder sDaemon

        -- Create a derivation
        let derivation = Derivation {
                derivName = "example",
                derivHash = "0123456789abcdef0123456789abcdef",
                derivBuilder = builderPath,
                derivArgs = ["--build"],
                derivInputs = Set.empty,
                derivOutputs = Set.fromList [
                    DerivationOutput "out" $ Store.StorePath "0123456789abcdef" "example"
                ],
                derivEnv = Map.empty,
                derivSystem = "x86_64-linux",
                derivStrategy = ApplicativeStrategy,
                derivMeta = Map.empty
            }

        return derivation

    evaluateExpressionBuilder :: BS.ByteString -> TenM 'Eval 'Builder Derivation
    evaluateExpressionBuilder content = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create eval request
                let request = Request {
                        reqId = 0,
                        reqType = "eval-expression",
                        reqParams = Map.empty,
                        reqPayload = Just content
                    }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (EvalResponse drv) -> return drv
                    Right _ -> throwError $ EvalError "Unexpected response type"

            _ -> do
                -- Fallback for standalone mode - simplified evaluation
                builderPath <- createBasicBuilder sBuilder

                -- Create a simple derivation
                let derivation = Derivation {
                        derivName = "example",
                        derivHash = "0123456789abcdef0123456789abcdef",
                        derivBuilder = builderPath,
                        derivArgs = ["--build"],
                        derivInputs = Set.empty,
                        derivOutputs = Set.fromList [
                            DerivationOutput "out" $ Store.StorePath "0123456789abcdef" "example"
                        ],
                        derivEnv = Map.empty,
                        derivSystem = "x86_64-linux",
                        derivStrategy = ApplicativeStrategy,
                        derivMeta = Map.empty
                    }

                return derivation

-- | Create a basic builder for testing
createBasicBuilder :: SPrivilegeTier t -> TenM 'Eval t StorePath
createBasicBuilder spt = do
    env <- ask

    -- In a real implementation, this would create an actual builder
    -- For demo purposes, just return a dummy path
    result <- case fromSing spt of
        Daemon -> do
            -- In daemon context, could actually create/store the builder
            let dummyPath = Store.StorePath "0123456789abcdef" "basic-builder"
            return dummyPath

        Builder -> do
            -- In builder context, would request from daemon
            let dummyPath = Store.StorePath "0123456789abcdef" "basic-builder"
            return dummyPath

    return result

-- | Request to build a derivation through daemon
requestBuildDerivation :: SPrivilegeTier 'Builder -> Derivation -> TenM 'Build 'Builder BuildResult
requestBuildDerivation spt derivation = do
    env <- ask

    -- Get daemon connection
    case runMode env of
        ClientMode conn -> do
            -- Create build request
            let derivContent = Derivation.serializeDerivation derivation
            let request = Request {
                    reqId = 0,
                    reqType = "build-derivation",
                    reqParams = Map.singleton "hasDerivation" "true",
                    reqPayload = Just derivContent
                }

            -- Send request
            response <- liftIO $ sendRequestSync conn request 3600000000  -- 1 hour timeout

            -- Handle response
            case response of
                Left err -> throwError err
                Right (BuildResultResponse result) -> return result
                Right _ -> throwError $ BuildFailed "Unexpected response type"

        _ -> throwError $ DaemonError "Not connected to daemon"

-- | Helper function for handleBuildTenExpression
handleBuildTenExpression :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleBuildTenExpression spt file = case fromSing spt of
    Daemon -> handleBuildTenExpressionDaemon file
    Builder -> handleBuildTenExpressionBuilder file

-- | Handle build expression in Daemon context
handleBuildTenExpressionDaemon :: FilePath -> TenM 'Build 'Daemon ()
handleBuildTenExpressionDaemon file = do
    -- Read file content
    content <- liftIO $ BS.readFile file

    -- First evaluate the expression to get a derivation
    -- For this we need to run in the Eval phase
    derivation <- runTenDaemonEval (evaluateExpression sDaemon content)

    -- Now build the derivation
    logMsg 1 $ "Building derivation: " <> derivName derivation

    -- Type-safe operation dispatched through type class
    buildResult <- Core.buildDerivation derivation

    -- Report build result
    case brExitCode buildResult of
        ExitSuccess -> do
            liftIO $ putStrLn "Build succeeded!"
            liftIO $ putStrLn "Outputs:"
            forM_ (Set.toList $ brOutputPaths buildResult) $ \path ->
                liftIO $ putStrLn $ "  " ++ T.unpack (storePathToText path)

        ExitFailure code -> do
            liftIO $ hPutStrLn stderr $ "Build failed with exit code: " ++ show code
            liftIO $ hPutStrLn stderr $ "Build log:"
            liftIO $ hPutStrLn stderr $ T.unpack $ brLog buildResult
            throwError $ BuildFailed "Build failed"

-- | Handle build expression in Builder context
handleBuildTenExpressionBuilder :: FilePath -> TenM 'Build 'Builder ()
handleBuildTenExpressionBuilder file = do
    -- Read file content
    content <- liftIO $ BS.readFile file

    -- Go through the daemon for evaluation and building
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Evaluate expression through daemon
            let evalRequest = Request {
                    reqId = 0,
                    reqType = "eval",
                    reqParams = Map.singleton "file" (T.pack file),
                    reqPayload = Just content
                }

            evalResp <- liftIO $ sendRequestSync conn evalRequest 30000000
            case evalResp of
                Left err -> throwError err
                Right (EvalResponse drv) -> do
                    -- Now build the derivation
                    logMsg 1 $ "Building derivation: " <> derivName drv

                    -- Type-safe operation dispatched through type class
                    buildResult <- requestBuildDerivation sBuilder drv

                    -- Report build result
                    case brExitCode buildResult of
                        ExitSuccess -> do
                            liftIO $ putStrLn "Build succeeded!"
                            liftIO $ putStrLn "Outputs:"
                            forM_ (Set.toList $ brOutputPaths buildResult) $ \path ->
                                liftIO $ putStrLn $ "  " ++ T.unpack (storePathToText path)

                        ExitFailure code -> do
                            liftIO $ hPutStrLn stderr $ "Build failed with exit code: " ++ show code
                            liftIO $ hPutStrLn stderr $ "Build log:"
                            liftIO $ hPutStrLn stderr $ T.unpack $ brLog buildResult
                            throwError $ BuildFailed "Build failed"

                Right _ -> throwError $ BuildFailed "Unexpected response to eval request"
        _ ->
            throwError $ DaemonError "Not connected to daemon"

-- | Helper function for handleBuildDerivationFile
handleBuildDerivationFile :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleBuildDerivationFile spt file = case fromSing spt of
    Daemon -> handleBuildDerivationFileDaemon file
    Builder -> handleBuildDerivationFileBuilder file

-- | Handle build derivation file in Daemon context
handleBuildDerivationFileDaemon :: FilePath -> TenM 'Build 'Daemon ()
handleBuildDerivationFileDaemon file = do
    -- Read derivation file
    content <- liftIO $ BS.readFile file

    -- Deserialize the derivation
    drv <- case Derivation.deserializeDerivation content of
        Left err -> throwError err
        Right d -> return d

    -- Build the derivation using type class
    logMsg 1 $ "Building derivation: " <> derivName drv
    buildResult <- Core.buildDerivation drv

    -- Report build result
    case brExitCode buildResult of
        ExitSuccess -> do
            liftIO $ putStrLn "Build succeeded!"
            liftIO $ putStrLn "Outputs:"
            forM_ (Set.toList $ brOutputPaths buildResult) $ \path ->
                liftIO $ putStrLn $ "  " ++ T.unpack (storePathToText path)

        ExitFailure code -> do
            liftIO $ hPutStrLn stderr $ "Build failed with exit code: " ++ show code
            liftIO $ hPutStrLn stderr $ "Build log:"
            liftIO $ hPutStrLn stderr $ T.unpack $ brLog buildResult
            throwError $ BuildFailed "Build failed"

-- | Handle build derivation file in Builder context
handleBuildDerivationFileBuilder :: FilePath -> TenM 'Build 'Builder ()
handleBuildDerivationFileBuilder file = do
    -- Read derivation file
    content <- liftIO $ BS.readFile file

    -- Deserialize the derivation
    drv <- case Derivation.deserializeDerivation content of
        Left err -> throwError err
        Right d -> return d

    -- Build through daemon connection using type class
    logMsg 1 $ "Building derivation: " <> derivName drv
    buildResult <- requestBuildDerivation sBuilder drv

    -- Report build result
    case brExitCode buildResult of
        ExitSuccess -> do
            liftIO $ putStrLn "Build succeeded!"
            liftIO $ putStrLn "Outputs:"
            forM_ (Set.toList $ brOutputPaths buildResult) $ \path ->
                liftIO $ putStrLn $ "  " ++ T.unpack (storePathToText path)

        ExitFailure code -> do
            liftIO $ hPutStrLn stderr $ "Build failed with exit code: " ++ show code
            liftIO $ hPutStrLn stderr $ "Build log:"
            liftIO $ hPutStrLn stderr $ T.unpack $ brLog buildResult
            throwError $ BuildFailed "Build failed"

-- | Helper function for breaking stale locks
breakStaleLock :: SPrivilegeTier 'Daemon -> FilePath -> TenM 'Build 'Daemon ()
breakStaleLock spt lockPath = do
    -- Check if lock file exists
    exists <- liftIO $ doesFileExist lockPath

    when exists $ do
        -- Read the lock file
        content <- liftIO $ try $ readFile lockPath

        case content of
            Left (_ :: SomeException) ->
                logMsg 1 $ "Could not read lock file: " <> T.pack lockPath

            Right pidStr -> case reads pidStr of
                [(pid, "")] -> do
                    -- Check if process is still running
                    isRunning <- liftIO $ checkProcessRunning pid

                    unless isRunning $ do
                        -- Process is not running, break the lock
                        logMsg 1 $ "Breaking stale lock: " <> T.pack lockPath
                        liftIO $ removeFile lockPath `catch` \(e :: SomeException) ->
                            hPutStrLn stderr $ "Warning: Failed to remove stale lock: " ++ show e

                _ -> logMsg 1 $ "Invalid content in lock file: " <> T.pack lockPath
  where
    checkProcessRunning :: ProcessID -> IO Bool
    checkProcessRunning pid = do
        -- Try to send signal 0 to process (doesn't actually send a signal)
        result <- try $ signalProcess 0 pid
        case result of
            Left _ -> return False  -- Process doesn't exist
            Right _ -> return True   -- Process exists

-- | Helper function for breaking stale locks
breakStaleLock' :: FilePath -> IO ()
breakStaleLock' lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath

    when exists $ do
        -- Read the lock file
        content <- try $ readFile lockPath

        case content of
            Left (_ :: SomeException) ->
                hPutStrLn stderr $ "Could not read lock file: " ++ lockPath

            Right pidStr -> case reads pidStr of
                [(pid, "")] -> do
                    -- Check if process is still running
                    isRunning <- checkProcessRunning pid

                    unless isRunning $ do
                        -- Process is not running, break the lock
                        hPutStrLn stderr $ "Breaking stale lock: " ++ lockPath
                        removeFile lockPath `catch` \(e :: SomeException) ->
                            hPutStrLn stderr $ "Warning: Failed to remove stale lock: " ++ show e

                _ -> hPutStrLn stderr $ "Invalid content in lock file: " ++ lockPath
  where
    checkProcessRunning :: ProcessID -> IO Bool
    checkProcessRunning pid = do
        -- Try to send signal 0 to process (doesn't actually send a signal)
        result <- try $ signalProcess 0 pid
        case result of
            Left _ -> return False  -- Process doesn't exist
            Right _ -> return True   -- Process exists

-- | Helper function to get a database connection
getDatabaseConn :: TenM 'Build 'Daemon (Database 'Daemon)
getDatabaseConn = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- In a real implementation, this would use a connection pool
    -- For simplicity, we're creating a new connection each time
    conn <- liftIO $ SQL.open dbPath
    liftIO $ SQL.execute_ conn "PRAGMA foreign_keys = ON"

    -- Return the database connection wrapped in our Database type
    return $ Database conn dbPath True 5000 3 sDaemon

-- | Execute statement against the database
tenExecute :: (SQL.ToRow q) => Database 'Daemon -> Query -> q -> TenM 'Build 'Daemon ()
tenExecute db query params = do
    -- For a real implementation, this would handle retries and error cases
    liftIO $ SQL.execute (dbConn db) query params

-- | Query the database
tenQuery :: (SQL.ToRow q, SQL.FromRow r) => Database 'Daemon -> Query -> q -> TenM 'Build 'Daemon [r]
tenQuery db query params = do
    -- For a real implementation, this would handle retries and error cases
    liftIO $ SQL.query (dbConn db) query params
