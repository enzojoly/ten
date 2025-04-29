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
import Control.Monad (forM, forM_, when, unless, void)
import Control.Monad.Reader (ask, asks, runReaderT)
import Control.Monad.State (get, modify, gets)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Bits ((.&.), (.|.))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, isJust)
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
import System.Directory
import System.FilePath
import qualified System.Process as Process
import System.Exit
import System.Environment (getArgs)
import System.Console.GetOpt
import System.IO (hPutStrLn, stderr, Handle, hGetContents, hClose, IOMode(..), withFile, hSetBuffering, BufferMode(..))
import System.Posix.Files (setFileMode, getFileStatus, fileMode, fileOwner, fileGroup, setOwnerAndGroup)
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
import Data.Singletons

import Ten.Core
import qualified Ten.Core as Core
import Ten.Store
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
    optDaemonSocket :: Maybe FilePath
} deriving (Show, Eq)

-- | Default options
defaultOptions :: Options
defaultOptions = Options {
    optVerbosity = 1,
    optShowHelp = False,
    optShowVersion = False,
    optStoreDir = Nothing,
    optWorkDir = Nothing,
    optDaemonSocket = Nothing
}

-- | Privilege requirements for commands
data PrivilegeRequirement =
    DaemonRequired    -- Requires daemon privileges
  | BuilderSufficient -- Can be done from builder context
  deriving (Show, Eq)

-- | Command types
data Command
    = CmdBuild FilePath                -- Build a file
    | CmdEval FilePath                 -- Evaluate a file
    | CmdGC                           -- Run garbage collection
    | CmdStore StoreCommand           -- Store operations
    | CmdDeriv DerivationCommand      -- Derivation operations
    | CmdInfo FilePath                -- Show info about a path
    | CmdDaemon DaemonCommand         -- Daemon management
    | CmdHelp                         -- Show help
    | CmdVersion                      -- Show version
    deriving (Show, Eq)

-- | Store commands
data StoreCommand
    = StoreAdd FilePath                -- Add a file to the store
    | StoreVerify FilePath             -- Verify a store path
    | CmdStorePath FilePath           -- Calculate path for a file
    | StoreGC                         -- Run garbage collection
    | StoreList                       -- List store contents
    deriving (Show, Eq)

-- | Derivation commands
data DerivationCommand
    = DerivInfo FilePath               -- Show derivation info
    | QueryDeriver FilePath            -- Query the deriver of a path
    | StoreDerivation FilePath         -- Store a derivation
    | RegisterDerivation FilePath      -- Register a derivation
    | ListDerivations                  -- List all derivations
    deriving (Show, Eq)

-- | Daemon commands
data DaemonCommand
    = DaemonStart                      -- Start the daemon
    | DaemonStop                       -- Stop the daemon
    | DaemonStatus                     -- Show daemon status
    | DaemonRestart                    -- Restart the daemon
    | DaemonConfig                     -- Show daemon configuration
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
    ]

-- Helper function to sanitize file paths
sanitizeFilePath :: FilePath -> FilePath
sanitizeFilePath path = normalise path

-- | Parse command line arguments
parseArgs :: [String] -> Either String (Command, Options)
parseArgs [] = Right (CmdHelp, defaultOptions)
parseArgs (cmdStr:args) = do
    let (flags, nonOpts, errors) = getOpt Permute options args

    unless (null errors) $
        Left $ "Error parsing options: " ++ concat errors

    let opts = foldr ($) defaultOptions flags

    cmd <- case cmdStr of
        "build" ->
            case nonOpts of
                (file:_) -> Right $ CmdBuild (sanitizeFilePath file)
                [] -> Left "build: missing file argument"
        "eval" ->
            case nonOpts of
                (file:_) -> Right $ CmdEval (sanitizeFilePath file)
                [] -> Left "eval: missing file argument"
        "gc" -> Right CmdGC
        "store" ->
            case nonOpts of
                [] -> Left "store: missing subcommand"
                (subcmd:args') ->
                    case subcmd of
                        "add" ->
                            case args' of
                                (file:_) -> Right $ CmdStore (StoreAdd (sanitizeFilePath file))
                                [] -> Left "store add: missing file argument"
                        "verify" ->
                            case args' of
                                (path:_) -> Right $ CmdStore (StoreVerify (sanitizeFilePath path))
                                [] -> Left "store verify: missing path argument"
                        "path" ->
                            case args' of
                                (file:_) -> Right $ CmdStore (CmdStorePath (sanitizeFilePath file))
                                [] -> Left "store path: missing file argument"
                        "gc" -> Right $ CmdStore StoreGC
                        "list" -> Right $ CmdStore StoreList
                        _ -> Left $ "unknown store subcommand: " ++ subcmd
        "derivation" ->
            case nonOpts of
                [] -> Left "derivation: missing subcommand"
                (subcmd:args') ->
                    case subcmd of
                        "info" ->
                            case args' of
                                (path:_) -> Right $ CmdDeriv (DerivInfo (sanitizeFilePath path))
                                [] -> Left "derivation info: missing path argument"
                        "query-deriver" ->
                            case args' of
                                (path:_) -> Right $ CmdDeriv (QueryDeriver (sanitizeFilePath path))
                                [] -> Left "derivation query-deriver: missing path argument"
                        "store" ->
                            case args' of
                                (file:_) -> Right $ CmdDeriv (StoreDerivation (sanitizeFilePath file))
                                [] -> Left "derivation store: missing file argument"
                        "register" ->
                            case args' of
                                (file:_) -> Right $ CmdDeriv (RegisterDerivation (sanitizeFilePath file))
                                [] -> Left "derivation register: missing file argument"
                        "list" -> Right $ CmdDeriv ListDerivations
                        _ -> Left $ "unknown derivation subcommand: " ++ subcmd
        "info" ->
            case nonOpts of
                (path:_) -> Right $ CmdInfo (sanitizeFilePath path)
                [] -> Left "info: missing path argument"
        "daemon" ->
            case nonOpts of
                [] -> Left "daemon: missing subcommand"
                (subcmd:_) ->
                    case subcmd of
                        "start" -> Right $ CmdDaemon DaemonStart
                        "stop" -> Right $ CmdDaemon DaemonStop
                        "status" -> Right $ CmdDaemon DaemonStatus
                        "restart" -> Right $ CmdDaemon DaemonRestart
                        "config" -> Right $ CmdDaemon DaemonConfig
                        _ -> Left $ "unknown daemon subcommand: " ++ subcmd
        "help" -> Right CmdHelp
        "version" -> Right CmdVersion
        _ -> Left $ "unknown command: " ++ cmdStr

    return (cmd, opts)

-- | Determine the privilege requirement for a command
commandPrivilege :: Command -> PrivilegeRequirement
commandPrivilege = \case
    CmdBuild _ -> BuilderSufficient       -- Build can use either context
    CmdEval _ -> BuilderSufficient        -- Eval can use either context
    CmdStore (StoreAdd _) -> DaemonRequired    -- Adding to store needs daemon privileges
    CmdStore (StoreList) -> BuilderSufficient  -- Listing can use either
    CmdStore (StoreVerify _) -> BuilderSufficient  -- Verification can use either
    CmdStore (CmdStorePath _) -> BuilderSufficient  -- Path calculation can use either
    CmdStore (StoreGC) -> DaemonRequired       -- GC needs daemon privileges
    CmdGC -> DaemonRequired                   -- GC needs daemon privileges
    CmdDeriv (RegisterDerivation _) -> DaemonRequired  -- Registration needs daemon
    CmdDeriv (StoreDerivation _) -> DaemonRequired    -- Store write needs daemon
    CmdDeriv (DerivInfo _) -> BuilderSufficient  -- Info can use either
    CmdDeriv (QueryDeriver _) -> BuilderSufficient    -- Query can use either
    CmdDeriv (ListDerivations) -> BuilderSufficient   -- Listing can use either
    CmdInfo _ -> BuilderSufficient             -- Info can use either
    CmdDaemon _ -> DaemonRequired              -- Daemon management needs daemon privileges
    CmdHelp -> BuilderSufficient              -- Help works in any context
    CmdVersion -> BuilderSufficient           -- Version info works in any context

-- | Determine which phase a command belongs to
commandPhase :: Command -> Core.Phase
commandPhase = \case
    CmdBuild _ -> Core.Build              -- Build command uses Build phase
    CmdEval _ -> Core.Eval                -- Eval command uses Eval phase
    CmdStore _ -> Core.Build              -- Store commands use Build phase
    CmdGC -> Core.Build                   -- GC uses Build phase
    CmdDeriv _ -> Core.Eval               -- Derivation commands use Eval phase
    CmdInfo _ -> Core.Build               -- Info works in Build phase
    CmdDaemon _ -> Core.Build             -- Daemon commands use Build phase
    CmdHelp -> Core.Build                 -- Help works in any phase
    CmdVersion -> Core.Build              -- Version works in any phase

-- | Helper to get user name with appropriate qualification
userName :: User.UserEntry -> String
userName = User.userName

-- | Type alias for daemon connection
type DaemonConnection = DaemonClient.DaemonConnection 'Builder
type DerivInfoResult = (Text, Text)  -- Simple definition for the CLI module's needs

-- | Main entry point to run a command
runCommand :: Command -> Options -> IO ()
runCommand cmd opts = case commandPrivilege cmd of
    -- Commands that can run in builder context (potentially via daemon)
    BuilderSufficient -> do
        let storeDir = fromMaybe "/var/lib/ten/store" (optStoreDir opts)
        let workDir = fromMaybe "/var/lib/ten/work" (optWorkDir opts)

        -- Check if daemon socket is specified, attempt to connect
        daemonResult <- case optDaemonSocket opts of
            Just socket -> do
                client <- DaemonClient.connectToDaemon socket
                case client of
                    Left err -> do
                        hPutStrLn stderr $ "Warning: Could not connect to daemon: " ++ show err
                        return Nothing
                    Right conn -> do
                        -- Authenticate as builder
                        creds <- getUserCredentials socket
                        result <- DaemonClient.authenticate conn creds
                        case result of
                            Left err -> do
                                hPutStrLn stderr $ "Authentication failed: " ++ show err
                                return Nothing
                            Right _ -> return $ Just conn
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
                result <- evalTen (handleCommand cmd opts) env' (initBuildState Core.Eval bid)
                case result of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        exitWith (ExitFailure 1)
                    Right _ -> return ()

            Core.Build -> do
                bid <- BuildId <$> newUnique
                result <- buildTen (handleCommand cmd opts) env' (initBuildState Core.Build bid)
                case result of
                    Left err -> do
                        hPutStrLn stderr $ "Error: " ++ show err
                        exitWith (ExitFailure 1)
                    Right _ -> return ()

    -- Commands that require daemon privileges
    DaemonRequired -> case optDaemonSocket opts of
        -- If socket specified, use the daemon
        Just socket -> daemonCommand (commandToString cmd) opts socket

        -- Otherwise, attempt to get daemon privileges
        Nothing -> do
            uid <- User.getRealUserID
            if uid == 0
                then do
                    -- Running as root, use daemon privileges
                    let storeDir = fromMaybe "/var/lib/ten/store" (optStoreDir opts)
                    let workDir = fromMaybe "/var/lib/ten/work" (optWorkDir opts)
                    let env = initDaemonEnv workDir storeDir Nothing
                    let env' = env { verbosity = optVerbosity opts }

                    -- Run the command based on its phase
                    case commandPhase cmd of
                        Core.Eval -> do
                            bid <- BuildId <$> newUnique
                            result <- evalTen (handleCommand cmd opts) env' (initBuildState Core.Eval bid)
                            case result of
                                Left err -> do
                                    hPutStrLn stderr $ "Error: " ++ show err
                                    exitWith (ExitFailure 1)
                                Right _ -> return ()

                        Core.Build -> do
                            bid <- BuildId <$> newUnique
                            result <- buildTen (handleCommand cmd opts) env' (initBuildState Core.Build bid)
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
                    exitWith (ExitFailure 1)

-- | Handler for all commands
handleCommand :: Command -> Options -> TenM p t ()
handleCommand cmd opts = case cmd of
    CmdBuild file -> handleBuild (sing @t) file
    CmdEval file -> handleEval (sing @t) file
    CmdGC -> handleGC (sing @('Daemon)) True
    CmdStore subcmd -> handleStore (sing @t) subcmd
    CmdDeriv subcmd -> handleDerivation (sing @t) subcmd
    CmdInfo path -> handleInfo (sing @t) path
    CmdDaemon subcmd -> handleDaemon (sing @('Daemon)) subcmd
    CmdHelp -> showHelp
    CmdVersion -> showVersion

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
    putStrLn "Options:"
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
    CmdBuild path -> "build " ++ path
    CmdEval path -> "eval " ++ path
    CmdGC -> "gc"
    CmdStore subcmd -> case subcmd of
        StoreAdd path -> "store add " ++ path
        StoreVerify path -> "store verify " ++ path
        CmdStorePath path -> "store path " ++ path
        StoreGC -> "store gc"
        StoreList -> "store list"
    CmdDeriv subcmd -> case subcmd of
        DerivInfo path -> "derivation info " ++ path
        QueryDeriver path -> "derivation query-deriver " ++ path
        StoreDerivation path -> "derivation store " ++ path
        RegisterDerivation path -> "derivation register " ++ path
        ListDerivations -> "derivation list"
    CmdInfo path -> "info " ++ path
    CmdDaemon subcmd -> case subcmd of
        DaemonStart -> "daemon start"
        DaemonStop -> "daemon stop"
        DaemonStatus -> "daemon status"
        DaemonRestart -> "daemon restart"
        DaemonConfig -> "daemon config"
    CmdHelp -> "help"
    CmdVersion -> "version"

-- | Convert command to argument list
commandToArgs :: Command -> [String]
commandToArgs cmd = words (commandToString cmd)

-- | Execute daemon command (via client protocol)
executeDaemonCommand :: SPrivilegeTier 'Builder -> Command -> Options -> DaemonConnection -> IO Bool
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
        CmdGC -> Request 0 "gc" Map.empty Nothing
        CmdStore StoreGC -> Request 0 "gc" Map.empty Nothing
        CmdStore StoreList -> Request 0 "store-list" Map.empty Nothing
        CmdStore (StoreVerify path) -> Request 0 "store-verify"
                                       (Map.singleton "path" (T.pack path)) Nothing
        CmdStore (CmdStorePath path) -> do
            -- For getting a store path, we need to read the file and send it
            Request 0 "store-path"
                   (Map.singleton "path" (T.pack path))
                   Nothing  -- Would need file content here
        CmdStore (StoreAdd path) -> do
            -- For adding to store, we need to read the file and send it
            Request 0 "store-add"
                   (Map.singleton "path" (T.pack path))
                   Nothing  -- Would need file content here
        CmdDeriv (DerivInfo path) ->
            Request 0 "derivation-info" (Map.singleton "path" (T.pack path)) Nothing
        CmdDeriv ListDerivations ->
            Request 0 "list-derivations" Map.empty Nothing
        CmdDaemon DaemonStatus ->
            Request 0 "status" Map.empty Nothing
        CmdDaemon DaemonConfig ->
            Request 0 "config" Map.empty Nothing
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
executeStoreCommand :: SPrivilegeTier 'Builder -> StoreCommand -> DaemonConnection -> Options -> IO Bool
executeStoreCommand spt cmd conn opts = case cmd of
    StoreAdd file -> do
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

    StoreVerify path -> do
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

    CmdStorePath file -> do
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

    StoreGC -> do
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

    StoreList -> do
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
executeDerivationCommand :: SPrivilegeTier 'Builder -> DerivationCommand -> DaemonConnection -> Options -> IO Bool
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

    -- Evaluate the expression
    result <- evaluateExpression spt content

    -- Print the result
    liftIO $ putStrLn $ "Evaluated to derivation: " ++ T.unpack (derivName result)

    -- Output details about the derivation
    liftIO $ do
        putStrLn $ "Builder: " ++ T.unpack (storePathToText (derivBuilder result))
        putStrLn $ "Hash: " ++ T.unpack (derivHash result)
        putStrLn $ "System: " ++ T.unpack (derivSystem result)
        putStrLn $ "Inputs: " ++ show (Set.size (derivInputs result))
        putStrLn $ "Outputs: " ++ show (Set.size (derivOutputs result))

-- | Handle GC command
handleGC :: SPrivilegeTier 'Daemon -> Bool -> TenM 'Build 'Daemon ()
handleGC spt dryRun = do
    -- Acquire GC lock
    env <- ask
    let lockPath = gcLockPath (storeLocation env)

    -- Check for stale lock
    liftIO $ breakStaleLock' lockPath

    -- Run garbage collection
    logMsg 1 "Starting garbage collection"
    when dryRun $
        logMsg 1 "Dry run mode - no paths will be deleted"

    let gcFunc = if dryRun
                 then collectGarbageDryRun
                 else collectGarbage

    (collected, live, bytes) <- withGCLock gcFunc

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

    -- Dry run version that doesn't delete anything
    collectGarbageDryRun :: TenM 'Build 'Daemon (Int, Int, Integer)
    collectGarbageDryRun = do
        env <- ask
        let storeDir = storeLocation env

        -- Find all store paths
        allPaths <- listAllStorePaths

        -- Find all roots
        rootPaths <- findGCRoots

        -- Find reachable paths
        reachable <- computeReachablePaths rootPaths

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

        -- Report only, don't delete anything
        return (length unreachable, length allPaths - length unreachable, totalSize)

-- | Handle store commands
handleStore :: SPrivilegeTier t -> StoreCommand -> TenM 'Build t ()
handleStore spt cmd = case cmd of
    StoreAdd file -> case spt of
        SDaemon -> do
            -- Check if file exists
            exists <- liftIO $ doesFileExist file
            unless exists $
                throwError $ InputNotFound file

            -- Determine if it's a file or directory
            isDir <- liftIO $ doesDirectoryExist file
            result <- if isDir
                        then storeDirectory file
                        else storeFile file

            -- Output the result
            liftIO $ putStrLn $ "Added to store: " ++ T.unpack (storePathToText result)

        _ -> throwError $ PrivilegeError "Storing files requires daemon privileges"

    StoreVerify path -> do
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

    CmdStorePath file -> do
        -- Check if file exists
        exists <- liftIO $ doesFileExist file
        unless exists $
            throwError $ InputNotFound file

        -- Calculate hash and display store path
        content <- liftIO $ BS.readFile file
        let nameHint = T.pack $ takeFileName file
        storePathForContent nameHint content

    StoreGC -> case spt of
        SDaemon -> handleGC spt False
        _ -> throwError $ PrivilegeError "Garbage collection requires daemon privileges"

    StoreList -> do
        -- List store paths
        paths <- listStorePaths
        liftIO $ do
            putStrLn $ "Found " ++ show (length paths) ++ " paths in store:"
            forM_ paths $ \path -> putStrLn $ "  " ++ path
  where
    -- Calculate store path for content without storing it
    storePathForContent :: Text -> BS.ByteString -> TenM 'Build t ()
    storePathForContent nameHint content = do
        env <- ask

        -- Calculate hash of content
        let contentHashDigest = hashByteString content
        let contentHash = T.pack $ show contentHashDigest

        -- Create store path
        let path = StorePath contentHash nameHint

        -- Display the path
        liftIO $ putStrLn $ T.unpack (storePathToText path)

-- | Handle derivation commands
handleDerivation :: SPrivilegeTier t -> DerivationCommand -> TenM 'Eval t ()
handleDerivation spt cmd = case cmd of
    DerivInfo path -> case textToStorePath (T.pack path) of
        Just storePath -> do
            -- Read the derivation
            result <- case spt of
                SDaemon -> readDerivationDaemon storePath
                _ -> readDerivationBuilder storePath

            -- Process the result
            case result of
                Left err -> throwError err
                Right drv -> do
                    -- Display derivation info
                    liftIO $ do
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

        Nothing -> throwError $ StoreError $ "Invalid store path format: " <> T.pack path

    QueryDeriver path -> case textToStorePath (T.pack path) of
        Just storePath -> do
            -- Query for the deriver in the database
            env <- ask
            case spt of
                SDaemon -> do
                    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                        result <- DB.withTenReadTransaction db $ \conn -> do
                            DBDeriv.getDeriverForPath storePath

                        case result of
                            Just deriverPath -> liftIO $ putStrLn $
                                "Deriver: " ++ T.unpack (storePathToText deriverPath)
                            Nothing -> liftIO $ putStrLn "No deriver found for this path"

                _ -> throwError $ PrivilegeError "Querying derivers requires daemon privileges"

        Nothing -> throwError $ StoreError $ "Invalid store path format: " <> T.pack path

    StoreDerivation file -> case spt of
        SDaemon -> do
            -- Check if file exists
            exists <- liftIO $ doesFileExist file
            unless exists $
                throwError $ InputNotFound file

            -- Read and deserialize the derivation
            content <- liftIO $ BS.readFile file
            deriv <- case deserializeDerivation content of
                Left err -> throwError err
                Right drv -> return drv

            -- Store the derivation
            path <- storeDerivationDaemon deriv
            liftIO $ putStrLn $ "Stored derivation at: " ++ T.unpack (storePathToText path)

        _ -> throwError $ PrivilegeError "Storing derivations requires daemon privileges"

    RegisterDerivation file -> case spt of
        SDaemon -> do
            -- Check if file exists
            exists <- liftIO $ doesFileExist file
            unless exists $
                throwError $ InputNotFound file

            -- Read and deserialize the derivation
            content <- liftIO $ BS.readFile file
            deriv <- case deserializeDerivation content of
                Left err -> throwError err
                Right drv -> return drv

            -- Store and register the derivation
            env <- ask
            withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                -- Store the derivation first
                path <- storeDerivationDaemon deriv

                -- Register it in the database
                derivId <- DB.withTenWriteTransaction db $ \conn -> do
                    DBDeriv.registerDerivationFile deriv path

                -- Output results
                liftIO $ do
                    putStrLn $ "Registered derivation: " ++ T.unpack (derivName deriv)
                    putStrLn $ "Storage path: " ++ T.unpack (storePathToText path)
                    putStrLn $ "Database ID: " ++ show derivId

        _ -> throwError $ PrivilegeError "Registering derivations requires daemon privileges"

    ListDerivations -> do
        -- List all derivations in the store
        env <- ask
        case spt of
            SDaemon -> do
                withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                    derivs <- DB.withTenReadTransaction db $ \conn -> do
                        DBDeriv.listAllDerivations 100

                    liftIO $ do
                        putStrLn $ "Found " ++ show (length derivs) ++ " derivations:"
                        forM_ derivs $ \(path, name) ->
                            putStrLn $ "  " ++ T.unpack name ++ ": " ++ T.unpack (storePathToText path)

            _ -> do
                -- In builder context, we need to go through daemon
                conn <- getDaemonConnection
                let request = Request {
                        reqId = 0,
                        reqType = "list-derivations",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationListResponse paths) -> do
                        liftIO $ do
                            putStrLn $ "Found " ++ show (length paths) ++ " derivations:"
                            forM_ paths $ \path -> putStrLn $ "  " ++ T.unpack (storePathToText path)
                    Right _ -> throwError $ StoreError "Unexpected response type"

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
            Just storePath -> do
                -- Get store path info
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
                        isRoot <- case spt of
                            SDaemon -> isGCRoot storePath
                            _ -> return False

                        -- Display store path info
                        liftIO $ do
                            putStrLn $ "Store path: " ++ T.unpack (storePathToText storePath)
                            putStrLn $ "Size: " ++ formatSize size
                            putStrLn $ "Hash: " ++ T.unpack (storeHash storePath)
                            putStrLn $ "GC root: " ++ if isRoot then "Yes" else "No"
                            putStrLn $ "Referenced by: " ++ show (Set.size refsTo) ++ " paths"
                            putStrLn $ "References: " ++ show (Set.size refsFrom) ++ " paths"
                    else liftIO $ putStrLn $ "Path does not exist in store: " ++ path

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

-- | Handle daemon commands
handleDaemon :: SPrivilegeTier 'Daemon -> DaemonCommand -> TenM 'Build 'Daemon ()
handleDaemon spt cmd = do
    env <- ask

    -- Check if running as daemon
    unless (isRunningAsDaemon env) $
        throwError $ PrivilegeError "Daemon commands must be run in daemon mode"

    case cmd of
        DaemonStart -> startDaemon
        DaemonStop -> stopDaemon
        DaemonStatus -> showDaemonStatus
        DaemonRestart -> restartDaemon
        DaemonConfig -> showDaemonConfig
  where
    isRunningAsDaemon :: BuildEnv -> Bool
    isRunningAsDaemon env = runMode env == DaemonMode

    startDaemon :: TenM 'Build 'Daemon ()
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

    stopDaemon :: TenM 'Build 'Daemon ()
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

    showDaemonStatus :: TenM 'Build 'Daemon ()
    showDaemonStatus = do
        -- Get daemon status
        status <- getDaemonStatus

        -- Display status
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

    restartDaemon :: TenM 'Build 'Daemon ()
    restartDaemon = do
        -- Stop and then start the daemon
        stopDaemon
        liftIO $ threadDelay 1000000  -- 1 second
        startDaemon

    showDaemonConfig :: TenM 'Build 'Daemon ()
    showDaemonConfig = do
        -- Get daemon configuration
        env <- ask

        -- Display configuration
        liftIO $ do
            putStrLn "Daemon configuration:"
            putStrLn $ "Store directory: " ++ storeLocation env
            putStrLn $ "Work directory: " ++ workDir env
            putStrLn $ "Verbosity: " ++ show (verbosity env)
            putStrLn $ "Max concurrent builds: " ++ showMaybe (maxConcurrentBuilds env)
            putStrLn $ "Max recursion depth: " ++ show (maxRecursionDepth env)
      where
        showMaybe :: Maybe Int -> String
        showMaybe Nothing = "unlimited"
        showMaybe (Just n) = show n

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

-- | List all paths in the store
listStorePaths :: TenM 'Build t [FilePath]
listStorePaths = do
    env <- ask
    let storeDir = storeLocation env

    -- List all items in the store
    exists <- liftIO $ doesDirectoryExist storeDir
    if not exists
        then return []
        else do
            -- List directory contents
            entries <- liftIO $ listDirectory storeDir

            -- Filter out special directories and files
            let validEntries = filter (\e -> not $ e `elem` ["gc-roots", "tmp", "var", "locks"]) entries

            -- Return full paths
            return $ map (\e -> storeDir </> e) validEntries

-- | Get daemon status information
getDaemonStatus :: TenM 'Build 'Daemon DaemonStatus
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

    -- Get active builds from database
    activeBuilds <- withDatabase (defaultDBPath storeDir) 5000 $ \db -> do
        DB.withTenReadTransaction db $ \conn -> do
            rows <- DB.query conn
                "SELECT COUNT(*) FROM ActiveBuilds WHERE status = 'running'"
                () :: TenM 'Build 'Daemon [Only Int]
            case rows of
                [Only count] -> return count
                _ -> return 0

    -- Get completed builds from database
    completedBuilds <- withDatabase (defaultDBPath storeDir) 5000 $ \db -> do
        DB.withTenReadTransaction db $ \conn -> do
            rows <- DB.query conn
                "SELECT COUNT(*) FROM ActiveBuilds WHERE status = 'completed'"
                () :: TenM 'Build 'Daemon [Only Int]
            case rows of
                [Only count] -> return count
                _ -> return 0

    -- Get failed builds from database
    failedBuilds <- withDatabase (defaultDBPath storeDir) 5000 $ \db -> do
        DB.withTenReadTransaction db $ \conn -> do
            rows <- DB.query conn
                "SELECT COUNT(*) FROM ActiveBuilds WHERE status = 'failed'"
                () :: TenM 'Build 'Daemon [Only Int]
            case rows of
                [Only count] -> return count
                _ -> return 0

    -- Calculate uptime
    -- In a real implementation, we'd get this from the daemon process start time
    uptime <- liftIO $ do
        let pidFile = storeDir </> "var/ten/daemon.pid"
        exists <- doesFileExist pidFile
        if exists
            then do
                stat <- getFileStatus pidFile
                now <- getCurrentTime
                let modTime = posixSecondsToUTCTime (fromIntegral $ Posix.modificationTime stat)
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

-- | Execute daemon command via protocol
daemonCommand :: SPrivilegeTier 'Daemon -> [String] -> FilePath -> IO ExitCode
daemonCommand spt args socket = do
    -- Connect to daemon
    client <- DaemonClient.connectToDaemon socket
    case client of
        Left err -> do
            hPutStrLn stderr $ "Error connecting to daemon: " ++ show err
            return $ ExitFailure 1

        Right conn -> do
            -- Authenticate with daemon
            creds <- getUserCredentials socket
            result <- DaemonClient.authenticate conn creds
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Authentication failed: " ++ show err
                    return $ ExitFailure 1

                Right _ -> do
                    -- Parse as command
                    let cmdStr:cmdArgs = args
                    let command = parseCommandString cmdStr cmdArgs

                    -- Execute command through daemon
                    case command of
                        Left err -> do
                            hPutStrLn stderr $ "Error: " ++ err
                            return $ ExitFailure 1

                        Right cmd -> do
                            -- Create a request based on the command
                            let request = commandToRequest cmd

                            -- Send request to daemon
                            response <- DaemonClient.sendRequestSync conn request 300000000

                            -- Process response
                            case response of
                                Left err -> do
                                    hPutStrLn stderr $ "Error: " ++ show err
                                    return $ ExitFailure 1

                                Right resp -> do
                                    -- Display response
                                    displayDaemonResponse cmd resp
                                    return ExitSuccess
  where
    -- Parse command string and arguments
    parseCommandString :: String -> [String] -> Either String Command
    parseCommandString "gc" _ = Right CmdGC
    parseCommandString "store" ("gc":_) = Right $ CmdStore StoreGC
    parseCommandString "store" ("list":_) = Right $ CmdStore StoreList
    parseCommandString "store" ("verify":path:_) = Right $ CmdStore (StoreVerify path)
    parseCommandString "store" ("path":file:_) = Right $ CmdStore (CmdStorePath file)
    parseCommandString "store" ("add":file:_) = Right $ CmdStore (StoreAdd file)
    parseCommandString "daemon" ("status":_) = Right $ CmdDaemon DaemonStatus
    parseCommandString "daemon" ("config":_) = Right $ CmdDaemon DaemonConfig
    parseCommandString "daemon" ("start":_) = Right $ CmdDaemon DaemonStart
    parseCommandString "daemon" ("stop":_) = Right $ CmdDaemon DaemonStop
    parseCommandString "daemon" ("restart":_) = Right $ CmdDaemon DaemonRestart
    parseCommandString cmd _ = Left $ "Unknown command: " ++ cmd

    -- Convert command to request
    commandToRequest :: Command -> Request
    commandToRequest CmdGC = Request 0 "gc" Map.empty Nothing
    commandToRequest (CmdStore StoreGC) = Request 0 "gc" Map.empty Nothing
    commandToRequest (CmdStore StoreList) = Request 0 "store-list" Map.empty Nothing
    commandToRequest (CmdStore (StoreVerify path)) =
        Request 0 "store-verify" (Map.singleton "path" (T.pack path)) Nothing
    commandToRequest (CmdStore (CmdStorePath path)) =
        Request 0 "store-path" (Map.singleton "path" (T.pack path)) Nothing
    commandToRequest (CmdStore (StoreAdd path)) =
        Request 0 "store-add" (Map.singleton "path" (T.pack path)) Nothing
    commandToRequest (CmdDaemon DaemonStatus) = Request 0 "status" Map.empty Nothing
    commandToRequest (CmdDaemon DaemonConfig) = Request 0 "config" Map.empty Nothing
    commandToRequest (CmdDaemon DaemonStart) = Request 0 "daemon-start" Map.empty Nothing
    commandToRequest (CmdDaemon DaemonStop) = Request 0 "daemon-stop" Map.empty Nothing
    commandToRequest (CmdDaemon DaemonRestart) = Request 0 "daemon-restart" Map.empty Nothing
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
        token <- show <$> randomIO
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

    -- Create a basic builder
    builderPath <- createBasicBuilder spt

    -- Create a derivation
    let derivation = Derivation {
            derivName = "example",
            derivHash = "0123456789abcdef0123456789abcdef",
            derivBuilder = builderPath,
            derivArgs = ["--build"],
            derivInputs = Set.empty,
            derivOutputs = Set.fromList [
                DerivationOutput "out" $ StorePath "0123456789abcdef" "example"
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

    let dummyPath = StorePath "0123456789abcdef" "basic-builder"
    return dummyPath

-- | Request to build a derivation
requestBuildDerivation :: SPrivilegeTier 'Builder -> Derivation -> TenM 'Build 'Builder BuildResult
requestBuildDerivation spt derivation = do
    env <- ask

    -- Get daemon connection
    case runMode env of
        ClientMode conn -> do
            -- Create build request
            let derivContent = serializeDerivation derivation
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
handleBuildTenExpression spt file = do
    -- Read file content
    content <- liftIO $ BS.readFile file

    -- Execute expression
    case spt of
        SDaemon -> do
            -- First evaluate the expression to get a derivation
            result <- runTenDaemonEval (evaluateExpression spt content) =<< ask <*> get

            case result of
                Left err -> throwError err
                Right (drv, _) -> do
                    -- Now build the derivation
                    logMsg 1 $ "Building derivation: " <> derivName drv
                    buildResult <- buildDerivation drv

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

        SBuilder -> do
            -- Go through the daemon for evaluation and building
            -- First convert to daemon connection
            conn <- getDaemonConnection

            -- Evaluate expression
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
                    buildResult <- requestBuildDerivation spt drv

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

-- | Helper function for handleBuildDerivationFile
handleBuildDerivationFile :: SPrivilegeTier t -> FilePath -> TenM 'Build t ()
handleBuildDerivationFile spt file = do
    -- Read derivation file
    content <- liftIO $ BS.readFile file

    -- Deserialize the derivation
    drv <- case deserializeDerivation content of
        Left err -> throwError err
        Right d -> return d

    -- Build the derivation
    case spt of
        SDaemon -> do
            -- Build directly
            logMsg 1 $ "Building derivation: " <> derivName drv
            buildResult <- buildDerivation drv

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

        SBuilder -> do
            -- Build through daemon connection
            logMsg 1 $ "Building derivation: " <> derivName drv
            buildResult <- requestBuildDerivation spt drv

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

-- | Break a stale lock
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

-- | Helper function for withDatabase that properly uses the context
withDatabase :: FilePath -> Int -> (Database 'Daemon -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withDatabase dbPath timeout action = do
    -- Initialize database connection
    conn <- liftIO $ SQL.open dbPath

    -- Create Database object
    let db = Database conn dbPath True timeout 3 sDaemon

    -- Run the action with proper cleanup
    result <- action db `catchError` \e -> do
        -- Close database on error
        liftIO $ SQL.close conn
        throwError e

    -- Close database connection
    liftIO $ SQL.close conn

    -- Return the result
    return result

-- | Get a daemon connection from the environment
getDaemonConnection :: TenM p 'Builder DaemonConnection
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ DaemonError "Not connected to daemon"

-- | Main entry point
main :: IO ()
main = do
    -- Parse command line arguments
    args <- getArgs
    let result = parseArgs args

    case result of
        Left err -> do
            hPutStrLn stderr err
            exitWith (ExitFailure 1)

        Right (cmd, opts) -> do
            -- Run the command
            runCommand cmd opts
