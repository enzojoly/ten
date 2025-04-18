{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ten (
    -- Re-exports from Core
    TenM,
    Phase(..),
    BuildEnv(..),
    BuildState(..),
    BuildError(..),
    BuildStatus(..),
    BuildStrategy(..),
    RunMode(..),
    StorePath(..),
    Proof(..),
    PhaseTransition(..),

    -- Monad operations
    runTen,
    evalTen,
    buildTen,
    transitionPhase,
    logMsg,
    assertTen,
    atomicallyTen,

    -- Environment initialization
    initBuildEnv,
    initClientEnv,
    initDaemonEnv,
    initBuildState,

    -- Re-exports from Store
    addToStore,
    ensureInStore,
    readFromStore,
    verifyStorePath,
    storePathExists,
    storePathToFilePath,

    -- Re-exports from Hash
    hashByteString,
    hashFile,
    hashText,
    showHash,

    -- Re-exports from Derivation
    Derivation(..),
    DerivationInput(..),
    DerivationOutput(..),
    mkDerivation,
    instantiateDerivation,
    serializeDerivation,
    deserializeDerivation,
    hashDerivation,
    joinDerivation,
    derivationEquals,

    -- Re-exports from Build
    BuildResult(..),
    buildDerivation,
    buildDerivationFile,
    buildToGetInnerDerivation,
    collectBuildResult,
    verifyBuildResult,

    -- Re-exports from Sandbox
    SandboxConfig(..),
    defaultSandboxConfig,
    withSandbox,
    returnDerivationPath,

    -- Re-exports from Graph
    BuildGraph(..),
    BuildNode(..),
    createBuildGraph,
    validateGraph,
    detectCycles,
    topologicalSort,

    -- Re-exports from GC
    GCRoot(..),
    GCStats(..),
    collectGarbage,
    daemonCollectGarbage,
    addRoot,
    removeRoot,
    listRoots,

    -- Re-exports from CLI
    Command(..),
    Options(..),
    defaultOptions,
    parseArgs,
    runCommand,

    -- Re-exports from Daemon modules
    DaemonConfig(..),
    defaultDaemonConfig,
    startDaemon,
    stopDaemon,
    isDaemonRunning,

    -- High-level operations
    build,
    eval,
    clean,
    daemon
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import System.Exit
import System.Environment (getArgs)

-- Core imports
import Ten.Core

-- Basic build system modules
import Ten.Hash
import Ten.Store
import Ten.Derivation
import Ten.Build
import Ten.Sandbox
import Ten.Graph
import Ten.GC

-- CLI and user interface
import Ten.CLI

-- Optional daemon modules
import qualified Ten.Daemon.Config as DaemonConfig
import qualified Ten.Daemon.Client as DaemonClient
import qualified Ten.Daemon.Core as DaemonCore

-- | Build a Ten expression or derivation file
build :: FilePath -> IO (Either BuildError BuildResult)
build file = do
    -- Set up environment
    storePath <- getDefaultStorePath
    workDir <- getDefaultWorkDir

    createDirectoryIfMissing True storePath
    createDirectoryIfMissing True workDir

    let env = initBuildEnv workDir storePath

    -- Check file existence
    exists <- doesFileExist file
    if not exists
        then return $ Left $ InputNotFound file
        else do
            -- Determine file type and build it
            case takeExtension file of
                ".ten" -> buildTenFile env file
                ".drv" -> buildDrvFile env file
                _ -> do
                    -- Attempt to autodetect file type
                    content <- readFile file
                    if "{" `isPrefixOf` (dropWhile isSpace content)
                        then buildTenFile env file
                        else buildDrvFile env file

-- | Build a Ten expression file
buildTenFile :: BuildEnv -> FilePath -> IO (Either BuildError BuildResult)
buildTenFile env file = do
    -- Read and parse Ten expression
    content <- TIO.readFile file

    -- Evaluate to get derivation
    evalResult <- evalTen (evalExpression content) env
    case evalResult of
        Left err -> return $ Left err
        Right (derivation, _) ->
            -- Build the derivation
            buildDerivation' env derivation

-- | Build a derivation file
buildDrvFile :: BuildEnv -> FilePath -> IO (Either BuildError BuildResult)
buildDrvFile env file = do
    -- Read derivation file
    content <- BS.readFile file

    -- Parse derivation
    case deserializeDerivation content of
        Left err -> return $ Left $ SerializationError err
        Right derivation ->
            -- Build the derivation
            buildDerivation' env derivation

-- | Build a derivation
buildDerivation' :: BuildEnv -> Derivation -> IO (Either BuildError BuildResult)
buildDerivation' env derivation = do
    -- Run the build
    buildResult <- buildTen (buildDerivation derivation) env

    -- Return just the build result, discarding state
    return $ case buildResult of
        Left err -> Left err
        Right (result, _) -> Right result

-- | Evaluate a Ten expression file
eval :: FilePath -> IO (Either BuildError Derivation)
eval file = do
    -- Set up environment
    storePath <- getDefaultStorePath
    workDir <- getDefaultWorkDir

    let env = initBuildEnv workDir storePath

    -- Check file existence
    exists <- doesFileExist file
    if not exists
        then return $ Left $ InputNotFound file
        else do
            -- Read and evaluate file
            content <- TIO.readFile file
            evalResult <- evalTen (evalExpression content) env

            -- Return just the derivation, discarding state
            return $ case evalResult of
                Left err -> Left err
                Right (derivation, _) -> Right derivation

-- | Evaluate a Ten expression
evalExpression :: T.Text -> TenM 'Eval Derivation
evalExpression expr = do
    -- Log start of evaluation
    logMsg 1 $ "Evaluating expression: " <> expr

    -- In a real implementation, this would parse and evaluate the Ten expression
    -- For now we throw an error as this requires the Ten language parser
    throwError $ EvalError "Expression evaluation not implemented yet"

-- | Clean temporary files and caches
clean :: IO (Either BuildError ())
clean = do
    -- Set up environment
    storePath <- getDefaultStorePath
    workDir <- getDefaultWorkDir

    let env = initBuildEnv workDir storePath

    -- Run garbage collection
    gcResult <- runTen (withGCLock collectGarbage) env (initBuildState Build)

    -- Remove work directory contents
    cleanWorkDir workDir

    -- Return GC result
    return $ case gcResult of
        Left err -> Left err
        Right _ -> Right ()

-- | Clean work directory
cleanWorkDir :: FilePath -> IO ()
cleanWorkDir workDir = do
    -- Remove temporary files, preserving the directory itself
    exists <- doesDirectoryExist workDir
    when exists $ do
        -- List directory contents
        entries <- listDirectory workDir

        -- Remove each entry
        forM_ entries $ \entry -> do
            let path = workDir </> entry
            removePathForcibly path

-- | Run a daemon operation
daemon :: [String] -> IO (Either BuildError ())
daemon [] = return $ Left $ DaemonError "Missing daemon command"
daemon (cmd:args) = do
    -- Determine and run the appropriate daemon command
    case cmd of
        "start" -> startDaemon' args
        "stop" -> stopDaemon'
        "status" -> daemonStatus
        "restart" -> daemonRestart args
        _ -> return $ Left $ DaemonError $ "Unknown daemon command: " <> T.pack cmd

-- | Start the daemon
startDaemon' :: [String] -> IO (Either BuildError ())
startDaemon' args = do
    -- Check if daemon is already running
    running <- isDaemonRunning'
    if running
        then return $ Left $ DaemonError "Daemon is already running"
        else do
            -- Set up daemon configuration
            config <- case args of
                [] -> DaemonConfig.getDefaultConfig
                _ -> DaemonConfig.parseConfigArgs args

            -- Start the daemon
            DaemonCore.startDaemon config
            return $ Right ()

-- | Stop the daemon
stopDaemon' :: IO (Either BuildError ())
stopDaemon' = do
    -- Check if daemon is running
    running <- isDaemonRunning'
    if not running
        then return $ Left $ DaemonError "Daemon is not running"
        else do
            -- Stop the daemon
            DaemonCore.stopDaemon
            return $ Right ()

-- | Check daemon status
daemonStatus :: IO (Either BuildError ())
daemonStatus = do
    -- Check if daemon is running
    running <- isDaemonRunning'

    -- Get daemon status if running
    if running
        then do
            -- Get socket path
            socketPath <- DaemonClient.getDefaultSocketPath

            -- Connect and get status
            DaemonClient.getDaemonStatus' socketPath
        else
            return $ Left $ DaemonError "Daemon is not running"

-- | Restart the daemon
daemonRestart :: [String] -> IO (Either BuildError ())
daemonRestart args = do
    -- Stop the daemon if running
    _ <- stopDaemon'

    -- Start the daemon with new arguments
    startDaemon' args

-- | Check if daemon is running
isDaemonRunning' :: IO Bool
isDaemonRunning' = do
    -- Get default socket path
    socketPath <- DaemonClient.getDefaultSocketPath

    -- Check if daemon is running at that path
    DaemonClient.isDaemonRunning socketPath

-- | Get default store path
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

-- | Get default work directory
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
