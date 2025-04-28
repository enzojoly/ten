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

module Ten.Build (
    -- Core build functions
    buildDerivation,
    buildApplicativeStrategy,
    buildMonadicStrategy,

    -- Build result handling
    verifyBuildResult,

    -- Return-continuation handling
    checkForReturnedDerivation,
    handleReturnedDerivation,

    -- Build graph execution
    buildDerivationGraph,
    buildInDependencyOrder,

    -- Parallel building
    buildDependenciesConcurrently,
    waitForDependencies,

    -- Build status reporting
    reportBuildProgress,
    reportBuildStatus,

    -- Build utilities
    runBuilder,
    setupBuilder,
    getBuildEnvironment,

    -- Type classes for privilege-aware operations
    CanBuildDerivation(..),
    CanBuildStrategy(..),
    CanManageBuildStatus(..)
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (try, catch, finally, mask, bracket, throwIO, onException, evaluate, SomeException, ErrorCall(..), handle)
import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify, gets)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, mapMaybe)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.List (nub, isPrefixOf, isInfixOf)
import System.Directory
import System.FilePath
import qualified System.Process as Process
import System.Exit
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
import System.Random (randomRIO)
import Control.Concurrent.Async (async, Async, wait, cancel, waitCatch, race, withAsync)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import System.FilePath.Posix (normalise, takeDirectory)
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import Data.Bits ((.|.), (.&.))
import Data.Singletons
import Data.Singletons.TH

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
import qualified Ten.Daemon.Client as Client

-- Foreign imports for system calls
foreign import ccall unsafe "unistd.h dup2"
    c_dup2 :: CInt -> CInt -> IO CInt

-- | Safe wrapper for dup2
dup2 :: Fd -> Fd -> IO ()
dup2 oldfd newfd = throwErrnoIfMinus1_ "dup2" $ c_dup2 (fromIntegral oldfd) (fromIntegral newfd)

-- | Runtime environment for a builder process
data BuilderEnv = BuilderEnv
    { builderProgram :: FilePath         -- Path to builder executable
    , builderArgs :: [String]            -- Command-line arguments
    , builderDir :: FilePath             -- Working directory
    , builderEnvVars :: Map Text Text    -- Environment variables
    , builderUid :: UserID               -- User ID to run as
    , builderGid :: GroupID              -- Group ID to run as
    , builderUser :: Core.UserEntry      -- User entry to run as
    , builderGroup :: Core.GroupEntry    -- Group entry to run as
    , builderTimeoutSecs :: Int          -- Timeout in seconds
    , builderIsolation :: Bool           -- Whether to use extra isolation
    , builderTempDir :: FilePath         -- Temporary directory for the build
    , builderOutputDir :: FilePath       -- Directory for build outputs
    }

-- | Type classes for privilege-aware operations

-- | Type class for building derivations with appropriate privilege tier
class CanBuildDerivation (t :: PrivilegeTier) where
    -- | Build a derivation, respecting privilege tier constraints
    buildDerivation :: Derivation -> TenM 'Build t BuildResult

-- | Type class for building with specific strategies
class CanBuildStrategy (t :: PrivilegeTier) where
    -- | Build using applicative (parallel) strategy
    buildApplicativeStrategy :: Derivation -> TenM 'Build t BuildResult

    -- | Build using monadic (sequential) strategy
    buildMonadicStrategy :: Derivation -> TenM 'Build t BuildResult

-- | Type class for managing build status
class CanManageBuildStatus (t :: PrivilegeTier) where
    -- | Get status of a build
    getBuildStatus :: BuildId -> TenM 'Build t BuildStatus

    -- | Update build status in store
    updateBuildStatus :: BuildId -> BuildStatus -> TenM 'Build t ()

-- | Daemon instance for building derivations
instance CanBuildDerivation 'Daemon where
    buildDerivation deriv = do
        -- Log start of build
        logMsg 1 $ "Building derivation: " <> derivName deriv

        -- Serialize and store the derivation in the store
        derivPath <- Core.storeDerivationDaemon deriv

        -- Get current build ID
        bid <- gets currentBuildId

        -- Register the derivation in the database
        env <- ask
        -- Create database connection with proper privileges
        db <- liftIO $ DB.initDatabaseDaemon sDaemon (defaultDBPath (storeLocation env)) 5000

        -- Use proper transaction handling and ensure cleanup
        _ <- DB.withTenWriteTransaction db $ \dbConn ->
            liftIO $ DBDeriv.registerDerivationFile dbConn deriv derivPath

        -- Clean up connection
        liftIO $ DB.closeDatabaseDaemon db

        -- First instantiate the derivation
        Derivation.instantiateDerivation deriv

        -- Select build strategy based on derivation
        case derivStrategy deriv of
            ApplicativeStrategy -> do
                logMsg 2 $ "Using applicative (parallel) build strategy for " <> derivName deriv
                buildApplicativeStrategy deriv
            MonadicStrategy -> do
                logMsg 2 $ "Using monadic (sequential) build strategy for " <> derivName deriv
                buildMonadicStrategy deriv

-- | Builder instance for building derivations via protocol
instance CanBuildDerivation 'Builder where
    buildDerivation deriv = do
        -- Log start of build
        logMsg 1 $ "Building derivation via daemon: " <> derivName deriv

        -- Send build request to daemon
        daemonConn <- getDaemonConnection

        -- Create build request with derivation payload
        let serialized = Derivation.serializeDerivation deriv
        let request = Request {
            reqId = 0,  -- Will be set by sendRequest
            reqType = "build-derivation",
            reqParams = Map.fromList [
                ("hasDerivation", "true")
            ],
            reqPayload = Just serialized
        }

        -- Send request and wait for response
        response <- liftIO $ Client.sendRequestSync daemonConn request (3600 * 1000000) -- 1 hour timeout

        case response of
            Left err -> throwError err
            Right (BuildResultResponse result) -> return result
            Right resp -> throwError $ BuildFailed $ "Unexpected response from daemon: " <> T.pack (show resp)

-- | Daemon instance for building with specific strategies
instance CanBuildStrategy 'Daemon where
    buildApplicativeStrategy deriv = do
        env <- ask

        -- Get all dependencies
        let inputs = Set.map inputPath (derivInputs deriv)

        -- Create sandbox config
        let config = Sandbox.defaultSandboxConfig {
            Sandbox.sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"],
            Sandbox.sandboxReturnSupport = False,  -- Not using return continuation for applicative builds
            Sandbox.sandboxPrivileged = False,     -- Always run as unprivileged user
            Sandbox.sandboxUser = "nobody",        -- Default unprivileged user
            Sandbox.sandboxGroup = "nogroup",      -- Default unprivileged group
            Sandbox.sandboxUseMountNamespace = True, -- Use mount namespace for better isolation
            Sandbox.sandboxUseNetworkNamespace = True -- Isolate network by default
        }

        -- Check if we need to build dependencies first
        missingDeps <- filterM (\path -> not <$> Store.checkStorePathExists path) (Set.toList inputs)

        if null missingDeps
            then buildWithSandboxDaemon deriv config
            else do
                -- Build missing dependencies concurrently
                logMsg 2 $ "Building " <> T.pack (show $ length missingDeps) <> " dependencies first"

                -- Find the derivations for each missing dependency
                deps <- findDependencyDerivations missingDeps

                -- Build dependencies concurrently
                results <- buildDependenciesConcurrently (Map.elems deps)

                -- Check results for errors
                let failures = Map.filter isLeft results
                unless (Map.null failures) $ do
                    let (hash, err) = head $ Map.toList failures
                    case fromLeft (BuildFailed "Unknown error") err of
                        err' -> throwError $ BuildFailed $ "Failed to build dependency: " <>
                                            fromMaybe (T.pack hash) (Map.lookup hash (Map.map derivName deps)) <>
                                            " - " <> getErrorMessage err'

                -- Now build with all dependencies available
                buildWithSandboxDaemon deriv config

    buildMonadicStrategy deriv = do
        env <- ask

        -- Get all direct dependencies
        let inputs = Set.map inputPath (derivInputs deriv)

        -- Check if all inputs exist
        missingDeps <- filterM (\path -> not <$> Store.checkStorePathExists path) (Set.toList inputs)
        unless (null missingDeps) $
            throwError $ BuildFailed $ "Missing dependencies: " <>
                         T.intercalate ", " (map storeName missingDeps)

        -- Create sandbox config with return-continuation support
        let config = Sandbox.defaultSandboxConfig {
            Sandbox.sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"],
            Sandbox.sandboxReturnSupport = True,
            Sandbox.sandboxPrivileged = False,     -- Always run as unprivileged user
            Sandbox.sandboxUser = "nobody",        -- Default unprivileged user
            Sandbox.sandboxGroup = "nogroup",      -- Default unprivileged group
            Sandbox.sandboxUseMountNamespace = True, -- Use mount namespace for better isolation
            Sandbox.sandboxUseNetworkNamespace = True -- Isolate network by default
        }

        -- Build in sandbox
        result <- buildWithSandboxDaemon deriv config

        -- Check if this build returned a derivation
        returnDerivExists <- checkIfReturnDerivation result

        if returnDerivExists
            then do
                -- Handle return-continuation case
                innerDrv <- handleReturnedDerivation result
                -- Track this in the derivation chain
                Graph.addToDerivationChain innerDrv

                -- Check for potential recursive build limits
                chain <- gets buildChain
                maxDepth <- asks maxRecursionDepth
                when (length chain > maxDepth) $
                    throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show maxDepth)

                -- Recursively build the inner derivation (must use monadic strategy)
                logMsg 1 $ "Building inner derivation returned by " <> derivName deriv
                buildMonadicStrategy innerDrv
            else
                -- Normal build result
                return result

-- | Builder instance for build strategies (all go through daemon)
instance CanBuildStrategy 'Builder where
    buildApplicativeStrategy = buildDerivation
    buildMonadicStrategy = buildDerivation

-- | Daemon instance for managing build status
instance CanManageBuildStatus 'Daemon where
    getBuildStatus buildId = do
        env <- ask
        -- Get database connection with proper privileges
        db <- liftIO $ DB.initDatabaseDaemon sDaemon (defaultDBPath (storeLocation env)) 5000

        -- Use proper transaction handling
        results <- DB.withTenReadTransaction db $ \dbConn ->
            DB.query dbConn
                "SELECT status FROM BuildStatus WHERE build_id = ? ORDER BY timestamp DESC LIMIT 1"
                (Only (T.pack (show buildId))) :: TenM 'Build 'Daemon [Only Text]

        -- Clean up connection
        liftIO $ DB.closeDatabaseDaemon db

        case results of
            [Only statusText] -> case parseStatus statusText of
                Just status -> return status
                Nothing -> throwError $ BuildFailed $ "Invalid build status: " <> statusText
            _ -> throwError $ BuildFailed $ "Cannot find build: " <> T.pack (show buildId)
      where
        parseStatus :: Text -> Maybe BuildStatus
        parseStatus "BuildPending" = Just BuildPending
        parseStatus t | "BuildRunning " `T.isPrefixOf` t =
            case T.stripPrefix "BuildRunning " t of
                Just progressText ->
                    case readMaybe (T.unpack progressText) of
                        Just progress -> Just (BuildRunning progress)
                        Nothing -> Nothing
                Nothing -> Nothing
        parseStatus t | "BuildRecursing " `T.isPrefixOf` t =
            case T.stripPrefix "BuildRecursing " t of
                Just bidText ->
                    case readMaybe (T.unpack bidText) of
                        Just bid -> Just (BuildRecursing (BuildIdFromInt bid))
                        Nothing -> Nothing
                Nothing -> Nothing
        parseStatus "BuildCompleted" = Just BuildCompleted
        parseStatus "BuildFailed'" = Just BuildFailed'
        parseStatus _ = Nothing

    updateBuildStatus buildId status = do
        -- In a real daemon implementation, this would update a shared TVar
        -- and notify any clients waiting for status updates
        env <- ask
        -- Get database connection with proper privileges
        db <- liftIO $ DB.initDatabaseDaemon sDaemon (defaultDBPath (storeLocation env)) 5000

        -- Use proper transaction handling
        _ <- DB.withTenWriteTransaction db $ \dbConn ->
            liftIO $ DB.execute dbConn
                "INSERT OR REPLACE INTO BuildStatus (build_id, status, timestamp) VALUES (?, ?, strftime('%s','now'))"
                (T.pack (show buildId), T.pack (show status))

        -- Clean up connection
        liftIO $ DB.closeDatabaseDaemon db

-- | Builder instance for managing build status (via protocol)
instance CanManageBuildStatus 'Builder where
    getBuildStatus buildId = do
        daemonConn <- getDaemonConnection

        -- Create build status request
        let request = Request {
            reqId = 0,
            reqType = "build-status",
            reqParams = Map.singleton "buildId" (Protocol.renderBuildId buildId),
            reqPayload = Nothing
        }

        -- Send request and wait for response
        response <- liftIO $ Client.sendRequestSync daemonConn request 10000000
        case response of
            Left err -> throwError err
            Right (BuildStatusResponse update) -> return (buildStatus update)
            Right resp -> throwError $ BuildFailed "Unexpected response from daemon for status request"

    updateBuildStatus buildId status = do
        daemonConn <- getDaemonConnection

        -- Create status update request
        let request = Request {
            reqId = 0,
            reqType = "update-build-status",
            reqParams = Map.fromList [
                ("buildId", Protocol.renderBuildId buildId),
                ("status", T.pack $ show status)
            ],
            reqPayload = Nothing
        }

        -- Send request (fire and forget)
        void $ liftIO $ Client.sendRequest daemonConn request

-- | Find derivation objects for dependencies (daemon operation)
findDependencyDerivations :: [StorePath] -> TenM 'Build 'Daemon (Map String Derivation)
findDependencyDerivations paths = do
    env <- ask

    -- Use the database to look up derivations for the provided output paths
    -- Get database connection with proper privileges
    db <- liftIO $ DB.initDatabaseDaemon sDaemon (defaultDBPath (storeLocation env)) 5000

    -- Use proper transaction handling
    result <- DB.withTenReadTransaction db $ \dbConn ->
        DBDeriv.findDerivationsByOutputs dbConn paths

    -- Clean up connection
    liftIO $ DB.closeDatabaseDaemon db

    -- If any lookup failed, throw an error
    when (Map.size result /= length paths) $ do
        let missingPaths = filter (\p -> not $ any (\(h, d) -> (storeHash p) `T.isInfixOf` (T.pack h)) (Map.toList result)) paths
        throwError $ StoreError $ "Could not find derivations for outputs: " <>
                    T.intercalate ", " (map storeName missingPaths)

    return result

-- Helper function for determining left values in a Map
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

fromLeft :: a -> Either a b -> a
fromLeft def (Left x) = x
fromLeft def _ = def

getErrorMessage :: BuildError -> Text
getErrorMessage (BuildFailed msg) = msg
getErrorMessage (StoreError msg) = msg
getErrorMessage (SandboxError msg) = msg
getErrorMessage _ = "Build error"

-- | Build a derivation in a sandbox (daemon operation)
buildWithSandboxDaemon :: Derivation -> Sandbox.SandboxConfig -> TenM 'Build 'Daemon BuildResult
buildWithSandboxDaemon deriv config = do
    -- Get all inputs
    let inputs = Set.map inputPath $ derivInputs deriv

    -- Run the build in a sandbox with proper privilege handling
    Sandbox.withSandbox inputs config $ \buildDir -> do
        -- Get the builder path in the store
        let builderPath = derivBuilder deriv
        builderContent <- Store.readStoreContent builderPath

        -- Set up builder with proper permissions
        execPath <- setupBuilder builderPath builderContent buildDir

        -- Prepare environment variables
        env <- ask
        buildEnv <- getBuildEnvironment env deriv buildDir

        -- Create output directory
        liftIO $ createDirectoryIfMissing True (buildDir </> "out")

        -- Determine user/group for builder process
        builderUser <- liftIO $ do
            -- If running as root, we can select a user
            uid <- User.getRealUserID
            if uid == 0
                then do
                    -- Try to use the specified user, fallback to nobody
                    catch (User.getUserEntryForName (Sandbox.sandboxUser config)) $ \(_ :: SomeException) ->
                        User.getUserEntryForName "nobody"
                else
                    -- Not running as root, use current user
                    User.getUserEntryForID uid

        builderGroup <- liftIO $ do
            -- If running as root, we can select a group
            uid <- User.getRealUserID
            if uid == 0
                then do
                    -- Try to use the specified group, fallback to nogroup
                    catch (User.getGroupEntryForName (Sandbox.sandboxGroup config)) $ \(_ :: SomeException) ->
                        User.getGroupEntryForName "nogroup"
                else
                    -- Not running as root, use current user's primary group
                    User.getGroupEntryForID (User.userGroupID builderUser)

        -- Set up the build environment structure
        let builderEnv = BuilderEnv {
            builderProgram = execPath,
            builderArgs = map T.unpack $ derivArgs deriv,
            builderDir = buildDir,
            builderEnvVars = buildEnv,
            builderUid = User.userID builderUser,
            builderGid = User.groupID builderGroup,
            builderUser = builderUser,
            builderGroup = builderGroup,
            builderTimeoutSecs = 3600, -- 1 hour default timeout
            builderIsolation = Sandbox.sandboxUseMountNamespace config && Sandbox.sandboxUseNetworkNamespace config,
            builderTempDir = buildDir </> "tmp",
            builderOutputDir = buildDir </> "out"
        }

        -- Log the build command
        logMsg 1 $ "Building: " <> derivName deriv
        logMsg 2 $ "Command: " <> T.pack execPath <> " " <>
                   T.intercalate " " (map T.pack $ map T.unpack $ derivArgs deriv)
        logMsg 3 $ "Running as: " <> T.pack (User.userName builderUser) <> ":" <> T.pack (User.groupName builderGroup)

        -- Run the builder with proper privilege handling
        buildResult <- runBuilder builderEnv

        case buildResult of
            Left err ->
                throwError $ BuildFailed $ "Build execution failed: " <> err
            Right (exitCode, stdout, stderr) -> do
                -- Combine stdout and stderr for the build log
                let buildLog = T.pack $ stdout ++ "\n" ++ stderr

                -- Log the result
                case exitCode of
                    ExitSuccess -> logMsg 1 $ "Build succeeded: " <> derivName deriv
                    ExitFailure code -> do
                        logMsg 1 $ "Build failed (" <> T.pack (show code) <> "): " <> derivName deriv
                        logMsg 2 $ "Build output: " <> T.pack stdout
                        logMsg 2 $ "Build error: " <> T.pack stderr

                -- Check for returned derivation
                returnDrvExists <- liftIO $ doesFileExist (Sandbox.returnDerivationPath buildDir)

                if returnDrvExists && exitCode == ExitSuccess
                    then do
                        -- Return the result for return-continuation handling
                        return $ Core.BuildResult
                            { Core.brOutputPaths = Set.empty
                            , Core.brExitCode = exitCode
                            , Core.brLog = buildLog
                            , Core.brReferences = Set.empty
                            , Core.brMetadata = Map.singleton "returnDerivation" (T.pack $ Sandbox.returnDerivationPath buildDir)
                            }
                    else do
                        -- Collect normal build results
                        outputs <- collectBuildResultDaemon deriv buildDir

                        -- Add build proof
                        addProof BuildProof

                        -- Return the build result
                        return Core.BuildResult
                            { Core.brOutputPaths = outputs
                            , Core.brExitCode = exitCode
                            , Core.brLog = buildLog
                            , Core.brReferences = Set.empty
                            , Core.brMetadata = Map.empty
                            }

-- | Run a builder process with proper privilege handling
runBuilder :: BuilderEnv -> TenM 'Build 'Daemon (Either Text (ExitCode, String, String))
runBuilder env = do
    -- Validate paths to prevent path traversal
    let program = normalise (builderProgram env)
    let buildDir = normalise (builderDir env)

    -- Verify that the program exists and is executable
    progExists <- liftIO $ doesFileExist program
    unless progExists $
        return $ Left $ "Builder program does not exist: " <> T.pack program

    -- Check execute permission
    perms <- liftIO $ getFileStatus program
    let mode = fileMode perms
    let isExecutable = mode .&. 0o100 /= 0  -- Check for owner execute bit
    unless isExecutable $ liftIO $ do
        -- Try to make it executable
        setFileMode program (mode .|. 0o100)

        -- Verify the change
        newPerms <- getFileStatus program
        let newMode = fileMode newPerms
        unless (newMode .&. 0o100 /= 0) $
            return $ Left $ "Builder program could not be made executable: " <> T.pack program

    -- Create temporary directory if it doesn't exist
    liftIO $ createDirectoryIfMissing True (builderTempDir env)

    -- Create output directory if it doesn't exist
    liftIO $ createDirectoryIfMissing True (builderOutputDir env)

    -- Create pipes for stdout and stderr
    (stdoutRead, stdoutWrite) <- liftIO $ createPipe
    (stderrRead, stderrWrite) <- liftIO $ createPipe

    -- Set buffer mode to line buffering for better streaming of output
    stdoutHandle <- liftIO $ fdToHandle stdoutRead
    stderrHandle <- liftIO $ fdToHandle stderrRead
    liftIO $ do
        hSetBuffering stdoutHandle LineBuffering
        hSetBuffering stderrHandle LineBuffering

    -- Convert environment variables
    let envList = map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList (builderEnvVars env)

    -- Determine if we need to drop privileges
    needDropPrivs <- liftIO $ do
        euid <- User.getEffectiveUserID
        return $ euid == 0 && User.userName (builderUser env) /= "root"

    -- Create new process group ID to make it easier to kill the entire process tree
    newPgid <- liftIO $ fmap (Just . fromIntegral) $ getProcessID

    -- Create a reference to track the child PID
    pidRef <- liftIO $ newIORef Nothing

    -- Set up temporary files for cleanup
    let tmpFiles = [(builderTempDir env </> "build.log", Nothing)]

    -- Generate a unique identifier for this build
    buildId <- liftIO $ randomRIO (100000, 999999 :: Int)

    -- Start the builder process with proper privilege handling and timeout
    (mPid, result) <- liftIO $ mask $ \restore -> do
        -- Generate cleanup function
        let cleanup = do
                -- Read the PID
                mpid <- readIORef pidRef

                -- Kill the process if it's still running
                forM_ mpid $ \pid -> do
                    -- Try to kill gracefully first, then force kill
                    signalProcess sigTERM pid `catch` ignoreException
                    threadDelay 500000  -- Give it 0.5 seconds to terminate
                    signalProcess sigKILL pid `catch` ignoreException

                -- Close the pipes
                hClose stdoutHandle
                hClose stderrHandle
                closeFd stdoutWrite
                closeFd stderrWrite

                -- Clean up temporary files
                forM_ tmpFiles $ \(path, mHandle) -> do
                    -- Close handle if it exists
                    forM_ mHandle $ \h -> handle (\(_ :: SomeException) -> return ()) $ hClose h
                    -- Remove file if it exists
                    handle (\(_ :: SomeException) -> return ()) $ removeFile path

        -- Function to run with timeout
        let timeoutAction = do
                -- Fork a child process
                pid <- forkProcess $ do
                    -- In child process

                    -- Set a new process group
                    setpgid 0 0

                    -- Redirect stdout and stderr
                    closeFd stdoutRead
                    closeFd stderrRead

                    -- Connect stdout and stderr
                    dup2 stdoutWrite 1  -- 1 is stdout
                    dup2 stderrWrite 2  -- 2 is stderr

                    closeFd stdoutWrite
                    closeFd stderrWrite

                    -- Change to build directory
                    setCurrentDirectory buildDir

                    -- Prepare a clean environment
                    prepareCleanEnvironment

                    -- Drop privileges if needed
                    when needDropPrivs $ do
                        -- Set group first, then user
                        User.setGroupID (builderGid env)
                        User.setUserID (builderUid env)

                    -- Execute the builder
                    executeFile program True (builderArgs env) (Just envList)

                    -- This should never be reached
                    exitWith (ExitFailure 127)

                -- Store the PID
                writeIORef pidRef (Just pid)

                -- Read stdout and stderr from the child process
                stdoutVar <- newEmptyMVar
                stderrVar <- newEmptyMVar

                -- Set up background threads to read output
                stdoutThread <- forkIO $ do
                    contents <- hGetContents stdoutHandle
                    evaluate (length contents)
                    putMVar stdoutVar contents

                stderrThread <- forkIO $ do
                    contents <- hGetContents stderrHandle
                    evaluate (length contents)
                    putMVar stderrVar contents

                -- Wait for process to complete
                exitStatus <- getProcessStatus True True pid

                -- Get stdout and stderr content
                stdoutContent <- takeMVar stdoutVar
                stderrContent <- takeMVar stderrVar

                -- Process exit status
                case exitStatus of
                    Just (Process.Exited exitCode) -> do
                        let exitResult = case exitCode of
                                0 -> ExitSuccess
                                n -> ExitFailure n
                        return (Just pid, Right (exitResult, stdoutContent, stderrContent))
                    Just (Process.Terminated sig _) ->
                        return (Just pid, Left $ "Builder terminated by signal: " <> T.pack (show sig))
                    Just (Process.Stopped sig) ->
                        return (Just pid, Left $ "Builder stopped by signal: " <> T.pack (show sig))
                    Nothing ->
                        return (Just pid, Left "Builder process disappeared")

        -- Apply timeout if configured
        if builderTimeoutSecs env > 0
            then do
                -- Run builder with timeout
                timeoutResult <- SystemTimeout.timeout (builderTimeoutSecs env * 1000000) timeoutAction
                case timeoutResult of
                    Nothing -> do
                        -- Timeout occurred, cleanup and return error
                        cleanup
                        return (Nothing, Left "Build timed out")
                    Just r -> return r
            else do
                -- Run without timeout
                r <- timeoutAction `onException` cleanup
                return r

    -- Close the pipes (should be done by cleanup, but ensure it's done)
    liftIO $ do
        handle (\(_ :: SomeException) -> return ()) $ hClose stdoutHandle
        handle (\(_ :: SomeException) -> return ()) $ hClose stderrHandle
        handle (\(_ :: SomeException) -> return ()) $ closeFd stdoutWrite
        handle (\(_ :: SomeException) -> return ()) $ closeFd stderrWrite

    -- Return the result
    return result

-- | Set process group ID
setpgid :: ProcessID -> ProcessID -> IO ()
setpgid childPid pgid =
    throwErrnoIfMinus1_ "setpgid" $ c_setpgid (fromIntegral childPid) (fromIntegral pgid)

-- | FFI declaration for setpgid
foreign import ccall unsafe "unistd.h setpgid"
    c_setpgid :: CInt -> CInt -> IO CInt

-- | Prepare a clean environment for the builder
prepareCleanEnvironment :: IO ()
prepareCleanEnvironment = do
    -- Reset signal handlers to default
    resetSignalHandlers
    -- Close all open file descriptors except 0, 1, 2
    closeUnusedFileDescriptors

-- | Reset signal handlers to default
resetSignalHandlers :: IO ()
resetSignalHandlers = do
    -- Reset common signals to default
    installHandler sigTERM Default Nothing
    installHandler sigKILL Default Nothing
    return ()

-- | Close all unused file descriptors (except stdin, stdout, stderr)
closeUnusedFileDescriptors :: IO ()
closeUnusedFileDescriptors = do
    -- We'll try to use /proc/self/fd if available for more accurate FD scanning
    procFdExists <- doesDirectoryExist "/proc/self/fd"

    if procFdExists
        then do
            -- Get list of open file descriptors from /proc
            fds <- try $ listDirectory "/proc/self/fd"
            case fds of
                Right fdList -> do
                    -- Convert to integers and filter out 0, 1, 2
                    let fdInts = filter (> 2) $ catMaybes $ map readMaybe fdList
                    -- Close each FD
                    forM_ fdInts $ \fd ->
                        catch (closeFd (fromIntegral fd)) (\(_ :: SomeException) -> return ())
                Left (_ :: SomeException) ->
                    -- Fallback to closing a reasonable range
                    forM_ [3..1024] $ \fd ->
                        catch (closeFd (fromIntegral fd)) (\(_ :: SomeException) -> return ())
        else
            -- If /proc is not available, close a reasonable range
            forM_ [3..1024] $ \fd ->
                catch (closeFd (fromIntegral fd)) (\(_ :: SomeException) -> return ())

    return ()
  where
    readMaybe :: String -> Maybe Int
    readMaybe s = case reads s of
        [(x, "")] -> Just x
        _ -> Nothing

-- | Set up a builder executable in the sandbox
setupBuilder :: StorePath -> BS.ByteString -> FilePath -> TenM 'Build 'Daemon FilePath
setupBuilder builderPath builderContent buildDir = do
    -- Write the builder to the sandbox
    let execPath = buildDir </> "builder"
    liftIO $ BS.writeFile execPath builderContent

    -- Calculate hash of the builder content for validation
    let contentHash = Crypto.hashWith SHA256 builderContent

    -- Verify the content was written correctly
    writtenContent <- liftIO $ try $ BS.readFile execPath
    case writtenContent of
        Left (e :: SomeException) ->
            throwError $ BuildFailed $ "Failed to read back builder: " <> T.pack (show e)
        Right content -> do
            let writtenHash = Crypto.hashWith SHA256 content
            when (contentHash /= writtenHash) $
                throwError $ BuildFailed "Builder content verification failed"

    -- Make sure the builder is executable and has proper permissions
    -- Set permissions: owner rwx, group r-x, other r-x (0755)
    liftIO $ setFileMode execPath 0o755

    -- Get current UID to check if we're root
    uid <- liftIO $ User.getRealUserID
    when (uid == 0) $ do
        -- If we're root, set ownership to a non-root user for additional security
        (targetUid, targetGid) <- liftIO $ do
            -- Try to get the 'nobody' user, fallback to current user if not found
            userEntry <- catch (User.getUserEntryForName "nobody") $
                \(_ :: SomeException) -> User.getUserEntryForID uid
            groupEntry <- catch (User.getGroupEntryForName "nogroup") $
                \(_ :: SomeException) -> User.getGroupEntryForID (User.userGroupID userEntry)
            return (User.userID userEntry, User.groupID groupEntry)

        -- Set ownership of the builder
        liftIO $ setOwnerAndGroup execPath targetUid targetGid

    -- Return the path to the executable
    return execPath

-- | Get environment variables for the build
getBuildEnvironment :: BuildEnv -> Derivation -> FilePath -> TenM 'Build t (Map Text Text)
getBuildEnvironment env deriv buildDir = do
    -- Get current build state
    state <- get

    -- Construct environment variables map
    return $ Map.unions
        [ -- Base environment from derivation
          derivEnv deriv
        , -- Ten-specific environment variables
          Map.fromList
            [ ("TEN_STORE", T.pack $ storeLocation env)
            , ("TEN_BUILD_DIR", T.pack buildDir)
            , ("TEN_OUT", T.pack $ buildDir </> "out")
            , ("TEN_RETURN_PATH", T.pack $ Sandbox.returnDerivationPath buildDir)
            , ("PATH", "/bin:/usr/bin:/usr/local/bin") -- Explicit PATH
            , ("HOME", T.pack buildDir) -- Set HOME to build directory
            , ("TMPDIR", T.pack $ buildDir </> "tmp") -- Set TMPDIR to sandbox tmp
            , ("TMP", T.pack $ buildDir </> "tmp") -- Alternative tmp env var
            , ("TEMP", T.pack $ buildDir </> "tmp") -- Another alternative
            , ("TEN_BUILD_ID", T.pack . show $ currentBuildId state) -- Current build ID
            , ("TEN_DERIVATION_NAME", derivName deriv) -- Name of the derivation
            , ("TEN_SYSTEM", derivSystem deriv) -- Target system
            ]
        , -- Security-related environment variables
          Map.fromList
            [ ("TEN_SANDBOX", "1") -- Indicate running in sandbox
            , ("TEN_RESTRICTED", "1") -- Indicate restricted environment
            , ("TEN_UNPRIVILEGED", "1") -- Indicate unprivileged execution
            ]
        ]

-- | Collect output files from a build and add them to the store (daemon operation)
collectBuildResultDaemon :: Derivation -> FilePath -> TenM 'Build 'Daemon (Set StorePath)
collectBuildResultDaemon drv buildDir = do
    -- Get the output directory
    let outDir = buildDir </> "out"

    -- Verify the output directory exists
    outDirExists <- liftIO $ doesDirectoryExist outDir
    unless outDirExists $ throwError $
        BuildFailed $ "Output directory not created: " <> T.pack outDir

    -- List directory contents for debugging
    outFiles <- liftIO $ listDirectory outDir
    logMsg 2 $ "Output directory contents: " <> T.pack (show outFiles)

    -- Get environment information
    env <- ask

    -- Get database connection
    db <- liftIO $ initDatabaseDaemon sDaemon (defaultDBPath (storeLocation env)) 5000

    -- Process in a transaction for atomicity
    outputPaths <- withTenWriteTransaction db $ \dbConn -> do
        -- Process each expected output
        outputPaths <- foldM (processOutput dbConn outDir) Set.empty (Set.toList $ derivOutputs drv)

        -- Register all input-output references
        -- Get derivation path in the store
        let derivStorePath = StorePath (derivHash drv) (derivName drv <> ".drv")

        -- Register each output in the database
        derivId <- DBDeriv.storeDerivation dbConn deriv derivStorePath

        forM_ (Set.toList outputPaths) $ \outputPath -> do
            -- 1. Register the derivation as a reference for this output
            DBDeriv.addDerivationReference dbConn outputPath derivStorePath

            -- 2. Register all input references for this output
            forM_ (Set.toList $ derivInputs drv) $ \input -> do
                DBDeriv.addDerivationReference dbConn outputPath (inputPath input)

            -- 3. Scan the output file for additional references
            refs <- Store.scanFileForStoreReferences $ storePathToFilePath outputPath env
            DBDeriv.bulkRegisterReferences dbConn $ map (\ref -> (outputPath, ref)) $ Set.toList refs

        -- Return the set of outputs
        return outputPaths

    -- Clean up database connection
    liftIO $ closeDatabaseDaemon db

    -- Add output proof
    addProof OutputProof

    -- Return the set of outputs
    return outputPaths
  where
    -- Process a single output, adding it to the store
    processOutput :: Database 'Daemon -> FilePath -> Set StorePath -> DerivationOutput -> TenM 'Build 'Daemon (Set StorePath)
    processOutput db outDir accPaths output = do
        env <- ask
        let outputFile = outDir </> T.unpack (outputName output)

        -- Normalize and validate path
        let outputFile' = normalise outputFile

        -- Security check - make sure output is within the build directory
        let buildPath = normalise buildDir
        unless (buildPath `isPrefixOf` outputFile') $
            throwError $ BuildFailed $ "Output path escapes build directory: " <> T.pack outputFile'

        exists <- liftIO $ doesPathExist outputFile'

        logMsg 2 $ "Checking for output: " <> outputName output <>
                   " at " <> T.pack outputFile' <>
                   " (exists: " <> T.pack (show exists) <> ")"

        if exists
            then do
                -- Check if it's a file or directory
                isDir <- liftIO $ doesDirectoryExist outputFile'
                outputPath <- if isDir
                    then do
                        -- For directories, create a tarball
                        let tarballPath = outputFile' <> ".tar.gz"
                        liftIO $ createTarball outputFile' tarballPath

                        -- Read the tarball and add it to the store
                        content <- liftIO $ BS.readFile tarballPath
                        Store.addToStore sDaemon (outputName output <> ".tar.gz") content
                    else do
                        -- For regular files, read directly
                        content <- liftIO $ BS.readFile outputFile'
                        Store.addToStore sDaemon (outputName output) content

                -- Register this as a valid path in the database
                let derivPath = StorePath (derivHash drv) (derivName drv <> ".drv")
                DBDeriv.registerValidPath db outputPath (Just derivPath)

                -- Return updated set
                return $ Set.insert outputPath accPaths
            else if Derivation.isReturnContinuationDerivation (derivName drv) (derivArgs drv) (derivEnv drv)
                then do
                    -- For return-continuation builds, outputs might not be created
                    -- Return the predicted output path anyway
                    let outputPath' = outputPath output
                    return $ Set.insert outputPath' accPaths
                else
                    throwError $ BuildFailed $
                        "Expected output not produced: " <> outputName output

-- | Create a tarball from a directory
createTarball :: FilePath -> FilePath -> IO ()
createTarball sourceDir targetFile = do
    -- Use tar to create gzipped tarball (requires tar command to be available)
    let cmd = "tar -czf " ++ targetFile ++ " -C " ++ takeDirectory sourceDir ++ " " ++ takeFileName sourceDir

    (exitCode, stdout, stderr) <- Process.readCreateProcessWithExitCode (Process.shell cmd) ""

    case exitCode of
        ExitSuccess -> return ()
        _ -> throwIO $ userError $ "Failed to create tarball: " ++ stderr

-- | Verify that a build result matches the expected outputs
verifyBuildResult :: Derivation -> Core.BuildResult -> TenM 'Build t Bool
verifyBuildResult deriv result = do
    -- First check exit code
    if Core.brExitCode result /= ExitSuccess
        then return False
        else do
            -- Check if this is a return-continuation build
            if Derivation.isReturnContinuationDerivation (derivName deriv) (derivArgs deriv) (derivEnv deriv) &&
               Map.member "returnDerivation" (Core.brMetadata result)
                then do
                    -- For return-continuation, just check that the returned derivation exists
                    let returnPath = T.unpack $ Core.brMetadata result Map.! "returnDerivation"
                    returnExists <- liftIO $ doesFileExist returnPath

                    if returnExists
                        then do
                            -- Verify the returned derivation is properly formed
                            content <- liftIO $ BS.readFile returnPath
                            case Core.deserializeDerivation content of
                                Left _ -> return False
                                Right _ -> return True
                        else
                            return False
                else do
                    -- For normal builds, check outputs
                    let expectedOutputNames = Set.map outputName (derivOutputs deriv)
                    let actualOutputNames = Set.map storeName (Core.brOutputPaths result)

                    -- Check if all expected outputs are included in actual outputs
                    let allOutputsPresent = expectedOutputNames `Set.isSubsetOf` actualOutputNames

                    -- Check that each output exists in the store
                    validOutputs <- forM (Set.toList (Core.brOutputPaths result)) Store.checkStorePathExists

                    -- Return True if all checks pass
                    return $ allOutputsPresent && and validOutputs

-- | Check if a build result includes a returned derivation
checkIfReturnDerivation :: Core.BuildResult -> TenM 'Build t Bool
checkIfReturnDerivation result =
    return $ Core.brExitCode result == ExitSuccess &&
             Map.member "returnDerivation" (Core.brMetadata result)

-- | Check for a returned derivation in a build directory
checkForReturnedDerivation :: FilePath -> TenM 'Build t (Maybe Derivation)
checkForReturnedDerivation buildDir = do
    let returnPath = Sandbox.returnDerivationPath buildDir

    -- Normalize and validate path
    let returnPath' = normalise returnPath

    -- Security check - ensure returnPath is within buildDir
    let buildDir' = normalise buildDir
    unless (buildDir' `isPrefixOf` returnPath') $
        throwError $ BuildFailed $ "Return derivation path escapes build directory: " <> T.pack returnPath'

    returnDrvExists <- liftIO $ doesFileExist returnPath'

    if returnDrvExists
        then do
            -- Read and deserialize the returned derivation
            content <- liftIO $ BS.readFile returnPath'
            case Core.deserializeDerivation content of
                Left err -> do
                    logMsg 1 $ "Error deserializing returned derivation: " <> Core.buildErrorToText err
                    return Nothing
                Right drv -> return $ Just drv
        else
            return Nothing

-- | Handle a returned derivation
handleReturnedDerivation :: Core.BuildResult -> TenM 'Build t Derivation
handleReturnedDerivation result = do
    -- Get the returned derivation path
    let returnPath = T.unpack $ Core.brMetadata result Map.! "returnDerivation"

    -- Normalize and validate path
    let returnPath' = normalise returnPath

    -- Read and deserialize
    content <- liftIO $ BS.readFile returnPath'
    case Core.deserializeDerivation content of
        Left err -> throwError $ SerializationError $
            "Failed to deserialize returned derivation: " <> Core.buildErrorToText err
        Right innerDrv -> do
            -- Add proof that we successfully got a returned derivation
            addProof RecursionProof

            -- Get current environment (Daemon or Builder)
            env <- ask
            let tier = currentPrivilegeTier env

            -- Store the inner derivation in the content store - in appropriate context
            derivPath <- case tier of
                Daemon -> Core.storeDerivationDaemon innerDrv
                Builder -> do
                    -- For Builder tier, we need to get a database connection first
                    db <- liftIO $ initDatabaseDaemon sDaemon (defaultDBPath (storeLocation env)) 5000
                    path <- DBDeriv.storeDerivation db innerDrv
                    liftIO $ closeDatabaseDaemon db
                    return path

            -- Check for cycles - extract boolean from monadic context
            isCyclicResult <- Graph.detectRecursionCycle innerDrv
            when isCyclicResult $
                throwError $ CyclicDependency $
                    "Cyclic dependency detected in returned derivation: " <> derivName innerDrv

            return innerDrv

-- | Build a graph of derivations
buildDerivationGraph :: BuildGraph -> TenM 'Build t (Map Text Core.BuildResult)
buildDerivationGraph graph = do
    -- First get dependencies in topological order
    env <- ask
    let tierSingleton = case currentPrivilegeTier env of
                          Daemon -> sDaemon
                          Builder -> sBuilder

    -- Get evaluation phase result from topological sort
    evalResult <- liftIO $ runTenDaemonEval (Graph.topologicalSort tierSingleton graph) env (initBuildState Eval (BuildIdFromInt 0))

    case evalResult of
        Left err -> throwError err
        Right (sorted, _) -> do
            -- Filter for DerivationNode nodes
            let derivations = mapMaybe getDerivation sorted

            -- Build each derivation
            foldM buildAndCollect Map.empty derivations
  where
    getDerivation (DerivationNode drv) = Just drv
    getDerivation _ = Nothing

    buildAndCollect results drv = do
        -- Build the derivation
        result <- buildDerivation drv
        -- Add to results
        let drvId = derivHash drv
        return $ Map.insert drvId result results

-- | Build derivations in dependency order
buildInDependencyOrder :: [Derivation] -> TenM 'Build t [Core.BuildResult]
buildInDependencyOrder derivations = do
    -- First check for cycles using a pure function
    let cycle = any isDerivationCyclic derivations
    when cycle $
        throwError $ CyclicDependency "Cycle detected in build dependencies"

    -- Build each derivation in order
    mapM buildDerivation derivations
  where
    -- Pure cycle detection function
    isDerivationCyclic :: Derivation -> Bool
    isDerivationCyclic d =
        -- Simple pure implementation for structural cycle detection
        let outputs = Set.map outputPath (derivOutputs d)
            inputs = Set.map inputPath (derivInputs d)
        in not $ Set.null $ Set.intersection outputs inputs

-- | Build dependencies concurrently
buildDependenciesConcurrently :: [Derivation] -> TenM 'Build 'Daemon (Map String (Either BuildError Core.BuildResult))
buildDependenciesConcurrently derivations = do
    env <- ask
    state <- get

    -- Create a map to hold results
    resultMap <- liftIO $ newTVarIO Map.empty

    -- Create a semaphore to limit concurrency
    maxConcurrent <- asks (\e -> fromMaybe 4 (maxConcurrentBuilds e))
    sem <- liftIO $ Control.Concurrent.newQSem maxConcurrent

    -- Track all build threads
    threads <- liftIO $ newTVarIO []

    -- Start a thread for each derivation
    liftIO $ forM_ derivations $ \drv -> do
        let hash = T.unpack $ derivHash drv
        thread <- mask $ \restore -> forkIO $ do
            -- Acquire semaphore
            bracket (Control.Concurrent.waitQSem sem) (\_ -> Control.Concurrent.signalQSem sem) $ \_ -> restore $ do
                -- Run the build with daemon privileges
                result <- runTenDaemon (buildDerivation drv) env state
                -- Store the result
                atomically $ modifyTVar resultMap $ \m ->
                    Map.insert hash (either Left (Right . fst) result) m

        -- Store thread reference
        atomically $ modifyTVar threads (thread:)

    -- Wait for all threads to complete
    liftIO $ do
        allThreads <- readTVarIO threads
        -- First try graceful termination
        forM_ allThreads $ \t -> catch (killThread t) $ \(_ :: SomeException) -> return ()

        -- Wait a short time for threads to clean up
        threadDelay 1000000  -- 1 second

        -- Return the results
        readTVarIO resultMap

-- | Wait for dependencies to complete
waitForDependencies :: Set BuildId -> TenM 'Build t ()
waitForDependencies depIds = do
    -- Get max wait time
    maxWaitTime <- asks (\e -> 60 * 60)  -- Default to 1 hour

    -- Check dependencies
    complete <- checkDependencies depIds

    unless complete $ do
        -- Wait for dependencies with timeout
        result <- liftIO $ Core.timeout (maxWaitTime * 1000000) $ waitForDeps depIds

        when (isNothing result) $
            throwError $ BuildFailed "Timeout waiting for dependencies to complete"
  where
    checkDependencies :: Set BuildId -> TenM 'Build t Bool
    checkDependencies deps = do
        env <- ask
        let tier = currentPrivilegeTier env

        statuses <- forM (Set.toList deps) $ \bid -> do
            -- Use the appropriate instance based on privilege tier
            status <- getBuildStatus bid
            return $ case status of
                BuildCompleted -> True
                _ -> False
        return $ and statuses

    waitForDeps :: Set BuildId -> IO ()
    waitForDeps deps = do
        -- Wait a bit between checks
        threadDelay (10 * 1000000)  -- 10 seconds

-- | Report build progress
reportBuildProgress :: BuildId -> Float -> TenM 'Build t ()
reportBuildProgress buildId progress = do
    -- Log progress
    logMsg 2 $ "Build progress for " <> T.pack (show buildId) <> ": " <>
               T.pack (show (progress * 100)) <> "%"

    -- Update build status based on context
    env <- ask
    updateBuildStatus buildId (BuildRunning progress)

-- | Report build status
reportBuildStatus :: BuildId -> BuildStatus -> TenM 'Build t ()
reportBuildStatus buildId status = do
    -- Log status change
    logMsg 2 $ "Build status for " <> T.pack (show buildId) <> ": " <>
               T.pack (show status)

    -- Update status based on context
    env <- ask
    updateBuildStatus buildId status

-- | Get daemon connection (should be available in builder context)
getDaemonConnection :: TenM p 'Builder (DaemonConnection 'Builder)
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ DaemonError "No daemon connection available"

-- Helper function for ignoring exceptions during cleanup
ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

-- Helper for reading safely
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing
