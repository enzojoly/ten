{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Ten.Build (
    -- Core build functions
    buildDerivation,
    buildApplicativeStrategy,
    buildMonadicStrategy,

    -- Build result handling
    BuildResult(..),
    collectBuildResult,
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
    getBuildEnvironment
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (try, catch, finally, mask, bracket, throwIO, onException, evaluate, SomeException, ErrorCall(..))
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
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes)
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
import System.Posix.Process (ProcessStatus(..), getProcessStatus, forkProcess, executeFile, getProcessID)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM, installHandler, Handler(..))
import System.Posix.Types (ProcessID, FileMode, UserID, GroupID)
import System.Posix.IO (openFd, createFile, closeFd, setLock, getLock,
                       defaultFileFlags, OpenMode(..), OpenFileFlags(..),
                       exclusive, fdToHandle, dup2)
import Foreign.C.Error (Errno(..), getErrno)
import Foreign.C (CInt(..))
import GHC.IO.Handle.FD (handleToFd)
import System.Timeout (timeout)
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
import Ten.Store
import Ten.Derivation
import Ten.Sandbox
import Ten.Graph
import Ten.DB.Core
import Ten.DB.Derivations
import Ten.DB.References
import Ten.Daemon.Protocol
import Ten.Daemon.Client

-- | Result of a build operation
data BuildResult = BuildResult
    { resultOutputs :: Set StorePath    -- Paths to the build outputs
    , resultExitCode :: ExitCode        -- Exit code from the builder
    , resultLog :: Text                 -- Build log output
    , resultMetadata :: Map Text Text   -- Additional metadata
    } deriving (Show, Eq)

-- | Runtime environment for a builder process
data BuilderEnv = BuilderEnv
    { builderProgram :: FilePath       -- Path to builder executable
    , builderArgs :: [String]          -- Command-line arguments
    , builderDir :: FilePath           -- Working directory
    , builderEnvVars :: Map Text Text  -- Environment variables
    , builderUser :: User.UserEntry    -- User to run as
    , builderGroup :: User.GroupEntry  -- Group to run as
    , builderTimeoutSecs :: Int        -- Timeout in seconds (0 = no timeout)
    , builderIsolation :: Bool         -- Whether to use extra isolation
    , builderTempDir :: FilePath       -- Temporary directory for the build
    , builderOutputDir :: FilePath     -- Directory for build outputs
    }

-- | Build a derivation, dispatching based on privilege tier
buildDerivation :: Derivation -> TenM 'Build t BuildResult
buildDerivation deriv = do
    -- Determine if we're in daemon or builder tier
    env <- ask
    case currentPrivilegeTier env of
        Daemon -> withSPrivilegeTier sDaemon $ \st -> buildDerivationDaemon st deriv
        Builder -> withSPrivilegeTier sBuilder $ \st -> buildDerivationBuilder st deriv

-- | Build a derivation in daemon context
buildDerivationDaemon :: SPrivilegeTier 'Daemon -> Derivation -> TenM 'Build 'Daemon BuildResult
buildDerivationDaemon _ deriv = do
    -- Log start of build
    logMsg 1 $ "Building derivation: " <> derivName deriv

    -- Serialize and store the derivation in the store (daemon operation)
    derivPath <- storeDaemonDerivation deriv

    -- Get current build ID
    bid <- gets currentBuildId

    -- Register the derivation in the database (daemon operation)
    env <- ask
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        liftIO $ registerDerivationFile db deriv derivPath

    -- First instantiate the derivation
    instantiateDerivation deriv

    -- Select build strategy based on derivation
    case derivStrategy deriv of
        ApplicativeStrategy -> do
            logMsg 2 $ "Using applicative (parallel) build strategy for " <> derivName deriv
            buildApplicativeStrategyDaemon deriv
        MonadicStrategy -> do
            logMsg 2 $ "Using monadic (sequential) build strategy for " <> derivName deriv
            buildMonadicStrategyDaemon deriv

-- | Build a derivation in builder context
buildDerivationBuilder :: SPrivilegeTier 'Builder -> Derivation -> TenM 'Build 'Builder BuildResult
buildDerivationBuilder _ deriv = do
    -- Log start of build
    logMsg 1 $ "Building derivation via daemon: " <> derivName deriv

    -- Send build request to daemon
    daemonConn <- getDaemonConnection
    buildRequest <- createBuildDerivationRequest deriv

    -- Wait for response from daemon
    response <- sendToDaemon daemonConn buildRequest

    case response of
        BuildResponse result -> return result
        ErrorResponse err -> throwError err
        _ -> throwError $ BuildFailed "Unexpected response from daemon"

-- | Store a derivation in daemon context
storeDaemonDerivation :: Derivation -> TenM 'Build 'Daemon StorePath
storeDaemonDerivation deriv = do
    -- Create serialized representation
    let derivContent = serializeDerivation deriv
    let derivName' = derivName deriv <> ".drv"

    -- Store in content-addressable store (daemon operation)
    storePath <- addToStore derivName' derivContent

    -- Return the store path
    return storePath

-- | Request daemon to store a derivation in builder context
storeBuilderDerivation :: Derivation -> TenM 'Build 'Builder StorePath
storeBuilderDerivation deriv = do
    -- Get daemon connection
    daemonConn <- getDaemonConnection

    -- Create store request
    let derivContent = serializeDerivation deriv
    let derivName' = derivName deriv <> ".drv"
    storeRequest <- createStoreRequest derivName' derivContent

    -- Send request to daemon
    response <- sendToDaemon daemonConn storeRequest

    case response of
        StoreAddResponse path -> return path
        ErrorResponse err -> throwError err
        _ -> throwError $ BuildFailed "Unexpected response from daemon for store request"

-- | Instantiate a derivation (implement input handling and preparation)
instantiateDerivation :: Derivation -> TenM 'Build t ()
instantiateDerivation deriv = do
    -- Log instantiation start
    logMsg 2 $ "Instantiating derivation: " <> derivName deriv

    -- Add all inputs to the build inputs set
    let inputs = Set.map inputPath (derivInputs deriv)
    modify $ \s -> s { buildInputs = Set.union inputs (buildInputs s) }

    -- Add proof for the build
    addProof InputProof

-- | Build a derivation using applicative strategy in daemon context
buildApplicativeStrategyDaemon :: Derivation -> TenM 'Build 'Daemon BuildResult
buildApplicativeStrategyDaemon deriv = do
    env <- ask

    -- Get all dependencies
    let inputs = Set.map inputPath (derivInputs deriv)

    -- Create sandbox config
    let config = defaultSandboxConfig {
        sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"],
        sandboxReturnSupport = False,  -- Not using return continuation for applicative builds
        sandboxPrivileged = False,     -- Always run as unprivileged user
        sandboxUser = "nobody",        -- Default unprivileged user
        sandboxGroup = "nogroup",      -- Default unprivileged group
        sandboxUseMountNamespace = True, -- Use mount namespace for better isolation
        sandboxUseNetworkNamespace = True -- Isolate network by default
    }

    -- Check if we need to build dependencies first
    missingDeps <- filterM (\path -> not <$> daemonStorePathExists path) (Set.toList inputs)

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
  where
    isLeft (Left _) = True
    isLeft _ = False

    fromLeft def (Left x) = x
    fromLeft def _ = def

    getErrorMessage (BuildFailed msg) = msg
    getErrorMessage (StoreError msg) = msg
    getErrorMessage (SandboxError msg) = msg
    getErrorMessage _ = "Build error"

-- | Build using applicative strategy in builder context
buildApplicativeStrategy :: Derivation -> TenM 'Build 'Builder BuildResult
buildApplicativeStrategy = buildDerivationBuilder sBuilder

-- | Find derivation objects for dependencies (daemon operation)
findDependencyDerivations :: [StorePath] -> TenM 'Build 'Daemon (Map String Derivation)
findDependencyDerivations paths = do
    env <- ask

    -- Use the database to look up derivations for the provided output paths
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Use the database to look up derivations for the provided output paths
        result <- liftIO $ findDerivationsByOutputs db paths

        -- If any lookup failed, throw an error
        when (Map.size result /= length paths) $ do
            let missingPaths = filter (\p -> not $ any (\(h, d) -> (storeHash p) `T.isInfixOf` (T.pack h)) (Map.toList result)) paths
            throwError $ StoreError $ "Could not find derivations for outputs: " <>
                        T.intercalate ", " (map storeName missingPaths)

        return result

-- | Build a derivation using monadic strategy in daemon context
buildMonadicStrategyDaemon :: Derivation -> TenM 'Build 'Daemon BuildResult
buildMonadicStrategyDaemon deriv = do
    env <- ask

    -- Get all direct dependencies
    let inputs = Set.map inputPath (derivInputs deriv)

    -- Check if all inputs exist
    missingDeps <- filterM (\path -> not <$> daemonStorePathExists path) (Set.toList inputs)
    unless (null missingDeps) $
        throwError $ BuildFailed $ "Missing dependencies: " <>
                     T.intercalate ", " (map storeName missingDeps)

    -- Create sandbox config with return-continuation support
    let config = defaultSandboxConfig {
        sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"],
        sandboxReturnSupport = True,
        sandboxPrivileged = False,     -- Always run as unprivileged user
        sandboxUser = "nobody",        -- Default unprivileged user
        sandboxGroup = "nogroup",      -- Default unprivileged group
        sandboxUseMountNamespace = True, -- Use mount namespace for better isolation
        sandboxUseNetworkNamespace = True -- Isolate network by default
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
            addToDerivationChain innerDrv

            -- Check for potential recursive build limits
            chain <- gets buildChain
            maxDepth <- asks maxRecursionDepth
            when (length chain > maxDepth) $
                throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show maxDepth)

            -- Recursively build the inner derivation (must use monadic strategy)
            logMsg 1 $ "Building inner derivation returned by " <> derivName deriv
            buildMonadicStrategyDaemon innerDrv
        else
            -- Normal build result
            return result

-- | Build using monadic strategy in builder context
buildMonadicStrategy :: Derivation -> TenM 'Build 'Builder BuildResult
buildMonadicStrategy = buildDerivationBuilder sBuilder

-- | Check if store path exists (daemon operation)
daemonStorePathExists :: StorePath -> TenM p 'Daemon Bool
daemonStorePathExists path = do
    env <- ask
    let fullPath = storePathToFilePath path env
    liftIO $ doesPathExist fullPath

-- | Check if store path exists (builder operation via daemon)
builderStorePathExists :: StorePath -> TenM p 'Builder Bool
builderStorePathExists path = do
    daemonConn <- getDaemonConnection
    verifyRequest <- createStoreVerifyRequest path
    response <- sendToDaemon daemonConn verifyRequest

    case response of
        StoreVerifyResponse exists -> return exists
        ErrorResponse err -> throwError err
        _ -> throwError $ StoreError "Unexpected response from daemon for store verification"

-- | Build a derivation in a sandbox (daemon operation)
buildWithSandboxDaemon :: Derivation -> SandboxConfig -> TenM 'Build 'Daemon BuildResult
buildWithSandboxDaemon deriv config = do
    -- Get all inputs
    let inputs = Set.map inputPath $ derivInputs deriv

    -- Run the build in a sandbox with proper privilege handling
    withSandbox inputs config $ \buildDir -> do
        -- Get the builder path in the store
        let builderPath = derivBuilder deriv
        builderContent <- readFromStore builderPath

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
                    catch (User.getUserEntryForName (sandboxUser config)) $ \(_ :: SomeException) ->
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
                    catch (User.getGroupEntryForName (sandboxGroup config)) $ \(_ :: SomeException) ->
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
            builderUser = builderUser,
            builderGroup = builderGroup,
            builderTimeoutSecs = 3600, -- 1 hour default timeout
            builderIsolation = sandboxUseMountNamespace config && sandboxUseNetworkNamespace config,
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
                returnDrvExists <- liftIO $ doesFileExist (returnDerivationPath buildDir)

                if returnDrvExists && exitCode == ExitSuccess
                    then do
                        -- Return the result for return-continuation handling
                        return $ BuildResult
                            { resultOutputs = Set.empty
                            , resultExitCode = exitCode
                            , resultLog = buildLog
                            , resultMetadata = Map.singleton "returnDerivation" (T.pack $ returnDerivationPath buildDir)
                            }
                    else do
                        -- Collect normal build results
                        outputs <- collectBuildResultDaemon deriv buildDir

                        -- Add build proof
                        addProof BuildProof

                        -- Return the build result
                        return BuildResult
                            { resultOutputs = outputs
                            , resultExitCode = exitCode
                            , resultLog = buildLog
                            , resultMetadata = Map.empty
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
    liftIO $ do
        hSetBuffering stdoutRead LineBuffering
        hSetBuffering stderrRead LineBuffering

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
    buildId <- liftIO $ fmap (fromIntegral) $ randomRIO (100000, 999999 :: Int)

    -- Start the builder process with proper privilege handling and timeout
    (mPid, result) <- liftIO $ mask $ \restore -> do
        -- Generate cleanup function
        let cleanup = do
                -- Read the PID
                mpid <- readIORef pidRef

                -- Kill the process if it's still running
                forM_ mpid $ \pid -> do
                    -- Try to kill gracefully first, then force kill
                    handle (\(_ :: SomeException) -> return ()) $ signalProcess sigTERM pid
                    threadDelay 500000  -- Give it 0.5 seconds to terminate
                    handle (\(_ :: SomeException) -> return ()) $ signalProcess sigKILL pid

                -- Close the pipes
                hClose stdoutRead
                hClose stderrRead
                hClose stdoutWrite
                hClose stderrWrite

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
                    hClose stdoutRead
                    hClose stderrRead

                    -- Connect stdout and stderr
                    stdoutFd <- handleToFd stdoutWrite
                    stderrFd <- handleToFd stderrWrite

                    -- Use the proper dup2 call to redirect file descriptors
                    void $ dup2 stdoutFd 1  -- 1 is stdout
                    void $ dup2 stderrFd 2  -- 2 is stderr

                    hClose stdoutWrite
                    hClose stderrWrite

                    -- Change to build directory
                    setCurrentDirectory buildDir

                    -- Prepare a clean environment
                    prepareCleanEnvironment

                    -- Drop privileges if needed
                    when needDropPrivs $ do
                        -- Set group first, then user
                        User.setGroupID (User.groupID (builderGroup env))
                        User.setUserID (User.userID (builderUser env))

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
                    contents <- hGetContents stdoutRead
                    evaluate (length contents)
                    putMVar stdoutVar contents

                stderrThread <- forkIO $ do
                    contents <- hGetContents stderrRead
                    evaluate (length contents)
                    putMVar stderrVar contents

                -- Wait for process to complete
                exitStatus <- getProcessStatus True True pid

                -- Get stdout and stderr content
                stdoutContent <- takeMVar stdoutVar
                stderrContent <- takeMVar stderrVar

                -- Process exit status
                case exitStatus of
                    Just (Exited exitCode) -> do
                        let exitResult = case exitCode of
                                0 -> ExitSuccess
                                n -> ExitFailure n
                        return (Just pid, Right (exitResult, stdoutContent, stderrContent))

                    Just (Terminated sig _) ->
                        return (Just pid, Left $ "Builder terminated by signal: " <> T.pack (show sig))

                    Just (Stopped sig) ->
                        return (Just pid, Left $ "Builder stopped by signal: " <> T.pack (show sig))

                    Nothing ->
                        return (Just pid, Left "Builder process disappeared")

        -- Apply timeout if configured
        if builderTimeoutSecs env > 0
            then do
                -- Run builder with timeout
                timeoutResult <- timeout (builderTimeoutSecs env * 1000000) timeoutAction
                case timeoutResult of
                    Nothing -> do
                        -- Timeout occurred, cleanup and return error
                        cleanup
                        return (Nothing, Left "Build timed out")
                    Just result -> result
            else
                -- Run without timeout
                timeoutAction `onException` cleanup

    -- Close the pipes (should be done by cleanup, but ensure it's done)
    liftIO $ do
        handle (\(_ :: SomeException) -> return ()) $ hClose stdoutRead
        handle (\(_ :: SomeException) -> return ()) $ hClose stderrRead
        handle (\(_ :: SomeException) -> return ()) $ hClose stdoutWrite
        handle (\(_ :: SomeException) -> return ()) $ hClose stderrWrite

    -- Return the result
    return result

-- | Create a pipe for process communication
createPipe :: IO (Handle, Handle)
createPipe = do
    (readFd, writeFd) <- System.Posix.IO.createPipe
    readHandle <- fdToHandle readFd
    writeHandle <- fdToHandle writeFd
    return (readHandle, writeHandle)

-- | Set process group ID
setpgid :: ProcessID -> ProcessID -> IO ()
setpgid childPid pgid = do
    -- Use the C function via FFI
    let result = c_setpgid (fromIntegral childPid) (fromIntegral pgid)
    return ()

-- | FFI declaration for setpgid
foreign import ccall unsafe "setpgid"
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
    -- We'll implement a simplified version that works on Linux
    -- Ideally, we'd scan /proc/self/fd for complete coverage
    -- For now, we'll close a reasonable range of file descriptors
    forM_ [3..1024] $ \fd -> do
        -- Try to close each FD, ignoring errors
        catch (closeFd (fromIntegral fd)) (\(_ :: SomeException) -> return ())
    return ()

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
getBuildEnvironment :: BuildEnv -> Derivation -> FilePath -> TenM 'Build t Map Text Text
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
            , ("TEN_RETURN_PATH", T.pack $ returnDerivationPath buildDir)
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
collectBuildResultDaemon deriv buildDir = do
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

    -- Process each expected output with database connection
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Process each expected output
        outputPaths <- foldM (processOutput db outDir) Set.empty (Set.toList $ derivOutputs deriv)

        -- Register all input-output references
        liftIO $ withTransaction db ReadWrite $ \txn -> do
            -- Get derivation path in the store
            let derivStorePath = StorePath (derivHash deriv) (derivName deriv <> ".drv")

            -- Register each output in the database
            derivInfo <- getDerivationPath txn (derivHash deriv)
            case derivInfo of
                Just storedDerivPath -> do
                    -- Register each output for this derivation
                    derivId <- storeDerivation txn deriv storedDerivPath
                    forM_ (Set.toList outputPaths) $ \outputPath -> do
                        -- 1. Register the derivation as a reference for this output
                        registerReference txn outputPath storedDerivPath

                        -- 2. Register all input references for this output
                        forM_ (Set.toList $ derivInputs deriv) $ \input -> do
                            registerReference txn outputPath (inputPath input)

                        -- 3. Scan the output file for additional references
                        scanAndRegisterReferences txn (storeLocation env) outputPath

                Nothing -> return () -- Should never happen if we stored the derivation first

        -- Add output proof
        addProof OutputProof

        -- Return the set of outputs
        return outputPaths
  where
    -- Process a single output, adding it to the store
    processOutput :: Database -> FilePath -> Set StorePath -> DerivationOutput -> TenM 'Build 'Daemon (Set StorePath)
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
                        addToStore (outputName output <> ".tar.gz") content
                    else do
                        -- For regular files, read directly
                        content <- liftIO $ BS.readFile outputFile'
                        addToStore (outputName output) content

                -- Register this as a valid path in the database
                liftIO $ dbExecute db
                    "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time, deriver, is_valid) \
                    \VALUES (?, ?, strftime('%s','now'), ?, 1)"
                    (storePathToText outputPath, storeHash outputPath, Just $ derivHash deriv)

                -- Return updated set
                return $ Set.insert outputPath accPaths
            else if isReturnContinuationDerivation (derivName deriv) (derivArgs deriv) (derivEnv deriv)
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
verifyBuildResult :: Derivation -> BuildResult -> TenM 'Build t Bool
verifyBuildResult deriv result = do
    -- First check exit code
    if resultExitCode result /= ExitSuccess
        then return False
        else do
            -- Check if this is a return-continuation build
            if isReturnContinuationDerivation (derivName deriv) (derivArgs deriv) (derivEnv deriv) &&
               Map.member "returnDerivation" (resultMetadata result)
                then do
                    -- For return-continuation, just check that the returned derivation exists
                    let returnPath = T.unpack $ resultMetadata result Map.! "returnDerivation"
                    returnExists <- liftIO $ doesFileExist returnPath

                    if returnExists
                        then do
                            -- Verify the returned derivation is properly formed
                            content <- liftIO $ BS.readFile returnPath
                            case deserializeDerivation content of
                                Left _ -> return False
                                Right _ -> return True
                        else
                            return False
                else do
                    -- For normal builds, check outputs
                    let expectedOutputNames = Set.map outputName (derivOutputs deriv)
                    let actualOutputNames = Set.map storeName (resultOutputs result)

                    -- Check if all expected outputs are included in actual outputs
                    let allOutputsPresent = expectedOutputNames `Set.isSubsetOf` actualOutputNames

                    -- Check that each output verifies correctly - verify differently based on context
                    env <- ask
                    validOutputs <- case currentPrivilegeTier env of
                        Daemon -> forM (Set.toList (resultOutputs result)) verifyStorePathDaemon
                        Builder -> forM (Set.toList (resultOutputs result)) verifyStorePathBuilder

                    -- Return True if all checks pass
                    return $ allOutputsPresent && and validOutputs

-- | Verify a store path in daemon context
verifyStorePathDaemon :: StorePath -> TenM p 'Daemon Bool
verifyStorePathDaemon path = do
    env <- ask
    let fullPath = storePathToFilePath path env
    exists <- liftIO $ doesPathExist fullPath
    if not exists
        then return False
        else do
            -- For a real implementation, we'd verify hash integrity here
            return True

-- | Verify a store path in builder context (via daemon)
verifyStorePathBuilder :: StorePath -> TenM p 'Builder Bool
verifyStorePathBuilder path = do
    daemonConn <- getDaemonConnection
    verifyRequest <- createStoreVerifyRequest path
    response <- sendToDaemon daemonConn verifyRequest

    case response of
        StoreVerifyResponse valid -> return valid
        _ -> return False

-- | Check if a build result includes a returned derivation
checkIfReturnDerivation :: BuildResult -> TenM 'Build t Bool
checkIfReturnDerivation result =
    return $ resultExitCode result == ExitSuccess &&
             Map.member "returnDerivation" (resultMetadata result)

-- | Check for a returned derivation in a build directory
checkForReturnedDerivation :: FilePath -> TenM 'Build t (Maybe Derivation)
checkForReturnedDerivation buildDir = do
    let returnPath = returnDerivationPath buildDir

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
            case deserializeDerivation content of
                Left err -> do
                    logMsg 1 $ "Error deserializing returned derivation: " <> err
                    return Nothing
                Right drv -> return $ Just drv
        else
            return Nothing

-- | Handle a returned derivation
handleReturnedDerivation :: BuildResult -> TenM 'Build t Derivation
handleReturnedDerivation result = do
    -- Get the returned derivation path
    let returnPath = T.unpack $ resultMetadata result Map.! "returnDerivation"

    -- Normalize and validate path
    let returnPath' = normalise returnPath

    -- Read and deserialize
    content <- liftIO $ BS.readFile returnPath'
    case deserializeDerivation content of
        Left err -> throwError $ SerializationError $
            "Failed to deserialize returned derivation: " <> err
        Right innerDrv -> do
            -- Add proof that we successfully got a returned derivation
            addProof RecursionProof

            -- Store the inner derivation in the content store - context appropriate
            env <- ask
            derivPath <- case currentPrivilegeTier env of
                Daemon -> storeDaemonDerivation innerDrv
                Builder -> storeBuilderDerivation innerDrv

            -- Register the inner derivation appropriately
            case currentPrivilegeTier env of
                Daemon -> do
                    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                        liftIO $ registerDerivationFile db innerDrv derivPath
                Builder -> do
                    daemonConn <- getDaemonConnection
                    regRequest <- createRegisterDerivationRequest innerDrv derivPath
                    _ <- sendToDaemon daemonConn regRequest
                    return ()

            -- Check for cycles
            chain <- gets buildChain
            let isCyclic = detectRecursionCycle (innerDrv : chain)
            when isCyclic $
                throwError $ CyclicDependency $
                    "Cyclic dependency detected in returned derivation: " <> derivName innerDrv

            return innerDrv

-- | Build a graph of derivations
buildDerivationGraph :: BuildGraph -> TenM 'Build t (Map Text BuildResult)
buildDerivationGraph graph = do
    -- First get dependencies in topological order
    sorted <- topologicalSort graph

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
buildInDependencyOrder :: [Derivation] -> TenM 'Build t [BuildResult]
buildInDependencyOrder derivations = do
    -- First check for cycles
    let cycle = detectRecursionCycle derivations
    when cycle $
        throwError $ CyclicDependency "Cycle detected in build dependencies"

    -- Build each derivation in order
    mapM buildDerivation derivations

-- | Build dependencies concurrently
buildDependenciesConcurrently :: [Derivation] -> TenM 'Build 'Daemon (Map String (Either BuildError BuildResult))
buildDependenciesConcurrently derivations = do
    env <- ask
    state <- get

    -- Create a map to hold results
    resultMap <- liftIO $ atomically $ newTVar Map.empty

    -- Create a semaphore to limit concurrency
    maxConcurrent <- asks (\e -> fromMaybe 4 (maxConcurrentBuilds e))
    sem <- liftIO $ newQSem maxConcurrent

    -- Track all build threads
    threads <- liftIO $ newTVarIO []

    -- Start a thread for each derivation
    liftIO $ forM_ derivations $ \drv -> do
        let hash = T.unpack $ derivHash drv
        thread <- mask $ \restore -> forkIO $ do
            -- Acquire semaphore
            bracket (waitQSem sem) (\_ -> signalQSem sem) $ \_ -> restore $ do
                -- Run the build in a separate thread
                result <- runTenDaemon (buildDerivationDaemon sDaemon drv) env state
                -- Store the result
                atomically $ modifyTVar resultMap $ \m ->
                    Map.insert hash (either Left (Right . fst) result) m

        -- Store thread reference
        atomically $ modifyTVar threads (thread:)

    -- Wait for all threads to complete
    liftIO $ do
        allThreads <- atomically $ readTVar threads
        -- First try graceful termination
        forM_ allThreads $ \t -> catch (killThread t) $ \(_ :: SomeException) -> return ()

        -- Wait a short time for threads to clean up
        threadDelay 1000000  -- 1 second

        -- Return the results
        atomically $ readTVar resultMap

-- | Wait for dependencies to complete
waitForDependencies :: Set BuildId -> TenM 'Build t ()
waitForDependencies depIds = do
    -- Get max wait time
    maxWaitTime <- asks (\e -> 60 * 60)  -- Default to 1 hour

    -- Check dependencies
    complete <- checkDependencies depIds

    unless complete $ do
        -- Wait for dependencies with timeout
        result <- liftIO $ timeout (maxWaitTime * 1000000) $ waitForDeps depIds

        when (isNothing result) $
            throwError $ BuildFailed "Timeout waiting for dependencies to complete"
  where
    checkDependencies :: Set BuildId -> TenM 'Build t Bool
    checkDependencies deps = do
        statuses <- forM (Set.toList deps) $ \bid -> do
            stat <- try $ getBuildStatus bid
            return $ case stat of
                Right BuildCompleted -> True
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

    -- Update build status in daemon state if in daemon mode
    env <- ask
    case currentPrivilegeTier env of
        Daemon -> do
            isDaemon <- isDaemonMode
            when isDaemon $ do
                -- Update the status and notify any waiting clients
                updateBuildStatus buildId (BuildRunning progress)
        Builder -> do
            -- Notify daemon of progress via protocol
            daemonConn <- getDaemonConnection
            progressRequest <- createBuildProgressRequest buildId progress
            _ <- sendToDaemon daemonConn progressRequest
            return ()

-- | Report build status
reportBuildStatus :: BuildId -> BuildStatus -> TenM 'Build t ()
reportBuildStatus buildId status = do
    -- Log status change
    logMsg 2 $ "Build status for " <> T.pack (show buildId) <> ": " <>
               T.pack (show status)

    -- Update status based on context
    env <- ask
    case currentPrivilegeTier env of
        Daemon -> do
            -- Update status in daemon state if in daemon mode
            isDaemon <- isDaemonMode
            when isDaemon $
                updateBuildStatus buildId status
        Builder -> do
            -- Notify daemon of status via protocol
            daemonConn <- getDaemonConnection
            statusRequest <- createBuildStatusRequest buildId status
            _ <- sendToDaemon daemonConn statusRequest
            return ()

-- | Update build status in daemon state
updateBuildStatus :: BuildId -> BuildStatus -> TenM 'Build 'Daemon ()
updateBuildStatus buildId status = do
    -- In a real daemon implementation, this would update a shared TVar
    -- and notify any clients waiting for status updates
    env <- ask
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        liftIO $ dbExecute db
            "INSERT OR REPLACE INTO BuildStatus (build_id, status, timestamp) VALUES (?, ?, strftime('%s','now'))"
            (T.pack (show buildId), T.pack (show status))

-- | Get build status from daemon state
getBuildStatus :: BuildId -> TenM 'Build t BuildStatus
getBuildStatus buildId = do
    env <- ask
    case currentPrivilegeTier env of
        Daemon -> getBuildStatusDaemon buildId
        Builder -> getBuildStatusBuilder buildId

-- | Get build status in daemon context
getBuildStatusDaemon :: BuildId -> TenM 'Build 'Daemon BuildStatus
getBuildStatusDaemon buildId = do
    env <- ask
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        results <- liftIO $ dbQuery db
            "SELECT status FROM BuildStatus WHERE build_id = ? ORDER BY timestamp DESC LIMIT 1"
            [T.pack (show buildId)] :: TenM 'Build 'Daemon [Only Text]

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

-- | Get build status in builder context
getBuildStatusBuilder :: BuildId -> TenM 'Build 'Builder BuildStatus
getBuildStatusBuilder buildId = do
    daemonConn <- getDaemonConnection
    statusRequest <- createBuildStatusRequest buildId BuildPending  -- Dummy status for request
    response <- sendToDaemon daemonConn statusRequest

    case response of
        BuildStatusResponse update -> return (buildStatus update)
        ErrorResponse err -> throwError err
        _ -> throwError $ BuildFailed "Unexpected response from daemon for status request"

-- | Semaphore implementation for controlling concurrency
data QSem = QSem (MVar Int)

newQSem :: Int -> IO QSem
newQSem n = do
    mvar <- newMVar n
    return (QSem mvar)

waitQSem :: QSem -> IO ()
waitQSem (QSem mvar) = do
    modifyMVar_ mvar $ \n -> do
        if n > 0
            then return (n - 1)
            else do
                -- Wait until resources are available
                let loop = do
                        threadDelay 100000  -- 0.1 seconds
                        modifyMVar mvar $ \n' ->
                            if n' > 0
                                then return (n' - 1, True)
                                else return (n', False)
                available <- loop
                if available
                    then return 0
                    else loop >> return 0

signalQSem :: QSem -> IO ()
signalQSem (QSem mvar) = do
    modifyMVar_ mvar $ \n -> return (n + 1)

-- | Get daemon connection (should be available in builder context)
getDaemonConnection :: TenM p 'Builder DaemonConnection
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ DaemonError "No daemon connection available"

-- | Create daemon protocol requests
createBuildDerivationRequest :: Derivation -> TenM p 'Builder DaemonRequest
createBuildDerivationRequest deriv =
    return $ BuildDerivationRequest deriv defaultBuildRequestInfo

createStoreRequest :: Text -> BS.ByteString -> TenM p 'Builder DaemonRequest
createStoreRequest name content =
    return $ StoreAddRequest name content

createStoreVerifyRequest :: StorePath -> TenM p 'Builder DaemonRequest
createStoreVerifyRequest path =
    return $ StoreVerifyRequest (storePathToText path)

createRegisterDerivationRequest :: Derivation -> StorePath -> TenM p 'Builder DaemonRequest
createRegisterDerivationRequest deriv path =
    return $ StoreDerivationCmd $ StoreDerivationRequest (serializeDerivation deriv) True

createBuildProgressRequest :: BuildId -> Float -> TenM p 'Builder DaemonRequest
createBuildProgressRequest bid progress =
    return $ BuildStatusRequest bid

createBuildStatusRequest :: BuildId -> BuildStatus -> TenM p 'Builder DaemonRequest
createBuildStatusRequest bid status =
    return $ BuildStatusRequest bid

-- | Send request to daemon and get response
sendToDaemon :: DaemonConnection -> DaemonRequest -> TenM p 'Builder DaemonResponse
sendToDaemon conn req = do
    -- This would use the actual client implementation from Ten.Daemon.Client
    response <- liftIO $ requestFromDaemon conn req
    return response

-- Helper for reading safely
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

-- Default build request info
defaultBuildRequestInfo :: BuildRequestInfo
defaultBuildRequestInfo = BuildRequestInfo
    { buildRequestTimeout = Nothing
    , buildRequestEnv = Map.empty
    , buildRequestFlags = []
    }
