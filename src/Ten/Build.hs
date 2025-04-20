{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Exception (try, catch, finally, mask, bracket, throwIO, onException, evaluate, SomeException)
import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify)
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
import Data.List (nub, isPrefixOf, isInfixOf)
import System.Directory
import System.FilePath
import qualified System.Process as Process
import System.Exit
import System.IO (hPutStrLn, stderr, Handle, hGetContents, hClose, IOMode(..), withFile, hDuplicateTo, hSetBuffering, BufferMode(..))
import System.Posix.Files (setFileMode, getFileStatus, fileMode, fileOwner, fileGroup, setOwnerAndGroup)
import qualified System.Posix.User as User
import System.Posix.Process (ProcessStatus(..), getProcessStatus, forkProcess, executeFile, getProcessID)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM, installHandler, Handler(..))
import System.Posix.Types (ProcessID, FileMode, UserID, GroupID)
import qualified System.Posix.IO as PosixIO
import System.Timeout (timeout)
import System.Random (randomRIO)
import Control.Concurrent.Async (async, Async, wait, cancel, waitCatch, race, withAsync)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import System.FilePath.Posix (normalise, takeDirectory)
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import Data.Bits ((.|.), (.&.))
import Foreign.C.Error (Errno(..), getErrno)

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Sandbox
import Ten.Graph
import Ten.DB.Core
import Ten.DB.Derivations
import Ten.DB.References

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

-- | Build a derivation, using the appropriate strategy
buildDerivation :: Derivation -> TenM 'Build BuildResult
buildDerivation deriv = do
    -- Log start of build
    logMsg 1 $ "Building derivation: " <> derivName deriv

    -- Serialize and store the derivation in the store
    derivPath <- storeDerivationFile deriv

    -- Generate a unique build ID if not already set
    currentBID <- gets currentBuildId
    when (isNothing currentBID) $ do
        newBid <- newBuildId
        setCurrentBuildId newBid

    -- Register the derivation in the database
    env <- ask
    db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000
    liftIO $ registerDerivationFile db deriv derivPath `finally` closeDatabase db

    -- First instantiate the derivation
    instantiateDerivation deriv

    -- Select build strategy based on derivation
    case derivStrategy deriv of
        ApplicativeStrategy -> do
            logMsg 2 $ "Using applicative (parallel) build strategy for " <> derivName deriv
            buildApplicativeStrategy deriv
        MonadicStrategy -> do
            logMsg 2 $ "Using monadic (sequential) build strategy for " <> derivName deriv
            buildMonadicStrategy deriv

-- | Build a derivation using applicative strategy (parallel dependencies)
buildApplicativeStrategy :: Derivation -> TenM 'Build BuildResult
buildApplicativeStrategy deriv = do
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
    missingDeps <- filterM (\path -> not <$> storePathExists path) (Set.toList inputs)

    if null missingDeps
        then buildWithSandbox deriv config
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
            buildWithSandbox deriv config
  where
    isLeft (Left _) = True
    isLeft _ = False

    fromLeft def (Left x) = x
    fromLeft def _ = def

    getErrorMessage (BuildFailed msg) = msg
    getErrorMessage (StoreError msg) = msg
    getErrorMessage (SandboxError msg) = msg
    getErrorMessage _ = "Build error"

-- | Find derivation objects for dependencies
findDependencyDerivations :: [StorePath] -> TenM 'Build (Map String Derivation)
findDependencyDerivations paths = do
    env <- ask

    -- Initialize the database connection
    db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000

    -- Use the database to look up derivations for the provided output paths
    result <- liftIO $ findDerivationsByOutputs db paths `finally` closeDatabase db

    -- If any lookup failed, throw an error
    when (Map.size result /= length paths) $ do
        let missingPaths = filter (\p -> not $ any (\(h, d) -> (storeHash p) `T.isInfixOf` (T.pack h)) (Map.toList result)) paths
        throwError $ StoreError $ "Could not find derivations for outputs: " <>
                    T.intercalate ", " (map storeName missingPaths)

    return result

-- | Build a derivation using monadic strategy (sequential with return-continuation)
buildMonadicStrategy :: Derivation -> TenM 'Build BuildResult
buildMonadicStrategy deriv = do
    env <- ask

    -- Get all direct dependencies
    let inputs = Set.map inputPath (derivInputs deriv)

    -- Check if all inputs exist
    missingDeps <- filterM (\path -> not <$> storePathExists path) (Set.toList inputs)
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
    result <- buildWithSandbox deriv config

    -- Check if this build returned a derivation
    returnDerivExists <- checkIfReturnDerivation result

    if returnDerivExists
        then do
            -- Handle return-continuation case
            innerDrv <- handleReturnedDerivation result
            -- Track this in the derivation chain
            addToDerivationChain' deriv

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

-- | Add derivation to build chain
addToDerivationChain' :: Derivation -> TenM 'Build ()
addToDerivationChain' drv = modify $ \s -> s { buildChain = drv : buildChain s }

-- | Build a derivation in a sandbox
buildWithSandbox :: Derivation -> SandboxConfig -> TenM 'Build BuildResult
buildWithSandbox deriv config = do
    -- Get all inputs
    let inputs = Set.map inputPath (derivInputs deriv)

    -- Run the build in a sandbox with proper privilege handling
    withSandbox inputs config $ \buildDir -> do
        -- Get the builder path in the store
        let builderPath = derivBuilder deriv
        builderContent <- readFromStore builderPath

        -- Set up builder with proper permissions
        execPath <- setupBuilder builderPath builderContent buildDir

        -- Prepare environment variables
        env <- ask
        let buildEnv = getBuildEnvironment env deriv buildDir

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
                        outputs <- collectBuildResult deriv buildDir

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
runBuilder :: BuilderEnv -> TenM 'Build (Either Text (ExitCode, String, String))
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
    newPgid <- liftIO $ fmap (Just . Fd) $ getProcessID

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
                    hDuplicateTo stdoutWrite 1
                    hDuplicateTo stderrWrite 2
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
    (readFd, writeFd) <- PosixIO.createPipe
    readHandle <- PosixIO.fdToHandle readFd
    writeHandle <- PosixIO.fdToHandle writeFd
    return (readHandle, writeHandle)

-- | Set process group ID
setpgid :: ProcessID -> ProcessID -> IO ()
setpgid childPid pgid = do
    -- Stubbed placeholder - in actual code, should call system function:
    -- c_setpgid (fromIntegral childPid) (fromIntegral pgid)
    return ()

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
    -- In real implementation, would reset common signals to default
    -- For now, just reset SIGTERM as an example
    installHandler sigTERM Default Nothing
    return ()

-- | Close all unused file descriptors (except stdin, stdout, stderr)
closeUnusedFileDescriptors :: IO ()
closeUnusedFileDescriptors = do
    -- This would normally iterate through /proc/self/fd and close
    -- most file descriptors, but we'll skip for simplicity
    return ()

-- | Set up a builder executable in the sandbox
setupBuilder :: StorePath -> BS.ByteString -> FilePath -> TenM 'Build FilePath
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
getBuildEnvironment :: BuildEnv -> Derivation -> FilePath -> Map Text Text
getBuildEnvironment env deriv buildDir =
    Map.unions
        [ -- Base environment from derivation
          derivEnv deriv
        , -- Ten-specific environment variables
          Map.fromList
            [ ("TEN_STORE", T.pack $ storePath env)
            , ("TEN_BUILD_DIR", T.pack buildDir)
            , ("TEN_OUT", T.pack $ buildDir </> "out")
            , ("TEN_RETURN_PATH", T.pack $ returnDerivationPath buildDir)
            , ("PATH", "/bin:/usr/bin:/usr/local/bin") -- Explicit PATH
            , ("HOME", T.pack buildDir) -- Set HOME to build directory
            , ("TMPDIR", T.pack $ buildDir </> "tmp") -- Set TMPDIR to sandbox tmp
            , ("TMP", T.pack $ buildDir </> "tmp") -- Alternative tmp env var
            , ("TEMP", T.pack $ buildDir </> "tmp") -- Another alternative
            , ("TEN_BUILD_ID", maybe "unknown" (T.pack . show) =<< gets currentBuildId) -- Current build ID
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

-- | Collect output files from a build and add them to the store
collectBuildResult :: Derivation -> FilePath -> TenM 'Build (Set StorePath)
collectBuildResult deriv buildDir = do
    -- Get the output directory
    let outDir = buildDir </> "out"

    -- Verify the output directory exists
    outDirExists <- liftIO $ doesDirectoryExist outDir
    unless outDirExists $ throwError $
        BuildFailed $ "Output directory not created: " <> T.pack outDir

    -- List directory contents for debugging
    outFiles <- liftIO $ listDirectory outDir
    logMsg 2 $ "Output directory contents: " <> T.pack (show outFiles)

    -- Process each expected output
    outputPaths <- forM (Set.toList $ derivOutputs deriv) $ \output -> do
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
                if isDir
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
            else if isReturnContinuationDerivation (derivName deriv) (derivArgs deriv) (derivEnv deriv)
                then do
                    -- For return-continuation builds, outputs might not be created
                    -- Return the predicted output path anyway
                    return $ outputPath output
                else
                    throwError $ BuildFailed $
                        "Expected output not produced: " <> outputName output

    -- Add output proof
    addProof OutputProof

    -- Register derivation-output mappings in the database
    env <- ask
    db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000

    -- Get derivation path in the store
    derivPath <- storeDerivationFile deriv

    -- Register each output in the database
    liftIO $ withTransaction db ReadWrite $ \db' -> do
        derivInfo <- getDerivationPath db' (derivHash deriv)
        case derivInfo of
            Just storedDerivPath -> do
                -- Register each output for this derivation
                derivId <- storeDerivation db' deriv storedDerivPath
                forM_ (Set.toList $ derivOutputs deriv) $ \output -> do
                    let outputPath' = outputPath output
                    registerDerivationOutput db' derivId (outputName output) outputPath'

                    -- Register reference from output to derivation
                    addDerivationReference db' outputPath' storedDerivPath

            Nothing -> return () -- Should never happen if we stored the derivation first

    -- Close the database connection
    liftIO $ closeDatabase db

    -- Return the set of outputs
    return $ Set.fromList outputPaths

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
verifyBuildResult :: Derivation -> BuildResult -> TenM 'Build Bool
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

                    -- Check that each output verifies correctly
                    validOutputs <- forM (Set.toList (resultOutputs result)) $ \path -> do
                        verifyStorePath path

                    -- Return True if all checks pass
                    return $ allOutputsPresent && and validOutputs

-- | Check if a build result includes a returned derivation
checkIfReturnDerivation :: BuildResult -> TenM 'Build Bool
checkIfReturnDerivation result =
    return $ resultExitCode result == ExitSuccess &&
             Map.member "returnDerivation" (resultMetadata result)

-- | Check for a returned derivation in a build directory
checkForReturnedDerivation :: FilePath -> TenM 'Build (Maybe Derivation)
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
handleReturnedDerivation :: BuildResult -> TenM 'Build Derivation
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

            -- Store the inner derivation in the content store
            derivPath <- storeDerivationFile innerDrv

            -- Register the inner derivation in the database
            env <- ask
            db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000
            liftIO $ registerDerivationFile db innerDrv derivPath `finally` closeDatabase db

            -- Check for cycles
            chain <- gets buildChain
            let isCyclic = detectRecursionCycle (innerDrv : chain)
            when isCyclic $
                throwError $ CyclicDependency $
                    "Cyclic dependency detected in returned derivation: " <> derivName innerDrv

            return innerDrv

-- | Build a graph of derivations
buildDerivationGraph :: BuildGraph -> TenM 'Build (Map Text BuildResult)
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
        let drvId = derivNodeId drv
        return $ Map.insert drvId result results

-- | Build derivations in dependency order
buildInDependencyOrder :: [Derivation] -> TenM 'Build [BuildResult]
buildInDependencyOrder derivations = do
    -- First check for cycles
    let cycle = detectRecursionCycle derivations
    when cycle $
        throwError $ CyclicDependency "Cycle detected in build dependencies"

    -- Build each derivation in order
    mapM buildDerivation derivations

-- | Build dependencies concurrently
buildDependenciesConcurrently :: [Derivation] -> TenM 'Build (Map String (Either BuildError BuildResult))
buildDependenciesConcurrently derivations = do
    env <- ask
    state <- get

    -- Create a map to hold results
    resultMap <- liftIO $ atomically $ newTVar Map.empty

    -- Create a semaphore to limit concurrency
    maxConcurrent <- asks (\e -> fromMaybe 4 (maxConcurrentBuilds e))
    sem <- liftIO $ Ten.Build.newQSem maxConcurrent

    -- Track all build threads
    threads <- liftIO $ newTVarIO []

    -- Start a thread for each derivation
    liftIO $ forM_ derivations $ \drv -> do
        let hash = T.unpack $ derivHash drv
        thread <- mask $ \restore -> forkIO $ do
            -- Acquire semaphore
            bracket (Ten.Build.waitQSem sem) (\_ -> Ten.Build.signalQSem sem) $ \_ -> restore $ do
                -- Run the build in a separate thread
                result <- runTen (buildDerivation drv) env state
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
waitForDependencies :: Set BuildId -> TenM 'Build ()
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
    checkDependencies :: Set BuildId -> TenM 'Build Bool
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
reportBuildProgress :: BuildId -> Float -> TenM 'Build ()
reportBuildProgress buildId progress = do
    -- Log progress
    logMsg 2 $ "Build progress for " <> T.pack (show buildId) <> ": " <>
               T.pack (show (progress * 100)) <> "%"

    -- Update build status in daemon state if in daemon mode
    isDaemon <- isDaemonMode
    when isDaemon $ do
        -- Update the status and notify any waiting clients
        updateBuildStatus buildId (BuildRunning progress)

-- | Report build status
reportBuildStatus :: BuildId -> BuildStatus -> TenM 'Build ()
reportBuildStatus buildId status = do
    -- Log status change
    logMsg 2 $ "Build status for " <> T.pack (show buildId) <> ": " <>
               T.pack (show status)

    -- Update status in daemon state if in daemon mode
    isDaemon <- isDaemonMode
    when isDaemon $
        updateBuildStatus buildId status

-- | Update build status in daemon state
updateBuildStatus :: BuildId -> BuildStatus -> TenM 'Build ()
updateBuildStatus buildId status = do
    -- In a real daemon implementation, this would update a shared TVar
    -- and notify any clients waiting for status updates
    return ()

-- | Get build status from daemon state
getBuildStatus :: BuildId -> TenM 'Build BuildStatus
getBuildStatus buildId = do
    -- In a real daemon implementation, this would query shared state
    -- For now, return a placeholder
    throwError $ BuildFailed $ "Cannot find build: " <> T.pack (show buildId)

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

-- | Detect recursion cycles in a chain of derivations
detectRecursionCycle :: [Derivation] -> Bool
detectRecursionCycle derivations =
    -- Check for duplicate hashes which indicates a cycle
    let hashes = map derivHash derivations
        uniqueHashes = nub hashes
    in length uniqueHashes < length hashes
