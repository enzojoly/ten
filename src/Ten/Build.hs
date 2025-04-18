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
import Control.Exception (try, catch, finally, mask, bracket, throwIO, SomeException)
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes)
import Data.Time (getCurrentTime, diffUTCTime)
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO (hPutStrLn, stderr, Handle, hGetContents, hClose, IOMode(..), withFile)
import System.Posix.Files (setFileMode, getFileStatus, fileMode)
import System.Posix.User (getUserEntryForName, UserEntry(..), getGroupEntryForName, GroupEntry(..))
import System.Posix.Process (ProcessStatus(..), getProcessStatus)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Posix.Types (ProcessID, FileMode)
import System.Timeout (timeout)
import System.Random (randomRIO)
import Control.Concurrent.Async (async, Async, wait, cancel, waitCatch)

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Sandbox
import Ten.Graph

-- | Result of a build operation
data BuildResult = BuildResult
    { resultOutputs :: Set StorePath    -- Paths to the build outputs
    , resultExitCode :: ExitCode        -- Exit code from the builder
    , resultLog :: Text                 -- Build log output
    , resultMetadata :: Map Text Text   -- Additional metadata
    } deriving (Show, Eq)

-- | Build a derivation, using the appropriate strategy
buildDerivation :: Derivation -> TenM 'Build BuildResult
buildDerivation deriv = do
    -- Log start of build
    logMsg 1 $ "Building derivation: " <> derivName deriv

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

    -- Create basic sandbox config
    let config = defaultSandboxConfig {
        sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"],
        sandboxReturnSupport = False,  -- Not using return continuation for applicative builds
        sandboxPrivileged = False,     -- Always run as unprivileged user
        sandboxUser = "nobody",        -- Default unprivileged user
        sandboxGroup = "nogroup"       -- Default unprivileged group
    }

    -- Check if we need to build dependencies first
    missingDeps <- filterM (\path -> not <$> storePathExists path) (Set.toList inputs)

    if null missingDeps
        then buildWithSandbox deriv config
        else do
            -- Build missing dependencies concurrently
            logMsg 2 $ "Building " <> T.pack (show $ length missingDeps) <> " dependencies first"

            -- Create a map of dependency derivations
            depDerivs <- liftIO $ atomically $ newTVar Map.empty

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

    -- Find derivation objects for dependencies
    findDependencyDerivations :: [StorePath] -> TenM 'Build (Map String Derivation)
    findDependencyDerivations paths = do
        -- In a real implementation, we would look up the derivations from
        -- a derivation store that maps outputs to their derivations
        -- For now, we'll create a simulated dependency mapping
        foldM (\acc path -> do
            let hash = T.unpack $ storeHash path
            let name = T.unpack $ storeName path
            -- Find or create a derivation for this path
            drv <- simulateDerivationForOutput path
            return $ Map.insert hash drv acc
          ) Map.empty paths

    -- Create a simulated derivation for an output
    simulateDerivationForOutput :: StorePath -> TenM 'Build Derivation
    simulateDerivationForOutput path = do
        env <- ask

        -- Create a simple builder that copies from the store to the output
        let builderScript = T.unlines [
                "#!/bin/sh",
                "set -e",
                "cp \"$1\" \"$TEN_OUT/" <> storeName path <> "\"",
                "echo \"Built " <> storeName path <> "\""
              ]

        -- Add builder to store
        builder <- addToStore "simple-builder.sh" (TE.encodeUtf8 builderScript)

        -- Create a derivation that produces this output
        let name = "build-" <> storeName path
        let args = [T.pack $ storePathToFilePath path env]
        let inputs = Set.singleton (DerivationInput path (storeName path))
        let outputs = Set.singleton (storeName path)
        let envVars = Map.singleton "PATH" "/bin:/usr/bin"
        let system = "x86_64-linux"

        -- Create the derivation
        mkDerivation name builder args inputs outputs envVars system

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
        sandboxGroup = "nogroup"       -- Default unprivileged group
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

        -- Determine user/group for builder process
        builderUser <- liftIO $ do
            -- If running as root, we can select a user
            uid <- User.getRealUserID
            if uid == 0
                then do
                    -- Try to use the specified user, fallback to nobody
                    catch (getUserEntryForName (sandboxUser config)) $ \(_ :: SomeException) ->
                        getUserEntryForName "nobody"
                else
                    -- Not running as root, use current user
                    User.getUserEntryForID uid

        builderGroup <- liftIO $ do
            -- If running as root, we can select a group
            uid <- User.getRealUserID
            if uid == 0
                then do
                    -- Try to use the specified group, fallback to nogroup
                    catch (getGroupEntryForName (sandboxGroup config)) $ \(_ :: SomeException) ->
                        getGroupEntryForName "nogroup"
                else
                    -- Not running as root, use current user's primary group
                    User.getGroupEntryForID (userGroupID builderUser)

        -- Log the build command
        logMsg 1 $ "Building: " <> derivName deriv
        logMsg 2 $ "Command: " <> T.pack execPath <> " " <>
                   T.intercalate " " (map T.pack $ map T.unpack $ derivArgs deriv)
        logMsg 3 $ "Running as: " <> T.pack (userName builderUser) <> ":" <> T.pack (groupName builderGroup)

        -- Run the builder with proper privilege handling
        buildResult <- runBuilder execPath (map T.unpack $ derivArgs deriv) buildDir buildEnv builderUser builderGroup

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
runBuilder :: FilePath -> [String] -> FilePath -> Map Text Text -> UserEntry -> GroupEntry
           -> TenM 'Build (Either Text (ExitCode, String, String))
runBuilder program args buildDir envVars userEntry groupEntry = do
    -- Verify that the program exists and is executable
    progExists <- liftIO $ doesFileExist program
    unless progExists $
        return $ Left $ "Builder program does not exist: " <> T.pack program

    -- Check execute permission
    perms <- liftIO $ getFileStatus program
    let mode = fileMode perms
    let isExecutable = mode .&. 0o100 /= 0  -- Check for owner execute bit
    unless isExecutable $
        return $ Left $ "Builder program is not executable: " <> T.pack program

    -- Create pipes for stdout and stderr
    (stdoutRead, stdoutWrite) <- liftIO createPipe
    (stderrRead, stderrWrite) <- liftIO createPipe

    -- Convert environment variables
    let envList = map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList envVars

    -- Determine if we need to drop privileges
    needDropPrivs <- liftIO $ do
        uid <- User.getRealUserID
        return $ uid == 0 && userName userEntry /= "root"

    -- Start the builder process with proper privilege handling
    (mPid, result) <- liftIO $ mask $ \restore -> do
        -- Fork a child process
        pid <- forkProcess $ do
            -- In child process

            -- Redirect stdout and stderr
            hClose stdoutRead
            hClose stderrRead

            -- Connect stdout and stderr
            hDuplicateTo stdoutWrite 1
            hDuplicateTo stderrWrite 2
            hClose stdoutWrite
            hClose stderrWrite

            -- Drop privileges if needed
            when needDropPrivs $ do
                -- Set group first, then user
                User.setGroupID (groupID groupEntry)
                User.setUserID (userID userEntry)

            -- Change to build directory
            setCurrentDirectory buildDir

            -- Execute the builder
            executeFile program True args (Just envList)

            -- This should never be reached
            exitWith (ExitFailure 127)

        -- In parent process
        hClose stdoutWrite
        hClose stderrWrite

        -- Read stdout and stderr from the child process
        stdoutContent <- hGetContents stdoutRead
        stderrContent <- hGetContents stderrRead

        -- Wait for process to complete with timeout
        let maxWaitTime = 3600  -- 1 hour timeout
        mExitStatus <- timeout (maxWaitTime * 1000000) $ getProcessStatus True True pid

        case mExitStatus of
            Nothing -> do
                -- Timeout, kill the process
                catch (signalProcess sigKILL pid) $ \(_ :: SomeException) -> return ()
                return (Just pid, Left "Build timed out after 1 hour")

            Just exitStatus -> do
                case exitStatus of
                    Exited exitCode -> do
                        let exitResult = case exitCode of
                                0 -> ExitSuccess
                                n -> ExitFailure n
                        return (Just pid, Right (exitResult, stdoutContent, stderrContent))

                    Terminated sig _ ->
                        return (Just pid, Left $ "Builder terminated by signal: " <> T.pack (show sig))

                    Stopped sig ->
                        return (Just pid, Left $ "Builder stopped by signal: " <> T.pack (show sig))

    -- Close the read handles
    liftIO $ do
        hClose stdoutRead
        hClose stderrRead

    -- Return the result
    return result

-- | Set up a builder executable in the sandbox
setupBuilder :: StorePath -> BS.ByteString -> FilePath -> TenM 'Build FilePath
setupBuilder builderPath builderContent buildDir = do
    -- Write the builder to the sandbox
    let execPath = buildDir </> "builder"
    liftIO $ BS.writeFile execPath builderContent

    -- Make sure the builder is executable
    liftIO $ setFileMode execPath 0o755

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
            , ("TEN_BUILD_ID", maybe "unknown" (T.pack . show) =<< gets currentBuildId) -- Current build ID
            , ("TEN_DERIVATION_NAME", derivName deriv) -- Name of the derivation
            , ("TEN_SYSTEM", derivSystem deriv) -- Target system
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
    outputs <- forM (Set.toList $ derivOutputs deriv) $ \output -> do
        let outputFile = outDir </> T.unpack (outputName output)
        exists <- liftIO $ doesFileExist outputFile

        logMsg 2 $ "Checking for output: " <> outputName output <>
                   " at " <> T.pack outputFile <>
                   " (exists: " <> T.pack (show exists) <> ")"

        if exists
            then do
                -- Read the output and add it to the store
                content <- liftIO $ BS.readFile outputFile
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

    -- Return the set of outputs
    return $ Set.fromList outputs

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
                    liftIO $ doesFileExist returnPath
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
    returnDrvExists <- liftIO $ doesFileExist returnPath

    if returnDrvExists
        then do
            -- Read and deserialize the returned derivation
            content <- liftIO $ BS.readFile returnPath
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

    -- Read and deserialize
    content <- liftIO $ BS.readFile returnPath
    case deserializeDerivation content of
        Left err -> throwError $ SerializationError $
            "Failed to deserialize returned derivation: " <> err
        Right innerDrv -> do
            -- Add proof that we successfully got a returned derivation
            addProof RecursionProof

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
                result <- runTen (buildDerivation drv) env state
                -- Store the result
                atomically $ modifyTVar resultMap $ \m ->
                    Map.insert hash (either Left (Right . fst) result) m

        -- Store thread reference
        atomically $ modifyTVar threads (thread:)

    -- Wait for all threads to complete
    liftIO $ do
        allThreads <- atomically $ readTVar threads
        forM_ allThreads $ \t -> catch (killThread t) $ \(_ :: SomeException) -> return ()

        -- Return the results
        atomically $ readTVar resultMap

-- | Wait for dependencies to complete
waitForDependencies :: Set BuildId -> TenM 'Build ()
waitForDependencies depIds = do
    -- In a daemon environment, we would track build status in shared state
    -- Here we'll use a simple wait-and-check approach

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

    -- In a real implementation, we would use blocking notifications
    -- rather than polling

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

-- | Utility function to create a pipe
createPipe :: IO (Handle, Handle)
createPipe = do
    (readFd, writeFd) <- System.Posix.IO.createPipe
    readHandle <- System.Posix.IO.fdToHandle readFd
    writeHandle <- System.Posix.IO.fdToHandle writeFd
    return (readHandle, writeHandle)

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

-- | Bitwise AND operation for file modes
(.&.) :: FileMode -> FileMode -> FileMode
a .&. b = a * b
