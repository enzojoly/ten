{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Exception (try, SomeException)
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO (hPutStrLn, stderr)

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
        sandboxReturnSupport = False  -- Not using return continuation for applicative builds
    }

    -- Check if we need to build dependencies first
    missingDeps <- filterM (\path -> not <$> storePathExists path) (Set.toList inputs)

    -- If we have missing dependencies, we would build them concurrently here
    unless (null missingDeps) $ do
        logMsg 2 $ "Building " <> T.pack (show $ length missingDeps) <> " dependencies first"
        -- Note: In a real implementation, we would use STM-based concurrency for parallel builds
        -- For now, we'll assume all dependencies are available
        throwError $ BuildFailed $ "Missing dependencies: " <>
                     T.intercalate ", " (map storeName missingDeps)

    -- Now build with all dependencies available
    buildWithSandbox deriv config

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
        sandboxReturnSupport = True
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

    -- Run the build in a sandbox
    withSandbox inputs config $ \buildDir -> do
        -- Get the builder path in the store
        let builderPath = derivBuilder deriv
        builderContent <- readFromStore builderPath

        -- Set up builder
        execPath <- setupBuilder builderPath builderContent buildDir

        -- Prepare environment variables
        env <- ask
        let buildEnv = getBuildEnvironment env deriv buildDir

        -- Log the build command
        logMsg 1 $ "Building: " <> derivName deriv
        logMsg 2 $ "Command: " <> T.pack execPath <> " " <>
                   T.intercalate " " (map T.pack $ map T.unpack $ derivArgs deriv)

        -- Run the builder
        (exitCode, stdout, stderr) <- runBuilder execPath (map T.unpack $ derivArgs deriv) buildDir buildEnv
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

-- | Run a builder process
runBuilder :: FilePath -> [String] -> FilePath -> Map Text Text -> TenM 'Build (ExitCode, String, String)
runBuilder program args buildDir envVars = do
    -- Create process configuration
    let processConfig = (proc program args)
            { cwd = Just buildDir
            , env = Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList envVars
            , std_in = NoStream
            , std_out = CreatePipe
            , std_err = CreatePipe
            }

    -- Run the process
    liftIO $ readCreateProcessWithExitCode processConfig ""

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
            isCyclic <- detectRecursionCycle chain
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
    cycle <- detectRecursionCycle derivations
    when cycle $
        throwError $ CyclicDependency "Cycle detected in build dependencies"

    -- Build each derivation in order
    mapM buildDerivation derivations

-- | Build dependencies concurrently
buildDependenciesConcurrently :: [Derivation] -> TenM 'Build (Map Text BuildResult)
buildDependenciesConcurrently derivations = do
    env <- ask
    state <- get

    -- Create TVars for results
    resultVars <- liftIO $ atomically $
        mapM (\drv -> do
                resultVar <- newEmptyTMVar
                return (derivHash drv, resultVar)
              ) derivations

    -- Start a thread for each derivation
    let resultMap = Map.fromList resultVars
    liftIO $ forM_ derivations $ \drv -> do
        let resultVar = resultMap Map.! derivHash drv
        _ <- forkIO $ do
            -- Run the build in a separate thread
            result <- runTen (buildDerivation drv) env state
            -- Store the result
            atomically $ case result of
                Left err -> putTMVar resultVar (Left err)
                Right (r, _) -> putTMVar resultVar (Right r)
        return ()

    -- Wait for all results
    results <- liftIO $ atomically $
        mapM (\(hash, var) -> do
                result <- readTMVar var
                return (hash, result)
              ) (Map.toList resultMap)

    -- Process the results
    let (errors, successes) = foldr collectResults ([], Map.empty) results

    -- If there were errors, report them
    unless (null errors) $
        throwError $ BuildFailed $ "Failed to build dependencies: " <>
                     T.intercalate ", " errors

    return successes
  where
    collectResults (hash, result) (errs, succs) =
        case result of
            Left err -> (T.pack (show err):errs, succs)
            Right r -> (errs, Map.insert hash r succs)

-- | Wait for dependencies to complete
waitForDependencies :: Set BuildId -> TenM 'Build ()
waitForDependencies depIds = do
    -- In a real implementation, this would use STM to wait for builds to complete
    -- For now, just a placeholder
    return ()

-- | Report build progress
reportBuildProgress :: BuildId -> Float -> TenM 'Build ()
reportBuildProgress buildId progress = do
    -- Log progress
    logMsg 2 $ "Build progress for " <> T.pack (show buildId) <> ": " <>
               T.pack (show (progress * 100)) <> "%"

    -- In a real implementation, this would update a TVar in the daemon state
    return ()

-- | Report build status
reportBuildStatus :: BuildId -> BuildStatus -> TenM 'Build ()
reportBuildStatus buildId status = do
    -- Log status change
    logMsg 2 $ "Build status for " <> T.pack (show buildId) <> ": " <>
               T.pack (show status)

    -- In a real implementation, this would update a TVar in the daemon state
    return ()

-- | Set file permissions
setFileMode :: FilePath -> Int -> IO ()
setFileMode path mode = do
    -- Set permissions for the file
    perms <- getPermissions path
    setPermissions path $
        setOwnerExecutable True $
        setOwnerWritable True $
        setOwnerReadable True perms
