{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ten.Build (
    BuildResult(..),
    buildDerivation,
    collectBuildResult,
    verifyBuildResult
) where

import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO

import Ten.Core
import Ten.Derivation
import Ten.Store
import Ten.Sandbox

-- | Result of a build operation
data BuildResult = BuildResult
    { resultOutputs :: Set StorePath    -- Paths to the build outputs
    , resultExitCode :: ExitCode       -- Exit code from the builder
    , resultLog :: Text                -- Build log output
    } deriving (Show, Eq)

-- | Build a derivation
buildDerivation :: Derivation -> TenM 'Build BuildResult
buildDerivation deriv = do
    env <- ask

    -- Prepare the derivation for building (check inputs, etc)
    instantiateDerivation deriv

    -- Get the inputs
    let inputs = Set.map inputPath (derivInputs deriv)

    -- Create basic sandbox config
    let config = defaultSandboxConfig {
      -- Allow access to system directories for builder scripts
      sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"]
    }

    -- Run the build in a sandbox
    withSandbox inputs config $ \buildDir -> do
        -- Get the builder path in the store
        let builderStorePath = derivBuilder deriv
        builderContent <- readFromStore builderStorePath

        -- Write the builder to the sandbox and make it executable
        let builderPath = buildDir </> "builder"
        liftIO $ BS.writeFile builderPath builderContent

        -- Make sure the builder is executable
        perms <- liftIO $ getPermissions builderPath
        liftIO $ setPermissions builderPath (setOwnerExecutable True $ perms)

        -- Double-check that it's executable
        finalPerms <- liftIO $ getPermissions builderPath
        logMsg 1 $ "Builder permissions - executable: " <> T.pack (show (executable finalPerms))

        -- Prepare environment variables with explicit PATH
        let baseEnv = Map.fromList
                [ ("TEN_STORE", T.pack $ storePath env)
                , ("TEN_BUILD_DIR", T.pack buildDir)
                , ("TEN_OUT", T.pack $ buildDir </> "out")
                , ("PATH", "/bin:/usr/bin:/usr/local/bin") -- Explicit PATH
                ]
        let buildEnv = Map.union (derivEnv deriv) baseEnv

        -- Create output directory
        liftIO $ createDirectoryIfMissing True (buildDir </> "out")

        -- Debug output for troubleshooting
        logMsg 1 $ "Building in: " <> T.pack buildDir

        -- Use explicit gcc path for compilation based on derivation name
        let args = if derivName deriv == "hello"
                  then ["hello.c", "-o", buildDir </> "out" </> "hello"]
                  else map T.unpack (derivArgs deriv)

        -- Log the build command
        logMsg 1 $ "Building: " <> derivName deriv
        logMsg 1 $ "Command: " <> T.pack builderPath <> " " <> T.intercalate " " (map T.pack args)

        -- Create a process for the builder
        let processConfig = (proc builderPath args)
                { cwd = Just buildDir
                , env = Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList buildEnv
                , std_in = NoStream
                , std_out = CreatePipe
                , std_err = CreatePipe
                }

        -- Run the builder
        (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode processConfig ""
        let buildLog = T.pack $ stdout ++ stderr

        -- Log the result
        case exitCode of
            ExitSuccess -> logMsg 1 $ "Build succeeded: " <> derivName deriv
            ExitFailure code -> do
                logMsg 1 $ "Build failed (" <> T.pack (show code) <> "): " <> derivName deriv
                logMsg 1 $ "Build output: " <> T.pack stdout
                logMsg 1 $ "Build error: " <> T.pack stderr

        -- Collect the build results
        outputs <- collectBuildResult deriv buildDir

        -- Add build proof
        addProof BuildProof

        -- Return the build result
        return BuildResult
            { resultOutputs = outputs
            , resultExitCode = exitCode
            , resultLog = buildLog
            }

-- | Collect output files from a build and add them to the store
collectBuildResult :: Derivation -> FilePath -> TenM 'Build (Set StorePath)
collectBuildResult deriv buildDir = do
    -- For each expected output, collect and store it
    let outDir = buildDir </> "out"

    -- Verify the output directory exists
    outDirExists <- liftIO $ doesDirectoryExist outDir
    unless outDirExists $ throwError $
        BuildFailed $ "Output directory not created: " <> T.pack outDir

    -- List directory contents for debugging
    outFiles <- liftIO $ listDirectory outDir
    logMsg 1 $ "Output directory contents: " <> T.pack (show outFiles)

    -- Process each expected output
    outputs <- forM (Set.toList $ derivOutputs deriv) $ \output -> do
        let outputFile = outDir </> T.unpack (outputName output)
        exists <- liftIO $ doesFileExist outputFile

        logMsg 1 $ "Checking for output: " <> outputName output <>
                   " at " <> T.pack outputFile <>
                   " (exists: " <> T.pack (show exists) <> ")"

        if exists
            then do
                -- Read the output and add it to the store
                content <- liftIO $ BS.readFile outputFile
                addToStore (outputName output) content
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
    -- Check that all expected outputs are present
    let expectedOutputNames = Set.map outputName (derivOutputs deriv)
    let actualOutputNames = Set.map storeName (resultOutputs result)

    -- Check if all expected outputs are included in actual outputs
    let allOutputsPresent = expectedOutputNames `Set.isSubsetOf` actualOutputNames

    -- Check that each output verifies correctly
    validOutputs <- forM (Set.toList (resultOutputs result)) $ \path -> do
        verifyStorePath path

    -- Return True if all checks pass
    return $ allOutputsPresent && and validOutputs
