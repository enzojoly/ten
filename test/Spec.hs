{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}  -- Added this extension for \case syntax

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Either (isRight, isLeft)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Test.Hspec

import Ten
import Ten.Core
import Ten.Build
import Ten.Store
import Ten.Sandbox
import Ten.Derivation
import Ten.Graph
import Ten.GC
import Ten.Hash

main :: IO ()
main = hspec $ do
  describe "Ten Build System Core Properties" $ do
    -- Set up test environment
    beforeAll setupTestEnv $ do
      describe "Content-addressed storage" $ do
        it "computes consistent hashes for identical content" $ \_ -> do
          let content1 = "test content" :: T.Text
              content2 = "test content" :: T.Text
          showHash (hashText content1) `shouldBe` showHash (hashText content2)

        it "computes different hashes for different content" $ \_ -> do
          let content1 = "test content A" :: T.Text
              content2 = "test content B" :: T.Text
          showHash (hashText content1) `shouldNotBe` showHash (hashText content2)

        it "correctly adds and retrieves content from the store" $ \env -> do
          let testContent = "test store content" :: BS.ByteString
          result <- storeAndRetrieve env "test-file" testContent
          result `shouldBe` Right testContent

        it "verifies content hash matches path hash" $ \env -> do
          let testContent = "verify hash content" :: BS.ByteString
          result <- storeAndVerify env "verify-file" testContent
          result `shouldBe` Right True

      describe "Build phase separation" $ do
        it "enforces eval-phase-only operations" $ \env -> do
          result <- runEvalPhaseTest env
          result `shouldSatisfy` isRight

        it "enforces build-phase-only operations" $ \env -> do
          result <- runBuildPhaseTest env
          result `shouldSatisfy` isRight

        it "fails when build operations are attempted in eval phase" $ \env -> do
          result <- runInvalidPhaseTest env
          result `shouldSatisfy` isLeft

      describe "Proof system" $ do
        it "generates and validates type proofs" $ \env -> do
          result <- runProofGenerationTest env TypeProof
          result `shouldSatisfy` isRight

        it "generates and validates acyclic proofs" $ \env -> do
          result <- runProofGenerationTest env AcyclicProof
          result `shouldSatisfy` isRight

        it "generates and validates build proofs" $ \env -> do
          result <- runProofGenerationTest env BuildProof
          result `shouldSatisfy` isRight

        it "composes proofs correctly" $ \env -> do
          result <- runProofCompositionTest env
          result `shouldSatisfy` isRight

      describe "Deterministic builds" $ do
        it "produces identical outputs for identical inputs" $ \env -> do
          (result1, result2) <- runDeterminismTest env
          result1 `shouldSatisfy` isRight
          result2 `shouldSatisfy` isRight

          let hash1 = case result1 of
                Right (path, _) -> storeHash path
                _ -> ""
          let hash2 = case result2 of
                Right (path, _) -> storeHash path
                _ -> ""

          hash1 `shouldBe` hash2
          hash1 `shouldNotBe` ""

      describe "Build isolation" $ do
        it "correctly isolates builds in sandboxes" $ \env -> do
          result <- runSandboxTest env
          result `shouldSatisfy` isRight

        it "prevents access to unauthorized paths" $ \env -> do
          result <- runSandboxRestrictionTest env
          result `shouldSatisfy` isLeft

      describe "Dependency management" $ do
        it "correctly builds dependency graphs" $ \env -> do
          result <- runGraphBuildTest env
          result `shouldSatisfy` isRight

        it "validates graph acyclicity" $ \env -> do
          result <- runGraphValidationTest env
          result `shouldSatisfy` isRight

      describe "Garbage collection" $ do
        it "correctly identifies and collects unreachable paths" $ \env -> do
          result <- runGCTest env
          result `shouldSatisfy` isRight
          case result of
            Right stats -> do
              gcCollected stats `shouldBeGreaterThan` 0
              gcBytes stats `shouldBeGreaterThan` 0
            _ -> expectationFailure "GC failed"

        it "preserves reachable paths" $ \env -> do
          result <- runGCPreservationTest env
          result `shouldSatisfy` isRight

      describe "C compilation example" $ do
        it "successfully builds hello.c" $ \env -> do
          result <- buildHelloC env
          result `shouldSatisfy` isRight

        it "produces a verified executable output" $ \env -> do
          result <- buildAndVerifyHelloC env
          result `shouldSatisfy` isRight

        it "fails gracefully with missing input" $ \env -> do
          result <- runMissingInputTest env
          result `shouldSatisfy` isLeft

        it "reproduces identical build results" $ \env -> do
          (hash1, hash2) <- reproducibleBuildTest env
          hash1 `shouldNotBe` ""
          hash1 `shouldBe` hash2

-- | Setup test environment and return the build environment
setupTestEnv :: IO BuildEnv
setupTestEnv = do
  -- Ensure clean test directories
  let dirs = ["/tmp/ten-build", "/tmp/ten-store"]
  mapM_ removePathForcibly dirs
  mapM_ (createDirectoryIfMissing True) dirs

  -- Copy test files to known location
  writeFile "test/hello.c" helloCSrc
  copyFile "test/hello.c" "hello.c"

  -- Create and return the build environment
  return $ initBuildEnv "/tmp/ten-build" "/tmp/ten-store"

-- | Hello World C source
helloCSrc :: String
helloCSrc = "// hello.c\n#include <stdio.h>\n\nint main() {\n    printf(\"Hello from Ten!\\n\");\n    return 0;\n}\n"

-- | Test storing and retrieving content
storeAndRetrieve :: BuildEnv -> T.Text -> BS.ByteString -> IO (Either BuildError BS.ByteString)
storeAndRetrieve env name content = do
  -- Store content
  storeResult <- buildTen (addToStore name content) env
  case storeResult of
    Left err -> return $ Left err
    Right (path, _) -> do
      -- Retrieve content
      retrieveResult <- runTen (readFromStore path) env (initBuildState Build)
      case retrieveResult of
        Left err -> return $ Left err
        Right (content', _) -> return $ Right content'

-- | Test storing and verifying content hash
storeAndVerify :: BuildEnv -> T.Text -> BS.ByteString -> IO (Either BuildError Bool)
storeAndVerify env name content = do
  -- Store content
  storeResult <- buildTen (addToStore name content) env
  case storeResult of
    Left err -> return $ Left err
    Right (path, _) -> do
      -- Verify content hash
      verifyResult <- runTen (verifyStorePath path) env (initBuildState Build)
      case verifyResult of
        Left err -> return $ Left err
        Right (valid, _) -> return $ Right valid

-- | Test eval phase operations
runEvalPhaseTest :: BuildEnv -> IO (Either BuildError ())
runEvalPhaseTest env = evalTen evalPhaseOps env
  where
    evalPhaseOps :: TenM 'Eval ()
    evalPhaseOps = do
      -- Operations that should be valid in eval phase
      addProof TypeProof
      addProof AcyclicProof
      return ()

-- | Test build phase operations
runBuildPhaseTest :: BuildEnv -> IO (Either BuildError ())
runBuildPhaseTest env = buildTen buildPhaseOps env
  where
    buildPhaseOps :: TenM 'Build ()
    buildPhaseOps = do
      -- Operations that should be valid in build phase
      content <- liftIO $ BS.readFile "hello.c"
      _ <- addToStore "hello.c" content
      addProof BuildProof
      addProof OutputProof
      return ()

-- | Test invalid phase operations
runInvalidPhaseTest :: BuildEnv -> IO (Either BuildError ())
runInvalidPhaseTest env = evalTen invalidPhaseOps env
  where
    invalidPhaseOps :: TenM 'Eval ()
    invalidPhaseOps = do
      -- This is a build-phase operation, should fail in eval phase
      content <- liftIO $ BS.readFile "hello.c"
      _ <- (addToStore "hello.c" content :: TenM 'Build StorePath)
      return ()

-- | Test proof generation
runProofGenerationTest :: BuildEnv -> Proof p -> IO (Either BuildError ())
runProofGenerationTest env proofType = case proofType of
  TypeProof -> evalTen (addProof TypeProof) env
  AcyclicProof -> evalTen (addProof AcyclicProof) env
  BuildProof -> buildTen (addProof BuildProof) env
  _ -> return $ Right () -- Other proof types not tested individually

-- | Test proof composition
runProofCompositionTest :: BuildEnv -> IO (Either BuildError ())
runProofCompositionTest env = evalTen compositionTest env
  where
    compositionTest :: TenM 'Eval ()
    compositionTest = do
      -- Create and compose proofs
      let proof1 = TypeProof
          proof2 = AcyclicProof
          composed = ComposeProof proof1 proof2
      addProof composed
      return ()

-- | Test build determinism
runDeterminismTest :: BuildEnv -> IO (Either BuildError (StorePath, BuildState), Either BuildError (StorePath, BuildState))
runDeterminismTest env = do
  -- Create identical content
  let content = "determinism test content" :: BS.ByteString

  -- Run build twice with the same content
  result1 <- buildTen (addToStore "determinism.txt" content) env
  result2 <- buildTen (addToStore "determinism.txt" content) env

  return (result1, result2)

-- | Test sandbox isolation
runSandboxTest :: BuildEnv -> IO (Either BuildError Bool)
runSandboxTest env = do
  -- Create a test file to add to the store
  let content = "sandbox test" :: BS.ByteString

  -- Add to store
  storeResult <- buildTen (addToStore "sandbox-test.txt" content) env
  case storeResult of
    Left err -> return $ Left err
    Right (path, _) -> do
      -- Run a sandboxed operation
      sandboxResult <- buildTen (runSandboxedOp (Set.singleton path)) env
      return sandboxResult
  where
    runSandboxedOp :: Set.Set StorePath -> TenM 'Build Bool
    runSandboxedOp inputs = do
      -- Create sandbox configuration
      let config = defaultSandboxConfig

      -- Run in sandbox
      withSandbox inputs config $ \buildDir -> do
        -- Check that we can access inputs
        inputExists <- liftIO $ doesDirectoryExist (buildDir </> "store")
        return inputExists

-- | Test sandbox restrictions
runSandboxRestrictionTest :: BuildEnv -> IO (Either BuildError ())
runSandboxRestrictionTest env = do
  -- Create a test file to add to the store
  let content = "sandbox test" :: BS.ByteString

  -- Add to store
  storeResult <- buildTen (addToStore "sandbox-test.txt" content) env
  case storeResult of
    Left err -> return $ Left err
    Right (path, _) -> do
      -- Run a sandboxed operation that tries to access restricted paths
      buildTen (runRestrictedOp (Set.singleton path)) env
  where
    runRestrictedOp :: Set.Set StorePath -> TenM 'Build ()
    runRestrictedOp inputs = do
      -- Create sandbox configuration
      let config = defaultSandboxConfig

      -- Run in sandbox
      withSandbox inputs config $ \buildDir -> do
        -- Try to access a path outside the sandbox (should fail)
        content <- liftIO $ readFile "/etc/passwd"
        logMsg 1 $ T.pack content
        return ()

-- | Test graph building
runGraphBuildTest :: BuildEnv -> IO (Either BuildError BuildGraph)
runGraphBuildTest env = evalTen graphTest env
  where
    graphTest :: TenM 'Eval BuildGraph
    graphTest = do
      -- Create a simple derivation
      let builder = StorePath "test-builder-hash" "test-builder"
          inputs = Set.empty
          outputs = Set.singleton "test-output"
          args = []
          env = Map.empty

      deriv <- mkDerivation "test-deriv" builder args inputs outputs env

      -- Create graph from derivation
      let outputPaths = Set.map outputPath (derivOutputs deriv)
      createBuildGraph outputPaths (Set.singleton deriv)

-- | Test graph validation
runGraphValidationTest :: BuildEnv -> IO (Either BuildError GraphProof)
runGraphValidationTest env = evalTen graphValidationTest env
  where
    graphValidationTest :: TenM 'Eval GraphProof
    graphValidationTest = do
      -- Create a simple derivation
      let builder = StorePath "test-builder-hash" "test-builder"
          inputs = Set.empty
          outputs = Set.singleton "test-output"
          args = []
          env = Map.empty

      deriv <- mkDerivation "test-deriv" builder args inputs outputs env

      -- Create graph from derivation
      let outputPaths = Set.map outputPath (derivOutputs deriv)
      graph <- createBuildGraph outputPaths (Set.singleton deriv)

      -- Validate the graph
      validateGraph graph

-- | Test garbage collection
runGCTest :: BuildEnv -> IO (Either BuildError GCStats)
runGCTest env = do
  -- Add some content to the store
  _ <- buildTen (addToStore "gc-test1.txt" "gc test 1") env
  _ <- buildTen (addToStore "gc-test2.txt" "gc test 2") env

  -- Add a root for one path
  rootResult <- buildTen (addToStore "gc-root.txt" "gc root") env
  case rootResult of
    Left err -> return $ Left err
    Right (path, _) -> do
      -- Add root
      _ <- runTen (addRoot path "test-root" False) env (initBuildState Build)

      -- Run garbage collection
      runTen collectGarbage env (initBuildState Build) >>= \case
        Left err -> return $ Left err
        Right (stats, _) -> return $ Right stats

-- | Test garbage collection preservation
runGCPreservationTest :: BuildEnv -> IO (Either BuildError Bool)
runGCPreservationTest env = do
  -- Add content to the store
  rootResult <- buildTen (addToStore "gc-preserve.txt" "gc preserve test") env
  case rootResult of
    Left err -> return $ Left err
    Right (path, _) -> do
      -- Add root
      _ <- runTen (addRoot path "preserve-root" False) env (initBuildState Build)

      -- Run garbage collection
      _ <- runTen collectGarbage env (initBuildState Build)

      -- Check if the path still exists
      runTen (storePathExists path) env (initBuildState Build) >>= \case
        Left err -> return $ Left err
        Right (exists, _) -> return $ Right exists

-- | Helper to build hello.c
buildHelloC :: BuildEnv -> IO (Either BuildError BuildResult)
buildHelloC env = do
  -- Create a derivation for hello.c
  createResult <- createHelloDerivation env
  case createResult of
    Left err -> return $ Left err
    Right (deriv, _) -> do
      -- Build the derivation
      buildResult <- buildTen (buildDerivation deriv) env
      case buildResult of
        Left err -> return $ Left err
        Right (result, _) -> return $ Right result

-- | Helper to build and verify hello.c
buildAndVerifyHelloC :: BuildEnv -> IO (Either BuildError Bool)
buildAndVerifyHelloC env = do
  -- Build hello.c
  buildResult <- buildHelloC env
  case buildResult of
    Left err -> return $ Left err
    Right result -> do
      -- Get the derivation
      createResult <- createHelloDerivation env
      case createResult of
        Left err -> return $ Left err
        Right (deriv, _) -> do
          -- Verify the build result
          verifyResult <- buildTen (verifyBuildResult deriv result) env
          case verifyResult of
            Left err -> return $ Left err
            Right (valid, _) -> return $ Right valid

-- | Helper to test missing input
runMissingInputTest :: BuildEnv -> IO (Either BuildError BuildResult)
runMissingInputTest env = do
  -- Create a derivation with a nonexistent input
  let nonexistentPath = StorePath "nonexistent" "nonexistent.c"
      builder = StorePath "hello-builder-hash" "hello-builder"
      inputs = Set.singleton $ DerivationInput nonexistentPath "nonexistent.c"
      outputs = Set.singleton "hello"
      args = ["nonexistent.c"]
      dEnv = Map.empty

  -- Create derivation
  createResult <- evalTen (mkDerivation "hello" builder args inputs outputs dEnv) env
  case createResult of
    Left err -> return $ Left err
    Right (deriv, _) -> do
      -- Try to build (should fail)
      buildResult <- buildTen (buildDerivation deriv) env
      case buildResult of
        Left err -> return $ Left err
        Right (result, _) -> return $ Right result

-- | Test reproducible builds
reproducibleBuildTest :: BuildEnv -> IO (T.Text, T.Text)
reproducibleBuildTest env = do
  -- Build hello.c twice
  result1 <- buildHelloC env
  result2 <- buildHelloC env

  -- Extract hashes
  let hash1 = case result1 of
        Right br ->
          let outputs = Set.toList (resultOutputs br)
          in if null outputs then "" else storeHash (head outputs)
        _ -> ""

  let hash2 = case result2 of
        Right br ->
          let outputs = Set.toList (resultOutputs br)
          in if null outputs then "" else storeHash (head outputs)
        _ -> ""

  return (hash1, hash2)

-- | Create a hello.c derivation
createHelloDerivation :: BuildEnv -> IO (Either BuildError (Derivation, BuildState))
createHelloDerivation env = do
  -- Add hello.c to the store
  content <- BS.readFile "hello.c"
  sourceResult <- buildTen (addToStore "hello.c" content) env

  -- Add a builder to the store
  builderContent <- BS.pack <$> readFile =<< getGccPath
  builderResult <- buildTen (addToStore "gcc" builderContent) env

  case (sourceResult, builderResult) of
    (Right (sourcePath, _), Right (builderPath, _)) -> do
      -- Create the derivation
      let inputs = Set.singleton $ DerivationInput sourcePath "hello.c"
          outputs = Set.singleton "hello"
          args = ["-c", "hello.c", "-o", "$TEN_OUT/hello"]
          dEnv = Map.empty

      evalTen (mkDerivation "hello" builderPath args inputs outputs dEnv) env

    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err

-- | Get gcc path for testing
getGccPath :: IO FilePath
getGccPath = do
  -- Find gcc in the system
  (exitCode, gccPath, _) <- readProcessWithExitCode "which" ["gcc"] ""
  case exitCode of
    ExitSuccess -> return $ init gccPath  -- Remove trailing newline
    _ -> return "/usr/bin/gcc"  -- Default fallback

-- | Custom shouldBeGreaterThan for numbers
shouldBeGreaterThan :: (Show a, Ord a) => a -> a -> Expectation
shouldBeGreaterThan actual expected =
  if actual > expected
    then return ()
    else expectationFailure $
      "Expected " ++ show actual ++ " to be greater than " ++ show expected
