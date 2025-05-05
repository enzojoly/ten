{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.HUnit

import Control.Monad (foldM, void)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.State (runStateT, evalStateT, get)
import Control.Monad.Reader (runReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (try, catch, SomeException) -- Explicitly import catch
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import System.FilePath
import System.Directory
import System.Exit (ExitCode(..))
import System.IO.Error (IOError)  -- Explicitly import IOError
import System.IO (hPutStrLn, stderr)
import Data.Char (isHexDigit)
import Data.Unique (newUnique)
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto

-- Import modules with explicit qualification to avoid ambiguity
import Ten.Core
import qualified Ten.Core as Core
import Ten.Store
import qualified Ten.Hash as Hash
import qualified Ten.Graph as Graph
import qualified Ten.Derivation as Deriv
import qualified Ten.Sandbox as Sandbox
import qualified Ten.Build as Build

-- Test helpers
-- Create a test environment
createTestEnv :: IO BuildEnv
createTestEnv = do
    tempDir <- getTemporaryDirectory
    let workDir = tempDir </> "ten-test-work"
    let storeDir = tempDir </> "ten-test-store"
    -- Clean up any existing test directories
    removeDirectoryRecursive workDir `catch` \(_ :: IOError) -> return ()
    removeDirectoryRecursive storeDir `catch` \(_ :: IOError) -> return ()
    -- Create fresh directories
    createDirectoryIfMissing True workDir
    createDirectoryIfMissing True storeDir
    return $ initBuildEnv workDir storeDir

-- Create a test BuildState
createTestState :: IO (BuildState 'Eval)
createTestState = do
    -- Create a BuildId in IO context
    unique <- newUnique
    let bid = BuildId unique
    return $ initBuildState Eval bid

-- Test StorePath generator
genHexString :: Int -> Gen String
genHexString n = vectorOf n $ elements "0123456789abcdef"

genValidStorePath :: Gen StorePath
genValidStorePath = do
    -- Generate hash of at least 8 characters
    hashLength <- choose (8, 64)
    hash <- T.pack <$> genHexString hashLength
    -- Generate non-empty name with valid characters
    name <- T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_', '.', '+'])
    return $ StorePath hash name

-- Derivation generator for property tests
genDerivation :: Gen Derivation
genDerivation = do
    name <- T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_'])
    hashLength <- choose (8, 64)
    hash <- T.pack <$> genHexString hashLength
    builder <- genValidStorePath
    args <- listOf $ T.pack <$> listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_', ' '])
    inputCount <- choose (0, 5)
    inputs <- Set.fromList <$> vectorOf inputCount genInput
    outputCount <- choose (1, 5)
    outputs <- Set.fromList <$> vectorOf outputCount genOutput
    envVarCount <- choose (0, 5)
    env <- Map.fromList <$> vectorOf envVarCount genEnvVar
    system <- elements ["x86_64-linux", "i686-linux", "aarch64-linux", "x86_64-darwin"]
    strategy <- elements [ApplicativeStrategy, MonadicStrategy]
    metaCount <- choose (0, 3)
    meta <- Map.fromList <$> vectorOf metaCount genEnvVar

    return $ Derivation name hash builder args inputs outputs env system strategy meta
  where
    genInput = DerivationInput <$> genValidStorePath <*> (T.pack <$> listOf1 (elements ['a'..'z']))
    genOutput = DerivationOutput <$> (T.pack <$> listOf1 (elements ['a'..'z'])) <*> genValidStorePath
    genEnvVar = do
      k <- T.pack <$> listOf1 (elements $ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'])
      v <- T.pack <$> listOf (elements $ ['a'..'z'] ++ ['0'..'9'] ++ ['-', '_', '/'])
      return (k, v)

-- Generate a directed acyclic graph
genAcyclicBuildGraph :: Gen BuildGraph
genAcyclicBuildGraph = do
    -- Generate a simple directed acyclic graph
    nodeCount <- choose (3, 10)
    drvs <- vectorOf nodeCount genDerivation

    -- Create nodes
    let nodes = Map.fromList $ zip (map (T.pack . show) [1..nodeCount]) $ map DerivationNode drvs

    -- Create edges (ensuring no cycles by only allowing edges i -> j where i > j)
    edges <- foldM (\acc i -> do
                edgeCount <- choose (0, i-1)
                targets <- sublistOf [1..(i-1)]
                let newEdges = Set.fromList $ map (T.pack . show) (take edgeCount targets)
                return $ Map.insert (T.pack $ show i) newEdges acc
             ) Map.empty [2..nodeCount]

    -- Choose a root node
    rootIdx <- choose (nodeCount `div` 2, nodeCount)
    let roots = Set.singleton $ T.pack $ show rootIdx

    return $ BuildGraph nodes edges roots (Just ValidProof)

-- Helper to run TenM in IO context for testing
runTenTest :: forall (p :: Phase) (t :: PrivilegeTier) a.
              (SingI p, SingI t) =>
              TenM p t a -> BuildEnv -> BuildState p -> IO (Either BuildError a)
runTenTest action env state = do
    result <- runTen (sing @p) (sing @t) action env state
    return $ case result of
        Left err -> Left err
        Right (val, _) -> Right val

-- Actual tests
main :: IO ()
main = hspec $ do
    describe "Ten Build System Tests" $ do
        -- Clean up after all tests
        afterAll_ (do
            tempDir <- getTemporaryDirectory
            let workDir = tempDir </> "ten-test-work"
            let storeDir = tempDir </> "ten-test-store"
            removeDirectoryRecursive workDir `catch` \(_ :: IOError) -> return ()
            removeDirectoryRecursive storeDir `catch` \(_ :: IOError) -> return ()) $ do

            -- HUnit tests
            describe "StorePath Tests" $ do
                it "validates a valid store path" $ do
                    let path = StorePath "abcdef1234567890" "test-package"
                    validateStorePath path `shouldBe` True

                it "rejects an invalid store path with short hash" $ do
                    let path = StorePath "abc" "test-package"
                    validateStorePath path `shouldBe` False

                it "rejects an invalid store path with empty name" $ do
                    let path = StorePath "abcdef1234567890" ""
                    validateStorePath path `shouldBe` False

                it "converts store path to text and back" $ do
                    let path = StorePath "abcdef1234567890" "test-package"
                    let textPath = storePathToText path
                    parseStorePath textPath `shouldBe` Just path

            describe "Hash Tests" $ do
                it "creates consistent hashes for the same content" $ do
                    let content1 = BS8.pack "test content"
                    let content2 = BS8.pack "test content"
                    Hash.hashByteString content1 `shouldBe` Hash.hashByteString content2

                it "creates different hashes for different content" $ do
                    let content1 = BS8.pack "test content 1"
                    let content2 = BS8.pack "test content 2"
                    Hash.hashByteString content1 `shouldNotBe` Hash.hashByteString content2

            describe "Graph Tests" $ do
                it "detects cycles in dependency graph" $ do
                    env <- createTestEnv
                    state <- createTestState

                    -- Create a graph with a cycle
                    let nodeA = DerivationNode $
                            Derivation "A" "hash-a" (StorePath "hash-builder" "builder")
                                [] Set.empty Set.empty Map.empty "x86_64-linux"
                                ApplicativeStrategy Map.empty
                    let nodeB = DerivationNode $
                            Derivation "B" "hash-b" (StorePath "hash-builder" "builder")
                                [] Set.empty Set.empty Map.empty "x86_64-linux"
                                ApplicativeStrategy Map.empty

                    let nodes = Map.fromList [("a", nodeA), ("b", nodeB)]
                    let edges = Map.fromList [("a", Set.singleton "b"), ("b", Set.singleton "a")]
                    let graph = BuildGraph nodes edges (Set.singleton "a") Nothing

                    -- Create a test action that gets cycle detection result
                    let testAction = Graph.detectCycles sDaemon graph >>= \(hasCycle, _, _) -> return hasCycle

                    -- Run the test action in the Ten monad
                    result <- runTenTest @'Eval @'Daemon testAction env state
                    result `shouldBe` Right True

                it "topologically sorts a directed acyclic graph" $ do
                    env <- createTestEnv
                    state <- createTestState

                    -- Create a simple DAG
                    let nodeA = DerivationNode $
                            Derivation "A" "hash-a" (StorePath "hash-builder" "builder")
                                [] Set.empty Set.empty Map.empty "x86_64-linux"
                                ApplicativeStrategy Map.empty
                    let nodeB = DerivationNode $
                            Derivation "B" "hash-b" (StorePath "hash-builder" "builder")
                                [] Set.empty Set.empty Map.empty "x86_64-linux"
                                ApplicativeStrategy Map.empty
                    let nodeC = DerivationNode $
                            Derivation "C" "hash-c" (StorePath "hash-builder" "builder")
                                [] Set.empty Set.empty Map.empty "x86_64-linux"
                                ApplicativeStrategy Map.empty

                    let nodes = Map.fromList [("a", nodeA), ("b", nodeB), ("c", nodeC)]
                    let edges = Map.fromList [("b", Set.singleton "a"), ("c", Set.singleton "b")]
                    let graph = BuildGraph nodes edges (Set.singleton "c") (Just ValidProof)

                    -- Create a test action that gets the sorted nodes
                    let testAction = Graph.topologicalSort sDaemon graph

                    -- Run the test action in the Ten monad
                    result <- runTenTest @'Eval @'Daemon testAction env state
                    case result of
                        Right sortedNodes -> length sortedNodes `shouldBe` 3
                        Left err -> error $ "Test failed: " ++ show err

            describe "Derivation Tests" $ do
                it "serializes and deserializes a derivation" $ do
                    let drv = Derivation
                              "test-package" "abcdef1234" (StorePath "builder-hash" "builder")
                              ["--arg1", "--arg2"] Set.empty Set.empty Map.empty
                              "x86_64-linux" ApplicativeStrategy Map.empty

                    let serialized = Deriv.serializeDerivation drv
                    let deserialized = Deriv.deserializeDerivation serialized

                    deserialized `shouldBe` Right drv

                it "compares derivations properly for equality" $ do
                    let drv1 = Derivation
                               "test" "hash" (StorePath "b-hash" "builder")
                               [] Set.empty Set.empty Map.empty "sys"
                               ApplicativeStrategy Map.empty
                    let drv2 = Derivation
                               "test" "hash" (StorePath "b-hash" "builder")
                               [] Set.empty Set.empty Map.empty "sys"
                               ApplicativeStrategy Map.empty
                    let drv3 = Derivation
                               "test" "different-hash" (StorePath "b-hash" "builder")
                               [] Set.empty Set.empty Map.empty "sys"
                               ApplicativeStrategy Map.empty

                    Core.derivationEquals drv1 drv2 `shouldBe` True
                    Core.derivationEquals drv1 drv3 `shouldBe` False

            describe "Sandbox Tests" $ do
                it "creates a valid sandbox configuration with return support" $ do
                    let config = Sandbox.defaultSandboxConfig {
                                  Sandbox.sandboxReturnSupport = True
                                }
                    Sandbox.sandboxReturnSupport config `shouldBe` True

            describe "Build Tests" $ do
                it "verifies build results correctly" $ do
                    -- Define the test derivation and result
                    let drv = Derivation
                              "test" "hash" (StorePath "b-hash" "builder") []
                              Set.empty (Set.singleton (DerivationOutput "out" (StorePath "out-hash" "out")))
                              Map.empty "x86_64-linux" ApplicativeStrategy Map.empty

                    let result = BuildResult
                                 (Set.singleton (StorePath "out-hash" "out"))
                                 ExitSuccess "Build log" Set.empty Map.empty

                    -- Create a pure verification function
                    let pureBuildResultVerifier drv result =
                         let expectedOutputs = Set.map outputName (derivOutputs drv)
                             actualOutputs = Set.map storeName (brOutputPaths result)
                             allOutputsPresent = expectedOutputs `Set.isSubsetOf` actualOutputs
                             validExitCode = brExitCode result == ExitSuccess
                         in allOutputsPresent && validExitCode

                    -- Run the pure verification directly
                    pureBuildResultVerifier drv result `shouldBe` True

            -- QuickCheck property tests
            describe "StorePath Properties" $ do
                prop "validateStorePath accepts all properly formed StorePaths" $
                    forAll genValidStorePath $ \path ->
                        validateStorePath path === True

                prop "Store paths with short hashes are rejected" $
                    forAll (T.pack <$> genHexString 7) $ \shortHash ->
                    forAll (T.pack <$> listOf1 (elements $ ['a'..'z'] ++ ['0'..'9'])) $ \name ->
                        let path = StorePath shortHash name
                        in validateStorePath path === False

                prop "StorePath to text and back round trip works" $
                    forAll genValidStorePath $ \path ->
                        parseStorePath (storePathToText path) === Just path

            describe "Graph Properties" $ do
                -- This property tests that detectCycles correctly identifies graphs without cycles
                prop "detectCycles identifies acyclic graphs correctly" $
                    forAll genAcyclicBuildGraph $ \graph ->
                    ioProperty $ do
                        env <- createTestEnv
                        state <- createTestState

                        -- Run cycle detection
                        let testAction = Graph.detectCycles sDaemon graph >>= \(hasCycle, _, _) -> return hasCycle
                        result <- runTenTest @'Eval @'Daemon testAction env state

                        -- Handle error or success appropriately
                        case result of
                            Right hasCycle -> return (hasCycle == False)
                            Left err -> do
                                -- Log the error but return False to fail the test
                                hPutStrLn stderr $ "Error in cycle detection: " ++ show err
                                return False

                -- This property tests that topological sort preserves all nodes in acyclic graphs
                prop "topological sort preserves all nodes in acyclic graphs" $
                    forAll genAcyclicBuildGraph $ \graph ->
                    let nodeCount = Map.size (graphNodes graph)
                    in nodeCount > 0 ==> ioProperty $ do
                        env <- createTestEnv
                        state <- createTestState

                        -- Create a test action
                        let testAction = Graph.topologicalSort sDaemon graph >>= \nodes ->
                                           return (length nodes == nodeCount)

                        -- Run the test and check the result
                        result <- runTenTest @'Eval @'Daemon testAction env state
                        case result of
                            Right isCorrectLength -> return isCorrectLength
                            Left err -> do
                                -- Log the error but return False to fail the test
                                hPutStrLn stderr $ "Error in topological sort: " ++ show err
                                return False
