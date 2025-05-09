{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Ten.Verification (
    -- Core verification types
    BuildProof(..),
    VerificationError(..),

    -- Verification functions
    verifyStoreIntegrity,
    verifyDerivation,
    verifyBuildResult,
    verifyDependencyGraph,

    -- Build property enforcement
    enforceReproducibility,
    enforceSandboxIsolation,

    -- Verification context
    VerificationContext,
    newVerificationContext,
    withVerifiedGraph
) where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as BS
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Control.Monad.Except

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Graph

-- | Concrete proofs for build system properties
data BuildProof where
    -- Store integrity proofs
    ContentHashProof :: StorePath -> Crypto.Digest Crypto.SHA256 -> BuildProof
    PathImmutabilityProof :: StorePath -> BuildProof

    -- Derivation proofs
    DerivationProof :: Derivation -> BuildProof
    InputAvailabilityProof :: Set StorePath -> BuildProof

    -- Build result proofs
    BuildResultProof :: Derivation -> BuildResult -> BuildProof
    ReproducibilityProof :: StorePath -> StorePath -> BuildProof -- Two identical builds

    -- Graph proofs
    AcyclicGraphProof :: BuildGraph -> BuildProof
    ClosureCompletenessProof :: BuildGraph -> BuildProof

    -- Sandbox proofs
    SandboxIsolationProof :: FilePath -> Set StorePath -> BuildProof
    NetworkIsolationProof :: BuildProof

-- | Specific verification errors
data VerificationError
    = HashMismatch StorePath Crypto.Digest Crypto.SHA256 Crypto.Digest Crypto.SHA256
    | MissingInput StorePath
    | PathModified StorePath
    | CyclicDependency [StorePath]
    | IncompleteClosure Set StorePath
    | SandboxViolation Text
    | EnvironmentLeakage Text
    | BuildNonDeterminism StorePath StorePath
    deriving (Show)

-- | Context for verification operations
data VerificationContext = VerificationContext
    { verifiedPaths :: Set StorePath
    , verifiedDerivations :: Set Derivation
    , verifiedBuilds :: Map Derivation BuildResult
    , verifiedGraphs :: Set BuildGraph
    }

-- | Create a new verification context
newVerificationContext :: VerificationContext
newVerificationContext = VerificationContext
    { verifiedPaths = Set.empty
    , verifiedDerivations = Set.empty
    , verifiedBuilds = Map.empty
    , verifiedGraphs = Set.empty
    }

-- | Verify store path integrity by checking content hash
verifyStoreIntegrity :: StorePath -> TenM p (Either VerificationError BuildProof)
verifyStoreIntegrity path = do
    -- Read content from store
    content <- readFromStore path

    -- Compute actual hash
    let actualHash = Crypto.hash content :: Crypto.Digest Crypto.SHA256

    -- Compare with expected hash (from path)
    let expectedHashText = storeHash path
    let expectedHash = readHash expectedHashText

    -- Verify match
    if actualHash == expectedHash
        then return $ Right $ ContentHashProof path actualHash
        else return $ Left $ HashMismatch path expectedHash actualHash
  where
    readHash :: Text -> Crypto.Digest Crypto.SHA256
    readHash = undefined -- Convert Text to Hash in real implementation

-- | Verify a derivation has all its inputs available and correct
verifyDerivation :: Derivation -> TenM p (Either VerificationError BuildProof)
verifyDerivation deriv = do
    -- Check all inputs exist and have correct hashes
    let inputPaths = Set.map inputPath (derivInputs deriv)

    -- Check each input
    results <- mapM verifyStoreIntegrity (Set.toList inputPaths)

    -- If any input verification failed, return the first error
    let errors = [err | Left err <- results]
    if not (null errors)
        then return $ Left (head errors)
        else return $ Right $ InputAvailabilityProof inputPaths

-- | Verify build result matches derivation expectations
verifyBuildResult :: Derivation -> BuildResult -> TenM p (Either VerificationError BuildProof)
verifyBuildResult deriv result = do
    -- Verify all expected outputs are present
    let expectedOutputPaths = Set.map outputPath (derivOutputs deriv)
    let actualOutputPaths = resultOutputs result

    -- Check for missing outputs
    let missingOutputs = expectedOutputPaths `Set.difference` actualOutputPaths
    if not (Set.null missingOutputs)
        then return $ Left $ MissingInput (Set.findMin missingOutputs)
        else do
            -- Verify all outputs have correct hashes
            outputResults <- mapM verifyStoreIntegrity (Set.toList actualOutputPaths)

            -- If any output verification failed, return the first error
            let errors = [err | Left err <- outputResults]
            if not (null errors)
                then return $ Left (head errors)
                else return $ Right $ BuildResultProof deriv result

-- | Verify dependency graph is acyclic and complete
verifyDependencyGraph :: BuildGraph -> TenM p (Either VerificationError BuildProof)
verifyDependencyGraph graph = do
    -- Check for cycles using standard graph algorithm
    cycles <- findCycles graph
    if not (null cycles)
        then return $ Left $ CyclicDependency (head cycles)
        else do
            -- Check for completeness (all references are in the graph)
            complete <- checkGraphCompleteness graph
            if not complete
                then return $ Left $ IncompleteClosure Set.empty -- Would include missing deps
                else return $ Right $ AcyclicGraphProof graph

-- | Enforce build reproducibility by building twice and comparing
enforceReproducibility :: Derivation -> TenM 'Build (Either VerificationError BuildProof)
enforceReproducibility deriv = do
    -- Build once
    result1 <- buildDerivation deriv

    -- Build again with clean environment
    result2 <- withCleanBuildEnv $ buildDerivation deriv

    -- Compare results
    if resultOutputs result1 /= resultOutputs result2
        then do
            -- Find the first differing output
            let outputs1 = resultOutputs result1
            let outputs2 = resultOutputs result2
            let differing = head $ Set.toList $
                  Set.filter (\p -> not $ Set.member p outputs2) outputs1

            -- Find matching output in result2 with same name
            let name = storeName differing
            let matching = Set.filter (\p -> storeName p == name) outputs2

            case Set.lookupMin matching of
                Just differentPath ->
                    return $ Left $ BuildNonDeterminism differing differentPath
                Nothing ->
                    -- Shouldn't happen if outputs have same structure
                    return $ Left $ BuildNonDeterminism differing differing
        else
            -- Builds match - create reproducibility proof
            let someOutput = Set.findMin (resultOutputs result1)
            return $ Right $ ReproducibilityProof someOutput someOutput

-- | Enforce sandbox isolation
enforceSandboxIsolation :: FilePath -> Set StorePath -> TenM 'Build (Either VerificationError BuildProof)
enforceSandboxIsolation sandboxDir allowedPaths = do
    -- Check sandbox directory structure
    valid <- verifySandboxStructure sandboxDir

    if not valid
        then return $ Left $ SandboxViolation "Invalid sandbox structure"
        else do
            -- Verify only allowed paths are accessible
            breaches <- findIsolationBreaches sandboxDir allowedPaths

            if not (null breaches)
                then return $ Left $ SandboxViolation (head breaches)
                else return $ Right $ SandboxIsolationProof sandboxDir allowedPaths

-- | Run an action with a verified dependency graph
withVerifiedGraph :: BuildGraph -> (BuildProof -> TenM p a) -> TenM p (Either VerificationError a)
withVerifiedGraph graph action = do
    -- First verify the graph
    graphProofResult <- verifyDependencyGraph graph

    case graphProofResult of
        Left err -> return $ Left err
        Right proof -> do
            -- Run the action with the proof
            result <- action proof
            return $ Right result

-- | Helper functions (would be implemented in real code)
findCycles :: BuildGraph -> TenM p [[StorePath]]
findCycles = undefined

checkGraphCompleteness :: BuildGraph -> TenM p Bool
checkGraphCompleteness = undefined

verifySandboxStructure :: FilePath -> TenM 'Build Bool
verifySandboxStructure = undefined

findIsolationBreaches :: FilePath -> Set StorePath -> TenM 'Build [Text]
findIsolationBreaches = undefined

withCleanBuildEnv :: TenM 'Build a -> TenM 'Build a
withCleanBuildEnv = undefined
