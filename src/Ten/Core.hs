{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ten.Core where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Process
import System.Exit

-- | Build phases for type-level separation between evaluation and execution
data Phase = Eval | Build

-- | Core error types
data BuildError
    = EvalError Text           -- Errors during evaluation phase
    | BuildFailed Text         -- Errors during build phase
    | StoreError Text          -- Errors interacting with the store
    | SandboxError Text        -- Errors in sandbox creation/management
    | InputNotFound FilePath   -- Input file missing
    | HashError Text           -- Error computing hash
    | GraphError Text          -- Error in dependency graph
    | ResourceError Text       -- Error with system resources
    deriving (Show, Eq)

-- | Store path representing a content-addressed location
data StorePath = StorePath
    { storeHash :: Text      -- Hash part of the path
    , storeName :: Text      -- Human-readable name component
    } deriving (Show, Eq, Ord)

-- | Proof of a property, parameterized by phase
data Proof (p :: Phase) where
    -- Proofs for evaluation phase
    TypeProof      :: Proof 'Eval
    AcyclicProof   :: Proof 'Eval
    EvalCompleteProof :: Proof 'Eval

    -- Proofs for build phase
    InputProof     :: Proof 'Build
    BuildProof     :: Proof 'Build
    OutputProof    :: Proof 'Build

    -- Composite proofs
    ComposeProof   :: Proof p -> Proof p -> Proof p

-- Add standalone deriving instances for Proof
deriving instance Show (Proof p)
deriving instance Eq (Proof p)

-- | Environment for build operations
data BuildEnv = BuildEnv
    { workDir :: FilePath       -- Temporary build directory
    , storePath :: FilePath     -- Root of content-addressed store
    , verbosity :: Int          -- Logging verbosity level
    , allowedPaths :: Set FilePath -- Paths accessible during build
    } deriving (Show, Eq)

-- | State carried through build operations
data BuildState = BuildState
    { currentPhase :: Phase       -- Current execution phase
    , buildProofs :: [Proof 'Build] -- Accumulated proofs
    , buildInputs :: Set StorePath  -- Input paths for current build
    , buildOutputs :: Set StorePath -- Output paths for current build
    } deriving (Show)

-- | The core monad for all Ten operations
newtype TenM (p :: Phase) a = TenM
    { runTenM :: ReaderT BuildEnv (StateT BuildState (ExceptT BuildError IO)) a }
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadReader BuildEnv
        , MonadState BuildState
        , MonadError BuildError
        , MonadIO
        )

-- | Initialize the build environment
initBuildEnv :: FilePath -> FilePath -> BuildEnv
initBuildEnv wd sp = BuildEnv
    { workDir = wd
    , storePath = sp
    , verbosity = 1
    , allowedPaths = Set.empty
    }

-- | Initialize build state for a given phase
initBuildState :: Phase -> BuildState
initBuildState phase = BuildState
    { currentPhase = phase
    , buildProofs = []
    , buildInputs = Set.empty
    , buildOutputs = Set.empty
    }

-- | Execute a Ten monad in the given environment and state
runTen :: TenM p a -> BuildEnv -> BuildState -> IO (Either BuildError (a, BuildState))
runTen m env state = runExceptT $ runStateT (runReaderT (runTenM m) env) state

-- | Execute an evaluation-phase computation
evalTen :: TenM 'Eval a -> BuildEnv -> IO (Either BuildError (a, BuildState))
evalTen m env = runTen m env (initBuildState Eval)

-- | Execute a build-phase computation
buildTen :: TenM 'Build a -> BuildEnv -> IO (Either BuildError (a, BuildState))
buildTen m env = runTen m env (initBuildState Build)

-- | Convert a store path to a filesystem path
storePathToFilePath :: StorePath -> BuildEnv -> FilePath
storePathToFilePath sp env = storePath env </> T.unpack (storeHash sp) ++ "-" ++ T.unpack (storeName sp)

-- | Logging function
logMsg :: Int -> Text -> TenM p ()
logMsg level msg = do
    v <- asks verbosity
    when (v >= level) $ liftIO $ putStrLn $ T.unpack msg

-- | Record a proof in the build state
addProof :: Proof p -> TenM p ()
addProof proof = modify $ \s ->
    case proof of
        p@(BuildProof {}) -> s { buildProofs = p : buildProofs s }
        _ -> s  -- Only track build proofs in state for now

-- | Assert that a condition holds, or throw an error
assertTen :: Bool -> BuildError -> TenM p ()
assertTen condition err = unless condition $ throwError err

-- | Operations only allowed in evaluation phase
class EvalPhase (p :: Phase) where
    evalOnly :: TenM 'Eval a -> TenM p a

-- Only actual evaluation phase can run evaluation operations
instance EvalPhase 'Eval where
    evalOnly = id

-- | Operations only allowed in build phase
class BuildPhase (p :: Phase) where
    buildOnly :: TenM 'Build a -> TenM p a

-- Only actual build phase can run build operations
instance BuildPhase 'Build where
    buildOnly = id
