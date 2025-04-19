{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ten.Core (
    -- Core types
    Phase(..),
    TenM(..),
    BuildEnv(..),
    BuildState(..),
    BuildError(..),
    BuildId(..),
    BuildStatus(..),
    BuildStrategy(..),
    RunMode(..),
    DaemonConfig(..),
    UserCredentials(..),
    UserId(..),
    AuthToken(..),

    -- Return-Continuation types
    PhaseTransition(..),
    transitionPhase,

    -- Store types
    StorePath(..),

    -- Re-exports from Ten.Store (removed redundant local definitions)
    -- storePathToFilePath is now imported from Ten.Store

    -- Core derivation types
    Derivation(..),
    DerivationInput(..),
    DerivationOutput(..),
    derivationEquals,
    derivationPathsEqual,

    -- Graph types
    BuildGraph(..),
    BuildNode(..),
    GraphError(..),
    GraphProof(..),

    -- Proof system
    Proof(..),
    addProof,

    -- Environment/state handling
    initBuildEnv,
    initClientEnv,
    initDaemonEnv,
    initBuildState,

    -- Monad operations
    runTen,
    evalTen,
    buildTen,
    logMsg,
    assertTen,

    -- Type class constraints
    EvalPhase(..),
    BuildPhase(..),

    -- Build chain handling
    newBuildId,
    setCurrentBuildId,
    addToDerivationChain,
    isInDerivationChain,

    -- STM operations
    atomicallyTen,

    -- Daemon operations
    withDaemon,
    isDaemonMode,
    isClientMode,

    -- Linux-specific operations
    dropPrivileges,
    getSystemUser,
    getSystemGroup
) where

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Unique (Unique, newUnique, hashUnique)
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
import Data.Unique (Unique, newUnique)
import Network.Socket (Socket)
import System.IO (Handle)
import Data.Maybe (isJust)
import System.Posix.User (getUserEntryForName, getGroupEntryForName,
                         setUserID, setGroupID, getEffectiveUserID,
                         userID, groupID)
import System.Posix.Types (UserID, GroupID)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (bracket, try, SomeException, Exception)
import System.Environment (lookupEnv)
import System.Posix.Types (ProcessID)
import Text.Read (readPrec)
import qualified Text.Read as Read
import System.IO.Unsafe (unsafePerformIO)

-- | Build phases for type-level separation between evaluation and execution
data Phase = Eval | Build
    deriving (Show, Eq)

-- | Build identifier type
data BuildId
    = BuildId Unique               -- Normal constructor
    | BuildIdFromInt Int           -- For deserialization only
    deriving (Eq, Ord)

instance Show BuildId where
    show (BuildId u) = "build-" ++ show (hashUnique u)
    show (BuildIdFromInt n) = "build-" ++ show n

-- | Read instance for BuildId to support deserialization
instance Read BuildId where
    readPrec = do
        Read.Ident s <- Read.lexP
        case stripPrefix "build-" s of
            Just numStr ->
                case reads numStr of
                    [(n, "")] -> return $ BuildIdFromInt n
                    _ -> Read.pfail
            Nothing -> Read.pfail

-- Helper function to strip prefix (like Data.List.stripPrefix)
stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
    | x == y = stripPrefix xs ys
    | otherwise = Nothing
stripPrefix _ _ = Nothing

-- | Core error types
data BuildError
    = EvalError Text                     -- Errors during evaluation phase
    | BuildFailed Text                   -- Errors during build phase
    | StoreError Text                    -- Errors interacting with the store
    | SandboxError Text                  -- Errors in sandbox creation/management
    | InputNotFound FilePath             -- Input file missing
    | HashError Text                     -- Error computing hash
    | GraphError Text                    -- Error in dependency graph
    | ResourceError Text                 -- Error with system resources
    | DaemonError Text                   -- Errors in daemon communication
    | AuthError Text                     -- Authentication/Authorization errors
    | CyclicDependency Text              -- Cyclic dependency detected
    | SerializationError Text            -- Error serializing/deserializing data
    | RecursionLimit Text                -- Too many recursive derivations
    | NetworkError Text                  -- Network-related errors
    | ParseError Text                    -- Parsing errors
    deriving (Show, Eq)

instance Exception BuildError

-- | Store path representing a content-addressed location
data StorePath = StorePath
    { storeHash :: Text                  -- Hash part of the path
    , storeName :: Text                  -- Human-readable name component
    } deriving (Show, Eq, Ord)

-- | Input to a derivation
data DerivationInput = DerivationInput
    { inputPath :: StorePath             -- Path in the store
    , inputName :: Text                  -- Name to expose this input as
    } deriving (Show, Eq, Ord)

-- | Expected output from a derivation
data DerivationOutput = DerivationOutput
    { outputName :: Text                 -- Name of the output
    , outputPath :: StorePath            -- Where it will be stored
    } deriving (Show, Eq, Ord)

-- | A derivation is a pure description of a build
data Derivation = Derivation
    { derivName :: Text                      -- Human-readable name
    , derivHash :: Text                      -- Deterministic hash of derivation
    , derivBuilder :: StorePath              -- Path to the builder executable
    , derivArgs :: [Text]                    -- Arguments to the builder
    , derivInputs :: Set DerivationInput     -- Input dependencies
    , derivOutputs :: Set DerivationOutput   -- Expected outputs
    , derivEnv :: Map Text Text              -- Environment variables
    , derivSystem :: Text                    -- Target system (e.g., "x86_64-linux")
    , derivStrategy :: BuildStrategy         -- How to build this derivation
    , derivMeta :: Map Text Text             -- Metadata for derivation
    } deriving (Show, Eq)

-- | Compare two derivations for equality
derivationEquals :: Derivation -> Derivation -> Bool
derivationEquals d1 d2 = derivHash d1 == derivHash d2

-- | Compare StorePaths for equality
derivationPathsEqual :: StorePath -> StorePath -> Bool
derivationPathsEqual p1 p2 = storeHash p1 == storeHash p2 && storeName p1 == storeName p2

-- | Node in a build graph
data BuildNode
    = InputNode StorePath                  -- An input that doesn't need to be built
    | DerivationNode Derivation            -- A derivation that needs to be built
    | OutputNode StorePath Derivation      -- An output produced by a derivation
    deriving (Show, Eq)

-- | Graph errors
data GraphError
    = CycleError [Text]                    -- A cycle was detected (node IDs)
    | MissingNodeError Text                -- A referenced node doesn't exist
    | InconsistentGraphError Text          -- Graph is in an inconsistent state
    | DeserializationError Text            -- Couldn't deserialize graph
    deriving (Show, Eq)

-- | Proof about a build graph
data GraphProof
    = AcyclicGraphProof      -- Graph has no cycles
    | CompleteProof     -- Graph contains all dependencies
    | ValidProof        -- Graph is both acyclic and complete
    deriving (Show, Eq)

-- | A build graph representing the dependency relationships
data BuildGraph = BuildGraph
    { graphNodes :: Map Text BuildNode     -- Nodes indexed by ID
    , graphEdges :: Map Text (Set Text)    -- Edges from node -> dependencies
    , graphRoots :: Set Text               -- Root nodes (outputs requested)
    , graphProof :: Maybe GraphProof       -- Proof about this graph
    } deriving (Show, Eq)

-- | Proof of a property, parameterized by phase
data Proof (p :: Phase) where
    -- Proofs for evaluation phase
    TypeProof      :: Proof 'Eval
    AcyclicProof   :: Proof 'Eval  -- Now references the GraphProof concept
    EvalCompleteProof :: Proof 'Eval

    -- Proofs for build phase
    InputProof     :: Proof 'Build
    BuildProof     :: Proof 'Build
    OutputProof    :: Proof 'Build

    -- Proofs for Return-Continuation
    ReturnProof    :: Proof 'Build
    RecursionProof :: Proof 'Build

    -- Composite proofs
    ComposeProof   :: Proof p -> Proof p -> Proof p

-- Add standalone deriving instances for Proof
deriving instance Show (Proof p)
deriving instance Eq (Proof p)

-- | Phase transition type for Return-Continuation
data PhaseTransition p q where
    EvalToBuild :: PhaseTransition 'Eval 'Build
    BuildToEval :: PhaseTransition 'Build 'Eval

deriving instance Show (PhaseTransition p q)
deriving instance Eq (PhaseTransition p q)

-- | User identifier
newtype UserId = UserId Text
    deriving (Show, Eq, Ord)

-- | Authentication token
newtype AuthToken = AuthToken Text
    deriving (Show, Eq)

-- | Daemon connection type
data DaemonConnection = DaemonConnection
    { connSocket :: Socket
    , connUserId :: UserId
    , connAuthToken :: AuthToken
    , connectionState :: ConnectionState
    }

-- | Connection state for daemon communication
data ConnectionState = ConnectionState {
    csSocket :: Socket,                     -- ^ Socket connected to daemon
    csHandle :: Handle,                     -- ^ Handle for socket I/O
    csUserId :: UserId,                     -- ^ Authenticated user ID
    csToken :: AuthToken,                   -- ^ Authentication token
    csRequestMap :: IORef (Map RequestId (MVar Response)), -- ^ Map of pending requests
    csNextReqId :: IORef RequestId,         -- ^ Next request ID
    csReaderThread :: ThreadId,             -- ^ Thread ID of the response reader
    csShutdown :: IORef Bool                -- ^ Flag to indicate connection shutdown
}

-- | Request ID type
type RequestId = Int

-- | Request type (stub for compilation)
data Request = Request deriving (Show, Eq)

-- | Response type (stub for compilation)
data Response = Response deriving (Show, Eq)

-- | Thread ID type (stub for compilation)
type ThreadId = Int

-- | IORef type (stub for compilation)
type IORef a = TVar a

-- | Running mode for Ten
data RunMode
    = StandaloneMode                     -- Direct execution without daemon
    | ClientMode DaemonConnection        -- Connect to existing daemon
    | DaemonMode                         -- Running as daemon process

-- Custom instances for RunMode to avoid needing instances for DaemonConnection
instance Show RunMode where
    show StandaloneMode = "StandaloneMode"
    show (ClientMode _) = "ClientMode <connection>"
    show DaemonMode = "DaemonMode"

instance Eq RunMode where
    StandaloneMode == StandaloneMode = True
    (ClientMode _) == (ClientMode _) = True  -- Considers all client modes equal
    DaemonMode == DaemonMode = True
    _ == _ = False

-- | Build status for tracking builds
data BuildStatus
    = BuildPending
    | BuildRunning Float                 -- Progress percentage (0.0-1.0)
    | BuildRecursing BuildId             -- Switched to a new derivation
    | BuildCompleted
    | BuildFailed'
    deriving (Show, Eq)

-- | Build strategy
data BuildStrategy
    = ApplicativeStrategy                -- Static dependencies, can parallelize
    | MonadicStrategy                    -- Dynamic dependencies or Return-Continuation
    deriving (Show, Eq)

-- | Daemon configuration
data DaemonConfig = DaemonConfig
    { daemonSocketPath :: FilePath       -- Path to daemon socket
    , daemonStorePath :: FilePath        -- Path to store
    , daemonStateFile :: FilePath        -- Path to state file
    , daemonLogLevel :: Int              -- Log verbosity level
    , daemonGcInterval :: Maybe Int      -- Garbage collection interval in seconds
    , daemonUser :: Maybe Text           -- User to run as
    , daemonAllowedUsers :: Set Text     -- Users allowed to connect
    } deriving (Show, Eq)

-- | User credentials for daemon authentication
data UserCredentials = UserCredentials
    { username :: Text
    , token :: Text
    } deriving (Show, Eq)

-- | Environment for build operations
data BuildEnv = BuildEnv
    { workDir :: FilePath                -- Temporary build directory
    , storePath :: FilePath              -- Root of content-addressed store
    , verbosity :: Int                   -- Logging verbosity level
    , allowedPaths :: Set FilePath       -- Paths accessible during build
    , runMode :: RunMode                 -- Current running mode
    , userName :: Maybe Text             -- Current user name (for daemon mode)
    , buildStrategy :: BuildStrategy     -- How to build derivations
    , maxRecursionDepth :: Int           -- Maximum allowed derivation recursion
    } deriving (Show, Eq)

-- | State carried through build operations
data BuildState = BuildState
    { currentPhase :: Phase              -- Current execution phase
    , buildProofs :: [Proof 'Build]      -- Accumulated proofs
    , buildInputs :: Set StorePath       -- Input paths for current build
    , buildOutputs :: Set StorePath      -- Output paths for current build
    , currentBuildId :: Maybe BuildId    -- Current build identifier
    , buildChain :: [Derivation]         -- Chain of return-continuation derivations
    , recursionDepth :: Int              -- Tracks recursion depth for cycles
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
    , runMode = StandaloneMode
    , userName = Nothing
    , buildStrategy = MonadicStrategy
    , maxRecursionDepth = 100
    }

-- | Initialize client build environment
initClientEnv :: FilePath -> FilePath -> DaemonConnection -> BuildEnv
initClientEnv wd sp conn = (initBuildEnv wd sp)
    { runMode = ClientMode conn
    }

-- | Initialize daemon build environment
initDaemonEnv :: FilePath -> FilePath -> Maybe Text -> BuildEnv
initDaemonEnv wd sp user = (initBuildEnv wd sp)
    { runMode = DaemonMode
    , userName = user
    }

-- | Initialize build state for a given phase
initBuildState :: Phase -> BuildState
initBuildState phase = BuildState
    { currentPhase = phase
    , buildProofs = []
    , buildInputs = Set.empty
    , buildOutputs = Set.empty
    , currentBuildId = Nothing
    , buildChain = []
    , recursionDepth = 0
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

-- | Safely transition between phases
transitionPhase :: PhaseTransition p q -> TenM p a -> TenM q a
transitionPhase trans action = TenM $ do
    env <- ask
    state <- get

    case trans of
        EvalToBuild -> do
            -- Run action in eval phase with original state
            result <- liftIO $ runTen action env state
            case result of
                Left err -> throwError err
                Right (val, newState) -> do
                    -- Update state with build phase
                    put newState { currentPhase = Build }
                    return val

        BuildToEval -> do
            -- Run action in build phase with original state
            result <- liftIO $ runTen action env state
            case result of
                Left err -> throwError err
                Right (val, newState) -> do
                    -- Update state with eval phase
                    put newState { currentPhase = Eval }
                    return val

-- | Generate a new unique build ID
newBuildId :: TenM p BuildId
newBuildId = liftIO $ BuildId <$> newUnique

-- | Set the current build ID
setCurrentBuildId :: BuildId -> TenM p ()
setCurrentBuildId bid = modify $ \s -> s { currentBuildId = Just bid }

-- | Add a derivation to the build chain
addToDerivationChain :: Derivation -> TenM p ()
addToDerivationChain drv = do
    depth <- gets recursionDepth
    maxDepth <- asks maxRecursionDepth
    when (depth >= maxDepth) $
        throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show maxDepth)
    modify $ \s -> s { buildChain = drv : buildChain s, recursionDepth = depth + 1 }

-- | Check if a derivation is in the build chain (cycle detection)
isInDerivationChain :: Derivation -> TenM p Bool
isInDerivationChain drv = do
    chain <- gets buildChain
    return $ any (derivationEquals drv) chain

-- | Logging function
logMsg :: Int -> Text -> TenM p ()
logMsg level msg = do
    v <- asks verbosity
    when (v >= level) $ liftIO $ putStrLn $ T.unpack msg

-- | Record a proof in the build state
addProof :: Proof p -> TenM p ()
addProof proof = case proof of
    p@BuildProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@OutputProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@ReturnProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@RecursionProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@(ComposeProof p1 p2) -> do
        addProof p1
        addProof p2
    _ -> return () -- Other proofs don't affect build state

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

-- | Run an STM transaction from Ten monad
atomicallyTen :: STM a -> TenM p a
atomicallyTen = liftIO . atomically

-- | Execute a build operation using the daemon (if in client mode)
withDaemon :: (DaemonConnection -> IO (Either BuildError a)) -> TenM p a
withDaemon f = do
    mode <- asks runMode
    case mode of
        ClientMode conn -> do
            result <- liftIO $ f conn
            case result of
                Left err -> throwError err
                Right val -> return val
        _ -> throwError $ DaemonError "Operation requires daemon connection"

-- | Check if running in daemon mode
isDaemonMode :: TenM p Bool
isDaemonMode = (== DaemonMode) <$> asks runMode

-- | Check if connected to daemon
isClientMode :: TenM p Bool
isClientMode = do
    mode <- asks runMode
    case mode of
        ClientMode _ -> return True
        _ -> return False

-- | Linux-specific: Drop privileges to a specified user and group
dropPrivileges :: Text -> Text -> TenM p ()
dropPrivileges user group = liftIO $ do
    -- Get the current (effective) user ID
    euid <- getEffectiveUserID

    -- Only proceed if we're running as root
    when (euid == 0) $ do
        -- Get user and group entries
        userEntry <- getUserEntryForName (T.unpack user)
        groupEntry <- getGroupEntryForName (T.unpack group)

        -- Set group first (needed before dropping user privileges)
        setGroupID (groupID groupEntry)

        -- Then set user
        setUserID (userID userEntry)

-- | Linux-specific: Get a system user ID
getSystemUser :: Text -> TenM p UserID
getSystemUser username = liftIO $ do
    userEntry <- getUserEntryForName (T.unpack username)
    return $ userID userEntry

-- | Linux-specific: Get a system group ID
getSystemGroup :: Text -> TenM p GroupID
getSystemGroup groupname = liftIO $ do
    groupEntry <- getGroupEntryForName (T.unpack groupname)
    return $ groupID groupEntry
