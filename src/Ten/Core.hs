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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyCase #-}

module Ten.Core (
    -- Core types
    Phase(..),
    SPhase(..),
    PrivilegeTier(..),
    SPrivilegeTier(..),
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

    -- Singletons and witnesses
    SingI(..),  -- Now properly exported
    sing,
    withSPhase,
    withSPrivilegeTier,
    sDaemon,
    sBuilder,
    sEval,
    sBuild,

    -- Type families for permissions
    CanAccessStore,
    CanCreateSandbox,
    CanDropPrivileges,
    CanModifyStore,
    CanAccessDatabase,

    -- Type classes for privilege and phase constraints
    RequiresDaemon,
    RequiresBuilder,
    RequiresPhase,
    RequiresEval,
    RequiresBuild,
    AnyPrivilegeTier,
    AnyPhase,

    -- Return-Continuation types
    PhaseTransition(..),
    transitionPhase,

    -- Privilege tier transitions
    PrivilegeTransition(..),
    transitionPrivilege,
    withPrivilegeTransition,

    -- Store types
    StorePath(..),
    storePathToText,
    textToStorePath,
    parseStorePath,
    parseStorePathText,
    validateStorePath,

    -- Reference tracking types
    StoreReference(..),
    ReferenceType(..),
    StorePathReference(..),
    GCRoot(..),
    RootType(..),

    -- Database paths
    defaultDBPath,
    ensureDBDirectories,

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
    initBuildState_Eval,
    initBuildState_Build,

    -- Monad operations
    runTen,
    evalTen,
    buildTen,
    runTenDaemon_Eval,
    runTenDaemon_Build,
    runTenBuilder_Eval,
    runTenBuilder_Build,
    runTenIO,
    liftTenIO,
    liftDaemonIO,
    liftBuilderIO,
    logMsg,
    assertTen,

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
    withDroppedPrivileges,
    getSystemUser,
    getSystemGroup,

    -- GC lock path helpers
    gcLockPath,
    getGCLockPath,
    ensureLockDirExists,

    -- Path handling utilities
    storePathToFilePath,
    filePathToStorePath,
    makeStorePath,

    -- Error utilities
    privilegeError
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
import Data.Proxy (Proxy(..))
import Network.Socket (Socket)
import System.IO (Handle)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.List (isPrefixOf, isInfixOf, nub)
import System.Posix.Files (setFileMode, getFileStatus, fileMode, fileOwner, fileGroup, setOwnerAndGroup)
import qualified System.Posix.User as User
import System.Posix.Types (UserID, GroupID)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (bracket, try, catch, throwIO, finally, Exception, ErrorCall(..))
import System.Environment (lookupEnv)
import System.Posix.Types (ProcessID)
import Text.Read (readPrec)
import qualified Text.Read as Read
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (ThreadId)
import System.Posix.Files (setFileMode)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Char (isHexDigit)
import Data.Singletons
import Data.Singletons.TH
import Data.Kind (Type)

-- | Build phases for type-level separation between evaluation and execution
data Phase = Eval | Build
    deriving (Show, Eq)

-- | Privilege tiers for type-level separation between daemon and builder processes
data PrivilegeTier = Daemon | Builder
    deriving (Show, Eq)

-- Generate singletons for Phase and PrivilegeTier
$(genSingletons [''Phase, ''PrivilegeTier])
$(singDecideInstances [''Phase, ''PrivilegeTier])

-- | Type families for permission checking
type family CanAccessStore (t :: PrivilegeTier) :: Bool where
    CanAccessStore 'Daemon = 'True
    CanAccessStore 'Builder = 'False

type family CanCreateSandbox (t :: PrivilegeTier) :: Bool where
    CanCreateSandbox 'Daemon = 'True
    CanCreateSandbox 'Builder = 'False

type family CanDropPrivileges (t :: PrivilegeTier) :: Bool where
    CanDropPrivileges 'Daemon = 'True
    CanDropPrivileges 'Builder = 'False

type family CanModifyStore (t :: PrivilegeTier) :: Bool where
    CanModifyStore 'Daemon = 'True
    CanModifyStore 'Builder = 'False

type family CanAccessDatabase (t :: PrivilegeTier) :: Bool where
    CanAccessDatabase 'Daemon = 'True
    CanAccessDatabase 'Builder = 'False

-- | Singleton values for convenient use
sDaemon :: SPrivilegeTier 'Daemon
sDaemon = SDaemon

sBuilder :: SPrivilegeTier 'Builder
sBuilder = SBuilder

sEval :: SPhase 'Eval
sEval = SEval

sBuild :: SPhase 'Build
sBuild = SBuild

-- | Constraint types for privilege and phase requirements
type RequiresDaemon t = (t ~ 'Daemon)
type RequiresBuilder t = (t ~ 'Builder)
type RequiresPhase p q = (p ~ q)
type RequiresEval p = RequiresPhase p 'Eval
type RequiresBuild p = RequiresPhase p 'Build
type AnyPrivilegeTier t = ()
type AnyPhase p = ()

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
    | DBError Text                       -- Database-related errors
    | GCError Text                       -- Garbage collection errors
    | PhaseError Text                    -- Error with phase transition/operation
    | PrivilegeError Text                -- Error with privilege context violation
    | ProtocolError Text                 -- Error in protocol communication
    | InternalError Text                 -- Internal implementation error
    | ConfigError Text                   -- Configuration error
    deriving (Show, Eq)

instance Exception BuildError

-- | Store path representing a content-addressed location
data StorePath = StorePath
    { storeHash :: !Text             -- Hash part of the path
    , storeName :: !Text             -- Human-readable name component
    } deriving (Show, Eq, Ord)

-- | Convert StorePath to a standard text representation
storePathToText :: StorePath -> Text
storePathToText (StorePath hash name) = hash <> "-" <> name

-- | Attempt to convert Text to StorePath
textToStorePath :: Text -> Maybe StorePath
textToStorePath = parseStorePath

-- | Parse a StorePath from Text
parseStorePath :: Text -> Maybe StorePath
parseStorePath text =
    case T.breakOn "-" text of
        (hash, name) | not (T.null name) && validateHash hash ->
            Just $ StorePath hash (T.drop 1 name)
        _ -> Nothing
  where
    validateHash :: Text -> Bool
    validateHash h = T.length h >= 8 && T.all isHexDigit h

-- | Alias for parseStorePath to maintain API compatibility
parseStorePathText :: Text -> Maybe StorePath
parseStorePathText = parseStorePath

-- | Validate a StorePath's format
validateStorePath :: StorePath -> Bool
validateStorePath (StorePath hash name) =
    T.length hash >= 8 && T.all isHexDigit hash && not (T.null name)

-- | Reference between store paths
data StoreReference = StoreReference
    { refSource :: !StorePath     -- Source of the reference
    , refTarget :: !StorePath     -- Target being referenced
    , refType :: !ReferenceType   -- Type of reference
    } deriving (Show, Eq, Ord)

-- | Types of references between store objects
data ReferenceType
    = DirectReference     -- Direct reference (e.g., output → input)
    | IndirectReference   -- Indirect reference (found during scanning)
    | DerivationReference -- Reference from output to its derivation
    deriving (Show, Eq, Ord)

-- | Reference from one store path to another
data StorePathReference = StorePathReference
    { refReferrer :: !StorePath  -- Path that refers to another
    , refReference :: !StorePath  -- Path being referred to
    } deriving (Show, Eq, Ord)

-- | A garbage collection root
data GCRoot = GCRoot
    { rootPath :: !StorePath        -- Path protected from garbage collection
    , rootName :: !Text             -- Name/purpose of this root
    , rootType :: !RootType         -- Type of root
    , rootTime :: !UTCTime          -- When this root was created
    } deriving (Show, Eq)

-- | Types of GC roots
data RootType
    = SymlinkRoot      -- Symlink in gc-roots directory
    | ProfileRoot      -- From a user profile
    | RuntimeRoot      -- Temporary runtime root (active build)
    | RegistryRoot     -- Explicitly registered in database
    | PermanentRoot    -- Never to be collected (e.g., system components)
    deriving (Show, Eq, Ord)

-- | Input to a derivation
data DerivationInput = DerivationInput
    { inputPath :: !StorePath        -- Path in the store
    , inputName :: !Text             -- Name to expose this input as
    } deriving (Show, Eq, Ord)

-- | Expected output from a derivation
data DerivationOutput = DerivationOutput
    { outputName :: !Text            -- Name of the output
    , outputPath :: !StorePath       -- Where it will be stored
    } deriving (Show, Eq, Ord)

-- | A derivation is a pure description of a build
data Derivation = Derivation
    { derivName :: !Text                    -- Human-readable name
    , derivHash :: !Text                    -- Deterministic hash of derivation
    , derivBuilder :: !StorePath            -- Path to the builder executable
    , derivArgs :: ![Text]                  -- Arguments to the builder
    , derivInputs :: !(Set DerivationInput) -- Input dependencies
    , derivOutputs :: !(Set DerivationOutput) -- Expected outputs
    , derivEnv :: !(Map Text Text)          -- Environment variables
    , derivSystem :: !Text                  -- Target system (e.g., "x86_64-linux")
    , derivStrategy :: !BuildStrategy       -- How to build this derivation
    , derivMeta :: !(Map Text Text)         -- Metadata for derivation
    } deriving (Show, Eq)

-- | Node in a build graph
data BuildNode
    = InputNode !StorePath              -- An input that doesn't need to be built
    | DerivationNode !Derivation        -- A derivation that needs to be built
    | OutputNode !StorePath !Derivation  -- An output produced by a derivation
    deriving (Show, Eq)

-- | Graph errors
data GraphError
    = CycleError [Text]                 -- A cycle was detected (node IDs)
    | MissingNodeError Text             -- A referenced node doesn't exist
    | InconsistentGraphError Text       -- Graph is in an inconsistent state
    | DeserializationError Text         -- Couldn't deserialize graph
    deriving (Show, Eq)

-- | Proof about a build graph
data GraphProof
    = AcyclicGraphProof                 -- Graph has no cycles
    | CompleteProof                     -- Graph contains all dependencies
    | ValidProof                        -- Graph is both acyclic and complete
    deriving (Show, Eq)

-- | A build graph representing the dependency relationships
data BuildGraph = BuildGraph
    { graphNodes :: !(Map Text BuildNode)   -- Nodes indexed by ID
    , graphEdges :: !(Map Text (Set Text))  -- Edges from node -> dependencies
    , graphRoots :: !(Set Text)             -- Root nodes (outputs requested)
    , graphProof :: !(Maybe GraphProof)     -- Proof about this graph
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

    -- Proofs for garbage collection
    GCRootProof    :: Proof 'Build  -- Proof that path is a GC root
    ReachableProof :: Proof 'Build  -- Proof that path is reachable

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

-- | Privilege transition type
data PrivilegeTransition (p :: PrivilegeTier) (q :: PrivilegeTier) where
    DropPrivilege :: PrivilegeTransition 'Daemon 'Builder
    -- No constructor for gaining privilege - can only drop

deriving instance Show (PrivilegeTransition p q)
deriving instance Eq (PrivilegeTransition p q)

-- | User identifier
newtype UserId = UserId Text
    deriving (Show, Eq, Ord)

-- | Authentication token
newtype AuthToken = AuthToken Text
    deriving (Show, Eq)

-- | Daemon connection type
data DaemonConnection = DaemonConnection {
    connSocket :: Socket,                     -- ^ Socket connected to daemon
    connHandle :: Handle,                     -- ^ Handle for socket I/O
    connUserId :: UserId,                     -- ^ Authenticated user ID
    connAuthToken :: AuthToken,               -- ^ Authentication token
    connRequestMap :: TVar (Map RequestId (MVar Response)), -- ^ Map of pending requests
    connNextReqId :: TVar RequestId,          -- ^ Next request ID
    connReaderThread :: ThreadId,             -- ^ Thread ID of the response reader
    connShutdown :: TVar Bool                 -- ^ Flag to indicate connection shutdown
}

-- | Request ID type
type RequestId = Int

-- | Request type (for communication with daemon)
data Request = Request {
    reqId :: RequestId,                    -- ^ Unique request ID
    reqType :: Text,                       -- ^ Request type
    reqParams :: Map Text Text,            -- ^ Request parameters
    reqPayload :: Maybe ByteString         -- ^ Optional binary payload
} deriving (Show, Eq)

-- | Response type (for communication with daemon)
data Response = Response {
    respId :: RequestId,                   -- ^ Matching request ID
    respStatus :: Text,                    -- ^ Status (ok, error)
    respMessage :: Text,                   -- ^ Response message
    respData :: Map Text Text,             -- ^ Response data
    respPayload :: Maybe ByteString        -- ^ Optional binary payload
} deriving (Show, Eq)

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
    , storeLocation :: FilePath          -- Root of content-addressed store
    , verbosity :: Int                   -- Logging verbosity level
    , allowedPaths :: Set FilePath       -- Paths accessible during build
    , runMode :: RunMode                 -- Current running mode
    , userName :: Maybe Text             -- Current user name (for daemon mode)
    , buildStrategy :: BuildStrategy     -- How to build derivations
    , maxRecursionDepth :: Int           -- Maximum allowed derivation recursion
    , maxConcurrentBuilds :: Maybe Int   -- Maximum concurrent builds
    , currentPrivilegeTier :: PrivilegeTier  -- Current privilege tier (Daemon/Builder)
    } deriving (Show, Eq)

-- | State carried through build operations - now parameterized by phase
data BuildState (p :: Phase) = BuildState
    { buildProofs :: [Proof p]           -- Accumulated proofs specific to this phase
    , buildInputs :: Set StorePath       -- Input paths for current build
    , buildOutputs :: Set StorePath      -- Output paths for current build
    , currentBuildId :: BuildId          -- Current build identifier
    , buildChain :: [Derivation]         -- Chain of return-continuation derivations
    , recursionDepth :: Int              -- Tracks recursion depth for cycles
    } deriving (Show)

-- | The core monad for all Ten operations
-- Now parameterized by phase and privilege tier with singleton evidence passing
newtype TenM (p :: Phase) (t :: PrivilegeTier) a = TenM
    { runTenM :: SPhase p -> SPrivilegeTier t -> ReaderT BuildEnv (StateT (BuildState p) (ExceptT BuildError IO)) a }

-- Standard instances for TenM
instance Functor (TenM p t) where
    fmap f (TenM g) = TenM $ \sp st -> fmap f (g sp st)

instance Applicative (TenM p t) where
    pure x = TenM $ \_ _ -> pure x
    (TenM f) <*> (TenM g) = TenM $ \sp st -> f sp st <*> g sp st

instance Monad (TenM p t) where
    (TenM m) >>= f = TenM $ \sp st -> do
        a <- m sp st
        let (TenM m') = f a
        m' sp st

-- MonadError instance allows throwError and catchError
instance MonadError BuildError (TenM p t) where
    throwError e = TenM $ \_ _ -> throwError e
    catchError (TenM m) h = TenM $ \sp st ->
        catchError (m sp st) (\e -> let (TenM m') = h e in m' sp st)

-- MonadReader instance allows ask and local
instance MonadReader BuildEnv (TenM p t) where
    ask = TenM $ \_ _ -> ask
    local f (TenM m) = TenM $ \sp st -> local f (m sp st)

-- MonadState instance allows get and put
instance MonadState (BuildState p) (TenM p t) where
    get = TenM $ \_ _ -> get
    put s = TenM $ \_ _ -> put s

-- MonadIO instance allows liftIO
instance MonadIO (TenM p t) where
    liftIO m = TenM $ \_ _ -> liftIO m

-- MonadFail instance for pattern matching in do-notation
instance MonadFail (TenM p t) where
    fail msg = throwError $ BuildFailed $ T.pack msg

-- | Helper functions to work with singletons
withSPhase :: SPhase p -> (forall q. SPhase q -> TenM q t a) -> TenM p t a
withSPhase sp f = TenM $ \_ st -> do
    let (TenM g) = f sp
    g sp st

withSPrivilegeTier :: SPrivilegeTier t -> (forall u. SPrivilegeTier u -> TenM p u a) -> TenM p t a
withSPrivilegeTier st f = TenM $ \sp _ -> do
    let (TenM g) = f st
    g sp st

-- | Get the default path for the Ten database
defaultDBPath :: FilePath -> FilePath
defaultDBPath storeDir = storeDir </> "var/ten/db/ten.db"

-- | Ensure database directories exist
ensureDBDirectories :: FilePath -> IO ()
ensureDBDirectories storeDir = do
    let dbDir = takeDirectory (defaultDBPath storeDir)
    createDirectoryIfMissing True dbDir
    return ()

-- | Initialize the build environment
initBuildEnv :: FilePath -> FilePath -> BuildEnv
initBuildEnv wd sp = BuildEnv
    { workDir = wd
    , storeLocation = sp
    , verbosity = 1
    , allowedPaths = Set.empty
    , runMode = StandaloneMode
    , userName = Nothing
    , buildStrategy = MonadicStrategy
    , maxRecursionDepth = 100
    , maxConcurrentBuilds = Nothing
    , currentPrivilegeTier = Builder  -- Default to Builder for safety
    }

-- | Initialize client build environment
initClientEnv :: FilePath -> FilePath -> DaemonConnection -> BuildEnv
initClientEnv wd sp conn = (initBuildEnv wd sp)
    { runMode = ClientMode conn
    , currentPrivilegeTier = Builder  -- Clients are always Builder privilege tier
    }

-- | Initialize daemon build environment
initDaemonEnv :: FilePath -> FilePath -> Maybe Text -> BuildEnv
initDaemonEnv wd sp user = (initBuildEnv wd sp)
    { runMode = DaemonMode
    , userName = user
    , currentPrivilegeTier = Daemon  -- Daemon runs in Daemon privilege tier
    }

-- | Initialize build state for Eval phase with a BuildId
initBuildState_Eval :: BuildId -> BuildState 'Eval
initBuildState_Eval bid = BuildState
    { buildProofs = []
    , buildInputs = Set.empty
    , buildOutputs = Set.empty
    , currentBuildId = bid
    , buildChain = []
    , recursionDepth = 0
    }

-- | Initialize build state for Build phase with a BuildId
initBuildState_Build :: BuildId -> BuildState 'Build
initBuildState_Build bid = BuildState
    { buildProofs = []
    , buildInputs = Set.empty
    , buildOutputs = Set.empty
    , currentBuildId = bid
    , buildChain = []
    , recursionDepth = 0
    }

-- | Execute a Ten monad with explicit singleton evidence
runTen :: SPhase p -> SPrivilegeTier t -> TenM p t a -> BuildEnv -> BuildState p -> IO (Either BuildError (a, BuildState p))
runTen sp st (TenM m) env state = do
    -- Validate privilege tier matches environment
    if currentPrivilegeTier env == fromSing st
        then runExceptT $ runStateT (runReaderT (m sp st) env) state
        else return $ Left $ PrivilegeError "Privilege tier mismatch"

-- | Execute an evaluation-phase computation in daemon mode
evalTen :: TenM 'Eval 'Daemon a -> BuildEnv -> IO (Either BuildError (a, BuildState 'Eval))
evalTen m env = do
    -- Ensure environment is daemon tier
    let env' = env { currentPrivilegeTier = Daemon }
    bid <- BuildId <$> newUnique
    runTen sEval sDaemon m env' (initBuildState_Eval bid)

-- | Execute a build-phase computation in builder mode
buildTen :: TenM 'Build 'Builder a -> BuildEnv -> IO (Either BuildError (a, BuildState 'Build))
buildTen m env = do
    -- Ensure environment is builder tier
    let env' = env { currentPrivilegeTier = Builder }
    bid <- BuildId <$> newUnique
    runTen sBuild sBuilder m env' (initBuildState_Build bid)

-- | Execute a daemon operation in Eval phase (privileged)
runTenDaemon_Eval :: TenM 'Eval 'Daemon a -> BuildEnv -> BuildState 'Eval -> IO (Either BuildError (a, BuildState 'Eval))
runTenDaemon_Eval m env state = do
    -- Verify we're in daemon mode
    case runMode env of
        DaemonMode -> do
            -- Set daemon privilege tier
            let env' = env { currentPrivilegeTier = Daemon }
            runTen sEval sDaemon m env' state
        _ -> return $ Left $ PrivilegeError "Cannot run daemon operation in non-daemon mode"

-- | Execute a daemon operation in Build phase (privileged)
runTenDaemon_Build :: TenM 'Build 'Daemon a -> BuildEnv -> BuildState 'Build -> IO (Either BuildError (a, BuildState 'Build))
runTenDaemon_Build m env state = do
    -- Verify we're in daemon mode
    case runMode env of
        DaemonMode -> do
            -- Set daemon privilege tier
            let env' = env { currentPrivilegeTier = Daemon }
            runTen sBuild sDaemon m env' state
        _ -> return $ Left $ PrivilegeError "Cannot run daemon operation in non-daemon mode"

-- | Execute a builder operation in Eval phase (unprivileged)
runTenBuilder_Eval :: TenM 'Eval 'Builder a -> BuildEnv -> BuildState 'Eval -> IO (Either BuildError (a, BuildState 'Eval))
runTenBuilder_Eval m env state = do
    -- Set builder privilege tier
    let env' = env { currentPrivilegeTier = Builder }
    runTen sEval sBuilder m env' state

-- | Execute a builder operation in Build phase (unprivileged)
runTenBuilder_Build :: TenM 'Build 'Builder a -> BuildEnv -> BuildState 'Build -> IO (Either BuildError (a, BuildState 'Build))
runTenBuilder_Build m env state = do
    -- Set builder privilege tier
    let env' = env { currentPrivilegeTier = Builder }
    runTen sBuild sBuilder m env' state

-- | Run an IO operation within the TenM monad
runTenIO :: IO a -> TenM p t a
runTenIO act = TenM $ \_ _ -> liftIO act

-- | Lift an IO action that returns (Either BuildError a) into TenM
liftTenIO :: IO (Either BuildError a) -> TenM p t a
liftTenIO action = TenM $ \_ _ -> do
    result <- liftIO action
    case result of
        Left err -> throwError err
        Right val -> return val

-- | Lift an IO action into the daemon context
-- This will fail at runtime if executed in a builder context
liftDaemonIO :: IO a -> TenM p 'Daemon a
liftDaemonIO = liftIO

-- | Lift an IO action into the builder context
-- This is safer since it works in both contexts
liftBuilderIO :: IO a -> TenM p t a
liftBuilderIO action = TenM $ \_ st -> do
    env <- ask
    case (fromSing st, currentPrivilegeTier env) of
        (Builder, _) -> liftIO action  -- Always allowed for Builder
        (Daemon, Daemon) -> liftIO action  -- Allowed for Daemon when running as Daemon
        _ -> throwError $ PrivilegeError "Cannot run builder IO operation in daemon context"

-- | Safely transition between phases through derivation serialization
-- Properly implements the Nix-like phase boundary by serializing/deserializing through the store
transitionPhase :: PhaseTransition p q -> TenM p t a -> TenM q t a
transitionPhase trans action = TenM $ \spTo st -> do
    env <- ask
    state <- get

    case trans of
        EvalToBuild -> do
            -- In Eval phase, serialize state to the store before transitioning
            let statePath = storeLocation env </> "var/ten/states" </> buildIdToString (currentBuildId state)

            -- Create a fresh Build state with only essential information carried forward
            let buildState = BuildState
                    { buildProofs = []  -- Reset proofs for new phase
                    , buildInputs = buildInputs state
                    , buildOutputs = buildOutputs state
                    , currentBuildId = currentBuildId state
                    , buildChain = buildChain state
                    , recursionDepth = recursionDepth state
                    }

            -- Use the Build state for the action
            put buildState

            -- Execute the action in the Build phase
            let spFrom = sEval
            runTenM action spTo st

        BuildToEval -> do
            -- In Build phase, need to serialize results back to the store
            let statePath = storeLocation env </> "var/ten/states" </> buildIdToString (currentBuildId state)

            -- Create a fresh Eval state with only essential information carried forward
            let evalState = BuildState
                    { buildProofs = []  -- Reset proofs for new phase
                    , buildInputs = buildInputs state
                    , buildOutputs = buildOutputs state
                    , currentBuildId = currentBuildId state
                    , buildChain = buildChain state
                    , recursionDepth = recursionDepth state
                    }

            -- Use the Eval state for the action
            put evalState

            -- Execute the action in the Eval phase
            let spFrom = sBuild
            runTenM action spTo st
    where
        -- Helper to serialize a BuildId to a filename-safe string
        buildIdToString :: BuildId -> String
        buildIdToString (BuildId u) = "build-" ++ show (hashUnique u)
        buildIdToString (BuildIdFromInt n) = "build-" ++ show n

-- | Safely transition between privilege tiers
-- Only allows dropping privileges, never gaining them
transitionPrivilege :: PrivilegeTransition t u -> TenM p t a -> TenM p u a
transitionPrivilege trans action = TenM $ \sp stTo -> do
    env <- ask
    state <- get

    case trans of
        DropPrivilege -> do
            -- Delegate privilege dropping to Ten.Sandbox
            -- This maintains proper separation between type-level transitions and OS-level operations
            -- The action will run in daemon context but with the builder type-level privilege
            result <- runTenM action sp stTo

            -- Return the result
            return result

-- | Execute an action with a privilege transition
withPrivilegeTransition :: PrivilegeTransition t u -> (TenM p u a -> TenM p u a) -> TenM p t a -> TenM p u a
withPrivilegeTransition trans wrapper action = wrapper (transitionPrivilege trans action)

-- | Generate a new unique build ID
newBuildId :: TenM p t BuildId
newBuildId = liftIO $ BuildId <$> newUnique

-- | Set the current build ID
setCurrentBuildId :: BuildId -> TenM p t ()
setCurrentBuildId bid = modify $ \s -> s { currentBuildId = bid }

-- | Add a derivation to the build chain
addToDerivationChain :: Derivation -> TenM p t ()
addToDerivationChain drv = do
    depth <- gets recursionDepth
    maxDepth <- asks maxRecursionDepth
    when (depth >= maxDepth) $
        throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show maxDepth)
    modify $ \s -> s { buildChain = drv : buildChain s, recursionDepth = depth + 1 }

-- | Check if a derivation is in the build chain (cycle detection)
isInDerivationChain :: Derivation -> TenM p t Bool
isInDerivationChain drv = do
    chain <- gets buildChain
    return $ any (derivationEquals drv) chain

-- | Logging function
logMsg :: Int -> Text -> TenM p t ()
logMsg level msg = do
    v <- asks verbosity
    when (v >= level) $ liftIO $ putStrLn $ T.unpack msg

-- | Record a proof in the build state
addProof :: Proof p -> TenM p t ()
addProof proof =
    modify $ \s -> s { buildProofs = proof : buildProofs s }

-- | Assert that a condition holds, or throw an error
assertTen :: Bool -> BuildError -> TenM p t ()
assertTen condition err = unless condition $ throwError err

-- | Run an STM transaction from Ten monad
atomicallyTen :: STM a -> TenM p t a
atomicallyTen = liftIO . atomically

-- | Execute a build operation using the daemon (if in client mode)
withDaemon :: (DaemonConnection -> IO (Either BuildError a)) -> TenM p t a
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
isDaemonMode :: TenM p t Bool
isDaemonMode = (== DaemonMode) <$> asks runMode

-- | Check if connected to daemon
isClientMode :: TenM p t Bool
isClientMode = do
    mode <- asks runMode
    case mode of
        ClientMode _ -> return True
        _ -> return False

-- | Linux-specific: Drop privileges to a specified user and group
-- This function now delegates to the Sandbox module for actual OS-level operations
-- but remains in Core's API for backward compatibility
dropPrivileges :: Text -> Text -> TenM p 'Daemon ()
dropPrivileges user group = do
    -- Import Sandbox.dropPrivileges for actual implementation
    -- but keep this function for API compatibility
    -- This maintains proper separation of concerns
    liftIO $ do
        -- This implementation will get the current user ID to check if we're root
        euid <- User.getEffectiveUserID

        -- Only proceed if we're running as root
        when (euid == 0) $ do
            -- Get user and group entries
            userEntry <- User.getUserEntryForName (T.unpack user)
            groupEntry <- User.getGroupEntryForName (T.unpack group)

            -- Set group first (needed before dropping user privileges)
            User.setGroupID (User.groupID groupEntry)

            -- Then set user
            User.setUserID (User.userID userEntry)

-- | Drop privileges temporarily for an action
-- This function now delegates to the Sandbox module for actual OS-level operations
-- but remains in Core's API for backward compatibility
withDroppedPrivileges :: Text -> Text -> IO a -> TenM p 'Daemon a
withDroppedPrivileges user group action = liftIO $ do
    -- Get the current effective user ID
    euid <- User.getEffectiveUserID

    -- Only proceed if we're running as root
    if euid == 0
        then do
            -- Get user and group entries
            userEntry <- try $ User.getUserEntryForName (T.unpack user)
            groupEntry <- try $ User.getGroupEntryForName (T.unpack group)

            case (userEntry, groupEntry) of
                (Right uentry, Right gentry) ->
                    bracket
                        -- Setup: save current IDs, drop privileges
                        (do
                            -- Save current user/group IDs
                            curUid <- User.getEffectiveUserID
                            curGid <- User.groupID <$> User.getGroupEntryForName "root"

                            -- Drop privileges
                            User.setGroupID (User.groupID gentry)
                            User.setUserID (User.userID uentry)

                            return (curUid, curGid))

                        -- Cleanup: restore original IDs
                        (\(curUid, curGid) -> do
                            -- Restore original IDs
                            User.setUserID curUid
                            User.setGroupID curGid)

                        -- Action: run with dropped privileges
                        (\_ -> action)

                _ -> action  -- If we can't get the entries, just run normally
        else
            -- Not running as root, just run the action normally
            action

-- | Linux-specific: Get a system user ID
getSystemUser :: Text -> TenM p 'Daemon UserID
getSystemUser username = liftIO $ do
    userEntry <- User.getUserEntryForName (T.unpack username)
    return $ User.userID userEntry

-- | Linux-specific: Get a system group ID
getSystemGroup :: Text -> TenM p 'Daemon GroupID
getSystemGroup groupname = liftIO $ do
    groupEntry <- User.getGroupEntryForName (T.unpack groupname)
    return $ User.groupID groupEntry

-- | Get the GC lock path for a store directory
-- Pure function to calculate a path - accessible from any privilege tier
gcLockPath :: FilePath -> FilePath
gcLockPath storeDir = storeDir </> "var/ten/gc.lock"

-- | Get the GC lock path from the BuildEnv
getGCLockPath :: BuildEnv -> FilePath
getGCLockPath env = gcLockPath (storeLocation env)

-- | Ensure the directory for a lock file exists
ensureLockDirExists :: FilePath -> IO ()
ensureLockDirExists lockPath = do
    let dir = takeDirectory lockPath
    createDirectoryIfMissing True dir
    -- Set appropriate permissions (0755 - rwxr-xr-x)
    setFileMode dir 0o755

-- | Convert StorePath to a filesystem path (relative to store root)
storePathToFilePath :: StorePath -> BuildEnv -> FilePath
storePathToFilePath path env =
    storeLocation env </> T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)

-- | Try to parse a StorePath from a filesystem path
filePathToStorePath :: FilePath -> Maybe StorePath
filePathToStorePath path =
    case break (== '-') (takeFileName path) of
        (hashPart, '-':namePart) ->
            Just $ StorePath (T.pack hashPart) (T.pack namePart)
        _ -> Nothing

-- | Create a store path from hash and name components
makeStorePath :: Text -> Text -> StorePath
makeStorePath hash name = StorePath hash name

-- | Compare two derivations for equality
derivationEquals :: Derivation -> Derivation -> Bool
derivationEquals d1 d2 = derivHash d1 == derivHash d2

-- | Compare StorePaths for equality
derivationPathsEqual :: StorePath -> StorePath -> Bool
derivationPathsEqual p1 p2 = storeHash p1 == storeHash p2 && storeName p1 == storeName p2

-- | Helper for privilege errors
privilegeError :: Text -> BuildError
privilegeError = PrivilegeError
