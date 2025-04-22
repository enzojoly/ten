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
    SingI(..),
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

    -- Process boundary primitives
    storeDerivation,
    readDerivation,
    storeBuildResult,
    readBuildResult,
    serializeDerivation,
    deserializeDerivation,
    serializeBuildResult,
    deserializeBuildResult,

    -- Privilege and process management
    PrivilegeTransition(..),
    spawnBuilder,
    runAsBuilder,
    requestFromDaemon,

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
    runTenDaemon,
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
import qualified Data.ByteString.Lazy as LBS
import Data.Unique (Unique, newUnique, hashUnique)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Data.Unique (Unique, newUnique)
import Data.Proxy (Proxy(..))
import Network.Socket (Socket)
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush, hPutStrLn, stderr)
import Data.Maybe (isJust, isNothing, fromMaybe, catMaybes)
import Data.List (isPrefixOf, isInfixOf, nub)
import qualified System.Posix.User as User
import System.Posix.Files (setFileMode)
import System.Posix.Types (ProcessID, UserID, GroupID)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (bracket, try, catch, throwIO, finally, Exception, ErrorCall(..))
import System.Environment (lookupEnv, getEnvironment)
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import System.Posix.IO (createPipe, fdToHandle, closeFd)
import Text.Read (readPrec)
import qualified Text.Read as Read
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Char (isHexDigit)
import Data.Singletons
import Data.Singletons.TH
import Data.Kind (Type)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encoding as Aeson
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import System.Random (randomRIO)

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

-- | Build result - represents what comes out of the build phase
data BuildResult = BuildResult
    { brOutputPaths :: Set StorePath        -- Outputs produced
    , brExitCode :: ExitCode                -- Exit code from builder
    , brLog :: Text                         -- Build log
    , brReferences :: Set StorePath         -- References from outputs
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

-- | Daemon connection type for protocol communication
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
    , currentPhase :: Phase              -- Current phase (for runtime checks)
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
    , currentPhase = Eval
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
    , currentPhase = Build
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

-- | Execute a daemon operation with appropriate phase (general version)
runTenDaemon :: forall p a. (SingI p) => TenM p 'Daemon a -> BuildEnv -> BuildState p -> IO (Either BuildError (a, BuildState p))
runTenDaemon m env state = do
    -- Verify we're in daemon mode
    case runMode env of
        DaemonMode -> do
            -- Set daemon privilege tier
            let env' = env { currentPrivilegeTier = Daemon }
            runTen (sing @p) sDaemon m env' state
        _ -> return $ Left $ PrivilegeError "Cannot run daemon operation in non-daemon mode"

-- | Execute a daemon operation in Eval phase (privileged)
runTenDaemon_Eval :: TenM 'Eval 'Daemon a -> BuildEnv -> BuildState 'Eval -> IO (Either BuildError (a, BuildState 'Eval))
runTenDaemon_Eval = runTenDaemon

-- | Execute a daemon operation in Build phase (privileged)
runTenDaemon_Build :: TenM 'Build 'Daemon a -> BuildEnv -> BuildState 'Build -> IO (Either BuildError (a, BuildState 'Build))
runTenDaemon_Build = runTenDaemon

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

-- | Protocol message for daemon-builder communication
data DaemonMessage
    = BuildRequest StorePath              -- Request to build a derivation by path
    | BuildResponse BuildResult           -- Response with build result
    | StoreWriteRequest StorePath BS.ByteString  -- Request to write to store
    | StoreWriteResponse StorePath        -- Response after store write
    | StoreReadRequest StorePath          -- Request to read from store
    | StoreReadResponse (Maybe BS.ByteString)  -- Response with store contents
    | ErrorMessage Text                   -- Error message
    deriving (Show, Eq)

-- JSON instances for secure serialization
instance Aeson.ToJSON DaemonMessage where
    toJSON (BuildRequest path) = Aeson.object [
        "type" .= ("build" :: Text),
        "path" .= storePathToText path
        ]
    toJSON (BuildResponse result) = Aeson.object [
        "type" .= ("buildResult" :: Text),
        "result" .= Aeson.toJSON result
        ]
    toJSON (StoreWriteRequest path content) = Aeson.object [
        "type" .= ("storeWrite" :: Text),
        "path" .= storePathToText path,
        "contentLength" .= BS.length content
        -- Content is sent separately as binary
        ]
    toJSON (StoreWriteResponse path) = Aeson.object [
        "type" .= ("storeWriteResponse" :: Text),
        "path" .= storePathToText path
        ]
    toJSON (StoreReadRequest path) = Aeson.object [
        "type" .= ("storeRead" :: Text),
        "path" .= storePathToText path
        ]
    toJSON (StoreReadResponse Nothing) = Aeson.object [
        "type" .= ("storeReadResponse" :: Text),
        "exists" .= False
        ]
    toJSON (StoreReadResponse (Just content)) = Aeson.object [
        "type" .= ("storeReadResponse" :: Text),
        "exists" .= True,
        "contentLength" .= BS.length content
        -- Content is sent separately as binary
        ]
    toJSON (ErrorMessage msg) = Aeson.object [
        "type" .= ("error" :: Text),
        "message" .= msg
        ]

instance Aeson.FromJSON DaemonMessage where
    parseJSON = Aeson.withObject "DaemonMessage" $ \v -> do
        msgType <- v .: "type" :: Aeson.Parser Text
        case msgType of
            "build" -> BuildRequest <$> (parseStorePath =<< v .: "path")
            "buildResult" -> BuildResponse <$> v .: "result"
            "storeWrite" -> do
                path <- parseStorePath =<< v .: "path"
                -- Content would be read separately as binary
                return $ StoreWriteRequest path BS.empty
            "storeWriteResponse" -> StoreWriteResponse <$> (parseStorePath =<< v .: "path")
            "storeRead" -> StoreReadRequest <$> (parseStorePath =<< v .: "path")
            "storeReadResponse" -> do
                exists <- v .: "exists"
                if exists
                    then return $ StoreReadResponse (Just BS.empty) -- Content read separately
                    else return $ StoreReadResponse Nothing
            "error" -> ErrorMessage <$> v .: "message"
            _ -> fail $ "Unknown message type: " ++ T.unpack msgType
      where
        parseStorePath txt = case textToStorePath txt of
            Just p -> return p
            Nothing -> fail $ "Invalid store path: " ++ T.unpack txt

-- | JSON instance for BuildResult
instance Aeson.ToJSON BuildResult where
    toJSON BuildResult{..} = Aeson.object [
        "outputPaths" .= map storePathToText (Set.toList brOutputPaths),
        "exitCode" .= exitCodeToJSON brExitCode,
        "log" .= brLog,
        "references" .= map storePathToText (Set.toList brReferences)
        ]
      where
        exitCodeToJSON ExitSuccess = Aeson.object [
            "success" .= True,
            "code" .= (0 :: Int)
            ]
        exitCodeToJSON (ExitFailure code) = Aeson.object [
            "success" .= False,
            "code" .= code
            ]

instance Aeson.FromJSON BuildResult where
    parseJSON = Aeson.withObject "BuildResult" $ \v -> do
        paths <- v .: "outputPaths" :: Aeson.Parser [Text]
        outputPaths <- mapM parseStorePath paths
        exitCodeObj <- v .: "exitCode"
        exitCode <- parseExitCode exitCodeObj
        log <- v .: "log"
        refs <- v .: "references" :: Aeson.Parser [Text]
        references <- mapM parseStorePath refs
        return BuildResult {
            brOutputPaths = Set.fromList outputPaths,
            brExitCode = exitCode,
            brLog = log,
            brReferences = Set.fromList references
            }
      where
        parseStorePath txt = case textToStorePath txt of
            Just p -> return p
            Nothing -> fail $ "Invalid store path: " ++ T.unpack txt

        parseExitCode = Aeson.withObject "ExitCode" $ \obj -> do
            success <- obj .: "success"
            if success
                then return ExitSuccess
                else ExitFailure <$> obj .: "code"

-- | Implementation to serialize a derivation to JSON
serializeDerivation :: Derivation -> BS.ByteString
serializeDerivation deriv =
    LBS.toStrict $ Aeson.encode $ Aeson.object [
        "name" .= derivName deriv,
        "hash" .= derivHash deriv,
        "builder" .= storePathToText (derivBuilder deriv),
        "args" .= derivArgs deriv,
        "inputs" .= map serializeInput (Set.toList $ derivInputs deriv),
        "outputs" .= map serializeOutput (Set.toList $ derivOutputs deriv),
        "env" .= derivEnv deriv,
        "system" .= derivSystem deriv,
        "strategy" .= (if derivStrategy deriv == ApplicativeStrategy then "applicative" else "monadic" :: Text),
        "meta" .= derivMeta deriv
    ]
  where
    serializeInput (DerivationInput path name) = Aeson.object [
        "path" .= storePathToText path,
        "name" .= name
        ]
    serializeOutput (DerivationOutput name path) = Aeson.object [
        "name" .= name,
        "path" .= storePathToText path
        ]

-- | Implementation to deserialize a derivation from JSON
deserializeDerivation :: BS.ByteString -> Either Text Derivation
deserializeDerivation bs = case Aeson.eitherDecodeStrict bs of
    Left err -> Left $ "JSON parse error: " <> T.pack err
    Right obj -> parseDerivation obj
  where
    parseDerivation :: Aeson.Value -> Either Text Derivation
    parseDerivation = Aeson.parseEither $ \obj -> do
        name <- obj .: "name"
        hash <- obj .: "hash"
        builderText <- obj .: "builder"
        builder <- case textToStorePath builderText of
            Just p -> return p
            Nothing -> fail "Invalid builder path"
        args <- obj .: "args"
        inputObjs <- obj .: "inputs"
        inputs <- Set.fromList <$> mapM parseInput inputObjs
        outputObjs <- obj .: "outputs"
        outputs <- Set.fromList <$> mapM parseOutput outputObjs
        env <- obj .: "env"
        system <- obj .: "system"
        strategyText <- obj .: "strategy"
        let strategy = if strategyText == "applicative" then ApplicativeStrategy else MonadicStrategy
        meta <- obj .: "meta"
        return Derivation {
            derivName = name,
            derivHash = hash,
            derivBuilder = builder,
            derivArgs = args,
            derivInputs = inputs,
            derivOutputs = outputs,
            derivEnv = env,
            derivSystem = system,
            derivStrategy = strategy,
            derivMeta = meta
        }

    parseInput = Aeson.withObject "DerivationInput" $ \obj -> do
        pathText <- obj .: "path"
        path <- case textToStorePath pathText of
            Just p -> return p
            Nothing -> fail "Invalid input path"
        name <- obj .: "name"
        return $ DerivationInput path name

    parseOutput = Aeson.withObject "DerivationOutput" $ \obj -> do
        name <- obj .: "name"
        pathText <- obj .: "path"
        path <- case textToStorePath pathText of
            Just p -> return p
            Nothing -> fail "Invalid output path"
        return $ DerivationOutput name path

-- | Implementation to serialize a build result
serializeBuildResult :: BuildResult -> BS.ByteString
serializeBuildResult = LBS.toStrict . Aeson.encode

-- | Implementation to deserialize a build result
deserializeBuildResult :: BS.ByteString -> Either Text BuildResult
deserializeBuildResult bs = case Aeson.eitherDecodeStrict bs of
    Left err -> Left $ "JSON parse error: " <> T.pack err
    Right result -> Right result

-- | Store a derivation in the store (Daemon privilege)
storeDerivation :: Derivation -> TenM 'Eval 'Daemon StorePath
storeDerivation deriv = do
    env <- ask

    -- Serialize the derivation to JSON
    let serialized = serializeDerivation deriv

    -- Calculate a hash for the content
    let contentHash = hashByteString serialized
    let hashHex = T.pack $ showHex contentHash

    -- Determine store path
    let name = derivName deriv <> ".drv"
    let storePath = StorePath hashHex name

    -- Write to the store
    let fullPath = storePathToFilePath storePath env

    -- Create parent directories
    liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)

    -- Write the file with proper permissions
    liftIO $ BS.writeFile fullPath serialized
    liftIO $ setFileMode fullPath 0o444  -- Read-only for everyone

    logMsg 1 $ "Stored derivation at " <> storePathToText storePath

    -- Return the store path
    return storePath
  where
    showHex :: Int -> String
    showHex n = if n < 0 then error "Cannot show negative hash" else show n

    hashByteString :: BS.ByteString -> Int
    hashByteString bs = fromIntegral $ BS.foldl' (\acc byte -> acc * 33 + fromIntegral byte) 5381 bs

-- | Read a derivation from the store (any privilege tier through protocol)
readDerivation :: StorePath -> TenM p t (Either BuildError Derivation)
readDerivation path = do
    env <- ask

    case currentPrivilegeTier env of
        -- Direct store access in daemon context
        Daemon -> do
            let fullPath = storePathToFilePath path env

            -- Check if file exists
            exists <- liftIO $ doesFileExist fullPath
            if not exists
                then return $ Left $ StoreError $ "Derivation not found: " <> storePathToText path
                else do
                    -- Read and deserialize
                    content <- liftIO $ BS.readFile fullPath
                    case deserializeDerivation content of
                        Left err -> return $ Left $ SerializationError $
                                          "Failed to deserialize derivation: " <> err
                        Right deriv -> return $ Right deriv

        -- Must use protocol in builder context
        Builder -> case runMode env of
            ClientMode conn -> do
                -- Use daemon protocol to read derivation
                result <- requestFromDaemon conn (StoreReadRequest path)
                case result of
                    StoreReadResponse (Just content) ->
                        case deserializeDerivation content of
                            Left err -> return $ Left $ SerializationError $
                                              "Failed to deserialize derivation: " <> err
                            Right deriv -> return $ Right deriv
                    StoreReadResponse Nothing ->
                        return $ Left $ StoreError $ "Derivation not found: " <> storePathToText path
                    ErrorMessage msg ->
                        return $ Left $ StoreError msg
                    _ -> return $ Left $ ProtocolError "Unexpected response from daemon"

            _ -> return $ Left $ PrivilegeError
                             "Cannot read derivation in builder context without daemon connection"

-- | Store a build result (Daemon privilege)
storeBuildResult :: BuildId -> BuildResult -> TenM 'Build 'Daemon StorePath
storeBuildResult buildId result = do
    env <- ask

    -- Serialize the result to JSON
    let serialized = serializeBuildResult result

    -- Calculate a hash for the content
    let contentHash = hashByteString serialized
    let hashHex = T.pack $ showHex contentHash

    -- Determine store path
    let name = T.pack (show buildId) <> "-result.json"
    let storePath = StorePath hashHex name

    -- Write to the store
    let fullPath = storePathToFilePath storePath env

    -- Create parent directories
    liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)

    -- Write the file with proper permissions
    liftIO $ BS.writeFile fullPath serialized
    liftIO $ setFileMode fullPath 0o444  -- Read-only for everyone

    logMsg 1 $ "Stored build result at " <> storePathToText storePath

    -- Return the store path
    return storePath
  where
    showHex :: Int -> String
    showHex n = if n < 0 then error "Cannot show negative hash" else show n

    hashByteString :: BS.ByteString -> Int
    hashByteString bs = fromIntegral $ BS.foldl' (\acc byte -> acc * 33 + fromIntegral byte) 5381 bs

-- | Read a build result from the store (any privilege tier through protocol)
readBuildResult :: StorePath -> TenM p t (Either BuildError BuildResult)
readBuildResult path = do
    env <- ask

    case currentPrivilegeTier env of
        -- Direct store access in daemon context
        Daemon -> do
            let fullPath = storePathToFilePath path env

            -- Check if file exists
            exists <- liftIO $ doesFileExist fullPath
            if not exists
                then return $ Left $ StoreError $ "Build result not found: " <> storePathToText path
                else do
                    -- Read and deserialize
                    content <- liftIO $ BS.readFile fullPath
                    case deserializeBuildResult content of
                        Left err -> return $ Left $ SerializationError $
                                          "Failed to deserialize build result: " <> err
                        Right result -> return $ Right result

        -- Must use protocol in builder context
        Builder -> case runMode env of
            ClientMode conn -> do
                -- Use daemon protocol to read build result
                result <- requestFromDaemon conn (StoreReadRequest path)
                case result of
                    StoreReadResponse (Just content) ->
                        case deserializeBuildResult content of
                            Left err -> return $ Left $ SerializationError $
                                              "Failed to deserialize build result: " <> err
                            Right buildResult -> return $ Right buildResult
                    StoreReadResponse Nothing ->
                        return $ Left $ StoreError $ "Build result not found: " <> storePathToText path
                    ErrorMessage msg ->
                        return $ Left $ StoreError msg
                    _ -> return $ Left $ ProtocolError "Unexpected response from daemon"

            _ -> return $ Left $ PrivilegeError
                             "Cannot read build result in builder context without daemon connection"

-- | Send a request to the daemon and get a response (Builder context)
requestFromDaemon :: DaemonConnection -> DaemonMessage -> TenM p 'Builder DaemonMessage
requestFromDaemon conn msg = do
    reqId <- liftIO $ atomically $ do
        rid <- readTVar (connNextReqId conn)
        writeTVar (connNextReqId conn) (rid + 1)
        return rid

    -- Create a response MVar
    respVar <- liftIO $ newEmptyMVar

    -- Register the request
    liftIO $ atomically $ modifyTVar' (connRequestMap conn) $ Map.insert reqId respVar

    -- Serialize the message
    let msgJson = Aeson.encode msg
    let msgLen = LBS.length msgJson
    let lenBytes = Aeson.encode msgLen

    -- Send the message with length prefix
    liftIO $ LBS.hPut (connHandle conn) lenBytes
    liftIO $ LBS.hPut (connHandle conn) msgJson
    liftIO $ hFlush (connHandle conn)

    -- Handle binary content if present
    case msg of
        StoreWriteRequest _ content -> do
            -- Send binary content separately
            liftIO $ BS.hPut (connHandle conn) content
            liftIO $ hFlush (connHandle conn)
        _ -> return ()

    -- Wait for response with timeout (5 minutes for build)
    let timeout = case msg of
            BuildRequest _ -> 5 * 60 * 1000000  -- 5 min for builds
            _ -> 30 * 1000000  -- 30 sec for everything else

    mResp <- liftIO $ withTimeout timeout $ takeMVar respVar

    -- Clean up the request mapping
    liftIO $ atomically $ modifyTVar' (connRequestMap conn) $ Map.delete reqId

    case mResp of
        Nothing -> throwError $ DaemonError "Timeout waiting for daemon response"
        Just resp -> do
            -- Handle binary content in response
            case resp of
                StoreReadResponse (Just _) -> do
                    -- Read binary content based on length specified in headers
                    -- This is simplified - in a real impl you'd extract the length
                    content <- liftIO $ BS.hGet (connHandle conn) 4096
                    return $ StoreReadResponse (Just content)
                _ -> return resp

-- | Helper for timeouts
withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout micros action = do
    result <- newEmptyMVar
    tid <- forkIO $ do
        r <- action
        putMVar result r

    -- Wait for result or timeout
    threadDelay micros
    gotResult <- isJust <$> tryTakeMVar result

    unless gotResult $ killThread tid

    if gotResult
        then Just <$> readMVar result
        else return Nothing

-- | Spawn a builder process with dropped privileges (Daemon context)
spawnBuilder :: BuildId -> StorePath -> TenM 'Build 'Daemon (Either BuildError BuildResult)
spawnBuilder buildId derivPath = do
    env <- ask

    -- Create pipes for communication
    (readFd, writeFd) <- liftIO $ createPipe
    readHandle <- liftIO $ fdToHandle readFd
    writeHandle <- liftIO $ fdToHandle writeFd

    -- Get builder script location
    let builderScript = storeLocation env </> "libexec/ten-builder"

    -- Create a unique work directory
    let workDir' = workDir env </> "builders" </> show buildId
    liftIO $ createDirectoryIfMissing True workDir'

    -- Determine user/group to run as
    targetUser <- case userName env of
        Just name -> return $ T.unpack name
        Nothing -> return "nobody"  -- Default to nobody

    -- Prepare environment variables for the builder
    baseEnv <- liftIO getEnvironment
    let builderEnv = [
            ("TEN_STORE", storeLocation env),
            ("TEN_WORK_DIR", workDir'),
            ("TEN_BUILD_ID", show buildId),
            ("TEN_DERIVATION_PATH", storePathToFilePath derivPath env)
            ] ++ baseEnv

    -- Fork a child process
    pid <- liftIO $ forkProcess $ do
        -- In child process

        -- Close read end of pipe
        liftIO $ closeFd readFd

        -- Drop privileges if running as root
        uid <- liftIO User.getRealUserID
        when (uid == 0) $ do
            targetUid <- safeGetUserUID targetUser
            targetGid <- safeGetGroupGID "nogroup"  -- Default to nogroup

            -- Set groups first, then user
            liftIO $ User.setGroupID targetGid
            liftIO $ User.setUserID targetUid

        -- Execute the builder with the derivation path
        liftIO $ executeFile builderScript True [
            "--derivation", storePathToFilePath derivPath env,
            "--build-id", show buildId,
            "--store", storeLocation env
            ] (Just builderEnv)

        -- Should never get here
        liftIO $ exitWith $ ExitFailure 1

    -- Close write end of pipe in parent
    liftIO $ closeFd writeFd

    -- Wait for child to complete and collect output
    result <- liftIO $ waitForChildProcess pid readHandle

    liftIO $ hClose readHandle

    -- Process the result
    case result of
        Left err -> return $ Left $ BuildFailed $ "Builder process failed: " <> err
        Right output -> do
            -- Parse the output as a BuildResult
            case deserializeBuildResult output of
                Left err -> return $ Left $ SerializationError $
                              "Failed to parse builder output: " <> err
                Right buildResult -> return $ Right buildResult

-- | Wait for a child process and collect output
waitForChildProcess :: ProcessID -> Handle -> IO (Either Text BS.ByteString)
waitForChildProcess pid handle = do
    -- Read output from the handle
    output <- BS.hGetContents handle

    -- Wait for process to exit
    status <- getProcessStatus True True pid

    case status of
        Just (Exited ExitSuccess) -> return $ Right output
        Just (Exited (ExitFailure code)) -> return $ Left $ "Process exited with status " <> T.pack (show code)
        Just (Terminated signum _) -> return $ Left $ "Process terminated by signal " <> T.pack (show signum)
        Just (Stopped signum) -> return $ Left $ "Process stopped by signal " <> T.pack (show signum)
        Nothing -> return $ Left "Failed to get process status"

-- | Run a function as an unprivileged builder (replaces withPrivilegeTransition)
runAsBuilder :: String -> DaemonConfig -> StorePath -> IO (Either BuildError BuildResult)
runAsBuilder userName config derivPath = do
    -- Generate a build ID
    buildId <- randomBuildId

    -- Create a work directory
    let workDir' = "/tmp/ten-builder-" ++ show buildId
    createDirectoryIfMissing True workDir'

    -- Prepare environment variables
    env <- getEnvironment
    let builderEnv = [
            ("TEN_STORE", daemonStorePath config),
            ("TEN_WORK_DIR", workDir'),
            ("TEN_BUILD_ID", show buildId),
            ("TEN_DERIVATION_PATH", daemonStorePath config </> T.unpack (storeHashText derivPath) ++ "-" ++ T.unpack (storeName derivPath))
            ] ++ env

    -- Get builder script location
    let builderScript = daemonStorePath config </> "libexec/ten-builder"

    -- Create pipes for IPC
    (readFd, writeFd) <- createPipe
    readHandle <- fdToHandle readFd
    writeHandle <- fdToHandle writeFd

    -- Fork a child process
    pid <- forkProcess $ do
        -- In child process

        -- Close read end of pipe
        closeFd readFd

        -- Drop privileges if running as root
        uid <- User.getRealUserID
        when (uid == 0) $ do
            targetUid <- safeGetUserUID userName
            targetGid <- safeGetGroupGID "nogroup"  -- Default to nogroup

            -- Set groups first, then user
            User.setGroupID targetGid
            User.setUserID targetUid

        -- Execute the builder with the derivation path
        executeFile builderScript True [
            "--derivation", daemonStorePath config </> T.unpack (storeHashText derivPath) ++ "-" ++ T.unpack (storeName derivPath),
            "--build-id", show buildId,
            "--store", daemonStorePath config
            ] (Just builderEnv)

        -- Should never get here
        exitWith $ ExitFailure 1

    -- Close write end of pipe in parent
    closeFd writeFd

    -- Wait for child to complete and collect output
    result <- waitForChildProcess pid readHandle

    hClose readHandle

    -- Clean up work directory
    removePathForcibly workDir' `catch` (\(_ :: SomeException) -> return ())

    -- Return the result
    case result of
        Left err -> return $ Left $ BuildFailed $ "Builder process failed: " <> err
        Right output ->
            case deserializeBuildResult output of
                Left err -> return $ Left $ SerializationError $
                              "Failed to parse builder output: " <> err
                Right buildResult -> return $ Right buildResult
  where
    storeHashText :: StorePath -> Text
    storeHashText = storeHash

    randomBuildId :: IO BuildId
    randomBuildId = BuildIdFromInt <$> randomRIO (1000, 9999999)

-- | Safely get a user's UID with fallback options
safeGetUserUID :: String -> IO UserID
safeGetUserUID username = do
    result <- try $ User.getUserEntryForName username
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nobody
            hPutStrLn stderr $ "Warning: User " ++ username ++ " not found, falling back to nobody"
            nobodyResult <- try $ User.getUserEntryForName "nobody"
            case nobodyResult of
                Left (_ :: SomeException) -> do
                    -- Ultimate fallback to current user if even nobody doesn't exist
                    User.getRealUserID
                Right entry ->
                    return $ User.userID entry
        Right entry ->
            return $ User.userID entry

-- | Safely get a group's GID with fallback options
safeGetGroupGID :: String -> IO GroupID
safeGetGroupGID groupname = do
    result <- try $ User.getGroupEntryForName groupname
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nogroup
            hPutStrLn stderr $ "Warning: Group " ++ groupname ++ " not found, falling back to nogroup"
            nogroupResult <- try $ User.getGroupEntryForName "nogroup"
            case nogroupResult of
                Left (_ :: SomeException) -> do
                    -- Try nobody as group
                    nobodyResult <- try $ User.getGroupEntryForName "nobody"
                    case nobodyResult of
                        Left (_ :: SomeException) -> do
                            -- Ultimate fallback to current group
                            uid <- User.getRealUserID
                            entry <- User.getUserEntryForID uid
                            return $ User.userGroupID entry
                        Right entry ->
                            return $ User.groupID entry
                Right entry ->
                    return $ User.groupID entry
        Right entry ->
            return $ User.groupID entry

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
