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

module Ten.Core (
    -- Core types
    Phase(..),
    PrivilegeContext(..),
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

    -- Type classes for type-level context mapping
    GetContextFromType(..),
    GetPhaseFromType(..),

    -- Return-Continuation types
    PhaseTransition(..),
    transitionPhase,

    -- Privilege context transitions
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
    initBuildState,

    -- Monad operations
    runTen,
    evalTen,
    buildTen,
    runTenDaemon,
    runTenBuilder,
    runTenIO,
    liftTenIO,
    liftPrivilegedIO,
    liftUnprivilegedIO,
    logMsg,
    assertTen,

    -- Type class constraints
    EvalPhase(..),
    BuildPhase(..),
    DaemonContext(..),
    BuilderContext(..),

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
    getSystemGroup,

    -- GC lock path helpers
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
import System.Posix.User (getUserEntryForName, getGroupEntryForName,
                         setUserID, setGroupID, getEffectiveUserID,
                         userID, groupID)
import System.Posix.Types (UserID, GroupID)
import System.IO.Error (isDoesNotExistError)
import Control.Exception (bracket, try, catch, throwIO, SomeException, Exception, ErrorCall(..))
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

-- | Build phases for type-level separation between evaluation and execution
data Phase = Eval | Build
    deriving (Show, Eq)

-- | Privilege context for type-level separation between daemon and builder
data PrivilegeContext = Privileged | Unprivileged
    deriving (Show, Eq)

-- | Type class for mapping context types to runtime values
class GetContextFromType (ctx :: PrivilegeContext) where
    getContextFromType :: Proxy ctx -> PrivilegeContext

instance GetContextFromType 'Privileged where
    getContextFromType _ = Privileged

instance GetContextFromType 'Unprivileged where
    getContextFromType _ = Unprivileged

-- | Type class for mapping phase types to runtime values
class GetPhaseFromType (p :: Phase) where
    getPhaseFromType :: Proxy p -> Phase

instance GetPhaseFromType 'Eval where
    getPhaseFromType _ = Eval

instance GetPhaseFromType 'Build where
    getPhaseFromType _ = Build

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
data PrivilegeTransition (p :: PrivilegeContext) (q :: PrivilegeContext) where
    DropPrivilege :: PrivilegeTransition 'Privileged 'Unprivileged
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
    csRequestMap :: TVar (Map RequestId (MVar Response)), -- ^ Map of pending requests
    csNextReqId :: TVar RequestId,          -- ^ Next request ID
    csReaderThread :: ThreadId,             -- ^ Thread ID of the response reader
    csShutdown :: TVar Bool                 -- ^ Flag to indicate connection shutdown
}

-- | Request ID type
type RequestId = Int

-- | Request type (for communication with daemon)
data Request = Request deriving (Show, Eq)

-- | Response type (for communication with daemon)
data Response = Response deriving (Show, Eq)

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
    , gcLockPath :: FilePath             -- Path to GC lock file
    , privilegeContext :: PrivilegeContext -- Current privilege context
    } deriving (Show, Eq)

-- | State carried through build operations
data BuildState = BuildState
    { currentPhase :: Phase              -- Current execution phase
    , buildProofs :: [Proof 'Build]      -- Accumulated proofs
    , buildInputs :: Set StorePath       -- Input paths for current build
    , buildOutputs :: Set StorePath      -- Output paths for current build
    , currentBuildId :: BuildId          -- Current build identifier
    , buildChain :: [Derivation]         -- Chain of return-continuation derivations
    , recursionDepth :: Int              -- Tracks recursion depth for cycles
    } deriving (Show)

-- | The core monad for all Ten operations
newtype TenM (p :: Phase) (ctx :: PrivilegeContext) a = TenM
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

-- Add MonadFail instance for TenM - critical for pattern matching in do-notation
instance MonadFail (TenM p ctx) where
    fail msg = throwError $ BuildFailed $ T.pack msg

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
    , gcLockPath = sp </> "var/ten/gc.lock"
    , privilegeContext = Unprivileged  -- Default to unprivileged for safety
    }

-- | Initialize client build environment
initClientEnv :: FilePath -> FilePath -> DaemonConnection -> BuildEnv
initClientEnv wd sp conn = (initBuildEnv wd sp)
    { runMode = ClientMode conn
    , privilegeContext = Unprivileged  -- Clients are always unprivileged
    }

-- | Initialize daemon build environment
initDaemonEnv :: FilePath -> FilePath -> Maybe Text -> BuildEnv
initDaemonEnv wd sp user = (initBuildEnv wd sp)
    { runMode = DaemonMode
    , userName = user
    , privilegeContext = Privileged  -- Daemon runs in privileged context
    }

-- | Initialize build state for a given phase with a BuildId
initBuildState :: Phase -> BuildId -> BuildState
initBuildState phase bid = BuildState
    { currentPhase = phase
    , buildProofs = []
    , buildInputs = Set.empty
    , buildOutputs = Set.empty
    , currentBuildId = bid
    , buildChain = []
    , recursionDepth = 0
    }

-- | Execute a Ten monad in the given environment and state
runTen :: forall p ctx a. GetContextFromType ctx
       => TenM p ctx a -> BuildEnv -> BuildState -> IO (Either BuildError (a, BuildState))
runTen m env state = do
    -- Validate privilege context matches
    if privilegeContext env == getContextFromType (Proxy :: Proxy ctx)
        then runExceptT $ runStateT (runReaderT (runTenM m) env) state
        else return $ Left $ PrivilegeError "Privilege context mismatch"

-- | Execute an evaluation-phase computation in privileged mode
evalTen :: TenM 'Eval 'Privileged a -> BuildEnv -> IO (Either BuildError (a, BuildState))
evalTen m env = do
    -- Ensure environment is privileged
    let env' = env { privilegeContext = Privileged }
    bid <- BuildId <$> newUnique
    runTen m env' (initBuildState Eval bid)

-- | Execute a build-phase computation in unprivileged mode
buildTen :: TenM 'Build 'Unprivileged a -> BuildEnv -> IO (Either BuildError (a, BuildState))
buildTen m env = do
    -- Ensure environment is unprivileged
    let env' = env { privilegeContext = Unprivileged }
    bid <- BuildId <$> newUnique
    runTen m env' (initBuildState Build bid)

-- | Execute a daemon operation (privileged)
runTenDaemon :: forall p a. GetPhaseFromType p
             => TenM p 'Privileged a -> BuildEnv -> IO (Either BuildError (a, BuildState))
runTenDaemon m env = do
    -- Verify we're in daemon mode
    case runMode env of
        DaemonMode -> do
            -- Set privileged context
            let env' = env { privilegeContext = Privileged }
            bid <- BuildId <$> newUnique
            runTen m env' (initBuildState (getPhaseFromType (Proxy :: Proxy p)) bid)
        _ -> return $ Left $ PrivilegeError "Cannot run daemon operation in non-daemon mode"

-- | Execute a builder operation (unprivileged)
runTenBuilder :: forall p a. GetPhaseFromType p
              => TenM p 'Unprivileged a -> BuildEnv -> IO (Either BuildError (a, BuildState))
runTenBuilder m env = do
    -- Set unprivileged context
    let env' = env { privilegeContext = Unprivileged }
    bid <- BuildId <$> newUnique
    runTen m env' (initBuildState (getPhaseFromType (Proxy :: Proxy p)) bid)

-- | Run an IO operation within the TenM monad
runTenIO :: IO a -> TenM p ctx a
runTenIO = liftIO

-- | Lift an IO action that returns (Either BuildError a) into TenM
liftTenIO :: IO (Either BuildError a) -> TenM p ctx a
liftTenIO action = do
    result <- liftIO action
    case result of
        Left err -> throwError err
        Right val -> return val

-- | Lift an IO action into the privileged context
-- This will fail at runtime if executed in an unprivileged context
liftPrivilegedIO :: IO a -> TenM p 'Privileged a
liftPrivilegedIO = liftIO

-- | Lift an IO action into the unprivileged context
-- This is safer since it works in both contexts
liftUnprivilegedIO :: IO a -> TenM p ctx a
liftUnprivilegedIO action = do
    ctx <- asks privilegeContext
    case ctx of
        Privileged -> liftIO action
        Unprivileged -> liftIO action

-- | Safely transition between phases
transitionPhase :: PhaseTransition p q -> TenM p ctx a -> TenM q ctx a
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

-- | Safely transition between privilege contexts
-- Only allows dropping privileges, never gaining them
transitionPrivilege :: PrivilegeTransition ctx ctx' -> TenM p ctx a -> TenM p ctx' a
transitionPrivilege trans action = TenM $ do
    env <- ask
    state <- get

    case trans of
        DropPrivilege -> do
            -- Run action in privileged context with original state
            let env' = env { privilegeContext = Unprivileged }
            result <- liftIO $ runTen action env state
            case result of
                Left err -> throwError err
                Right (val, newState) -> do
                    -- Update state with new values but keep privilege
                    put newState
                    return val

-- | Execute an action with a privilege transition
withPrivilegeTransition :: PrivilegeTransition ctx ctx' -> (TenM p ctx' a -> TenM p ctx' a) -> TenM p ctx a -> TenM p ctx' a
withPrivilegeTransition trans wrapper action = wrapper (transitionPrivilege trans action)

-- | Generate a new unique build ID
newBuildId :: TenM p ctx BuildId
newBuildId = liftIO $ BuildId <$> newUnique

-- | Set the current build ID
setCurrentBuildId :: BuildId -> TenM p ctx ()
setCurrentBuildId bid = modify $ \s -> s { currentBuildId = bid }

-- | Add a derivation to the build chain
addToDerivationChain :: Derivation -> TenM p ctx ()
addToDerivationChain drv = do
    depth <- gets recursionDepth
    maxDepth <- asks maxRecursionDepth
    when (depth >= maxDepth) $
        throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show maxDepth)
    modify $ \s -> s { buildChain = drv : buildChain s, recursionDepth = depth + 1 }

-- | Check if a derivation is in the build chain (cycle detection)
isInDerivationChain :: Derivation -> TenM p ctx Bool
isInDerivationChain drv = do
    chain <- gets buildChain
    return $ any (derivationEquals drv) chain

-- | Logging function
logMsg :: Int -> Text -> TenM p ctx ()
logMsg level msg = do
    v <- asks verbosity
    when (v >= level) $ liftIO $ putStrLn $ T.unpack msg

-- | Record a proof in the build state
addProof :: Proof p -> TenM ph ctx ()
addProof proof = case proof of
    p@BuildProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@OutputProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@ReturnProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@RecursionProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@GCRootProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@ReachableProof{} -> modify $ \s -> s { buildProofs = p : buildProofs s }
    p@(ComposeProof p1 p2) -> do
        addProof p1
        addProof p2
    _ -> return () -- Other proofs don't affect build state

-- | Assert that a condition holds, or throw an error
assertTen :: Bool -> BuildError -> TenM p ctx ()
assertTen condition err = unless condition $ throwError err

-- | Operations only allowed in evaluation phase
class EvalPhase (p :: Phase) (ctx :: PrivilegeContext) where
    evalOnly :: TenM 'Eval ctx a -> TenM p ctx a

-- Only actual evaluation phase can run evaluation operations
instance EvalPhase 'Eval ctx where
    evalOnly = id

-- | Operations only allowed in build phase
class BuildPhase (p :: Phase) (ctx :: PrivilegeContext) where
    buildOnly :: TenM 'Build ctx a -> TenM p ctx a

-- Only actual build phase can run build operations
instance BuildPhase 'Build ctx where
    buildOnly = id

-- | Operations only allowed in daemon context
class DaemonContext (ctx :: PrivilegeContext) where
    daemonOnly :: TenM p 'Privileged a -> TenM p ctx a

-- Only daemon context can run privileged operations
instance DaemonContext 'Privileged where
    daemonOnly = id

-- | Operations only allowed in builder context
class BuilderContext (ctx :: PrivilegeContext) where
    builderOnly :: TenM p 'Unprivileged a -> TenM p ctx a

-- Both contexts can run unprivileged operations, but differently
instance BuilderContext 'Unprivileged where
    builderOnly = id

instance BuilderContext 'Privileged where
    builderOnly action = TenM $ do
        env <- ask
        state <- get

        -- Create environment with dropped privileges
        let env' = env { privilegeContext = Unprivileged }

        -- Run the unprivileged action in that environment
        result <- liftIO $ runTen action env' state

        -- Bring the result back to the privileged context
        case result of
            Left err -> throwError err
            Right (val, newState) -> do
                put newState
                return val

-- | Run an STM transaction from Ten monad
atomicallyTen :: STM a -> TenM p ctx a
atomicallyTen = liftIO . atomically

-- | Execute a build operation using the daemon (if in client mode)
withDaemon :: (DaemonConnection -> IO (Either BuildError a)) -> TenM p ctx a
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
isDaemonMode :: TenM p ctx Bool
isDaemonMode = (== DaemonMode) <$> asks runMode

-- | Check if connected to daemon
isClientMode :: TenM p ctx Bool
isClientMode = do
    mode <- asks runMode
    case mode of
        ClientMode _ -> return True
        _ -> return False

-- | Linux-specific: Drop privileges to a specified user and group
dropPrivileges :: Text -> Text -> TenM p 'Privileged ()
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
getSystemUser :: Text -> TenM p 'Privileged UserID
getSystemUser username = liftIO $ do
    userEntry <- getUserEntryForName (T.unpack username)
    return $ userID userEntry

-- | Linux-specific: Get a system group ID
getSystemGroup :: Text -> TenM p 'Privileged GroupID
getSystemGroup groupname = liftIO $ do
    groupEntry <- getGroupEntryForName (T.unpack groupname)
    return $ groupID groupEntry

-- | Get the GC lock path from the BuildEnv
getGCLockPath :: BuildEnv -> FilePath
getGCLockPath = gcLockPath

-- | Ensure the directory for a lock file exists
ensureLockDirExists :: FilePath -> IO ()
ensureLockDirExists lockPath = do
    let dir = takeDirectory lockPath
    createDirectoryIfMissing True dir
    -- Set appropriate permissions (0755 - rwxr-xr-x)
    setFileMode dir 0o755

-- | Compare two derivations for equality
derivationEquals :: Derivation -> Derivation -> Bool
derivationEquals d1 d2 = derivHash d1 == derivHash d2

-- | Compare StorePaths for equality
derivationPathsEqual :: StorePath -> StorePath -> Bool
derivationPathsEqual p1 p2 = storeHash p1 == storeHash p2 && storeName p1 == storeName p2

-- | Helper for privilege errors
privilegeError :: Text -> BuildError
privilegeError = PrivilegeError
