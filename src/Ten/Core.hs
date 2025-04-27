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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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
    BuildRequestInfo(..),
    BuildStatus(..),
    BuildStrategy(..),
    RunMode(..),
    runMode,
    DaemonConfig(..),
    UserId(..),
    AuthToken(..),

    -- Protocol Connection types
    DaemonConnection(..),
    Request(..),
    RequestId(..),
    Response(..),
    Message(..),
    MessageType(..),
    AuthResult(..),
    Capability(..),
    ProtocolVersion(..),
    currentProtocolVersion,

    -- Added Daemon Response types
    DaemonResponse(..),
    DerivationInfo(..),
    BuildStatusUpdate(..),
    GCStatusInfo(..),
    GCRequestParams(..),
    GCStatusRequestParams(..),
    DaemonStatus(..),
    DerivationInfoResponse(..),
    DerivationOutputMappingResponse(..),

    -- Protocol actions
    createDaemonConnection,
    closeDaemonConnection,
    withDaemonConnection,
    sendRequest,
    receiveRequest,
    receiveResponse,
    sendRequestSync,
    encodeRequest,
    decodeResponse,
    sendMessage,
    receiveMessage,

    -- Singletons and witnesses
    SingI(..),
    sing,
    fromSing,
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
    CanRunGC,

    -- Type classes for privilege and phase constraints
    RequiresDaemon,
    RequiresBuilder,
    RequiresPhase,
    RequiresEval,
    RequiresBuild,
    AnyPrivilegeTier,
    AnyPhase,

    -- Process boundary primitives
    storeDerivationDaemon,
    storeDerivationBuilder,
    readDerivationDaemon,
    readDerivationBuilder,
    storeBuildResultDaemon,
    readBuildResultDaemon,
    readBuildResultBuilder,

    -- Store types
    StorePath(..),
    storePathToText,
    textToStorePath,
    parseStorePath,
    parseStorePathText,
    validateStorePath,
    validateStorePathWithContext,

    -- Reference tracking types
    StoreReference(..),
    ReferenceType(..),
    StorePathReference(..),
    GCRoot(..),
    GCStats(..),
    RootType(..),

    -- Database paths
    defaultDBPath,

    -- Core derivation types
    Derivation(..),
    DerivationInput(..),
    DerivationOutput(..),
    derivationEquals,
    derivationPathsEqual,
    serializeDerivation,
    deserializeDerivation,

    -- Build result types
    BuildResult(..),

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
    runTenDaemonEval,
    runTenDaemonBuild,
    runTenBuilderEval,
    runTenBuilderBuild,
    runTenIO,
    liftTenIO,
    liftDaemonIO,
    liftBuilderIO,
    logMsg,
    assertTen,

    -- Build chain handling
    newBuildId,
    setCurrentBuildId,
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
    throwError,
    catchError,
    privilegeError,
    buildErrorToText,

    -- Builder process control
    spawnBuilderProcess,
    waitForBuilderProcess,

    -- Process isolation utilities
    withStore,

    -- Database
    Database(..),
    TransactionMode(..),

    -- Time utilities
    getCurrentMillis,
    timeout,

    -- State access helpers
    currentBuildId,
    verbosity,

    -- Protocol utilities
    parseRequestFrame,
    receiveFramedResponse,
    deserializeDaemonResponse,
    splitResponseBytes,
    extractRequestIdFromBytes,

    hashByteString
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
import Data.Scientific (toBoundedInteger)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified System.Timeout as SystemTimeout
import System.Directory
import System.FilePath
import qualified System.Process as Process
import System.Exit
import Data.Proxy (Proxy(..))
import Network.Socket (Socket, SockAddr(..), socketToHandle, close)
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush, hPutStrLn, stderr, stdin,
                 openFile, hGetLine, BufferMode(..), hSetBuffering)
import System.IO.Error (isDoesNotExistError, isPermissionError)
import Database.SQLite.Simple (Connection, Query(..), ToRow(..), FromRow(..), SQLData, Only(..))
import Data.Int (Int64)
import qualified System.Posix.User as User
import System.Posix.Files (fileExist, getFileStatus, isRegularFile, setFileMode)
import qualified System.Posix.IO as PosixIO
import System.IO.Error (isDoesNotExistError)
import Control.Exception (bracket, try, catch, throwIO, finally, mask, Exception, ErrorCall(..), SomeException)
import System.Environment (lookupEnv, getEnvironment)
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import qualified System.Posix.IO as PosixIO
import System.Posix.Types (ProcessID, Fd, FileMode, UserID, GroupID)
import Text.Read (readPrec, readMaybe)
import qualified Text.Read as Read
import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay, myThreadId)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Char (isHexDigit)
import Data.Singletons
import Data.Singletons.TH
import Data.Kind (Type)
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Types as Aeson (parseEither, parseMaybe)
import qualified Data.Aeson.KeyMap as AKeyMap
import qualified Data.Aeson.Key as Key
import Network.Socket.ByteString (sendAll, recv)
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import System.Random (randomRIO)
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Vector as Vector

-- | Build phases for type-level separation between evaluation and execution
data Phase = Eval | Build
    deriving (Show, Eq)

-- | Privilege tiers for type-level separation between daemon and builder processes
data PrivilegeTier = Daemon | Builder
    deriving (Show, Eq)

-- Add explicit Ord instance to enable PrivilegeTier to be used in Sets
-- This instance reflects the hierarchical nature of privileges: Daemon > Builder
instance Ord PrivilegeTier where
    compare Daemon Daemon = EQ
    compare Builder Builder = EQ
    compare Daemon Builder = GT  -- Daemon has higher privileges
    compare Builder Daemon = LT  -- Builder has lower privileges

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

type family CanRunGC (t :: PrivilegeTier) :: Bool where
    CanRunGC 'Daemon = 'True
    CanRunGC 'Builder = 'False

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

-- | Protocol version
data ProtocolVersion = ProtocolVersion {
    protocolMajor :: Int,
    protocolMinor :: Int,
    protocolPatch :: Int
} deriving (Eq, Show)

instance Ord ProtocolVersion where
    compare a b = compare (protocolMajor a, protocolMinor a, protocolPatch a)
                          (protocolMajor b, protocolMinor b, protocolPatch b)

-- | Current protocol version
currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = ProtocolVersion 1 0 0

-- | Request ID type for tracking requests
newtype RequestId = RequestId Int
    deriving (Eq, Ord, Show)

-- | Transaction modes
data TransactionMode
    = ReadOnly     -- ^ Read-only transaction
    | ReadWrite    -- ^ Read-write transaction
    | Exclusive    -- ^ Exclusive access
    deriving (Show, Eq)

-- | Database connection wrapper, parameterized by privilege tier
data Database (t :: PrivilegeTier) = Database {
    dbConn :: Connection,         -- ^ SQLite connection (Daemon) or Nothing (Builder)
    dbPath :: FilePath,           -- ^ Path to database file
    dbInitialized :: Bool,        -- ^ Whether schema is initialized
    dbBusyTimeout :: Int,         -- ^ Busy timeout in milliseconds
    dbMaxRetries :: Int,          -- ^ Maximum number of retries for busy operations
    dbPrivEvidence :: SPrivilegeTier t  -- ^ Runtime evidence of privilege tier
}

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


instance Aeson.FromJSON BuildError where
    parseJSON = Aeson.withObject "BuildError" $ \o -> do
        errType <- o Aeson..: "type"
        msg <- o Aeson..: "message"
        case errType of
            "eval" -> return $ EvalError msg
            "build" -> return $ BuildFailed msg
            "store" -> return $ StoreError msg
            "sandbox" -> return $ SandboxError msg
            "input" -> return $ InputNotFound (T.unpack msg)
            "hash" -> return $ HashError msg
            "graph" -> return $ GraphError msg
            "resource" -> return $ ResourceError msg
            "daemon" -> return $ DaemonError msg
            "auth" -> return $ AuthError msg
            "cycle" -> return $ CyclicDependency msg
            "serialization" -> return $ SerializationError msg
            "recursion" -> return $ RecursionLimit msg
            "network" -> return $ NetworkError msg
            "parse" -> return $ ParseError msg
            "db" -> return $ DBError msg
            "gc" -> return $ GCError msg
            "phase" -> return $ PhaseError msg
            "privilege" -> return $ PrivilegeError msg
            "protocol" -> return $ ProtocolError msg
            "internal" -> return $ InternalError msg
            "config" -> return $ ConfigError msg
            _ -> fail $ "Unknown error type: " <> T.unpack errType

instance Exception BuildError

-- | Convert BuildError to Text for easier handling across module boundaries
buildErrorToText :: BuildError -> Text
buildErrorToText = \case
    EvalError msg -> msg
    BuildFailed msg -> msg
    StoreError msg -> msg
    SandboxError msg -> msg
    InputNotFound path -> T.pack path
    HashError msg -> msg
    GraphError msg -> msg
    ResourceError msg -> msg
    DaemonError msg -> msg
    AuthError msg -> msg
    CyclicDependency msg -> msg
    SerializationError msg -> msg
    RecursionLimit msg -> msg
    NetworkError msg -> msg
    ParseError msg -> msg
    DBError msg -> msg
    GCError msg -> msg
    PhaseError msg -> msg
    PrivilegeError msg -> msg
    ProtocolError msg -> msg
    InternalError msg -> msg
    ConfigError msg -> msg

-- | Store path representing a content-addressed location
data StorePath = StorePath
    { storeHash :: !Text             -- Hash part of the path
    , storeName :: !Text             -- Human-readable name component
    } deriving (Show, Eq, Ord)

-- | Binary instance for StorePath
instance Binary.Binary StorePath where
    put (StorePath hash name) = do
        Binary.put hash
        Binary.put name

    get = do
        hash <- Binary.get
        name <- Binary.get
        let path = StorePath hash name
        if validateStorePath path
            then return path
            else fail $ "Invalid store path format during deserialization: " ++
                       T.unpack hash ++ "-" ++ T.unpack name

extractRequestIdFromBytes :: ByteString -> Maybe Int
extractRequestIdFromBytes headerBytes =
    case Aeson.eitherDecodeStrict headerBytes of
        Left _ -> Nothing
        Right val -> case Aeson.fromJSON val of
            Aeson.Error _ -> Nothing
            Aeson.Success obj -> do
                -- Extract the "id" field from the JSON object
                case AKeyMap.lookup "id" obj of
                    Just (Aeson.Number reqId) ->
                        -- Convert Scientific to Int
                        case toBoundedInteger reqId of
                            Just n -> Just n
                            Nothing -> Nothing
                    _ -> Nothing

splitResponseBytes :: ByteString -> (ByteString, Maybe ByteString)
splitResponseBytes rawBytes =
    -- Check if there's enough data for a header
    if BS.length rawBytes < 4
        then (rawBytes, Nothing)
        else do
            -- Extract header length from first 4 bytes
            let (lenBytes, rest) = BS.splitAt 4 rawBytes
                headerLen = fromIntegral $ -- Convert 4 bytes to header length
                    (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
                    (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
                    (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
                    (fromIntegral (BS.index lenBytes 3) :: Word32)

            -- Split header and check for payload
            if BS.length rest < headerLen
                then (rawBytes, Nothing)
                else do
                    let (header, payloadPart) = BS.splitAt headerLen rest

                    -- If there's more data, it's a payload with its own length prefix
                    if BS.length payloadPart >= 4
                        then do
                            -- Extract payload length
                            let (payloadLenBytes, payloadRest) = BS.splitAt 4 payloadPart
                                payloadLen = fromIntegral $ -- Parse payload length
                                    (fromIntegral (BS.index payloadLenBytes 0) :: Word32) `shiftL` 24 .|.
                                    (fromIntegral (BS.index payloadLenBytes 1) :: Word32) `shiftL` 16 .|.
                                    (fromIntegral (BS.index payloadLenBytes 2) :: Word32) `shiftL` 8 .|.
                                    (fromIntegral (BS.index payloadLenBytes 3) :: Word32)

                            -- Return payload if complete
                            if BS.length payloadRest >= payloadLen
                                then (header, Just $ BS.take payloadLen payloadRest)
                                else (header, Nothing)
                        else (header, Nothing)

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

-- | Validate a StorePath with context for logging
validateStorePathWithContext :: StorePath -> Text -> IO Bool
validateStorePathWithContext path context = do
    let isValid = validateStorePath path
    unless isValid $
        hPutStrLn stderr $ "Invalid StorePath detected: " ++ T.unpack (storePathToText path) ++
                          " [context: " ++ T.unpack context ++ "]"
    return isValid

-- | Message types for protocol communication
data MessageType
    = AuthType
    | BuildType
    | EvalType
    | BuildDerivationType
    | BuildStatusType
    | CancelBuildType
    | QueryBuildOutputType
    | ListBuildsType
    | StoreAddType
    | StoreVerifyType
    | StorePathType
    | StoreListType
    | StoreDerivationType
    | RetrieveDerivationType
    | QueryDerivationType
    | GetDerivationForOutputType
    | ListDerivationsType
    | GCType
    | GCStatusType
    | AddGCRootType
    | RemoveGCRootType
    | ListGCRootsType
    | PingType
    | ShutdownType
    | StatusType
    | ConfigType
    | ErrorType
    deriving (Show, Eq, Read)

-- | Message data for protocol communication
data Message
    = AuthMessage ByteString
    | RequestMessage Request
    | ResponseMessage Response
    | ErrorMessage BuildError
    deriving (Show, Eq)

-- | Daemon capabilities for permission checking
data Capability
    = StoreAccess             -- Ability to access the store (read/write)
    | SandboxCreation         -- Ability to create sandboxes
    | GarbageCollection       -- Ability to perform garbage collection
    | DerivationRegistration  -- Ability to register derivations
    | DerivationBuild         -- Ability to build derivations
    | StoreQuery              -- Ability to query the store
    | BuildQuery              -- Ability to query build status
    deriving (Show, Eq, Ord, Bounded, Enum)

-- | Authentication result
data AuthResult
    = AuthAccepted UserId AuthToken (Set Capability)  -- Successful auth with capabilities
    | AuthRejected Text                               -- Failed auth with reason
    | AuthSuccess UserId AuthToken                    -- Legacy success format
    deriving (Show, Eq)

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

-- | GC Statistics
data GCStats = GCStats {
    gcTotal :: Int,            -- Total paths in store
    gcLive :: Int,             -- Paths still reachable
    gcCollected :: Int,        -- Paths collected
    gcBytes :: Integer,        -- Bytes freed
    gcElapsedTime :: Double    -- Time taken for GC in seconds
} deriving (Show, Eq)

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
    , brMetadata :: Map Text Text           -- Metadata for the build result
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
    AcyclicProof   :: Proof 'Eval
    EvalCompleteProof :: Proof 'Eval

    -- Proofs for build phase
    InputProof     :: Proof 'Build
    BuildProof     :: Proof 'Build
    OutputProof    :: Proof 'Build

    -- Proofs for Return-Continuation
    ReturnProof    :: Proof 'Build
    RecursionProof :: Proof 'Build

    -- Proofs for garbage collection
    GCRootProof    :: Proof 'Build
    ReachableProof :: Proof 'Build

    -- Composite proofs
    ComposeProof   :: Proof p -> Proof p -> Proof p

-- Add standalone deriving instances for Proof
deriving instance Show (Proof p)
deriving instance Eq (Proof p)

-- | User identifier
newtype UserId = UserId Text
    deriving (Show, Eq, Ord)

-- | Authentication token
newtype AuthToken = AuthToken Text
    deriving (Show, Eq)

-- | Request type (for communication with daemon)
data Request = Request {
    reqId :: Int,                         -- ^ Unique request ID
    reqType :: Text,                      -- ^ Request type
    reqParams :: Map Text Text,           -- ^ Request parameters
    reqPayload :: Maybe ByteString        -- ^ Optional binary payload
} deriving (Show, Eq)

-- | Response type (for communication with daemon)
data Response = Response {
    respId :: Int,                        -- ^ Matching request ID
    respStatus :: Text,                   -- ^ Status (ok, error)
    respMessage :: Text,                  -- ^ Response message
    respData :: Map Text Text,            -- ^ Response data
    respPayload :: Maybe ByteString       -- ^ Optional binary payload
} deriving (Show, Eq)

-- | Build request information for protocol communication
data BuildRequestInfo = BuildRequestInfo {
    buildTimeout :: Maybe Int,      -- ^ Optional timeout in seconds
    buildEnv :: Map Text Text,      -- ^ Additional environment variables
    buildFlags :: [Text]            -- ^ Build flags
} deriving (Show, Eq)

-- | Full derivation information (Moved from Protocol.hs)
data DerivationInfo = DerivationInfo {
    derivationId :: Int,
    derivationHash :: Text,
    derivationStorePath :: StorePath,
    derivationTimestamp :: Int
} deriving (Show, Eq)

-- | Response with derivation information (Moved from Protocol.hs)
data DerivationInfoResponse = DerivationInfoResponse {
    derivationResponseDrv :: Derivation,
    derivationResponseOutputs :: Set StorePath,
    derivationResponseInputs :: Set StorePath,
    derivationResponseStorePath :: StorePath,
    derivationResponseMetadata :: Map Text Text
} deriving (Show, Eq)

-- | Response with derivation-output mappings (Moved from Protocol.hs)
data DerivationOutputMappingResponse = DerivationOutputMappingResponse {
    outputMappings :: [(StorePath, StorePath)],  -- [(derivation path, output path)]
    outputMappingCount :: Int,
    outputMappingComplete :: Bool   -- Whether all mappings were included
} deriving (Show, Eq)

-- | Build status update (Moved from Protocol.hs)
data BuildStatusUpdate = BuildStatusUpdate {
    buildId :: BuildId,
    buildStatus :: BuildStatus,
    buildTimeElapsed :: Double,   -- Time elapsed in seconds
    buildTimeRemaining :: Maybe Double,  -- Estimated time remaining (if available)
    buildLogUpdate :: Maybe Text,  -- New log messages since last update
    buildResourceUsage :: Map Text Double  -- Resource usage metrics
} deriving (Show, Eq)

-- | GC Request parameters (Moved from Protocol.hs)
data GCRequestParams = GCRequestParams {
    gcForce :: Bool  -- Force GC (run even if unsafe)
} deriving (Show, Eq)

-- | GC Status Request parameters (Moved from Protocol.hs)
data GCStatusRequestParams = GCStatusRequestParams {
    gcForceCheck :: Bool  -- Whether to force recheck the lock file
} deriving (Show, Eq)

-- | GC Status Response content (Moved from Protocol.hs)
data GCStatusInfo = GCStatusInfo {
    gcRunning :: Bool,           -- Whether GC is currently running
    gcOwner :: Maybe Text,       -- Process/username owning the GC lock (if running)
    gcLockTime :: Maybe UTCTime  -- When the GC lock was acquired (if running)
} deriving (Show, Eq)

-- | Daemon status information (Moved from Protocol.hs)
data DaemonStatus = DaemonStatus {
    daemonStatus :: Text,           -- "running", "starting", etc.
    daemonUptime :: Double,         -- Uptime in seconds
    daemonActiveBuilds :: Int,      -- Number of active builds
    daemonCompletedBuilds :: Int,   -- Number of completed builds since startup
    daemonFailedBuilds :: Int,      -- Number of failed builds since startup
    daemonGcRoots :: Int,           -- Number of GC roots
    daemonStoreSize :: Integer,     -- Store size in bytes
    daemonStorePaths :: Int         -- Number of paths in store
} deriving (Show, Eq)

-- | Daemon response types for protocol communication (Moved from Protocol.hs)
data DaemonResponse
    = AuthResponse AuthResult
    | BuildStartedResponse BuildId
    | BuildResultResponse BuildResult
    | BuildStatusResponse BuildStatusUpdate
    | BuildOutputResponse Text
    | BuildListResponse [(BuildId, BuildStatus, Float)]
    | CancelBuildResponse Bool
    | StoreAddResponse StorePath
    | StoreVerifyResponse Bool
    | StorePathResponse StorePath
    | StoreListResponse [StorePath]
    | StoreReadResponse ByteString
    | DerivationResponse Derivation
    | DerivationStoredResponse StorePath
    | DerivationRetrievedResponse (Maybe Derivation)
    | DerivationQueryResponse [Derivation]
    | DerivationOutputResponse (Set StorePath)
    | DerivationListResponse [StorePath]
    | GCResultResponse GCStats
    | GCStartedResponse
    | GCStatusResponse GCStatusInfo
    | GCRootAddedResponse Text
    | GCRootRemovedResponse Text
    | GCRootListResponse [GCRoot]
    | PongResponse
    | ShutdownResponse
    | StatusResponse DaemonStatus
    | ConfigResponse DaemonConfig
    | EvalResponse Derivation
    | ErrorResponse BuildError
    | SuccessResponse
    deriving (Show, Eq)

-- | Daemon connection type - core implementation for client-server communication
-- Parameterized by privilege tier for type-level enforcement
data DaemonConnection (t :: PrivilegeTier) = DaemonConnection {
    connHandle :: Handle,                     -- ^ Handle for all I/O operations
    connUserId :: UserId,                     -- ^ Authenticated user ID
    connAuthToken :: AuthToken,               -- ^ Authentication token
    connRequestMap :: TVar (Map Int (MVar BS.ByteString)), -- ^ Map of pending requests (raw bytes)
    connNextReqId :: TVar Int,                -- ^ Next request ID
    connReaderThread :: ThreadId,             -- ^ Thread ID of the response reader
    connShutdown :: TVar Bool,                -- ^ Flag to indicate connection shutdown
    connPrivEvidence :: SPrivilegeTier t      -- ^ Runtime evidence of privilege tier
}

-- | Running mode for Ten
data RunMode
    = StandaloneMode                     -- Direct execution without daemon
    | ClientMode (DaemonConnection 'Builder)  -- Connect to existing daemon
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
    , daemonLogFile :: Maybe FilePath    -- Optional log file path
    , daemonLogLevel :: Int              -- Log verbosity level
    , daemonGcInterval :: Maybe Int      -- Garbage collection interval in seconds
    , daemonUser :: Maybe Text           -- User to run as
    , daemonGroup :: Maybe Text          -- Group to run as
    , daemonAllowedUsers :: Set Text     -- Users allowed to connect
    , daemonMaxJobs :: Int               -- Maximum concurrent jobs
    , daemonForeground :: Bool           -- Run in foreground instead of daemonizing
    , daemonTmpDir :: FilePath           -- Directory for temporary files
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

-- | State carried through build operations - parameterized by phase
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
-- Parameterized by phase and privilege tier with singleton evidence passing
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

-- | Create a new daemon connection with privilege tier singleton evidence
createDaemonConnection :: Handle -> UserId -> AuthToken -> SPrivilegeTier t -> IO (DaemonConnection t)
createDaemonConnection handle userId authToken priEvidence = do
    requestMap <- newTVarIO Map.empty
    nextReqId <- newTVarIO 1
    shutdownFlag <- newTVarIO False

    -- Start background thread to read responses using handle
    readerThread <- forkIO $ responseReaderThread handle requestMap shutdownFlag

    return DaemonConnection {
        connHandle = handle,
        connUserId = userId,
        connAuthToken = authToken,
        connRequestMap = requestMap,
        connNextReqId = nextReqId,
        connReaderThread = readerThread,
        connShutdown = shutdownFlag,
        connPrivEvidence = priEvidence
    }

-- | Helper function to work with singletons
withSPhase :: SPhase p -> (forall q. SPhase q -> TenM q t a) -> TenM p t a
withSPhase sp f = TenM $ \_ st -> do
    let (TenM g) = f sp
    g sp st

-- | Close a daemon connection, cleaning up resources
closeDaemonConnection :: DaemonConnection t -> IO ()
closeDaemonConnection conn = do
    -- Signal reader thread to shut down
    atomically $ writeTVar (connShutdown conn) True

    -- Close handle
    hClose (connHandle conn)

    -- Kill reader thread if it doesn't exit on its own
    killThread (connReaderThread conn)

-- | Execute an action with a daemon connection
withDaemonConnection :: Handle -> UserId -> AuthToken -> SPrivilegeTier t
                     -> (DaemonConnection t -> IO a) -> IO a
withDaemonConnection handle userId authToken priEvidence =
    bracket
        (createDaemonConnection handle userId authToken priEvidence)
        closeDaemonConnection

-- | Send a request to the daemon
sendRequest :: DaemonConnection t -> Request -> IO Int
sendRequest conn request = do
    -- Generate request ID
    reqId <- atomically $ do
        rid <- readTVar (connNextReqId conn)
        writeTVar (connNextReqId conn) (rid + 1)
        return rid

    -- Create response variable
    respVar <- newEmptyMVar

    -- Register the pending request
    atomically $ modifyTVar' (connRequestMap conn) $ Map.insert reqId respVar

    -- Update the request with the ID
    let request' = request { reqId = reqId }

    -- Serialize and send the request
    let encoded = encodeRequest request'
    BS.hPut (connHandle conn) encoded
    hFlush (connHandle conn)

    -- Return the request ID for tracking
    return reqId

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection 'Builder -> Int -> Int -> IO (Either BuildError DaemonResponse)
receiveResponse conn reqId timeoutMicros = do
    reqMap <- readTVarIO (connRequestMap conn)
    case Map.lookup reqId reqMap of
        Nothing ->
            return $ Left $ DaemonError $ "Unknown request ID: " <> T.pack (show reqId)
        Just respVar -> do
            -- Get raw bytes from MVar with timeout
            mResult <- SystemTimeout.timeout timeoutMicros $ takeMVar respVar

            case mResult of
                Nothing -> return $ Left $ DaemonError "Timeout waiting for daemon response"
                Just rawBytes -> do
                    -- Split the raw bytes into header and payload
                    case splitResponseBytes rawBytes of
                        (header, mPayload) ->
                            -- Now deserialize
                            case deserializeDaemonResponse header mPayload of
                                Left err -> return $ Left $ ProtocolError err
                                Right resp -> return $ Right resp

-- | Send a request and wait for response (synchronous)
-- Always uses Builder privilege tier to enforce Nix-like privilege model
sendRequestSync :: DaemonConnection 'Builder -> Request -> Int -> IO (Either BuildError DaemonResponse)
sendRequestSync conn request timeoutMicros = do
    -- Send the request
    reqId <- sendRequest conn request

    -- Wait for response
    receiveResponse conn reqId timeoutMicros

-- | Background thread to read and dispatch responses
responseReaderThread :: Handle -> TVar (Map Int (MVar BS.ByteString)) -> TVar Bool -> IO ()
responseReaderThread handle requestMap shutdownFlag = forever $ do
    -- Check if we should shut down
    shutdown <- readTVarIO shutdownFlag
    when shutdown $ return ()

    -- Try to read a response as raw bytes
    eResp <- try $ receiveFramedResponse handle
    case eResp of
        Left (e :: SomeException) ->
            -- Connection error, exit thread
            return ()
        Right (respData, mRespPayload) -> do
            -- Extract request ID from respData without fully deserializing
            let mReqId = extractRequestIdFromBytes respData

            -- Store the raw response bytes in the MVar
            case mReqId of
                Just reqId -> do
                    reqMap <- readTVarIO requestMap
                    case Map.lookup reqId reqMap of
                        Just respVar -> do
                            -- Concatenate header and payload into a single ByteString for storage
                            let fullResponse = case mRespPayload of
                                    Just payload -> BS.concat [respData, payload]
                                    Nothing -> respData
                            putMVar respVar fullResponse
                            atomically $ modifyTVar' requestMap $ Map.delete reqId
                        Nothing -> return ()
                Nothing -> return ()

-- | Helper function to work with singletons
-- | Extract the privilege tier from a singleton
fromSingTier :: SPrivilegeTier t -> PrivilegeTier
fromSingTier SDaemon = Daemon
fromSingTier SBuilder = Builder

class CanBuildDerivation (t :: PrivilegeTier) where
  buildDerivation :: Derivation -> TenM 'Build t BuildResult

class CanQueryBuildStatus (t :: PrivilegeTier) where
  getBuildStatus :: BuildId -> TenM 'Build t BuildStatus

-- | Get the default path for the Ten database
-- This function computes a path but doesn't interact with the filesystem
defaultDBPath :: FilePath -> FilePath
defaultDBPath storeDir = storeDir </> "var/ten/db/ten.db"

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
initClientEnv :: FilePath -> FilePath -> DaemonConnection 'Builder -> BuildEnv
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

-- | Initialize build state for a specific phase with a BuildId
initBuildState :: Phase -> BuildId -> BuildState p
initBuildState phase bid = BuildState
    { buildProofs = []
    , buildInputs = Set.empty
    , buildOutputs = Set.empty
    , currentBuildId = bid
    , buildChain = []
    , recursionDepth = 0
    , currentPhase = phase
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
    runTen sEval sDaemon m env' (initBuildState Eval bid)

-- | Execute a build-phase computation in builder mode
buildTen :: TenM 'Build 'Builder a -> BuildEnv -> IO (Either BuildError (a, BuildState 'Build))
buildTen m env = do
    -- Ensure environment is builder tier
    let env' = env { currentPrivilegeTier = Builder }
    bid <- BuildId <$> newUnique
    runTen sBuild sBuilder m env' (initBuildState Build bid)

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
runTenDaemonEval :: TenM 'Eval 'Daemon a -> BuildEnv -> BuildState 'Eval -> IO (Either BuildError (a, BuildState 'Eval))
runTenDaemonEval = runTenDaemon

-- | Execute a daemon operation in Build phase (privileged)
runTenDaemonBuild :: TenM 'Build 'Daemon a -> BuildEnv -> BuildState 'Build -> IO (Either BuildError (a, BuildState 'Build))
runTenDaemonBuild = runTenDaemon

-- | Execute a builder operation in Eval phase (unprivileged)
runTenBuilderEval :: TenM 'Eval 'Builder a -> BuildEnv -> BuildState 'Eval -> IO (Either BuildError (a, BuildState 'Eval))
runTenBuilderEval m env state = do
    -- Set builder privilege tier
    let env' = env { currentPrivilegeTier = Builder }
    runTen sEval sBuilder m env' state

-- | Execute a builder operation in Build phase (unprivileged)
runTenBuilderBuild :: TenM 'Build 'Builder a -> BuildEnv -> BuildState 'Build -> IO (Either BuildError (a, BuildState 'Build))
runTenBuilderBuild m env state = do
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

-- | Store a derivation in the store (Daemon privilege)
storeDerivationDaemon :: Derivation -> TenM 'Eval 'Daemon StorePath
storeDerivationDaemon deriv = do
    env <- ask

    -- Serialize the derivation
    let serialized = serializeDerivation deriv

    -- Calculate a hash for the content
    let contentHash = hashByteString serialized
    let hashHex = T.pack $ show contentHash

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

-- | Store a derivation in builder context
storeDerivationBuilder :: Derivation -> TenM 'Eval 'Builder StorePath
storeDerivationBuilder deriv = do
    -- Get daemon connection
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create store request with derivation payload
            let serialized = serializeDerivation deriv
            let request = Request {
                reqId = 0,  -- Will be set by sendRequest
                reqType = "store-derivation",
                reqParams = Map.singleton "name" (derivName deriv <> ".drv"),
                reqPayload = Just serialized
            }

            -- Send request and wait for response
            response <- liftIO $ sendRequestSync conn request 30000000  -- 30 second timeout

            case response of
                Left err -> throwError err
                Right (DerivationStoredResponse path) -> return path
                Right resp -> throwError $ StoreError $ "Unexpected response: " <> T.pack (show resp)
        _ -> throwError $ PrivilegeError "Cannot store derivation in builder context without daemon connection"

-- | Read a derivation from the store (Daemon context with direct store access)
readDerivationDaemon :: StorePath -> TenM p 'Daemon (Either BuildError Derivation)
readDerivationDaemon path = do
    env <- ask
    let fullPath = storePathToFilePath path env

    -- Check if file exists
    exists <- liftIO $ doesFileExist fullPath
    if not exists
        then return $ Left $ StoreError $ "Derivation not found: " <> storePathToText path
        else do
            -- Read and deserialize
            content <- liftIO $ BS.readFile fullPath
            return $ deserializeDerivation content

-- | Request a derivation via the daemon protocol (Builder context)
readDerivationBuilder :: StorePath -> TenM p 'Builder (Either BuildError Derivation)
readDerivationBuilder path = do
    -- Get daemon connection
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create request for derivation content
            let request = Request {
                reqId = 0,  -- Will be set by sendRequest
                reqType = "retrieve-derivation",
                reqParams = Map.singleton "path" (storePathToText path),
                reqPayload = Nothing
            }

            -- Send request and wait for response
            response <- liftIO $ sendRequestSync conn request 30000000  -- 30 second timeout

            case response of
                Left err -> return $ Left err
                Right (DerivationRetrievedResponse mDeriv) ->
                    case mDeriv of
                        Just drv -> return $ Right drv
                        Nothing -> return $ Left $ StoreError $ "Derivation not found: " <> storePathToText path
                Right resp -> return $ Left $ StoreError $ "Unexpected response: " <> T.pack (show resp)
        _ -> return $ Left $ PrivilegeError "Cannot read derivation in builder context without daemon connection"

-- | Store a build result (Daemon context with direct store access)
storeBuildResultDaemon :: BuildId -> BuildResult -> TenM 'Build 'Daemon StorePath
storeBuildResultDaemon buildId result = do
    env <- ask

    -- Serialize the build result
    let serialized = serializeBuildResult result

    -- Calculate a hash for the content
    let contentHash = hashByteString serialized
    let hashHex = T.pack $ show contentHash

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

-- | Read a build result from the store (Daemon context with direct store access)
readBuildResultDaemon :: StorePath -> TenM p 'Daemon (Either BuildError BuildResult)
readBuildResultDaemon path = do
    env <- ask
    let fullPath = storePathToFilePath path env

    -- Check if file exists
    exists <- liftIO $ doesFileExist fullPath
    if not exists
        then return $ Left $ StoreError $ "Build result not found: " <> storePathToText path
        else do
            -- Read and deserialize
            content <- liftIO $ BS.readFile fullPath
            return $ deserializeBuildResult content

-- | Read build result via daemon protocol (Builder context)
readBuildResultBuilder :: StorePath -> TenM p 'Builder (Either BuildError BuildResult)
readBuildResultBuilder path = do
    -- Get daemon connection
    env <- ask
    case runMode env of
        ClientMode conn -> do
            -- Create request for build result content
            let request = Request {
                reqId = 0,  -- Will be set by sendRequest
                reqType = "store-read",
                reqParams = Map.singleton "path" (storePathToText path),
                reqPayload = Nothing
            }

            -- Send request and wait for response
            response <- liftIO $ sendRequestSync conn request 30000000  -- 30 second timeout

            case response of
                Left err -> return $ Left err
                Right (StoreReadResponse content) ->
                    return $ deserializeBuildResult content
                Right resp -> return $ Left $ StoreError $ "Unexpected response: " <> T.pack (show resp)
        _ -> return $ Left $ PrivilegeError "Cannot read build result in builder context without daemon connection"

-- | Encode a request for transmission
encodeRequest :: Request -> BS.ByteString
encodeRequest req =
    let reqJson = Aeson.encode $ Aeson.object [
            "id" Aeson..= reqId req,
            "type" Aeson..= reqType req,
            "params" Aeson..= reqParams req,
            "hasPayload" Aeson..= isJust (reqPayload req)
            ]
        headerLen = BS.length (LBS.toStrict reqJson)
        lenBytes = BS.pack [
            fromIntegral (headerLen `shiftR` 24) .&. 0xFF,
            fromIntegral (headerLen `shiftR` 16) .&. 0xFF,
            fromIntegral (headerLen `shiftR` 8) .&. 0xFF,
            fromIntegral headerLen .&. 0xFF
            ]
    in
        case reqPayload req of
            Nothing ->
                BS.concat [lenBytes, LBS.toStrict reqJson]
            Just payload ->
                let payloadLen = BS.length payload
                    payloadLenBytes = BS.pack [
                        fromIntegral (payloadLen `shiftR` 24) .&. 0xFF,
                        fromIntegral (payloadLen `shiftR` 16) .&. 0xFF,
                        fromIntegral (payloadLen `shiftR` 8) .&. 0xFF,
                        fromIntegral payloadLen .&. 0xFF
                        ]
                in BS.concat [lenBytes, LBS.toStrict reqJson, payloadLenBytes, payload]

-- | Decode a response from bytes
decodeResponse :: BS.ByteString -> Either Text Response
decodeResponse bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "JSON decode error: " <> T.pack err
        Right val -> case Aeson.fromJSON val of
            Aeson.Error err -> Left $ "JSON conversion error: " <> T.pack err
            Aeson.Success resp -> Right resp

-- | Parse a framed request from the wire
parseRequestFrame :: BS.ByteString -> Either Text (BS.ByteString, BS.ByteString)
parseRequestFrame bs
    | BS.length bs < 4 = Left "Message too short to contain length"
    | otherwise = do
        let (lenBytes, rest) = BS.splitAt 4 bs
        let len = (fromIntegral :: Word32 -> Int) $
              (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
              (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
              (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
              (fromIntegral (BS.index lenBytes 3) :: Word32)

        if len > 100 * 1024 * 1024  -- 100 MB limit
            then Left $ T.pack $ "Message too large (" ++ show len ++ " bytes)"
            else if BS.length rest < len
                then Left "Incomplete message"
                else do
                    let (content, remaining) = BS.splitAt len rest
                    Right (content, remaining)

-- | Receive a framed response from a handle
receiveFramedResponse :: Handle -> IO (BS.ByteString, Maybe BS.ByteString)
receiveFramedResponse handle = do
    -- Read the length prefix (4 bytes)
    lenBytes <- BS.hGet handle 4
    when (BS.length lenBytes /= 4) $
        throwIO $ DaemonError "Incomplete message length"

    -- Parse message length
    let len = fromIntegral $
             (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
             (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
             (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
             (fromIntegral (BS.index lenBytes 3) :: Word32)

    -- Check message size
    when (len > 100 * 1024 * 1024) $ -- 100 MB limit
        throwIO $ DaemonError $ "Message too large: " <> T.pack (show len)

    -- Read message body
    respJson <- BS.hGet handle len
    when (BS.length respJson /= len) $
        throwIO $ DaemonError "Incomplete message body"

    -- Check if there's a payload by parsing the JSON and looking for hasContent flag
    let hasPayload = case Aeson.decodeStrict respJson of
            Just obj -> case Aeson.fromJSON obj of
                Aeson.Success (Aeson.Object o) ->
                    case AKeyMap.lookup "hasContent" o of
                        Just (Aeson.Bool True) -> True
                        _ -> False
                _ -> False
            _ -> False

    if hasPayload
        then do
            -- Read payload frame (same format - 4 byte length followed by content)
            payloadLenBytes <- BS.hGet handle 4
            when (BS.length payloadLenBytes /= 4) $
                throwIO $ DaemonError "Incomplete payload length"

            let payloadLen = fromIntegral $
                          (fromIntegral (BS.index payloadLenBytes 0) :: Word32) `shiftL` 24 .|.
                          (fromIntegral (BS.index payloadLenBytes 1) :: Word32) `shiftL` 16 .|.
                          (fromIntegral (BS.index payloadLenBytes 2) :: Word32) `shiftL` 8 .|.
                          (fromIntegral (BS.index payloadLenBytes 3) :: Word32)

            -- Check payload size
            when (payloadLen > 100 * 1024 * 1024) $ -- 100 MB limit
                throwIO $ DaemonError $ "Payload too large: " <> T.pack (show payloadLen)

            -- Read payload
            payload <- BS.hGet handle payloadLen
            when (BS.length payload /= payloadLen) $
                throwIO $ DaemonError "Incomplete payload"

            return (respJson, Just payload)
        else
            return (respJson, Nothing)

-- | Deserialize a daemon response
deserializeDaemonResponse :: BS.ByteString -> Maybe BS.ByteString -> Either Text DaemonResponse
deserializeDaemonResponse bs mPayload =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "JSON parse error: " <> T.pack err
        Right val -> case Aeson.fromJSON val of
            Aeson.Error err -> Left $ "JSON conversion error: " <> T.pack err
            Aeson.Success obj -> do
                -- Get the message type
                case AKeyMap.lookup "type" obj of
                    Nothing -> Left "Missing message type"
                    Just (Aeson.String "auth") -> do
                        case AKeyMap.lookup "result" obj of
                            Just result -> case Aeson.fromJSON result of
                                Aeson.Success authResult -> Right $ AuthResponse authResult
                                Aeson.Error err -> Left $ "Invalid auth result: " <> T.pack err
                            Nothing -> Left "Missing auth result"

                    Just (Aeson.String "build-started") -> do
                        buildIdText <- maybe (Left "Missing buildId") Right $
                            AKeyMap.lookup "buildId" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        case parseBuildId buildIdText of
                            Left err -> Left err
                            Right bid -> Right $ BuildStartedResponse bid

                    Just (Aeson.String "build-result") -> do
                        result <- maybe (Left "Missing build result") Right $
                            AKeyMap.lookup "result" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ BuildResultResponse result

                    Just (Aeson.String "build-status") -> do
                        update <- maybe (Left "Missing build status update") Right $
                            AKeyMap.lookup "update" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ BuildStatusResponse update

                    Just (Aeson.String "build-output") -> do
                        output <- maybe (Left "Missing output") Right $
                            AKeyMap.lookup "output" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ BuildOutputResponse output

                    Just (Aeson.String "build-list") -> do
                        builds <- maybe (Left "Missing builds") Right $
                            AKeyMap.lookup "builds" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        buildsList <- parseBuilds builds
                        Right $ BuildListResponse buildsList

                    Just (Aeson.String "cancel-build") -> do
                        success <- maybe (Left "Missing success") Right $
                            AKeyMap.lookup "success" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ CancelBuildResponse success

                    Just (Aeson.String "store-add") -> do
                        pathText <- maybe (Left "Missing path") Right $
                            AKeyMap.lookup "path" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        case parseStorePath pathText of
                            Just path -> Right $ StoreAddResponse path
                            Nothing -> Left $ "Invalid store path: " <> pathText

                    Just (Aeson.String "store-verify") -> do
                        valid <- maybe (Left "Missing valid") Right $
                            AKeyMap.lookup "valid" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ StoreVerifyResponse valid

                    Just (Aeson.String "store-path") -> do
                        pathText <- maybe (Left "Missing path") Right $
                            AKeyMap.lookup "path" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        case parseStorePath pathText of
                            Just path -> Right $ StorePathResponse path
                            Nothing -> Left $ "Invalid store path: " <> pathText

                    Just (Aeson.String "store-list") -> do
                        pathTexts <- maybe (Left "Missing paths") Right $
                            AKeyMap.lookup "paths" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        let paths = catMaybes $ map parseStorePath pathTexts
                        Right $ StoreListResponse paths

                    Just (Aeson.String "store-read") -> do
                        case mPayload of
                            Nothing -> Left "Missing store content payload"
                            Just content -> Right $ StoreReadResponse content

                    Just (Aeson.String "derivation") -> do
                        case mPayload of
                            Nothing -> Left "Missing derivation content"
                            Just content ->
                                case deserializeDerivation content of
                                    Left err -> Left $ "Invalid derivation: " <> buildErrorToText err
                                    Right drv -> Right $ DerivationResponse drv

                    Just (Aeson.String "derivation-stored") -> do
                        pathText <- maybe (Left "Missing path") Right $
                            AKeyMap.lookup "path" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        case parseStorePath pathText of
                            Just path -> Right $ DerivationStoredResponse path
                            Nothing -> Left $ "Invalid store path: " <> pathText

                    Just (Aeson.String "derivation-retrieved") -> do
                        found <- maybe (Left "Missing found flag") Right $
                            AKeyMap.lookup "found" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        if found
                            then case mPayload of
                                Nothing -> Left "Missing derivation content"
                                Just content ->
                                    case deserializeDerivation content of
                                        Left err -> Left $ "Invalid derivation: " <> buildErrorToText err
                                        Right drv -> Right $ DerivationRetrievedResponse (Just drv)
                            else Right $ DerivationRetrievedResponse Nothing

                    Just (Aeson.String "derivation-query") -> do
                        countVal <- maybe (Left "Missing count") Right $
                            AKeyMap.lookup "count" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        let count = countVal :: Int  -- Explicit type annotation
                        case mPayload of
                            Nothing ->
                                if count == 0
                                    then Right $ DerivationQueryResponse []
                                    else Left "Missing derivation content"
                            Just content ->
                                -- Parse multiple derivations from the content
                                -- In a real implementation, this would properly deserialize
                                -- a list of derivations from the binary content
                                parseDerivationsFromContent content

                    Just (Aeson.String "derivation-outputs") -> do
                        outputTexts <- maybe (Left "Missing outputs") Right $
                            AKeyMap.lookup "outputs" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        let outputs = Set.fromList $ catMaybes $ map parseStorePath outputTexts
                        Right $ DerivationOutputResponse outputs

                    Just (Aeson.String "derivation-list") -> do
                        pathTexts <- maybe (Left "Missing paths") Right $
                            AKeyMap.lookup "paths" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        let paths = catMaybes $ map parseStorePath pathTexts
                        Right $ DerivationListResponse paths

                    Just (Aeson.String "gc-result") -> do
                        stats <- maybe (Left "Missing stats") Right $
                            AKeyMap.lookup "stats" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ GCResultResponse stats

                    Just (Aeson.String "gc-started") -> Right GCStartedResponse

                    Just (Aeson.String "gc-status") -> do
                        status <- maybe (Left "Missing status") Right $
                            AKeyMap.lookup "status" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ GCStatusResponse status

                    Just (Aeson.String "gc-root-added") -> do
                        name <- maybe (Left "Missing name") Right $
                            AKeyMap.lookup "name" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ GCRootAddedResponse name

                    Just (Aeson.String "gc-root-removed") -> do
                        name <- maybe (Left "Missing name") Right $
                            AKeyMap.lookup "name" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ GCRootRemovedResponse name

                    Just (Aeson.String "gc-roots-list") -> do
                        -- Simple implementation returning empty list
                        Right $ GCRootListResponse []

                    Just (Aeson.String "pong") -> Right PongResponse

                    Just (Aeson.String "shutdown") -> Right ShutdownResponse

                    Just (Aeson.String "status") -> do
                        status <- maybe (Left "Missing status") Right $
                            AKeyMap.lookup "status" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ StatusResponse status

                    Just (Aeson.String "config") -> do
                        config <- maybe (Left "Missing config") Right $
                            AKeyMap.lookup "config" obj >>= Aeson.parseMaybe Aeson.parseJSON
                        Right $ ConfigResponse config

                    Just (Aeson.String "eval") -> do
                        case mPayload of
                            Nothing -> Left "Missing derivation content"
                            Just content ->
                                case deserializeDerivation content of
                                    Left err -> Left $ "Invalid derivation: " <> buildErrorToText err
                                    Right drv -> Right $ EvalResponse drv

                    Just (Aeson.String "error") -> do
                        errObj <- maybe (Left "Missing error") Right $
                            AKeyMap.lookup "error" obj
                        case parseError errObj of
                            Left err -> Left $ "Invalid error: " <> err
                            Right err -> Right $ ErrorResponse err

                    Just (Aeson.String "success") -> Right SuccessResponse

                    Just (Aeson.String typ) -> Left $ "Unknown response type: " <> typ
                    _ -> Left "Invalid response type"
  where
    parseBuilds :: Aeson.Value -> Either Text [(BuildId, BuildStatus, Float)]
    parseBuilds builds = do
        case Aeson.parseEither parseBuildsArray builds of
            Left err -> Left $ "Invalid builds format: " <> T.pack err
            Right builds' -> Right builds'

    parseBuildsArray :: Aeson.Value -> Parser [(BuildId, BuildStatus, Float)]
    parseBuildsArray = Aeson.withArray "Builds" $ \arr ->
        mapM parseBuildEntry (Vector.toList arr)

    parseBuildEntry :: Aeson.Value -> Parser (BuildId, BuildStatus, Float)
    parseBuildEntry = Aeson.withObject "BuildEntry" $ \o -> do
        idText <- o Aeson..: "id"
        statusText <- o Aeson..: "status" :: Parser Text
        progress <- o Aeson..: "progress"

        buildId <- case parseBuildId idText of
            Left err -> fail $ T.unpack err
            Right bid -> return bid

        status <- case statusText of
            "pending" -> return BuildPending
            "running" -> return $ BuildRunning progress
            "completed" -> return BuildCompleted
            "failed" -> return BuildFailed'
            "recursing" -> do
                childId <- o Aeson..: "childId"
                case parseBuildId childId of
                    Left err -> fail $ T.unpack err
                    Right cid -> return $ BuildRecursing cid
            _ -> fail $ "Unknown build status: " ++ T.unpack statusText

        return (buildId, status, progress)

    parseDerivationsFromContent :: BS.ByteString -> Either Text DaemonResponse
    parseDerivationsFromContent content = do
        -- In a real implementation, this would deserialize a list of derivations
        -- For now, we'll just return an empty list
        Right $ DerivationQueryResponse []

    parseError errObj = do
        case Aeson.fromJSON errObj of
            Aeson.Success err -> Right err
            Aeson.Error err -> Left $ "Invalid error format: " <> T.pack err

-- | Convert a BuildId to Text representation for serialization
renderBuildId :: BuildId -> Text
renderBuildId (BuildId u) = "build-" <> T.pack (show (hashUnique u))
renderBuildId (BuildIdFromInt n) = "build-" <> T.pack (show n)

-- | Parse a BuildId from Text
parseBuildId :: Text -> Either Text BuildId
parseBuildId txt =
    case T.stripPrefix "build-" txt of
        Just numStr ->
            case readMaybe (T.unpack numStr) of
                Just n -> Right $ BuildIdFromInt n
                Nothing -> Left $ "Invalid BuildId number format: " <> numStr
        Nothing ->
            case readMaybe (T.unpack txt) of
                Just n -> Right $ BuildIdFromInt n
                Nothing -> Left $ "Invalid BuildId format: " <> txt

-- | Read a message with timeout
readResponseWithTimeout :: Handle -> Int -> IO Response
readResponseWithTimeout handle timeoutMicros = do
    -- Read the length prefix (4 bytes)
    lenBytes <- BS.hGet handle 4
    when (BS.length lenBytes /= 4) $
        throwIO $ DaemonError "Incomplete response length"

    -- Decode message length
    let len :: Int
        len = fromIntegral (
            (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
            (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
            (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
            (fromIntegral (BS.index lenBytes 3) :: Word32))

    -- Check if message is too large
    when (len > 100 * 1024 * 1024) $ -- 100 MB limit
        throwIO $ DaemonError $ "Response too large: " <> T.pack (show len) <> " bytes"

    -- Read the message body
    respJson <- BS.hGet handle len
    when (BS.length respJson /= len) $
        throwIO $ DaemonError "Incomplete response body"

    -- Check if there's a payload
    case Aeson.eitherDecodeStrict respJson of
        Left err -> throwIO $ DaemonError $ "Invalid JSON: " <> T.pack err
        Right val -> case Aeson.fromJSON val of
            Aeson.Error err -> throwIO $ DaemonError $ "JSON conversion error: " <> T.pack err
            Aeson.Success (resp :: Response) -> do
                -- Check if there's a payload
                if Map.member "hasPayload" (respData resp) &&
                   Map.lookup "hasPayload" (respData resp) == Just "true"
                    then do
                        -- Read payload length
                        payloadLenBytes <- BS.hGet handle 4
                        when (BS.length payloadLenBytes /= 4) $
                            throwIO $ DaemonError "Incomplete payload length"

                        let payloadLen :: Int
                            payloadLen = fromIntegral (
                                (fromIntegral (BS.index payloadLenBytes 0) :: Word32) `shiftL` 24 .|.
                                (fromIntegral (BS.index payloadLenBytes 1) :: Word32) `shiftL` 16 .|.
                                (fromIntegral (BS.index payloadLenBytes 2) :: Word32) `shiftL` 8 .|.
                                (fromIntegral (BS.index payloadLenBytes 3) :: Word32))

                        -- Read payload
                        payload <- BS.hGet handle payloadLen
                        when (BS.length payload /= payloadLen) $
                            throwIO $ DaemonError "Incomplete payload"

                        return resp { respPayload = Just payload }
                    else return resp

-- | Send a message to a handle
sendMessage :: Handle -> Message -> IO ()
sendMessage handle msg = do
    -- Encode the message
    let encoded = encodeMessage msg
    -- Send the message
    BS.hPut handle encoded
    hFlush handle

-- | Receive a message from a handle
receiveMessage :: Handle -> IO (Either BuildError Message)
receiveMessage handle = do
    -- Try to read the message
    result <- try $ do
        -- Read the length prefix (4 bytes)
        lenBytes <- BS.hGet handle 4
        when (BS.length lenBytes /= 4) $
            throwIO $ DaemonError "Incomplete message length"

        -- Decode message length
        let len :: Int
            len = fromIntegral (
                (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
                (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
                (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
                (fromIntegral (BS.index lenBytes 3) :: Word32))

        -- Check if message is too large
        when (len > 100 * 1024 * 1024) $ -- 100 MB limit
            throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

        -- Read the message body
        msgBytes <- BS.hGet handle len
        when (BS.length msgBytes /= len) $
            throwIO $ DaemonError "Incomplete message body"

        -- Decode the message
        case decodeMessage msgBytes of
            Left err -> throwIO $ DaemonError $ "Failed to decode message: " <> err
            Right msg -> return msg

    case result of
        Left (e :: SomeException) -> return $ Left $ DaemonError $ T.pack $ show e
        Right msg -> return $ Right msg

-- | Encode a message
encodeMessage :: Message -> BS.ByteString
encodeMessage msg =
    let encoded = case msg of
            AuthMessage payload ->
                let header = Aeson.encode $ Aeson.object [
                        "type" Aeson..= ("auth" :: Text),
                        "hasPayload" Aeson..= True
                        ]
                    headerLen = BS.length (LBS.toStrict header)
                    payloadLen = BS.length payload
                    headerLenBytes = BS.pack [
                        fromIntegral (headerLen `shiftR` 24) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 16) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 8) .&. 0xFF,
                        fromIntegral headerLen .&. 0xFF
                        ]
                    payloadLenBytes = BS.pack [
                        fromIntegral (payloadLen `shiftR` 24) .&. 0xFF,
                        fromIntegral (payloadLen `shiftR` 16) .&. 0xFF,
                        fromIntegral (payloadLen `shiftR` 8) .&. 0xFF,
                        fromIntegral payloadLen .&. 0xFF
                        ]
                in BS.concat [headerLenBytes, LBS.toStrict header, payloadLenBytes, payload]

            RequestMessage req ->
                encodeRequest req

            ResponseMessage resp ->
                let header = Aeson.encode $ Aeson.object [
                        "type" Aeson..= ("response" :: Text),
                        "id" Aeson..= respId resp,
                        "status" Aeson..= respStatus resp,
                        "message" Aeson..= respMessage resp,
                        "data" Aeson..= respData resp,
                        "hasPayload" Aeson..= isJust (respPayload resp)
                        ]
                    headerLen = BS.length (LBS.toStrict header)
                    headerLenBytes = BS.pack [
                        fromIntegral (headerLen `shiftR` 24) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 16) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 8) .&. 0xFF,
                        fromIntegral headerLen .&. 0xFF
                        ]
                in case respPayload resp of
                    Nothing -> BS.concat [headerLenBytes, LBS.toStrict header]
                    Just payload ->
                        let payloadLen = BS.length payload
                            payloadLenBytes = BS.pack [
                                fromIntegral (payloadLen `shiftR` 24) .&. 0xFF,
                                fromIntegral (payloadLen `shiftR` 16) .&. 0xFF,
                                fromIntegral (payloadLen `shiftR` 8) .&. 0xFF,
                                fromIntegral payloadLen .&. 0xFF
                                ]
                        in BS.concat [headerLenBytes, LBS.toStrict header, payloadLenBytes, payload]

            ErrorMessage err ->
                let header = Aeson.encode $ Aeson.object [
                        "type" Aeson..= ("error" :: Text),
                        "errorType" Aeson..= errorTypeString err,
                        "message" Aeson..= buildErrorToText err
                        ]
                    headerLen = BS.length (LBS.toStrict header)
                    headerLenBytes = BS.pack [
                        fromIntegral (headerLen `shiftR` 24) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 16) .&. 0xFF,
                        fromIntegral (headerLen `shiftR` 8) .&. 0xFF,
                        fromIntegral headerLen .&. 0xFF
                        ]
                in BS.concat [headerLenBytes, LBS.toStrict header]
    in encoded
  where
    errorTypeString :: BuildError -> Text
    errorTypeString (EvalError _) = "eval"
    errorTypeString (BuildFailed _) = "build"
    errorTypeString (StoreError _) = "store"
    errorTypeString (SandboxError _) = "sandbox"
    errorTypeString (InputNotFound _) = "input"
    errorTypeString (HashError _) = "hash"
    errorTypeString (GraphError _) = "graph"
    errorTypeString (ResourceError _) = "resource"
    errorTypeString (DaemonError _) = "daemon"
    errorTypeString (AuthError _) = "auth"
    errorTypeString (CyclicDependency _) = "cycle"
    errorTypeString (SerializationError _) = "serialization"
    errorTypeString (RecursionLimit _) = "recursion"
    errorTypeString (NetworkError _) = "network"
    errorTypeString (ParseError _) = "parse"
    errorTypeString (DBError _) = "db"
    errorTypeString (GCError _) = "gc"
    errorTypeString (PhaseError _) = "phase"
    errorTypeString (PrivilegeError _) = "privilege"
    errorTypeString (ProtocolError _) = "protocol"
    errorTypeString (InternalError _) = "internal"
    errorTypeString (ConfigError _) = "config"

-- | Decode a message
decodeMessage :: BS.ByteString -> Either Text Message
decodeMessage bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "JSON decode error: " <> T.pack err
        Right val -> case Aeson.fromJSON val of
            Aeson.Error err -> Left $ "JSON conversion error: " <> T.pack err
            Aeson.Success obj -> do
                -- Get the message type
                case AKeyMap.lookup "type" obj of
                    Nothing -> Left "Missing message type"
                    Just (Aeson.String "auth") -> do
                        -- This is an auth message
                        -- Auth messages always have a payload
                        case AKeyMap.lookup "hasPayload" obj of
                            Just (Aeson.Bool True) -> do
                                -- Need to read the payload bytes which should follow
                                -- But we can't do that here - this needs to be handled by the caller
                                -- For now, return a placeholder
                                Right $ AuthMessage BS.empty
                            _ -> Left "Auth message missing payload indicator"

                    Just (Aeson.String "request") -> do
                        -- This is a request message
                        case parseRequest obj of
                            Left err -> Left $ "Failed to parse request: " <> err
                            Right req -> Right $ RequestMessage req

                    Just (Aeson.String "response") -> do
                        -- This is a response message
                        case parseResponse obj of
                            Left err -> Left $ "Failed to parse response: " <> err
                            Right resp -> Right $ ResponseMessage resp

                    Just (Aeson.String "error") -> do
                        -- This is an error message
                        case parseError (Aeson.Object obj) of
                            Left err -> Left $ "Failed to parse error: " <> err
                            Right err -> Right $ ErrorMessage err

                    Just (Aeson.String typ) -> Left $ "Unknown message type: " <> typ
                    _ -> Left "Invalid message type"
  where
    parseRequest :: Aeson.Object -> Either Text Request
    parseRequest obj = do
        reqId <- maybe (Left "Missing request ID") Right $ AKeyMap.lookup "id" obj >>= Aeson.parseMaybe Aeson.parseJSON
        reqType <- maybe (Left "Missing request type") Right $ AKeyMap.lookup "type" obj >>= Aeson.parseMaybe Aeson.parseJSON
        reqParams <- maybe (Left "Missing request params") Right $ AKeyMap.lookup "params" obj >>= Aeson.parseMaybe Aeson.parseJSON
        hasPayload <- maybe (Left "Missing payload indicator") Right $ AKeyMap.lookup "hasPayload" obj >>= Aeson.parseMaybe Aeson.parseJSON

        -- Payload not included in this parse - it should follow the JSON in the byte stream
        -- For now, just return a placeholder
        Right $ Request reqId reqType reqParams (if hasPayload then Just BS.empty else Nothing)

    parseResponse :: Aeson.Object -> Either Text Response
    parseResponse obj = do
        respId <- maybe (Left "Missing response ID") Right $ AKeyMap.lookup "id" obj >>= Aeson.parseMaybe Aeson.parseJSON
        respStatus <- maybe (Left "Missing response status") Right $ AKeyMap.lookup "status" obj >>= Aeson.parseMaybe Aeson.parseJSON
        respMessage <- maybe (Left "Missing response message") Right $ AKeyMap.lookup "message" obj >>= Aeson.parseMaybe Aeson.parseJSON
        respData <- maybe (Left "Missing response data") Right $ AKeyMap.lookup "data" obj >>= Aeson.parseMaybe Aeson.parseJSON
        hasPayload <- maybe (Left "Missing payload indicator") Right $ AKeyMap.lookup "hasPayload" obj >>= Aeson.parseMaybe Aeson.parseJSON

        -- Payload not included in this parse - it should follow the JSON in the byte stream
        -- For now, just return a placeholder
        Right $ Response respId respStatus respMessage respData (if hasPayload then Just BS.empty else Nothing)


-- | Receive a request from the handle
receiveRequest :: Handle -> IO (Either BuildError Request)
receiveRequest handle = do
    msgResult <- receiveMessage handle
    case msgResult of
        Left err -> return $ Left err
        Right (RequestMessage req) -> return $ Right req
        Right _ -> return $ Left $ ProtocolError "Expected request message, got something else"

-- | Generate a new unique build ID
newBuildId :: TenM p t BuildId
newBuildId = liftIO $ BuildId <$> newUnique

-- | Set the current build ID
setCurrentBuildId :: BuildId -> TenM p t ()
setCurrentBuildId bid = modify $ \s -> s { currentBuildId = bid }

-- | Check if a derivation is in the build chain (cycle detection)
isInDerivationChain :: Derivation -> TenM p t Bool
isInDerivationChain drv = do
    chain <- gets buildChain
    -- Use hash comparison for basic equality check to avoid circular dependency
    return $ any (\d -> derivHash drv == derivHash d) chain

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
withDaemon :: (DaemonConnection 'Builder -> IO (Either BuildError a)) -> TenM p 'Builder a
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

-- | Compare two derivations for equality based on hash (core implementation)
derivationEquals :: Derivation -> Derivation -> Bool
derivationEquals d1 d2 = derivHash d1 == derivHash d2

-- | Compare StorePaths for equality
derivationPathsEqual :: StorePath -> StorePath -> Bool
derivationPathsEqual p1 p2 = storeHash p1 == storeHash p2 && storeName p1 == storeName p2

-- | Helper for privilege errors
privilegeError :: Text -> BuildError
privilegeError = PrivilegeError

-- | Spawn a builder process with proper process isolation - privileged operation
spawnBuilderProcess :: FilePath -> [String] -> Map Text Text -> TenM 'Build 'Daemon ProcessID
spawnBuilderProcess programPath args env = do
    -- Placeholder implementation - would include Linux namespace isolation
    pid <- liftIO $ forkProcess $ do
        -- In the real implementation, this would:
        -- 1. Set up a mount namespace with restricted views
        -- 2. Set up a network namespace with controlled access
        -- 3. Set up a user namespace with UID/GID mapping
        -- 4. Apply seccomp filters to restrict syscalls
        -- 5. Drop capabilities
        -- 6. Execute the builder program

        -- For now, just execute the program
        --ENVIRONMENT VARIABLE CONVERSION
        executeFile programPath True args (Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList env)

    return pid

-- | Wait for a builder process to complete
waitForBuilderProcess :: ProcessID -> TenM 'Build 'Daemon (Either BuildError (ExitCode, ByteString))
waitForBuilderProcess pid = do
    -- Wait for the process to exit
    result <- liftIO $ try $ getProcessStatus True True pid

    case result of
        Left (e :: SomeException) ->
            return $ Left $ BuildFailed $ "Error waiting for builder process: " <> T.pack (show e)

        Right Nothing ->
            return $ Left $ BuildFailed "Failed to get process status"

        Right (Just (Exited exitCode)) -> do
            -- Process exited normally with exit code
            let exitResult = case exitCode of
                    ExitSuccess -> ExitSuccess
                    ExitFailure code -> ExitFailure code

            -- In a real implementation, we would read output from pipes here
            -- For simplicity, we're returning empty content
            return $ Right (exitResult, BS.empty)

        Right (Just (Terminated signal _)) ->
            return $ Left $ BuildFailed $ "Builder process terminated by signal: " <> T.pack (show signal)

        Right (Just (Stopped signal)) ->
            return $ Left $ BuildFailed $ "Builder process stopped by signal: " <> T.pack (show signal)

-- | Type-safe operation for interacting with the store
withStore :: SPrivilegeTier 'Daemon -> (SPrivilegeTier 'Daemon -> TenM p 'Daemon a) -> TenM p 'Daemon a
withStore st action = do
    -- Since we already have daemon privileges, just execute the action
    action st

-- | Get current time in milliseconds
getCurrentMillis :: IO Int
getCurrentMillis = do
    now <- getCurrentTime
    let nowSeconds = diffUTCTime now (posixSecondsToUTCTime 0)
    return $ floor $ realToFrac nowSeconds * 1000

-- | Timeout helper
timeout :: Int -> IO a -> IO (Maybe a)
timeout micros action = do
    result <- newEmptyMVar
    tid <- forkIO $ do
        value <- try action
        case value of
            Left (e :: SomeException) -> return ()
            Right v -> putMVar result v

    -- Wait for result or timeout
    threadDelay micros
    filled <- isJust <$> tryTakeMVar result

    unless filled $ killThread tid

    if filled
        then Just <$> readMVar result
        else return Nothing

-- | Hash a ByteString
hashByteString :: BS.ByteString -> Digest SHA256
hashByteString = Crypto.hash

-- | Serialize a derivation to a ByteString for cross-boundary transmission
serializeDerivation :: Derivation -> BS.ByteString
serializeDerivation drv =
    -- Convert to JSON and then to ByteString with proper formatting
    LBS.toStrict $ Aeson.encode $ Aeson.object
        [ "name" Aeson..= derivName drv
        , "hash" Aeson..= derivHash drv
        , "builder" Aeson..= encodeStorePath (derivBuilder drv)
        , "args" Aeson..= derivArgs drv
        , "inputs" Aeson..= map encodeDerivationInput (Set.toList $ derivInputs drv)
        , "outputs" Aeson..= map encodeDerivationOutput (Set.toList $ derivOutputs drv)
        , "env" Aeson..= Map.mapKeys T.unpack (derivEnv drv)
        , "system" Aeson..= derivSystem drv
        , "strategy" Aeson..= (case derivStrategy drv of
                           ApplicativeStrategy -> "applicative" :: Text
                           MonadicStrategy -> "monadic" :: Text)
        , "meta" Aeson..= derivMeta drv
        ]
  where
    encodeStorePath :: StorePath -> Aeson.Value
    encodeStorePath (StorePath hash name) = Aeson.object
        [ "hash" Aeson..= hash
        , "name" Aeson..= name
        ]

    encodeDerivationInput :: DerivationInput -> Aeson.Value
    encodeDerivationInput (DerivationInput path name) = Aeson.object
        [ "path" Aeson..= encodeStorePath path
        , "name" Aeson..= name
        ]

    encodeDerivationOutput :: DerivationOutput -> Aeson.Value
    encodeDerivationOutput (DerivationOutput name path) = Aeson.object
        [ "name" Aeson..= name
        , "path" Aeson..= encodeStorePath path
        ]

-- | Deserialize a derivation from ByteString format
deserializeDerivation :: BS.ByteString -> Either BuildError Derivation
deserializeDerivation bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ SerializationError $ "JSON parse error: " <> T.pack err
        Right val -> case fromJSON val of
            Aeson.Error err -> Left $ SerializationError $ "JSON conversion error: " <> T.pack err
            Aeson.Success drv -> Right drv
  where
    fromJSON :: Aeson.Value -> Aeson.Result Derivation
    fromJSON val = case Aeson.parseEither parser val of
        Left err -> Aeson.Error err
        Right x -> Aeson.Success x

    parser = Aeson.withObject "Derivation" $ \o -> do
        name <- o Aeson..: "name"
        hash <- o Aeson..: "hash"
        builderObj <- o Aeson..: "builder"
        builder <- parseStorePath builderObj
        args <- o Aeson..: "args"
        inputsArray <- o Aeson..: "inputs"
        inputs <- mapM parseInput inputsArray
        outputsArray <- o Aeson..: "outputs"
        outputs <- mapM parseOutput outputsArray
        envObj <- o Aeson..: "env"
        env <- parseEnvMap envObj
        system <- o Aeson..: "system"
        strategyText <- o Aeson..: "strategy"
        let strategy = if strategyText == ("monadic" :: Text)
                       then MonadicStrategy
                       else ApplicativeStrategy
        metaObj <- o Aeson..: "meta"
        meta <- parseEnvMap metaObj

        return $ Derivation name hash builder args
                           (Set.fromList inputs) (Set.fromList outputs)
                           env system strategy meta

    parseStorePath :: Aeson.Value -> Parser StorePath
    parseStorePath = Aeson.withObject "StorePath" $ \o -> do
        hash <- o Aeson..: "hash"
        name <- o Aeson..: "name"
        let path = StorePath hash name
        if validateStorePath path
            then return path
            else fail $ "Invalid store path format: " ++ T.unpack hash ++ "-" ++ T.unpack name

    parseInput :: Aeson.Value -> Parser DerivationInput
    parseInput = Aeson.withObject "DerivationInput" $ \o -> do
        pathObj <- o Aeson..: "path"
        path <- parseStorePath pathObj
        name <- o Aeson..: "name"
        return $ DerivationInput path name

    parseOutput :: Aeson.Value -> Parser DerivationOutput
    parseOutput = Aeson.withObject "DerivationOutput" $ \o -> do
        name <- o Aeson..: "name"
        pathObj <- o Aeson..: "path"
        path <- parseStorePath pathObj
        return $ DerivationOutput name path

    parseEnvMap :: Aeson.Value -> Parser (Map Text Text)
    parseEnvMap = Aeson.withObject "Environment" $ \o ->
        return $ Map.fromList [(Key.toText k, v) | (k, Aeson.String v) <- AKeyMap.toList o]

-- | Serialize BuildResult to ByteString
serializeBuildResult :: BuildResult -> BS.ByteString
serializeBuildResult result =
    LBS.toStrict $ Aeson.encode $ Aeson.object
        [ "outputs" Aeson..= map encodeStorePath (Set.toList $ brOutputPaths result)
        , "exitCode" Aeson..= encodeExitCode (brExitCode result)
        , "log" Aeson..= brLog result
        , "references" Aeson..= map encodeStorePath (Set.toList $ brReferences result)
        ]
  where
    encodeStorePath :: StorePath -> Aeson.Value
    encodeStorePath (StorePath hash name) = Aeson.object
        [ "hash" Aeson..= hash
        , "name" Aeson..= name
        ]

    encodeExitCode :: ExitCode -> Aeson.Value
    encodeExitCode ExitSuccess = Aeson.object
        [ "type" Aeson..= ("success" :: Text)
        ]
    encodeExitCode (ExitFailure code) = Aeson.object
        [ "type" Aeson..= ("failure" :: Text)
        , "code" Aeson..= code
        ]

-- | Deserialize BuildResult from ByteString
deserializeBuildResult :: BS.ByteString -> Either BuildError BuildResult
deserializeBuildResult bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ SerializationError $ "JSON parse error: " <> T.pack err
        Right val -> case fromJSON val of
            Aeson.Error err -> Left $ SerializationError $ "JSON conversion error: " <> T.pack err
            Aeson.Success result -> Right result
  where
    fromJSON :: Aeson.Value -> Aeson.Result BuildResult
    fromJSON val = case Aeson.parseEither parseBuildResult val of
        Left err -> Aeson.Error err  -- Return Aeson.Error directly, not wrapped in Left
        Right x -> Aeson.Success x   -- Return Aeson.Success directly, not wrapped in Right

    parseBuildResult :: Aeson.Value -> Parser BuildResult
    parseBuildResult = Aeson.withObject "BuildResult" $ \o -> do
        outputsArray <- o Aeson..: "outputs"
        outputs <- mapM parseStorePath outputsArray
        exitCodeVal <- o Aeson..: "exitCode"
        exitCode <- parseExitCode exitCodeVal
        buildLog <- o Aeson..: "log"
        refsArray <- o Aeson..: "references"
        refs <- mapM parseStorePath refsArray

        return BuildResult
            { brOutputPaths = Set.fromList outputs
            , brExitCode = exitCode
            , brLog = buildLog
            , brReferences = Set.fromList refs
            }

    parseStorePath :: Aeson.Value -> Parser StorePath
    parseStorePath = Aeson.withObject "StorePath" $ \o -> do
        hash <- o Aeson..: "hash"
        name <- o Aeson..: "name"
        let path = StorePath hash name
        if validateStorePath path
            then return path
            else fail $ "Invalid store path format during deserialization: " ++
                       T.unpack hash ++ "-" ++ T.unpack name

    parseExitCode :: Aeson.Value -> Parser ExitCode
    parseExitCode = Aeson.withObject "ExitCode" $ \o -> do
        codeType <- o Aeson..: "type" :: Parser Text
        if codeType == "success"
            then return ExitSuccess
            else do
                code <- o Aeson..: "code"
                return $ ExitFailure code

instance Aeson.FromJSON Response where
  parseJSON = Aeson.withObject "Response" $ \o -> do
    id <- o Aeson..: "id"
    status <- o Aeson..: "status"
    message <- o Aeson..: "message"
    respData <- o Aeson..: "data"
    hasPayload <- o Aeson..:? "hasPayload" Aeson..!= False
    return Response {
      respId = id,
      respStatus = status,
      respMessage = message,
      respData = respData,
      respPayload = if hasPayload then Just BS.empty else Nothing
    }

instance Aeson.ToJSON BuildRequestInfo where
    toJSON BuildRequestInfo{..} = Aeson.object [
            "timeout" Aeson..= buildTimeout,
            "env" Aeson..= buildEnv,
            "flags" Aeson..= buildFlags
        ]

instance Aeson.FromJSON BuildRequestInfo where
    parseJSON = Aeson.withObject "BuildRequestInfo" $ \v -> do
        buildTimeout <- v Aeson..: "timeout"
        buildEnv <- v Aeson..: "env"
        buildFlags <- v Aeson..: "flags"
        return BuildRequestInfo{..}

instance Aeson.ToJSON Response where
  toJSON resp = Aeson.object [
      "id" Aeson..= respId resp,
      "status" Aeson..= respStatus resp,
      "message" Aeson..= respMessage resp,
      "data" Aeson..= respData resp,
      "hasPayload" Aeson..= isJust (respPayload resp)
    ]

-- Similarly for Request
instance Aeson.FromJSON Request where
  parseJSON = Aeson.withObject "Request" $ \o -> do
    id <- o Aeson..: "id"
    reqType <- o Aeson..: "type"
    params <- o Aeson..: "params"
    hasPayload <- o Aeson..:? "hasPayload" Aeson..!= False
    return Request {
      reqId = id,
      reqType = reqType,
      reqParams = params,
      reqPayload = if hasPayload then Just BS.empty else Nothing
    }

instance Aeson.ToJSON Request where
  toJSON req = Aeson.object [
      "id" Aeson..= reqId req,
      "type" Aeson..= reqType req,
      "params" Aeson..= reqParams req,
      "hasPayload" Aeson..= isJust (reqPayload req)
    ]

-- JSON instances for ProtocolVersion
instance Aeson.ToJSON ProtocolVersion where
    toJSON (ProtocolVersion major minor patch) = Aeson.object [
        "major" Aeson..= major,
        "minor" Aeson..= minor,
        "patch" Aeson..= patch
        ]

instance Aeson.FromJSON ProtocolVersion where
    parseJSON = Aeson.withObject "ProtocolVersion" $ \v -> do
        major <- v Aeson..: "major"
        minor <- v Aeson..: "minor"
        patch <- v Aeson..: "patch"
        return $ ProtocolVersion major minor patch

-- JSON instances for StorePath
instance Aeson.ToJSON StorePath where
    toJSON (StorePath hash name) = Aeson.object [
        "hash" Aeson..= hash,
        "name" Aeson..= name
        ]

instance Aeson.FromJSON StorePath where
    parseJSON = Aeson.withObject "StorePath" $ \v -> do
        hash <- v Aeson..: "hash"
        name <- v Aeson..: "name"
        let path = StorePath hash name
        if validateStorePath path
            then return path
            else fail $ "Invalid store path format: " ++ T.unpack hash ++ "-" ++ T.unpack name

-- JSON instances for AuthResult
instance Aeson.ToJSON AuthResult where
    toJSON (AuthAccepted userId token capabilities) = Aeson.object [
        "result" Aeson..= ("accepted" :: Text),
        "userId" Aeson..= case userId of UserId u -> u,
        "token" Aeson..= case token of AuthToken t -> t,
        "capabilities" Aeson..= map show (Set.toList capabilities)
        ]
    toJSON (AuthRejected reason) = Aeson.object [
        "result" Aeson..= ("rejected" :: Text),
        "reason" Aeson..= reason
        ]
    toJSON (AuthSuccess userId token) = Aeson.object [
        "result" Aeson..= ("success" :: Text),
        "userId" Aeson..= case userId of UserId u -> u,
        "token" Aeson..= case token of AuthToken t -> t
        ]

instance Aeson.FromJSON AuthResult where
    parseJSON = Aeson.withObject "AuthResult" $ \v -> do
        result <- v Aeson..: "result" :: Parser Text
        case result of
            "accepted" -> do
                userIdText <- v Aeson..: "userId"
                tokenText <- v Aeson..: "token"
                capabilitiesStr <- v Aeson..:? "capabilities" Aeson..!= []
                let capabilities = Set.fromList $ catMaybes $ map parseCapability capabilitiesStr
                return $ AuthAccepted (UserId userIdText) (AuthToken tokenText) capabilities
            "rejected" -> do
                reason <- v Aeson..: "reason"
                return $ AuthRejected reason
            "success" -> do
                userIdText <- v Aeson..: "userId"
                tokenText <- v Aeson..: "token"
                return $ AuthSuccess (UserId userIdText) (AuthToken tokenText)
            _ -> fail $ "Unknown auth result: " ++ T.unpack result
      where
        parseCapability :: Text -> Maybe Capability
        parseCapability "StoreAccess" = Just StoreAccess
        parseCapability "SandboxCreation" = Just SandboxCreation
        parseCapability "GarbageCollection" = Just GarbageCollection
        parseCapability "DerivationRegistration" = Just DerivationRegistration
        parseCapability "DerivationBuild" = Just DerivationBuild
        parseCapability "StoreQuery" = Just StoreQuery
        parseCapability "BuildQuery" = Just BuildQuery
        parseCapability _ = Nothing

-- JSON instances for BuildStatusUpdate
instance Aeson.ToJSON BuildStatusUpdate where
    toJSON BuildStatusUpdate{..} = Aeson.object [
        "buildId" Aeson..= renderBuildId buildId,
        "status" Aeson..= encodeStatus buildStatus,
        "timeElapsed" Aeson..= buildTimeElapsed,
        "timeRemaining" Aeson..= buildTimeRemaining,
        "logUpdate" Aeson..= buildLogUpdate,
        "resourceUsage" Aeson..= buildResourceUsage
        ]
      where
        encodeStatus :: BuildStatus -> Aeson.Value
        encodeStatus BuildPending = Aeson.object [
            "state" Aeson..= ("pending" :: Text),
            "progress" Aeson..= (0 :: Float)
            ]
        encodeStatus (BuildRunning progress) = Aeson.object [
            "state" Aeson..= ("running" :: Text),
            "progress" Aeson..= progress
            ]
        encodeStatus (BuildRecursing bid) = Aeson.object [
            "state" Aeson..= ("recursing" :: Text),
            "childId" Aeson..= renderBuildId bid
            ]
        encodeStatus BuildCompleted = Aeson.object [
            "state" Aeson..= ("completed" :: Text),
            "progress" Aeson..= (1.0 :: Float)
            ]
        encodeStatus BuildFailed' = Aeson.object [
            "state" Aeson..= ("failed" :: Text),
            "progress" Aeson..= (0 :: Float)
            ]

instance Aeson.FromJSON BuildStatusUpdate where
    parseJSON = Aeson.withObject "BuildStatusUpdate" $ \v -> do
        buildIdText <- v Aeson..: "buildId"
        buildId <- case parseBuildId buildIdText of
            Left err -> fail $ T.unpack err
            Right bid -> return bid

        statusObj <- v Aeson..: "status"
        buildStatus <- parseStatus statusObj

        buildTimeElapsed <- v Aeson..: "timeElapsed"
        buildTimeRemaining <- v Aeson..:? "timeRemaining"
        buildLogUpdate <- v Aeson..:? "logUpdate"
        buildResourceUsage <- v Aeson..:? "resourceUsage" Aeson..!= Map.empty

        return BuildStatusUpdate{..}
      where
        parseStatus :: Aeson.Value -> Parser BuildStatus
        parseStatus = Aeson.withObject "BuildStatus" $ \s -> do
            state <- s Aeson..: "state" :: Parser Text
            case state of
                "pending" -> return BuildPending
                "running" -> do
                    progress <- s Aeson..: "progress"
                    return $ BuildRunning progress
                "recursing" -> do
                    childId <- s Aeson..: "childId"
                    buildId <- case parseBuildId childId of
                        Left err -> fail $ T.unpack err
                        Right bid -> return bid
                    return $ BuildRecursing buildId
                "completed" -> return BuildCompleted
                "failed" -> return BuildFailed'
                _ -> fail $ "Unknown build status: " ++ T.unpack state

-- JSON instances for GCStatusInfo
instance Aeson.ToJSON GCStatusInfo where
    toJSON GCStatusInfo{..} = Aeson.object [
        "running" Aeson..= gcRunning,
        "owner" Aeson..= gcOwner,
        "lockTime" Aeson..= gcLockTime
        ]

instance Aeson.FromJSON GCStatusInfo where
    parseJSON = Aeson.withObject "GCStatusInfo" $ \v -> do
        gcRunning <- v Aeson..: "running"
        gcOwner <- v Aeson..:? "owner"
        gcLockTime <- v Aeson..:? "lockTime"
        return GCStatusInfo{..}

-- JSON instances for DaemonStatus
instance Aeson.ToJSON DaemonStatus where
    toJSON DaemonStatus{..} = Aeson.object [
        "status" Aeson..= daemonStatus,
        "uptime" Aeson..= daemonUptime,
        "activeBuilds" Aeson..= daemonActiveBuilds,
        "completedBuilds" Aeson..= daemonCompletedBuilds,
        "failedBuilds" Aeson..= daemonFailedBuilds,
        "gcRoots" Aeson..= daemonGcRoots,
        "storeSize" Aeson..= daemonStoreSize,
        "storePaths" Aeson..= daemonStorePaths
        ]

instance Aeson.FromJSON DaemonStatus where
    parseJSON = Aeson.withObject "DaemonStatus" $ \v -> do
        daemonStatus <- v Aeson..: "status"
        daemonUptime <- v Aeson..: "uptime"
        daemonActiveBuilds <- v Aeson..: "activeBuilds"
        daemonCompletedBuilds <- v Aeson..: "completedBuilds"
        daemonFailedBuilds <- v Aeson..: "failedBuilds"
        daemonGcRoots <- v Aeson..: "gcRoots"
        daemonStoreSize <- v Aeson..: "storeSize"
        daemonStorePaths <- v Aeson..: "storePaths"
        return DaemonStatus{..}

-- JSON instances for BuildResult
instance Aeson.ToJSON BuildResult where
    toJSON BuildResult{..} = Aeson.object [
        "outputs" Aeson..= Set.map storePathToText brOutputPaths,
        "exitCode" Aeson..= encodeExitCode brExitCode,
        "log" Aeson..= brLog,
        "references" Aeson..= Set.map storePathToText brReferences
        ]
      where
        encodeExitCode ExitSuccess = Aeson.object [
            "code" Aeson..= (0 :: Int),
            "success" Aeson..= True
            ]
        encodeExitCode (ExitFailure code) = Aeson.object [
            "code" Aeson..= code,
            "success" Aeson..= False
            ]

-- Add this after the GCStats definition:
instance Aeson.FromJSON GCStats where
    parseJSON = Aeson.withObject "GCStats" $ \v -> do
        gcTotal <- v Aeson..: "total"
        gcLive <- v Aeson..: "live"
        gcCollected <- v Aeson..: "collected"
        gcBytes <- v Aeson..: "bytes"
        gcElapsedTime <- v Aeson..: "elapsedTime"
        return GCStats{..}

-- Add this after the DaemonConfig definition:
instance Aeson.FromJSON DaemonConfig where
    parseJSON = Aeson.withObject "DaemonConfig" $ \v -> do
        daemonSocketPath <- v Aeson..: "socketPath"
        daemonStorePath <- v Aeson..: "storePath"
        daemonStateFile <- v Aeson..: "stateFile"
        daemonLogFile <- v Aeson..:? "logFile"
        daemonLogLevel <- v Aeson..: "logLevel"
        daemonGcInterval <- v Aeson..:? "gcInterval"
        daemonUser <- v Aeson..:? "user"
        daemonGroup <- v Aeson..:? "group"
        daemonAllowedUsers <- v Aeson..: "allowedUsers"
        daemonMaxJobs <- v Aeson..: "maxJobs"
        daemonForeground <- v Aeson..: "foreground"
        daemonTmpDir <- v Aeson..: "tmpDir"
        return DaemonConfig{..}

-- For BuildError, let's implement the parseError function:
parseError :: Aeson.Value -> Either Text BuildError
parseError (Aeson.Object o) = do
    errType <- maybe (Left "Missing error type") Right $
               AKeyMap.lookup "type" o >>= Aeson.parseMaybe Aeson.parseJSON
    msg <- maybe (Left "Missing error message") Right $
          AKeyMap.lookup "message" o >>= Aeson.parseMaybe Aeson.parseJSON

    case errType of
        "eval" -> Right $ EvalError msg
        "build" -> Right $ BuildFailed msg
        "store" -> Right $ StoreError msg
        "sandbox" -> Right $ SandboxError msg
        "input" -> Right $ InputNotFound (T.unpack msg)
        "hash" -> Right $ HashError msg
        "graph" -> Right $ GraphError msg
        "resource" -> Right $ ResourceError msg
        "daemon" -> Right $ DaemonError msg
        "auth" -> Right $ AuthError msg
        "cycle" -> Right $ CyclicDependency msg
        "serialization" -> Right $ SerializationError msg
        "recursion" -> Right $ RecursionLimit msg
        "network" -> Right $ NetworkError msg
        "parse" -> Right $ ParseError msg
        "db" -> Right $ DBError msg
        "gc" -> Right $ GCError msg
        "phase" -> Right $ PhaseError msg
        "privilege" -> Right $ PrivilegeError msg
        "protocol" -> Right $ ProtocolError msg
        "internal" -> Right $ InternalError msg
        "config" -> Right $ ConfigError msg
        _ -> Left $ "Unknown error type: " <> errType

parseError _ = Left "Error value is not an object"

parseExitCode :: Aeson.Value -> Parser ExitCode
parseExitCode = Aeson.withObject "ExitCode" $ \o -> do
    codeType <- o Aeson..: "type" :: Parser Text
    if codeType == "success"
        then return ExitSuccess
        else do
            code <- o Aeson..: "code"
            return $ ExitFailure code

instance Aeson.FromJSON BuildResult where
    parseJSON = Aeson.withObject "BuildResult" $ \v -> do
        outputTexts <- v Aeson..: "outputs" :: Parser [Text]
        exitCodeObj <- v Aeson..: "exitCode"
        log <- v Aeson..: "log"
        referenceTexts <- v Aeson..: "references" :: Parser [Text]

        -- Parse exit code
        exitCode <- parseExitCode exitCodeObj

        -- Parse store paths with validation
        let outputs = Set.fromList $ catMaybes $ map parseStorePath outputTexts
        let references = Set.fromList $ catMaybes $ map parseStorePath referenceTexts

        return $ BuildResult
            { brOutputPaths = outputs
            , brExitCode = exitCode
            , brLog = log
            , brReferences = references
            }
