module Ten
  ( -- Core types
    TenError(..)
    -- Env types
  , BuildEnv(..)
  , TenM
    -- Store operations
  , StorePath
  , addToStore
  , storeFile
  , storeDirectory
  , checkStorePathExists
  , readStoreContent
  , scanFileForStoreReferences
    -- Derivation operations
  , Derivation
  , BuildResult
  , mkDerivation
  , instantiateDerivation
  , hashDerivation
  , buildDerivationGraph
    -- Sandbox operations
  , SandboxConfig
  , withSandbox
    -- Build graph operations
  , createBuildGraph
  , detectCycles
    -- GC operations
  , registerGCRoot
    -- Database operations
  , Database
  , initializeDatabase
  , getReferences
  , getReferrers
    -- Logging
  , logMsg
    -- Auth operations
  , createAuthToken
    -- Capability operations
  , requestCapabilities
  , verifyCapabilities
  , capabilityRequiresDaemon
  , filterCapabilitiesForTier
    -- Environment setup
  , initDaemonEnv
  ) where

-- Standard library imports for basic types
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified System.IO.Error as IOE
import qualified System.FilePath as FP

-- Project-specific imports
import qualified Ten.Core as Core
import qualified Ten.Store as Store
import qualified Ten.Build as Build
import qualified Ten.Sandbox as Sandbox
import qualified Ten.DB.Core as DB
import qualified Ten.Daemon.Auth as Auth
import qualified Ten.Daemon.Protocol as Protocol

-- Re-exports from Ten.Core
import Ten.Core
  ( TenM
  , StorePath
  , Derivation
  , BuildResult
  , Database
  )

-- Main error type for the Ten system
data TenError
    = EvalError T.Text
    | BuildFailed T.Text
    | StoreError T.Text
    | SandboxError T.Text
    | IOError T.Text
    | HashError T.Text
    | GraphError T.Text
    | ResourceError T.Text
    | DaemonError T.Text
    | AuthError T.Text
    | CyclicDependency T.Text
    | SerializationError T.Text
    | RecursionLimit T.Text
    | NetworkError T.Text
    | ParseError T.Text
    | DBError T.Text
    | GCError T.Text
    | PhaseError T.Text
    | PrivilegeError T.Text
    | ProtocolError T.Text
    | InternalError T.Text
    | ConfigError T.Text
    deriving (Show, Eq)

-- Environment for build operations
data BuildEnv = BuildEnv
    { storeDir      :: FilePath
    , buildRoot     :: FilePath
    , authToken     :: Maybe T.Text
    , dbConnection  :: Maybe DB.DBConnection
    }

-- Initialize the daemon environment with the necessary paths and auth token
initDaemonEnv :: FilePath -> FilePath -> Maybe T.Text -> BuildEnv
initDaemonEnv store build token = BuildEnv
    { storeDir = store
    , buildRoot = build
    , authToken = token
    , dbConnection = Nothing
    }

-- Store content operations

-- Add content to the store with a given name and raw bytes
addToStore :: Store.StoreContentOps t => T.Text -> BS.ByteString -> TenM p t StorePath
addToStore name content = Store.addContent name content

-- Store a file in the content-addressable store
storeFile :: Store.StoreContentOps t => FilePath -> TenM p t StorePath
storeFile path = Store.addFile path

-- Store a directory in the content-addressable store
storeDirectory :: Store.StoreContentOps t => FilePath -> TenM p t StorePath
storeDirectory path = Store.addDirectory path

-- Check if a path exists in the store
checkStorePathExists :: Store.StoreAccessOps t => StorePath -> TenM p t Bool
checkStorePathExists path = Store.pathExists path

-- Read content from a store path
readStoreContent :: Store.StoreAccessOps t => StorePath -> TenM p t BS.ByteString
readStoreContent path = Store.readContent path

-- Scan a file for references to store paths
scanFileForStoreReferences :: Store.StoreScanOps t => FilePath -> TenM p t (Set.Set StorePath)
scanFileForStoreReferences path = Store.scanFile path

-- Derivation operations

-- Create a new derivation with the given parameters
mkDerivation :: T.Text                        -- ^ Derivation name
             -> StorePath                     -- ^ Builder path
             -> [T.Text]                      -- ^ Arguments to the builder
             -> Set.Set Core.DerivationInput  -- ^ Input derivations
             -> Set.Set T.Text                -- ^ Output paths
             -> Map.Map T.Text T.Text         -- ^ Environment variables
             -> T.Text                        -- ^ Platform
             -> TenM 'Core.Eval t Derivation
mkDerivation name builder args inputs outputs env platform =
    Core.createDerivation name builder args inputs outputs env platform

-- Instantiate a derivation (make it concrete and ready to build)
instantiateDerivation :: Build.CanBuildDerivation t => Derivation -> TenM 'Core.Build t ()
instantiateDerivation deriv = Build.instantiate deriv

-- Calculate the cryptographic hash of a derivation
hashDerivation :: Derivation -> T.Text
hashDerivation = Build.hashDerivation

-- Build a graph of derivations
buildDerivationGraph :: Build.CanBuildDerivation t => Core.BuildGraph -> TenM 'Core.Build t (Map.Map T.Text BuildResult)
buildDerivationGraph graph = Build.buildGraph graph

-- Sandbox operations

-- Execute an action within a sandbox environment
withSandbox :: Sandbox.SandboxOps t
            => Set.Set StorePath        -- ^ Input paths to make available in the sandbox
            -> SandboxConfig           -- ^ Sandbox configuration
            -> (FilePath -> TenM 'Core.Build t a)  -- ^ Action to run in the sandbox
            -> TenM 'Core.Build t a
withSandbox inputs config action = Sandbox.withSandbox inputs config action

-- Build graph operations

-- Create a build graph from a set of store paths and derivations
createBuildGraph :: Core.SingI t
                 => Core.SPrivilegeTier t
                 -> Set.Set StorePath
                 -> Set.Set Derivation
                 -> TenM 'Core.Eval t Core.BuildGraph
createBuildGraph tier paths derivs = Core.createBuildGraph tier paths derivs

-- Detect cycles in the build graph
detectCycles :: Core.SingI t
             => Core.SPrivilegeTier t
             -> Core.BuildGraph
             -> TenM 'Core.Eval t (Bool, Set.Set T.Text, Set.Set T.Text)
detectCycles tier graph = Core.detectCycles tier graph

-- GC operations

-- Register a store path as a GC root
registerGCRoot :: StorePath -> T.Text -> Bool -> TenM 'Core.Build 'Core.Daemon Core.GCRoot
registerGCRoot path name indirect = Core.registerGCRoot path name indirect

-- Database operations

-- Initialize the database
initializeDatabase :: FilePath -> TenM 'Core.Build 'Core.Daemon ()
initializeDatabase dbPath = DB.initialize dbPath

-- Get references from a store path
getReferences :: DB.CanQueryReferences t => Database t -> StorePath -> TenM p t (Set.Set StorePath)
getReferences db path = DB.getReferences db path

-- Get referrers to a store path
getReferrers :: DB.CanQueryReferences t => Database t -> StorePath -> TenM p t (Set.Set StorePath)
getReferrers db path = DB.getReferrers db path

-- Logging

-- Log a message with a given level
logMsg :: Int -> T.Text -> TenM p t ()
logMsg level msg = Core.logMessage level msg

-- Auth operations

-- Create an authentication token
createAuthToken :: T.Text -> IO Core.AuthToken
createAuthToken = Auth.create

-- Capability operations

-- Get the capabilities required by a daemon request
requestCapabilities :: Protocol.DaemonRequest -> Set.Set Protocol.DaemonCapability
requestCapabilities = Protocol.requestCapabilities

-- Verify that a privilege tier has the required capabilities
verifyCapabilities :: Core.SPrivilegeTier t
                   -> Set.Set Protocol.DaemonCapability
                   -> Either Core.PrivilegeError ()
verifyCapabilities tier caps = Auth.verifyCapabilities tier caps

-- Check if a capability requires daemon privileges
capabilityRequiresDaemon :: Protocol.DaemonCapability -> Bool
capabilityRequiresDaemon = Auth.requiresDaemon

-- Filter capabilities based on privilege tier
filterCapabilitiesForTier :: Core.SPrivilegeTier t
                          -> Set.Set Protocol.DaemonCapability
                          -> Set.Set Protocol.DaemonCapability
filterCapabilitiesForTier tier caps = Auth.filterCapabilitiesForTier tier caps
