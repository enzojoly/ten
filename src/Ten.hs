module Ten
  ( -- Core types
    TenError(..)
    -- Env types
  , BuildEnv(..)
  , TenM
    -- Type classes for store operations
  , StoreAccessOps(..)
  , StoreContentOps(..)
  , StoreModificationOps(..)
  , StoreQueryOps(..)
  , StoreScanOps(..)
  , GCManagementOps(..)
    -- Store operations (re-exported through type classes)
  , StorePath
  , addToStore
  , storeFile
  , storeDirectory
  , checkStorePathExists
  , readStoreContent
  , scanFileForStoreReferences
    -- Derivation type classes
  , CanStoreDerivation(..)
  , CanRetrieveDerivation(..)
  , CanStoreBuildDerivation(..)
    -- Derivation operations
  , Derivation
  , BuildResult
  , mkDerivation
  , instantiateDerivation
  , hashDerivation
  , buildDerivationGraph
    -- Sandbox operations
  , SandboxConfig
  , SandboxCreator(..)
  , withSandbox
    -- Build graph operations
  , createBuildGraph
  , detectCycles
    -- GC operations
  , registerGCRoot
    -- Database operations
  , Database
  , HasDirectQueryOps(..)
  , HasTransactionOps(..)
  , HasStoreOps(..)
  , HasDerivationOps(..)
  , HasBuildOps(..)
  , HasGCOps(..)
  , HasSchemaOps(..)
  , initializeDatabase
  , getReferences
  , getReferrers
    -- Logging
  , logMsg
    -- Auth operations
  , createAuthToken
    -- Capability operations
  , DaemonCapability(..)
  , requestCapabilities
  , verifyCapabilities
  , capabilityRequiresDaemon
  , filterCapabilitiesForTier
    -- Environment setup
  , initDaemonEnv
  ) where

-- Import modules with proper type constraints
import qualified Ten.Core as Core
import qualified Ten.Store as Store
import qualified Ten.Derivation as Derivation
import qualified Ten.DB.Core as DB
import qualified Ten.Daemon.Auth as Auth
import qualified Ten.Daemon.Protocol as Protocol

-- Re-export with proper type constraints
import Ten.Core
  ( TenM
  , StorePath
  , Derivation
  , BuildResult
  , Database
  , TenError(..)
  , BuildEnv(..)
  )

-- Re-export store operations with proper type constraints
addToStore :: Store.StoreContentOps t => Text -> ByteString -> TenM p t StorePath
addToStore = Store.addToStore

storeFile :: Store.StoreContentOps t => FilePath -> TenM p t StorePath
storeFile = Store.storeFile

storeDirectory :: Store.StoreContentOps t => FilePath -> TenM p t StorePath
storeDirectory = Store.storeDirectory

checkStorePathExists :: Store.StoreAccessOps t => StorePath -> TenM p t Bool
checkStorePathExists = Store.checkStorePathExists

readStoreContent :: Store.StoreAccessOps t => StorePath -> TenM p t ByteString
readStoreContent = Store.readStoreContent

scanFileForStoreReferences :: Store.StoreScanOps t => FilePath -> TenM p t (Set StorePath)
scanFileForStoreReferences = Store.scanFileForStoreReferences

-- Re-export derivation operations with proper constraints
mkDerivation :: Text -> StorePath -> [Text] -> Set DerivationInput -> Set Text -> Map Text Text -> Text -> TenM 'Eval t Derivation
mkDerivation = Derivation.mkDerivation

instantiateDerivation :: Derivation.CanStoreBuildDerivation t => Derivation -> TenM 'Build t ()
instantiateDerivation = Derivation.instantiateDerivation

hashDerivation :: Derivation -> Text
hashDerivation = Derivation.hashDerivation

buildDerivationGraph :: Build.CanBuildDerivation t => BuildGraph -> TenM 'Build t (Map Text BuildResult)
buildDerivationGraph = Build.buildGraph

-- Re-export sandbox operations
withSandbox :: Derivation.SandboxCreator t => Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build t a) -> TenM 'Build t a
withSandbox = Derivation.withSandbox

-- Re-export build graph operations
createBuildGraph :: Core.SingI t => SPrivilegeTier t -> Set StorePath -> Set Derivation -> TenM 'Eval t BuildGraph
createBuildGraph = Core.createBuildGraph

detectCycles :: Core.SingI t => SPrivilegeTier t -> BuildGraph -> TenM 'Eval t (Bool, Set Text, Set Text)
detectCycles = Core.detectCycles

-- Re-export GC operations
registerGCRoot :: StorePath -> Text -> Bool -> TenM 'Build 'Daemon GCRoot
registerGCRoot = Core.registerGCRoot

-- Re-export DB operations
initializeDatabase :: FilePath -> TenM 'Build 'Daemon ()
initializeDatabase = DB.initialize

getReferences :: DB.CanQueryReferences t => Database t -> StorePath -> TenM p t (Set StorePath)
getReferences = DB.getReferences

getReferrers :: DB.CanQueryReferences t => Database t -> StorePath -> TenM p t (Set StorePath)
getReferrers = DB.getReferrers

-- Re-export logging
logMsg :: Int -> Text -> TenM p t ()
logMsg = Core.logMessage

-- Re-export auth operations
createAuthToken :: Text -> IO AuthToken
createAuthToken = Auth.create

-- Re-export capability operations
requestCapabilities :: Protocol.DaemonRequest -> Set Protocol.DaemonCapability
requestCapabilities = Protocol.requestCapabilities

verifyCapabilities :: SPrivilegeTier t -> Set Protocol.DaemonCapability -> Either Core.PrivilegeError ()
verifyCapabilities = Protocol.verifyCapabilities

capabilityRequiresDaemon :: Protocol.DaemonCapability -> Bool
capabilityRequiresDaemon = Auth.requiresDaemon

filterCapabilitiesForTier :: SPrivilegeTier t -> Set Protocol.DaemonCapability -> Set Protocol.DaemonCapability
filterCapabilitiesForTier = Auth.filterCapabilitiesForTier

-- Re-export environment setup
initDaemonEnv :: FilePath -> FilePath -> Maybe Text -> BuildEnv
initDaemonEnv = Core.initDaemonEnv
