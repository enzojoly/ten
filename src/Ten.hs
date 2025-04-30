{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Ten
    ( -- * Core types
      PrivilegeTier(..)
    , Phase(..)
    , TenM
    , BuildEnv
    , BuildError(..)
    , BuildId
    , StorePath(..)
    , Derivation(..)
    , BuildResult(..)
    , BuildStatus(..)
    , BuildStrategy(..)

    -- * Store operations
    , addToStore
    , storeFile
    , storeDirectory
    , checkStorePathExists
    , readStoreContent
    , scanFileForStoreReferences

    -- * Derivation handling
    , mkDerivation
    , buildDerivation
    , instantiateDerivation
    , hashDerivation

    -- * Build operations
    , buildDerivationGraph
    , buildInDependencyOrder
    , withSandbox

    -- * Sandbox configuration
    , SandboxConfig(..)
    , defaultSandboxConfig

    -- * Graph operations
    , createBuildGraph
    , topologicalSort
    , detectCycles

    -- * Garbage collection
    , collectGarbage
    , registerGCRoot
    , isGCRoot
    , findGCRoots

    -- * Database types
    , Database
    , TransactionMode(..)

    -- * Database operations
    , initializeDatabase
    , ensureDBDirectories

    -- * Derivation DB operations
    , storeDerivationInDB
    , readDerivationFromStore

    -- * Reference management
    , registerReference
    , getReferences
    , getReferrers

    -- * Utilities
    , initBuildEnv
    , initClientEnv
    , initDaemonEnv
    , runTen
    , buildTen
    , evalTen
    , logMsg
    , version

    -- * Daemon connection management
    , connectToDaemon
    , disconnectFromDaemon
    , isDaemonRunning
    , getDefaultSocketPath

    -- * Authentication
    , UserCredentials(..)
    , createAuthToken
    , validateToken

    -- * Client-daemon communication
    , sendRequest
    , receiveResponse
    , sendRequestSync

    -- * Daemon management
    , startDaemonIfNeeded
    , getDaemonStatus
    , shutdownDaemon
    , getDaemonConfig

    -- * Daemon state
    , DaemonState(..)
    , initDaemonState
    , saveStateToFile
    , loadStateFromFile

    -- * Daemon capabilities
    , DaemonCapability(..)
    , requestCapabilities
    , verifyCapabilities
    , capabilityRequiresDaemon
    , filterCapabilitiesForTier
    ) where

import Data.Int (Int64)
import qualified Ten.Core as Core
import qualified Ten.Build as Build
import qualified Ten.Derivation as Derivation
import qualified Ten.Store as Store
import qualified Ten.Sandbox as Sandbox
import qualified Ten.Graph as Graph
import qualified Ten.GC as GC
import qualified Ten.Hash as Hash
import qualified Ten.DB.Core as DB
import qualified Ten.DB.Derivations as DBDeriv
import qualified Ten.DB.References as DBRef

-- Re-export daemon modules for client functionality
import qualified Ten.Daemon.Client as Client
import qualified Ten.Daemon.Protocol as Protocol
import qualified Ten.Daemon.Auth as Auth
import qualified Ten.Daemon.State as State

-- Re-export core types
type TenM = Core.TenM
type BuildEnv = Core.BuildEnv
type BuildId = Core.BuildId
type StorePath = Core.StorePath
type Derivation = Core.Derivation
type BuildResult = Core.BuildResult

-- Export core type constructors
data Phase = Eval | Build
    deriving (Show, Eq)

data PrivilegeTier = Daemon | Builder
    deriving (Show, Eq)

data BuildError
    = EvalError Core.Text
    | BuildFailed Core.Text
    | StoreError Core.Text
    | SandboxError Core.Text
    | InputNotFound FilePath
    | HashError Core.Text
    | GraphError Core.Text
    | ResourceError Core.Text
    | DaemonError Core.Text
    | AuthError Core.Text
    | CyclicDependency Core.Text
    | SerializationError Core.Text
    | RecursionLimit Core.Text
    | NetworkError Core.Text
    | ParseError Core.Text
    | DBError Core.Text
    | GCError Core.Text
    | PhaseError Core.Text
    | PrivilegeError Core.Text
    | ProtocolError Core.Text
    | InternalError Core.Text
    | ConfigError Core.Text
    deriving (Show, Eq)

data BuildStatus
    = BuildPending
    | BuildRunning Float                 -- Progress percentage (0.0-1.0)
    | BuildRecursing BuildId             -- Switched to a new derivation
    | BuildCompleted
    | BuildFailed'
    deriving (Show, Eq)

data BuildStrategy
    = ApplicativeStrategy                -- Static dependencies, can parallelize
    | MonadicStrategy                    -- Dynamic dependencies or Return-Continuation
    deriving (Show, Eq)

-- Re-export Database types
type Database = DB.Database
type TransactionMode = DB.TransactionMode

-- Re-export SandboxConfig
type SandboxConfig = Sandbox.SandboxConfig

-- Re-export daemon capability types
type DaemonCapability = Protocol.DaemonCapability

-- Re-export UserCredentials for auth
type UserCredentials = Auth.UserCredentials

-- Re-export DaemonState
type DaemonState = State.DaemonState

-- Re-export core functions
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = Sandbox.defaultSandboxConfig

initBuildEnv :: FilePath -> FilePath -> BuildEnv
initBuildEnv = Core.initBuildEnv

initClientEnv :: FilePath -> FilePath -> Core.DaemonConnection 'Builder -> BuildEnv
initClientEnv = Core.initClientEnv

initDaemonEnv :: FilePath -> FilePath -> Maybe Core.Text -> BuildEnv
initDaemonEnv = Core.initDaemonEnv

runTen :: Core.SPhase p -> Core.SPrivilegeTier t -> TenM p t a -> BuildEnv -> Core.BuildState p -> IO (Either BuildError (a, Core.BuildState p))
runTen sp st tm env state = do
    result <- Core.runTen sp st tm env state
    case result of
        Left err -> return $ Left $ translateError err
        Right r -> return $ Right r

evalTen :: TenM 'Core.Eval 'Core.Daemon a -> BuildEnv -> IO (Either BuildError (a, Core.BuildState 'Core.Eval))
evalTen tm env = do
    result <- Core.evalTen tm env
    case result of
        Left err -> return $ Left $ translateError err
        Right r -> return $ Right r

buildTen :: TenM 'Core.Build 'Core.Builder a -> BuildEnv -> IO (Either BuildError (a, Core.BuildState 'Core.Build))
buildTen tm env = do
    result <- Core.buildTen tm env
    case result of
        Left err -> return $ Left $ translateError err
        Right r -> return $ Right r

-- Export store operations
addToStore :: Core.StoreContentOps t => Core.Text -> Core.ByteString -> TenM p t StorePath
addToStore = Store.addToStore

storeFile :: Core.StoreContentOps t => FilePath -> TenM p t StorePath
storeFile = Store.storeFile

storeDirectory :: Core.StoreContentOps t => FilePath -> TenM p t StorePath
storeDirectory = Store.storeDirectory

checkStorePathExists :: Core.StoreAccessOps t => StorePath -> TenM p t Bool
checkStorePathExists = Store.checkStorePathExists

readStoreContent :: Core.StoreAccessOps t => StorePath -> TenM p t Core.ByteString
readStoreContent = Store.readStoreContent

scanFileForStoreReferences :: Core.StoreScanOps t => FilePath -> TenM p t (Core.Set StorePath)
scanFileForStoreReferences = Store.scanFileForStoreReferences

-- Export derivation operations
mkDerivation :: Core.Text -> StorePath -> [Core.Text] -> Core.Set Core.DerivationInput
             -> Core.Set Core.Text -> Core.Map Core.Text Core.Text -> Core.Text -> TenM 'Core.Eval t Derivation
mkDerivation = Derivation.mkDerivation

buildDerivation :: Build.CanBuildDerivation t => Derivation -> TenM 'Core.Build t BuildResult
buildDerivation = Build.buildDerivation

instantiateDerivation :: Build.CanStoreBuildDerivation t => Derivation -> TenM 'Core.Build t ()
instantiateDerivation = Derivation.instantiateDerivation

hashDerivation :: Derivation -> Core.Text
hashDerivation = Derivation.hashDerivation

-- Export build operations
buildDerivationGraph :: Build.CanBuildDerivation t => Core.BuildGraph -> TenM 'Core.Build t (Core.Map Core.Text BuildResult)
buildDerivationGraph = Build.buildDerivationGraph

buildInDependencyOrder :: Build.CanBuildDerivation t => [Derivation] -> TenM 'Core.Build t [BuildResult]
buildInDependencyOrder = Build.buildInDependencyOrder

withSandbox :: Sandbox.SandboxCreator t => Core.Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Core.Build t a) -> TenM 'Core.Build t a
withSandbox = Sandbox.withSandbox

-- Export graph operations
createBuildGraph :: Core.SingI t => Core.SPrivilegeTier t -> Core.Set StorePath -> Core.Set Derivation -> TenM 'Core.Eval t Core.BuildGraph
createBuildGraph = Graph.createBuildGraph

topologicalSort :: Core.SingI t => Core.SPrivilegeTier t -> Core.BuildGraph -> TenM 'Core.Eval t [Core.BuildNode]
topologicalSort = Graph.topologicalSort

detectCycles :: Core.SingI t => Core.SPrivilegeTier t -> Core.BuildGraph -> TenM 'Core.Eval t (Bool, Core.Set Core.Text, Core.Set Core.Text)
detectCycles = Graph.detectCycles

-- Export garbage collection operations
collectGarbage :: TenM 'Core.Build 'Core.Daemon (Int, Int, Integer)
collectGarbage = GC.collectGarbage

registerGCRoot :: StorePath -> Core.Text -> Bool -> TenM 'Core.Build 'Core.Daemon Core.GCRoot
registerGCRoot = GC.addRoot

isGCRoot :: StorePath -> TenM 'Core.Build 'Core.Daemon Bool
isGCRoot = GC.isGCRoot

findGCRoots :: TenM 'Core.Build 'Core.Daemon [Core.GCRoot]
findGCRoots = GC.listRoots

-- DB Core operations
initializeDatabase :: FilePath -> Int -> IO FilePath
initializeDatabase = DB.initializeDatabase

ensureDBDirectories :: FilePath -> TenM p 'Core.Daemon ()
ensureDBDirectories = DB.ensureDBDirectories

-- DB Derivation operations
storeDerivationInDB :: DBDeriv.CanStoreDerivation t => Derivation -> StorePath -> TenM p t Int64
storeDerivationInDB = DBDeriv.storeDerivationInDB

readDerivationFromStore :: DBDeriv.CanRetrieveDerivation p t => StorePath -> TenM p t (Maybe Derivation)
readDerivationFromStore = DBDeriv.readDerivationFromStore

-- DB Reference operations
registerReference :: DBRef.CanRegisterReferences t => Database t -> StorePath -> StorePath -> TenM p t ()
registerReference = DBRef.registerReference

getReferences :: DBRef.CanQueryReferences t => Database t -> StorePath -> TenM p t (Core.Set StorePath)
getReferences = DBRef.getReferences

getReferrers :: DBRef.CanQueryReferences t => Database t -> StorePath -> TenM p t (Core.Set StorePath)
getReferrers = DBRef.getReferrers

-- Utility functions
logMsg :: Int -> Core.Text -> TenM p t ()
logMsg = Core.logMsg

-- | Version information for the Ten build system
version :: String
version = "0.1.0.0"

-- Helper function to translate Core.BuildError to public BuildError
translateError :: Core.BuildError -> BuildError
translateError (Core.EvalError msg) = EvalError msg
translateError (Core.BuildFailed msg) = BuildFailed msg
translateError (Core.StoreError msg) = StoreError msg
translateError (Core.SandboxError msg) = SandboxError msg
translateError (Core.InputNotFound path) = InputNotFound path
translateError (Core.HashError msg) = HashError msg
translateError (Core.GraphError msg) = GraphError msg
translateError (Core.ResourceError msg) = ResourceError msg
translateError (Core.DaemonError msg) = DaemonError msg
translateError (Core.AuthError msg) = AuthError msg
translateError (Core.CyclicDependency msg) = CyclicDependency msg
translateError (Core.SerializationError msg) = SerializationError msg
translateError (Core.RecursionLimit msg) = RecursionLimit msg
translateError (Core.NetworkError msg) = NetworkError msg
translateError (Core.ParseError msg) = ParseError msg
translateError (Core.DBError msg) = DBError msg
translateError (Core.GCError msg) = GCError msg
translateError (Core.PhaseError msg) = PhaseError msg
translateError (Core.PrivilegeError msg) = PrivilegeError msg
translateError (Core.ProtocolError msg) = ProtocolError msg
translateError (Core.InternalError msg) = InternalError msg
translateError (Core.ConfigError msg) = ConfigError msg

-- Re-export daemon client functions
connectToDaemon :: FilePath -> UserCredentials -> IO (Either BuildError (Core.DaemonConnection 'Core.Builder))
connectToDaemon path creds = do
    result <- Client.connectToDaemon path creds
    case result of
        Left err -> return $ Left $ translateError err
        Right conn -> return $ Right conn

disconnectFromDaemon :: Core.DaemonConnection 'Core.Builder -> IO ()
disconnectFromDaemon = Client.disconnectFromDaemon

isDaemonRunning :: FilePath -> IO Bool
isDaemonRunning = Client.isDaemonRunning

getDefaultSocketPath :: IO FilePath
getDefaultSocketPath = Client.getDefaultSocketPath

-- Re-export request/response functions
sendRequest :: Core.DaemonConnection 'Core.Builder -> Core.Request -> IO (Either BuildError Int)
sendRequest conn req = do
    result <- Client.sendRequest conn req
    case result of
        Left err -> return $ Left $ translateError err
        Right reqId -> return $ Right reqId

receiveResponse :: Core.DaemonConnection 'Core.Builder -> Int -> Int -> IO (Either BuildError Core.DaemonResponse)
receiveResponse conn reqId timeout = do
    result <- Client.receiveResponse conn reqId timeout
    case result of
        Left err -> return $ Left $ translateError err
        Right resp -> return $ Right resp

sendRequestSync :: Core.DaemonConnection 'Core.Builder -> Core.Request -> Int -> IO (Either BuildError Core.DaemonResponse)
sendRequestSync conn req timeout = do
    result <- Client.sendRequestSync conn req timeout
    case result of
        Left err -> return $ Left $ translateError err
        Right resp -> return $ Right resp

-- Re-export daemon management functions
startDaemonIfNeeded :: FilePath -> IO (Either BuildError ())
startDaemonIfNeeded path = do
    result <- Client.startDaemonIfNeeded path
    case result of
        Left err -> return $ Left $ translateError err
        Right () -> return $ Right ()

getDaemonStatus :: Core.DaemonConnection 'Core.Builder -> IO (Either BuildError Core.DaemonStatus)
getDaemonStatus conn = do
    result <- Client.getDaemonStatus conn
    case result of
        Left err -> return $ Left $ translateError err
        Right status -> return $ Right status

shutdownDaemon :: Core.DaemonConnection 'Core.Builder -> IO (Either BuildError ())
shutdownDaemon conn = do
    result <- Client.shutdownDaemon conn
    case result of
        Left err -> return $ Left $ translateError err
        Right () -> return $ Right ()

getDaemonConfig :: Core.DaemonConnection 'Core.Builder -> IO (Either BuildError Core.DaemonConfig)
getDaemonConfig conn = do
    result <- Client.getDaemonConfig conn
    case result of
        Left err -> return $ Left $ translateError err
        Right config -> return $ Right config

-- Re-export authentication functions
createAuthToken :: Core.Text -> IO Core.AuthToken
createAuthToken = Auth.createAuthToken

validateToken :: Core.DaemonConnection 'Core.Builder -> IO Bool
validateToken conn = do
    -- Simplified placeholder - actual implementation would
    -- validate token with the daemon using Auth module
    return True

-- Re-export daemon capability functions
requestCapabilities :: Protocol.DaemonRequest -> Core.Set Protocol.DaemonCapability
requestCapabilities = Protocol.requestCapabilities

verifyCapabilities :: Core.SPrivilegeTier t -> Core.Set Protocol.DaemonCapability -> Either Auth.PrivilegeError ()
verifyCapabilities = Protocol.verifyCapabilities

capabilityRequiresDaemon :: Protocol.DaemonCapability -> Bool
capabilityRequiresDaemon = Protocol.capabilityRequiresDaemon

filterCapabilitiesForTier :: Core.SPrivilegeTier t -> Core.Set Protocol.DaemonCapability -> Core.Set Protocol.DaemonCapability
filterCapabilitiesForTier = Auth.filterCapabilitiesForTier

-- Re-export daemon state functions
initDaemonState :: Core.SPrivilegeTier t -> Int -> IO (State.DaemonState t)
initDaemonState st maxJobs = do
    -- Create default state file path
    stateFile <- Client.getDefaultSocketPath
    let stateFilePath = stateFile ++ ".state"
    State.initDaemonState st stateFilePath maxJobs 100

saveStateToFile :: (Core.CanAccessStore t ~ 'True, Core.CanModifyStore t ~ 'True) => State.DaemonState t -> IO ()
saveStateToFile = State.saveStateToFile

loadStateFromFile :: Core.SPrivilegeTier t -> FilePath -> Int -> Int -> IO (State.DaemonState t)
loadStateFromFile = State.loadStateFromFile
