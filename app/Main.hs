{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Ten (
    -- * Core Concepts & Monad
    module Ten.Core,

    -- * Building Derivations
    module Ten.Build,

    -- * Derivation Definition & Manipulation
    module Ten.Derivation,

    -- * Store Operations
    module Ten.Store,

    -- * Sandbox Configuration
    -- | (Primarily relevant for advanced customization or direct sandbox use)
    module Ten.Sandbox,

    -- * Hashing Utilities
    module Ten.Hash,

    -- * Garbage Collection (Daemon Operations)
    -- | (These functions typically require daemon interaction)
    module Ten.GC,

    -- * Client Operations (direct exports needed by app/Main.hs)
    connectToDaemon,
    disconnectFromDaemon,
    getDefaultSocketPath,
    isDaemonRunning,
    sendRequest,
    receiveResponse,
    sendRequestSync,
    startDaemonIfNeeded,

    -- High-level build operations
    buildFile,
    evalFile,
    buildDerivation,
    cancelBuild,
    getBuildStatus,
    getBuildOutput,
    listBuilds,

    -- Store operations and requests
    verifyStore,
    initializeStore,
    createStoreDirectories,
    ensureDBDirectories,
    requestGC,
    requestStoreAdd,
    requestStoreVerify,
    requestStorePath,
    requestStoreList,
    requestStatus,
    requestShutdown,
    requestPathInfo,
    PathInfo(..),
    DaemonResponse(..),

    -- Additional store operations
    addFileToStore,
    verifyStorePath,
    getStorePathForFile,
    readStoreContent,
    listStore,

    -- Additional derivation operations
    storeDerivation,
    retrieveDerivation,
    queryDerivationForOutput,
    queryOutputsForDerivation,
    listDerivations,
    getDerivationInfo,

    -- Additional GC operations
    collectGarbage,
    getGCStatus,
    addGCRoot,
    removeGCRoot,
    listGCRoots,

    -- Config-related
    getDaemonConfig,

    -- Authentication and Protocol Types
    UserCredentials(..),
    DaemonCapability(..),
    DaemonConnection,
    ProtocolVersion(..),

    -- * Re-exported Database types
    Database(..),
    DBError(..),
    TransactionMode(..),
    ensureSchema,
    initDatabaseDaemon,
    closeDatabaseDaemon
) where

-- Core modules
import Ten.Core
import Ten.Build
import Ten.Derivation
import Ten.Store
import Ten.Sandbox
import Ten.Hash
import Ten.GC

-- Database modules (with selective re-exports)
import Ten.DB.Core (
    Database(..),
    DBError(..),
    TransactionMode(..),
    ensureDBDirectories,
    initDatabaseDaemon,
    closeDatabaseDaemon
    )
import Ten.DB.Schema (ensureSchema)

-- Import client functionality with re-exports
import Ten.Daemon.Client (
    -- Connection management
    connectToDaemon,
    disconnectFromDaemon,
    getDefaultSocketPath,

    -- Client communication
    sendRequest,
    receiveResponse,
    sendRequestSync,

    -- Status checking
    isDaemonRunning,
    getDaemonStatus,

    -- Build operations
    buildFile,
    evalFile,
    buildDerivation,
    cancelBuild,
    getBuildStatus,
    getBuildOutput,
    listBuilds,

    -- Store operations
    addFileToStore,
    verifyStorePath,
    getStorePathForFile,
    readStoreContent,
    listStore,

    -- Derivation operations
    storeDerivation,
    retrieveDerivation,
    queryDerivationForOutput,
    queryOutputsForDerivation,
    listDerivations,
    getDerivationInfo,

    -- GC operations
    collectGarbage,
    getGCStatus,
    addGCRoot,
    removeGCRoot,
    listGCRoots,

    -- Daemon management
    startDaemonIfNeeded,
    shutdownDaemon,
    getDaemonConfig
    )

-- Import store operations
import Ten.Store (
    verifyStore,
    initializeStore,
    createStoreDirectories
    )

-- Import protocol and auth types
import Ten.Daemon.Protocol (
    DaemonResponse(..),
    UserCredentials(..),
    DaemonCapability(..),
    ProtocolVersion(..)
    )

-- | Request garbage collection from the daemon
requestGC :: DaemonConnection 'Builder -> Bool -> IO (Either BuildError GCStats)
requestGC = collectGarbage

-- | Request to add a file to the store
requestStoreAdd :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError StorePath)
requestStoreAdd = addFileToStore

-- | Request to verify a store path
requestStoreVerify :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError Bool)
requestStoreVerify = verifyStorePath

-- | Request to calculate a store path for a file
requestStorePath :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError StorePath)
requestStorePath = getStorePathForFile

-- | Request to list store contents
requestStoreList :: DaemonConnection 'Builder -> IO (Either BuildError [StorePath])
requestStoreList = listStore

-- | Request daemon status
requestStatus :: DaemonConnection 'Builder -> IO (Either BuildError DaemonStatus)
requestStatus = getDaemonStatus

-- | Request daemon shutdown
requestShutdown :: DaemonConnection 'Builder -> IO (Either BuildError ())
requestShutdown = shutdownDaemon

-- | Request information about a store path
requestPathInfo :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError PathInfo)
requestPathInfo conn path = do
    -- This is a stub implementation
    return $ Left $ DaemonError "Path info request not implemented"

-- | Path information type
data PathInfo = PathInfo {
    pathPath :: StorePath,
    pathHash :: Text,
    pathDeriver :: Maybe StorePath,
    pathRegistrationTime :: UTCTime,
    pathIsValid :: Bool
} deriving (Show, Eq)
