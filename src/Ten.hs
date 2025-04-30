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
    Ten.Build.buildDerivation,
    cancelBuild,
    getBuildStatus,
    getBuildOutput,
    listBuilds,

    -- Store operations and requests
    verifyStore,
    initializeStore,
    createStoreDirectories,
    ensureDBDirectories,
    requestStoreAdd,
    requestStoreVerify,
    requestStorePath,
    requestStoreList,
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

    -- * Re-exported Database types
    Database(..),
    DBError(..),
    TransactionMode(..),
    ensureSchema,
    initDatabaseDaemon,
    closeDatabaseDaemon
) where

-- Import core data types
import Data.Text (Text)
import Data.Int (Int64)

-- Core modules with explicit imports
import Ten.Core (
    BuildError(..), GCStats(..), BuildId(..),
    StorePath(..), DaemonConnection(..),
    Request(..), DaemonResponse(..),
    PrivilegeTier(..), storePathToText
    )

-- Build module
import Ten.Build (
    buildDerivation
    )

-- Derivation module
import Ten.Derivation (
    derivationEquals, derivationOutputPaths
    )

-- Store module - specific operations
import Ten.Store (
    verifyStore, initializeStore, createStoreDirectories
    )

-- Sandbox module - limited exports
import Ten.Sandbox (
    SandboxConfig(..), defaultSandboxConfig
    )

-- Hash module
import Ten.Hash (
    hashByteString, hashStorePath
    )

-- GC module
import Ten.GC (
    GCStats(..),
    )

-- Database modules
import Ten.DB.Core (
    Database(..),
    DBError(..),
    TransactionMode(..),
    ensureDBDirectories,
    initDatabaseDaemon,
    closeDatabaseDaemon
    )
import Ten.DB.Schema (ensureSchema)

-- Import client functionality with specific imports
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

-- Import protocol and auth types
import Ten.Daemon.Protocol (
    UserCredentials(..),
    DaemonCapability(..),
    )

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
    pathRegistrationTime :: Int64,  -- Unix timestamp instead of UTCTime
    pathIsValid :: Bool
} deriving (Show, Eq)
