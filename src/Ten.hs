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

    -- * Garbage Collection (Daemon Operations)
    -- | (These functions typically require daemon interaction)
    module Ten.GC,

    -- * Daemon Client Operations
    -- | (Functions for interacting with a running Ten daemon)
    module Ten.Daemon.Protocol -- Re-export necessary protocol types (e.g., UserCredentials)

    , initializeStore
    , createStoreDirectories
    , verifyStore
    , ensureDBDirectories
    , isDaemonRunning

) where

import Ten.Store (initializeStore, createStoreDirectories, verifyStore)
import Ten.DB.Core (ensureDBDirectories)
import Ten.Daemon.Client (isDaemonRunning)
import Ten.Core (
    -- Core Types
    Phase(..), SPhase(..),
    PrivilegeTier(..), SPrivilegeTier(..),
    TenM(..),
    BuildEnv(..), BuildState(..), BuildError(..), BuildId(..),
    BuildStrategy(..), RunMode(..), DaemonConfig(..),
    UserId(..), AuthToken(..), DaemonConnection(..), Request(..), Response(..),

    -- Store & Derivation Types
    StorePath(..), storePathToText, textToStorePath, parseStorePath, validateStorePath, storePathToFilePath, filePathToStorePath, makeStorePath,
    Derivation(..), DerivationInput(..), DerivationOutput(..),
    BuildResult(..),

    -- Singletons & Permissions
    SingI(..), sing, sDaemon, sBuilder, sEval, sBuild,
    CanAccessStore, CanCreateSandbox, CanDropPrivileges, CanModifyStore, CanAccessDatabase, CanRunGC,

    -- Running Operations
    runTen, evalTen, buildTen,
    runTenDaemon, runTenDaemonEval, runTenDaemonBuild,
    runTenBuilderEval, runTenBuilderBuild,
    runTenIO, liftTenIO,

    -- Monad Operations
    logMsg, throwError, catchError,
    addProof, Proof(..),
    atomicallyTen,

    -- Environment/State Handling
    initBuildEnv, initClientEnv, initDaemonEnv, initBuildState,

    -- Utility Functions
    hashByteString, buildErrorToText,
    serializeDerivation, deserializeDerivation,
    currentProtocolVersion, ProtocolVersion(..),

    -- Daemon/Client Interaction Helpers
    withDaemonConnection, isDaemonMode, isClientMode
    )

import Ten.Build (
    -- Core Build Functions
    buildDerivation,
    buildApplicativeStrategy,
    buildMonadicStrategy,

    -- Build Result Handling
    verifyBuildResult,

    -- Build Graph Execution
    buildDerivationGraph,
    buildInDependencyOrder,

    -- Parallel Building
    buildDependenciesConcurrently,

    -- Build Status Reporting
    reportBuildProgress,
    reportBuildStatus,

    -- Type Classes for Privilege-Aware Operations
    CanBuildDerivation(..),
    CanBuildStrategy(..),
    CanManageBuildStatus(..)
    )

import Ten.Derivation (
    -- Derivation Creation & Inspection
    mkDerivation,
    instantiateDerivation,
    derivationEquals,
    derivationOutputPaths,
    derivationInputPaths,

    -- Return-Continuation Pattern
    joinDerivation,
    buildToGetInnerDerivation,
    isReturnContinuationDerivation,

    -- Hashing
    -- hashDerivation, -- This is better handled by Ten.Hash

    -- Type Classes
    CanStoreDerivation(..),
    CanStoreBuildDerivation(..),
    CanRetrieveDerivation(..)
    )

import Ten.Store (
    -- Store Path Operations
    -- (Types re-exported from Ten.Core)

    -- Content Operations (Type Class based)
    StoreAccessOps(..),
    StoreContentOps(..),
    StoreModificationOps(..), -- Daemon only
    StoreQueryOps(..),
    StoreScanOps(..),
    GCManagementOps(..),      -- Daemon only

    -- High-level Public Functions
    addToStore,
    storeFile,
    storeDirectory,
    readStoreContent,
    checkStorePathExists,
    verifyStoreIntegrity,
    listAllStorePaths,
    findPathsWithPrefix_, -- Note the underscore to avoid clash if needed
    scanFileForStoreReferences,
    getReferencesTo,
    getReferencesFrom
    )

import Ten.Sandbox (
    -- Configuration
    SandboxConfig(..),
    defaultSandboxConfig,

    -- Core function (mostly for internal use or advanced cases)
    withSandbox
    )

import Ten.Hash (
    -- Hash Type and Operations
    Hash,
    hashByteString,
    hashLazyByteString,
    hashText,
    hashFile,
    hashStorePath,

    -- Verification
    verifyHash,

    -- Conversion
    showHash,
    hashFromText,

    -- Higher-level Hashing
    hashDerivation,
    hashEnvironment,

    -- Utilities
    hashFilePath,
    isValidHash
    )

import Ten.GC (
    -- Core GC Operations (Daemon Only, via Client)
    -- collectGarbage, -- Prefer client function below

    -- GC Roots Management (Daemon Only, via Client)
    -- addRoot, -- Prefer client function below
    -- removeRoot, -- Prefer client function below
    -- listRoots, -- Prefer client function below

    -- Client-accessible GC operations are re-exported below from Ten.Daemon.Client
    GCStats(..) -- Re-export the stats type
    )

import Ten.Daemon.Client (
    -- Connection Management
    connectToDaemon,
    disconnectFromDaemon,
    getDefaultSocketPath,

    -- Client Communication Primitives (Advanced)
    -- sendRequest, receiveResponse, sendRequestSync, -- Usually use higher-level functions

    -- Status Checking
    isDaemonRunning,
    getDaemonStatus,

    -- High-level Build Operations
    buildFile,
    evalFile,
    buildDerivation, -- Note: Re-exports client version
    cancelBuild,
    getBuildStatus,
    getBuildOutput,
    listBuilds,

    -- Store Operations (Client Interface)
    addFileToStore,
    verifyStorePath,
    getStorePathForFile,
    -- readStoreContent, -- Re-exported from Ten.Store as typeclass method
    listStore,

    -- Derivation Operations (Client Interface)
    storeDerivation, -- Note: Re-exports client version
    retrieveDerivation,
    queryDerivationForOutput,
    queryOutputsForDerivation,
    listDerivations,
    getDerivationInfo,

    -- GC Operations (Client Interface)
    collectGarbage,
    getGCStatus,
    addGCRoot,
    removeGCRoot,
    listGCRoots,

    -- Daemon Management (Client Interface)
    startDaemonIfNeeded,
    shutdownDaemon,
    getDaemonConfig
    )

import Ten.Daemon.Protocol (
    -- Authentication Types
    UserCredentials(..),

    -- Capability System
    DaemonCapability(..),
    PrivilegeRequirement(..),
    PrivilegeError(..)
    )
