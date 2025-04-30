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
    module Ten.Sandbox,

    -- * Hashing Utilities
    module Ten.Hash,

    -- * Garbage Collection (Daemon Operations)
    module Ten.GC,

    -- * Database Operations
    module Ten.DB.Core,
    module Ten.DB.Schema,
    module Ten.DB.Derivations,
    module Ten.DB.References,

    -- * Daemon Client Operations
    module Ten.Daemon.Client,

    -- * Daemon Server Operations (for daemon executable)
    module Ten.Daemon.Server,

    -- * Daemon State Management (for daemon executable)
    module Ten.Daemon.State,

    -- * Daemon Authentication (for daemon executable)
    module Ten.Daemon.Auth,

    -- * Daemon Protocol Types
    module Ten.Daemon.Protocol,

    -- * Re-exported types/functions for convenience
    -- Client Operations (needed by app/Main.hs)
    connectToDaemon,
    disconnectFromDaemon,
    getDefaultSocketPath,
    isDaemonRunning,
    sendRequestSync,
    startDaemonIfNeeded,
    buildFile,
    evalFile,
    Ten.Daemon.Client.buildDerivation, -- Qualify to avoid ambiguity with Ten.Build
    cancelBuild,
    getBuildStatus,
    getBuildOutput,
    listBuilds,
    verifyStorePath,
    getStorePathForFile,
    readStoreContent,
    listStore,
    storeDerivation,
    retrieveDerivation,
    queryDerivationForOutput,
    queryOutputsForDerivation,
    listDerivations,
    getDerivationInfo,
    collectGarbage,
    getGCStatus,
    addGCRoot,
    removeGCRoot,
    listGCRoots,
    shutdownDaemon,
    getDaemonStatus,
    getDaemonConfig,
    UserCredentials(..),
    DaemonConnection,

    -- Daemon Operations (needed by app/TenDaemon.hs)
    DaemonConfig(..), -- Re-export from Core if needed
    initDatabaseDaemon,
    closeDatabaseDaemon,
    ensureDBDirectories,
    initializeStore,
    verifyStore,
    Ten.Daemon.Server.startServer,
    Ten.Daemon.Server.createServerSocket,
    Ten.Daemon.State.initDaemonState,
    Ten.Daemon.State.loadStateFromFile,
    Ten.Daemon.State.saveStateToFile,
    Ten.Daemon.Auth.loadAuthFile,
    Ten.Daemon.Auth.saveAuthFile,
    SPrivilegeTier, sDaemon -- Re-export from Core

) where

-- Import core data types and functions
import Ten.Core

-- Build module
import Ten.Build

-- Derivation module
import Ten.Derivation

-- Store module
import Ten.Store

-- Sandbox module
import Ten.Sandbox

-- Hash module
import Ten.Hash

-- GC module
import Ten.GC

-- Database modules
import Ten.DB.Core
import Ten.DB.Schema
import Ten.DB.Derivations
import Ten.DB.References

-- Daemon Client module
import Ten.Daemon.Client

-- Daemon Server module
import Ten.Daemon.Server

-- Daemon State module
import Ten.Daemon.State

-- Daemon Auth module
import Ten.Daemon.Auth

-- Daemon Protocol module
import Ten.Daemon.Protocol

-- NOTE: Redundant aliases like requestStoreAdd are removed.
--       Clients should use the functions directly exported from Client.
-- NOTE: PathInfo definition removed, assuming it lives in Core or Store now.
--       If needed, it should be defined there and re-exported.
