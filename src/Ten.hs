{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-- | Ten: A content-addressed build system and store manager
--
-- This module serves as the main entry point for the Ten library,
-- re-exporting the core functionality while providing a simplified API.
module Ten
  ( -- * Core types
    module Ten.Core

    -- * Database operations
  , module Ten.DB.Core

    -- * Client functionality
  , connectToTenDaemon
  , disconnectFromDaemon
  , runClientCommand

    -- * Version information
  , version
  , versionString
  ) where

-- Import core modules
import Ten.Core
import Ten.DB.Core

-- Import daemon modules with qualifiers to avoid name clashes
import qualified Ten.Daemon.Auth as Auth
import qualified Ten.Daemon.Client as Client
import qualified Ten.Daemon.Protocol as Protocol
import qualified Ten.Daemon.Server as Server
import qualified Ten.Daemon.State as State

-- | Version information based on protocol version
version :: (Int, Int, Int)
version = case Ten.Core.currentProtocolVersion of
    Ten.Core.ProtocolVersion major minor patch -> (major, minor, patch)

-- | Version string for display purposes
versionString :: String
versionString = case Ten.Core.currentProtocolVersion of
    Ten.Core.ProtocolVersion major minor patch ->
        "Ten " ++ show major ++ "." ++ show minor ++ "." ++ show patch

-- | Connect to a Ten daemon
connectToTenDaemon :: FilePath -> Protocol.UserCredentials -> IO (Either BuildError (DaemonConnection 'Builder))
connectToTenDaemon = Client.connectToDaemon

-- | Disconnect from a Ten daemon
disconnectFromDaemon :: DaemonConnection 'Builder -> IO ()
disconnectFromDaemon = Client.disconnectFromDaemon

-- | Run a client command against a daemon
runClientCommand :: DaemonConnection 'Builder
                 -> (DaemonConnection 'Builder -> IO (Either BuildError a))
                 -> IO (Either BuildError a)
runClientCommand conn cmd = cmd conn

-- Reexport ServerControl type
type ServerControl = Server.ServerControl
