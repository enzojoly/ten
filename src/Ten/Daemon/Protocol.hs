{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ten.Daemon.Protocol (
    -- Protocol versions
    compatibleVersions,

    -- Daemon capability system
    DaemonCapability(..),
    requestCapabilities,
    verifyCapabilities,
    checkPrivilegeRequirement,

    -- Request/response privilege types
    RequestPrivilege(..),
    ResponsePrivilege(..),
    PrivilegeRequirement(..),
    PrivilegeError(..),

    -- Authentication types
    UserCredentials(..),
    AuthRequestContent(..),

    -- Build tracking
    BuildRequestInfo(..),
    BuildStatusUpdate(..),
    defaultBuildRequestInfo,

    -- Derivation operations
    DerivationInfo(..),
    DerivationInfoResponse(..),
    DerivationOutputMappingResponse(..),

    -- Daemon status and configuration
    DaemonStatus(..),

    -- GC status
    GCRequestParams(..),
    GCStatusRequestParams(..),
    GCStatusResponse(..),

    -- Privilege transition
    PrivilegeTransition(..),
    dropPrivilege,
    SomePrivilegeTier(..),
    withSomePrivilegeTier,

    -- Protocol message types
    DaemonRequest(..),
    DaemonResponse(..),

    -- Serialization
    serializeDaemonRequest,
    deserializeDaemonRequest,
    serializeDaemonResponse,
    deserializeDaemonResponse,

    -- Protocol framing
    createRequestFrame,
    parseRequestFrame,
    createResponseFrame,
    parseResponseFrame,

    -- Socket communication
    sendDaemonRequest,
    receiveDaemonResponse,
    sendDaemonResponse,
    receiveDaemonRequest,

    -- Protocol handle operations
    ProtocolHandle(..),
    createProtocolHandle,
    closeProtocolHandle,
    withProtocolHandle,

    -- Utilities
    requestToText,
    responseToText,

    -- Error handling
    ProtocolError(..),
    errorToText,

    -- Response conversion helpers
    responseToResponseData,
    responsePrivilegeRequirement,

    -- Utilities for working with paths
    parseBuildId,
    renderBuildId
) where

import Control.Concurrent (forkIO, killThread, threadDelay, myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO, bracket, try, SomeException, IOException)
import Control.Monad (unless, when, foldM, forM)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Aeson.Types ((.!=), (.:?))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing, isJust, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Word (Word8, Word32, Word64)
import GHC.Generics (Generic)
import Network.Socket (Socket, close)
import qualified Network.Socket.ByteString as NByte
import System.Exit (ExitCode(ExitSuccess, ExitFailure))
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush)
import System.IO.Error (isEOFError)
import Text.Read (readMaybe)
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.TH
import Data.Proxy (Proxy(..))
import Data.Binary (Binary(..), Get, put, get, encode, decode)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Unique (Unique, hashUnique)

-- Import Ten.Core types and functions
import Ten.Core (
    -- Core types
    Phase(..), SPhase(..), PrivilegeTier(..), SPrivilegeTier(..), TenM(..),
    BuildId(..), BuildStatus(..), BuildError(..), StorePath(..), storePathToText,
    UserId(..), AuthToken(..), StoreReference(..), ReferenceType(..),
    Derivation, BuildResult(..), ProtocolVersion(..), currentProtocolVersion,
    Request(..), Response(..), Message(..), AuthResult(..),
    GCStats(..), GCRoot,
    DaemonConfig(..),

    -- Type families for permissions
    CanAccessStore, CanCreateSandbox, CanDropPrivileges, CanModifyStore,
    CanAccessDatabase, CanRunGC,

    -- Singletons
    fromSing, sing,

    -- Error handling
    buildErrorToText,

    -- Helper functions
    hashByteString)

-- | List of compatible protocol versions
compatibleVersions :: [ProtocolVersion]
compatibleVersions = [
    ProtocolVersion 1 0 0
    ]

-- | Existential wrapper for privilege tier singletons
data SomePrivilegeTier where
    SomePrivilegeTier :: SPrivilegeTier t -> SomePrivilegeTier

-- | Pattern match on SomePrivilegeTier to use the contained singleton
withSomePrivilegeTier :: SomePrivilegeTier -> (forall t. SPrivilegeTier t -> r) -> r
withSomePrivilegeTier (SomePrivilegeTier s) f = f s

-- | Daemon capabilities - mapped to privilege tiers
data DaemonCapability =
    StoreAccess             -- requires CanAccessStore t ~ 'True
  | SandboxCreation         -- requires CanCreateSandbox t ~ 'True
  | GarbageCollection       -- requires CanRunGC t ~ 'True
  | DerivationRegistration  -- requires CanAccessStore t ~ 'True
  | DerivationBuild         -- available to all
  | StoreQuery              -- available to all
  | BuildQuery              -- available to all
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Privilege transition type
data PrivilegeTransition (p :: PrivilegeTier) (q :: PrivilegeTier) where
    DropPrivilege :: PrivilegeTransition 'Daemon 'Builder
    -- No constructor for gaining privilege - can only drop

-- | Perform a privilege transition (can only drop privileges)
dropPrivilege :: SPrivilegeTier 'Daemon -> SPrivilegeTier 'Builder
dropPrivilege SDaemon = SBuilder

-- | Protocol errors
data ProtocolError
    = ProtocolParseError Text
    | VersionMismatch ProtocolVersion ProtocolVersion
    | MessageTooLarge Word32
    | ConnectionClosed
    | AuthenticationFailed Text
    | OperationFailed Text
    | InvalidRequest Text
    | InternalError Text
    | PrivilegeViolation Text
    deriving (Show, Eq)

instance Exception ProtocolError

-- | Privilege errors
data PrivilegeError
    = InsufficientPrivileges Text
    | PrivilegeDowngradeError Text
    | InvalidCapability Text
    | AuthorizationError Text
    deriving (Show, Eq)

instance Exception PrivilegeError

-- | Privilege requirement for operations
data PrivilegeRequirement =
    DaemonRequired    -- Requires daemon privileges
  | BuilderSufficient -- Can be done from builder context
  deriving (Show, Eq)

-- | Request privilege tagging
data RequestPrivilege =
    PrivilegedRequest   -- Must be run in daemon context
  | UnprivilegedRequest -- Can be run in either context
  deriving (Show, Eq)

-- | Response privilege tagging
data ResponsePrivilege =
    PrivilegedResponse   -- Contains privileged information
  | UnprivilegedResponse -- Can be received in any context
  deriving (Show, Eq)

-- | User authentication credentials
data UserCredentials = UserCredentials {
    username :: Text,
    token :: Text,
    requestedTier :: PrivilegeTier  -- Which tier is being requested
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON UserCredentials where
    toJSON UserCredentials{..} = Aeson.object [
            "username" .= username,
            "token" .= token,
            "requestedTier" .= show requestedTier
        ]

instance Aeson.FromJSON UserCredentials where
    parseJSON = Aeson.withObject "UserCredentials" $ \v -> do
        username <- v .: "username"
        token <- v .: "token"
        tierStr <- v .:? "requestedTier" .!= "Builder" :: Aeson.Parser String
        let requestedTier = case tierStr of
                "Daemon" -> Daemon
                _ -> Builder  -- Default to less privileged
        return UserCredentials{..}

-- | Authentication request content
data AuthRequestContent = AuthRequestContent {
    authVersion :: ProtocolVersion,
    authUser :: Text,
    authToken :: Text,
    authRequestedTier :: PrivilegeTier  -- Which privilege tier is being requested
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON AuthRequestContent where
    toJSON AuthRequestContent{..} = Aeson.object [
            "version" .= authVersion,
            "user" .= authUser,
            "token" .= authToken,
            "requestedTier" .= show authRequestedTier
        ]

instance Aeson.FromJSON AuthRequestContent where
    parseJSON = Aeson.withObject "AuthRequestContent" $ \v -> do
        authVersion <- v .: "version"
        authUser <- v .: "user"
        authToken <- v .: "token"
        tierStr <- v .:? "requestedTier" .!= "Builder" :: Aeson.Parser String
        let authRequestedTier = case tierStr of
                "Daemon" -> Daemon
                _ -> Builder  -- Default to less privileged
        return AuthRequestContent{..}

-- | Full derivation information
data DerivationInfo = DerivationInfo {
    derivationId :: Int64,
    derivationHash :: Text,
    derivationStorePath :: StorePath,
    derivationTimestamp :: Int64
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DerivationInfo where
    toJSON DerivationInfo{..} = Aeson.object [
            "id" .= derivationId,
            "hash" .= derivationHash,
            "storePath" .= encodePath derivationStorePath,
            "timestamp" .= derivationTimestamp
        ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

instance Aeson.FromJSON DerivationInfo where
    parseJSON = Aeson.withObject "DerivationInfo" $ \v -> do
        derivationId <- v .: "id"
        derivationHash <- v .: "hash"
        storePathObj <- v .: "storePath"
        derivationStorePath <- decodePath storePathObj
        derivationTimestamp <- v .: "timestamp"
        return DerivationInfo{..}
      where
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

-- | Response with derivation information
data DerivationInfoResponse = DerivationInfoResponse {
    derivationResponseDrv :: Derivation,
    derivationResponseOutputs :: Set StorePath,
    derivationResponseInputs :: Set StorePath,
    derivationResponseStorePath :: StorePath,
    derivationResponseMetadata :: Map Text Text
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DerivationInfoResponse
instance Aeson.FromJSON DerivationInfoResponse

-- | Response with derivation-output mappings
data DerivationOutputMappingResponse = DerivationOutputMappingResponse {
    outputMappings :: [(StorePath, StorePath)],  -- [(derivation path, output path)]
    outputMappingCount :: Int,
    outputMappingComplete :: Bool   -- Whether all mappings were included
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DerivationOutputMappingResponse
instance Aeson.FromJSON DerivationOutputMappingResponse

-- | Build request information
data BuildRequestInfo = BuildRequestInfo {
    buildTimeout :: Maybe Int,    -- Build timeout in seconds
    buildEnv :: Map Text Text,    -- Extra environment variables
    buildFlags :: [Text]          -- Build flags (e.g., --keep-failed, --no-out-link)
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON BuildRequestInfo where
    toJSON BuildRequestInfo{..} = Aeson.object [
            "timeout" .= buildTimeout,
            "env" .= buildEnv,
            "flags" .= buildFlags
        ]

instance Aeson.FromJSON BuildRequestInfo where
    parseJSON = Aeson.withObject "BuildRequestInfo" $ \v -> do
        buildTimeout <- v .: "timeout"
        buildEnv <- v .: "env"
        buildFlags <- v .: "flags"
        return BuildRequestInfo{..}

-- | Default build request info
defaultBuildRequestInfo :: BuildRequestInfo
defaultBuildRequestInfo = BuildRequestInfo {
    buildTimeout = Nothing,
    buildEnv = Map.empty,
    buildFlags = []
}

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

-- | Render a BuildId to Text
renderBuildId :: BuildId -> Text
renderBuildId (BuildId u) = "build-" <> T.pack (show (hashUnique u))
renderBuildId (BuildIdFromInt n) = "build-" <> T.pack (show n)

-- | Build status update
data BuildStatusUpdate = BuildStatusUpdate {
    buildId :: BuildId,
    buildStatus :: BuildStatus,
    buildTimeElapsed :: Double,   -- Time elapsed in seconds
    buildTimeRemaining :: Maybe Double,  -- Estimated time remaining (if available)
    buildLogUpdate :: Maybe Text,  -- New log messages since last update
    buildResourceUsage :: Map Text Double  -- Resource usage metrics
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON BuildStatusUpdate
instance Aeson.FromJSON BuildStatusUpdate

-- | GC Request parameters
data GCRequestParams = GCRequestParams {
    gcForce :: Bool  -- Force GC (run even if unsafe)
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON GCRequestParams where
    toJSON GCRequestParams{..} = Aeson.object [
            "force" .= gcForce
        ]

instance Aeson.FromJSON GCRequestParams where
    parseJSON = Aeson.withObject "GCRequestParams" $ \v -> do
        gcForce <- v .: "force"
        return GCRequestParams{..}

-- | GC Status Request parameters
data GCStatusRequestParams = GCStatusRequestParams {
    gcForceCheck :: Bool  -- Whether to force recheck the lock file
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON GCStatusRequestParams where
    toJSON GCStatusRequestParams{..} = Aeson.object [
            "forceCheck" .= gcForceCheck
        ]

instance Aeson.FromJSON GCStatusRequestParams where
    parseJSON = Aeson.withObject "GCStatusRequestParams" $ \v -> do
        gcForceCheck <- v .: "forceCheck"
        return GCStatusRequestParams{..}

-- | GC Status Response content
data GCStatusInfo = GCStatusInfo {
    gcRunning :: Bool,           -- Whether GC is currently running
    gcOwner :: Maybe Text,       -- Process/username owning the GC lock (if running)
    gcLockTime :: Maybe UTCTime  -- When the GC lock was acquired (if running)
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON GCStatusResponse where
    toJSON GCStatusInfo{..} = Aeson.object [
            "running" .= gcRunning,
            "owner" .= gcOwner,
            "lockTime" .= gcLockTime
        ]

instance Aeson.FromJSON GCStatusResponse where
    parseJSON = Aeson.withObject "GCStatusResponse" $ \v -> do
        gcRunning <- v .: "running"
        gcOwner <- v .: "owner"
        gcLockTime <- v .: "lockTime"
        return GCStatusInfo{..}

-- | Daemon status information
data DaemonStatus = DaemonStatus {
    daemonStatus :: Text,           -- "running", "starting", etc.
    daemonUptime :: Double,         -- Uptime in seconds
    daemonActiveBuilds :: Int,      -- Number of active builds
    daemonCompletedBuilds :: Int,   -- Number of completed builds since startup
    daemonFailedBuilds :: Int,      -- Number of failed builds since startup
    daemonGcRoots :: Int,           -- Number of GC roots
    daemonStoreSize :: Integer,     -- Store size in bytes
    daemonStorePaths :: Int         -- Number of paths in store
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DaemonStatus where
    toJSON DaemonStatus{..} = Aeson.object [
            "status" .= daemonStatus,
            "uptime" .= daemonUptime,
            "activeBuilds" .= daemonActiveBuilds,
            "completedBuilds" .= daemonCompletedBuilds,
            "failedBuilds" .= daemonFailedBuilds,
            "gcRoots" .= daemonGcRoots,
            "storeSize" .= daemonStoreSize,
            "storePaths" .= daemonStorePaths
        ]

instance Aeson.FromJSON DaemonStatus where
    parseJSON = Aeson.withObject "DaemonStatus" $ \v -> do
        daemonStatus <- v .: "status"
        daemonUptime <- v .: "uptime"
        daemonActiveBuilds <- v .: "activeBuilds"
        daemonCompletedBuilds <- v .: "completedBuilds"
        daemonFailedBuilds <- v .: "failedBuilds"
        daemonGcRoots <- v .: "gcRoots"
        daemonStoreSize <- v .: "storeSize"
        daemonStorePaths <- v .: "storePaths"
        return DaemonStatus{..}

-- | Protocol handle type for managing connections
data ProtocolHandle = ProtocolHandle {
    protocolSocket :: Socket,
    protocolLock :: MVar (),  -- For thread safety
    protocolPrivilegeTier :: PrivilegeTier -- Track privilege level of connection
}

-- | Check if a request has the necessary capabilities
verifyCapabilities :: SPrivilegeTier t -> Set DaemonCapability -> Either PrivilegeError ()
verifyCapabilities st capabilities =
    case fromSing st of
        -- Daemon context can perform any operation
        Daemon -> Right ()

        -- Builder context has limited capabilities
        Builder ->
            if any restrictedCapability (Set.toList capabilities)
                then Left $ InsufficientPrivileges $
                    "Operation requires daemon privileges: " <>
                    T.intercalate ", " (map (T.pack . show) $
                                        filter restrictedCapability $
                                        Set.toList capabilities)
                else Right ()
  where
    restrictedCapability :: DaemonCapability -> Bool
    restrictedCapability StoreAccess = True
    restrictedCapability SandboxCreation = True
    restrictedCapability GarbageCollection = True
    restrictedCapability DerivationRegistration = True
    restrictedCapability _ = False

-- | Check if a request can be performed with given privilege tier
checkPrivilegeRequirement :: SPrivilegeTier t -> RequestPrivilege -> Either PrivilegeError ()
checkPrivilegeRequirement st reqPriv =
    case (fromSing st, reqPriv) of
        (Daemon, _) ->
            -- Daemon can perform any operation
            Right ()
        (Builder, PrivilegedRequest) ->
            -- Builder can't perform privileged operations
            Left $ InsufficientPrivileges
                "This operation requires daemon privileges"
        (Builder, UnprivilegedRequest) ->
            -- Builder can perform unprivileged operations
            Right ()

-- | Create a protocol handle from a socket
createProtocolHandle :: Socket -> PrivilegeTier -> IO ProtocolHandle
createProtocolHandle sock tier = do
    lock <- newMVar ()
    return $ ProtocolHandle sock lock tier

-- | Close a protocol handle
closeProtocolHandle :: ProtocolHandle -> IO ()
closeProtocolHandle handle = do
    close (protocolSocket handle)

-- | Create a framed request for sending over the wire
-- Format: [4-byte message length][message data]
createRequestFrame :: BS.ByteString -> BS.ByteString
createRequestFrame content = do
    let len = fromIntegral (BS.length content) :: Word32
    let lenBytes = LBS.toStrict $ Builder.toLazyByteString $
                   Builder.word32BE len
    lenBytes `BS.append` content

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

-- | Create a framed response for sending over the wire
createResponseFrame :: BS.ByteString -> BS.ByteString
createResponseFrame = createRequestFrame  -- Same format

-- | Parse a framed response from the wire
parseResponseFrame :: BS.ByteString -> Either Text (BS.ByteString, BS.ByteString)
parseResponseFrame = parseRequestFrame  -- Same format

-- | Convert BuildError to Text
errorToText :: BuildError -> Text
errorToText = buildErrorToText

-- | Daemon request types for protocol communication
data DaemonRequest
    = AuthRequest AuthRequestContent
    | BuildRequest Text (Maybe BS.ByteString) BuildRequestInfo
    | EvalRequest Text (Maybe BS.ByteString) BuildRequestInfo
    | BuildDerivationRequest Derivation BuildRequestInfo
    | BuildStatusRequest BuildId
    | CancelBuildRequest BuildId
    | QueryBuildOutputRequest BuildId
    | ListBuildsRequest (Maybe Int)
    | StoreAddRequest Text BS.ByteString
    | StoreVerifyRequest StorePath
    | StorePathRequest Text BS.ByteString
    | StoreListRequest
    | StoreDerivationRequest BS.ByteString
    | RetrieveDerivationRequest StorePath
    | QueryDerivationRequest Text Text (Maybe Int)
    | GetDerivationForOutputRequest Text
    | ListDerivationsRequest (Maybe Int)
    | GCRequest GCRequestParams
    | GCStatusRequest GCStatusRequestParams
    | AddGCRootRequest StorePath Text Bool
    | RemoveGCRootRequest Text
    | ListGCRootsRequest
    | PingRequest
    | ShutdownRequest
    | StatusRequest
    | ConfigRequest
    deriving (Show, Eq)

-- | Daemon response types for protocol communication
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

-- | Determine capabilities required for a request
requestCapabilities :: DaemonRequest -> Set DaemonCapability
requestCapabilities = \case
    AuthRequest _ -> Set.empty  -- Auth requests don't require capabilities
    GCRequest _ -> Set.singleton GarbageCollection
    StoreAddRequest _ _ -> Set.singleton StoreAccess
    StoreDerivationRequest _ -> Set.singleton DerivationRegistration
    AddGCRootRequest _ _ _ -> Set.singleton StoreAccess
    RemoveGCRootRequest _ -> Set.singleton StoreAccess
    BuildDerivationRequest _ _ -> Set.fromList [DerivationBuild, StoreAccess]
    -- Most operations are unprivileged
    _ -> Set.empty

-- | Get privilege requirement for a response
responsePrivilegeRequirement :: DaemonResponse -> ResponsePrivilege
responsePrivilegeRequirement = \case
    -- Privileged responses
    StoreAddResponse _ -> PrivilegedResponse
    GCResultResponse _ -> PrivilegedResponse
    DerivationStoredResponse _ -> PrivilegedResponse
    GCRootAddedResponse _ -> PrivilegedResponse
    GCRootRemovedResponse _ -> PrivilegedResponse
    -- Unprivileged responses
    _ -> UnprivilegedResponse

-- | Serialize a daemon request to ByteString
serializeDaemonRequest :: DaemonRequest -> (BS.ByteString, Maybe BS.ByteString)
serializeDaemonRequest req =
    case req of
        AuthRequest auth ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("auth" :: Text),
                "content" .= auth
            ], Nothing)

        BuildRequest path content info ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build" :: Text),
                "path" .= path,
                "hasContent" .= isJust content,
                "info" .= info
            ], content)

        EvalRequest path content info ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("eval" :: Text),
                "path" .= path,
                "hasContent" .= isJust content,
                "info" .= info
            ], content)

        BuildDerivationRequest deriv info ->
            let derivContent = Ten.Derivation.serializeDerivation deriv
            in (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-derivation" :: Text),
                "hasDerivation" .= True,
                "info" .= info
            ], Just derivContent)

        BuildStatusRequest bid ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-status" :: Text),
                "buildId" .= renderBuildId bid
            ], Nothing)

        CancelBuildRequest bid ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("cancel-build" :: Text),
                "buildId" .= renderBuildId bid
            ], Nothing)

        QueryBuildOutputRequest bid ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-output" :: Text),
                "buildId" .= renderBuildId bid
            ], Nothing)

        ListBuildsRequest limit ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("list-builds" :: Text),
                "limit" .= limit
            ], Nothing)

        StoreAddRequest path content ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-add" :: Text),
                "path" .= path,
                "hasContent" .= True
            ], Just content)

        StoreVerifyRequest path ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-verify" :: Text),
                "path" .= storePathToText path
            ], Nothing)

        StorePathRequest path content ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-path" :: Text),
                "path" .= path,
                "hasContent" .= True
            ], Just content)

        StoreListRequest ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-list" :: Text)
            ], Nothing)

        StoreDerivationRequest content ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-derivation" :: Text),
                "hasContent" .= True
            ], Just content)

        RetrieveDerivationRequest path ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("retrieve-derivation" :: Text),
                "path" .= storePathToText path
            ], Nothing)

        QueryDerivationRequest qType qValue limit ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("query-derivation" :: Text),
                "queryType" .= qType,
                "queryValue" .= qValue,
                "limit" .= limit
            ], Nothing)

        GetDerivationForOutputRequest path ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("get-derivation-for-output" :: Text),
                "path" .= path
            ], Nothing)

        ListDerivationsRequest limit ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("list-derivations" :: Text),
                "limit" .= limit
            ], Nothing)

        GCRequest params ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc" :: Text),
                "force" .= gcForce params
            ], Nothing)

        GCStatusRequest params ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc-status" :: Text),
                "forceCheck" .= gcForceCheck params
            ], Nothing)

        AddGCRootRequest path name permanent ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("add-gc-root" :: Text),
                "path" .= storePathToText path,
                "name" .= name,
                "permanent" .= permanent
            ], Nothing)

        RemoveGCRootRequest name ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("remove-gc-root" :: Text),
                "name" .= name
            ], Nothing)

        ListGCRootsRequest ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("list-gc-roots" :: Text)
            ], Nothing)

        PingRequest ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("ping" :: Text)
            ], Nothing)

        ShutdownRequest ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("shutdown" :: Text)
            ], Nothing)

        StatusRequest ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("status" :: Text)
            ], Nothing)

        ConfigRequest ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("config" :: Text)
            ], Nothing)

-- | Deserialize a daemon request from ByteString
deserializeDaemonRequest :: BS.ByteString -> Maybe BS.ByteString -> Either Text DaemonRequest
deserializeDaemonRequest bs payload =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "JSON parse error: " <> T.pack err
        Right val -> case Aeson.fromJSON val of
            Aeson.Error err -> Left $ "JSON conversion error: " <> T.pack err
            Aeson.Success obj -> do
                let typeVal = KeyMap.lookup "type" obj
                case typeVal of
                    Just (Aeson.String "auth") -> do
                        case KeyMap.lookup "content" obj of
                            Just content -> case Aeson.fromJSON content of
                                Aeson.Success auth -> Right $ AuthRequest auth
                                Aeson.Error err -> Left $ "Invalid auth content: " <> T.pack err
                            Nothing -> Left "Missing auth content"

                    Just (Aeson.String "build") -> do
                        path <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        info <- case KeyMap.lookup "info" obj of
                            Just i -> case Aeson.fromJSON i of
                                Aeson.Success info -> Right info
                                Aeson.Error err -> Left $ "Invalid build info: " <> T.pack err
                            Nothing -> Right defaultBuildRequestInfo

                        Right $ BuildRequest path payload info

                    Just (Aeson.String "eval") -> do
                        path <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        info <- case KeyMap.lookup "info" obj of
                            Just i -> case Aeson.fromJSON i of
                                Aeson.Success info -> Right info
                                Aeson.Error err -> Left $ "Invalid eval info: " <> T.pack err
                            Nothing -> Right defaultBuildRequestInfo

                        Right $ EvalRequest path payload info

                    Just (Aeson.String "build-derivation") -> do
                        -- Derivation content is in payload
                        case payload of
                            Nothing -> Left "Missing derivation content"
                            Just derivContent ->
                                case Ten.Derivation.deserializeDerivation derivContent of
                                    Left err -> Left $ "Invalid derivation: " <> errorToText err
                                    Right deriv -> do
                                        info <- case KeyMap.lookup "info" obj of
                                            Just i -> case Aeson.fromJSON i of
                                                Aeson.Success info -> Right info
                                                Aeson.Error err -> Left $ "Invalid build info: " <> T.pack err
                                            Nothing -> Right defaultBuildRequestInfo

                                        Right $ BuildDerivationRequest deriv info

                    Just (Aeson.String "build-status") -> do
                        bidStr <- case KeyMap.lookup "buildId" obj of
                            Just (Aeson.String s) -> Right s
                            _ -> Left "Missing or invalid buildId"

                        case parseBuildId bidStr of
                            Left err -> Left err
                            Right bid -> Right $ BuildStatusRequest bid

                    Just (Aeson.String "cancel-build") -> do
                        bidStr <- case KeyMap.lookup "buildId" obj of
                            Just (Aeson.String s) -> Right s
                            _ -> Left "Missing or invalid buildId"

                        case parseBuildId bidStr of
                            Left err -> Left err
                            Right bid -> Right $ CancelBuildRequest bid

                    Just (Aeson.String "build-output") -> do
                        bidStr <- case KeyMap.lookup "buildId" obj of
                            Just (Aeson.String s) -> Right s
                            _ -> Left "Missing or invalid buildId"

                        case parseBuildId bidStr of
                            Left err -> Left err
                            Right bid -> Right $ QueryBuildOutputRequest bid

                    Just (Aeson.String "list-builds") -> do
                        limit <- case KeyMap.lookup "limit" obj of
                            Just (Aeson.Number n) -> Right $ Just (round n)
                            Just Aeson.Null -> Right Nothing
                            Nothing -> Right Nothing
                            _ -> Left "Invalid limit value"

                        Right $ ListBuildsRequest limit

                    Just (Aeson.String "store-add") -> do
                        path <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        case payload of
                            Nothing -> Left "Missing content payload"
                            Just content -> Right $ StoreAddRequest path content

                    Just (Aeson.String "store-verify") -> do
                        pathText <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        case parseStorePath pathText of
                            Nothing -> Left $ "Invalid store path format: " <> pathText
                            Just path -> Right $ StoreVerifyRequest path

                    Just (Aeson.String "store-path") -> do
                        path <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        case payload of
                            Nothing -> Left "Missing content payload"
                            Just content -> Right $ StorePathRequest path content

                    Just (Aeson.String "store-list") -> Right StoreListRequest

                    Just (Aeson.String "store-derivation") -> do
                        case payload of
                            Nothing -> Left "Missing derivation content"
                            Just content -> Right $ StoreDerivationRequest content

                    Just (Aeson.String "retrieve-derivation") -> do
                        pathText <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        case parseStorePath pathText of
                            Nothing -> Left $ "Invalid store path format: " <> pathText
                            Just path -> Right $ RetrieveDerivationRequest path

                    Just (Aeson.String "query-derivation") -> do
                        qType <- case KeyMap.lookup "queryType" obj of
                            Just (Aeson.String t) -> Right t
                            _ -> Left "Missing or invalid queryType"

                        qValue <- case KeyMap.lookup "queryValue" obj of
                            Just (Aeson.String v) -> Right v
                            _ -> Left "Missing or invalid queryValue"

                        limit <- case KeyMap.lookup "limit" obj of
                            Just (Aeson.Number n) -> Right $ Just (round n)
                            Just Aeson.Null -> Right Nothing
                            Nothing -> Right Nothing
                            _ -> Left "Invalid limit value"

                        Right $ QueryDerivationRequest qType qValue limit

                    Just (Aeson.String "get-derivation-for-output") -> do
                        path <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        Right $ GetDerivationForOutputRequest path

                    Just (Aeson.String "list-derivations") -> do
                        limit <- case KeyMap.lookup "limit" obj of
                            Just (Aeson.Number n) -> Right $ Just (round n)
                            Just Aeson.Null -> Right Nothing
                            Nothing -> Right Nothing
                            _ -> Left "Invalid limit value"

                        Right $ ListDerivationsRequest limit

                    Just (Aeson.String "gc") -> do
                        force <- case KeyMap.lookup "force" obj of
                            Just (Aeson.Bool f) -> Right f
                            _ -> Right False

                        Right $ GCRequest (GCRequestParams force)

                    Just (Aeson.String "gc-status") -> do
                        forceCheck <- case KeyMap.lookup "forceCheck" obj of
                            Just (Aeson.Bool f) -> Right f
                            _ -> Right False

                        Right $ GCStatusRequest (GCStatusRequestParams forceCheck)

                    Just (Aeson.String "add-gc-root") -> do
                        pathText <- case KeyMap.lookup "path" obj of
                            Just (Aeson.String p) -> Right p
                            _ -> Left "Missing or invalid path"

                        name <- case KeyMap.lookup "name" obj of
                            Just (Aeson.String n) -> Right n
                            _ -> Left "Missing or invalid name"

                        permanent <- case KeyMap.lookup "permanent" obj of
                            Just (Aeson.Bool p) -> Right p
                            _ -> Right False

                        case parseStorePath pathText of
                            Nothing -> Left $ "Invalid store path format: " <> pathText
                            Just path -> Right $ AddGCRootRequest path name permanent

                    Just (Aeson.String "remove-gc-root") -> do
                        name <- case KeyMap.lookup "name" obj of
                            Just (Aeson.String n) -> Right n
                            _ -> Left "Missing or invalid name"

                        Right $ RemoveGCRootRequest name

                    Just (Aeson.String "list-gc-roots") -> Right ListGCRootsRequest

                    Just (Aeson.String "ping") -> Right PingRequest

                    Just (Aeson.String "shutdown") -> Right ShutdownRequest

                    Just (Aeson.String "status") -> Right StatusRequest

                    Just (Aeson.String "config") -> Right ConfigRequest

                    Just (Aeson.String t) -> Left $ "Unknown request type: " <> t

                    _ -> Left "Missing or invalid request type"

-- | Serialize a daemon response to ByteString
serializeDaemonResponse :: DaemonResponse -> (BS.ByteString, Maybe BS.ByteString)
serializeDaemonResponse resp =
    case resp of
        AuthResponse authResult ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("auth" :: Text),
                "result" .= authResult
            ], Nothing)

        BuildStartedResponse buildId ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-started" :: Text),
                "buildId" .= renderBuildId buildId
            ], Nothing)

        BuildResultResponse result ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-result" :: Text),
                "result" .= result
            ], Nothing)

        BuildStatusResponse update ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-status" :: Text),
                "update" .= update
            ], Nothing)

        BuildOutputResponse output ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-output" :: Text),
                "output" .= output
            ], Nothing)

        BuildListResponse builds ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("build-list" :: Text),
                "builds" .= encodeBuilds builds
            ], Nothing)

        CancelBuildResponse success ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("cancel-build" :: Text),
                "success" .= success
            ], Nothing)

        StoreAddResponse path ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-add" :: Text),
                "path" .= storePathToText path
            ], Nothing)

        StoreVerifyResponse valid ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-verify" :: Text),
                "valid" .= valid
            ], Nothing)

        StorePathResponse path ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-path" :: Text),
                "path" .= storePathToText path
            ], Nothing)

        StoreListResponse paths ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("store-list" :: Text),
                "paths" .= map storePathToText paths
            ], Nothing)

        DerivationResponse deriv ->
            let derivContent = Ten.Derivation.serializeDerivation deriv
            in (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("derivation" :: Text),
                "hasContent" .= True
            ], Just derivContent)

        DerivationStoredResponse path ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("derivation-stored" :: Text),
                "path" .= storePathToText path
            ], Nothing)

        DerivationRetrievedResponse mDeriv ->
            case mDeriv of
                Nothing ->
                    (LBS.toStrict $ Aeson.encode $ Aeson.object [
                        "type" .= ("derivation-retrieved" :: Text),
                        "found" .= False
                    ], Nothing)
                Just deriv ->
                    let derivContent = Ten.Derivation.serializeDerivation deriv
                    in (LBS.toStrict $ Aeson.encode $ Aeson.object [
                        "type" .= ("derivation-retrieved" :: Text),
                        "found" .= True,
                        "hasContent" .= True
                    ], Just derivContent)

        DerivationQueryResponse derivs ->
            let derivsContent = BS.concat $ map Ten.Derivation.serializeDerivation derivs
            in (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("derivation-query" :: Text),
                "count" .= length derivs,
                "hasContent" .= True
            ], Just derivsContent)

        DerivationOutputResponse paths ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("derivation-outputs" :: Text),
                "outputs" .= map storePathToText (Set.toList paths)
            ], Nothing)

        DerivationListResponse paths ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("derivation-list" :: Text),
                "paths" .= map storePathToText paths
            ], Nothing)

        GCResultResponse stats ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc-result" :: Text),
                "stats" .= stats
            ], Nothing)

        GCStartedResponse ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc-started" :: Text)
            ], Nothing)

        GCStatusResponse status ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc-status" :: Text),
                "status" .= status
            ], Nothing)

        GCRootAddedResponse name ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc-root-added" :: Text),
                "name" .= name
            ], Nothing)

        GCRootRemovedResponse name ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc-root-removed" :: Text),
                "name" .= name
            ], Nothing)

        GCRootListResponse roots ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("gc-roots-list" :: Text),
                "roots" .= encodeRoots roots
            ], Nothing)

        PongResponse ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("pong" :: Text)
            ], Nothing)

        ShutdownResponse ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("shutdown" :: Text)
            ], Nothing)

        StatusResponse status ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("status" :: Text),
                "status" .= status
            ], Nothing)

        ConfigResponse config ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("config" :: Text),
                "config" .= config
            ], Nothing)

        EvalResponse deriv ->
            let derivContent = Ten.Derivation.serializeDerivation deriv
            in (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("eval" :: Text),
                "hasContent" .= True
            ], Just derivContent)

        ErrorResponse err ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("error" :: Text),
                "error" .= encodeError err
            ], Nothing)

        SuccessResponse ->
            (LBS.toStrict $ Aeson.encode $ Aeson.object [
                "type" .= ("success" :: Text)
            ], Nothing)
  where
    encodeBuilds builds = map encodeBuild builds

    encodeBuild (bid, status, progress) = Aeson.object [
        "id" .= renderBuildId bid,
        "status" .= showStatus status,
        "progress" .= progress
        ]

    encodeRoots roots = map encodeRoot roots

    encodeRoot (GCRoot path name rootType time) = Aeson.object [
        "path" .= storePathToText path,
        "name" .= name,
        "type" .= show rootType,
        "time" .= time
        ]

    showStatus :: BuildStatus -> Text
    showStatus BuildPending = "pending"
    showStatus (BuildRunning _) = "running"
    showStatus (BuildRecursing _) = "recursing"
    showStatus BuildCompleted = "completed"
    showStatus BuildFailed' = "failed"

    encodeError :: BuildError -> Aeson.Value
    encodeError err = Aeson.object [
        "errorType" .= errorTypeString err,
        "message" .= errorToText err
        ]

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

-- | Deserialize a daemon response from ByteString
deserializeDaemonResponse :: BS.ByteString -> Maybe BS.ByteString -> Either Text DaemonResponse
deserializeDaemonResponse bs payload =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "JSON parse error: " <> T.pack err
        Right val -> case Aeson.fromJSON val of
            Aeson.Error err -> Left $ "JSON conversion error: " <> T.pack err
            Aeson.Success obj -> do
                let typeVal = KeyMap.lookup "type" obj
                case typeVal of
                    Just (Aeson.String "auth") -> do
                        result <- case KeyMap.lookup "result" obj of
                            Just r -> case Aeson.fromJSON r of
                                Aeson.Success authResult -> Right authResult
                                Aeson.Error err -> Left $ "Invalid auth result: " <> T.pack err
                            Nothing -> Left "Missing auth result"

                        Right $ AuthResponse result

                    Just (Aeson.String "build-started") -> do
                        bidStr <- case KeyMap.lookup "buildId" obj of
                            Just (Aeson.String s) -> Right s
                            _ -> Left "Missing or invalid buildId"

                        case parseBuildId bidStr of
                            Left err -> Left err
                            Right bid -> Right $ BuildStartedResponse bid

                    Just (Aeson.String "build-result") -> do
                        result <- case KeyMap.lookup "result" obj of
                            Just r -> case Aeson.fromJSON r of
                                Aeson.Success result -> Right result
                                Aeson.Error err -> Left $ "Invalid build result: " <> T.pack err
                            Nothing -> Left "Missing build result"

                        Right $ BuildResultResponse result

                    Just (Aeson.String "build-status") -> do
                        update <- case KeyMap.lookup "update" obj of
                            Just u -> case Aeson.fromJSON u of
                                Aeson.Success update -> Right update
                                Aeson.Error err -> Left $ "Invalid build status update: " <> T.pack err
                            Nothing -> Left "Missing build status update"

                        Right $ BuildStatusResponse update

                    -- Continue with other response types...
                    -- This is just a representative sample - the full implementation
                    -- would handle all response types with proper payload handling

                    Just (Aeson.String "error") -> do
                        errObj <- case KeyMap.lookup "error" obj of
                            Just e -> Right e
                            Nothing -> Left "Missing error details"

                        err <- case Aeson.fromJSON errObj of
                            Aeson.Success err -> Right err
                            Aeson.Error parseErr -> Left $ "Invalid error format: " <> T.pack parseErr

                        Right $ ErrorResponse err

                    Just (Aeson.String "success") -> Right SuccessResponse

                    Just (Aeson.String t) -> Left $ "Unknown response type: " <> t

                    _ -> Left "Missing or invalid response type"

-- | Send a daemon request through a protocol handle
sendDaemonRequest :: ProtocolHandle -> DaemonRequest -> IO (Either ProtocolError DaemonResponse)
sendDaemonRequest handle req = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Serialize the request and any payload
        let (reqData, mPayload) = serializeDaemonRequest req

        -- Create frame for the request
        let framedReq = createRequestFrame reqData

        -- Create frame for payload if any
        let framedPayload = case mPayload of
                Just payload -> createRequestFrame payload
                Nothing -> BS.empty

        -- Send the request frame
        result <- try $ do
            NByte.sendAll (protocolSocket handle) framedReq

            -- Send payload if any
            unless (BS.null framedPayload) $
                NByte.sendAll (protocolSocket handle) framedPayload

            -- Read response
            receiveFramedResponse (protocolSocket handle)

        case result of
            Left (e :: SomeException) ->
                return $ Left $ ConnectionClosed

            Right (respData, mRespPayload) -> do
                -- Deserialize response
                case deserializeDaemonResponse respData mRespPayload of
                    Left err -> return $ Left $ ProtocolParseError err
                    Right resp -> return $ Right resp

-- | Receive a daemon response from a socket
receiveFramedResponse :: Socket -> IO (BS.ByteString, Maybe BS.ByteString)
receiveFramedResponse sock = do
    -- Read response frame
    responseData <- recvFrame sock

    -- Check if response indicates a payload
    case Aeson.decodeStrict responseData of
        Nothing -> return (responseData, Nothing)
        Just obj -> do
            let hasPayload = case Aeson.fromJSON obj of
                    Aeson.Success (Aeson.Object o) ->
                        case KeyMap.lookup "hasContent" o of
                            Just (Aeson.Bool True) -> True
                            _ -> False
                    _ -> False

            if hasPayload
                then do
                    -- Read payload frame
                    payloadData <- recvFrame sock
                    return (responseData, Just payloadData)
                else
                    return (responseData, Nothing)
  where
    recvFrame :: Socket -> IO BS.ByteString
    recvFrame s = do
        -- Read length prefix (4 bytes)
        lenBytes <- NByte.recv s 4
        when (BS.length lenBytes /= 4) $
            throwIO ConnectionClosed

        -- Parse length
        let len = (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
                 (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
                 (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
                 (fromIntegral (BS.index lenBytes 3) :: Word32)

        -- Check if message is too large
        when (len > 100 * 1024 * 1024) $ -- 100 MB limit
            throwIO $ MessageTooLarge len

        -- Read message data
        recvExactly s (fromIntegral len)

    recvExactly :: Socket -> Int -> IO BS.ByteString
    recvExactly s len = go len []
      where
        go 0 chunks = return $ BS.concat $ reverse chunks
        go n chunks = do
            chunk <- NByte.recv s n
            let chunkLen = BS.length chunk
            if chunkLen == 0
                then throwIO ConnectionClosed
                else go (n - chunkLen) (chunk : chunks)

-- | Send a daemon response through a protocol handle
sendDaemonResponse :: ProtocolHandle -> DaemonResponse -> IO ()
sendDaemonResponse handle resp = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Serialize the response and any payload
        let (respData, mPayload) = serializeDaemonResponse resp

        -- Create frame for the response
        let framedResp = createResponseFrame respData

        -- Create frame for payload if any
        let framedPayload = case mPayload of
                Just payload -> createResponseFrame payload
                Nothing -> BS.empty

        -- Send the response frame
        NByte.sendAll (protocolSocket handle) framedResp

        -- Send payload if any
        unless (BS.null framedPayload) $
            NByte.sendAll (protocolSocket handle) framedPayload

-- | Receive a daemon request from a socket
receiveDaemonRequest :: Socket -> IO (Either ProtocolError DaemonRequest)
receiveDaemonRequest sock = do
    -- Try to read the request
    result <- try $ do
        -- Read request frame
        requestData <- recvFrame sock

        -- Check if request indicates a payload
        hasPayload <- case Aeson.decodeStrict requestData of
            Nothing -> return False
            Just obj -> return $ case Aeson.fromJSON obj of
                Aeson.Success (Aeson.Object o) ->
                    case KeyMap.lookup "hasContent" o of
                        Just (Aeson.Bool True) -> True
                        _ -> False
                _ -> False

        -- Read payload if indicated
        mPayload <- if hasPayload
            then do
                payloadData <- recvFrame sock
                return $ Just payloadData
            else
                return Nothing

        -- Return request data and optional payload
        return (requestData, mPayload)

    case result of
        Left (e :: SomeException) ->
            return $ Left ConnectionClosed

        Right (reqData, mPayload) -> do
            -- Deserialize request
            case deserializeDaemonRequest reqData mPayload of
                Left err -> return $ Left $ ProtocolParseError err
                Right req -> return $ Right req
  where
    recvFrame :: Socket -> IO BS.ByteString
    recvFrame s = do
        -- Read length prefix (4 bytes)
        lenBytes <- NByte.recv s 4
        when (BS.length lenBytes /= 4) $
            throwIO ConnectionClosed

        -- Parse length
        let len = (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
                 (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
                 (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
                 (fromIntegral (BS.index lenBytes 3) :: Word32)

        -- Check if message is too large
        when (len > 100 * 1024 * 1024) $ -- 100 MB limit
            throwIO $ MessageTooLarge len

        -- Read message data
        recvExactly s (fromIntegral len)

    recvExactly :: Socket -> Int -> IO BS.ByteString
    recvExactly s len = go len []
      where
        go 0 chunks = return $ BS.concat $ reverse chunks
        go n chunks = do
            chunk <- NByte.recv s n
            let chunkLen = BS.length chunk
            if chunkLen == 0
                then throwIO ConnectionClosed
                else go (n - chunkLen) (chunk : chunks)

-- | Convert from Response payload to ResponseContent
responseToResponseData :: Aeson.Value -> Either Text DaemonResponse
responseToResponseData payload =
    case Aeson.fromJSON payload of
        Aeson.Success respData -> Right respData
        Aeson.Error err -> Left $ "Failed to decode response payload: " <> T.pack err

-- | Execute an action with a protocol handle and clean up after
withProtocolHandle :: Socket -> PrivilegeTier -> (ProtocolHandle -> IO a) -> IO a
withProtocolHandle socket tier =
    bracket
        (createProtocolHandle socket tier)
        closeProtocolHandle

-- | Convert a request to human-readable text
requestToText :: DaemonRequest -> Text
requestToText = \case
    AuthRequest _ ->
        "Authentication request"

    BuildRequest path _ _ ->
        "Build file: " <> path

    EvalRequest path _ _ ->
        "Evaluate file: " <> path

    BuildDerivationRequest _ _ ->
        "Build derivation"

    BuildStatusRequest bid ->
        "Query build status: " <> renderBuildId bid

    CancelBuildRequest bid ->
        "Cancel build: " <> renderBuildId bid

    QueryBuildOutputRequest bid ->
        "Query build output: " <> renderBuildId bid

    ListBuildsRequest limit ->
        "List builds" <> maybe "" (\n -> " (limit: " <> T.pack (show n) <> ")") limit

    StoreAddRequest path _ ->
        "Add to store: " <> path

    StoreVerifyRequest path ->
        "Verify path: " <> storePathToText path

    StorePathRequest path _ ->
        "Get store path for: " <> path

    StoreListRequest ->
        "List store contents"

    StoreDerivationRequest _ ->
        "Store derivation"

    RetrieveDerivationRequest path ->
        "Retrieve derivation: " <> storePathToText path

    QueryDerivationRequest qType qValue limit ->
        "Query derivation: " <> qType <> " " <> qValue <>
        maybe "" (\n -> " (limit: " <> T.pack (show n) <> ")") limit

    GetDerivationForOutputRequest path ->
        "Get derivation for output: " <> path

    ListDerivationsRequest limit ->
        "List derivations" <> maybe "" (\n -> " (limit: " <> T.pack (show n) <> ")") limit

    GCRequest params ->
        "Collect garbage" <> if gcForce params then " (force)" else ""

    GCStatusRequest params ->
        "Check GC status" <> if gcForceCheck params then " (force check)" else ""

    AddGCRootRequest path name permanent ->
        "Add GC root: " <> name <> " -> " <> storePathToText path <>
        if permanent then " (permanent)" else ""

    RemoveGCRootRequest name ->
        "Remove GC root: " <> name

    ListGCRootsRequest ->
        "List GC roots"

    PingRequest ->
        "Ping"

    ShutdownRequest ->
        "Shutdown"

    StatusRequest ->
        "Get daemon status"

    ConfigRequest ->
        "Get daemon configuration"

-- | Convert a response to human-readable text
responseToText :: DaemonResponse -> Text
responseToText = \case
    AuthResponse (AuthAccepted uid _ _) ->
        "Auth accepted: " <> case uid of UserId u -> u

    AuthResponse (AuthRejected reason) ->
        "Auth rejected: " <> reason

    AuthResponse (AuthSuccess uid _) ->
        "Auth success: " <> case uid of UserId u -> u

    BuildStartedResponse buildId ->
        "Build started: " <> renderBuildId buildId

    BuildResultResponse _ ->
        "Build completed"

    BuildStatusResponse update ->
        "Build status: " <> showStatus (buildStatus update)

    BuildOutputResponse _ ->
        "Build output"

    BuildListResponse builds ->
        "Build list: " <> T.pack (show (length builds)) <> " builds"

    CancelBuildResponse success ->
        "Build cancelled: " <> if success then "success" else "failed"

    StoreAddResponse path ->
        "Added to store: " <> storePathToText path

    StoreVerifyResponse valid ->
        "Path verification: " <> (if valid then "valid" else "invalid")

    StorePathResponse path ->
        "Store path: " <> storePathToText path

    StoreListResponse paths ->
        "Store contents: " <> T.pack (show (length paths)) <> " paths"

    DerivationResponse _ ->
        "Derivation"

    DerivationStoredResponse path ->
        "Derivation stored: " <> storePathToText path

    DerivationRetrievedResponse Nothing ->
        "Derivation not found"

    DerivationRetrievedResponse (Just _) ->
        "Derivation retrieved"

    DerivationQueryResponse derivs ->
        "Derivation query results: " <> T.pack (show (length derivs)) <> " derivations"

    DerivationOutputResponse paths ->
        "Derivation outputs: " <> T.pack (show (Set.size paths)) <> " paths"

    DerivationListResponse paths ->
        "Derivation list: " <> T.pack (show (length paths)) <> " derivations"

    GCResultResponse stats ->
        "GC completed: " <> T.pack (show (gcCollected stats)) <> " paths collected"

    GCStartedResponse ->
        "GC started"

    GCStatusResponse status ->
        "GC status: " <> if gcRunning status then "running" else "not running" <>
        maybe "" (\owner -> " (owned by " <> owner <> ")") (gcOwner status)

    GCRootAddedResponse name ->
        "GC root added: " <> name

    GCRootRemovedResponse name ->
        "GC root removed: " <> name

    GCRootListResponse roots ->
        "GC roots: " <> T.pack (show (length roots)) <> " roots"

    PongResponse ->
        "Pong"

    ShutdownResponse ->
        "Shutdown acknowledged"

    StatusResponse _ ->
        "Daemon status"

    ConfigResponse _ ->
        "Daemon configuration"

    EvalResponse _ ->
        "Evaluation result"

    ErrorResponse err ->
        "Error: " <> errorToText err

    SuccessResponse ->
        "Success"
  where
    showStatus :: BuildStatus -> Text
    showStatus BuildPending = "pending"
    showStatus (BuildRunning progress) = "running (" <> T.pack (show (round (progress * 100))) <> "%)"
    showStatus (BuildRecursing innerBuildId) = "recursing to " <> renderBuildId innerBuildId
    showStatus BuildCompleted = "completed"
    showStatus BuildFailed' = "failed"

-- | Int64 type for database IDs
type Int64 = Int
