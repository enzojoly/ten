{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ten.Daemon.Protocol (
    -- Protocol versions
    ProtocolVersion(..),
    currentProtocolVersion,
    compatibleVersions,

    PrivilegeTier(..),
    SPrivilegeTier(..),
    SDaemon,
    SBuilder,

    -- Message types with privilege awareness
    Message(..),
    MessageType(..),
    RequestContent(..),
    ResponseContent(..),

    -- Capability system
    DaemonCapability(..),
    requestCapabilities,
    verifyCapabilities,
    checkPrivilegeRequirement,

    -- Request privilege types
    RequestPrivilege(..),
    ResponsePrivilege(..),
    PrivilegeRequirement(..),
    PrivilegeError(..),

    -- Authentication types
    UserCredentials(..),
    AuthRequestContent(..),
    AuthResult(..),

    -- Build tracking
    BuildRequestInfo(..),
    BuildStatusUpdate(..),
    defaultBuildRequestInfo,

    -- Build results
    BuildResult(..),

    -- Derivation operations
    DerivationInfo(..),
    DerivationInfoResponse(..),
    DerivationOutputMappingResponse(..),

    -- Daemon status and configuration
    DaemonStatus(..),
    DaemonConfig(..),

    -- GC status
    GCRequestParams(..),
    GCStatusRequestParams(..),
    GCStatusResponse(..),
    GCStats(..),

    -- Type families for permissions
    CanAccessStore,
    CanCreateSandbox,
    CanDropPrivileges,
    CanModifyStore,
    CanAccessDatabase,
    CanRunGC,

    -- Privilege transition
    PrivilegeTransition(..),
    dropPrivilege,
    SomePrivilegeTier(..),
    withSomePrivilegeTier,

    -- Serialization functions with privilege awareness
    serializeMessage,
    deserializeMessage,
    serializeRequest,
    deserializeRequest,
    serializeResponse,
    deserializeResponse,

    -- Protocol framing
    createRequestFrame,
    parseRequestFrame,
    createResponseFrame,
    parseResponseFrame,

    -- Socket communication with privilege checking
    sendRequest,
    receiveResponse,
    sendResponse,
    receiveRequest,

    -- Connection management
    ProtocolHandle(..),
    createHandle,
    closeHandle,
    withProtocolHandle,

    -- Utilities
    requestToText,
    responseToText,

    -- Exception types
    ProtocolError(..),

    -- Response conversion helpers
    responseToResponseData,
    responsePrivilegeRequirement,

    -- Utilities for working with paths
    parseBuildId,
    renderBuildId,
    hashByteString,

    -- Error handling utilities
    errorToText
) where

import Control.Concurrent (forkIO, killThread, threadDelay, myThreadId)
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO, bracket, try, SomeException)
import Control.Monad (unless, when, foldM)
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
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Unique (Unique, hashUnique)

-- Import Ten modules
import Ten.Core (fromSing, BuildId(..), BuildStatus(..), BuildError(..), StorePath(..),
                 UserId(..), AuthToken(..), StoreReference(..), ReferenceType(..),
                 Phase(..), PrivilegeTier(..), SPhase(..), SPrivilegeTier(..),
                 CanAccessStore, CanAccessDatabase, CanCreateSandbox, CanDropPrivileges,
                 CanModifyStore, Derivation, buildErrorToText)
import qualified Ten.Derivation (serializeDerivation, deserializeDerivation)

-- | Protocol version
data ProtocolVersion = ProtocolVersion {
    protocolMajor :: Int,
    protocolMinor :: Int,
    protocolPatch :: Int
} deriving (Eq, Generic)

instance Ord ProtocolVersion where
    compare a b = compare (protocolMajor a, protocolMinor a, protocolPatch a)
                          (protocolMajor b, protocolMinor b, protocolPatch b)

instance Show ProtocolVersion where
    show (ProtocolVersion major minor patch) =
        show major ++ "." ++ show minor ++ "." ++ show patch

instance Aeson.ToJSON ProtocolVersion where
    toJSON ProtocolVersion{..} = Aeson.object [
            "major" .= protocolMajor,
            "minor" .= protocolMinor,
            "patch" .= protocolPatch
        ]

instance Aeson.FromJSON ProtocolVersion where
    parseJSON = Aeson.withObject "ProtocolVersion" $ \v -> do
        protocolMajor <- v .: "major"
        protocolMinor <- v .: "minor"
        protocolPatch <- v .: "patch"
        return ProtocolVersion{..}

-- | Current protocol version
currentProtocolVersion :: ProtocolVersion
currentProtocolVersion = ProtocolVersion 1 0 0

-- | List of compatible protocol versions
compatibleVersions :: [ProtocolVersion]
compatibleVersions = [
    ProtocolVersion 1 0 0
    ]

-- | Singleton instances from Ten.Core
sDaemon :: SPrivilegeTier 'Daemon
sDaemon = SDaemon

sBuilder :: SPrivilegeTier 'Builder
sBuilder = SBuilder

-- | Existential wrapper for privilege tier singletons
data SomePrivilegeTier where
    SomePrivilegeTier :: SPrivilegeTier t -> SomePrivilegeTier

-- | Pattern match on SomePrivilegeTier to use the contained singleton
withSomePrivilegeTier :: SomePrivilegeTier -> (forall t. SPrivilegeTier t -> r) -> r
withSomePrivilegeTier (SomePrivilegeTier s) f = f s

-- | Type family for GC permissions
type family CanRunGC (t :: PrivilegeTier) :: Bool where
    CanRunGC 'Daemon = 'True
    CanRunGC 'Builder = 'False

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
    | PrivilegeViolation Text  -- Privilege violation error
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

-- | Authentication result
data AuthResult
    = AuthAccepted UserId AuthToken (Set DaemonCapability)  -- Now includes granted capabilities
    | AuthRejected Text
    | AuthSuccess UserId AuthToken  -- For backward compatibility
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON AuthResult where
    toJSON (AuthAccepted (UserId uid) (AuthToken token) caps) = Aeson.object [
            "status" .= ("accepted" :: Text),
            "userId" .= uid,
            "token" .= token,
            "capabilities" .= map (\c -> T.pack (show c)) (Set.toList caps)
        ]
    toJSON (AuthRejected reason) = Aeson.object [
            "status" .= ("rejected" :: Text),
            "reason" .= reason
        ]
    toJSON (AuthSuccess (UserId uid) (AuthToken token)) = Aeson.object [
            "status" .= ("success" :: Text),
            "userId" .= uid,
            "token" .= token
        ]

instance Aeson.FromJSON AuthResult where
    parseJSON = Aeson.withObject "AuthResult" $ \v -> do
        status <- v .: "status" :: Aeson.Parser Text
        case status of
            "accepted" -> do
                uid <- v .: "userId"
                token <- v .: "token"
                capsStrings <- v .:? "capabilities" .!= [] :: Aeson.Parser [Text]
                let caps = Set.fromList $ map parseCapability capsStrings
                return $ AuthAccepted (UserId uid) (AuthToken token) caps
            "rejected" -> do
                reason <- v .: "reason"
                return $ AuthRejected reason
            "success" -> do
                uid <- v .: "userId"
                token <- v .: "token"
                return $ AuthSuccess (UserId uid) (AuthToken token)
            _ -> fail $ "Unknown auth status: " ++ T.unpack status
        where
            parseCapability :: Text -> DaemonCapability
            parseCapability "StoreAccess" = StoreAccess
            parseCapability "SandboxCreation" = SandboxCreation
            parseCapability "GarbageCollection" = GarbageCollection
            parseCapability "DerivationRegistration" = DerivationRegistration
            parseCapability "DerivationBuild" = DerivationBuild
            parseCapability "StoreQuery" = StoreQuery
            parseCapability "BuildQuery" = BuildQuery
            parseCapability _ = BuildQuery  -- Default to safe capability

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

instance Aeson.ToJSON DerivationInfoResponse where
    toJSON DerivationInfoResponse{..} = Aeson.object [
            "derivation" .= TE.decodeUtf8 (Ten.Derivation.serializeDerivation derivationResponseDrv),
            "outputs" .= map encodePath (Set.toList derivationResponseOutputs),
            "inputs" .= map encodePath (Set.toList derivationResponseInputs),
            "storePath" .= encodePath derivationResponseStorePath,
            "metadata" .= derivationResponseMetadata
        ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

instance Aeson.FromJSON DerivationInfoResponse where
    parseJSON = Aeson.withObject "DerivationInfoResponse" $ \v -> do
        derivText <- v .: "derivation" :: Aeson.Parser Text
        derivation <- case Ten.Derivation.deserializeDerivation (TE.encodeUtf8 derivText) of
            Left err -> fail $ "Invalid derivation: " ++ T.unpack (buildErrorToText err)
            Right d -> return d

        outputsJson <- v .: "outputs"
        outputs <- mapM decodePath outputsJson

        inputsJson <- v .: "inputs"
        inputs <- mapM decodePath inputsJson

        storePathJson <- v .: "storePath"
        storePath <- decodePath storePathJson

        metadata <- v .: "metadata"

        return $ DerivationInfoResponse derivation (Set.fromList outputs) (Set.fromList inputs) storePath metadata
      where
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

-- | Response with derivation-output mappings
data DerivationOutputMappingResponse = DerivationOutputMappingResponse {
    outputMappings :: [(StorePath, StorePath)],  -- [(derivation path, output path)]
    outputMappingCount :: Int,
    outputMappingComplete :: Bool   -- Whether all mappings were included
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DerivationOutputMappingResponse where
    toJSON DerivationOutputMappingResponse{..} = Aeson.object [
            "mappings" .= map encodeMappings outputMappings,
            "count" .= outputMappingCount,
            "complete" .= outputMappingComplete
        ]
      where
        encodeMappings (derivPath, outPath) = Aeson.object [
                "derivation" .= encodePath derivPath,
                "output" .= encodePath outPath
            ]
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

instance Aeson.FromJSON DerivationOutputMappingResponse where
    parseJSON = Aeson.withObject "DerivationOutputMappingResponse" $ \v -> do
        mappingsJson <- v .: "mappings"
        mappings <- mapM decodeMappings mappingsJson
        count <- v .: "count"
        complete <- v .: "complete"
        return $ DerivationOutputMappingResponse mappings count complete
      where
        decodeMappings = Aeson.withObject "Mapping" $ \m -> do
            derivPathJson <- m .: "derivation"
            derivPath <- decodePath derivPathJson
            outPathJson <- m .: "output"
            outPath <- decodePath outPathJson
            return (derivPath, outPath)
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

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

instance Aeson.ToJSON BuildStatusUpdate where
    toJSON BuildStatusUpdate{..} = Aeson.object [
            "buildId" .= renderBuildId buildId,
            "status" .= serializeBuildStatus buildStatus,
            "timeElapsed" .= buildTimeElapsed,
            "timeRemaining" .= buildTimeRemaining,
            "logUpdate" .= buildLogUpdate,
            "resourceUsage" .= buildResourceUsage
        ]
      where
        serializeBuildStatus :: BuildStatus -> Aeson.Value
        serializeBuildStatus BuildPending = Aeson.object [
                "type" .= ("pending" :: Text)
            ]
        serializeBuildStatus (BuildRunning progress) = Aeson.object [
                "type" .= ("running" :: Text),
                "progress" .= progress
            ]
        serializeBuildStatus (BuildRecursing innerBuildId) = Aeson.object [
                "type" .= ("recursing" :: Text),
                "innerBuildId" .= renderBuildId innerBuildId
            ]
        serializeBuildStatus BuildCompleted = Aeson.object [
                "type" .= ("completed" :: Text)
            ]
        serializeBuildStatus BuildFailed' = Aeson.object [
                "type" .= ("failed" :: Text)
            ]

instance Aeson.FromJSON BuildStatusUpdate where
    parseJSON = Aeson.withObject "BuildStatusUpdate" $ \v -> do
        buildIdStr <- v .: "buildId" :: Aeson.Parser Text
        case parseBuildId buildIdStr of
            Left err -> fail $ T.unpack err
            Right buildId -> do
                statusObj <- v .: "status"
                buildStatus <- parseStatus statusObj
                buildTimeElapsed <- v .: "timeElapsed"
                buildTimeRemaining <- v .: "timeRemaining"
                buildLogUpdate <- v .: "logUpdate"
                buildResourceUsage <- v .: "resourceUsage"
                return BuildStatusUpdate{..}
      where
        parseStatus obj = Aeson.withObject "BuildStatus" (\o -> do
            statusType <- o .: "type" :: Aeson.Parser Text
            case statusType of
                "pending" -> return BuildPending
                "running" -> do
                    progress <- o .: "progress"
                    return $ BuildRunning progress
                "recursing" -> do
                    innerIdStr <- o .: "innerBuildId" :: Aeson.Parser Text
                    case parseBuildId innerIdStr of
                        Left err -> fail $ T.unpack err
                        Right innerBuildId -> return $ BuildRecursing innerBuildId
                "completed" -> return BuildCompleted
                "failed" -> return BuildFailed'
                _ -> fail $ "Unknown status type: " ++ T.unpack statusType
            ) obj

-- | GC Request parameters - renamed from GCRequestContent to avoid naming conflict
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

-- | GC Status Request parameters - renamed from GCStatusRequestContent to avoid naming conflict
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
data GCStatusResponse = GCStatusResponse {
    gcRunning :: Bool,           -- Whether GC is currently running
    gcOwner :: Maybe Text,       -- Process/username owning the GC lock (if running)
    gcLockTime :: Maybe UTCTime  -- When the GC lock was acquired (if running)
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON GCStatusResponse where
    toJSON GCStatusResponse{..} = Aeson.object [
            "running" .= gcRunning,
            "owner" .= gcOwner,
            "lockTime" .= gcLockTime
        ]

instance Aeson.FromJSON GCStatusResponse where
    parseJSON = Aeson.withObject "GCStatusResponse" $ \v -> do
        gcRunning <- v .: "running"
        gcOwner <- v .: "owner"
        gcLockTime <- v .: "lockTime"
        return GCStatusResponse{..}

-- | Message type tags for routing
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
    deriving (Show, Eq, Read, Generic)

-- | Core message type with privilege tracking
data Message (t :: PrivilegeTier) where
    -- Messages that can only be handled by the daemon (privileged operations)
    DaemonMessage :: (CanAccessStore t ~ 'True) =>
                     MessageType -> RequestContent -> Message t

    -- Messages that can be handled by the builder (unprivileged operations)
    BuilderMessage :: MessageType -> RequestContent -> Message t

    -- Authentication messages (special case)
    AuthMessage :: AuthRequestContent -> Message t

    -- Error messages
    ErrorMessage :: BuildError -> Message t

-- Add a deriving instance for Show
deriving instance Show (Message t)

-- | Contents of requests - separated from privilege tracking
-- Modified to use the standalone types instead of redefining fields
data RequestContent
    = BuildRequestContent {
        buildFilePath :: Text,
        buildFileContent :: Maybe BS.ByteString,
        buildOptions :: BuildRequestInfo
    }
    | EvalRequestContent {
        evalFilePath :: Text,
        evalFileContent :: Maybe BS.ByteString,
        evalOptions :: BuildRequestInfo
    }
    | BuildDerivationRequestContent {
        buildDerivation :: Derivation,
        buildDerivOptions :: BuildRequestInfo
    }
    | BuildStatusRequestContent {
        statusBuildId :: BuildId
    }
    | CancelBuildRequestContent {
        cancelBuildId :: BuildId
    }
    | QueryBuildOutputRequestContent {
        outputBuildId :: BuildId
    }
    | ListBuildsRequestContent {
        listLimit :: Maybe Int
    }
    | StoreAddRequestContent {
        storeAddPath :: Text,
        storeAddContent :: BS.ByteString
    }
    | StoreVerifyRequestContent {
        storeVerifyPath :: Text
    }
    | StorePathRequestContent {
        storePathForFile :: Text,
        storePathContent :: BS.ByteString
    }
    | StoreListRequestContent
    | StoreDerivationRequestContent {
        derivationContent :: BS.ByteString
    }
    | RetrieveDerivationRequestContent {
        derivationPath :: StorePath
    }
    | QueryDerivationRequestContent {
        queryType :: Text,
        queryValue :: Text,
        queryLimit :: Maybe Int
    }
    | GetDerivationForOutputRequestContent {
        getDerivationForPath :: Text
    }
    | ListDerivationsRequestContent {
        listDerivLimit :: Maybe Int
    }
    | GCRequestContent GCRequestParams  -- Reference to the standalone type instead of redefining
    | GCStatusRequestContent GCStatusRequestParams  -- Reference to the standalone type
    | AddGCRootRequestContent {
        rootPath :: StorePath,
        rootName :: Text,
        rootPermanent :: Bool
    }
    | RemoveGCRootRequestContent {
        rootNameToRemove :: Text
    }
    | ListGCRootsRequestContent
    | PingRequestContent
    | ShutdownRequestContent
    | StatusRequestContent
    | ConfigRequestContent
    deriving (Show, Eq)

-- | Content of responses
data ResponseContent
    = AuthResponseContent AuthResult
    | BuildStartedResponseContent BuildId
    | BuildResponseContent BuildResult
    | BuildStatusResponseContent BuildStatusUpdate
    | BuildOutputResponseContent Text
    | BuildListResponseContent [(BuildId, BuildStatus, Float)]
    | CancelBuildResponseContent Bool
    | StoreAddResponseContent StorePath
    | StoreVerifyResponseContent Bool
    | StorePathResponseContent StorePath
    | StoreListResponseContent [StorePath]
    | DerivationResponseContent Derivation
    | DerivationStoredResponseContent StorePath
    | DerivationRetrievedResponseContent (Maybe Derivation)
    | DerivationQueryResponseContent [Derivation]
    | DerivationOutputResponseContent (Set StorePath)
    | DerivationListResponseContent [StorePath]
    | GCResponseContent GCStats
    | GCStartedResponseContent
    | GCStatusResponseContent GCStatusResponse
    | GCRootAddedResponseContent Text
    | GCRootRemovedResponseContent Text
    | GCRootsListResponseContent [(StorePath, Text, Bool)]
    | PongResponseContent
    | ShutdownResponseContent
    | StatusResponseContent DaemonStatus
    | ConfigResponseContent DaemonConfig
    | EvalResponseContent Derivation
    | ErrorResponseContent BuildError
    deriving (Show, Eq)

-- | GC Statistics
data GCStats = GCStats {
    gcTotal :: Int,            -- Total paths in store
    gcLive :: Int,             -- Paths still reachable
    gcCollected :: Int,        -- Paths collected
    gcBytes :: Integer,        -- Bytes freed
    gcElapsedTime :: Double    -- Time taken for GC in seconds
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON GCStats where
    toJSON GCStats{..} = Aeson.object [
            "total" .= gcTotal,
            "live" .= gcLive,
            "collected" .= gcCollected,
            "bytes" .= gcBytes,
            "elapsedTime" .= gcElapsedTime
        ]

instance Aeson.FromJSON GCStats where
    parseJSON = Aeson.withObject "GCStats" $ \v -> do
        gcTotal <- v .: "total"
        gcLive <- v .: "live"
        gcCollected <- v .: "collected"
        gcBytes <- v .: "bytes"
        gcElapsedTime <- v .: "elapsedTime"
        return GCStats{..}

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

-- | Daemon configuration
data DaemonConfig = DaemonConfig {
    daemonSocketPath :: FilePath,       -- Path to daemon socket
    daemonStorePath :: FilePath,        -- Path to store
    daemonStateFile :: FilePath,        -- Path to state file
    daemonLogFile :: Maybe FilePath,    -- Optional log file path
    daemonLogLevel :: Int,              -- Log verbosity level
    daemonGcInterval :: Maybe Int,      -- Garbage collection interval in seconds
    daemonUser :: Maybe Text,           -- User to run as
    daemonGroup :: Maybe Text,          -- Group to run as
    daemonAllowedUsers :: Set Text,     -- Users allowed to connect
    daemonMaxJobs :: Int,               -- Maximum concurrent jobs
    daemonForeground :: Bool,           -- Run in foreground instead of daemonizing
    daemonTmpDir :: FilePath            -- Directory for temporary files
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DaemonConfig where
    toJSON DaemonConfig{..} = Aeson.object [
            "socketPath" .= daemonSocketPath,
            "storePath" .= daemonStorePath,
            "stateFile" .= daemonStateFile,
            "logFile" .= daemonLogFile,
            "logLevel" .= daemonLogLevel,
            "gcInterval" .= daemonGcInterval,
            "user" .= daemonUser,
            "group" .= daemonGroup,
            "allowedUsers" .= Set.toList daemonAllowedUsers,
            "maxJobs" .= daemonMaxJobs,
            "foreground" .= daemonForeground,
            "tmpDir" .= daemonTmpDir
        ]

instance Aeson.FromJSON DaemonConfig where
    parseJSON = Aeson.withObject "DaemonConfig" $ \v -> do
        daemonSocketPath <- v .: "socketPath"
        daemonStorePath <- v .: "storePath"
        daemonStateFile <- v .: "stateFile"
        daemonLogFile <- v .: "logFile"
        daemonLogLevel <- v .: "logLevel"
        daemonGcInterval <- v .: "gcInterval"
        daemonUser <- v .: "user"
        daemonGroup <- v .: "group"
        allowedUsersArray <- v .: "allowedUsers"
        let daemonAllowedUsers = Set.fromList allowedUsersArray
        daemonMaxJobs <- v .: "maxJobs"
        daemonForeground <- v .: "foreground"
        daemonTmpDir <- v .: "tmpDir"
        return DaemonConfig{..}

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
createHandle :: Socket -> PrivilegeTier -> IO ProtocolHandle
createHandle sock tier = do
    lock <- newMVar ()
    return $ ProtocolHandle sock lock tier

-- | Close a protocol handle
closeHandle :: ProtocolHandle -> IO ()
closeHandle handle = do
    close (protocolSocket handle)

-- | Create a framed request for sending over the wire
-- Format: [4-byte message length][message data]
createRequestFrame :: BS.ByteString -> BS.ByteString
createRequestFrame content = do
    let len = fromIntegral $ BS.length content
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
          ((fromIntegral (BS.index lenBytes 0) :: Word8 -> Word32) `shiftL` 24) .|.
          ((fromIntegral (BS.index lenBytes 1) :: Word8 -> Word32) `shiftL` 16) .|.
          ((fromIntegral (BS.index lenBytes 2) :: Word8 -> Word32) `shiftL` 8) .|.
          ((fromIntegral (BS.index lenBytes 3) :: Word8 -> Word32))

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

-- | Determine capabilities required for a request
requestCapabilities :: MessageType -> RequestContent -> Set DaemonCapability
requestCapabilities msgType content = case msgType of
    AuthType -> Set.empty  -- Auth requests don't require capabilities
    GCType -> Set.singleton GarbageCollection
    StoreAddType -> Set.singleton StoreAccess
    StoreDerivationType -> Set.singleton DerivationRegistration
    AddGCRootType -> Set.singleton StoreAccess
    RemoveGCRootType -> Set.singleton StoreAccess
    BuildDerivationType -> Set.fromList [DerivationBuild, StoreAccess]
    -- Add other privileged operations as needed
    _ -> Set.empty  -- Most operations are unprivileged

-- | Get privilege requirement for a response
responsePrivilegeRequirement :: ResponseContent -> ResponsePrivilege
responsePrivilegeRequirement resp = case resp of
    -- Privileged responses
    StoreAddResponseContent{} -> PrivilegedResponse
    GCResponseContent{} -> PrivilegedResponse
    DerivationStoredResponseContent{} -> PrivilegedResponse
    GCRootAddedResponseContent{} -> PrivilegedResponse
    GCRootRemovedResponseContent{} -> PrivilegedResponse

    -- Unprivileged responses
    _ -> UnprivilegedResponse

-- | Convert from Response payload to ResponseContent
responseToResponseData :: Aeson.Value -> Either Text ResponseContent
responseToResponseData payload =
    case Aeson.fromJSON payload of
        Aeson.Success respData -> Right respData
        Aeson.Error err -> Left $ "Failed to decode response payload: " <> T.pack err

-- | Hash a bytestring
hashByteString :: BS.ByteString -> Text
hashByteString bs =
    let digest = Crypto.hash bs :: Digest SHA256
    in T.pack $ show digest

-- | Build Result type
data BuildResult = BuildResult {
    brOutputPaths :: Set StorePath,     -- Outputs produced
    brExitCode :: ExitCode,            -- Exit code from builder
    brLog :: Text,                     -- Build log
    brReferences :: Set StorePath      -- References from outputs
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON BuildResult where
    toJSON BuildResult{..} = Aeson.object [
            "outputs" .= map encodePath (Set.toList brOutputPaths),
            "exitCode" .= encodeExitCode brExitCode,
            "log" .= brLog,
            "references" .= map encodePath (Set.toList brReferences)
        ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]
        encodeExitCode ExitSuccess = Aeson.object [
                "type" .= ("success" :: Text)
            ]
        encodeExitCode (ExitFailure code) = Aeson.object [
                "type" .= ("failure" :: Text),
                "code" .= code
            ]

instance Aeson.FromJSON BuildResult where
    parseJSON = Aeson.withObject "BuildResult" $ \v -> do
        outputsJson <- v .: "outputs"
        outputs <- mapM decodePath outputsJson
        exitCodeJson <- v .: "exitCode"
        exitCode <- decodeExitCode exitCodeJson
        log <- v .: "log"
        refsJson <- v .: "references"
        refs <- mapM decodePath refsJson
        return $ BuildResult (Set.fromList outputs) exitCode log (Set.fromList refs)
      where
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name
        decodeExitCode = Aeson.withObject "ExitCode" $ \e -> do
            typ <- e .: "type" :: Aeson.Parser Text
            case typ of
                "success" -> return ExitSuccess
                "failure" -> do
                    code <- e .: "code"
                    return $ ExitFailure code
                _ -> fail $ "Unknown exit code type: " ++ T.unpack typ

-- | Convert a request to human-readable text
requestToText :: RequestContent -> Text
requestToText req = case req of
    BuildRequestContent{..} ->
        "Build file: " <> buildFilePath

    EvalRequestContent{..} ->
        "Evaluate file: " <> evalFilePath

    BuildDerivationRequestContent{..} ->
        "Build derivation"

    BuildStatusRequestContent{..} ->
        "Query build status: " <> renderBuildId statusBuildId

    CancelBuildRequestContent{..} ->
        "Cancel build: " <> renderBuildId cancelBuildId

    QueryBuildOutputRequestContent{..} ->
        "Query build output: " <> renderBuildId outputBuildId

    ListBuildsRequestContent{..} ->
        "List builds" <> maybe "" (\n -> " (limit: " <> T.pack (show n) <> ")") listLimit

    StoreAddRequestContent{..} ->
        "Add to store: " <> storeAddPath

    StoreVerifyRequestContent{..} ->
        "Verify path: " <> storeVerifyPath

    StorePathRequestContent{..} ->
        "Get store path for: " <> storePathForFile

    StoreListRequestContent ->
        "List store contents"

    StoreDerivationRequestContent{..} ->
        "Store derivation"

    RetrieveDerivationRequestContent{..} ->
        "Retrieve derivation: " <> T.pack (show derivationPath)

    QueryDerivationRequestContent{..} ->
        "Query derivation: " <> queryType <> " " <> queryValue

    GetDerivationForOutputRequestContent{..} ->
        "Get derivation for output: " <> getDerivationForPath

    ListDerivationsRequestContent{..} ->
        "List derivations" <> maybe "" (\n -> " (limit: " <> T.pack (show n) <> ")") listDerivLimit

    GCRequestContent params ->
        "Collect garbage" <> if gcForce params then " (force)" else ""

    GCStatusRequestContent params ->
        "Check GC status" <> if gcForceCheck params then " (force check)" else ""

    AddGCRootRequestContent{..} ->
        "Add GC root: " <> rootName <> " -> " <> T.pack (show rootPath)

    RemoveGCRootRequestContent{..} ->
        "Remove GC root: " <> rootNameToRemove

    ListGCRootsRequestContent ->
        "List GC roots"

    PingRequestContent ->
        "Ping"

    ShutdownRequestContent ->
        "Shutdown"

    StatusRequestContent ->
        "Get daemon status"

    ConfigRequestContent ->
        "Get daemon configuration"

-- | Convert a response to human-readable text
responseToText :: ResponseContent -> Text
responseToText resp = case resp of
    AuthResponseContent (AuthAccepted uid _ _) ->
        "Auth accepted: " <> case uid of UserId u -> u

    AuthResponseContent (AuthRejected reason) ->
        "Auth rejected: " <> reason

    AuthResponseContent (AuthSuccess uid _) ->
        "Auth success: " <> case uid of UserId u -> u

    BuildStartedResponseContent buildId ->
        "Build started: " <> renderBuildId buildId

    BuildResponseContent _ ->
        "Build completed"

    BuildStatusResponseContent update ->
        "Build status: " <> showStatus (buildStatus update)

    BuildOutputResponseContent _ ->
        "Build output"

    BuildListResponseContent builds ->
        "Build list: " <> T.pack (show (length builds)) <> " builds"

    CancelBuildResponseContent success ->
        "Build cancelled: " <> if success then "success" else "failed"

    StoreAddResponseContent path ->
        "Added to store: " <> T.pack (show path)

    StoreVerifyResponseContent valid ->
        "Path verification: " <> (if valid then "valid" else "invalid")

    StorePathResponseContent path ->
        "Store path: " <> T.pack (show path)

    StoreListResponseContent paths ->
        "Store contents: " <> T.pack (show (length paths)) <> " paths"

    DerivationResponseContent _ ->
        "Derivation"

    DerivationStoredResponseContent path ->
        "Derivation stored: " <> T.pack (show path)

    DerivationRetrievedResponseContent Nothing ->
        "Derivation not found"

    DerivationRetrievedResponseContent (Just _) ->
        "Derivation retrieved"

    DerivationQueryResponseContent drvs ->
        "Derivation query results: " <> T.pack (show (length drvs)) <> " derivations"

    DerivationOutputResponseContent paths ->
        "Derivation outputs: " <> T.pack (show (Set.size paths)) <> " paths"

    DerivationListResponseContent paths ->
        "Derivation list: " <> T.pack (show (length paths)) <> " derivations"

    GCResponseContent stats ->
        "GC completed: " <> T.pack (show (gcCollected stats)) <> " paths collected"

    GCStartedResponseContent ->
        "GC started"

    GCStatusResponseContent status ->
        "GC status: " <> if gcRunning status then "running" else "not running" <>
        maybe "" (\owner -> " (owned by " <> owner <> ")") (gcOwner status)

    GCRootAddedResponseContent name ->
        "GC root added: " <> name

    GCRootRemovedResponseContent name ->
        "GC root removed: " <> name

    GCRootsListResponseContent roots ->
        "GC roots: " <> T.pack (show (length roots)) <> " roots"

    PongResponseContent ->
        "Pong"

    ShutdownResponseContent ->
        "Shutdown acknowledged"

    StatusResponseContent _ ->
        "Daemon status"

    ConfigResponseContent _ ->
        "Daemon configuration"

    EvalResponseContent _ ->
        "Evaluation result"

    ErrorResponseContent err ->
        "Error: " <> errorToText err
  where
    showStatus :: BuildStatus -> Text
    showStatus BuildPending = "pending"
    showStatus (BuildRunning progress) = "running (" <> T.pack (show (round (progress * 100))) <> "%)"
    showStatus (BuildRecursing innerBuildId) = "recursing to " <> renderBuildId innerBuildId
    showStatus BuildCompleted = "completed"
    showStatus BuildFailed' = "failed"

-- | Serialize a message with length prefix
serializeMessage :: Message t -> BS.ByteString
serializeMessage msg =
    -- Create content with length prefix
    createRequestFrame $ serializeMessageContent msg

-- | Serialize message content (internal)
serializeMessageContent :: Message t -> BS.ByteString
serializeMessageContent (DaemonMessage msgType content) =
    -- Serialize privileged message
    let jsonObj = Aeson.object [
            "type" .= ("daemon" :: Text),
            "messageType" .= show msgType,
            "content" .= Aeson.toJSON content,
            "privileged" .= True
            ]
    in LBS.toStrict $ Aeson.encode jsonObj

serializeMessageContent (BuilderMessage msgType content) =
    -- Serialize unprivileged message
    let jsonObj = Aeson.object [
            "type" .= ("builder" :: Text),
            "messageType" .= show msgType,
            "content" .= Aeson.toJSON content,
            "privileged" .= False
            ]
    in LBS.toStrict $ Aeson.encode jsonObj

serializeMessageContent (AuthMessage content) =
    -- Serialize auth message
    let jsonObj = Aeson.object [
            "type" .= ("auth" :: Text),
            "content" .= Aeson.toJSON content
            ]
    in LBS.toStrict $ Aeson.encode jsonObj

serializeMessageContent (ErrorMessage err) =
    -- Serialize error message
    let jsonObj = Aeson.object [
            "type" .= ("error" :: Text),
            "error" .= errorToJSON err
            ]
    in LBS.toStrict $ Aeson.encode jsonObj
  where
    errorToJSON :: BuildError -> Aeson.Value
    errorToJSON err = Aeson.object [
            "errorType" .= errorTypeString err,
            "message" .= errorMessage err
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
    errorTypeString _ = "unknown"

    errorMessage :: BuildError -> Text
    errorMessage = errorToText

-- | Deserialize a message with privilege checking
deserializeMessage :: BS.ByteString -> Either Text (Message t)
deserializeMessage bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "Failed to parse message: " <> T.pack err
        Right val -> deserializeMessageContent val

-- | Deserialize message content (internal)
deserializeMessageContent :: Aeson.Value -> Either Text (Message t)
deserializeMessageContent val =
    case val of
        Aeson.Object obj -> do
            msgType <- case KeyMap.lookup "type" obj of
                Just (Aeson.String typ) -> Right typ
                _ -> Left "Missing or invalid message type"

            case msgType of
                "daemon" -> do
                    -- Parse daemon message (privileged)
                    msgTypeStr <- case KeyMap.lookup "messageType" obj of
                        Just (Aeson.String t) -> Right t
                        _ -> Left "Missing or invalid messageType"

                    content <- case KeyMap.lookup "content" obj of
                        Just c -> case Aeson.fromJSON c of
                            Aeson.Success content -> Right content
                            Aeson.Error err -> Left $ "Invalid content: " <> T.pack err
                        _ -> Left "Missing message content"

                    -- We need to check privileges here, but can only do this properly at runtime
                    -- since the concrete type 't' is not available here
                    -- This will be checked at the call site using the verifyCapabilities function
                    let messageType = read (T.unpack msgTypeStr) :: MessageType

                    -- For a daemon message, the only valid type instance is t ~ 'Daemon
                    -- This will be enforced by the type system at use sites
                    case messageType of
                        -- Builder-only messages don't need to be privileged
                        PingType         -> Right (BuilderMessage messageType content)
                        StatusType       -> Right (BuilderMessage messageType content)
                        BuildStatusType  -> Right (BuilderMessage messageType content)

                        -- Messages that require store access must be Daemon messages
                        StoreAddType     -> Left "Cannot deserialize privileged message type StoreAddType"
                        StoreDerivationType -> Left "Cannot deserialize privileged message type StoreDerivationType"
                        GCType           -> Left "Cannot deserialize privileged message type GCType"

                        -- All other messages are considered unprivileged for this example
                        _                -> Right (BuilderMessage messageType content)

                "builder" -> do
                    -- Parse builder message (unprivileged - can be processed by both tiers)
                    msgTypeStr <- case KeyMap.lookup "messageType" obj of
                        Just (Aeson.String t) -> Right t
                        _ -> Left "Missing or invalid messageType"

                    content <- case KeyMap.lookup "content" obj of
                        Just c -> case Aeson.fromJSON c of
                            Aeson.Success content -> Right content
                            Aeson.Error err -> Left $ "Invalid content: " <> T.pack err
                        _ -> Left "Missing message content"

                    let messageType = read (T.unpack msgTypeStr) :: MessageType
                    Right (BuilderMessage messageType content)

                "auth" -> do
                    -- Parse auth message
                    content <- case KeyMap.lookup "content" obj of
                        Just c -> case Aeson.fromJSON c of
                            Aeson.Success content -> Right content
                            Aeson.Error err -> Left $ "Invalid auth content: " <> T.pack err
                        _ -> Left "Missing auth content"

                    Right (AuthMessage content)

                "error" -> do
                    -- Parse error message
                    errObj <- case KeyMap.lookup "error" obj of
                        Just e -> Right e
                        _ -> Left "Missing error content"

                    err <- parseError errObj
                    Right (ErrorMessage err)

                _ -> Left $ "Unknown message type: " <> msgType
        _ -> Left "Expected JSON object"
  where
    parseError :: Aeson.Value -> Either Text BuildError
    parseError = \val -> case Aeson.parseEither parseErrorObj val of
        Left err -> Left $ "Failed to parse error: " <> T.pack err
        Right buildErr -> Right buildErr

    parseErrorObj :: Aeson.Value -> Aeson.Parser BuildError
    parseErrorObj = Aeson.withObject "Error" $ \e -> do
        errType <- e .: "errorType" :: Aeson.Parser Text
        msg <- e .: "message" :: Aeson.Parser Text
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
            _ -> return $ BuildFailed $ "Unknown error: " <> msg

-- | Serialize a request
serializeRequest :: RequestContent -> BS.ByteString
serializeRequest = LBS.toStrict . Aeson.encode

-- | Deserialize a request
deserializeRequest :: BS.ByteString -> Either Text RequestContent
deserializeRequest bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "Failed to deserialize request: " <> T.pack err
        Right req -> Right req

-- | Serialize a response
serializeResponse :: ResponseContent -> BS.ByteString
serializeResponse = LBS.toStrict . Aeson.encode

-- | Deserialize a response
deserializeResponse :: BS.ByteString -> Either Text ResponseContent
deserializeResponse bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "Failed to deserialize response: " <> T.pack err
        Right resp -> Right resp

-- | Send a response over a protocol handle
sendResponse :: ProtocolHandle -> ResponseContent -> IO ()
sendResponse handle resp = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Check privilege requirements for response
        let respPriv = responsePrivilegeRequirement resp
        let privileged = case respPriv of
                PrivilegedResponse -> True
                UnprivilegedResponse -> False

        -- Create message with appropriate privileges
        let msg = if privileged
                    then case protocolPrivilegeTier handle of
                        Daemon ->
                            -- Only daemon can send privileged responses
                            createResponsePrivileged resp
                        Builder ->
                            -- Builder can't send privileged responses, convert to error
                            createErrorResponse $ PrivilegeError "Insufficient privileges to send response"
                    else
                        -- Unprivileged response can be sent by anyone
                        createResponseUnprivileged resp

        -- Send the message
        NByte.sendAll (protocolSocket handle) msg

-- | Create a privileged response frame
createResponsePrivileged :: ResponseContent -> BS.ByteString
createResponsePrivileged resp =
    let content = Aeson.object [
            "type" .= ("response" :: Text),
            "privileged" .= True,
            "content" .= Aeson.toJSON resp
            ]
    in createResponseFrame $ LBS.toStrict $ Aeson.encode content

-- | Create an unprivileged response frame
createResponseUnprivileged :: ResponseContent -> BS.ByteString
createResponseUnprivileged resp =
    let content = Aeson.object [
            "type" .= ("response" :: Text),
            "privileged" .= False,
            "content" .= Aeson.toJSON resp
            ]
    in createResponseFrame $ LBS.toStrict $ Aeson.encode content

-- | Create an error response
createErrorResponse :: BuildError -> BS.ByteString
createErrorResponse err =
    let content = Aeson.object [
            "type" .= ("error" :: Text),
            "error" .= Aeson.object [
                "errorType" .= errorTypeString err,
                "message" .= errorMessage err
            ]
            ]
    in createResponseFrame $ LBS.toStrict $ Aeson.encode content
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
    errorTypeString _ = "unknown"

    errorMessage :: BuildError -> Text
    errorMessage = errorToText

-- | Receive a request from a protocol handle with privilege checking
receiveRequest :: ProtocolHandle -> IO (Either ProtocolError (Message t))
receiveRequest handle = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Read a message
        msgBytes <- try $ readMessage (protocolSocket handle)
        case msgBytes of
            Left (e :: SomeException) ->
                return $ Left ConnectionClosed

            Right bytes -> do
                -- Deserialize
                case deserializeMessage bytes of
                    Left err ->
                        return $ Left (ProtocolParseError err)

                    Right msg ->
                        return $ Right msg

-- | Send a request to the daemon
sendRequest :: ProtocolHandle -> MessageType -> RequestContent -> IO ()
sendRequest handle msgType content = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Determine privilege requirements
        let capabilities = requestCapabilities msgType content
        let requiresDaemon = not $ Set.null capabilities

        -- Create appropriate message based on privilege
        let msg = if requiresDaemon
                    then case protocolPrivilegeTier handle of
                        Daemon ->
                            -- Daemon can send privileged requests
                            createDaemonRequest msgType content
                        Builder ->
                            -- Builder can't send privileged requests, convert to error
                            createErrorRequest $ PrivilegeError "Insufficient privileges to send request"
                    else
                        -- Unprivileged request can be sent by anyone
                        createBuilderRequest msgType content

        -- Send the message
        NByte.sendAll (protocolSocket handle) msg

-- | Create a daemon (privileged) request
createDaemonRequest :: MessageType -> RequestContent -> BS.ByteString
createDaemonRequest msgType content =
    let jsonContent = Aeson.object [
            "type" .= ("daemon" :: Text),
            "messageType" .= show msgType,
            "content" .= Aeson.toJSON content,
            "privileged" .= True
            ]
    in createRequestFrame $ LBS.toStrict $ Aeson.encode jsonContent

-- | Create a builder (unprivileged) request
createBuilderRequest :: MessageType -> RequestContent -> BS.ByteString
createBuilderRequest msgType content =
    let jsonContent = Aeson.object [
            "type" .= ("builder" :: Text),
            "messageType" .= show msgType,
            "content" .= Aeson.toJSON content,
            "privileged" .= False
            ]
    in createRequestFrame $ LBS.toStrict $ Aeson.encode jsonContent

-- | Create an error request
createErrorRequest :: BuildError -> BS.ByteString
createErrorRequest err =
    let jsonContent = Aeson.object [
            "type" .= ("error" :: Text),
            "error" .= Aeson.object [
                "errorType" .= errorTypeString err,
                "message" .= errorMessage err
            ]
            ]
    in createRequestFrame $ LBS.toStrict $ Aeson.encode jsonContent
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
    errorTypeString _ = "unknown"

    errorMessage :: BuildError -> Text
    errorMessage = errorToText

-- | Receive a response
receiveResponse :: ProtocolHandle -> IO (Either ProtocolError ResponseContent)
receiveResponse handle = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Read response
        respBytes <- try $ readMessage (protocolSocket handle)
        case respBytes of
            Left (e :: SomeException) ->
                return $ Left ConnectionClosed

            Right bytes -> do
                -- Parse the response
                case Aeson.eitherDecodeStrict bytes of
                    Left err ->
                        return $ Left $ ProtocolParseError $ T.pack err

                    Right val -> case val of
                        Aeson.Object obj -> do
                            -- Check response type
                            case KeyMap.lookup "type" obj of
                                Just (Aeson.String typ) -> do
                                    case typ of
                                        "response" -> do
                                            -- Check privilege requirement
                                            privileged <- case KeyMap.lookup "privileged" obj of
                                                Just (Aeson.Bool p) -> return p
                                                _ -> return False

                                            -- If response is privileged, check our privilege tier
                                            if privileged && protocolPrivilegeTier handle /= Daemon
                                                then return $ Left $ PrivilegeViolation "Insufficient privileges to receive this response"
                                                else do
                                                    -- Parse content
                                                    case KeyMap.lookup "content" obj of
                                                        Just c -> case Aeson.fromJSON c of
                                                            Aeson.Success resp -> return $ Right resp
                                                            Aeson.Error err -> return $ Left $ ProtocolParseError $ "Invalid content: " <> T.pack err
                                                        _ -> return $ Left $ ProtocolParseError "Missing response content"

                                        "error" -> do
                                            -- Parse error
                                            case KeyMap.lookup "error" obj of
                                                Just e -> case parseError e of
                                                    Left err -> return $ Left $ ProtocolParseError err
                                                    Right buildErr -> return $ Right $ ErrorResponseContent buildErr
                                                _ -> return $ Left $ ProtocolParseError "Missing error content"

                                        _ -> return $ Left $ ProtocolParseError $ "Unknown response type: " <> typ
                                _ -> return $ Left $ ProtocolParseError "Missing or invalid response type"
                        _ -> return $ Left $ ProtocolParseError "Expected JSON object"
  where
    parseError :: Aeson.Value -> Either Text BuildError
    parseError val = case Aeson.parseEither parseErrorObj val of
        Left err -> Left $ "Failed to parse error: " <> T.pack err
        Right buildErr -> Right buildErr

    parseErrorObj :: Aeson.Value -> Aeson.Parser BuildError
    parseErrorObj = Aeson.withObject "Error" $ \e -> do
        errType <- e .: "errorType" :: Aeson.Parser Text
        msg <- e .: "message" :: Aeson.Parser Text
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
            _ -> return $ BuildFailed $ "Unknown error: " <> msg

-- | Read a message from a socket
readMessage :: Socket -> IO BS.ByteString
readMessage sock = do
    -- Read length prefix
    lenBytes <- NByte.recv sock 4
    when (BS.length lenBytes /= 4) $
        throwIO ConnectionClosed

    -- Decode message length
    let len = fromIntegral $
              (fromIntegral (BS.index lenBytes 0) `shiftL` 24) .|.
              (fromIntegral (BS.index lenBytes 1) `shiftL` 16) .|.
              (fromIntegral (BS.index lenBytes 2) `shiftL` 8) .|.
              (fromIntegral (BS.index lenBytes 3))

    -- Check if message is too large
    when (len > 100 * 1024 * 1024) $ -- 100 MB limit
        throwIO (MessageTooLarge (fromIntegral len))

    -- Read message body
    recvExactly sock len

-- | Read exactly n bytes from a socket
recvExactly :: Socket -> Int -> IO BS.ByteString
recvExactly sock n = go n []
  where
    go 0 chunks = return $ BS.concat $ reverse chunks
    go remaining chunks = do
        chunk <- NByte.recv sock remaining
        let chunkSize = BS.length chunk
        if chunkSize == 0
            then throwIO ConnectionClosed
            else go (remaining - chunkSize) (chunk : chunks)

-- | Execute an action with a protocol handle and clean up after
withProtocolHandle :: Socket -> PrivilegeTier -> (ProtocolHandle -> IO a) -> IO a
withProtocolHandle socket tier =
    bracket
        (createHandle socket tier)
        closeHandle

-- | Int64 type for database IDs
type Int64 = Int
