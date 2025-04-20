{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Ten.Daemon.Protocol (
    -- Protocol versions
    ProtocolVersion(..),
    currentProtocolVersion,
    compatibleVersions,

    -- Request/response types
    DaemonRequest(..),
    DaemonResponse(..),

    -- Authentication types
    UserCredentials(..),
    AuthResult(..),

    -- Build tracking
    BuildRequestInfo(..),
    BuildStatusUpdate(..),

    -- Derivation operations
    StoreDerivationRequest(..),
    RetrieveDerivationRequest(..),
    QueryDerivationRequest(..),
    DerivationInfoResponse(..),
    DerivationOutputMappingResponse(..),

    -- GC status request/response
    GCStatusRequest(..),
    GCStatusResponse(..),

    -- Serialization functions
    serializeRequest,
    deserializeRequest,
    serializeResponse,
    deserializeResponse,

    -- Protocol framing
    createRequestFrame,
    parseRequestFrame,
    createResponseFrame,
    parseResponseFrame,

    -- Socket communication
    sendRequest,
    receiveResponse,
    sendResponse,
    receiveRequest,

    -- Connection management
    ProtocolHandle,
    createHandle,
    closeHandle,
    withProtocolHandle,

    -- Utilities
    requestToText,
    responseToText,

    -- Exception types
    ProtocolError(..)
) where

import Control.Concurrent.MVar
import Control.Exception (Exception, throwIO, bracket, try, SomeException)
import Control.Monad (unless, when, foldM)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
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
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)
import Network.Socket (Socket, close)
import qualified Network.Socket.ByteString as NByte
import System.IO (Handle, IOMode(..), withFile, hClose, hFlush)
import System.IO.Error (isEOFError)
import Text.Read (readMaybe)

-- Import Ten modules
import Ten.Core (BuildId(..), BuildStatus(..), BuildError(..), StorePath(..), UserId(..), AuthToken(..))
import Ten.Derivation (Derivation, DerivationInput, DerivationOutput, hashDerivation, serializeDerivation, deserializeDerivation)

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
    deriving (Show, Eq)

instance Exception ProtocolError

-- | User authentication credentials
data UserCredentials = UserCredentials {
    username :: Text,
    token :: Text
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON UserCredentials where
    toJSON UserCredentials{..} = Aeson.object [
            "username" .= username,
            "token" .= token
        ]

instance Aeson.FromJSON UserCredentials where
    parseJSON = Aeson.withObject "UserCredentials" $ \v -> do
        username <- v .: "username"
        token <- v .: "token"
        return UserCredentials{..}

-- | Authentication result
data AuthResult
    = AuthAccepted UserId AuthToken
    | AuthRejected Text
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON AuthResult where
    toJSON (AuthAccepted (UserId uid) (AuthToken token)) = Aeson.object [
            "status" .= ("accepted" :: Text),
            "userId" .= uid,
            "token" .= token
        ]
    toJSON (AuthRejected reason) = Aeson.object [
            "status" .= ("rejected" :: Text),
            "reason" .= reason
        ]

instance Aeson.FromJSON AuthResult where
    parseJSON = Aeson.withObject "AuthResult" $ \v -> do
        status <- v .: "status" :: Aeson.Parser Text
        case status of
            "accepted" -> do
                uid <- v .: "userId"
                token <- v .: "token"
                return $ AuthAccepted (UserId uid) (AuthToken token)
            "rejected" -> do
                reason <- v .: "reason"
                return $ AuthRejected reason
            _ -> fail $ "Unknown auth status: " ++ T.unpack status

-- | Request to store a derivation
data StoreDerivationRequest = StoreDerivationRequest {
    derivationContent :: BS.ByteString,  -- Serialized derivation
    registerOutputs :: Bool              -- Whether to register outputs in the DB
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON StoreDerivationRequest where
    toJSON StoreDerivationRequest{..} = Aeson.object [
            "derivation" .= TE.decodeUtf8 derivationContent,
            "registerOutputs" .= registerOutputs
        ]

instance Aeson.FromJSON StoreDerivationRequest where
    parseJSON = Aeson.withObject "StoreDerivationRequest" $ \v -> do
        derivText <- v .: "derivation" :: Aeson.Parser Text
        registerOutputs <- v .: "registerOutputs"
        return $ StoreDerivationRequest (TE.encodeUtf8 derivText) registerOutputs

-- | Request to retrieve a derivation
data RetrieveDerivationRequest = RetrieveDerivationRequest {
    derivationPath :: StorePath,
    includeOutputs :: Bool  -- Whether to include output info in the response
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON RetrieveDerivationRequest where
    toJSON RetrieveDerivationRequest{..} = Aeson.object [
            "path" .= encodePath derivationPath,
            "includeOutputs" .= includeOutputs
        ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

instance Aeson.FromJSON RetrieveDerivationRequest where
    parseJSON = Aeson.withObject "RetrieveDerivationRequest" $ \v -> do
        pathObj <- v .: "path"
        path <- decodePath pathObj
        includeOutputs <- v .: "includeOutputs"
        return $ RetrieveDerivationRequest path includeOutputs
      where
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

-- | Request to query derivation information
data QueryDerivationRequest = QueryDerivationRequest {
    queryType :: Text,        -- "by-hash", "by-output", "by-name", etc.
    queryValue :: Text,       -- Hash, output path, or name to search for
    queryLimit :: Maybe Int   -- Optional limit on number of results
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON QueryDerivationRequest where
    toJSON QueryDerivationRequest{..} = Aeson.object [
            "queryType" .= queryType,
            "queryValue" .= queryValue,
            "queryLimit" .= queryLimit
        ]

instance Aeson.FromJSON QueryDerivationRequest where
    parseJSON = Aeson.withObject "QueryDerivationRequest" $ \v -> do
        queryType <- v .: "queryType"
        queryValue <- v .: "queryValue"
        queryLimit <- v .: "queryLimit"
        return $ QueryDerivationRequest queryType queryValue queryLimit

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
            "derivation" .= TE.decodeUtf8 (serializeDerivation derivationResponseDrv),
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
        derivation <- case deserializeDerivation (TE.encodeUtf8 derivText) of
            Left err -> fail $ "Invalid derivation: " ++ T.unpack err
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
    buildArgs :: [(Text, Text)],  -- Extra build arguments
    buildTimeout :: Maybe Int,    -- Build timeout in seconds
    buildPriority :: Int,         -- Build priority (higher = more important)
    buildNotifyURL :: Maybe Text, -- URL to notify on completion (e.g., webhook)
    buildAllowRecursive :: Bool   -- Whether build is allowed to use return-continuation
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON BuildRequestInfo where
    toJSON BuildRequestInfo{..} = Aeson.object [
            "args" .= buildArgs,
            "timeout" .= buildTimeout,
            "priority" .= buildPriority,
            "notifyURL" .= buildNotifyURL,
            "allowRecursive" .= buildAllowRecursive
        ]

instance Aeson.FromJSON BuildRequestInfo where
    parseJSON = Aeson.withObject "BuildRequestInfo" $ \v -> do
        buildArgs <- v .: "args"
        buildTimeout <- v .: "timeout"
        buildPriority <- v .: "priority"
        buildNotifyURL <- v .: "notifyURL"
        buildAllowRecursive <- v .: "allowRecursive"
        return BuildRequestInfo{..}

-- | Default build request info
defaultBuildRequestInfo :: BuildRequestInfo
defaultBuildRequestInfo = BuildRequestInfo {
    buildArgs = [],
    buildTimeout = Nothing,
    buildPriority = 50,  -- Medium priority
    buildNotifyURL = Nothing,
    buildAllowRecursive = True
}

-- | Parse a BuildId from Text
parseBuildId :: Text -> Either Text BuildId
parseBuildId txt =
    case readMaybe (T.unpack txt) of
        Just buildId -> Right buildId
        Nothing -> Left $ "Invalid BuildId format: " <> txt

-- | Render a BuildId to Text
renderBuildId :: BuildId -> Text
renderBuildId = T.pack . show

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

-- | GC Status Request
data GCStatusRequest = GCStatusRequest {
    forceCheck :: Bool  -- Whether to force recheck the lock file
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON GCStatusRequest where
    toJSON GCStatusRequest{..} = Aeson.object [
            "forceCheck" .= forceCheck
        ]

instance Aeson.FromJSON GCStatusRequest where
    parseJSON = Aeson.withObject "GCStatusRequest" $ \v -> do
        forceCheck <- v .: "forceCheck"
        return GCStatusRequest{..}

-- | GC Status Response
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

-- | Daemon request types
data DaemonRequest
    -- Authentication
    = AuthRequest ProtocolVersion UserCredentials

    -- Build operations
    | BuildRequest {
        buildFilePath :: Text,
        buildFileContent :: Maybe BS.ByteString,
        buildOptions :: BuildRequestInfo
      }
    | EvalRequest {
        evalFilePath :: Text,
        evalFileContent :: Maybe BS.ByteString,
        evalOptions :: BuildRequestInfo
      }
    | BuildDerivationRequest {
        buildDerivation :: Derivation,
        buildDerivOptions :: BuildRequestInfo
      }
    | BuildStatusRequest {
        statusBuildId :: BuildId
      }
    | CancelBuildRequest {
        cancelBuildId :: BuildId
      }
    | QueryBuildOutputRequest {
        outputBuildId :: BuildId
      }
    | ListBuildsRequest {
        listLimit :: Maybe Int  -- Limit on number of builds to return
      }

    -- Store operations
    | StoreAddRequest {
        storeAddPath :: Text,
        storeAddContent :: BS.ByteString
      }
    | StoreVerifyRequest {
        storeVerifyPath :: Text
      }
    | StorePathRequest {
        storePathForFile :: Text,
        storePathContent :: BS.ByteString
      }
    | StoreListRequest

    -- Derivation operations
    | StoreDerivationRequest StoreDerivationRequest
    | RetrieveDerivationRequest RetrieveDerivationRequest
    | QueryDerivationRequest QueryDerivationRequest
    | GetDerivationForOutputRequest {
        getDerivationForPath :: Text
      }
    | ListDerivationsRequest {
        listDerivLimit :: Maybe Int  -- Limit on number of derivations to return
      }

    -- Garbage collection operations
    | GCRequest {
        gcForce :: Bool  -- Force GC (run even if unsafe)
      }
    | GCStatusRequest GCStatusRequest
    | AddGCRootRequest {
        rootPath :: StorePath,
        rootName :: Text,
        rootPermanent :: Bool
      }
    | RemoveGCRootRequest {
        rootNameToRemove :: Text
      }
    | ListGCRootsRequest

    -- Daemon management operations
    | PingRequest
    | ShutdownRequest
    | StatusRequest
    | ConfigRequest
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON DaemonRequest where
    toJSON req = case req of
        AuthRequest ver creds -> Aeson.object [
                "type" .= ("auth" :: Text),
                "version" .= ver,
                "credentials" .= creds
            ]

        BuildRequest{..} -> Aeson.object [
                "type" .= ("build" :: Text),
                "path" .= buildFilePath,
                "content" .= maybe Aeson.Null (\c -> Aeson.String (TE.decodeUtf8 c)) buildFileContent,
                "options" .= buildOptions
            ]

        EvalRequest{..} -> Aeson.object [
                "type" .= ("eval" :: Text),
                "path" .= evalFilePath,
                "content" .= maybe Aeson.Null (\c -> Aeson.String (TE.decodeUtf8 c)) evalFileContent,
                "options" .= evalOptions
            ]

        BuildDerivationRequest{..} -> Aeson.object [
                "type" .= ("build-derivation" :: Text),
                "derivation" .= TE.decodeUtf8 (serializeDerivation buildDerivation),
                "options" .= buildDerivOptions
            ]

        BuildStatusRequest{..} -> Aeson.object [
                "type" .= ("build-status" :: Text),
                "buildId" .= renderBuildId statusBuildId
            ]

        CancelBuildRequest{..} -> Aeson.object [
                "type" .= ("cancel-build" :: Text),
                "buildId" .= renderBuildId cancelBuildId
            ]

        QueryBuildOutputRequest{..} -> Aeson.object [
                "type" .= ("build-output" :: Text),
                "buildId" .= renderBuildId outputBuildId
            ]

        ListBuildsRequest{..} -> Aeson.object [
                "type" .= ("list-builds" :: Text),
                "limit" .= listLimit
            ]

        StoreAddRequest{..} -> Aeson.object [
                "type" .= ("store-add" :: Text),
                "path" .= storeAddPath,
                "content" .= TE.decodeUtf8 storeAddContent
            ]

        StoreVerifyRequest{..} -> Aeson.object [
                "type" .= ("store-verify" :: Text),
                "path" .= storeVerifyPath
            ]

        StorePathRequest{..} -> Aeson.object [
                "type" .= ("store-path" :: Text),
                "path" .= storePathForFile,
                "content" .= TE.decodeUtf8 storePathContent
            ]

        StoreListRequest -> Aeson.object [
                "type" .= ("store-list" :: Text)
            ]

        StoreDerivationRequest req -> Aeson.object [
                "type" .= ("store-derivation" :: Text),
                "request" .= req
            ]

        RetrieveDerivationRequest req -> Aeson.object [
                "type" .= ("retrieve-derivation" :: Text),
                "request" .= req
            ]

        QueryDerivationRequest req -> Aeson.object [
                "type" .= ("query-derivation" :: Text),
                "request" .= req
            ]

        GetDerivationForOutputRequest{..} -> Aeson.object [
                "type" .= ("get-derivation-for-output" :: Text),
                "path" .= getDerivationForPath
            ]

        ListDerivationsRequest{..} -> Aeson.object [
                "type" .= ("list-derivations" :: Text),
                "limit" .= listDerivLimit
            ]

        GCRequest{..} -> Aeson.object [
                "type" .= ("gc" :: Text),
                "force" .= gcForce
            ]

        GCStatusRequest req -> Aeson.object [
                "type" .= ("gc-status" :: Text),
                "request" .= req
            ]

        AddGCRootRequest{..} -> Aeson.object [
                "type" .= ("add-gc-root" :: Text),
                "path" .= encodePath rootPath,
                "name" .= rootName,
                "permanent" .= rootPermanent
            ]

        RemoveGCRootRequest{..} -> Aeson.object [
                "type" .= ("remove-gc-root" :: Text),
                "name" .= rootNameToRemove
            ]

        ListGCRootsRequest -> Aeson.object [
                "type" .= ("list-gc-roots" :: Text)
            ]

        PingRequest -> Aeson.object [
                "type" .= ("ping" :: Text)
            ]

        ShutdownRequest -> Aeson.object [
                "type" .= ("shutdown" :: Text)
            ]

        StatusRequest -> Aeson.object [
                "type" .= ("status" :: Text)
            ]

        ConfigRequest -> Aeson.object [
                "type" .= ("config" :: Text)
            ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

instance Aeson.FromJSON DaemonRequest where
    parseJSON = Aeson.withObject "DaemonRequest" $ \v -> do
        requestType <- v .: "type" :: Aeson.Parser Text
        case requestType of
            "auth" -> do
                ver <- v .: "version"
                creds <- v .: "credentials"
                return $ AuthRequest ver creds

            "build" -> do
                path <- v .: "path"
                content <- v .: "content"
                options <- v .: "options"
                let bsContent = case content of
                        Aeson.String s -> Just $ TE.encodeUtf8 s
                        _ -> Nothing
                return $ BuildRequest path bsContent options

            "eval" -> do
                path <- v .: "path"
                content <- v .: "content"
                options <- v .: "options"
                let bsContent = case content of
                        Aeson.String s -> Just $ TE.encodeUtf8 s
                        _ -> Nothing
                return $ EvalRequest path bsContent options

            "build-derivation" -> do
                derivText <- v .: "derivation" :: Aeson.Parser Text
                case deserializeDerivation (TE.encodeUtf8 derivText) of
                    Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                    Right drv -> do
                        options <- v .: "options"
                        return $ BuildDerivationRequest drv options

            "build-status" -> do
                buildIdStr <- v .: "buildId" :: Aeson.Parser Text
                case parseBuildId buildIdStr of
                    Left err -> fail $ T.unpack err
                    Right buildId -> return $ BuildStatusRequest buildId

            "cancel-build" -> do
                buildIdStr <- v .: "buildId" :: Aeson.Parser Text
                case parseBuildId buildIdStr of
                    Left err -> fail $ T.unpack err
                    Right buildId -> return $ CancelBuildRequest buildId

            "build-output" -> do
                buildIdStr <- v .: "buildId" :: Aeson.Parser Text
                case parseBuildId buildIdStr of
                    Left err -> fail $ T.unpack err
                    Right buildId -> return $ QueryBuildOutputRequest buildId

            "list-builds" -> do
                limit <- v .: "limit"
                return $ ListBuildsRequest limit

            "store-add" -> do
                path <- v .: "path"
                content <- v .: "content" :: Aeson.Parser Text
                return $ StoreAddRequest path (TE.encodeUtf8 content)

            "store-verify" -> do
                path <- v .: "path"
                return $ StoreVerifyRequest path

            "store-path" -> do
                path <- v .: "path"
                content <- v .: "content" :: Aeson.Parser Text
                return $ StorePathRequest path (TE.encodeUtf8 content)

            "store-list" -> return StoreListRequest

            "store-derivation" -> do
                req <- v .: "request"
                return $ StoreDerivationRequest req

            "retrieve-derivation" -> do
                req <- v .: "request"
                return $ RetrieveDerivationRequest req

            "query-derivation" -> do
                req <- v .: "request"
                return $ QueryDerivationRequest req

            "get-derivation-for-output" -> do
                path <- v .: "path"
                return $ GetDerivationForOutputRequest path

            "list-derivations" -> do
                limit <- v .: "limit"
                return $ ListDerivationsRequest limit

            "gc" -> do
                force <- v .: "force"
                return $ GCRequest force

            "gc-status" -> do
                req <- v .: "request"
                return $ GCStatusRequest req

            "add-gc-root" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                name <- v .: "name"
                permanent <- v .: "permanent"
                return $ AddGCRootRequest path name permanent

            "remove-gc-root" -> do
                name <- v .: "name"
                return $ RemoveGCRootRequest name

            "list-gc-roots" -> return ListGCRootsRequest

            "ping" -> return PingRequest

            "shutdown" -> return ShutdownRequest

            "status" -> return StatusRequest

            "config" -> return ConfigRequest

            _ -> fail $ "Unknown request type: " ++ T.unpack requestType
      where
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

-- | Daemon response types
data DaemonResponse
    -- Authentication responses
    = AuthResponse AuthResult

    -- Build responses
    | BuildStartedResponse BuildId
    | BuildResponse BuildResult
    | BuildStatusResponse BuildStatusUpdate
    | BuildOutputResponse Text
    | BuildListResponse [(BuildId, BuildStatus, Float)]

    -- Store responses
    | StoreAddResponse StorePath
    | StoreVerifyResponse Bool
    | StorePathResponse StorePath
    | StoreListResponse [StorePath]

    -- Derivation responses
    | DerivationResponse Derivation
    | DerivationStoredResponse StorePath
    | DerivationRetrievedResponse (Maybe Derivation)
    | DerivationQueryResponse [Derivation]
    | DerivationOutputResponse (Set StorePath)
    | DerivationListResponse [StorePath]

    -- Garbage collection responses
    | GCResponse GCStats
    | GCStatusResponse GCStatusResponse
    | GCRootAddedResponse Text
    | GCRootRemovedResponse Text
    | GCRootsListResponse [(StorePath, Text, Bool)]

    -- Daemon management responses
    | PongResponse
    | ShutdownResponse
    | StatusResponse DaemonStatus
    | ConfigResponse DaemonConfig

    -- Error response
    | ErrorResponse BuildError
    deriving (Show, Eq, Generic)

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

-- | Build result
data BuildResult = BuildResult {
    resultOutputs :: Set StorePath,
    resultExitCode :: ExitCode,
    resultLog :: Text,
    resultMetadata :: Map Text Text
} deriving (Show, Eq)

instance Aeson.ToJSON BuildResult where
    toJSON BuildResult{..} = Aeson.object [
            "outputs" .= map encodePath (Set.toList resultOutputs),
            "exitCode" .= exitCodeToJSON resultExitCode,
            "log" .= resultLog,
            "metadata" .= resultMetadata
        ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]
        exitCodeToJSON ExitSuccess = Aeson.object [
                "type" .= ("success" :: Text),
                "code" .= (0 :: Int)
            ]
        exitCodeToJSON (ExitFailure code) = Aeson.object [
                "type" .= ("failure" :: Text),
                "code" .= code
            ]

instance Aeson.FromJSON BuildResult where
    parseJSON = Aeson.withObject "BuildResult" $ \v -> do
        outputsJson <- v .: "outputs"
        outputs <- mapM decodePath outputsJson
        exitCodeJson <- v .: "exitCode"
        exitCode <- decodeExitCode exitCodeJson
        resultLog <- v .: "log"
        resultMetadata <- v .: "metadata"
        return BuildResult {
            resultOutputs = Set.fromList outputs,
            resultExitCode = exitCode,
            resultLog = resultLog,
            resultMetadata = resultMetadata
        }
      where
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name
        decodeExitCode = Aeson.withObject "ExitCode" $ \e -> do
            exitType <- e .: "type" :: Aeson.Parser Text
            case exitType of
                "success" -> return ExitSuccess
                "failure" -> do
                    code <- e .: "code"
                    return $ ExitFailure code
                _ -> fail $ "Unknown exit code type: " ++ T.unpack exitType

-- | Daemon configuration
data DaemonConfig = DaemonConfig {
    daemonVersion :: Text,
    daemonSocketPath :: FilePath,
    daemonStorePath :: FilePath,
    daemonMaxJobs :: Int,
    daemonGcInterval :: Maybe Int,
    daemonAllowedUsers :: [Text]
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DaemonConfig where
    toJSON DaemonConfig{..} = Aeson.object [
            "version" .= daemonVersion,
            "socketPath" .= daemonSocketPath,
            "storePath" .= daemonStorePath,
            "maxJobs" .= daemonMaxJobs,
            "gcInterval" .= daemonGcInterval,
            "allowedUsers" .= daemonAllowedUsers
        ]

instance Aeson.FromJSON DaemonConfig where
    parseJSON = Aeson.withObject "DaemonConfig" $ \v -> do
        daemonVersion <- v .: "version"
        daemonSocketPath <- v .: "socketPath"
        daemonStorePath <- v .: "storePath"
        daemonMaxJobs <- v .: "maxJobs"
        daemonGcInterval <- v .: "gcInterval"
        daemonAllowedUsers <- v .: "allowedUsers"
        return DaemonConfig{..}

instance Aeson.ToJSON DaemonResponse where
    toJSON resp = case resp of
        AuthResponse result -> Aeson.object [
                "type" .= ("auth" :: Text),
                "result" .= result
            ]

        BuildStartedResponse buildId -> Aeson.object [
                "type" .= ("build-started" :: Text),
                "buildId" .= renderBuildId buildId
            ]

        BuildResponse result -> Aeson.object [
                "type" .= ("build-result" :: Text),
                "result" .= result
            ]

        BuildStatusResponse update -> Aeson.object [
                "type" .= ("build-status" :: Text),
                "update" .= update
            ]

        BuildOutputResponse output -> Aeson.object [
                "type" .= ("build-output" :: Text),
                "output" .= output
            ]

        BuildListResponse builds -> Aeson.object [
                "type" .= ("build-list" :: Text),
                "builds" .= map (\(bid, stat, prog) -> Aeson.object [
                        "id" .= renderBuildId bid,
                        "status" .= showStatus stat,
                        "progress" .= prog
                    ]) builds
            ]

        StoreAddResponse path -> Aeson.object [
                "type" .= ("store-add" :: Text),
                "path" .= encodePath path
            ]

        StoreVerifyResponse valid -> Aeson.object [
                "type" .= ("store-verify" :: Text),
                "valid" .= valid
            ]

        StorePathResponse path -> Aeson.object [
                "type" .= ("store-path" :: Text),
                "path" .= encodePath path
            ]

        StoreListResponse paths -> Aeson.object [
                "type" .= ("store-list" :: Text),
                "paths" .= map encodePath paths
            ]

        DerivationResponse drv -> Aeson.object [
                "type" .= ("derivation" :: Text),
                "derivation" .= TE.decodeUtf8 (serializeDerivation drv)
            ]

        DerivationStoredResponse path -> Aeson.object [
                "type" .= ("derivation-stored" :: Text),
                "path" .= encodePath path
            ]

        DerivationRetrievedResponse mDrv -> Aeson.object [
                "type" .= ("derivation-retrieved" :: Text),
                "derivation" .= maybe Aeson.Null
                                    (\drv -> Aeson.String $ TE.decodeUtf8 $ serializeDerivation drv)
                                    mDrv
            ]

        DerivationQueryResponse drvs -> Aeson.object [
                "type" .= ("derivation-query" :: Text),
                "derivations" .= map (\drv -> TE.decodeUtf8 $ serializeDerivation drv) drvs
            ]

        DerivationOutputResponse paths -> Aeson.object [
                "type" .= ("derivation-outputs" :: Text),
                "outputs" .= map encodePath (Set.toList paths)
            ]

        DerivationListResponse paths -> Aeson.object [
                "type" .= ("derivation-list" :: Text),
                "paths" .= map encodePath paths
            ]

        GCResponse stats -> Aeson.object [
                "type" .= ("gc" :: Text),
                "stats" .= gcStatsToJSON stats
            ]

        GCStatusResponse status -> Aeson.object [
                "type" .= ("gc-status" :: Text),
                "status" .= status
            ]

        GCRootAddedResponse name -> Aeson.object [
                "type" .= ("gc-root-added" :: Text),
                "name" .= name
            ]

        GCRootRemovedResponse name -> Aeson.object [
                "type" .= ("gc-root-removed" :: Text),
                "name" .= name
            ]

        GCRootsListResponse roots -> Aeson.object [
                "type" .= ("gc-roots-list" :: Text),
                "roots" .= map (\(path, name, perm) -> Aeson.object [
                        "path" .= encodePath path,
                        "name" .= name,
                        "permanent" .= perm
                    ]) roots
            ]

        PongResponse -> Aeson.object [
                "type" .= ("pong" :: Text)
            ]

        ShutdownResponse -> Aeson.object [
                "type" .= ("shutdown" :: Text)
            ]

        StatusResponse status -> Aeson.object [
                "type" .= ("status" :: Text),
                "status" .= status
            ]

        ConfigResponse config -> Aeson.object [
                "type" .= ("config" :: Text),
                "config" .= config
            ]

        ErrorResponse err -> Aeson.object [
                "type" .= ("error" :: Text),
                "error" .= errorToJSON err
            ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

        showStatus :: BuildStatus -> Text
        showStatus BuildPending = "pending"
        showStatus (BuildRunning _) = "running"
        showStatus (BuildRecursing _) = "recursing"
        showStatus BuildCompleted = "completed"
        showStatus BuildFailed' = "failed"

        gcStatsToJSON :: GCStats -> Aeson.Value
        gcStatsToJSON stats = Aeson.object [
                "total" .= gcTotal stats,
                "live" .= gcLive stats,
                "collected" .= gcCollected stats,
                "bytes" .= gcBytes stats,
                "elapsedTime" .= gcElapsedTime stats
            ]

        errorToJSON :: BuildError -> Aeson.Value
        errorToJSON err = Aeson.object [
                "type" .= errorType err,
                "message" .= errorMessage err
            ]

        errorType :: BuildError -> Text
        errorType (EvalError _) = "eval"
        errorType (BuildFailed _) = "build"
        errorType (StoreError _) = "store"
        errorType (SandboxError _) = "sandbox"
        errorType (InputNotFound _) = "input"
        errorType (HashError _) = "hash"
        errorType (GraphError _) = "graph"
        errorType (ResourceError _) = "resource"
        errorType (DaemonError _) = "daemon"
        errorType (AuthError _) = "auth"
        errorType (CyclicDependency _) = "cycle"
        errorType (SerializationError _) = "serialization"
        errorType (RecursionLimit _) = "recursion"
        errorType (NetworkError _) = "network"
        errorType _ = "unknown"

        errorMessage :: BuildError -> Text
        errorMessage (EvalError msg) = msg
        errorMessage (BuildFailed msg) = msg
        errorMessage (StoreError msg) = msg
        errorMessage (SandboxError msg) = msg
        errorMessage (InputNotFound path) = T.pack path
        errorMessage (HashError msg) = msg
        errorMessage (GraphError msg) = msg
        errorMessage (ResourceError msg) = msg
        errorMessage (DaemonError msg) = msg
        errorMessage (AuthError msg) = msg
        errorMessage (CyclicDependency msg) = msg
        errorMessage (SerializationError msg) = msg
        errorMessage (RecursionLimit msg) = msg
        errorMessage (NetworkError msg) = msg
        errorMessage _ = "Unknown error"

instance Aeson.FromJSON DaemonResponse where
    parseJSON = Aeson.withObject "DaemonResponse" $ \v -> do
        responseType <- v .: "type" :: Aeson.Parser Text
        case responseType of
            "auth" -> do
                result <- v .: "result"
                return $ AuthResponse result

            "build-started" -> do
                buildIdStr <- v .: "buildId" :: Aeson.Parser Text
                case parseBuildId buildIdStr of
                    Left err -> fail $ T.unpack err
                    Right buildId -> return $ BuildStartedResponse buildId

            "build-result" -> do
                result <- v .: "result"
                return $ BuildResponse result

            "build-status" -> do
                update <- v .: "update"
                return $ BuildStatusResponse update

            "build-output" -> do
                output <- v .: "output"
                return $ BuildOutputResponse output

            "build-list" -> do
                buildsJson <- v .: "builds"
                builds <- mapM (\obj -> do
                    idStr <- obj .: "id" :: Aeson.Parser Text
                    statusStr <- obj .: "status" :: Aeson.Parser Text
                    progress <- obj .: "progress" :: Aeson.Parser Float
                    case parseBuildId idStr of
                        Left err -> fail $ T.unpack err
                        Right buildId -> do
                            let status = case statusStr of
                                    "pending" -> BuildPending
                                    "running" -> BuildRunning progress
                                    "recursing" -> BuildRecursing (BuildIdFromInt 0)  -- Temporary ID
                                    "completed" -> BuildCompleted
                                    "failed" -> BuildFailed'
                                    _ -> BuildPending  -- Default
                            return (buildId, status, progress)
                    ) buildsJson
                return $ BuildListResponse builds

            "store-add" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                return $ StoreAddResponse path

            "store-verify" -> do
                valid <- v .: "valid"
                return $ StoreVerifyResponse valid

            "store-path" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                return $ StorePathResponse path

            "store-list" -> do
                pathsJson <- v .: "paths"
                paths <- mapM decodePath pathsJson
                return $ StoreListResponse paths

            "derivation" -> do
                derivText <- v .: "derivation" :: Aeson.Parser Text
                case deserializeDerivation (TE.encodeUtf8 derivText) of
                    Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                    Right drv -> return $ DerivationResponse drv

            "derivation-stored" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                return $ DerivationStoredResponse path

            "derivation-retrieved" -> do
                derivValue <- v .: "derivation"
                case derivValue of
                    Aeson.Null -> return $ DerivationRetrievedResponse Nothing
                    Aeson.String derivText ->
                        case deserializeDerivation (TE.encodeUtf8 derivText) of
                            Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                            Right drv -> return $ DerivationRetrievedResponse (Just drv)
                    _ -> fail "Invalid derivation value"

            "derivation-query" -> do
                derivTexts <- v .: "derivations" :: Aeson.Parser [Text]
                drvs <- mapM (\derivText ->
                    case deserializeDerivation (TE.encodeUtf8 derivText) of
                        Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                        Right drv -> return drv
                    ) derivTexts
                return $ DerivationQueryResponse drvs

            "derivation-outputs" -> do
                outputsJson <- v .: "outputs"
                outputs <- mapM decodePath outputsJson
                return $ DerivationOutputResponse (Set.fromList outputs)

            "derivation-list" -> do
                pathsJson <- v .: "paths"
                paths <- mapM decodePath pathsJson
                return $ DerivationListResponse paths

            "gc" -> do
                statsJson <- v .: "stats"
                stats <- parseGCStats statsJson
                return $ GCResponse stats

            "gc-status" -> do
                status <- v .: "status"
                return $ GCStatusResponse status

            "gc-root-added" -> do
                name <- v .: "name"
                return $ GCRootAddedResponse name

            "gc-root-removed" -> do
                name <- v .: "name"
                return $ GCRootRemovedResponse name

            "gc-roots-list" -> do
                rootsJson <- v .: "roots"
                roots <- mapM (\obj -> do
                    pathObj <- obj .: "path"
                    path <- decodePath pathObj
                    name <- obj .: "name"
                    perm <- obj .: "permanent"
                    return (path, name, perm)
                    ) rootsJson
                return $ GCRootsListResponse roots

            "pong" -> return PongResponse

            "shutdown" -> return ShutdownResponse

            "status" -> do
                status <- v .: "status"
                return $ StatusResponse status

            "config" -> do
                config <- v .: "config"
                return $ ConfigResponse config

            "error" -> do
                errorJson <- v .: "error"
                err <- parseError errorJson
                return $ ErrorResponse err

            _ -> fail $ "Unknown response type: " ++ T.unpack responseType
      where
        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

        parseGCStats = Aeson.withObject "GCStats" $ \s -> do
            total <- s .: "total"
            live <- s .: "live"
            collected <- s .: "collected"
            bytes <- s .: "bytes"
            elapsedTime <- s .: "elapsedTime"
            return $ GCStats total live collected bytes elapsedTime

        parseError = Aeson.withObject "BuildError" $ \e -> do
            errorType <- e .: "type" :: Aeson.Parser Text
            message <- e .: "message"
            case errorType of
                "eval" -> return $ EvalError message
                "build" -> return $ BuildFailed message
                "store" -> return $ StoreError message
                "sandbox" -> return $ SandboxError message
                "input" -> return $ InputNotFound (T.unpack message)
                "hash" -> return $ HashError message
                "graph" -> return $ GraphError message
                "resource" -> return $ ResourceError message
                "daemon" -> return $ DaemonError message
                "auth" -> return $ AuthError message
                "cycle" -> return $ CyclicDependency message
                "serialization" -> return $ SerializationError message
                "recursion" -> return $ RecursionLimit message
                "network" -> return $ NetworkError message
                _ -> return $ BuildFailed $ "Unknown error: " <> message

-- | Protocol message type
data Message
    = AuthRequestMsg AuthRequest
    | AuthResponseMsg AuthResult
    | RequestMsg Int DaemonRequest
    | ResponseMsg Int DaemonResponse
    deriving (Show, Eq)

instance Aeson.ToJSON Message where
    toJSON (AuthRequestMsg req) = Aeson.object [
            "messageType" .= ("authRequest" :: Text),
            "request" .= req
        ]
    toJSON (AuthResponseMsg resp) = Aeson.object [
            "messageType" .= ("authResponse" :: Text),
            "response" .= resp
        ]
    toJSON (RequestMsg reqId req) = Aeson.object [
            "messageType" .= ("request" :: Text),
            "requestId" .= reqId,
            "request" .= req
        ]
    toJSON (ResponseMsg reqId resp) = Aeson.object [
            "messageType" .= ("response" :: Text),
            "requestId" .= reqId,
            "response" .= resp
        ]

instance Aeson.FromJSON Message where
    parseJSON = Aeson.withObject "Message" $ \v -> do
        messageType <- v .: "messageType" :: Aeson.Parser Text
        case messageType of
            "authRequest" -> do
                req <- v .: "request"
                return $ AuthRequestMsg req
            "authResponse" -> do
                resp <- v .: "response"
                return $ AuthResponseMsg resp
            "request" -> do
                reqId <- v .: "requestId"
                req <- v .: "request"
                return $ RequestMsg reqId req
            "response" -> do
                reqId <- v .: "requestId"
                resp <- v .: "response"
                return $ ResponseMsg reqId resp
            _ -> fail $ "Unknown message type: " ++ T.unpack messageType

-- | Serialize a message with length prefix
serializeMessage :: Message -> BS.ByteString
serializeMessage msg =
    let body = LBS.toStrict $ Aeson.encode msg
        len = fromIntegral $ BS.length body
        lenBytes = LBS.toStrict $ Builder.toLazyByteString $ Builder.word32BE len
    in lenBytes <> body

-- | Deserialize a message
deserializeMessage :: BS.ByteString -> Either Text Message
deserializeMessage bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "Failed to parse message: " <> T.pack err
        Right msg -> Right msg

-- | Protocol handle type for managing connections
data ProtocolHandle = ProtocolHandle {
    protocolSocket :: Socket,
    protocolLock :: MVar ()  -- For thread safety
}

-- | Create a protocol handle from a socket
createHandle :: Socket -> IO ProtocolHandle
createHandle sock = do
    lock <- newMVar ()
    return $ ProtocolHandle sock lock

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
        let len = fromIntegral $
                  (fromIntegral (BS.index lenBytes 0) `shiftL` 24) .|.
                  (fromIntegral (BS.index lenBytes 1) `shiftL` 16) .|.
                  (fromIntegral (BS.index lenBytes 2) `shiftL` 8) .|.
                  (fromIntegral (BS.index lenBytes 3))

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

-- | Send a request over a protocol handle
sendRequest :: ProtocolHandle -> DaemonRequest -> IO Int
sendRequest handle req = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Generate request ID
        reqId <- (`mod` 1000000) <$> getCurrentMillis

        -- Create message with request ID
        let msg = RequestMsg reqId req

        -- Send message
        let serialized = serializeMessage msg
        NByte.sendAll (protocolSocket handle) serialized

        -- Return request ID for tracking
        return reqId

-- | Receive a response from a protocol handle
receiveResponse :: ProtocolHandle -> Int -> Int -> IO (Either ProtocolError DaemonResponse)
receiveResponse handle reqId timeoutMicros = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Read a message
        msgBytes <- try $ readMessageWithTimeout (protocolSocket handle) timeoutMicros
        case msgBytes of
            Left (e :: SomeException) ->
                return $ Left $ ConnectionClosed

            Right bytes -> case deserializeMessage bytes of
                Left err ->
                    return $ Left $ ProtocolParseError err

                Right (ResponseMsg respId resp) ->
                    if respId == reqId
                        then return $ Right resp
                        else return $ Left $ InvalidRequest $ "Response ID mismatch: expected " <> T.pack (show reqId) <> ", got " <> T.pack (show respId)

                Right _ ->
                    return $ Left $ InvalidRequest "Expected response message"

-- | Send a response over a protocol handle
sendResponse :: ProtocolHandle -> Int -> DaemonResponse -> IO ()
sendResponse handle reqId resp = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Create message with request ID
        let msg = ResponseMsg reqId resp

        -- Send message
        let serialized = serializeMessage msg
        NByte.sendAll (protocolSocket handle) serialized

-- | Receive a request from a protocol handle
receiveRequest :: ProtocolHandle -> IO (Either ProtocolError (Int, DaemonRequest))
receiveRequest handle = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Read a message
        msgBytes <- try $ readMessage (protocolSocket handle)
        case msgBytes of
            Left (e :: SomeException) ->
                return $ Left ConnectionClosed

            Right bytes -> case deserializeMessage bytes of
                Left err ->
                    return $ Left $ ProtocolParseError err

                Right (RequestMsg reqId req) ->
                    return $ Right (reqId, req)

                Right _ ->
                    return $ Left $ InvalidRequest "Expected request message"

-- | Execute an action with a protocol handle and clean up after
withProtocolHandle :: Socket -> (ProtocolHandle -> IO a) -> IO a
withProtocolHandle socket action =
    bracket
        (createHandle socket)
        closeHandle
        action

-- | Read a message from a socket with timeout
readMessageWithTimeout :: Socket -> Int -> IO BS.ByteString
readMessageWithTimeout sock timeoutMicros = do
    -- Read length header with timeout
    lenBytes <- timeout timeoutMicros $ NByte.recv sock 4
    case lenBytes of
        Nothing -> throwIO $ ProtocolError $ MessageTooLarge 0
        Just bytes
            | BS.length bytes /= 4 -> throwIO $ ProtocolError ConnectionClosed
            | otherwise -> do
                -- Decode message length
                let len = fromIntegral $
                          (fromIntegral (BS.index bytes 0) `shiftL` 24) .|.
                          (fromIntegral (BS.index bytes 1) `shiftL` 16) .|.
                          (fromIntegral (BS.index bytes 2) `shiftL` 8) .|.
                          (fromIntegral (BS.index bytes 3))

                -- Check if message is too large
                when (len > 100 * 1024 * 1024) $ -- 100 MB limit
                    throwIO $ ProtocolError $ MessageTooLarge (fromIntegral len)

                -- Read message body with timeout
                body <- timeout timeoutMicros $ recvExactly sock len
                case body of
                    Nothing -> throwIO $ ProtocolError ConnectionClosed
                    Just bytes
                        | BS.length bytes /= len -> throwIO $ ProtocolError ConnectionClosed
                        | otherwise -> return bytes

-- | Read a message from a socket
readMessage :: Socket -> IO BS.ByteString
readMessage sock = do
    -- Read length header
    lenBytes <- NByte.recv sock 4
    when (BS.length lenBytes /= 4) $
        throwIO $ ProtocolError ConnectionClosed

    -- Decode message length
    let len = fromIntegral $
              (fromIntegral (BS.index lenBytes 0) `shiftL` 24) .|.
              (fromIntegral (BS.index lenBytes 1) `shiftL` 16) .|.
              (fromIntegral (BS.index lenBytes 2) `shiftL` 8) .|.
              (fromIntegral (BS.index lenBytes 3))

    -- Check if message is too large
    when (len > 100 * 1024 * 1024) $ -- 100 MB limit
        throwIO $ ProtocolError $ MessageTooLarge (fromIntegral len)

    -- Read message body
    body <- recvExactly sock len
    when (BS.length body /= len) $
        throwIO $ ProtocolError ConnectionClosed

    return body

-- | Read exactly n bytes from a socket
recvExactly :: Socket -> Int -> IO BS.ByteString
recvExactly sock n = go n []
  where
    go 0 chunks = return $ BS.concat $ reverse chunks
    go remaining chunks = do
        chunk <- NByte.recv sock remaining
        let chunkSize = BS.length chunk
        if chunkSize == 0
            then return $ BS.concat $ reverse chunks  -- Connection closed
            else go (remaining - chunkSize) (chunk : chunks)

-- | Convert a request to human-readable text
requestToText :: DaemonRequest -> Text
requestToText req = case req of
    AuthRequest ver _ ->
        T.pack $ "Auth request (protocol version " ++ show ver ++ ")"

    BuildRequest{..} ->
        "Build file: " <> buildFilePath

    EvalRequest{..} ->
        "Evaluate file: " <> evalFilePath

    BuildDerivationRequest{..} ->
        "Build derivation: " <> T.take 40 (hashDerivation buildDerivation) <> "..."

    BuildStatusRequest{..} ->
        "Query build status: " <> renderBuildId statusBuildId

    CancelBuildRequest{..} ->
        "Cancel build: " <> renderBuildId cancelBuildId

    QueryBuildOutputRequest{..} ->
        "Query build output: " <> renderBuildId outputBuildId

    ListBuildsRequest{..} ->
        "List builds" <> maybe "" (\n -> " (limit: " <> T.pack (show n) <> ")") listLimit

    StoreAddRequest{..} ->
        "Add to store: " <> storeAddPath

    StoreVerifyRequest{..} ->
        "Verify path: " <> storeVerifyPath

    StorePathRequest{..} ->
        "Get store path for: " <> storePathForFile

    StoreListRequest ->
        "List store contents"

    StoreDerivationRequest _ ->
        "Store derivation"

    RetrieveDerivationRequest _ ->
        "Retrieve derivation"

    QueryDerivationRequest _ ->
        "Query derivation"

    GetDerivationForOutputRequest{..} ->
        "Get derivation for output: " <> getDerivationForPath

    ListDerivationsRequest{..} ->
        "List derivations" <> maybe "" (\n -> " (limit: " <> T.pack (show n) <> ")") listDerivLimit

    GCRequest{..} ->
        "Collect garbage" <> if gcForce then " (force)" else ""

    GCStatusRequest req ->
        "Check GC status" <> if forceCheck req then " (force check)" else ""

    AddGCRootRequest{..} ->
        "Add GC root: " <> rootName <> " -> " <> storeHash rootPath <> "-" <> storeName rootPath

    RemoveGCRootRequest{..} ->
        "Remove GC root: " <> rootNameToRemove

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
responseToText resp = case resp of
    AuthResponse (AuthAccepted uid _) ->
        "Auth accepted: " <> case uid of UserId u -> u

    AuthResponse (AuthRejected reason) ->
        "Auth rejected: " <> reason

    BuildStartedResponse buildId ->
        "Build started: " <> renderBuildId buildId

    BuildResponse _ ->
        "Build completed"

    BuildStatusResponse update ->
        "Build status: " <> showStatus (buildStatus update)

    BuildOutputResponse _ ->
        "Build output"

    BuildListResponse builds ->
        "Build list: " <> T.pack (show (length builds)) <> " builds"

    StoreAddResponse path ->
        "Added to store: " <> storeHash path <> "-" <> storeName path

    StoreVerifyResponse valid ->
        "Path verification: " <> if valid then "valid" else "invalid"

    StorePathResponse path ->
        "Store path: " <> storeHash path <> "-" <> storeName path

    StoreListResponse paths ->
        "Store contents: " <> T.pack (show (length paths)) <> " paths"

    DerivationResponse _ ->
        "Derivation"

    DerivationStoredResponse path ->
        "Derivation stored: " <> storeHash path <> "-" <> storeName path

    DerivationRetrievedResponse Nothing ->
        "Derivation not found"

    DerivationRetrievedResponse (Just _) ->
        "Derivation retrieved"

    DerivationQueryResponse drvs ->
        "Derivation query results: " <> T.pack (show (length drvs)) <> " derivations"

    DerivationOutputResponse paths ->
        "Derivation outputs: " <> T.pack (show (Set.size paths)) <> " paths"

    DerivationListResponse paths ->
        "Derivation list: " <> T.pack (show (length paths)) <> " derivations"

    GCResponse stats ->
        "GC completed: " <> T.pack (show (gcCollected stats)) <> " paths collected"

    GCStatusResponse status ->
        "GC status: " <> if gcRunning status then "running" else "not running" <>
        maybe "" (\owner -> " (owned by " <> owner <> ")") (gcOwner status)

    GCRootAddedResponse name ->
        "GC root added: " <> name

    GCRootRemovedResponse name ->
        "GC root removed: " <> name

    GCRootsListResponse roots ->
        "GC roots: " <> T.pack (show (length roots)) <> " roots"

    PongResponse ->
        "Pong"

    ShutdownResponse ->
        "Shutdown acknowledged"

    StatusResponse _ ->
        "Daemon status"

    ConfigResponse _ ->
        "Daemon configuration"

    ErrorResponse err ->
        "Error: " <> errorToText err
  where
    showStatus :: BuildStatus -> Text
    showStatus BuildPending = "pending"
    showStatus (BuildRunning progress) = "running (" <> T.pack (show (round (progress * 100))) <> "%)"
    showStatus (BuildRecursing innerBuildId) = "recursing to " <> renderBuildId innerBuildId
    showStatus BuildCompleted = "completed"
    showStatus BuildFailed' = "failed"

    errorToText :: BuildError -> Text
    errorToText (EvalError msg) = "Evaluation error: " <> msg
    errorToText (BuildFailed msg) = "Build failed: " <> msg
    errorToText (StoreError msg) = "Store error: " <> msg
    errorToText (SandboxError msg) = "Sandbox error: " <> msg
    errorToText (InputNotFound path) = "Input not found: " <> T.pack path
    errorToText (HashError msg) = "Hash error: " <> msg
    errorToText (GraphError msg) = "Graph error: " <> msg
    errorToText (ResourceError msg) = "Resource error: " <> msg
    errorToText (DaemonError msg) = "Daemon error: " <> msg
    errorToText (AuthError msg) = "Authentication error: " <> msg
    errorToText (CyclicDependency msg) = "Cyclic dependency: " <> msg
    errorToText (SerializationError msg) = "Serialization error: " <> msg
    errorToText (RecursionLimit msg) = "Recursion limit exceeded: " <> msg
    errorToText (NetworkError msg) = "Network error: " <> msg
    errorToText _ = "Unknown error"

-- | Serialize a request
serializeRequest :: DaemonRequest -> BS.ByteString
serializeRequest = LBS.toStrict . Aeson.encode

-- | Deserialize a request
deserializeRequest :: BS.ByteString -> Either Text DaemonRequest
deserializeRequest bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "Failed to deserialize request: " <> T.pack err
        Right req -> Right req

-- | Serialize a response
serializeResponse :: DaemonResponse -> BS.ByteString
serializeResponse = LBS.toStrict . Aeson.encode

-- | Deserialize a response
deserializeResponse :: BS.ByteString -> Either Text DaemonResponse
deserializeResponse bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ "Failed to deserialize response: " <> T.pack err
        Right resp -> Right resp

-- | Get current time in milliseconds
getCurrentMillis :: IO Int
getCurrentMillis = do
    time <- getCurrentTime
    return $ round $ 1000 * realToFrac (diffUTCTime time (read "1970-01-01 00:00:00 UTC"))

-- | Timeout implementation
timeout :: Int -> IO a -> IO (Maybe a)
timeout micros action = do
    result <- newEmptyMVar
    tid <- forkIO $ do
        r <- try action
        putMVar result r
    threadDelay micros
    filled <- isJust <$> tryTakeMVar result
    unless filled $ killThread tid
    if filled
        then do
            r <- readMVar result
            case r of
                Left (e :: SomeException) -> throwIO e
                Right x -> return (Just x)
        else return Nothing

-- | Try to take a value from an MVar without blocking
tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar mvar = do
    empty <- isEmptyMVar mvar
    if empty
        then return Nothing
        else Just <$> takeMVar mvar

-- | Check if an MVar is empty
isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar mvar = do
    full <- newEmptyMVar
    tryPutMVar full ()
    isEmpty <- isJust <$> tryTakeMVar full
    return isEmpty

-- | Try to put a value in an MVar without blocking
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar mvar x = do
    empty <- isEmptyMVar mvar
    if empty
        then do
            putMVar mvar x
            return True
        else return False

-- | Bitwise operations for message length encoding/decoding
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b
