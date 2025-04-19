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
        status <- v .: "status"
        case status of
            "accepted" -> do
                uid <- v .: "userId"
                token <- v .: "token"
                return $ AuthAccepted (UserId uid) (AuthToken token)
            "rejected" -> do
                reason <- v .: "reason"
                return $ AuthRejected reason
            _ -> fail $ "Unknown auth status: " ++ T.unpack status

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
            "buildId" .= show buildId,
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
                "innerBuildId" .= show innerBuildId
            ]
        serializeBuildStatus BuildCompleted = Aeson.object [
                "type" .= ("completed" :: Text)
            ]
        serializeBuildStatus BuildFailed' = Aeson.object [
                "type" .= ("failed" :: Text)
            ]

instance Aeson.FromJSON BuildStatusUpdate where
    parseJSON = Aeson.withObject "BuildStatusUpdate" $ \v -> do
        buildIdStr <- v .: "buildId"
        let buildId = read buildIdStr  -- Will throw if invalid
        statusObj <- v .: "status"
        buildStatus <- parseStatus statusObj
        buildTimeElapsed <- v .: "timeElapsed"
        buildTimeRemaining <- v .: "timeRemaining"
        buildLogUpdate <- v .: "logUpdate"
        buildResourceUsage <- v .: "resourceUsage"
        return BuildStatusUpdate{..}
      where
        parseStatus obj = Aeson.withObject "BuildStatus" (\o -> do
            statusType <- o .: "type"
            case statusType of
                "pending" -> return BuildPending
                "running" -> do
                    progress <- o .: "progress"
                    return $ BuildRunning progress
                "recursing" -> do
                    innerIdStr <- o .: "innerBuildId"
                    return $ BuildRecursing (read innerIdStr)
                "completed" -> return BuildCompleted
                "failed" -> return BuildFailed'
                _ -> fail $ "Unknown status type: " ++ T.unpack statusType
            ) obj

-- | Daemon request types
data DaemonRequest
    -- Authentication
    = AuthRequest ProtocolVersion UserCredentials

    -- Build operations
    | BuildDerivation Derivation BuildRequestInfo
    | BuildFile FilePath BuildRequestInfo
    | CancelBuild BuildId
    | QueryBuildStatus BuildId
    | QueryBuildOutput BuildId
    | ListBuilds (Maybe Int)  -- Limit of builds to return, Nothing = all

    -- Store operations
    | QueryPath StorePath
    | AddToStore Text BS.ByteString
    | EnsureInStore Text FilePath
    | VerifyPath StorePath
    | ListStoreContents (Maybe Text)  -- Optional prefix filter

    -- Garbage collection
    | CollectGarbage Bool  -- force flag
    | AddGCRoot StorePath Text Bool  -- path, name, permanent flag
    | RemoveGCRoot Text  -- root name
    | ListGCRoots

    -- Daemon control
    | Ping
    | Shutdown
    | QueryDaemonStatus

    -- Graph operations
    | DetectCycle [Derivation]
    | ComputeBuildGraph Derivation

    -- Return-continuation specific
    | JoinDerivation Derivation
    deriving (Show, Eq, Generic)

instance Aeson.ToJSON DaemonRequest where
    toJSON req = case req of
        AuthRequest ver creds -> Aeson.object [
                "type" .= ("auth" :: Text),
                "version" .= ver,
                "credentials" .= creds
            ]

        BuildDerivation drv info -> Aeson.object [
                "type" .= ("build-derivation" :: Text),
                "derivation" .= encodeDrv drv,
                "info" .= info
            ]

        BuildFile path info -> Aeson.object [
                "type" .= ("build-file" :: Text),
                "path" .= path,
                "info" .= info
            ]

        CancelBuild buildId -> Aeson.object [
                "type" .= ("cancel-build" :: Text),
                "buildId" .= show buildId
            ]

        QueryBuildStatus buildId -> Aeson.object [
                "type" .= ("query-build-status" :: Text),
                "buildId" .= show buildId
            ]

        QueryBuildOutput buildId -> Aeson.object [
                "type" .= ("query-build-output" :: Text),
                "buildId" .= show buildId
            ]

        ListBuilds limit -> Aeson.object [
                "type" .= ("list-builds" :: Text),
                "limit" .= limit
            ]

        QueryPath path -> Aeson.object [
                "type" .= ("query-path" :: Text),
                "path" .= encodePath path
            ]

        AddToStore name content -> Aeson.object [
                "type" .= ("add-to-store" :: Text),
                "name" .= name,
                "content" .= Aeson.String (TE.decodeUtf8 content)
            ]

        EnsureInStore name path -> Aeson.object [
                "type" .= ("ensure-in-store" :: Text),
                "name" .= name,
                "path" .= path
            ]

        VerifyPath path -> Aeson.object [
                "type" .= ("verify-path" :: Text),
                "path" .= encodePath path
            ]

        ListStoreContents prefix -> Aeson.object [
                "type" .= ("list-store" :: Text),
                "prefix" .= prefix
            ]

        CollectGarbage force -> Aeson.object [
                "type" .= ("collect-garbage" :: Text),
                "force" .= force
            ]

        AddGCRoot path name permanent -> Aeson.object [
                "type" .= ("add-gc-root" :: Text),
                "path" .= encodePath path,
                "name" .= name,
                "permanent" .= permanent
            ]

        RemoveGCRoot name -> Aeson.object [
                "type" .= ("remove-gc-root" :: Text),
                "name" .= name
            ]

        ListGCRoots -> Aeson.object [
                "type" .= ("list-gc-roots" :: Text)
            ]

        Ping -> Aeson.object [
                "type" .= ("ping" :: Text)
            ]

        Shutdown -> Aeson.object [
                "type" .= ("shutdown" :: Text)
            ]

        QueryDaemonStatus -> Aeson.object [
                "type" .= ("query-daemon-status" :: Text)
            ]

        DetectCycle drvs -> Aeson.object [
                "type" .= ("detect-cycle" :: Text),
                "derivations" .= map encodeDrv drvs
            ]

        ComputeBuildGraph drv -> Aeson.object [
                "type" .= ("compute-build-graph" :: Text),
                "derivation" .= encodeDrv drv
            ]

        JoinDerivation drv -> Aeson.object [
                "type" .= ("join-derivation" :: Text),
                "derivation" .= encodeDrv drv
            ]
      where
        encodeDrv drv = Aeson.String $ TE.decodeUtf8 $ serializeDerivation drv
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

instance Aeson.FromJSON DaemonRequest where
    parseJSON = Aeson.withObject "DaemonRequest" $ \v -> do
        requestType <- v .: "type"
        case requestType of
            "auth" -> do
                ver <- v .: "version"
                creds <- v .: "credentials"
                return $ AuthRequest ver creds

            "build-derivation" -> do
                drvText <- v .: "derivation"
                case deserializeDerivation $ TE.encodeUtf8 $ extractString drvText of
                    Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                    Right drv -> do
                        info <- v .: "info"
                        return $ BuildDerivation drv info

            "build-file" -> do
                path <- v .: "path"
                info <- v .: "info"
                return $ BuildFile path info

            "cancel-build" -> do
                buildIdStr <- v .: "buildId"
                return $ CancelBuild (read buildIdStr)

            "query-build-status" -> do
                buildIdStr <- v .: "buildId"
                return $ QueryBuildStatus (read buildIdStr)

            "query-build-output" -> do
                buildIdStr <- v .: "buildId"
                return $ QueryBuildOutput (read buildIdStr)

            "list-builds" -> do
                limit <- v .: "limit"
                return $ ListBuilds limit

            "query-path" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                return $ QueryPath path

            "add-to-store" -> do
                name <- v .: "name"
                contentText <- v .: "content"
                return $ AddToStore name (TE.encodeUtf8 $ extractString contentText)

            "ensure-in-store" -> do
                name <- v .: "name"
                path <- v .: "path"
                return $ EnsureInStore name path

            "verify-path" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                return $ VerifyPath path

            "list-store" -> do
                prefix <- v .: "prefix"
                return $ ListStoreContents prefix

            "collect-garbage" -> do
                force <- v .: "force"
                return $ CollectGarbage force

            "add-gc-root" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                name <- v .: "name"
                permanent <- v .: "permanent"
                return $ AddGCRoot path name permanent

            "remove-gc-root" -> do
                name <- v .: "name"
                return $ RemoveGCRoot name

            "list-gc-roots" -> return ListGCRoots

            "ping" -> return Ping

            "shutdown" -> return Shutdown

            "query-daemon-status" -> return QueryDaemonStatus

            "detect-cycle" -> do
                drvTexts <- v .: "derivations"
                drvs <- mapM (\drvText ->
                    case deserializeDerivation $ TE.encodeUtf8 $ extractString drvText of
                        Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                        Right drv -> return drv
                    ) drvTexts
                return $ DetectCycle drvs

            "compute-build-graph" -> do
                drvText <- v .: "derivation"
                case deserializeDerivation $ TE.encodeUtf8 $ extractString drvText of
                    Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                    Right drv -> return $ ComputeBuildGraph drv

            "join-derivation" -> do
                drvText <- v .: "derivation"
                case deserializeDerivation $ TE.encodeUtf8 $ extractString drvText of
                    Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                    Right drv -> return $ JoinDerivation drv

            _ -> fail $ "Unknown request type: " ++ T.unpack requestType
      where
        extractString (Aeson.String s) = s
        extractString _ = T.empty

        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

-- | Daemon response types
data DaemonResponse
    -- Authentication responses
    = AuthResponse AuthResult

    -- Build responses
    | BuildStarted BuildId
    | BuildStatusResponse BuildStatusUpdate
    | BuildFinished BuildId (Either BuildError (Set StorePath))
    | BuildOutputResponse BuildId Text  -- Build log
    | BuildListResponse [(BuildId, BuildStatus, Float)]  -- BuildId, status, progress (changed from Double to Float)

    -- Store responses
    | PathInfoResponse StorePath Bool Text  -- StorePath, exists, info
    | PathAddedResponse StorePath
    | PathVerifiedResponse StorePath Bool  -- Path, isValid
    | StoreContentsResponse [StorePath]

    -- Garbage collection responses
    | GCCompleted Int Int Integer Double  -- paths collected, paths kept, bytes freed, time taken
    | GCRootAdded Text
    | GCRootRemoved Text
    | GCRootsListResponse [(StorePath, Text, Bool)]  -- path, name, permanent

    -- General responses
    | Pong
    | ShutdownResponse
    | DaemonStatusResponse DaemonStatusInfo
    | ErrorResponse BuildError

    -- Graph responses
    | CycleDetectionResponse Bool  -- Whether cycle exists
    | BuildGraphResponse Text  -- Serialized graph

    -- Return-continuation specific
    | JoinedDerivation Derivation
    deriving (Show, Eq, Generic)

-- | Daemon status information
data DaemonStatusInfo = DaemonStatusInfo {
    daemonUptime :: Double,  -- Uptime in seconds
    daemonVersion :: Text,   -- Ten version
    daemonBuildsActive :: Int,  -- Number of active builds
    daemonBuildsQueued :: Int,  -- Number of queued builds
    daemonBuildsCompleted :: Int,  -- Number of completed builds since startup
    daemonBuildsFailed :: Int,  -- Number of failed builds since startup
    daemonStorePaths :: Int,  -- Number of paths in store
    daemonStoreSize :: Integer,  -- Store size in bytes
    daemonLastGC :: Maybe UTCTime,  -- Last GC time
    daemonCPUUsage :: Double,  -- CPU usage (0-100%)
    daemonMemoryUsage :: Integer,  -- Memory usage in bytes
    daemonLoad :: [Double]  -- Load averages (1, 5, 15 min)
} deriving (Show, Eq, Generic)

instance Aeson.ToJSON DaemonStatusInfo where
    toJSON DaemonStatusInfo{..} = Aeson.object [
            "uptime" .= daemonUptime,
            "version" .= daemonVersion,
            "buildsActive" .= daemonBuildsActive,
            "buildsQueued" .= daemonBuildsQueued,
            "buildsCompleted" .= daemonBuildsCompleted,
            "buildsFailed" .= daemonBuildsFailed,
            "storePaths" .= daemonStorePaths,
            "storeSize" .= daemonStoreSize,
            "lastGC" .= daemonLastGC,
            "cpuUsage" .= daemonCPUUsage,
            "memoryUsage" .= daemonMemoryUsage,
            "load" .= daemonLoad
        ]

instance Aeson.FromJSON DaemonStatusInfo where
    parseJSON = Aeson.withObject "DaemonStatusInfo" $ \v -> do
        daemonUptime <- v .: "uptime"
        daemonVersion <- v .: "version"
        daemonBuildsActive <- v .: "buildsActive"
        daemonBuildsQueued <- v .: "buildsQueued"
        daemonBuildsCompleted <- v .: "buildsCompleted"
        daemonBuildsFailed <- v .: "buildsFailed"
        daemonStorePaths <- v .: "storePaths"
        daemonStoreSize <- v .: "storeSize"
        daemonLastGC <- v .: "lastGC"
        daemonCPUUsage <- v .: "cpuUsage"
        daemonMemoryUsage <- v .: "memoryUsage"
        daemonLoad <- v .: "load"
        return DaemonStatusInfo{..}

instance Aeson.ToJSON DaemonResponse where
    toJSON resp = case resp of
        AuthResponse result -> Aeson.object [
                "type" .= ("auth" :: Text),
                "result" .= result
            ]

        BuildStarted buildId -> Aeson.object [
                "type" .= ("build-started" :: Text),
                "buildId" .= show buildId
            ]

        BuildStatusResponse update -> Aeson.object [
                "type" .= ("build-status" :: Text),
                "update" .= update
            ]

        BuildFinished buildId result -> Aeson.object [
                "type" .= ("build-finished" :: Text),
                "buildId" .= show buildId,
                "result" .= encodeResult result
            ]

        BuildOutputResponse buildId output -> Aeson.object [
                "type" .= ("build-output" :: Text),
                "buildId" .= show buildId,
                "output" .= output
            ]

        BuildListResponse builds -> Aeson.object [
                "type" .= ("build-list" :: Text),
                "builds" .= map (\(bid, stat, prog) -> Aeson.object [
                        "id" .= show bid,
                        "status" .= showStatus stat,
                        "progress" .= prog
                    ]) builds
            ]

        PathInfoResponse path exists info -> Aeson.object [
                "type" .= ("path-info" :: Text),
                "path" .= encodePath path,
                "exists" .= exists,
                "info" .= info
            ]

        PathAddedResponse path -> Aeson.object [
                "type" .= ("path-added" :: Text),
                "path" .= encodePath path
            ]

        PathVerifiedResponse path isValid -> Aeson.object [
                "type" .= ("path-verified" :: Text),
                "path" .= encodePath path,
                "valid" .= isValid
            ]

        StoreContentsResponse paths -> Aeson.object [
                "type" .= ("store-contents" :: Text),
                "paths" .= map encodePath paths
            ]

        GCCompleted collected kept bytes time -> Aeson.object [
                "type" .= ("gc-completed" :: Text),
                "collected" .= collected,
                "kept" .= kept,
                "bytes" .= bytes,
                "time" .= time
            ]

        GCRootAdded name -> Aeson.object [
                "type" .= ("gc-root-added" :: Text),
                "name" .= name
            ]

        GCRootRemoved name -> Aeson.object [
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

        Pong -> Aeson.object [
                "type" .= ("pong" :: Text)
            ]

        ShutdownResponse -> Aeson.object [
                "type" .= ("shutdown" :: Text)
            ]

        DaemonStatusResponse info -> Aeson.object [
                "type" .= ("daemon-status" :: Text),
                "info" .= info
            ]

        ErrorResponse err -> Aeson.object [
                "type" .= ("error" :: Text),
                "error" .= showBuildError err
            ]

        CycleDetectionResponse hasCycle -> Aeson.object [
                "type" .= ("cycle-detection" :: Text),
                "hasCycle" .= hasCycle
            ]

        BuildGraphResponse graph -> Aeson.object [
                "type" .= ("build-graph" :: Text),
                "graph" .= graph
            ]

        JoinedDerivation drv -> Aeson.object [
                "type" .= ("joined-derivation" :: Text),
                "derivation" .= Aeson.String (TE.decodeUtf8 $ serializeDerivation drv)
            ]
      where
        encodePath (StorePath hash name) = Aeson.object [
                "hash" .= hash,
                "name" .= name
            ]

        encodeResult :: Either BuildError (Set StorePath) -> Aeson.Value
        encodeResult (Left err) = Aeson.object [
                "success" .= False,
                "error" .= showBuildError err
            ]
        encodeResult (Right paths) = Aeson.object [
                "success" .= True,
                "outputs" .= map encodePath (Set.toList paths)
            ]

        showBuildError :: BuildError -> Text
        showBuildError err = case err of
            EvalError msg -> T.pack $ "Evaluation error: " ++ T.unpack msg
            BuildFailed msg -> T.pack $ "Build failed: " ++ T.unpack msg
            StoreError msg -> T.pack $ "Store error: " ++ T.unpack msg
            SandboxError msg -> T.pack $ "Sandbox error: " ++ T.unpack msg
            InputNotFound path -> T.pack $ "Input not found: " ++ path
            HashError msg -> T.pack $ "Hash error: " ++ T.unpack msg
            GraphError msg -> T.pack $ "Graph error: " ++ T.unpack msg
            ResourceError msg -> T.pack $ "Resource error: " ++ T.unpack msg
            DaemonError msg -> T.pack $ "Daemon error: " ++ T.unpack msg
            AuthError msg -> T.pack $ "Authentication error: " ++ T.unpack msg
            CyclicDependency msg -> T.pack $ "Cyclic dependency: " ++ T.unpack msg
            SerializationError msg -> T.pack $ "Serialization error: " ++ T.unpack msg
            RecursionLimit msg -> T.pack $ "Recursion limit exceeded: " ++ T.unpack msg

        showStatus :: BuildStatus -> Text
        showStatus BuildPending = "pending"
        showStatus (BuildRunning _) = "running"
        showStatus (BuildRecursing _) = "recursing"
        showStatus BuildCompleted = "completed"
        showStatus BuildFailed' = "failed"

instance Aeson.FromJSON DaemonResponse where
    parseJSON = Aeson.withObject "DaemonResponse" $ \v -> do
        responseType <- v .: "type"
        case responseType of
            "auth" -> do
                result <- v .: "result"
                return $ AuthResponse result

            "build-started" -> do
                buildIdStr <- v .: "buildId"
                return $ BuildStarted (read buildIdStr)

            "build-status" -> do
                update <- v .: "update"
                return $ BuildStatusResponse update

            "build-finished" -> do
                buildIdStr <- v .: "buildId"
                resultObj <- v .: "result"
                success <- resultObj .: "success"
                result <- if success
                    then do
                        outputsJson <- resultObj .: "outputs"
                        outputs <- mapM decodePath outputsJson
                        return $ Right $ Set.fromList outputs
                    else do
                        errorMsg <- resultObj .: "error"
                        return $ Left $ BuildFailed errorMsg
                return $ BuildFinished (read buildIdStr) result

            "build-output" -> do
                buildIdStr <- v .: "buildId"
                output <- v .: "output"
                return $ BuildOutputResponse (read buildIdStr) output

            "build-list" -> do
                buildsJson <- v .: "builds"
                builds <- mapM (\obj -> do
                    idStr <- obj .: "id"
                    statusStr <- obj .: "status"
                    progress <- obj .: "progress"
                    let status = case statusStr of
                            "pending" -> BuildPending
                            "running" -> BuildRunning progress
                            "recursing" -> BuildRecursing (BuildId undefined)  -- Can't reconstruct inner ID
                            "completed" -> BuildCompleted
                            "failed" -> BuildFailed'
                            _ -> BuildPending  -- Default
                    return (read idStr, status, progress)
                    ) buildsJson
                return $ BuildListResponse builds

            "path-info" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                exists <- v .: "exists"
                info <- v .: "info"
                return $ PathInfoResponse path exists info

            "path-added" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                return $ PathAddedResponse path

            "path-verified" -> do
                pathObj <- v .: "path"
                path <- decodePath pathObj
                isValid <- v .: "valid"
                return $ PathVerifiedResponse path isValid

            "store-contents" -> do
                pathsJson <- v .: "paths"
                paths <- mapM decodePath pathsJson
                return $ StoreContentsResponse paths

            "gc-completed" -> do
                collected <- v .: "collected"
                kept <- v .: "kept"
                bytes <- v .: "bytes"
                time <- v .: "time"
                return $ GCCompleted collected kept bytes time

            "gc-root-added" -> do
                name <- v .: "name"
                return $ GCRootAdded name

            "gc-root-removed" -> do
                name <- v .: "name"
                return $ GCRootRemoved name

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

            "pong" -> return Pong

            "shutdown" -> return ShutdownResponse

            "daemon-status" -> do
                info <- v .: "info"
                return $ DaemonStatusResponse info

            "error" -> do
                errorMsg <- v .: "error"
                return $ ErrorResponse (BuildFailed errorMsg)

            "cycle-detection" -> do
                hasCycle <- v .: "hasCycle"
                return $ CycleDetectionResponse hasCycle

            "build-graph" -> do
                graph <- v .: "graph"
                return $ BuildGraphResponse graph

            "joined-derivation" -> do
                drvText <- v .: "derivation"
                case deserializeDerivation $ TE.encodeUtf8 $ extractString drvText of
                    Left err -> fail $ "Invalid derivation: " ++ T.unpack err
                    Right drv -> return $ JoinedDerivation drv

            _ -> fail $ "Unknown response type: " ++ T.unpack responseType
      where
        extractString (Aeson.String s) = s
        extractString _ = T.empty

        decodePath = Aeson.withObject "StorePath" $ \p -> do
            hash <- p .: "hash"
            name <- p .: "name"
            return $ StorePath hash name

-- | Serialize a request to JSON
serializeRequest :: DaemonRequest -> BS.ByteString
serializeRequest = LBS.toStrict . Aeson.encode

-- | Deserialize a request from JSON
deserializeRequest :: BS.ByteString -> Either Text DaemonRequest
deserializeRequest bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ T.pack $ "Failed to parse request: " ++ err
        Right req -> Right req

-- | Serialize a response to JSON
serializeResponse :: DaemonResponse -> BS.ByteString
serializeResponse = LBS.toStrict . Aeson.encode

-- | Deserialize a response from JSON
deserializeResponse :: BS.ByteString -> Either Text DaemonResponse
deserializeResponse bs =
    case Aeson.eitherDecodeStrict bs of
        Left err -> Left $ T.pack $ "Failed to parse response: " ++ err
        Right resp -> Right resp

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
sendRequest :: ProtocolHandle -> DaemonRequest -> IO ()
sendRequest handle req = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        let content = serializeRequest req
        let frame = createRequestFrame content
        NByte.sendAll (protocolSocket handle) frame

-- | Receive a response from a protocol handle
receiveResponse :: ProtocolHandle -> IO (Either ProtocolError DaemonResponse)
receiveResponse handle = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Read the 4-byte length prefix
        lenBytes <- try $ NByte.recv (protocolSocket handle) 4
        case lenBytes of
            Left (_ :: SomeException) ->
                return $ Left ConnectionClosed
            Right bytes
                | BS.length bytes < 4 ->
                    return $ Left ConnectionClosed
                | otherwise -> do
                    let len = fromIntegral $
                              (fromIntegral (BS.index bytes 0) `shiftL` 24) .|.
                              (fromIntegral (BS.index bytes 1) `shiftL` 16) .|.
                              (fromIntegral (BS.index bytes 2) `shiftL` 8) .|.
                              (fromIntegral (BS.index bytes 3))

                    -- Check for unreasonable message sizes
                    when (len > 100 * 1024 * 1024) $  -- 100 MB limit
                        throwIO $ MessageTooLarge (fromIntegral len)

                    -- Read the message content
                    content <- recvExactly (protocolSocket handle) len
                    if BS.length content < len
                        then return $ Left ConnectionClosed
                        else case deserializeResponse content of
                            Left err -> return $ Left $ ProtocolParseError err
                            Right resp -> return $ Right resp

-- | Send a response over a protocol handle
sendResponse :: ProtocolHandle -> DaemonResponse -> IO ()
sendResponse handle resp = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        let content = serializeResponse resp
        let frame = createResponseFrame content
        NByte.sendAll (protocolSocket handle) frame

-- | Receive a request from a protocol handle
receiveRequest :: ProtocolHandle -> IO (Either ProtocolError DaemonRequest)
receiveRequest handle = do
    -- Take the lock for thread safety
    withMVar (protocolLock handle) $ \_ -> do
        -- Read the 4-byte length prefix
        lenBytes <- try $ NByte.recv (protocolSocket handle) 4
        case lenBytes of
            Left (_ :: SomeException) ->
                return $ Left ConnectionClosed
            Right bytes
                | BS.length bytes < 4 ->
                    return $ Left ConnectionClosed
                | otherwise -> do
                    let len = fromIntegral $
                              (fromIntegral (BS.index bytes 0) `shiftL` 24) .|.
                              (fromIntegral (BS.index bytes 1) `shiftL` 16) .|.
                              (fromIntegral (BS.index bytes 2) `shiftL` 8) .|.
                              (fromIntegral (BS.index bytes 3))

                    -- Check for unreasonable message sizes
                    when (len > 100 * 1024 * 1024) $  -- 100 MB limit
                        throwIO $ MessageTooLarge (fromIntegral len)

                    -- Read the message content
                    content <- recvExactly (protocolSocket handle) len
                    if BS.length content < len
                        then return $ Left ConnectionClosed
                        else case deserializeRequest content of
                            Left err -> return $ Left $ ProtocolParseError err
                            Right req -> return $ Right req

-- | Execute an action with a protocol handle and clean up after
withProtocolHandle :: Socket -> (ProtocolHandle -> IO a) -> IO a
withProtocolHandle socket action =
    bracket
        (createHandle socket)
        closeHandle
        action

-- | Read exactly n bytes from a socket
recvExactly :: Socket -> Int -> IO BS.ByteString
recvExactly socket n = go n []
  where
    go 0 chunks = return $ BS.concat $ reverse chunks
    go remaining chunks = do
        chunk <- NByte.recv socket remaining
        let chunkSize = BS.length chunk
        if chunkSize == 0
            then return $ BS.concat $ reverse chunks  -- Connection closed
            else go (remaining - chunkSize) (chunk : chunks)

-- | Convert a request to human-readable text
requestToText :: DaemonRequest -> Text
requestToText req = case req of
    AuthRequest ver _ ->
        T.pack $ "Auth request (protocol version " ++ show ver ++ ")"

    BuildDerivation drv _ ->
        T.pack $ "Build derivation: " ++ T.unpack (T.take 40 (hashDerivation drv)) ++ "..."

    BuildFile path _ ->
        T.pack $ "Build file: " ++ path

    CancelBuild buildId ->
        T.pack $ "Cancel build: " ++ show buildId

    QueryBuildStatus buildId ->
        T.pack $ "Query build status: " ++ show buildId

    QueryBuildOutput buildId ->
        T.pack $ "Query build output: " ++ show buildId

    ListBuilds limit ->
        T.pack $ "List builds" ++ maybe "" (\n -> " (limit: " ++ show n ++ ")") limit

    QueryPath path ->
        T.pack $ "Query path: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)

    AddToStore name _ ->
        T.pack $ "Add to store: " ++ T.unpack name

    EnsureInStore name path ->
        T.pack $ "Ensure in store: " ++ T.unpack name ++ " (from " ++ path ++ ")"

    VerifyPath path ->
        T.pack $ "Verify path: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)

    ListStoreContents prefix ->
        T.pack $ "List store" ++ maybe "" (\p -> " (prefix: " ++ T.unpack p ++ ")") prefix

    CollectGarbage force ->
        T.pack $ "Collect garbage" ++ if force then " (force)" else ""

    AddGCRoot path name permanent ->
        T.pack $ "Add GC root: " ++ T.unpack name ++ " -> " ++
        T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path) ++
        if permanent then " (permanent)" else ""

    RemoveGCRoot name ->
        T.pack $ "Remove GC root: " ++ T.unpack name

    ListGCRoots ->
        T.pack "List GC roots"

    Ping ->
        T.pack "Ping"

    Shutdown ->
        T.pack "Shutdown"

    QueryDaemonStatus ->
        T.pack "Query daemon status"

    DetectCycle drvs ->
        T.pack $ "Detect cycle in " ++ show (length drvs) ++ " derivations"

    ComputeBuildGraph drv ->
        T.pack $ "Compute build graph for: " ++ T.unpack (T.take 40 (hashDerivation drv)) ++ "..."

    JoinDerivation drv ->
        T.pack $ "Join derivation: " ++ T.unpack (T.take 40 (hashDerivation drv)) ++ "..."

-- | Convert a response to human-readable text
responseToText :: DaemonResponse -> Text
responseToText resp = case resp of
    AuthResponse (AuthAccepted uid _) ->
        T.pack $ "Auth accepted for user: " ++ show uid

    AuthResponse (AuthRejected reason) ->
        T.pack $ "Auth rejected: " ++ T.unpack reason

    BuildStarted buildId ->
        T.pack $ "Build started: " ++ show buildId

    BuildStatusResponse update ->
        T.pack $ "Build status update for " ++ show (buildId update) ++
                 ": " ++ showStatus (buildStatus update) ++
                 " (" ++ show (round (buildTimeElapsed update)) ++ "s)"

    BuildFinished buildId (Right outputs) ->
        T.pack $ "Build finished: " ++ show buildId ++ " with " ++
                 show (Set.size outputs) ++ " outputs"

    BuildFinished buildId (Left err) ->
        T.pack $ "Build failed: " ++ show buildId ++ " - " ++ showError err

    BuildOutputResponse buildId _ ->
        T.pack $ "Build output for: " ++ show buildId

    BuildListResponse builds ->
        T.pack $ "Build list: " ++ show (length builds) ++ " builds"

    PathInfoResponse path exists _ ->
        T.pack $ "Path info: " ++ T.unpack (storeHash path) ++ "-" ++
                 T.unpack (storeName path) ++ (if exists then " (exists)" else " (missing)")

    PathAddedResponse path ->
        T.pack $ "Path added: " ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)

    PathVerifiedResponse path isValid ->
        T.pack $ "Path verified: " ++ T.unpack (storeHash path) ++ "-" ++
                 T.unpack (storeName path) ++ (if isValid then " (valid)" else " (invalid)")

    StoreContentsResponse paths ->
        T.pack $ "Store contents: " ++ show (length paths) ++ " paths"

    GCCompleted collected kept bytes time ->
        T.pack $ "GC completed: " ++ show collected ++ " paths collected, " ++
                 show kept ++ " paths kept, " ++ formatBytes bytes ++ " freed in " ++
                 show (round time) ++ "s"

    GCRootAdded name ->
        T.pack $ "GC root added: " ++ T.unpack name

    GCRootRemoved name ->
        T.pack $ "GC root removed: " ++ T.unpack name

    GCRootsListResponse roots ->
        T.pack $ "GC roots: " ++ show (length roots) ++ " roots"

    Pong ->
        T.pack "Pong"

    ShutdownResponse ->
        T.pack "Shutdown acknowledged"

    DaemonStatusResponse info ->
        T.pack $ "Daemon status: " ++ show (daemonBuildsActive info) ++ " active builds, " ++
                 show (daemonBuildsQueued info) ++ " queued"

    ErrorResponse err ->
        T.pack $ "Error: " ++ showError err

    CycleDetectionResponse hasCycle ->
        T.pack $ "Cycle detection: " ++ if hasCycle then "cycle found" else "no cycles"

    BuildGraphResponse _ ->
        T.pack "Build graph"

    JoinedDerivation drv ->
        T.pack $ "Joined derivation: " ++ T.unpack (T.take 40 (hashDerivation drv)) ++ "..."
  where
    showStatus :: BuildStatus -> String
    showStatus BuildPending = "pending"
    showStatus (BuildRunning progress) =
        "running (" ++ show (round (progress * 100)) ++ "%)"
    showStatus (BuildRecursing innerBuildId) =
        "recursing to " ++ show innerBuildId
    showStatus BuildCompleted = "completed"
    showStatus BuildFailed' = "failed"

    showError :: BuildError -> String
    showError (EvalError msg) = "Evaluation error: " ++ T.unpack msg
    showError (BuildFailed msg) = "Build failed: " ++ T.unpack msg
    showError (StoreError msg) = "Store error: " ++ T.unpack msg
    showError (SandboxError msg) = "Sandbox error: " ++ T.unpack msg
    showError (InputNotFound path) = "Input not found: " ++ path
    showError (HashError msg) = "Hash error: " ++ T.unpack msg
    showError (GraphError msg) = "Graph error: " ++ T.unpack msg
    showError (ResourceError msg) = "Resource error: " ++ T.unpack msg
    showError (DaemonError msg) = "Daemon error: " ++ T.unpack msg
    showError (AuthError msg) = "Authentication error: " ++ T.unpack msg
    showError (CyclicDependency msg) = "Cyclic dependency: " ++ T.unpack msg
    showError (SerializationError msg) = "Serialization error: " ++ T.unpack msg
    showError (RecursionLimit msg) = "Recursion limit exceeded: " ++ T.unpack msg

    formatBytes :: Integer -> String
    formatBytes bytes
        | bytes < 1024 = show bytes ++ " B"
        | bytes < 1024 * 1024 = show (bytes `div` 1024) ++ " KB"
        | bytes < 1024 * 1024 * 1024 = show (bytes `div` (1024 * 1024)) ++ " MB"
        | otherwise = show (bytes `div` (1024 * 1024 * 1024)) ++ " GB"

-- Bit manipulation helpers
shiftL :: Int -> Int -> Int
shiftL x n = x * (2 ^ n)

(.|.) :: Int -> Int -> Int
a .|. b = a + b
