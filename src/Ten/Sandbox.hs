{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Ten.Sandbox (
    -- Core sandbox functions
    withSandbox,
    setupSandbox,
    teardownSandbox,

    -- Configuration
    SandboxConfig(..),
    defaultSandboxConfig,

    -- Path management
    returnDerivationPath,
    sandboxInputPath,
    sandboxOutputPath,

    -- Directory and mount operations
    createAndSetupDir,
    makeInputsAvailable,

    -- Mount operations
    bindMount,
    bindMountReadOnly,
    unmountPath,

    -- Process management
    spawnSandboxedProcess,
    runBuilderProcess,
    dropSandboxPrivileges,

    -- Protocol-based sandbox operations
    requestSandbox,
    releaseSandbox,
    sandboxProtocolHandler,

    -- Sandbox connection types
    SandboxConnection(..),
    createSandboxConnection,
    closeSandboxConnection,

    -- Sandbox status
    SandboxStatus(..),
    SandboxResponse(..)
) where

import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets, put, modify)
import qualified Control.Monad.State as State
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, isInfixOf, sort)
import Control.Exception (bracket, try, tryJust, throwIO, catch, finally, handle, SomeException, IOException)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits ((.|.), (.&.))
import Data.Maybe (isJust, fromMaybe, listToMaybe, catMaybes)
import qualified Data.Binary as Binary
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, getHomeDirectory,
                        removeFile, getPermissions, setPermissions, removePathForcibly,
                        listDirectory, copyFile, Permissions, pathIsSymbolicLink)
import qualified System.Directory as Directory
import System.FilePath ((</>), takeDirectory, takeFileName, makeRelative)
import qualified System.FilePath as FilePath
import System.Process (createProcess, proc, readCreateProcessWithExitCode, CreateProcess(..), StdStream(..))
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Files as Posix
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus,
                          ProcessStatus(..), exitImmediately)
import System.Posix.Types (ProcessID, Fd, FileMode, UserID, GroupID)
import qualified System.Posix.User as User
import qualified System.Posix.Resource as Resource
import qualified System.Posix.IO as PosixIO
import System.Posix.IO (LockRequest(WriteLock, Unlock))
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1_, throwErrnoIfMinus1, getErrno, Errno(..), errnoToIOError)
import Foreign.C.String (CString, withCString, peekCString, newCString)
import Foreign.C.Types (CInt(..), CULong(..), CLong(..), CSize(..))
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, malloc, free)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Storable
import System.IO.Error (IOError, catchIOError, isPermissionError, isDoesNotExistError)
import System.IO (hPutStrLn, stderr, hClose, Handle, hFlush, hGetContents)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket (Socket)
import qualified Network.Socket as Network
import qualified Network.Socket.ByteString as NetworkBS
import System.Random (randomRIO)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Singletons (SingI, fromSing)
import Data.Word (Word8)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Unique (Unique, newUnique, hashUnique)
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as TE

import Ten.Core (
    -- Re-export essential types and functions
    Phase(..), SPhase(..), PrivilegeTier(..), SPrivilegeTier(..), TenM(..),
    BuildId(..), BuildStatus(..), BuildError(..), StorePath(..), storePathToText,
    UserId(..), AuthToken(..), BuildEnv(..), BuildState(..),
    DaemonConnection(..), TenM, runTen, addProof, throwError,
    workDir, storeLocation, currentBuildId, currentPhase, runMode,
    logMsg, sBuild, sDaemon, RunMode(..), storePathToFilePath,
    BuildError(..), timeout, Proof(..), validateStorePath, validateStorePathWithContext
    )
import qualified Ten.Daemon.Protocol as Protocol

-- | Sandbox connection type for proper process separation
data SandboxConnection = SandboxConnection {
    sandboxConnSocket :: Socket,         -- Socket for communicating with daemon
    sandboxConnAuthToken :: AuthToken,   -- Authentication token
    sandboxConnUserId :: UserId,         -- User ID
    sandboxConnRequestCounter :: TVar Int, -- Request counter
    sandboxConnResponses :: TVar (Map Int (MVar SandboxResponse)) -- Response tracking
}

-- | Create a sandbox connection from socket information
createSandboxConnection :: Socket -> AuthToken -> UserId -> IO SandboxConnection
createSandboxConnection sock authToken userId = do
    requestCounter <- newTVarIO 0
    responses <- newTVarIO Map.empty
    return SandboxConnection {
        sandboxConnSocket = sock,
        sandboxConnAuthToken = authToken,
        sandboxConnUserId = userId,
        sandboxConnRequestCounter = requestCounter,
        sandboxConnResponses = responses
    }

-- | Close a sandbox connection
closeSandboxConnection :: SandboxConnection -> IO ()
closeSandboxConnection conn = do
    Network.close (sandboxConnSocket conn)

-- | Sandbox request message types for daemon-builder communication
data SandboxRequest
    = SandboxCreateRequest {
        sandboxReqBuildId :: BuildId,
        sandboxReqInputs :: Set StorePath,
        sandboxReqConfig :: SandboxConfig
    }
    | SandboxReleaseRequest {
        sandboxReqId :: Text
    }
    | SandboxStatusRequest {
        sandboxReqId :: Text
    }
    | SandboxAbortRequest {
        sandboxReqId :: Text
    }
    deriving (Show, Eq)

-- | Sandbox response message types for daemon-builder communication
data SandboxResponse
    = SandboxCreatedResponse {
        sandboxRespId :: Text,
        sandboxRespPath :: FilePath
    }
    | SandboxReleasedResponse {
        sandboxRespSuccess :: Bool
    }
    | SandboxStatusResponse {
        sandboxRespStatus :: SandboxStatus
    }
    | SandboxErrorResponse {
        sandboxRespError :: Text
    }
    deriving (Show, Eq)

-- | Sandbox status information
data SandboxStatus = SandboxStatus {
    sandboxStatusActive :: Bool,
    sandboxStatusPath :: FilePath,
    sandboxStatusBuildId :: BuildId,
    sandboxStatusCreatedTime :: UTCTime
} deriving (Show, Eq)

-- | Configuration for a build sandbox
data SandboxConfig = SandboxConfig {
    sandboxAllowNetwork :: Bool,           -- Allow network access
    sandboxExtraPaths :: Set FilePath,     -- Additional paths to make available
    sandboxEnv :: Map Text Text,           -- Environment variables
    sandboxReadOnlyPaths :: Set FilePath,  -- Paths to mount read-only
    sandboxWritablePaths :: Set FilePath,  -- Paths to mount writable
    sandboxPrivileged :: Bool,             -- Run as privileged user
    sandboxReturnSupport :: Bool,          -- Allow return-continuation support
    sandboxUseMountNamespace :: Bool,      -- Use Linux mount namespace isolation
    sandboxUseNetworkNamespace :: Bool,    -- Use Linux network namespace isolation
    sandboxUseUserNamespace :: Bool,       -- Use Linux user namespace isolation
    sandboxUser :: String,                 -- User to run build as (for unprivileged)
    sandboxGroup :: String,                -- Group to run build as (for unprivileged)
    sandboxCPULimit :: Int,                -- CPU time limit in seconds (0 = no limit)
    sandboxMemoryLimit :: Int,             -- Memory limit in MB (0 = no limit)
    sandboxDiskLimit :: Int,               -- Disk space limit in MB (0 = no limit)
    sandboxMaxProcesses :: Int,            -- Maximum number of processes (0 = no limit)
    sandboxTimeoutSecs :: Int,             -- Timeout in seconds (0 = no timeout)
    sandboxKeepBuildOutput :: Bool         -- Flag to keep build output directory after completion
} deriving (Show, Eq)

-- | Default sandbox configuration (restrictive)
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig {
    sandboxAllowNetwork = False,
    sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"],
    sandboxEnv = Map.empty,
    sandboxReadOnlyPaths = Set.empty,
    sandboxWritablePaths = Set.empty,
    sandboxPrivileged = False,
    sandboxReturnSupport = True,
    sandboxUseMountNamespace = True,
    sandboxUseNetworkNamespace = True,     -- Isolate network by default
    sandboxUseUserNamespace = False,       -- Don't use user namespace by default (needs root)
    sandboxUser = "nobody",                -- Default unprivileged user
    sandboxGroup = "nogroup",              -- Default unprivileged group
    sandboxCPULimit = 3600,                -- 1 hour CPU time by default
    sandboxMemoryLimit = 2048,             -- 2GB memory by default
    sandboxDiskLimit = 5120,               -- 5GB disk space by default
    sandboxMaxProcesses = 32,              -- Maximum 32 processes by default
    sandboxTimeoutSecs = 0,                -- No timeout by default
    sandboxKeepBuildOutput = False         -- Don't keep build output by default
}

-- | Binary instance for SandboxRequest for serialization
instance Binary.Binary SandboxRequest where
    put (SandboxCreateRequest buildId inputs config) = do
        Binary.put (0 :: Word8)  -- Tag for CreateRequest
        Binary.put (show buildId)
        -- Serialize the set of store paths
        Binary.put (length $ Set.toList inputs)
        forM_ (Set.toList inputs) $ \path -> do
            Binary.put (storeHash path)
            Binary.put (storeName path)
        Binary.put (encodeConfig config)

    put (SandboxReleaseRequest sandboxId) = do
        Binary.put (1 :: Word8)  -- Tag for ReleaseRequest
        Binary.put sandboxId

    put (SandboxStatusRequest sandboxId) = do
        Binary.put (2 :: Word8)  -- Tag for StatusRequest
        Binary.put sandboxId

    put (SandboxAbortRequest sandboxId) = do
        Binary.put (3 :: Word8)  -- Tag for AbortRequest
        Binary.put sandboxId

    get = do
        tag <- Binary.get :: Binary.Get Word8
        case tag of
            0 -> do  -- CreateRequest
                buildIdStr <- Binary.get
                -- Deserialize the list of store paths with validation
                count <- Binary.get :: Binary.Get Int
                inputsList <- forM [1..count] $ \_ -> do
                    hash <- Binary.get
                    name <- Binary.get
                    let path = StorePath hash name
                    -- Validate the path during deserialization
                    if validateStorePath path
                        then return path
                        else fail $ "Invalid StorePath during deserialization: " ++
                             T.unpack hash ++ "-" ++ T.unpack name
                configData <- Binary.get
                return $ SandboxCreateRequest (read buildIdStr) (Set.fromList inputsList) (decodeConfig configData)
            1 -> SandboxReleaseRequest <$> Binary.get
            2 -> SandboxStatusRequest <$> Binary.get
            3 -> SandboxAbortRequest <$> Binary.get
            _ -> fail $ "Unknown SandboxRequest tag: " ++ show tag

-- | Binary instance for SandboxResponse for serialization
instance Binary.Binary SandboxResponse where
    put (SandboxCreatedResponse sid path) = do
        Binary.put (0 :: Word8)  -- Tag for CreatedResponse
        Binary.put sid
        Binary.put path

    put (SandboxReleasedResponse success) = do
        Binary.put (1 :: Word8)  -- Tag for ReleasedResponse
        Binary.put success

    put (SandboxStatusResponse status) = do
        Binary.put (2 :: Word8)  -- Tag for StatusResponse
        Binary.put (sandboxStatusActive status)
        Binary.put (sandboxStatusPath status)
        Binary.put (show $ sandboxStatusBuildId status)
        Binary.put (show $ sandboxStatusCreatedTime status)

    put (SandboxErrorResponse err) = do
        Binary.put (3 :: Word8)  -- Tag for ErrorResponse
        Binary.put err

    get = do
        tag <- Binary.get :: Binary.Get Word8
        case tag of
            0 -> SandboxCreatedResponse <$> Binary.get <*> Binary.get
            1 -> SandboxReleasedResponse <$> Binary.get
            2 -> do
                active <- Binary.get
                path <- Binary.get
                buildIdStr <- Binary.get
                timeStr <- Binary.get
                return $ SandboxStatusResponse $ SandboxStatus
                    active
                    path
                    (read buildIdStr)
                    (read timeStr)
            3 -> SandboxErrorResponse <$> Binary.get
            _ -> fail $ "Unknown SandboxResponse tag: " ++ show tag

-- Helper functions to encode/decode sandbox config
encodeConfig :: SandboxConfig -> BS.ByteString
encodeConfig = LBS.toStrict . Binary.encode . configToList

decodeConfig :: BS.ByteString -> SandboxConfig
decodeConfig = listToConfig . Binary.decode . LBS.fromStrict

-- Convert config to/from a list of key-value pairs for serialization
configToList :: SandboxConfig -> [(String, String)]
configToList config = [
    ("allowNetwork", show $ sandboxAllowNetwork config),
    ("extraPaths", show $ Set.toList $ sandboxExtraPaths config),
    ("readOnlyPaths", show $ Set.toList $ sandboxReadOnlyPaths config),
    ("writablePaths", show $ Set.toList $ sandboxWritablePaths config),
    ("privileged", show $ sandboxPrivileged config),
    ("returnSupport", show $ sandboxReturnSupport config),
    ("useMountNamespace", show $ sandboxUseMountNamespace config),
    ("useNetworkNamespace", show $ sandboxUseNetworkNamespace config),
    ("useUserNamespace", show $ sandboxUseUserNamespace config),
    ("user", sandboxUser config),
    ("group", sandboxGroup config),
    ("cpuLimit", show $ sandboxCPULimit config),
    ("memoryLimit", show $ sandboxMemoryLimit config),
    ("diskLimit", show $ sandboxDiskLimit config),
    ("maxProcesses", show $ sandboxMaxProcesses config),
    ("timeoutSecs", show $ sandboxTimeoutSecs config),
    ("keepBuildOutput", show $ sandboxKeepBuildOutput config)
    ]

listToConfig :: [(String, String)] -> SandboxConfig
listToConfig items =
    let
        getItem key = fromMaybe "" $ lookup key items
        getBool key = case lookup key items of
            Just "True" -> True
            _ -> False
        getInt key = case lookup key items of
            Just str -> read str
            _ -> 0
        getPaths key = case lookup key items of
            Just str -> Set.fromList (read str)
            _ -> Set.empty
    in
        SandboxConfig {
            sandboxAllowNetwork = getBool "allowNetwork",
            sandboxExtraPaths = getPaths "extraPaths",
            sandboxEnv = Map.empty,  -- Environment will be set later
            sandboxReadOnlyPaths = getPaths "readOnlyPaths",
            sandboxWritablePaths = getPaths "writablePaths",
            sandboxPrivileged = getBool "privileged",
            sandboxReturnSupport = getBool "returnSupport",
            sandboxUseMountNamespace = getBool "useMountNamespace",
            sandboxUseNetworkNamespace = getBool "useNetworkNamespace",
            sandboxUseUserNamespace = getBool "useUserNamespace",
            sandboxUser = getItem "user",
            sandboxGroup = getItem "group",
            sandboxCPULimit = getInt "cpuLimit",
            sandboxMemoryLimit = getInt "memoryLimit",
            sandboxDiskLimit = getInt "diskLimit",
            sandboxMaxProcesses = getInt "maxProcesses",
            sandboxTimeoutSecs = getInt "timeoutSecs",
            sandboxKeepBuildOutput = getBool "keepBuildOutput"
        }

-- | Helper function to convert BuildId to String for filesystem paths
showBuildId :: BuildId -> String
showBuildId (BuildId u) = "build-" ++ show (hashUnique u)
showBuildId (BuildIdFromInt n) = "build-" ++ show n

-- | Hash an integer in a cryptographically secure way
-- Replacement for the non-existent hashInt function
hashInt :: Int -> Int
hashInt n =
    let
        -- Convert int to ByteString using text encoding
        bs = BS8.pack (show n)
        -- Hash using SHA256
        digest = Crypto.hash bs :: Digest SHA256
        -- Convert to hex string and take first 8 chars
        hexStr = take 8 $ show digest
        -- Convert hex to int
        hexVal = foldl (\acc c -> acc * 16 + hexDigitToInt c) 0 hexStr
    in
        abs hexVal  -- Ensure positive value
  where
    hexDigitToInt :: Char -> Int
    hexDigitToInt c
        | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
        | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
        | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'
        | otherwise = 0

-- Linux-specific constants for mount operations
mS_RDONLY, mS_BIND, mS_NOSUID, mS_NODEV, mS_NOEXEC, mS_REMOUNT, mS_REC, mS_PRIVATE :: CInt
mS_RDONLY = 1
mS_BIND = 4096
mS_NOSUID = 2
mS_NODEV = 4
mS_NOEXEC = 8
mS_REMOUNT = 32
mS_REC = 16384
mS_PRIVATE = 262144

-- Umount flags
mNT_FORCE, mNT_DETACH :: CInt
mNT_FORCE = 1
mNT_DETACH = 2

-- Namespace flags
cLONE_NEWNS, cLONE_NEWUTS, cLONE_NEWIPC, cLONE_NEWUSER, cLONE_NEWPID, cLONE_NEWNET :: CInt
cLONE_NEWNS = 0x00020000    -- Mount namespace
cLONE_NEWUTS = 0x04000000   -- UTS namespace (hostname)
cLONE_NEWIPC = 0x08000000   -- IPC namespace
cLONE_NEWUSER = 0x10000000  -- User namespace
cLONE_NEWPID = 0x20000000   -- PID namespace
cLONE_NEWNET = 0x40000000   -- Network namespace

-- Resource limit constants
rLIMIT_CPU, rLIMIT_FSIZE, rLIMIT_DATA, rLIMIT_STACK, rLIMIT_CORE,
    rLIMIT_RSS, rLIMIT_NOFILE, rLIMIT_AS, rLIMIT_NPROC, rLIMIT_MEMLOCK :: CInt
rLIMIT_CPU = 0        -- CPU time in seconds
rLIMIT_FSIZE = 1      -- Maximum filesize
rLIMIT_DATA = 2       -- Max data size
rLIMIT_STACK = 3      -- Max stack size
rLIMIT_CORE = 4       -- Max core file size
rLIMIT_RSS = 5        -- Max resident set size
rLIMIT_NOFILE = 7     -- Max number of open files
rLIMIT_AS = 9         -- Address space limit
rLIMIT_NPROC = 6      -- Max number of processes
rLIMIT_MEMLOCK = 8    -- Max locked-in-memory address space

-- Foreign function imports for Linux-specific system calls
foreign import ccall unsafe "sys/mount.h mount"
    c_mount :: CString -> CString -> CString -> CInt -> Ptr () -> IO CInt

foreign import ccall unsafe "sys/mount.h umount2"
    c_umount2 :: CString -> CInt -> IO CInt

foreign import ccall unsafe "sched.h unshare"
    c_unshare :: CInt -> IO CInt

foreign import ccall unsafe "sys/resource.h getrlimit"
    c_getrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import ccall unsafe "sys/resource.h setrlimit"
    c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import ccall unsafe "unistd.h chroot"
    c_chroot :: CString -> IO CInt

-- Linux capabilities definitions
foreign import ccall unsafe "sys/capability.h cap_clear"
    c_cap_clear :: Ptr () -> IO CInt

foreign import ccall unsafe "sys/capability.h cap_get_proc"
    c_cap_get_proc :: IO (Ptr ())

foreign import ccall unsafe "sys/capability.h cap_set_proc"
    c_cap_set_proc :: Ptr () -> IO CInt

foreign import ccall unsafe "sys/capability.h cap_free"
    c_cap_free :: Ptr () -> IO CInt

-- RLimit structure for resource limits
data RLimit = RLimit {
    rlim_cur :: CULong,  -- Current limit (soft limit)
    rlim_max :: CULong   -- Maximum limit (hard limit)
}

instance Storable RLimit where
    sizeOf _ = 16
    alignment _ = 8
    peek ptr = do
        cur <- peekByteOff ptr 0 :: IO CULong
        max <- peekByteOff ptr 8 :: IO CULong
        return $ RLimit cur max
    poke ptr (RLimit cur max) = do
        pokeByteOff ptr 0 cur
        pokeByteOff ptr 8 max

-- | Type class for sandbox operations with privilege-specific implementations
class SandboxCreator (t :: PrivilegeTier) where
    withSandboxImpl :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build t a) -> TenM 'Build t a

-- | Daemon instance for direct sandbox creation (privileged)
instance SandboxCreator 'Daemon where
    withSandboxImpl = withSandboxDaemon

-- | Builder instance for protocol-based sandbox creation (unprivileged)
instance SandboxCreator 'Builder where
    withSandboxImpl = withSandboxViaProtocol

-- | Core function to run an action within a build sandbox with proper privilege separation
-- This function delegates to the appropriate implementation based on privilege tier
withSandbox :: SandboxCreator t => Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build t a) -> TenM 'Build t a
withSandbox = withSandboxImpl

-- | Daemon-specific implementation of withSandbox
withSandboxDaemon :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withSandboxDaemon inputs config action = do
    env <- ask
    currState <- get

    -- Verify we are in Build phase
    unless (currentPhase currState == Build) $
        throwError $ PhaseError "Sandbox can only be created in Build phase"

    bid <- gets currentBuildId

    -- Get unique sandbox directory
    let baseDir = workDir env </> "sandbox"
    let sandboxDir = baseDir </> showBuildId bid

    -- Create the directory if it doesn't exist
    liftIO $ createDirectoryIfMissing True baseDir
    liftIO $ createDirectoryIfMissing True sandboxDir

    -- Set proper permissions immediately
    liftIO $ Posix.setFileMode sandboxDir 0o755

    -- Acquire resource
    result <- (do
        -- Set up the sandbox with appropriate namespaces and mounts
        setupSandbox sandboxDir config

        -- Make inputs available in the sandbox
        makeInputsAvailable sandboxDir inputs

        -- Add read-only and writable paths from config
        forM_ (Set.toList $ sandboxReadOnlyPaths config) $
            makePathAvailable sandboxDir False

        forM_ (Set.toList $ sandboxWritablePaths config) $
            makePathAvailable sandboxDir True

        -- Add additional system paths specified in config
        forM_ (Set.toList $ sandboxExtraPaths config) $
            makeSystemPathAvailable sandboxDir

        -- Run the action inside the sandbox
        action sandboxDir) `catchError` \e -> do
            -- Clean up sandbox on error
            liftIO $ unmountAllBindMounts sandboxDir
            liftIO $ removePathForcibly sandboxDir `catch` \(_ :: SomeException) -> do
                liftIO $ setRecursiveWritePermissions sandboxDir
                liftIO $ removePathForcibly sandboxDir `catch` \(_ :: SomeException) ->
                    return ()
            throwError e

    -- Clean up sandbox unless config says to keep it
    unless (sandboxKeepBuildOutput config) $ do
        liftIO $ unmountAllBindMounts sandboxDir
        liftIO $ removePathForcibly sandboxDir `catch` \(_ :: SomeException) -> do
            liftIO $ setRecursiveWritePermissions sandboxDir
            liftIO $ removePathForcibly sandboxDir `catch` \(_ :: SomeException) ->
                return ()

    return result

-- | Builder-specific implementation of withSandbox using protocol communication
withSandboxViaProtocol :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build 'Builder a) -> TenM 'Build 'Builder a
withSandboxViaProtocol inputs config action = do
    env <- ask
    bid <- gets currentBuildId

    -- Verify we are in Build phase
    currState <- get
    unless (currentPhase currState == Build) $
        throwError $ PhaseError "Sandbox can only be created in Build phase"

    -- Validate all inputs before sending across privilege boundary
    forM_ (Set.toList inputs) $ \path ->
        unless (validateStorePath path) $
            throwError $ SandboxError $ "Invalid store path: " <> storePathToText path

    -- Get sandbox connection from run mode
    case runMode env of
        ClientMode conn -> do
            -- Create a sandbox connection from daemon connection
            sandboxConn <- liftIO $ getSandboxConnection conn

            -- Request a sandbox from the daemon via protocol
            sandboxCreation <- requestSandbox sandboxConn bid inputs config

            case sandboxCreation of
                SandboxCreatedResponse sandboxId sandboxPath -> do
                    -- Run the action in the sandbox with proper error handling
                    result <- (action sandboxPath) `catchError` \e -> do
                        -- Notify daemon of error
                        liftIO $ notifySandboxError sandboxConn sandboxId e
                        -- Re-throw the error
                        throwError e

                    -- Release the sandbox when done
                    liftIO $ releaseSandbox sandboxConn sandboxId

                    return result

                SandboxErrorResponse err ->
                    throwError $ SandboxError $ "Failed to create sandbox: " <> err

                _ -> throwError $ SandboxError "Unexpected response from daemon"

        _ -> throwError $ SandboxError "Builder cannot create sandboxes without daemon connection"

-- | Set up a sandbox with proper namespaces and isolation
setupSandbox :: FilePath -> SandboxConfig -> TenM 'Build 'Daemon ()
setupSandbox sandboxDir config = do
    env <- ask
    bid <- gets currentBuildId

    -- Create basic directory structure with proper permissions
    liftIO $ createAndSetupDir sandboxDir 0o755
    liftIO $ createAndSetupDir (sandboxDir </> "tmp") 0o1777  -- Set tmp directory with sticky bit
    liftIO $ createAndSetupDir (sandboxDir </> "build") 0o755
    liftIO $ createAndSetupDir (sandboxDir </> "out") 0o755
    liftIO $ createAndSetupDir (sandboxDir </> "store") 0o755

    -- Create return derivation directory if needed
    when (sandboxReturnSupport config) $ do
        let returnDir = takeDirectory $ returnDerivationPath sandboxDir
        liftIO $ createAndSetupDir returnDir 0o755

    -- Set ownership for unprivileged user if configured
    uid <- liftIO $ User.getRealUserID
    when (uid == 0 && not (sandboxPrivileged config)) $ do
        -- Get user/group IDs for sandbox
        uid' <- liftIO $ safeGetUserUID (sandboxUser config)
        gid <- liftIO $ safeGetGroupGID (sandboxGroup config)

        -- Set ownership of key directories
        liftIO $ setOwnerAndGroup sandboxDir uid' gid
        liftIO $ setOwnerAndGroup (sandboxDir </> "out") uid' gid
        liftIO $ setOwnerAndGroup (sandboxDir </> "tmp") uid' gid
        liftIO $ setOwnerAndGroup (sandboxDir </> "build") uid' gid

        -- Set ownership for return path if enabled
        when (sandboxReturnSupport config) $ do
            let returnDir = takeDirectory $ returnDerivationPath sandboxDir
            liftIO $ setOwnerAndGroup returnDir uid' gid

    -- Set up Linux namespaces if running as root
    when (uid == 0 && (sandboxUseMountNamespace config || sandboxUseNetworkNamespace config)) $ do
        liftIO $ setupNamespaces config sandboxDir

    -- Make /proc available if using mount namespace
    when (uid == 0 && sandboxUseMountNamespace config) $ do
        let procDir = sandboxDir </> "proc"
        liftIO $ createAndSetupDir procDir 0o555
        liftIO $ mountProc procDir `catch` \(_ :: SomeException) ->
            hPutStrLn stderr $ "Warning: Failed to mount proc in sandbox: " ++ procDir

    -- Set up minimal /dev if using mount namespace
    when (uid == 0 && sandboxUseMountNamespace config) $ do
        liftIO $ setupMinimalDev sandboxDir

    -- Add sandbox proof
    addProof (BuildProof)

-- | Tear down a sandbox, cleaning up resources
teardownSandbox :: FilePath -> TenM 'Build t ()
teardownSandbox sandboxDir = do
    logMsg 2 $ "Tearing down sandbox: " <> T.pack sandboxDir

    -- Check if we need to preserve the return derivation
    let returnPath = returnDerivationPath sandboxDir
    returnExists <- liftIO $ doesFileExist returnPath

    if returnExists
        then do
            -- If there's a return derivation, unmount everything but preserve the file
            liftIO $ unmountAllBindMounts sandboxDir
            logMsg 2 $ "Preserved return derivation at: " <> T.pack returnPath
        else do
            -- Otherwise remove the entire sandbox
            liftIO $ do
                unmountAllBindMounts sandboxDir
                removePathForcibly sandboxDir `catch` \(e :: SomeException) -> do
                    hPutStrLn stderr $ "Warning: Failed to remove sandbox: " ++ show e
                    -- Try to fix permissions and retry
                    setRecursiveWritePermissions sandboxDir
                    removePathForcibly sandboxDir `catch` \(e2 :: SomeException) ->
                        hPutStrLn stderr $ "Warning: Failed to remove sandbox after retry: " ++ show e2
            logMsg 2 $ "Sandbox removed: " <> T.pack sandboxDir

-- | Mount /proc in the sandbox
mountProc :: FilePath -> IO ()
mountProc procDir = do
    withCString procDir $ \destPtr ->
        withCString "proc" $ \fsTypePtr ->
            withCString "none" $ \sourcePtr ->
                withCString "" $ \dataPtr -> do
                    ret <- c_mount sourcePtr destPtr fsTypePtr 0 nullPtr
                    when (ret /= 0) $ do
                        errno <- getErrno
                        throwErrno $ "Failed to mount proc in " ++ procDir

-- | Set up a minimal /dev in the sandbox
setupMinimalDev :: FilePath -> IO ()
setupMinimalDev sandboxDir = do
    let devDir = sandboxDir </> "dev"

    -- Create the dev directory
    createAndSetupDir devDir 0o755

    -- Create essential device nodes
    let essentialDevices = [
            ("null", 1, 3),    -- /dev/null
            ("zero", 1, 5),    -- /dev/zero
            ("random", 1, 8),  -- /dev/random
            ("urandom", 1, 9), -- /dev/urandom
            ("tty", 5, 0)      -- /dev/tty
            ]

    -- Bind mount each device from the host
    forM_ essentialDevices $ \(name, _, _) -> do
        let hostDev = "/dev/" ++ name
        let sandboxDev = devDir </> name

        -- Create empty file as mount point
        writeFile sandboxDev ""
        Posix.setFileMode sandboxDev 0o666

        -- Try to bind mount
        bindMountReadOnly hostDev sandboxDev `catchIOError` \e ->
            hPutStrLn stderr $ "Warning: Failed to bind mount " ++ hostDev ++ ": " ++ show e

-- | Set up Linux namespaces for isolation
setupNamespaces :: SandboxConfig -> FilePath -> IO ()
setupNamespaces config sandboxDir = do
    -- Determine which namespaces to use
    let namespaceFlags = foldr (.|.) 0 [
            if sandboxUseMountNamespace config then cLONE_NEWNS else 0,
            if sandboxUseNetworkNamespace config then cLONE_NEWNET else 0,
            if sandboxUseUserNamespace config then cLONE_NEWUSER else 0
            ]

    -- Unshare namespaces
    when (namespaceFlags /= 0) $ do
        result <- c_unshare namespaceFlags
        when (result /= 0) $ do
            errno <- getErrno
            let errnoType = errnoToIOError "unshare" errno Nothing Nothing
            if namespaceFlags == cLONE_NEWNET && isPermissionError errnoType
                then hPutStrLn stderr "Warning: Failed to create network namespace (requires CAP_NET_ADMIN), continuing without network isolation"
                else if sandboxUseUserNamespace config && (isPermissionError errnoType || errno == Errno 95) -- 95 = EOPNOTSUPP
                    then hPutStrLn stderr "Warning: User namespace not supported or permission denied, continuing without user namespace"
                    else throwErrno "Failed to create namespaces"

    -- Make mount propagation private if using mount namespace
    when (sandboxUseMountNamespace config) $ do
        withCString "/" $ \rootPtr ->
            withCString "none" $ \fsTypePtr ->
                withCString "none" $ \sourcePtr ->
                    withCString "" $ \dataPtr -> do
                        ret <- c_mount sourcePtr rootPtr fsTypePtr (mS_REC .|. mS_PRIVATE) nullPtr
                        when (ret /= 0) $ do
                            errno <- getErrno
                            hPutStrLn stderr $ "Warning: Failed to make mounts private: " ++ show (case errno of Errno n -> n)

-- | Helper to create a directory and set proper permissions
createAndSetupDir :: FilePath -> FileMode -> IO ()
createAndSetupDir dir mode = do
    -- Create the directory if it doesn't exist
    createDirectoryIfMissing True dir

    -- Set standard permissions
    perms <- getPermissions dir
    setPermissions dir (Directory.setOwnerExecutable True $
                        Directory.setOwnerWritable True $
                        Directory.setOwnerReadable True $ perms)

    -- Set Unix permissions directly with the specified mode
    Posix.setFileMode dir mode

-- | Make a path available in the sandbox (read-only or writable)
makePathAvailable :: FilePath -> Bool -> FilePath -> TenM 'Build t ()
makePathAvailable sandboxDir writable sourcePath = do
    env <- ask

    -- Sanitize and normalize source path
    let sourcePath' = FilePath.normalise sourcePath

    -- Security check - prevent escaping the sandbox through .. paths
    when (".." `isInfixOf` sourcePath') $
        throwError $ SandboxError $ "Path contains .. segments: " <> T.pack sourcePath'

    -- Skip if source doesn't exist
    sourceExists <- liftIO $ doesPathExist sourcePath'
    unless sourceExists $ do
        liftIO $ hPutStrLn stderr $ "Path does not exist, skipping: " ++ sourcePath'
        return ()

    -- Determine destination path within sandbox
    let relPath = makeRelative "/" sourcePath'
    let destPath = FilePath.normalise $ sandboxDir </> relPath

    -- Security check - ensure destination is within sandbox
    let sandboxPath' = FilePath.normalise sandboxDir
    when (not $ sandboxPath' `isPrefixOf` destPath) $
        throwError $ SandboxError $ "Path escapes sandbox: " <> T.pack destPath

    -- Create parent directory structure
    liftIO $ createDirectoryIfMissing True (takeDirectory destPath)

    -- Bind mount the source to the destination
    isDir <- liftIO $ doesDirectoryExist sourcePath'

    if isDir
        then do
            -- For directories, create the destination and bind mount
            liftIO $ createAndSetupDir destPath 0o755
            liftIO $ bindMount sourcePath' destPath writable `catchIOError` \e -> do
                liftIO $ hPutStrLn stderr $ "Warning: Failed to bind mount directory " ++
                                  sourcePath' ++ " to " ++ destPath ++ ": " ++ show e
                -- Try copying instead as fallback
                liftIO $ copyDirectoryRecursive sourcePath' destPath
        else do
            -- For files, create an empty file and bind mount
            -- Create parent directory if needed
            liftIO $ createDirectoryIfMissing True (takeDirectory destPath)

            -- Create an empty file as mount point
            liftIO $ writeFile destPath ""
            liftIO $ Posix.setFileMode destPath 0o644

            -- Try to mount the source file
            liftIO $ bindMount sourcePath' destPath writable `catchIOError` \e -> do
                liftIO $ hPutStrLn stderr $ "Warning: Failed to bind mount file " ++
                                  sourcePath' ++ " to " ++ destPath ++ ": " ++ show e
                -- Try copying instead as fallback
                liftIO $ copyFile sourcePath' destPath
                when (not writable) $ do
                    -- Make read-only
                    perms <- getPermissions destPath
                    setPermissions destPath $ Directory.setOwnerWritable False perms

-- | Recursively copy a directory (fallback when bind mount fails)
copyDirectoryRecursive :: FilePath -> FilePath -> IO ()
copyDirectoryRecursive src dst = do
    -- Create destination directory if it doesn't exist
    createDirectoryIfMissing True dst

    -- Copy directory permissions
    (perms, mode) <- Posix.getFileStatus src >>= \stat -> do
        perms <- getPermissions src
        return (perms, Posix.fileMode stat)

    setPermissions dst perms
    Posix.setFileMode dst mode

    -- List source contents
    entries <- listDirectory src

    -- Process each entry
    forM_ entries $ \entry -> do
        let srcPath = src </> entry
        let dstPath = dst </> entry

        isDir <- doesDirectoryExist srcPath
        if isDir
            then copyDirectoryRecursive srcPath dstPath
            else copyFile srcPath dstPath

-- | Make inputs available in the sandbox
makeInputsAvailable :: FilePath -> Set StorePath -> TenM 'Build t ()
makeInputsAvailable sandboxDir inputs = do
    env <- ask

    -- Validate all inputs before processing
    forM_ (Set.toList inputs) $ \path ->
        unless (validateStorePath path) $
            throwError $ SandboxError $ "Invalid store path: " <> storePathToText path

    -- Process each input
    forM_ (Set.toList inputs) $ \input -> do
        -- Get source path
        let sourcePath = storePathToFilePath input env

        -- Create two paths:
        -- 1. One with the full store path format
        let storeDest = sandboxDir </> "store" </> T.unpack (storeHash input) ++ "-" ++ T.unpack (storeName input)
        -- 2. One with just the name (what the builder expects to find)
        let nameDest = sandboxDir </> T.unpack (storeName input)

        -- Validate and normalize paths
        let storeDest' = FilePath.normalise storeDest
        let nameDest' = FilePath.normalise nameDest

        -- Security checks
        let sandboxPath' = FilePath.normalise sandboxDir
        when (not $ sandboxPath' `isPrefixOf` storeDest') $
            throwError $ SandboxError $ "Path escapes sandbox: " <> T.pack storeDest'
        when (not $ sandboxPath' `isPrefixOf` nameDest') $
            throwError $ SandboxError $ "Path escapes sandbox: " <> T.pack nameDest'

        -- Mount or copy the input to both destinations
        -- Create destination directories
        liftIO $ createDirectoryIfMissing True (takeDirectory storeDest')
        liftIO $ createDirectoryIfMissing True (takeDirectory nameDest')

        -- Create mount points
        isDir <- liftIO $ doesDirectoryExist sourcePath
        if isDir then do
            liftIO $ createAndSetupDir storeDest' 0o755
            liftIO $ createAndSetupDir nameDest' 0o755
        else do
            liftIO $ writeFile storeDest' ""
            liftIO $ writeFile nameDest' ""

            -- Make sure they are writable for the bind mount
            liftIO $ Posix.setFileMode storeDest' 0o644
            liftIO $ Posix.setFileMode nameDest' 0o644

        -- Try to mount the input (read-only)
        liftIO $ catchIOError
            (bindMountReadOnly sourcePath storeDest')
            (\e -> do
                hPutStrLn stderr $ "Warning: Failed to bind mount store path " ++
                                  sourcePath ++ " to " ++ storeDest' ++ ": " ++ show e
                -- Try copying instead as fallback
                if isDir
                    then copyDirectoryRecursive sourcePath storeDest'
                    else copyFile sourcePath storeDest')

        liftIO $ catchIOError
            (bindMountReadOnly sourcePath nameDest')
            (\e -> do
                hPutStrLn stderr $ "Warning: Failed to bind mount store path " ++
                                  sourcePath ++ " to " ++ nameDest' ++ ": " ++ show e
                -- Try copying instead as fallback - if we already copied to storeDest, make a symlink
                storeDestExists <- doesPathExist storeDest'
                if storeDestExists
                    then createFileLink storeDest' nameDest'
                    else if isDir
                        then copyDirectoryRecursive sourcePath nameDest'
                        else copyFile sourcePath nameDest')

-- | Make a system path available in the sandbox
makeSystemPathAvailable :: FilePath -> FilePath -> TenM 'Build t ()
makeSystemPathAvailable sandboxDir systemPath = do
    env <- ask

    -- Sanitize and normalize system path
    let systemPath' = FilePath.normalise systemPath

    -- Security check - prevent escaping the sandbox through .. paths
    when (".." `isInfixOf` systemPath') $
        throwError $ SandboxError $ "Path contains .. segments: " <> T.pack systemPath'

    -- Skip if the path doesn't exist
    pathExists <- liftIO $ doesPathExist systemPath'
    unless pathExists $ do
        liftIO $ hPutStrLn stderr $ "System path does not exist, skipping: " ++ systemPath'
        return ()

    -- Determine the relative path in the sandbox
    let relPath = makeRelative "/" systemPath'
    let sandboxPath = FilePath.normalise $ sandboxDir </> relPath

    -- Security check - ensure destination is within sandbox
    let sandboxPath' = FilePath.normalise sandboxDir
    when (not $ sandboxPath' `isPrefixOf` sandboxPath) $
        throwError $ SandboxError $ "Path escapes sandbox: " <> T.pack sandboxPath

    -- Skip if already mounted
    destExists <- liftIO $ doesPathExist sandboxPath
    when destExists $ do
        liftIO $ hPutStrLn stderr $ "Path already exists in sandbox, skipping: " ++ sandboxPath
        return ()

    -- Create parent directories
    liftIO $ createDirectoryIfMissing True (takeDirectory sandboxPath)

    -- Mount the system path
    isDir <- liftIO $ doesDirectoryExist systemPath'
    if isDir
        then do
            -- Create directory
            liftIO $ createAndSetupDir sandboxPath 0o755
            -- Mount directory (read-only)
            liftIO $ bindMountReadOnly systemPath' sandboxPath `catchIOError` \e -> do
                hPutStrLn stderr $ "Warning: Failed to bind mount system directory " ++
                                  systemPath' ++ " to " ++ sandboxPath ++ ": " ++ show e
                -- Try copying instead as fallback for important system directories
                when (systemPath' `elem` essentialSystemPaths) $ do
                    copyDirectoryRecursive systemPath' sandboxPath
        else do
            -- Create empty file
            liftIO $ writeFile sandboxPath ""
            -- Set proper permissions
            liftIO $ Posix.setFileMode sandboxPath 0o644
            -- Mount file (read-only)
            liftIO $ bindMountReadOnly systemPath' sandboxPath `catchIOError` \e -> do
                hPutStrLn stderr $ "Warning: Failed to bind mount system file " ++
                                  systemPath' ++ " to " ++ sandboxPath ++ ": " ++ show e
                -- Try copying instead as fallback
                copyFile systemPath' sandboxPath
  where
    -- List of essential system paths that should be copied if mounting fails
    essentialSystemPaths = [
        "/bin", "/usr/bin", "/lib", "/usr/lib",
        "/etc/resolv.conf", "/etc/ssl/certs"
        ]

-- | Linux bind mount implementation
bindMount :: FilePath -> FilePath -> Bool -> IO ()
bindMount source dest writable = do
    -- Sanitize and normalize paths
    let source' = FilePath.normalise source
    let dest' = FilePath.normalise dest

    -- Verify paths
    sourceExists <- doesPathExist source'
    unless sourceExists $ throwIO $ userError $ "Source path does not exist: " ++ source'

    -- First mount with MS_BIND
    withCString source' $ \sourcePtr ->
        withCString dest' $ \destPtr ->
            withCString "none" $ \fsTypePtr ->
                withCString "" $ \dataPtr -> do
                    ret <- c_mount sourcePtr destPtr fsTypePtr mS_BIND (castPtr dataPtr)
                    when (ret /= 0) $ do
                        errno <- getErrno
                        throwErrno $ "Failed to bind mount " ++ source' ++ " to " ++ dest' ++
                                    " (errno code: " ++ show (case errno of Errno n -> n) ++ ")"

    -- If read-only, remount with MS_RDONLY
    unless writable $ do
        withCString source' $ \sourcePtr ->
            withCString dest' $ \destPtr ->
                withCString "none" $ \fsTypePtr ->
                    withCString "" $ \dataPtr -> do
                        let flags = mS_BIND .|. mS_REMOUNT .|. mS_RDONLY
                        ret <- c_mount sourcePtr destPtr fsTypePtr flags (castPtr dataPtr)
                        when (ret /= 0) $ do
                            errno <- getErrno
                            -- Don't fail, just warn - the mount is already established
                            hPutStrLn stderr $ "Warning: Failed to remount read-only " ++ dest' ++
                                             " (errno code: " ++ show (case errno of Errno n -> n) ++ ")"

-- | Shorthand for read-only bind mounts
bindMountReadOnly :: FilePath -> FilePath -> IO ()
bindMountReadOnly source dest = bindMount source dest False

-- | Unmount a path
unmountPath :: FilePath -> IO ()
unmountPath path = do
    -- Check if the path exists before attempting to unmount
    exists <- doesPathExist path
    if not exists
        then return () -- Nothing to unmount
        else withCString path $ \pathPtr -> do
            -- First try a normal unmount
            ret <- c_umount2 pathPtr 0

            -- If that fails, try with DETACH flag
            when (ret /= 0) $ do
                ret2 <- c_umount2 pathPtr mNT_DETACH

                -- If both fail, only report if it's not already unmounted
                when (ret2 /= 0) $ do
                    errno <- getErrno
                    unless (isPermissionError (errnoToIOError "umount" errno Nothing Nothing) ||
                            isDoesNotExistError (errnoToIOError "umount" errno Nothing Nothing)) $ do
                        hPutStrLn stderr $ "Warning: Failed to unmount " ++ path ++
                                          " (errno code: " ++ show (case errno of Errno n -> n) ++ ")"

-- | Find and unmount all bind mounts in a sandbox directory
unmountAllBindMounts :: FilePath -> IO ()
unmountAllBindMounts dir = do
    -- Get the normalized and absolute path
    absDir <- canonicalizePath dir `catch` \(_ :: SomeException) -> return dir
    let dir' = FilePath.normalise absDir

    -- Try to read /proc/mounts
    entries <- readMounts dir'

    -- Find mounts that are within our sandbox directory
    -- Sort in reverse order to unmount deepest paths first
    let relevantMounts = reverse $ sort entries

    -- Try to unmount each, ignoring errors
    forM_ relevantMounts $ \path -> do
        catch (unmountPath path) $ \(_ :: SomeException) -> return ()

    -- Force unmount the sandbox dir itself as a final measure
    catch (unmountPath dir') $ \(_ :: SomeException) -> return ()
  where
    -- Read /proc/mounts to find all mounted filesystems
    readMounts :: FilePath -> IO [FilePath]
    readMounts target = do
        exists <- doesFileExist "/proc/mounts"
        if not exists
            then fallbackUnmount target  -- /proc might not be available
            else do
                contents <- readFile "/proc/mounts" `catch`
                           \(_ :: SomeException) -> return ""
                let allMounts = extractMountPaths contents
                -- Filter only mounts under our target directory
                return $ filter (\path -> target `isPrefixOf` path) allMounts

    -- Extract mount paths from /proc/mounts content
    extractMountPaths :: String -> [FilePath]
    extractMountPaths content =
        -- Each line is: device mountpoint fstype options dump pass
        map ((\parts -> if length parts > 1 then parts !! 1 else "")) $
        map words $
        lines content

    -- Fallback unmount strategy when /proc/mounts can't be read
    fallbackUnmount :: FilePath -> IO [FilePath]
    fallbackUnmount target = do
        -- Try common paths within the sandbox
        let paths = [
                target </> "store",
                target </> "out",
                target </> "tmp",
                target </> "build",
                target </> "proc",
                target </> "dev",
                target  -- Unmount the sandbox dir itself last
                ]
        -- Return these paths to be unmounted
        return paths

-- | Spawn a sandboxed process with proper privilege isolation
spawnSandboxedProcess :: FilePath -> FilePath -> [String] -> Map Text Text ->
                       SandboxConfig -> TenM 'Build 'Daemon ProcessID
spawnSandboxedProcess sandboxDir programPath args env config = do
    -- Verify program exists
    programExists <- liftIO $ doesFileExist programPath
    unless programExists $
        throwError $ SandboxError $ "Program does not exist: " <> T.pack programPath

    -- Create builder environment variables
    buildEnv <- gets currentBuildId >>= \bid ->
        return $ Map.insert "TEN_BUILD_ID" (T.pack $ showBuildId bid) env

    -- Spawn the process with proper privilege
    pid <- liftIO $ do
        -- Fork a child process
        forkProcess $ do
            -- Set up namespaces if configured and running as root
            uid <- User.getRealUserID
            when (uid == 0 && (sandboxUseMountNamespace config ||
                              sandboxUseNetworkNamespace config)) $ do
                setupNamespaces config sandboxDir

            -- Change to sandbox directory
            Directory.setCurrentDirectory sandboxDir

            -- Drop privileges if running as root and unprivileged mode is requested
            when (uid == 0 && not (sandboxPrivileged config)) $ do
                dropSandboxPrivileges (sandboxUser config) (sandboxGroup config)

            -- Set resource limits
            setResourceLimits' config

            -- Properly convert environment variables
            let envVars = map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList buildEnv

            -- Execute the program
            executeFile programPath True args (Just envVars)

            -- Should never reach here
            exitImmediately (ExitFailure 127)

    return pid

-- | Run a builder process with proper privilege isolation
runBuilderProcess :: FilePath -> [String] -> [String] -> UserID -> GroupID ->
                    IO (ExitCode, String, String)
runBuilderProcess program args env uid gid = do
    -- Validate program path
    programExists <- doesFileExist program
    unless programExists $
        throwIO $ userError $ "Builder program does not exist: " ++ program

    -- Check if we need to drop privileges (are we root?)
    currentUid <- User.getRealUserID

    -- Create process configuration
    let process = (proc program args) {
            env = Just env,
            std_in = NoStream,
            std_out = CreatePipe,
            std_err = CreatePipe
        }

    if currentUid == 0
        then do
            -- Need to execute with dropped privileges
            -- Use createProcess but drop privileges in the forked child
            bracket
                (do
                    -- Create pipes for stdin/stdout/stderr
                    (readFd, writeFd) <- PosixIO.createPipe
                    (readErrFd, writeErrFd) <- PosixIO.createPipe
                    return (readFd, writeFd, readErrFd, writeErrFd)
                )
                (\(readFd, writeFd, readErrFd, writeErrFd) -> do
                    -- Close file descriptors
                    PosixIO.closeFd readFd
                    PosixIO.closeFd writeFd
                    PosixIO.closeFd readErrFd
                    PosixIO.closeFd writeErrFd
                )
                (\(readFd, writeFd, readErrFd, writeErrFd) -> do
                    -- Fork a child process
                    pid <- forkProcess $ do
                        -- Close read ends of pipes
                        PosixIO.closeFd readFd
                        PosixIO.closeFd readErrFd

                        -- Redirect stdout/stderr
                        PosixIO.dupTo writeFd 1
                        PosixIO.dupTo writeErrFd 2

                        -- Close write ends after dup
                        PosixIO.closeFd writeFd
                        PosixIO.closeFd writeErrFd

                        -- Drop privileges to specified user/group
                        -- Set group first (must be done before dropping user privileges)
                        User.setGroupID gid
                        -- Then set user ID
                        User.setUserID uid

                        -- Execute the program
                        executeFile program True args (Just env)
                        -- Should never reach here
                        exitImmediately (ExitFailure 127)

                    -- Close write ends of pipes in parent
                    PosixIO.closeFd writeFd
                    PosixIO.closeFd writeErrFd

                    -- Create handles from file descriptors
                    stdoutHandle <- PosixIO.fdToHandle readFd
                    stderrHandle <- PosixIO.fdToHandle readErrFd

                    -- Read stdout and stderr
                    stdoutContent <- hGetContents' stdoutHandle
                    stderrContent <- hGetContents' stderrHandle

                    -- Wait for process to exit
                    exitStatus <- getProcessStatus True True pid
                    let exitCode = case exitStatus of
                         Just (Exited ExitSuccess) -> ExitSuccess
                         Just (Exited (ExitFailure n)) -> ExitFailure n
                         Just (Terminated _ _) -> ExitFailure 128 -- Standard Unix signal exit
                         Just (Stopped _) -> ExitFailure 127 -- Stopped processes
                         Nothing -> ExitFailure 1 -- Unknown status

                    -- Close handles
                    hClose stdoutHandle
                    hClose stderrHandle

                    return (exitCode, stdoutContent, stderrContent)
                )
        else do
            -- Just run normally if we're not root
            readCreateProcessWithExitCode process ""
  where
    -- Helper to read all content from a handle immediately
    hGetContents' h = do
        contents <- hGetContents h
        length contents `seq` return contents

-- | Drop privileges to run as unprivileged user in a sandbox
dropSandboxPrivileges :: String -> String -> IO ()
dropSandboxPrivileges userName groupName = do
    -- Get the current (effective) user ID
    euid <- User.getEffectiveUserID

    -- Only proceed if we're running as root
    when (euid == 0) $ do
        -- Get user and group info
        uid <- safeGetUserUID userName
        gid <- safeGetGroupGID groupName

        -- Drop all capabilities first
        dropAllCapabilities

        -- Clear supplementary groups
        User.setGroups []

        -- Set the group ID first (must be done before dropping user privileges)
        User.setGroupID gid

        -- Then set the user ID
        User.setUserID uid

        -- Verify the change
        newEuid <- User.getEffectiveUserID
        when (newEuid == 0) $
            error "Failed to drop privileges - still running as root"

-- | Drop all Linux capabilities
dropAllCapabilities :: IO ()
dropAllCapabilities = do
    -- Get the current capabilities
    capsPtr <- c_cap_get_proc
    when (capsPtr /= nullPtr) $ do
        -- Clear all capabilities
        _ <- c_cap_clear capsPtr
        -- Set the cleared capabilities
        _ <- c_cap_set_proc capsPtr
        -- Free the capabilities structure
        _ <- c_cap_free capsPtr
        return ()

-- | Set resource limits based on sandbox configuration
setResourceLimits' :: SandboxConfig -> IO ()
setResourceLimits' config = do
    -- Set core dump size to 0
    setRLimit rLIMIT_CORE 0 0

    -- Set CPU time limit if configured
    when (sandboxCPULimit config > 0) $
        setRLimit rLIMIT_CPU (fromIntegral $ sandboxCPULimit config)
                             (fromIntegral $ sandboxCPULimit config)

    -- Set memory limit if configured
    when (sandboxMemoryLimit config > 0) $
        let memLimit = fromIntegral (sandboxMemoryLimit config * 1024 * 1024) in
        setRLimit rLIMIT_AS memLimit memLimit

    -- Set file size limit
    let fsLimit = if sandboxDiskLimit config > 0
                  then fromIntegral (sandboxDiskLimit config * 1024 * 1024)
                  else 1024 * 1024 * 1024 -- 1GB default
    setRLimit rLIMIT_FSIZE fsLimit fsLimit

    -- Set process limit if configured
    when (sandboxMaxProcesses config > 0) $
        setRLimit rLIMIT_NPROC (fromIntegral $ sandboxMaxProcesses config)
                               (fromIntegral $ sandboxMaxProcesses config)

    -- Set reasonable number of open files
    setRLimit rLIMIT_NOFILE 1024 1024

-- | Helper to set a resource limit
setRLimit :: CInt -> Integer -> Integer -> IO ()
setRLimit resource softLimit hardLimit = do
    -- Create RLimit structure
    let rlim = RLimit (fromIntegral softLimit) (fromIntegral hardLimit)

    -- Allocate memory for the structure
    alloca $ \rlimPtr -> do
        -- Fill in the structure
        poke rlimPtr rlim

        -- Set the limit
        ret <- c_setrlimit resource rlimPtr
        when (ret /= 0) $ do
            errno <- getErrno
            hPutStrLn stderr $ "Warning: Failed to set resource limit " ++
                             show resource ++ ": " ++ show (case errno of Errno n -> n)

-- | Standard path for returning a derivation from a builder
returnDerivationPath :: FilePath -> FilePath
returnDerivationPath buildDir = buildDir </> "return.drv"

-- | Standard path for inputs in sandbox
sandboxInputPath :: FilePath -> FilePath -> FilePath
sandboxInputPath sandboxDir inputName = sandboxDir </> inputName

-- | Standard path for outputs in sandbox
sandboxOutputPath :: FilePath -> FilePath -> FilePath
sandboxOutputPath sandboxDir outputName = sandboxDir </> "out" </> outputName

-- | Get a sandbox connection from a daemon connection
getSandboxConnection :: DaemonConnection 'Builder -> IO SandboxConnection
getSandboxConnection conn = do
    -- Create request counter and response map
    requestCounter <- newTVarIO 0
    responses <- newTVarIO Map.empty

    -- Create the sandbox connection
    return SandboxConnection {
        sandboxConnSocket = connSocket conn,
        sandboxConnAuthToken = connAuthToken conn,
        sandboxConnUserId = connUserId conn,
        sandboxConnRequestCounter = requestCounter,
        sandboxConnResponses = responses
    }

-- | Request a sandbox from the daemon via protocol
requestSandbox :: SandboxConnection -> BuildId -> Set StorePath -> SandboxConfig ->
                TenM 'Build 'Builder SandboxResponse
requestSandbox conn buildId inputs config = do
    -- Validate all inputs before sending across privilege boundary
    forM_ (Set.toList inputs) $ \path ->
        unless (validateStorePath path) $
            throwError $ SandboxError $ "Invalid store path: " <> storePathToText path

    -- Create the request
    let request = SandboxCreateRequest buildId inputs config

    -- Create a new request ID
    reqId <- liftIO $ atomically $ do
        currentId <- readTVar (sandboxConnRequestCounter conn)
        let newId = currentId + 1
        writeTVar (sandboxConnRequestCounter conn) newId
        return newId

    -- Create a place to store the response
    responseMVar <- liftIO $ newEmptyMVar

    -- Register the request
    liftIO $ atomically $ do
        modifyTVar' (sandboxConnResponses conn) $
            Map.insert reqId responseMVar

    -- Serialize the request
    let requestData = Binary.encode request

    -- Send the request to the daemon
    liftIO $ do
        -- Create a header with the request ID and size
        let header = LBS.toStrict $ Binary.encode (reqId, LBS.length requestData)

        -- Send header followed by request data through the socket
        NetworkBS.sendAll (sandboxConnSocket conn) header
        NetworkBS.sendAll (sandboxConnSocket conn) (LBS.toStrict requestData)

        -- Wait for response with a reasonable timeout (30 seconds)
        response <- timeout 30000000 $ takeMVar responseMVar

        -- Clean up the request entry
        atomically $ modifyTVar' (sandboxConnResponses conn) $
            Map.delete reqId

        -- Return the response or error
        case response of
            Just r -> return r
            Nothing -> return $ SandboxErrorResponse "Timeout waiting for daemon response"

-- | Release a sandbox back to the daemon
releaseSandbox :: SandboxConnection -> Text -> IO ()
releaseSandbox conn sandboxId = do
    -- Create the release request
    let request = SandboxReleaseRequest sandboxId

    -- Create a new request ID
    reqId <- atomically $ do
        currentId <- readTVar (sandboxConnRequestCounter conn)
        let newId = currentId + 1
        writeTVar (sandboxConnRequestCounter conn) newId
        return newId

    -- Create a place to store the response
    responseMVar <- newEmptyMVar

    -- Register the request
    atomically $ do
        modifyTVar' (sandboxConnResponses conn) $
            Map.insert reqId responseMVar

    -- Serialize the request
    let requestData = Binary.encode request

    -- Send the request to the daemon
    -- Create a header with the request ID and size
    let header = LBS.toStrict $ Binary.encode (reqId, LBS.length requestData)

    -- Send header followed by request data through the socket
    NetworkBS.sendAll (sandboxConnSocket conn) header
    NetworkBS.sendAll (sandboxConnSocket conn) (LBS.toStrict requestData)

    -- Wait for response with a timeout (10 seconds)
    _ <- timeout 10000000 $ takeMVar responseMVar

    -- Clean up the request entry
    atomically $ modifyTVar' (sandboxConnResponses conn) $
        Map.delete reqId

    -- We don't care about the response for release operations
    return ()

-- | Notify daemon of sandbox error
notifySandboxError :: SandboxConnection -> Text -> BuildError -> IO ()
notifySandboxError conn sandboxId err = do
    -- Create the error request
    let request = SandboxAbortRequest sandboxId

    -- Create a new request ID
    reqId <- atomically $ do
        currentId <- readTVar (sandboxConnRequestCounter conn)
        let newId = currentId + 1
        writeTVar (sandboxConnRequestCounter conn) newId
        return newId

    -- Create a place to store the response
    responseMVar <- newEmptyMVar

    -- Register the request
    atomically $ do
        modifyTVar' (sandboxConnResponses conn) $
            Map.insert reqId responseMVar

    -- Serialize the request
    let requestData = Binary.encode request

    -- Send the request to the daemon
    -- Create a header with the request ID and size
    let header = LBS.toStrict $ Binary.encode (reqId, LBS.length requestData)

    -- Send header followed by request data through the socket
    NetworkBS.sendAll (sandboxConnSocket conn) header
    NetworkBS.sendAll (sandboxConnSocket conn) (LBS.toStrict requestData)

    -- Wait for response with a timeout (5 seconds)
    _ <- timeout 5000000 $ takeMVar responseMVar

    -- Clean up the request entry
    atomically $ modifyTVar' (sandboxConnResponses conn) $
        Map.delete reqId

    -- We don't care about the response for error notifications
    return ()

-- | Server-side handler for sandbox protocol messages
sandboxProtocolHandler :: SPrivilegeTier 'Daemon -> SandboxRequest ->
                         TenM 'Build 'Daemon SandboxResponse
sandboxProtocolHandler st request = do
    case request of
        SandboxCreateRequest buildId inputs config -> do
            -- Validate all inputs before processing
            forM_ (Set.toList inputs) $ \path ->
                unless (validateStorePath path) $
                    throwError $ SandboxError $ "Invalid store path: " <> storePathToText path

            -- Create a new sandbox
            sandboxDir <- getSandboxDir

            -- Generate a unique identifier
            sandboxId <- liftIO UUID.nextRandom >>= return . T.pack . UUID.toString

            -- Set up the sandbox
            setupSandboxResult <- (do
                setupSandbox sandboxDir config
                makeInputsAvailable sandboxDir inputs
                mapM_ (makePathAvailable sandboxDir False) (Set.toList $ sandboxReadOnlyPaths config)
                mapM_ (makePathAvailable sandboxDir True) (Set.toList $ sandboxWritablePaths config)
                mapM_ (makeSystemPathAvailable sandboxDir) (Set.toList $ sandboxExtraPaths config)
                return $ Right $ SandboxCreatedResponse sandboxId sandboxDir
                ) `catchError` \e -> do
                return $ Left e

            case setupSandboxResult of
                Left e ->
                    return $ SandboxErrorResponse $ "Failed to create sandbox: " <> buildErrorToText e
                Right response ->
                    return response

        SandboxReleaseRequest sandboxId -> do
            -- Get the sandbox path
            getSandboxDirResult <- (Right <$> getSandboxDirFromId sandboxId) `catchError` \e -> do
                return $ Left e

            case getSandboxDirResult of
                Left e ->
                    return $ SandboxErrorResponse $ "Failed to find sandbox: " <> buildErrorToText e
                Right sandboxDir -> do
                    -- Tear down the sandbox
                    teardownResult <- (do
                        teardownSandbox sandboxDir
                        return $ Right ()) `catchError` \e -> do
                            return $ Left e

                    case teardownResult of
                        Left e ->
                            return $ SandboxErrorResponse $ "Failed to release sandbox: " <> buildErrorToText e
                        Right _ ->
                            return $ SandboxReleasedResponse True

        SandboxStatusRequest sandboxId -> do
            -- Get sandbox info
            statusResult <- (Right <$> getSandboxInfo sandboxId) `catchError` \e -> do
                return $ Left e

            case statusResult of
                Left e ->
                    return $ SandboxErrorResponse $ "Failed to get sandbox status: " <> buildErrorToText e
                Right status ->
                    return $ SandboxStatusResponse status

        SandboxAbortRequest sandboxId -> do
            -- Force cleanup of a sandbox
            getSandboxDirResult <- (Right <$> getSandboxDirFromId sandboxId) `catchError` \e -> do
                return $ Left e

            case getSandboxDirResult of
                Left e ->
                    return $ SandboxErrorResponse $ "Failed to find sandbox: " <> buildErrorToText e
                Right sandboxDir -> do
                    -- Force cleanup
                    result <- liftIO $ try $ do
                        unmountAllBindMounts sandboxDir
                        removePathForcibly sandboxDir `catch` \(_ :: SomeException) -> do
                            setRecursiveWritePermissions sandboxDir
                            removePathForcibly sandboxDir `catch` \(_ :: SomeException) -> return ()
                        return ()

                    case result of
                        Left e ->
                            return $ SandboxErrorResponse $ "Failed to abort sandbox: " <> T.pack (show (e :: SomeException))
                        Right _ ->
                            return $ SandboxReleasedResponse True

-- | Get a unique sandbox directory
getSandboxDir :: TenM 'Build 'Daemon FilePath
getSandboxDir = do
    env <- ask
    bid <- gets currentBuildId
    let baseDir = workDir env </> "sandbox"
    liftIO $ createDirectoryIfMissing True baseDir

    -- Generate unique directory using BuildId
    let uniqueName = "build-" ++ showBuildId bid
    let uniqueDir = baseDir </> uniqueName

    liftIO $ createDirectoryIfMissing True uniqueDir

    -- Set proper permissions immediately
    liftIO $ do
        Posix.setFileMode uniqueDir 0o755
        perms <- getPermissions uniqueDir
        setPermissions uniqueDir $ Directory.setOwnerWritable True perms

    return uniqueDir

-- | Lookup sandbox directory from ID (daemon context)
getSandboxDirFromId :: Text -> TenM 'Build 'Daemon FilePath
getSandboxDirFromId sandboxId = do
    env <- ask
    -- In a real implementation, there would be a state database tracking sandboxes
    -- For now, we'll use a simple convention where the sandbox dir is based on the ID
    let baseDir = workDir env </> "sandbox"
    let sandboxDir = baseDir </> T.unpack sandboxId

    -- Verify it exists
    exists <- liftIO $ doesDirectoryExist sandboxDir
    unless exists $
        throwError $ SandboxError $ "Sandbox not found: " <> sandboxId

    return sandboxDir

-- | Get sandbox status information
getSandboxInfo :: Text -> TenM 'Build 'Daemon SandboxStatus
getSandboxInfo sandboxId = do
    -- Get the sandbox directory
    sandboxDir <- getSandboxDirFromId sandboxId

    -- In a real implementation, we'd look up more details from a state database
    -- For now, just create a basic status object
    now <- liftIO getCurrentTime

    return $ SandboxStatus {
        sandboxStatusActive = True,
        sandboxStatusPath = sandboxDir,
        sandboxStatusBuildId = BuildIdFromInt 0,  -- Placeholder
        sandboxStatusCreatedTime = now
    }

-- | Set recursive write permissions to help with cleanup
setRecursiveWritePermissions :: FilePath -> IO ()
setRecursiveWritePermissions path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            -- Make the directory writable
            perms <- getPermissions path
            setPermissions path (Directory.setOwnerWritable True perms)

            -- Process all entries
            entries <- listDirectory path
            forM_ entries $ \entry ->
                setRecursiveWritePermissions (path </> entry)
        else do
            -- Make the file writable
            perms <- getPermissions path
            setPermissions path (Directory.setOwnerWritable True perms)

-- | Safely get a user's UID with fallback options
safeGetUserUID :: String -> IO UserID
safeGetUserUID username = do
    result <- try $ User.getUserEntryForName username
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nobody
            hPutStrLn stderr $ "Warning: User " ++ username ++ " not found, falling back to nobody"
            nobodyResult <- try $ User.getUserEntryForName "nobody"
            case nobodyResult of
                Left (_ :: SomeException) -> do
                    -- Ultimate fallback to current user if even nobody doesn't exist
                    User.getRealUserID
                Right entry ->
                    return $ User.userID entry
        Right entry ->
            return $ User.userID entry

-- | Safely get a group's GID with fallback options
safeGetGroupGID :: String -> IO GroupID
safeGetGroupGID groupname = do
    result <- try $ User.getGroupEntryForName groupname
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nogroup
            hPutStrLn stderr $ "Warning: Group " ++ groupname ++ " not found, falling back to nogroup"
            nogroupResult <- try $ User.getGroupEntryForName "nogroup"
            case nogroupResult of
                Left (_ :: SomeException) -> do
                    -- Try nobody as group
                    nobodyResult <- try $ User.getGroupEntryForName "nobody"
                    case nobodyResult of
                        Left (_ :: SomeException) -> do
                            -- Ultimate fallback to current group
                            getRealGroupID
                        Right entry ->
                            return $ User.groupID entry
                Right entry ->
                    return $ User.groupID entry
        Right entry ->
            return $ User.groupID entry

-- | Get the real group ID
getRealGroupID :: IO GroupID
getRealGroupID = do
    uid <- User.getRealUserID
    entry <- User.getUserEntryForID uid
    return $ User.userGroupID entry

-- | Set owner and group of a file
setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup path uid gid = Posix.setOwnerAndGroup path uid gid

-- | Check if a path exists (file or directory)
doesPathExist :: FilePath -> IO Bool
doesPathExist path = do
    fileExists <- doesFileExist path
    if fileExists
        then return True
        else doesDirectoryExist path

-- | Create a symbolic link
createFileLink :: FilePath -> FilePath -> IO ()
createFileLink target link =
    Posix.createSymbolicLink target link

-- | Get the absolute path of a file
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = Directory.canonicalizePath
