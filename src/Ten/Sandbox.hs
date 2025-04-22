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
    sandboxedProcessConfig,
    prepareSandboxEnvironment,

    -- Privilege management
    dropPrivileges,
    withDroppedPrivileges,
    runBuilderAsUser,

    -- Protocol-based sandbox operations
    requestSandbox,
    releaseSandbox,
    sandboxProtocolHandler
) where

import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets, modify)
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
import Data.Maybe (isJust, fromMaybe, listToMaybe)
import System.Directory
import System.FilePath
import System.Process (createProcess, proc, readCreateProcessWithExitCode, CreateProcess(..), StdStream(..))
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Files as Posix
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import System.Posix.Types (ProcessID, Fd, FileMode, UserID, GroupID)
import qualified System.Posix.User as User
import qualified System.Posix.Resource as Resource
import qualified System.Posix.IO as PosixIO
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1_, throwErrnoIfMinus1, getErrno, Errno(..), errnoToIOError)
import Foreign.C.String (CString, withCString, peekCString, newCString)
import Foreign.C.Types (CInt(..), CULong(..), CLong(..), CSize(..))
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, malloc, free)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Storable
import System.IO.Error (IOError, catchIOError, isPermissionError, isDoesNotExistError)
import System.IO (hPutStrLn, stderr, hClose, Handle)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Socket (Socket)
import Data.Binary (Binary(..), encode, decode)
import qualified Data.ByteString.Lazy as LBS
import System.Random (randomRIO)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Data.Singletons (SingI, fromSing)

import Ten.Core

-- | Daemon connection type for protocol communication
data DaemonConnection = DaemonConnection {
    connSocket :: Socket,                   -- Socket connected to daemon
    connUserId :: UserId,                   -- Authenticated user ID
    connAuthToken :: AuthToken,             -- Authentication token
    connHandle :: Handle,                   -- Socket handle for I/O
    connNextRequestId :: TVar Int,          -- Next request ID counter
    connPendingRequests :: TVar (Map Int (TMVar DaemonResponse)), -- Map of pending requests
    connShutdownFlag :: TVar Bool           -- Shutdown flag
}

-- | Protocol message types for sandbox communication
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

instance Binary SandboxRequest where
    put (SandboxCreateRequest buildId inputs config) = do
        put (0 :: Word8)  -- Tag for CreateRequest
        put (show buildId)
        put (Set.toList inputs)
        put (encodeConfig config)

    put (SandboxReleaseRequest sandboxId) = do
        put (1 :: Word8)  -- Tag for ReleaseRequest
        put sandboxId

    put (SandboxStatusRequest sandboxId) = do
        put (2 :: Word8)  -- Tag for StatusRequest
        put sandboxId

    put (SandboxAbortRequest sandboxId) = do
        put (3 :: Word8)  -- Tag for AbortRequest
        put sandboxId

    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> do  -- CreateRequest
                buildIdStr <- get
                inputs <- Set.fromList <$> get
                configData <- get
                return $ SandboxCreateRequest (read buildIdStr) inputs (decodeConfig configData)
            1 -> SandboxReleaseRequest <$> get
            2 -> SandboxStatusRequest <$> get
            3 -> SandboxAbortRequest <$> get
            _ -> fail $ "Unknown SandboxRequest tag: " ++ show tag

-- | Sandbox response types
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

instance Binary SandboxResponse where
    put (SandboxCreatedResponse sid path) = do
        put (0 :: Word8)  -- Tag for CreatedResponse
        put sid
        put path

    put (SandboxReleasedResponse success) = do
        put (1 :: Word8)  -- Tag for ReleasedResponse
        put success

    put (SandboxStatusResponse status) = do
        put (2 :: Word8)  -- Tag for StatusResponse
        put (sandboxStatusActive status)
        put (sandboxStatusPath status)
        put (show $ sandboxStatusBuildId status)
        put (show $ sandboxStatusCreatedTime status)

    put (SandboxErrorResponse err) = do
        put (3 :: Word8)  -- Tag for ErrorResponse
        put err

    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> SandboxCreatedResponse <$> get <*> get
            1 -> SandboxReleasedResponse <$> get
            2 -> do
                active <- get
                path <- get
                buildIdStr <- get
                timeStr <- get
                return $ SandboxStatusResponse $ SandboxStatus
                    active
                    path
                    (read buildIdStr)
                    (read timeStr)
            3 -> SandboxErrorResponse <$> get
            _ -> fail $ "Unknown SandboxResponse tag: " ++ show tag

-- Helper functions to encode/decode sandbox config
encodeConfig :: SandboxConfig -> ByteString
encodeConfig = LBS.toStrict . encode . configToList

decodeConfig :: ByteString -> SandboxConfig
decodeConfig = listToConfig . decode . LBS.fromStrict

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
    ("timeoutSecs", show $ sandboxTimeoutSecs config)
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
            sandboxTimeoutSecs = getInt "timeoutSecs"
        }

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
    sandboxTimeoutSecs :: Int              -- Timeout in seconds (0 = no timeout)
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
    sandboxTimeoutSecs = 0                 -- No timeout by default
}

-- Helper function to convert BuildId to String for filesystem paths
showBuildId :: BuildId -> String
showBuildId (BuildId u) = "build-" ++ show (hashUnique u)
showBuildId (BuildIdFromInt n) = "build-" ++ show n

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

-- | Run an action within a build sandbox
-- Phase and privilege tier aware implementation
withSandbox :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build t a) -> TenM 'Build t a
withSandbox inputs config action = do
    env <- ask
    state <- get

    -- Verify we are in Build phase
    unless (currentPhase state == Build) $
        throwError $ PhaseError "Sandbox can only be created in Build phase"

    -- Use the right implementation based on privilege tier
    case currentPrivilegeTier env of
        -- When running as daemon, we can create the sandbox directly
        Daemon -> withSPhase sBuild $ \sp ->
                   withSPrivilegeTier sDaemon $ \st ->
                       withSandboxDaemon sp st inputs config action

        -- When running as builder, we must use the protocol
        Builder -> withSPhase sBuild $ \sp ->
                    withSPrivilegeTier sBuilder $ \st -> do
                        -- Get daemon connection from run mode
                        case runMode env of
                            ClientMode conn ->
                                withSandboxViaProtocol sp st conn inputs config action
                            _ ->
                                throwError $ SandboxError "Builder cannot create sandboxes without daemon connection"

-- | Create a sandbox in daemon context (privileged operation)
withSandboxDaemon :: SPhase 'Build -> SPrivilegeTier 'Daemon -> Set StorePath -> SandboxConfig
                  -> (FilePath -> TenM 'Build t a) -> TenM 'Build 'Daemon a
withSandboxDaemon sp _ inputs config action = do
    env <- ask
    state <- get

    -- Get unique sandbox directory
    sandboxDir <- getSandboxDir

    -- Create a sandboxing function with proper daemon privileges
    let sandboxFunc dir = do
            -- Create the sandbox with appropriate namespaces and mounts
            setupSandbox dir config

            -- Make store inputs available in the sandbox
            makeInputsAvailable dir inputs

            -- Add read-only and writable paths from config
            mapM_ (makePathAvailable dir False) (Set.toList $ sandboxReadOnlyPaths config)
            mapM_ (makePathAvailable dir True) (Set.toList $ sandboxWritablePaths config)

            -- Add additional system paths specified in config
            mapM_ (makeSystemPathAvailable dir) (Set.toList $ sandboxExtraPaths config)

            -- Log sandbox creation
            logMsg 1 $ "Created sandbox at: " <> T.pack dir

            -- Run the action inside the sandbox with appropriate privilege transition
            -- Daemon needs to drop privileges to builder for sandbox execution
            withSPrivilegeTier sBuilder $ \builderSt ->
                transitionPrivilege DropPrivilege (action dir)

    -- Use bracket to ensure proper cleanup even if exceptions occur
    liftIO (bracket
                (return sandboxDir)
                (\dir -> do
                    -- Ensure any mounts are unmounted before removing
                    unmountAllBindMounts dir `catch` \(_ :: SomeException) -> return ()

                    -- Remove the sandbox directory with force to handle any permission issues
                    let retryRemove = do
                          result <- try $ removePathForcibly dir
                          case result of
                             Left (e :: SomeException) -> do
                                 hPutStrLn stderr $ "Warning: Failed to remove sandbox: " ++ show e
                                 -- Try to fix permissions and retry
                                 setRecursiveWritePermissions dir
                                 removePathForcibly dir `catch` \(e2 :: SomeException) ->
                                     hPutStrLn stderr $ "Warning: Failed to remove sandbox after retry: " ++ show e2
                             Right _ -> return ()
                    retryRemove)
                (\dir -> runSandboxed env state sp sDaemon sandboxFunc dir))
        >>= either throwError return

-- | Create a sandbox via daemon protocol (for builder context)
withSandboxViaProtocol :: SPhase 'Build -> SPrivilegeTier 'Builder -> DaemonConnection
                       -> Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build t a)
                       -> TenM 'Build 'Builder a
withSandboxViaProtocol _ _ conn inputs config action = do
    env <- ask
    state <- get
    bid <- gets currentBuildId

    -- Request a sandbox from the daemon via protocol
    sandboxResp <- requestSandbox conn bid inputs config

    case sandboxResp of
        SandboxCreatedResponse sandboxId sandboxPath -> do
            -- Run the action in the sandbox
            result <- action sandboxPath `catchError` \e -> do
                -- Notify daemon of error
                notifySandboxError conn sandboxId e
                -- Re-throw the error
                throwError e

            -- Release the sandbox when done
            releaseSandbox conn sandboxId

            return result

        SandboxErrorResponse err ->
            throwError $ SandboxError $ "Failed to create sandbox: " <> err

        _ -> throwError $ SandboxError "Unexpected response from daemon when requesting sandbox"

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
        setPermissions uniqueDir $ setOwnerWritable True perms

    return uniqueDir

-- | Set up a sandbox with proper namespaces and isolation
setupSandbox :: FilePath -> SandboxConfig -> TenM 'Build 'Daemon ()
setupSandbox sandboxDir config = do
    -- Create basic directory structure with proper permissions
    liftIO $ do
        createAndSetupDir sandboxDir 0o755
        createAndSetupDir (sandboxDir </> "tmp") 0o1777  -- Set tmp directory with sticky bit
        createAndSetupDir (sandboxDir </> "build") 0o755
        createAndSetupDir (sandboxDir </> "out") 0o755
        createAndSetupDir (sandboxDir </> "store") 0o755

    -- Create return derivation directory if needed
    when (sandboxReturnSupport config) $ do
        let returnDir = takeDirectory $ returnDerivationPath sandboxDir
        liftIO $ createAndSetupDir returnDir 0o755

    -- Set ownership for unprivileged user if configured
    liftIO $ do
        uid <- User.getRealUserID
        when (uid == 0 && not (sandboxPrivileged config)) $ do
            -- Get user/group IDs for sandbox
            uid' <- safeGetUserUID (sandboxUser config)
            gid <- safeGetGroupGID (sandboxGroup config)

            -- Set ownership of key directories
            setOwnerAndGroup sandboxDir uid' gid
            setOwnerAndGroup (sandboxDir </> "out") uid' gid
            setOwnerAndGroup (sandboxDir </> "tmp") uid' gid
            setOwnerAndGroup (sandboxDir </> "build") uid' gid

            -- Set ownership for return path if enabled
            when (sandboxReturnSupport config) $ do
                let returnDir = takeDirectory $ returnDerivationPath sandboxDir
                setOwnerAndGroup returnDir uid' gid

    -- Set up Linux namespaces if running as root
    isRoot <- liftIO $ do
        uid <- User.getRealUserID
        return (uid == 0)

    when (isRoot && (sandboxUseMountNamespace config || sandboxUseNetworkNamespace config)) $ do
        liftIO $ setupNamespaces config sandboxDir

    -- Make /proc available if using mount namespace
    when (isRoot && sandboxUseMountNamespace config) $ do
        liftIO $ do
            let procDir = sandboxDir </> "proc"
            createAndSetupDir procDir 0o555
            mountProc procDir `catch` \(_ :: SomeException) ->
                hPutStrLn stderr $ "Warning: Failed to mount proc in sandbox: " ++ procDir

    -- Set up minimal /dev if using mount namespace
    when (isRoot && sandboxUseMountNamespace config) $ do
        liftIO $ setupMinimalDev sandboxDir

    -- Add sandbox proof
    addProof BuildProof

    logMsg 2 $ "Sandbox setup completed: " <> T.pack sandboxDir

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
    setPermissions dir (setOwnerExecutable True $
                        setOwnerWritable True $
                        setOwnerReadable True $ perms)

    -- Set Unix permissions directly with the specified mode
    Posix.setFileMode dir mode

-- | Helper to run a sandbox action with the Ten monad context
runSandboxed :: BuildEnv -> BuildState -> SPhase 'Build -> SPrivilegeTier 'Daemon
             -> (FilePath -> TenM 'Build 'Daemon a) -> FilePath -> IO (Either BuildError a)
runSandboxed env state sp st action sandboxDir = do
    result <- runTen sp st (action sandboxDir) env state
    return $ case result of
        Left err -> Left err
        Right (val, _) -> Right val

-- | Make a path available in the sandbox (read-only or writable)
makePathAvailable :: FilePath -> Bool -> FilePath -> TenM 'Build t ()
makePathAvailable sandboxDir writable sourcePath = do
    -- Sanitize and normalize source path
    let sourcePath' = normalise sourcePath

    -- Security check - prevent escaping the sandbox through .. paths
    when (".." `isInfixOf` sourcePath') $
        throwError $ SandboxError $ "Path contains .. segments: " <> T.pack sourcePath'

    -- Skip if source doesn't exist
    sourceExists <- liftIO $ doesPathExist sourcePath'
    unless sourceExists $ do
        logMsg 3 $ "Path does not exist, skipping: " <> T.pack sourcePath'
        return ()

    -- Determine destination path within sandbox
    let relPath = makeRelative "/" sourcePath'
    let destPath = normalise $ sandboxDir </> relPath

    -- Security check - ensure destination is within sandbox
    let sandboxPath' = normalise sandboxDir
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
                hPutStrLn stderr $ "Warning: Failed to bind mount directory " ++
                                  sourcePath' ++ " to " ++ destPath ++ ": " ++ show e
                -- Try copying instead as fallback
                copyDirectoryRecursive sourcePath' destPath

            logMsg 3 $ "Made available directory: " <> T.pack sourcePath' <>
                      " -> " <> T.pack destPath <>
                      (if writable then " (writable)" else " (read-only)")
        else do
            -- For files, create an empty file and bind mount
            liftIO $ do
                -- Create parent directory if needed
                createDirectoryIfMissing True (takeDirectory destPath)

                -- Create an empty file as mount point
                writeFile destPath ""
                Posix.setFileMode destPath 0o644

                -- Try to mount the source file
                bindMount sourcePath' destPath writable `catchIOError` \e -> do
                    hPutStrLn stderr $ "Warning: Failed to bind mount file " ++
                                      sourcePath' ++ " to " ++ destPath ++ ": " ++ show e
                    -- Try copying instead as fallback
                    copyFile sourcePath' destPath
                    when (not writable) $ do
                        -- Make read-only
                        perms <- getPermissions destPath
                        setPermissions destPath $ setOwnerWritable False perms

            logMsg 3 $ "Made available file: " <> T.pack sourcePath' <>
                      " -> " <> T.pack destPath <>
                      (if writable then " (writable)" else " (read-only)")

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
        let storeDest' = normalise storeDest
        let nameDest' = normalise nameDest

        -- Security checks
        let sandboxPath' = normalise sandboxDir
        when (not $ sandboxPath' `isPrefixOf` storeDest') $
            throwError $ SandboxError $ "Path escapes sandbox: " <> T.pack storeDest'
        when (not $ sandboxPath' `isPrefixOf` nameDest') $
            throwError $ SandboxError $ "Path escapes sandbox: " <> T.pack nameDest'

        -- Mount or copy the input to both destinations
        liftIO $ do
            -- Create destination directories
            createDirectoryIfMissing True (takeDirectory storeDest')
            createDirectoryIfMissing True (takeDirectory nameDest')

            -- Create mount points
            isDir <- doesDirectoryExist sourcePath
            if isDir then do
                createAndSetupDir storeDest' 0o755
                createAndSetupDir nameDest' 0o755
            else do
                writeFile storeDest' ""
                writeFile nameDest' ""

                -- Make sure they are writable for the bind mount
                Posix.setFileMode storeDest' 0o644
                Posix.setFileMode nameDest' 0o644

            -- Try to mount the input (read-only)
            catchIOError
                (bindMountReadOnly sourcePath storeDest')
                (\e -> do
                    hPutStrLn stderr $ "Warning: Failed to bind mount store path " ++
                                      sourcePath ++ " to " ++ storeDest' ++ ": " ++ show e
                    -- Try copying instead as fallback
                    if isDir
                        then copyDirectoryRecursive sourcePath storeDest'
                        else copyFile sourcePath storeDest')

            catchIOError
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

        logMsg 3 $ "Made input available: " <> storeHash input <> "-" <> storeName input

    logMsg 2 $ "Made " <> T.pack (show $ Set.size inputs) <> " inputs available in sandbox"

-- | Make a system path available in the sandbox
makeSystemPathAvailable :: FilePath -> FilePath -> TenM 'Build t ()
makeSystemPathAvailable sandboxDir systemPath = do
    -- Sanitize and normalize system path
    let systemPath' = normalise systemPath

    -- Security check - prevent escaping the sandbox through .. paths
    when (".." `isInfixOf` systemPath') $
        throwError $ SandboxError $ "Path contains .. segments: " <> T.pack systemPath'

    -- Skip if the path doesn't exist
    pathExists <- liftIO $ doesPathExist systemPath'
    unless pathExists $ do
        logMsg 3 $ "System path does not exist, skipping: " <> T.pack systemPath'
        return ()

    -- Determine the relative path in the sandbox
    let relPath = makeRelative "/" systemPath'
    let sandboxPath = normalise $ sandboxDir </> relPath

    -- Security check - ensure destination is within sandbox
    let sandboxPath' = normalise sandboxDir
    when (not $ sandboxPath' `isPrefixOf` sandboxPath) $
        throwError $ SandboxError $ "Path escapes sandbox: " <> T.pack sandboxPath

    -- Skip if already mounted
    destExists <- liftIO $ doesPathExist sandboxPath
    when destExists $ do
        logMsg 3 $ "Path already exists in sandbox, skipping: " <> T.pack sandboxPath
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

            logMsg 3 $ "System directory mounted: " <> T.pack systemPath'
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

            logMsg 3 $ "System file mounted: " <> T.pack systemPath'
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
    let source' = normalise source
    let dest' = normalise dest

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
    let dir' = normalise absDir

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

-- | Prepare process configuration for a command run in the sandbox
sandboxedProcessConfig :: FilePath -> FilePath -> [String] -> Map Text Text -> SandboxConfig -> CreateProcess
sandboxedProcessConfig sandboxDir programPath args envVars config =
    (proc programPath args)
        { cwd = Just sandboxDir
        , env = Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList envVars
        , std_in = NoStream
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

-- | Prepare environment variables for sandbox
prepareSandboxEnvironment :: BuildEnv -> BuildState 'Build -> FilePath -> Map Text Text -> Map Text Text
prepareSandboxEnvironment env buildState sandboxDir extraEnv =
    Map.unions
        [ -- Base essential environment variables
          baseEnv
          -- Custom environment variables from extra configuration
        , extraEnv
          -- Security-related environment variables
        , securityEnv
          -- Build information variables
        , buildInfoEnv
        ]
  where
    -- Core environment variables that all sandboxes need
    baseEnv = Map.fromList
        [ ("TEN_STORE", T.pack $ storeLocation env)
        , ("TEN_BUILD_DIR", T.pack sandboxDir)
        , ("TEN_OUT", T.pack $ sandboxDir </> "out")
        , ("TEN_RETURN_PATH", T.pack $ returnDerivationPath sandboxDir)
        , ("TEN_TMP", T.pack $ sandboxDir </> "tmp")
        , ("PATH", "/bin:/usr/bin:/usr/local/bin")     -- Explicit PATH
        , ("HOME", T.pack sandboxDir)                  -- Set HOME to sandbox
        , ("TMPDIR", T.pack $ sandboxDir </> "tmp")    -- Set TMPDIR
        , ("TMP", T.pack $ sandboxDir </> "tmp")       -- Alternative tmp env var
        , ("TEMP", T.pack $ sandboxDir </> "tmp")      -- Another alternative
        , ("TEN_BUILD_ID", T.pack $ showBuildId $ currentBuildId buildState) -- Current build ID
        , ("TEN_DERIVATION_NAME", "unknown") -- Will be set by caller for actual builds
        , ("TEN_SYSTEM", "unknown") -- Will be set by caller for actual builds
        ]

    -- Security-related variables
    securityEnv = Map.fromList
        [ ("TEN_SANDBOX", "1")           -- Indicate running in sandbox
        , ("TEN_RESTRICTED", "1")        -- Indicate restricted environment
        , ("TEN_UNPRIVILEGED", "1")      -- Indicate unprivileged execution
        ]

    -- Build information variables
    buildInfoEnv = Map.fromList
        [ ("TEN_BUILD_ID", T.pack $ showBuildId $ currentBuildId buildState)
        ]

-- | Drop privileges to run as unprivileged user
-- Only available in daemon context
dropPrivileges :: String -> String -> TenM 'Build 'Daemon ()
dropPrivileges userName groupName = liftIO $ do
    -- Get the current (effective) user ID
    euid <- User.getEffectiveUserID

    -- Only proceed if we're running as root
    when (euid == 0) $ do
        -- Get user and group info
        uid <- safeGetUserUID userName
        gid <- safeGetGroupGID groupName

        -- Drop all capabilities first
        dropAllCapabilities

        -- Set resource limits for unprivileged user
        setResourceLimits

        -- Set the group ID first (must be done before dropping user privileges)
        User.setGroupID gid

        -- Then set the user ID
        User.setUserID uid

        -- Verify the change
        newEuid <- User.getEffectiveUserID
        when (newEuid == 0) $
            error "Failed to drop privileges - still running as root"

-- | Execute action with dropped privileges
withDroppedPrivileges :: String -> String -> IO a -> IO a
withDroppedPrivileges user group action =
    bracket
        (do
            -- Check if we need to drop privileges (are we root?)
            uid <- User.getEffectiveUserID
            if uid == 0
                then do
                    -- Save current uid/gid
                    euid <- User.getEffectiveUserID
                    egid <- User.getEffectiveGroupID

                    -- Drop privileges
                    uid' <- safeGetUserUID user
                    gid' <- safeGetGroupGID group

                    -- Change group first, then user
                    User.setGroupID gid'
                    User.setUserID uid'

                    -- Return the original IDs for restoration
                    return $ Just (euid, egid)
                else
                    -- Not running as root, nothing to do
                    return Nothing)
        (\case
            Just (euid, egid) -> do
                -- Restore original privileges
                -- If we can't, just log a warning
                try (do
                    User.setGroupID egid
                    User.setUserID euid) :: IO (Either SomeException ())
                return ()
            Nothing ->
                return ())
        (\_ -> action)

-- | Run a builder process as an unprivileged user
runBuilderAsUser :: FilePath -> [String] -> String -> String -> Map Text Text -> IO (ExitCode, String, String)
runBuilderAsUser program args user group env = do
    -- Validate paths
    let program' = normalise program
    programExists <- doesFileExist program'
    unless programExists $
        throwIO $ userError $ "Builder program does not exist: " ++ program'

    -- Set executable bit if needed
    fileStatus <- Posix.getFileStatus program'
    let mode = Posix.fileMode fileStatus
    unless (mode .&. 0o100 /= 0) $ do
        Posix.setFileMode program' (mode .|. 0o100)

    -- Get current UID to check if we're root
    uid <- User.getRealUserID

    if uid == 0
        then do
            -- We're root, so use privilege dropping
            withDroppedPrivileges user group $ do
                -- Execute the builder as the unprivileged user
                execBuilder program' args env
        else do
            -- Not running as root, just execute directly
            execBuilder program' args env
  where
    -- Execute the builder with environment variables
    execBuilder :: FilePath -> [String] -> Map Text Text -> IO (ExitCode, String, String)
    execBuilder prog args' env' = do
        -- Set up process configuration
        let process = (proc prog args') {
                env = Just $ map (\(k, v) -> (T.unpack k, T.unpack v)) $ Map.toList env'
            }

        -- Run the process and capture output
        readCreateProcessWithExitCode process ""

-- | Standard path for returning a derivation from a builder
returnDerivationPath :: FilePath -> FilePath
returnDerivationPath buildDir = buildDir </> "return.drv"

-- | Standard path for inputs in sandbox
sandboxInputPath :: FilePath -> FilePath -> FilePath
sandboxInputPath sandboxDir inputName = sandboxDir </> inputName

-- | Standard path for outputs in sandbox
sandboxOutputPath :: FilePath -> FilePath -> FilePath
sandboxOutputPath sandboxDir outputName = sandboxDir </> "out" </> outputName

-- | Request a sandbox from the daemon via protocol
requestSandbox :: DaemonConnection -> BuildId -> Set StorePath -> SandboxConfig -> TenM 'Build 'Builder SandboxResponse
requestSandbox conn buildId inputs config = do
    -- Create sandbox request
    let request = SandboxCreateRequest buildId inputs config

    -- Generate a request ID for tracking the response
    reqId <- liftIO $ atomically $ do
        nextId <- readTVar (connNextRequestId conn)
        writeTVar (connNextRequestId conn) (nextId + 1)
        return nextId

    -- Create response variable
    respVar <- liftIO $ newEmptyTMVarIO

    -- Register the pending request
    liftIO $ atomically $ modifyTVar (connPendingRequests conn) $ Map.insert reqId respVar

    -- Serialize and send the request
    let encodedRequest = LBS.toStrict $ encode (reqId, request)
    liftIO $ BS.hPut (connHandle conn) encodedRequest >> hFlush (connHandle conn)

    -- Wait for the response (with timeout)
    result <- liftIO $ atomically $ do
        -- Wait for the response to be put in the TMVar
        takeTMVar respVar

    -- Clean up the request mapping
    liftIO $ atomically $ modifyTVar (connPendingRequests conn) $ Map.delete reqId

    -- Return the response
    return result

-- | Release a sandbox back to the daemon
releaseSandbox :: DaemonConnection -> Text -> TenM 'Build 'Builder ()
releaseSandbox conn sandboxId = do
    -- Create release request
    let request = SandboxReleaseRequest sandboxId

    -- Generate a request ID for tracking the response
    reqId <- liftIO $ atomically $ do
        nextId <- readTVar (connNextRequestId conn)
        writeTVar (connNextRequestId conn) (nextId + 1)
        return nextId

    -- Create response variable
    respVar <- liftIO $ newEmptyTMVarIO

    -- Register the pending request
    liftIO $ atomically $ modifyTVar (connPendingRequests conn) $ Map.insert reqId respVar

    -- Serialize and send the request
    let encodedRequest = LBS.toStrict $ encode (reqId, request)
    liftIO $ BS.hPut (connHandle conn) encodedRequest >> hFlush (connHandle conn)

    -- Wait for the response (with timeout)
    result <- liftIO $ atomically $ do
        -- Wait for the response to be put in the TMVar
        takeTMVar respVar

    -- Clean up the request mapping
    liftIO $ atomically $ modifyTVar (connPendingRequests conn) $ Map.delete reqId

    -- Log the result
    case result of
        SandboxReleasedResponse True ->
            logMsg 2 $ "Sandbox " <> sandboxId <> " released successfully"
        SandboxErrorResponse err ->
            logMsg 1 $ "Error releasing sandbox " <> sandboxId <> ": " <> err
        _ ->
            logMsg 1 $ "Unexpected response when releasing sandbox " <> sandboxId

-- | Notify daemon of sandbox error
notifySandboxError :: DaemonConnection -> Text -> BuildError -> TenM 'Build 'Builder ()
notifySandboxError conn sandboxId err = do
    -- Create abort request with error info
    let request = SandboxAbortRequest sandboxId

    -- Generate a request ID for tracking the response
    reqId <- liftIO $ atomically $ do
        nextId <- readTVar (connNextRequestId conn)
        writeTVar (connNextRequestId conn) (nextId + 1)
        return nextId

    -- Create response variable
    respVar <- liftIO $ newEmptyTMVarIO

    -- Register the pending request
    liftIO $ atomically $ modifyTVar (connPendingRequests conn) $ Map.insert reqId respVar

    -- Serialize and send the request
    let encodedRequest = LBS.toStrict $ encode (reqId, request)
    liftIO $ BS.hPut (connHandle conn) encodedRequest >> hFlush (connHandle conn)

    -- Wait for the response (with timeout)
    liftIO $ atomically $ do
        -- Wait for the response to be put in the TMVar
        takeTMVar respVar

    -- Clean up the request mapping
    liftIO $ atomically $ modifyTVar (connPendingRequests conn) $ Map.delete reqId

    -- Log the error
    logMsg 1 $ "Notified daemon of error in sandbox " <> sandboxId <> ": " <> T.pack (show err)

-- | Server-side handler for sandbox protocol messages
sandboxProtocolHandler :: SPrivilegeTier 'Daemon -> SandboxRequest -> TenM 'Build 'Daemon SandboxResponse
sandboxProtocolHandler st request = do
    case request of
        SandboxCreateRequest buildId inputs config -> do
            -- Create sandbox directory
            sandboxDir <- getSandboxDir

            -- Generate unique sandbox ID
            sandboxId <- liftIO $ do
                uuid <- UUID.nextRandom
                return $ T.pack $ UUID.toString uuid

            -- Set up the sandbox with daemon privileges
            result <- try $ do
                setupSandbox sandboxDir config
                makeInputsAvailable sandboxDir inputs

                -- Add extra paths from config
                mapM_ (makePathAvailable sandboxDir False) (Set.toList $ sandboxReadOnlyPaths config)
                mapM_ (makePathAvailable sandboxDir True) (Set.toList $ sandboxWritablePaths config)
                mapM_ (makeSystemPathAvailable sandboxDir) (Set.toList $ sandboxExtraPaths config)

                return $ SandboxCreatedResponse sandboxId sandboxDir

            case result of
                Left (e :: SomeException) ->
                    return $ SandboxErrorResponse $ "Failed to create sandbox: " <> T.pack (show e)
                Right response ->
                    return response

        SandboxReleaseRequest sandboxId -> do
            -- Get the sandbox directory from ID
            sandboxDir <- getSandboxDirFromId sandboxId

            -- Tear down the sandbox
            result <- try $ teardownSandbox sandboxDir

            case result of
                Left (e :: SomeException) ->
                    return $ SandboxErrorResponse $ "Failed to release sandbox: " <> T.pack (show e)
                Right _ ->
                    return $ SandboxReleasedResponse True

        SandboxStatusRequest sandboxId -> do
            -- Get sandbox info from ID
            result <- try $ getSandboxInfo sandboxId

            case result of
                Left (e :: SomeException) ->
                    return $ SandboxErrorResponse $ "Failed to get sandbox status: " <> T.pack (show e)
                Right status ->
                    return $ SandboxStatusResponse status

        SandboxAbortRequest sandboxId -> do
            -- Get the sandbox directory from ID
            sandboxDir <- getSandboxDirFromId sandboxId

            -- Force cleanup of the sandbox
            result <- try $ do
                -- Unmount everything first
                liftIO $ unmountAllBindMounts sandboxDir

                -- Then remove the directory
                liftIO $ removePathForcibly sandboxDir `catch` \(_ :: SomeException) -> do
                    -- Try to fix permissions and retry
                    setRecursiveWritePermissions sandboxDir
                    removePathForcibly sandboxDir

            case result of
                Left (e :: SomeException) ->
                    return $ SandboxErrorResponse $ "Failed to abort sandbox: " <> T.pack (show e)
                Right _ ->
                    return $ SandboxReleasedResponse True

-- | Lookup sandbox directory from ID (daemon context)
getSandboxDirFromId :: Text -> TenM 'Build 'Daemon FilePath
getSandboxDirFromId _ = do
    -- This would normally do a lookup in a daemon-side mapping
    -- For now, just throw an error as placeholder
    throwError $ SandboxError "Sandbox ID lookup not implemented"

-- | Get sandbox status information
getSandboxInfo :: Text -> TenM 'Build 'Daemon SandboxStatus
getSandboxInfo _ = do
    -- This would normally get the status from a daemon-side mapping
    -- For now, just throw an error as placeholder
    throwError $ SandboxError "Sandbox status lookup not implemented"

-- | Set recursive write permissions to help with cleanup
setRecursiveWritePermissions :: FilePath -> IO ()
setRecursiveWritePermissions path = do
    isDir <- doesDirectoryExist path
    if isDir
        then do
            -- Make the directory writable
            perms <- getPermissions path
            setPermissions path (setOwnerWritable True perms)

            -- Process all entries
            entries <- listDirectory path
            forM_ entries $ \entry ->
                setRecursiveWritePermissions (path </> entry)
        else do
            -- Make the file writable
            perms <- getPermissions path
            setPermissions path (setOwnerWritable True perms)

-- | Drop all Linux capabilities
dropAllCapabilities :: IO ()
dropAllCapabilities = do
    -- This would use the Linux capabilities API
    -- For now, we assume it worked and don't actually call the system functions
    return ()

-- | Set resource limits for the unprivileged user
setResourceLimits :: IO ()
setResourceLimits = do
    -- Set core dump size to 0
    setRLimit rLIMIT_CORE 0 0

    -- Set file size limit (1GB)
    setRLimit rLIMIT_FSIZE (1024*1024*1024) (1024*1024*1024)

    -- Set reasonable number of open files
    setRLimit rLIMIT_NOFILE 1024 1024

    -- Set process limit
    setRLimit rLIMIT_NPROC 128 128

    -- Set memory limit (2GB)
    setRLimit rLIMIT_AS (2*1024*1024*1024) (2*1024*1024*1024)

    -- Set CPU time limit (1 hour)
    setRLimit rLIMIT_CPU (60*60) (60*60)

    -- Set stack size limit (8MB)
    setRLimit rLIMIT_STACK (8*1024*1024) (8*1024*1024)

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
                    return $ userID entry
        Right entry ->
            return $ userID entry

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
                            return $ groupID entry
                Right entry ->
                    return $ groupID entry
        Right entry ->
            return $ groupID entry

-- | Get the real group ID
getRealGroupID :: IO GroupID
getRealGroupID = do
    uid <- User.getRealUserID
    entry <- User.getUserEntryForID uid
    return $ User.userGroupID entry
