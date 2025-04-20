{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ForeignFunctionInterface #-}

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
    dropSandboxPrivileges,
    regainPrivileges,
    withDroppedPrivileges,
    runBuilderAsUser
) where

import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (get, gets)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf, isInfixOf)
import Control.Exception (bracket, try, tryJust, throwIO, catch, finally, handle, SomeException, IOException)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Bits ((.|.), (.&.))
import Data.Maybe (isJust, fromMaybe)
import System.Directory
import System.FilePath
import System.Process (createProcess, proc, readCreateProcessWithExitCode, CreateProcess(..), StdStream(..))
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Files as Posix
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import System.Posix.Types (ProcessID, UserID, GroupID, CPid(..), Fd(..), FileMode)
import qualified System.Posix.User as User
import qualified System.Posix.Resource as Resource
import qualified System.Posix.IO as PosixIO
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1_, throwErrnoIfMinus1, getErrno, Errno(..), errnoToIOError)
import Foreign.C.String (CString, withCString, peekCString, newCString)
import Foreign.C.Types (CInt(..), CULong(..), CLong(..), CSize(..))
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, malloc, free)
import Foreign.Marshal.Array (allocaArray, peekArray, pokeArray)
import Foreign.Storable (peek, poke)
import System.IO.Error (IOError, catchIOError, isPermissionError, isDoesNotExistError)
import System.IO (hPutStrLn, stderr, hClose)

import Ten.Core
import qualified Ten.Store as Store

-- Helper function to convert BuildId to String
showBuildId :: Maybe BuildId -> String
showBuildId Nothing = "unknown"
showBuildId (Just (BuildId _)) = "build"
showBuildId (Just (BuildIdFromInt n)) = "build-" ++ show n

-- RLimit structure for resource limits
data RLimit = RLimit {
    rlim_cur :: CULong,  -- Current limit (soft limit)
    rlim_max :: CULong   -- Maximum limit (hard limit)
}

-- Linux capability set structure
newtype CapSet = CapSet (Ptr ())

-- Foreign function imports for Linux-specific system calls

-- Mount namespace functions
foreign import ccall unsafe "sys/mount.h mount"
    c_mount :: CString -> CString -> CString -> CInt -> Ptr () -> IO CInt

foreign import ccall unsafe "sys/mount.h umount2"
    c_umount2 :: CString -> CInt -> IO CInt

-- Namespace creation
foreign import ccall unsafe "sched.h unshare"
    c_unshare :: CInt -> IO CInt

-- Process namespaces
foreign import ccall unsafe "sched.h setns"
    c_setns :: CInt -> CInt -> IO CInt

-- Linux capability management
foreign import ccall unsafe "sys/capability.h cap_get_proc"
    c_cap_get_proc :: IO (Ptr ())

foreign import ccall unsafe "sys/capability.h cap_clear"
    c_cap_clear :: Ptr () -> IO CInt

foreign import ccall unsafe "sys/capability.h cap_set_flag"
    c_cap_set_flag :: Ptr () -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

foreign import ccall unsafe "sys/capability.h cap_set_proc"
    c_cap_set_proc :: Ptr () -> IO CInt

foreign import ccall unsafe "sys/capability.h cap_free"
    c_cap_free :: Ptr () -> IO CInt

-- Process resource limits
foreign import ccall unsafe "sys/resource.h getrlimit"
    c_getrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import ccall unsafe "sys/resource.h setrlimit"
    c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

-- Chroot operation
foreign import ccall unsafe "unistd.h chroot"
    c_chroot :: CString -> IO CInt

-- Mount flags (from Linux sys/mount.h)
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

-- Namespace flags (from Linux sched.h)
cLONE_NEWNS, cLONE_NEWUTS, cLONE_NEWIPC, cLONE_NEWUSER, cLONE_NEWPID, cLONE_NEWNET :: CInt
cLONE_NEWNS = 0x00020000    -- Mount namespace
cLONE_NEWUTS = 0x04000000   -- UTS namespace (hostname)
cLONE_NEWIPC = 0x08000000   -- IPC namespace
cLONE_NEWUSER = 0x10000000  -- User namespace
cLONE_NEWPID = 0x20000000   -- PID namespace
cLONE_NEWNET = 0x40000000   -- Network namespace

-- Linux capability flags
cAP_EFFECTIVE, cAP_PERMITTED, cAP_INHERITABLE :: CInt
cAP_EFFECTIVE = 0
cAP_PERMITTED = 1
cAP_INHERITABLE = 2

-- Linux capability values (from sys/capability.h)
cAP_CHOWN, cAP_DAC_OVERRIDE, cAP_DAC_READ_SEARCH, cAP_FOWNER, cAP_FSETID,
    cAP_KILL, cAP_SETGID, cAP_SETUID, cAP_SETPCAP, cAP_NET_BIND_SERVICE,
    cAP_NET_BROADCAST, cAP_NET_ADMIN, cAP_NET_RAW, cAP_SYS_MODULE, cAP_SYS_CHROOT,
    cAP_SYS_PTRACE, cAP_SYS_ADMIN, cAP_SYS_BOOT, cAP_SYS_NICE, cAP_SYS_RESOURCE,
    cAP_SYS_TIME, cAP_MKNOD, cAP_AUDIT_WRITE, cAP_SETFCAP :: CInt
cAP_CHOWN = 0
cAP_DAC_OVERRIDE = 1
cAP_DAC_READ_SEARCH = 2
cAP_FOWNER = 3
cAP_FSETID = 4
cAP_KILL = 5
cAP_SETGID = 6
cAP_SETUID = 7
cAP_SETPCAP = 8
cAP_NET_BIND_SERVICE = 10
cAP_NET_BROADCAST = 11
cAP_NET_ADMIN = 12
cAP_NET_RAW = 13
cAP_SYS_MODULE = 16
cAP_SYS_CHROOT = 18
cAP_SYS_PTRACE = 19
cAP_SYS_ADMIN = 21
cAP_SYS_BOOT = 22
cAP_SYS_NICE = 23
cAP_SYS_RESOURCE = 24
cAP_SYS_TIME = 25
cAP_MKNOD = 27
cAP_AUDIT_WRITE = 29
cAP_SETFCAP = 30

-- Resources to limit
rLIMIT_CPU, rLIMIT_FSIZE, rLIMIT_DATA, rLIMIT_STACK, rLIMIT_CORE,
    rLIMIT_RSS, rLIMIT_NOFILE, rLIMIT_AS, rLIMIT_NPROC, rLIMIT_MEMLOCK,
    rLIMIT_LOCKS, rLIMIT_SIGPENDING, rLIMIT_MSGQUEUE, rLIMIT_NICE,
    rLIMIT_RTPRIO, rLIMIT_RTTIME :: CInt
rLIMIT_CPU = 0      -- CPU time in seconds
rLIMIT_FSIZE = 1    -- Maximum filesize
rLIMIT_DATA = 2     -- Max data size
rLIMIT_STACK = 3    -- Max stack size
rLIMIT_CORE = 4     -- Max core file size
rLIMIT_RSS = 5      -- Max resident set size
rLIMIT_NOFILE = 7   -- Max number of open files
rLIMIT_AS = 9       -- Address space limit
rLIMIT_NPROC = 6    -- Max number of processes
rLIMIT_MEMLOCK = 8  -- Max locked-in-memory address space
rLIMIT_LOCKS = 10   -- Maximum file locks
rLIMIT_SIGPENDING = 11 -- Max number of pending signals
rLIMIT_MSGQUEUE = 12   -- Max bytes in POSIX message queues
rLIMIT_NICE = 13       -- Max nice priority allowed
rLIMIT_RTPRIO = 14     -- Max realtime priority
rLIMIT_RTTIME = 15     -- Max realtime timeout

-- | Configuration for a build sandbox
data SandboxConfig = SandboxConfig
    { sandboxAllowNetwork :: Bool           -- Allow network access
    , sandboxExtraPaths :: Set FilePath     -- Additional paths to make available
    , sandboxEnv :: Map Text Text           -- Environment variables
    , sandboxReadOnlyPaths :: Set FilePath  -- Paths to mount read-only
    , sandboxWritablePaths :: Set FilePath  -- Paths to mount writable
    , sandboxPrivileged :: Bool             -- Run as privileged user
    , sandboxReturnSupport :: Bool          -- Allow return-continuation support
    , sandboxUseMountNamespace :: Bool      -- Use Linux mount namespace isolation
    , sandboxUseNetworkNamespace :: Bool    -- Use Linux network namespace isolation
    , sandboxUseUserNamespace :: Bool       -- Use Linux user namespace isolation
    , sandboxUser :: String                 -- User to run build as (for unprivileged)
    , sandboxGroup :: String                -- Group to run build as (for unprivileged)
    , sandboxCPULimit :: Int                -- CPU time limit in seconds (0 = no limit)
    , sandboxMemoryLimit :: Int             -- Memory limit in MB (0 = no limit)
    , sandboxDiskLimit :: Int               -- Disk space limit in MB (0 = no limit)
    , sandboxMaxProcesses :: Int            -- Maximum number of processes (0 = no limit)
    , sandboxTimeoutSecs :: Int             -- Timeout in seconds (0 = no timeout)
    } deriving (Show, Eq)

-- | Default sandbox configuration (restrictive)
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig
    { sandboxAllowNetwork = False
    , sandboxExtraPaths = Set.fromList ["/usr/bin", "/bin", "/lib", "/usr/lib"]
    , sandboxEnv = Map.empty
    , sandboxReadOnlyPaths = Set.empty
    , sandboxWritablePaths = Set.empty
    , sandboxPrivileged = False
    , sandboxReturnSupport = True
    , sandboxUseMountNamespace = True
    , sandboxUseNetworkNamespace = True     -- Isolate network by default
    , sandboxUseUserNamespace = False       -- Don't use user namespace by default (needs root)
    , sandboxUser = "nobody"                -- Default unprivileged user
    , sandboxGroup = "nogroup"              -- Default unprivileged group
    , sandboxCPULimit = 3600                -- 1 hour CPU time by default
    , sandboxMemoryLimit = 2048             -- 2GB memory by default
    , sandboxDiskLimit = 5120               -- 5GB disk space by default
    , sandboxMaxProcesses = 32              -- Maximum 32 processes by default
    , sandboxTimeoutSecs = 0                -- No timeout by default
    }

-- | Standard path for returning a derivation from a builder
returnDerivationPath :: FilePath -> FilePath
returnDerivationPath buildDir = buildDir </> "return.drv"

-- | Standard path for inputs in sandbox
sandboxInputPath :: FilePath -> FilePath -> FilePath
sandboxInputPath sandboxDir inputName = sandboxDir </> inputName

-- | Standard path for outputs in sandbox
sandboxOutputPath :: FilePath -> FilePath -> FilePath
sandboxOutputPath sandboxDir outputName = sandboxDir </> "out" </> outputName

-- | Run an action within a build sandbox
withSandbox :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build a) -> TenM 'Build a
withSandbox inputs config action = do
    env <- ask
    state <- get

    -- Only buildable in the Build phase
    when (currentPhase state /= Build) $
        throwError $ BuildFailed "Sandbox can only be created in Build phase"

    -- Check if running as root (daemon mode)
    isRoot <- liftIO $ do
        uid <- User.getRealUserID
        return (uid == 0)

    -- Need root for proper sandbox isolation with most namespaces
    unless isRoot $
        if sandboxUseMountNamespace config || sandboxUseUserNamespace config
            then logMsg 2 $ "Warning: Not running as root, some sandbox isolation features may be limited"
            else return ()

    -- Create a sandbox function
    let sandboxFunc sandboxDir = do
            -- Setup the sandbox
            setupSandbox sandboxDir config

            -- Make inputs available
            makeInputsAvailable sandboxDir inputs

            -- Add read-only and writable paths
            mapM_ (makePathAvailable sandboxDir False) (Set.toList $ sandboxReadOnlyPaths config)
            mapM_ (makePathAvailable sandboxDir True) (Set.toList $ sandboxWritablePaths config)

            -- Add system paths
            mapM_ (makeSystemPathAvailable sandboxDir) (Set.toList $ sandboxExtraPaths config)

            -- Log sandbox creation
            logMsg 1 $ "Created sandbox at: " <> T.pack sandboxDir

            -- Run the action inside the sandbox
            result <- action sandboxDir `catchError` \e -> do
                -- Log error before cleanup
                logMsg 1 $ "Error in sandbox: " <> T.pack (show e)
                -- Re-throw the error
                throwError e

            -- Cleanup sandbox if we're in daemon mode
            isDaemonMode >>= \daemon -> when daemon $ do
                logMsg 2 $ "Cleaning up daemon sandbox: " <> T.pack sandboxDir
                teardownSandbox sandboxDir

            return result

    -- Get sandbox directory and ensure cleanup
    sandboxDir <- getSandboxDir env
    liftIO (bracket
                (return sandboxDir)
                (\dir -> unless (runMode env == DaemonMode) $ do
                     -- Make sure we cleanup any mounts even if removal fails
                     unmountAllBindMounts dir `catch` \(_ :: SomeException) -> return ()

                     -- Use force to remove the directory tree
                     let retryRemove = do
                           result <- try $ removePathForcibly dir
                           case result of
                              Left (e :: SomeException) -> do
                                  hPutStrLn stderr $ "Warning: Failed to remove sandbox directory: " ++ dir ++ " - " ++ show e
                                  -- Try to fix permissions and retry
                                  setRecursiveWritePermissions dir
                                  removePathForcibly dir `catch` \(e2 :: SomeException) ->
                                      hPutStrLn stderr $ "Warning: Failed to remove sandbox directory after retry: " ++ dir ++ " - " ++ show e2
                              Right _ -> return ()

                     -- Try to remove with retries
                     retryRemove)
                (runSandboxed env state sandboxFunc))
        >>= either throwError return

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

-- | Create and get a sandbox directory path
getSandboxDir :: BuildEnv -> TenM 'Build FilePath
getSandboxDir env = do
    bid <- gets currentBuildId
    pid <- liftIO getProcessID
    let baseDir = workDir env </> "sandbox"
    liftIO $ createDirectoryIfMissing True baseDir

    -- Generate a unique directory name
    uniqueName <- case bid of
        Just buildId -> return $ "build-" ++ showBuildId bid
        Nothing -> do
            -- Use process ID for standalone mode with timestamp for uniqueness
            timestamp <- liftIO $ getPOSIXTime
            return $ "process-" ++ show pid ++ "-" ++ show (round timestamp :: Integer)

    let uniqueDir = baseDir </> uniqueName
    liftIO $ createDirectoryIfMissing True uniqueDir

    -- Set proper permissions immediately
    liftIO $ do
        Posix.setFileMode uniqueDir 0o755
        perms <- getPermissions uniqueDir
        setPermissions uniqueDir $ setOwnerWritable True perms

    return uniqueDir

-- | Get POSIX timestamp
getPOSIXTime :: IO Double
getPOSIXTime = do
    current <- getCurrentTime
    return $ utcTimeToPOSIXSeconds current
  where
    -- Placeholder implementation - in a real program you'd use Data.Time.Clock.POSIX
    getCurrentTime = return undefined
    utcTimeToPOSIXSeconds = const 1234567890.0

-- | Setup a sandbox directory with the proper structure
setupSandbox :: FilePath -> SandboxConfig -> TenM 'Build ()
setupSandbox sandboxDir config = do
    -- Create the basic directory structure with proper permissions
    liftIO $ createAndSetupDir sandboxDir 0o755
    liftIO $ createAndSetupDir (sandboxDir </> "tmp") 0o1777  -- Set tmp directory with sticky bit
    liftIO $ createAndSetupDir (sandboxDir </> "build") 0o755
    liftIO $ createAndSetupDir (sandboxDir </> "out") 0o755
    liftIO $ createAndSetupDir (sandboxDir </> "store") 0o755

    -- Create the return derivation directory if return support is enabled
    when (sandboxReturnSupport config) $ do
        let returnDir = takeDirectory $ returnDerivationPath sandboxDir
        liftIO $ createAndSetupDir returnDir 0o755

    -- Set appropriate permissions
    liftIO $ do
        -- Make sure key directories are writable for the sandbox user
        uid <- User.getRealUserID
        when (uid == 0 && not (sandboxPrivileged config)) $ do
            -- Get the user/group IDs for the sandbox user/group
            userEntry <- safeGetUserEntry (sandboxUser config)
            groupEntry <- safeGetGroupEntry (sandboxGroup config)

            let ownerId = User.userID userEntry
            let groupId = User.groupID groupEntry

            -- Set ownership of key directories
            setOwnerAndGroup sandboxDir ownerId groupId
            setOwnerAndGroup (sandboxDir </> "out") ownerId groupId
            setOwnerAndGroup (sandboxDir </> "tmp") ownerId groupId
            setOwnerAndGroup (sandboxDir </> "build") ownerId groupId

            -- Set ownership for return path if enabled
            when (sandboxReturnSupport config) $ do
                let returnDir = takeDirectory $ returnDerivationPath sandboxDir
                setOwnerAndGroup returnDir ownerId groupId

    -- Set up namespaces if enabled and running as root
    isRoot <- liftIO $ do
        uid <- User.getRealUserID
        return (uid == 0)

    when (isRoot && (sandboxUseMountNamespace config || sandboxUseNetworkNamespace config)) $ do
        logMsg 2 $ "Setting up sandbox namespaces"
        liftIO $ setupNamespaces config sandboxDir

    -- Make sure /proc is available in the sandbox if needed
    when (isRoot && sandboxUseMountNamespace config) $ do
        logMsg 3 $ "Setting up /proc in sandbox"
        liftIO $ do
            let procDir = sandboxDir </> "proc"
            createAndSetupDir procDir 0o555
            mountProc procDir `catch` \(_ :: SomeException) ->
                hPutStrLn stderr $ "Warning: Failed to mount proc in sandbox: " ++ procDir

    -- Set up dev pseudo-filesystem if needed
    when (isRoot && sandboxUseMountNamespace config) $ do
        logMsg 3 $ "Setting up minimal /dev in sandbox"
        liftIO $ setupMinimalDev sandboxDir

    -- Add sandbox proof
    addProof BuildProof

    logMsg 2 $ "Sandbox setup completed: " <> T.pack sandboxDir

-- | Safe wrapper for getting user entry with fallback
safeGetUserEntry :: String -> IO User.UserEntry
safeGetUserEntry username = do
    result <- try $ User.getUserEntryForName username
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nobody
            hPutStrLn stderr $ "Warning: User " ++ username ++ " not found, falling back to nobody"
            User.getUserEntryForName "nobody" `catch` \(_ :: SomeException) ->
                -- Ultimate fallback to current user if even nobody doesn't exist
                User.getUserEntryForID =<< User.getRealUserID
        Right entry -> return entry

-- | Safe wrapper for getting group entry with fallback
safeGetGroupEntry :: String -> IO User.GroupEntry
safeGetGroupEntry groupname = do
    result <- try $ User.getGroupEntryForName groupname
    case result of
        Left (_ :: SomeException) -> do
            -- Fallback to nogroup
            hPutStrLn stderr $ "Warning: Group " ++ groupname ++ " not found, falling back to nogroup"
            User.getGroupEntryForName "nogroup" `catch` \(_ :: SomeException) ->
                User.getGroupEntryForName "nobody" `catch` \(_ :: SomeException) ->
                    -- Ultimate fallback to current group if even nogroup/nobody doesn't exist
                    User.getGroupEntryForID =<< User.getRealGroupID
        Right entry -> return entry

-- | Mount /proc in the sandbox
mountProc :: FilePath -> IO ()
mountProc procDir = do
    -- Mount proc filesystem
    withCString procDir $ \destPtr ->
        withCString "proc" $ \fsTypePtr ->
            withCString "none" $ \sourcePtr ->
                withCString "" $ \dataPtr -> do
                    ret <- c_mount sourcePtr destPtr fsTypePtr 0 nullPtr
                    when (ret /= 0) $ do
                        errno <- getErrno
                        throwErrno $ "Failed to mount proc in " ++ procDir

-- | Setup a minimal /dev in the sandbox
setupMinimalDev :: FilePath -> IO ()
setupMinimalDev sandboxDir = do
    let devDir = sandboxDir </> "dev"

    -- Create the dev directory
    createAndSetupDir devDir 0o755

    -- Create essential device nodes (minimal set)
    let essentialDevices = [
            ("null", 1, 3),    -- /dev/null
            ("zero", 1, 5),    -- /dev/zero
            ("full", 1, 7),    -- /dev/full
            ("random", 1, 8),  -- /dev/random
            ("urandom", 1, 9), -- /dev/urandom
            ("tty", 5, 0)      -- /dev/tty
            ]

    -- Try to bind mount each device from the host
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
                else if sandboxUseUserNamespace config && (isPermissionError errnoType || errno == eOPNOTSUPP)
                    then hPutStrLn stderr "Warning: User namespace not supported or permission denied, continuing without user namespace"
                    else throwErrno "Failed to create namespaces"

    -- Make mount propagation private if using mount namespace
    -- This prevents mounts from leaking outside the sandbox
    when (sandboxUseMountNamespace config) $ do
        withCString "/" $ \rootPtr ->
            withCString "none" $ \fsTypePtr ->
                withCString "none" $ \sourcePtr ->
                    withCString "" $ \dataPtr -> do
                        ret <- c_mount sourcePtr rootPtr fsTypePtr (mS_REC .|. mS_PRIVATE) nullPtr
                        when (ret /= 0) $ do
                            errno <- getErrno
                            hPutStrLn stderr $ "Warning: Failed to make mounts private: " ++ show errno

-- POSIX constants
eOPNOTSUPP :: Errno
eOPNOTSUPP = Errno 95  -- Operation not supported

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
runSandboxed :: BuildEnv -> BuildState -> (FilePath -> TenM 'Build a) -> FilePath -> IO (Either BuildError a)
runSandboxed env state action sandboxDir = do
    result <- runTen (action sandboxDir) env state
    return $ case result of
        Left err -> Left err
        Right (val, _) -> Right val

-- | Make a path available in the sandbox (read-only or writable)
makePathAvailable :: FilePath -> Bool -> FilePath -> TenM 'Build ()
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
makeInputsAvailable :: FilePath -> Set StorePath -> TenM 'Build ()
makeInputsAvailable sandboxDir inputs = do
    env <- ask

    -- Process each input
    forM_ (Set.toList inputs) $ \input -> do
        -- Get source path
        let sourcePath = Store.storePathToFilePath input env

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
makeSystemPathAvailable :: FilePath -> FilePath -> TenM 'Build ()
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
                    ret <- c_mount sourcePtr destPtr fsTypePtr mS_BIND dataPtr
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
                        ret <- c_mount sourcePtr destPtr fsTypePtr flags dataPtr
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
teardownSandbox :: FilePath -> TenM 'Build ()
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
prepareSandboxEnvironment :: BuildEnv -> FilePath -> Map Text Text -> Map Text Text
prepareSandboxEnvironment env sandboxDir extraEnv =
    Map.unions
        [ baseEnv
        , extraEnv
        ]
  where
    baseEnv = Map.fromList
        [ ("TEN_STORE", T.pack $ storePath env)
        , ("TEN_BUILD_DIR", T.pack sandboxDir)
        , ("TEN_OUT", T.pack $ sandboxDir </> "out")
        , ("TEN_RETURN_PATH", T.pack $ returnDerivationPath sandboxDir)
        , ("TEN_TMP", T.pack $ sandboxDir </> "tmp")
        , ("PATH", "/bin:/usr/bin:/usr/local/bin") -- Explicit PATH
        , ("HOME", T.pack sandboxDir)              -- Set HOME to sandbox
        , ("TMPDIR", T.pack $ sandboxDir </> "tmp") -- Set TMPDIR
        , ("TMP", T.pack $ sandboxDir </> "tmp")   -- Alternative tmp env var
        , ("TEMP", T.pack $ sandboxDir </> "tmp")  -- Another alternative
        ]

-- | Drop privileges to run as unprivileged user
dropSandboxPrivileges :: String -> String -> IO ()
dropSandboxPrivileges userName groupName = do
    -- Get the current (effective) user ID
    euid <- User.getEffectiveUserID

    -- Only proceed if we're running as root
    when (euid == 0) $ do
        -- Get user and group info
        userEntry <- safeGetUserEntry userName
        groupEntry <- safeGetGroupEntry groupName

        -- Drop all capabilities first
        dropAllCapabilities

        -- Set resource limits for unprivileged user
        setResourceLimits

        -- Set the group ID first (must be done before dropping user privileges)
        User.setGroupID (User.groupID groupEntry)

        -- Then set the user ID
        User.setUserID (User.userID userEntry)

        -- Verify the change
        newEuid <- User.getEffectiveUserID
        when (newEuid == 0) $
            error "Failed to drop privileges - still running as root"

-- | Drop all Linux capabilities
dropAllCapabilities :: IO ()
dropAllCapabilities = do
    -- Get the current capability set
    capPtr <- c_cap_get_proc
    when (capPtr == nullPtr) $
        throwErrno "Failed to get capabilities"

    -- Clear all capabilities
    ret <- c_cap_clear capPtr
    when (ret /= 0) $ do
        c_cap_free capPtr
        throwErrno "Failed to clear capabilities"

    -- Apply the changes
    ret2 <- c_cap_set_proc capPtr
    when (ret2 /= 0) $ do
        c_cap_free capPtr
        throwErrno "Failed to set capabilities"

    -- Free the capability set
    _ <- c_cap_free capPtr
    return ()

-- | Set restrictive resource limits for the unprivileged user
setResourceLimits :: IO ()
setResourceLimits = do
    -- Set reasonable resource limits

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

-- | Regain privileges (back to root)
regainPrivileges :: IO ()
regainPrivileges = do
    -- Set effective user ID back to 0 (root)
    -- This only works if the real UID is root
    ruid <- User.getRealUserID

    if ruid == 0
        then do
            User.setEffectiveUserID 0

            -- Verify
            euid <- User.getEffectiveUserID
            when (euid /= 0) $
                error "Failed to regain privileges - not running as root"
        else
            error "Cannot regain privileges - real user is not root"

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
                    dropSandboxPrivileges user group

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
                    User.setEffectiveGroupID egid
                    User.setEffectiveUserID euid) :: IO (Either SomeException ())
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

-- | Helper to set ownership of a file or directory
setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup path uid gid = do
    -- Check if path exists
    exists <- doesPathExist path
    when exists $ do
        -- Set ownership
        catch (Posix.setOwnerAndGroup path uid gid) $ \e ->
            hPutStrLn stderr $ "Warning: Failed to set ownership on " ++ path ++ ": " ++ show (e :: IOException)
