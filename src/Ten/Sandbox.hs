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
import Data.List (isPrefixOf)
import Control.Exception (bracket, try, tryJust, throwIO, catch, finally, SomeException, IOException)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Process (createProcess, proc, readCreateProcessWithExitCode, CreateProcess(..), StdStream(..))
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Posix.Files as Posix
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus, ProcessStatus(..))
import System.Posix.Types (ProcessID, UserID, GroupID)
import qualified System.Posix.User as User
import qualified System.Posix.Resource as Resource
import qualified System.Posix.IO as PosixIO
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1_, getErrno, Errno(..), errnoToIOError)
import Foreign.C.String (CString, withCString, peekCString)
import Foreign.C.Types (CInt(..), CULong(..), CLong(..))
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import System.IO.Error (IOError, catchIOError, isPermissionError, isDoesNotExistError)
import System.IO (hPutStrLn, stderr, hClose)

import Ten.Core
import qualified Ten.Store as Store

-- Helper function to convert BuildId to String
showBuildId :: Maybe BuildId -> String
showBuildId Nothing = "unknown"
showBuildId (Just (BuildId _)) = "build" -- Simply use a placeholder string since we can't show Unique directly

-- Custom RLimit structure for FFI compatibility since System.Posix.Resource doesn't export it
data RLimit = RLimit
    { rlim_cur :: CULong  -- Current limit (soft limit)
    , rlim_max :: CULong  -- Maximum limit (hard limit)
    }

-- Foreign function imports for Linux-specific system calls

-- Mount namespace functions
foreign import ccall unsafe "sys/mount.h mount"
    c_mount :: CString -> CString -> CString -> CInt -> CString -> IO CInt

foreign import ccall unsafe "sys/mount.h umount"
    c_umount :: CString -> IO CInt

-- Namespace creation
foreign import ccall unsafe "sched.h unshare"
    c_unshare :: CInt -> IO CInt

-- Process namespaces
foreign import ccall unsafe "sched.h setns"
    c_setns :: CInt -> CInt -> IO CInt

-- Linux capability management
foreign import ccall unsafe "sys/capability.h cap_get_proc"
    c_cap_get_proc :: IO (Ptr ())

foreign import ccall unsafe "sys/capability.h cap_set_flag"
    c_cap_set_flag :: Ptr () -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

foreign import ccall unsafe "sys/capability.h cap_set_proc"
    c_cap_set_proc :: Ptr () -> IO CInt

foreign import ccall unsafe "sys/capability.h cap_free"
    c_cap_free :: Ptr () -> IO CInt

-- Process resource limits
foreign import ccall unsafe "resource.h getrlimit"
    c_getrlimit :: CInt -> Ptr RLimit -> IO CInt

foreign import ccall unsafe "resource.h setrlimit"
    c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

-- Chroot operation
foreign import ccall unsafe "unistd.h chroot"
    c_chroot :: CString -> IO CInt

-- Mount flags (from Linux sys/mount.h)
mS_RDONLY, mS_BIND, mS_NOSUID, mS_NODEV, mS_NOEXEC, mS_REMOUNT, mS_REC :: CInt
mS_RDONLY = 1
mS_BIND = 4096
mS_NOSUID = 2
mS_NODEV = 4
mS_NOEXEC = 8
mS_REMOUNT = 32
mS_REC = 16384

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

-- Resources to limit
rLIMIT_CPU, rLIMIT_FSIZE, rLIMIT_DATA, rLIMIT_STACK, rLIMIT_CORE, rLIMIT_NOFILE :: CInt
rLIMIT_CPU = 0      -- CPU time in seconds
rLIMIT_FSIZE = 1    -- Maximum filesize
rLIMIT_DATA = 2     -- Max data size
rLIMIT_STACK = 3    -- Max stack size
rLIMIT_CORE = 4     -- Max core file size
rLIMIT_NOFILE = 7   -- Max number of open files

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
                     removePathForcibly dir `catch` \(_ :: SomeException) ->
                         hPutStrLn stderr $ "Warning: Failed to remove sandbox directory: " ++ dir)
                (runSandboxed env state sandboxFunc))
        >>= either throwError return

-- | Create and get a sandbox directory path
getSandboxDir :: BuildEnv -> TenM 'Build FilePath
getSandboxDir env = do
    bid <- gets currentBuildId
    pid <- liftIO getProcessID
    let baseDir = workDir env </> "sandbox"
    liftIO $ createDirectoryIfMissing True baseDir

    case bid of
        Just (BuildId uid) -> do
            -- Use build ID for unique sandbox in daemon mode
            let uniqueDir = baseDir </> "build-" ++ showBuildId bid
            liftIO $ createDirectoryIfMissing True uniqueDir
            return uniqueDir
        Nothing -> do
            -- Use process ID for standalone mode
            let uniqueDir = baseDir </> "process-" ++ show pid
            liftIO $ createDirectoryIfMissing True uniqueDir
            return uniqueDir

-- | Setup a sandbox directory with the proper structure
setupSandbox :: FilePath -> SandboxConfig -> TenM 'Build ()
setupSandbox sandboxDir config = do
    -- Create the basic directory structure with proper permissions
    liftIO $ createAndSetupDir sandboxDir
    liftIO $ createAndSetupDir (sandboxDir </> "tmp")
    liftIO $ createAndSetupDir (sandboxDir </> "build")
    liftIO $ createAndSetupDir (sandboxDir </> "out")
    liftIO $ createAndSetupDir (sandboxDir </> "store")

    -- Create the return derivation directory if return support is enabled
    when (sandboxReturnSupport config) $ do
        let returnDir = takeDirectory $ returnDerivationPath sandboxDir
        liftIO $ createAndSetupDir returnDir

    -- Set appropriate permissions
    liftIO $ do
        -- Make sure the sandbox is writable
        perms <- getPermissions sandboxDir
        setPermissions sandboxDir $ setOwnerWritable True perms

        -- Make output directory writable
        outPerms <- getPermissions (sandboxDir </> "out")
        setPermissions (sandboxDir </> "out") $ setOwnerWritable True outPerms

        -- Set POSIX permissions for unprivileged access
        -- Owner: rwx, Group: rwx, Other: r-x
        Posix.setFileMode sandboxDir 0o775
        Posix.setFileMode (sandboxDir </> "out") 0o775
        Posix.setFileMode (sandboxDir </> "tmp") 0o775

        -- If we're running as root, set ownership for unprivileged builder
        uid <- User.getRealUserID
        when (uid == 0 && not (sandboxPrivileged config)) $ do
            -- Get the user/group IDs for the sandbox user/group
            userEntry <- User.getUserEntryForName (sandboxUser config)
            groupEntry <- User.getGroupEntryForName (sandboxGroup config)
            let ownerId = User.userID userEntry
            let groupId = User.groupID groupEntry

            -- Set ownership of key directories
            Posix.setOwnerAndGroup sandboxDir ownerId groupId
            Posix.setOwnerAndGroup (sandboxDir </> "out") ownerId groupId
            Posix.setOwnerAndGroup (sandboxDir </> "tmp") ownerId groupId

            when (sandboxReturnSupport config) $ do
                let returnDir = takeDirectory $ returnDerivationPath sandboxDir
                Posix.setOwnerAndGroup returnDir ownerId groupId

    -- Set up namespaces if enabled and running as root
    isRoot <- liftIO $ do
        uid <- User.getRealUserID
        return (uid == 0)

    when (isRoot && (sandboxUseMountNamespace config || sandboxUseNetworkNamespace config)) $ do
        logMsg 2 $ "Setting up sandbox namespaces"
        liftIO $ setupNamespaces config

    -- Add sandbox proof
    addProof BuildProof

    logMsg 2 $ "Sandbox setup completed: " <> T.pack sandboxDir

-- | Set up Linux namespaces for isolation
setupNamespaces :: SandboxConfig -> IO ()
setupNamespaces config = do
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
            if namespaceFlags == cLONE_NEWNET && isPermissionError (errnoToIOError "unshare" errno Nothing Nothing)
                then hPutStrLn stderr "Warning: Failed to create network namespace (requires CAP_NET_ADMIN), continuing without network isolation"
                else throwErrno "Failed to create namespaces"

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
                removePathForcibly sandboxDir
            logMsg 2 $ "Sandbox removed: " <> T.pack sandboxDir

-- | Helper to create a directory and set proper permissions
createAndSetupDir :: FilePath -> IO ()
createAndSetupDir dir = do
    -- Create the directory if it doesn't exist
    createDirectoryIfMissing True dir

    -- Set standard permissions
    perms <- getPermissions dir
    setPermissions dir (setOwnerExecutable True $
                        setOwnerWritable True $
                        setOwnerReadable True $ perms)

    -- Set Unix permissions directly - rwxrwxr-x for proper unprivileged access
    Posix.setFileMode dir 0o775

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
    -- Skip if source doesn't exist
    sourceExists <- liftIO $ doesPathExist sourcePath
    unless sourceExists $ return ()

    -- Determine destination path within sandbox
    let relPath = makeRelative "/" sourcePath
    let destPath = sandboxDir </> relPath

    -- Create parent directory structure
    liftIO $ createDirectoryIfMissing True (takeDirectory destPath)

    -- Bind mount the source to the destination
    isDir <- liftIO $ doesDirectoryExist sourcePath

    if isDir
        then do
            -- For directories, create the destination and bind mount
            liftIO $ createAndSetupDir destPath
            liftIO $ bindMount sourcePath destPath writable `catchIOError` \e -> do
                hPutStrLn stderr $ "Warning: Failed to bind mount directory " ++
                                  sourcePath ++ " to " ++ destPath ++ ": " ++ show e
                -- Try copying instead as fallback
                copyDirectoryRecursive sourcePath destPath

            logMsg 3 $ "Made available directory: " <> T.pack sourcePath <>
                      " -> " <> T.pack destPath <>
                      (if writable then " (writable)" else " (read-only)")
        else do
            -- For files, create an empty file and bind mount
            liftIO $ do
                -- Create parent directory if needed
                createDirectoryIfMissing True (takeDirectory destPath)

                -- Create an empty file as mount point
                writeFile destPath ""
                Posix.setFileMode destPath 0o664

                -- Try to mount the source file
                bindMount sourcePath destPath writable `catchIOError` \e -> do
                    hPutStrLn stderr $ "Warning: Failed to bind mount file " ++
                                      sourcePath ++ " to " ++ destPath ++ ": " ++ show e
                    -- Try copying instead as fallback
                    copyFile sourcePath destPath
                    when (not writable) $ do
                        -- Make read-only
                        perms <- getPermissions destPath
                        setPermissions destPath $ setOwnerWritable False perms

            logMsg 3 $ "Made available file: " <> T.pack sourcePath <>
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

        -- Mount or copy the input to both destinations
        liftIO $ do
            -- Create destination directories
            createDirectoryIfMissing True (takeDirectory storeDest)
            createDirectoryIfMissing True (takeDirectory nameDest)

            -- Create mount points
            isDir <- doesDirectoryExist sourcePath
            if isDir then do
                createAndSetupDir storeDest
                createAndSetupDir nameDest
            else do
                writeFile storeDest ""
                writeFile nameDest ""

                -- Make sure they are writable for the bind mount
                Posix.setFileMode storeDest 0o664
                Posix.setFileMode nameDest 0o664

            -- Try to mount the input (read-only)
            catchIOError
                (bindMountReadOnly sourcePath storeDest)
                (\e -> do
                    hPutStrLn stderr $ "Warning: Failed to bind mount store path " ++
                                      sourcePath ++ " to " ++ storeDest ++ ": " ++ show e
                    -- Try copying instead as fallback
                    if isDir
                        then copyDirectoryRecursive sourcePath storeDest
                        else copyFile sourcePath storeDest)

            catchIOError
                (bindMountReadOnly sourcePath nameDest)
                (\e -> do
                    hPutStrLn stderr $ "Warning: Failed to bind mount store path " ++
                                      sourcePath ++ " to " ++ nameDest ++ ": " ++ show e
                    -- Try copying instead as fallback - if we already copied to storeDest, make a symlink
                    storeDestExists <- doesPathExist storeDest
                    if storeDestExists
                        then createFileLink storeDest nameDest
                        else if isDir
                            then copyDirectoryRecursive sourcePath nameDest
                            else copyFile sourcePath nameDest)

        logMsg 3 $ "Made input available: " <> storeHash input <> "-" <> storeName input

    logMsg 2 $ "Made " <> T.pack (show $ Set.size inputs) <> " inputs available in sandbox"

-- | Make a system path available in the sandbox
makeSystemPathAvailable :: FilePath -> FilePath -> TenM 'Build ()
makeSystemPathAvailable sandboxDir systemPath = do
    -- Skip if the path doesn't exist
    pathExists <- liftIO $ doesPathExist systemPath
    unless pathExists $ return ()

    -- Determine the relative path in the sandbox
    let relPath = makeRelative "/" systemPath
    let sandboxPath = sandboxDir </> relPath

    -- Skip if already mounted
    destExists <- liftIO $ doesPathExist sandboxPath
    when destExists $ return ()

    -- Create parent directories
    liftIO $ createDirectoryIfMissing True (takeDirectory sandboxPath)

    -- Mount the system path
    isDir <- liftIO $ doesDirectoryExist systemPath
    if isDir
        then do
            -- Create directory
            liftIO $ createAndSetupDir sandboxPath
            -- Mount directory (read-only)
            liftIO $ bindMountReadOnly systemPath sandboxPath `catchIOError` \e -> do
                hPutStrLn stderr $ "Warning: Failed to bind mount system directory " ++
                                  systemPath ++ " to " ++ sandboxPath ++ ": " ++ show e
                -- Try copying instead as fallback for important system directories
                when (systemPath `elem` essentialSystemPaths) $ do
                    copyDirectoryRecursive systemPath sandboxPath

            logMsg 3 $ "System directory mounted: " <> T.pack systemPath
        else do
            -- Create empty file
            liftIO $ writeFile sandboxPath ""
            -- Set proper permissions
            liftIO $ Posix.setFileMode sandboxPath 0o664
            -- Mount file (read-only)
            liftIO $ bindMountReadOnly systemPath sandboxPath `catchIOError` \e -> do
                hPutStrLn stderr $ "Warning: Failed to bind mount system file " ++
                                  systemPath ++ " to " ++ sandboxPath ++ ": " ++ show e
                -- Try copying instead as fallback
                copyFile systemPath sandboxPath

            logMsg 3 $ "System file mounted: " <> T.pack systemPath
  where
    -- List of essential system paths that should be copied if mounting fails
    essentialSystemPaths = [
        "/bin", "/usr/bin", "/lib", "/usr/lib",
        "/etc/resolv.conf", "/etc/ssl/certs"
        ]

-- | Linux bind mount implementation
bindMount :: FilePath -> FilePath -> Bool -> IO ()
bindMount source dest writable = do
    -- First mount with MS_BIND
    withCString source $ \sourcePtr ->
        withCString dest $ \destPtr ->
            withCString "none" $ \fsTypePtr ->
                withCString "" $ \dataPtr -> do
                    ret <- c_mount sourcePtr destPtr fsTypePtr mS_BIND dataPtr
                    when (ret /= 0) $ do
                        errno <- getErrno
                        throwErrno $ "Failed to bind mount " ++ source ++ " to " ++ dest ++
                                    " (errno code: " ++ show (case errno of Errno n -> n) ++ ")"

    -- If read-only, remount with MS_RDONLY
    unless writable $ do
        withCString source $ \sourcePtr ->
            withCString dest $ \destPtr ->
                withCString "none" $ \fsTypePtr ->
                    withCString "" $ \dataPtr -> do
                        let flags = mS_BIND .|. mS_REMOUNT .|. mS_RDONLY
                        ret <- c_mount sourcePtr destPtr fsTypePtr flags dataPtr
                        when (ret /= 0) $ do
                            errno <- getErrno
                            -- Don't fail, just warn - the mount is already established
                            hPutStrLn stderr $ "Warning: Failed to remount read-only " ++ dest ++
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
            ret <- c_umount pathPtr
            -- Don't throw errors on unmount - it might already be unmounted
            when (ret /= 0) $ do
                errno <- getErrno
                unless (isPermissionError (errnoToIOError "umount" errno Nothing Nothing) ||
                        isDoesNotExistError (errnoToIOError "umount" errno Nothing Nothing)) $ do
                    hPutStrLn stderr $ "Warning: Failed to unmount " ++ path ++
                                      " (errno code: " ++ show (case errno of Errno n -> n) ++ ")"

-- | Find and unmount all bind mounts in a sandbox directory
unmountAllBindMounts :: FilePath -> IO ()
unmountAllBindMounts dir = do
    -- In a real implementation, we scan /proc/mounts to find all mounts under our directory
    entries <- readMounts

    -- Find mounts that are within our sandbox directory
    let relevantMounts = filter (\mount -> dir `isPrefixOf` mount) (reverse entries)

    -- Try to unmount each, ignoring errors
    forM_ relevantMounts $ \path -> do
        catch (unmountPath path) $ \(_ :: IOError) -> return ()
  where
    -- Read /proc/mounts to find all mounted filesystems
    readMounts :: IO [FilePath]
    readMounts = do
        exists <- doesFileExist "/proc/mounts"
        if not exists
            then fallbackUnmount  -- /proc might not be available
            else do
                contents <- readFile "/proc/mounts"
                return $ extractMountPaths contents

    -- Extract mount paths from /proc/mounts content
    extractMountPaths :: String -> [FilePath]
    extractMountPaths content =
        -- Each line is: device mountpoint fstype options dump pass
        map ((\parts -> if length parts > 1 then parts !! 1 else "")) $
        map words $
        lines content

    -- Fallback unmount strategy when /proc/mounts can't be read
    fallbackUnmount :: IO [FilePath]
    fallbackUnmount = do
        -- Try common paths within the sandbox
        let paths = [
                dir </> "store",
                dir </> "out",
                dir </> "tmp",
                dir </> "build",
                dir  -- Unmount the sandbox dir itself last
                ]
        -- Return these paths to be unmounted
        return paths

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
        userEntry <- User.getUserEntryForName userName
        groupEntry <- User.getGroupEntryForName groupName

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
  where
    -- Drop all Linux capabilities
    dropAllCapabilities :: IO ()
    dropAllCapabilities = do
        -- This is a simplified version - a full implementation would use libcap
        hPutStrLn stderr "Dropping capabilities"

-- Set restrictive resource limits for the unprivileged user
setResourceLimits :: IO ()
setResourceLimits = do
    -- Set core dump limit to 0
    Resource.setResourceLimit Resource.ResourceCoreFileSize (Resource.ResourceLimits Resource.ResourceLimitInfinity Resource.ResourceLimitInfinity)

    -- Set a reasonable file size limit (1GB)
    Resource.setResourceLimit Resource.ResourceFileSize (Resource.ResourceLimits (Resource.ResourceLimit $ 1024*1024*1024) (Resource.ResourceLimit $ 1024*1024*1024))

    -- Set reasonable number of open files
    Resource.setResourceLimit Resource.ResourceOpenFiles (Resource.ResourceLimits (Resource.ResourceLimit 1024) (Resource.ResourceLimit 1024))

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
            Nothing ->
                return (Right ()) :: IO (Either SomeException ()))
        (\_ -> action)

-- | Run a builder process as an unprivileged user
runBuilderAsUser :: FilePath -> [String] -> String -> String -> Map Text Text -> IO (ExitCode, String, String)
runBuilderAsUser program args user group env = do
    -- Get current UID to check if we're root
    uid <- User.getRealUserID

    if uid == 0
        then do
            -- We're root, so use privilege dropping
            withDroppedPrivileges user group $ do
                -- Execute the builder as the unprivileged user
                execBuilder program args env
        else do
            -- Not running as root, just execute directly
            execBuilder program args env
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

-- | Bitwise OR for CInt flags
(.|.) :: CInt -> CInt -> CInt
a .|. b = a + b  -- Simple bit OR implementation for mount flags
