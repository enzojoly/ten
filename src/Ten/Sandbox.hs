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
    dropPrivileges,
    regainPrivileges,
    withDroppedPrivileges,
    runBuilderAsUser
) where

import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket, try, SomeException, catch, finally)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files
import System.Posix.Process (getProcessID, forkProcess, executeFile, getProcessStatus)
import System.Posix.User
import Foreign.C.Error (throwErrno, throwErrnoIfMinus1_, getErrno)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt(..))
import System.IO.Error (IOError)
import System.IO (hPutStrLn, stderr)

import Ten.Core
import Ten.Store

-- Foreign function imports for Linux-specific calls
foreign import ccall unsafe "sys/mount.h mount"
    c_mount :: CString -> CString -> CString -> CInt -> CString -> IO CInt

foreign import ccall unsafe "sys/mount.h umount"
    c_umount :: CString -> IO CInt

-- Mount flags (from Linux sys/mount.h)
mS_RDONLY, mS_BIND, mS_NOSUID, mS_NODEV, mS_NOEXEC, mS_REMOUNT :: CInt
mS_RDONLY = 1
mS_BIND = 4096
mS_NOSUID = 2
mS_NODEV = 4
mS_NOEXEC = 8
mS_REMOUNT = 32

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
    , sandboxUser :: String                 -- User to run build as (for unprivileged)
    , sandboxGroup :: String                -- Group to run build as (for unprivileged)
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
    , sandboxUser = "nobody"                -- Default unprivileged user
    , sandboxGroup = "nogroup"              -- Default unprivileged group
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
        uid <- getRealUserID
        return (uid == 0)

    -- Need root for proper sandbox isolation
    unless (isRoot || not (sandboxUseMountNamespace config)) $
        logMsg 2 $ "Warning: Not running as root, sandbox isolation may be limited"

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
            result <- action sandboxDir

            -- Clean up sandbox if needed
            isDaemonMode >>= \daemon -> when daemon $ do
                logMsg 2 $ "Cleaning up daemon sandbox: " <> T.pack sandboxDir
                teardownSandbox sandboxDir

            return result

    -- Get sandbox directory and run with cleanup
    sandboxDir <- getSandboxDir env
    liftIO (bracket
               (return sandboxDir)
               (\dir -> unless (runMode env == DaemonMode) $ removePathForcibly dir)
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
            let uniqueDir = baseDir </> "build-" ++ show uid
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
        setFileMode sandboxDir 0o775
        setFileMode (sandboxDir </> "out") 0o775

        -- If we're running as root, set ownership for unprivileged builder
        uid <- getRealUserID
        when (uid == 0) $ do
            -- Get the user/group IDs for the sandbox user/group
            userEntry <- getUserEntryForName (sandboxUser config)
            groupEntry <- getGroupEntryForName (sandboxGroup config)
            let ownerId = userID userEntry
            let groupId = groupID groupEntry

            -- Set ownership of key directories
            setOwnerAndGroup sandboxDir ownerId groupId
            setOwnerAndGroup (sandboxDir </> "out") ownerId groupId
            setOwnerAndGroup (sandboxDir </> "tmp") ownerId groupId

            when (sandboxReturnSupport config) $ do
                let returnDir = takeDirectory $ returnDerivationPath sandboxDir
                setOwnerAndGroup returnDir ownerId groupId

    -- Add sandbox proof
    addProof BuildProof

    logMsg 2 $ "Sandbox setup completed: " <> T.pack sandboxDir

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
            liftIO $ do
                unmountAllBindMounts sandboxDir
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
    createDirectoryIfMissing True dir
    perms <- getPermissions dir
    setPermissions dir (setOwnerExecutable True $
                        setOwnerWritable True $
                        setOwnerReadable True $ perms)

    -- Set Unix permissions directly - rwxrwxr-x for proper unprivileged access
    setFileMode dir 0o775

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
            liftIO $ bindMount sourcePath destPath writable
            logMsg 3 $ "Bind mounted directory: " <> T.pack sourcePath <>
                       " -> " <> T.pack destPath <>
                       (if writable then " (writable)" else " (read-only)")
        else do
            -- For files, create an empty file and bind mount
            liftIO $ do
                -- Create an empty file as mount point
                writeFile destPath ""
                -- Mount the source file
                bindMount sourcePath destPath writable

            logMsg 3 $ "Bind mounted file: " <> T.pack sourcePath <>
                       " -> " <> T.pack destPath <>
                       (if writable then " (writable)" else " (read-only)")

-- | Make inputs available in the sandbox
makeInputsAvailable :: FilePath -> Set StorePath -> TenM 'Build ()
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
                setFileMode storeDest 0o664
                setFileMode nameDest 0o664

            -- Mount the input (read-only)
            bindMountReadOnly sourcePath storeDest
            bindMountReadOnly sourcePath nameDest

        logMsg 3 $ "Made input available: " <> T.pack (storeHash input) <> "-" <> T.pack (storeName input)

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
            liftIO $ bindMountReadOnly systemPath sandboxPath
            logMsg 3 $ "System directory mounted: " <> T.pack systemPath
        else do
            -- Create empty file
            liftIO $ writeFile sandboxPath ""
            -- Set proper permissions
            liftIO $ setFileMode sandboxPath 0o664
            -- Mount file (read-only)
            liftIO $ bindMountReadOnly systemPath sandboxPath
            logMsg 3 $ "System file mounted: " <> T.pack systemPath

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
                        let errMsg = "Failed to bind mount " ++ source ++ " to " ++ dest ++
                                     " (errno: " ++ show errno ++ ")"
                        hPutStrLn stderr errMsg
                        throwErrno errMsg

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
                            let errMsg = "Failed to remount read-only " ++ dest ++
                                         " (errno: " ++ show errno ++ ")"
                            hPutStrLn stderr errMsg
                            -- Don't fail, just warn - the mount is already established
                            -- and read-only may not be critical

-- | Shorthand for read-only bind mounts
bindMountReadOnly :: FilePath -> FilePath -> IO ()
bindMountReadOnly source dest = bindMount source dest False

-- | Unmount a path
unmountPath :: FilePath -> IO ()
unmountPath path =
    withCString path $ \pathPtr -> do
        ret <- c_umount pathPtr
        -- Don't throw errors on unmount - it might already be unmounted
        when (ret /= 0) $ do
            errno <- getErrno
            hPutStrLn stderr $ "Warning: Failed to unmount " ++ path ++
                               " (errno: " ++ show errno ++ ")"

-- | Find and unmount all bind mounts in a directory
unmountAllBindMounts :: FilePath -> IO ()
unmountAllBindMounts dir = do
    -- Simple approach: try to unmount common mount points
    let possibleMounts = [
            dir </> "store",
            dir </> "out",
            dir </> "tmp"
            ]

    -- Try to unmount each, ignoring errors
    forM_ possibleMounts $ \path -> do
        catch (unmountPath path) $ \(_ :: IOError) -> return ()

    -- In a real implementation, we would scan /proc/mounts to find
    -- all mounts related to this sandbox

-- | Prepare process configuration for a command run in the sandbox
sandboxedProcessConfig :: FilePath -> FilePath -> [String] -> Map Text Text -> SandboxConfig -> ProcessConfig () () ()
sandboxedProcessConfig sandboxDir programPath args envVars config =
    -- In a real implementation, this would set up the proper namespaces,
    -- cgroups, and other isolation mechanisms.
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
        , ("PATH", "/bin:/usr/bin:/usr/local/bin") -- Explicit PATH
        ]

-- | Drop privileges to run as unprivileged user
dropPrivileges :: String -> String -> IO ()
dropPrivileges userName groupName = do
    -- Get the current (effective) user ID - should be root
    euid <- getEffectiveUserID

    -- Only proceed if we're running as root
    when (euid == 0) $ do
        -- Get user and group info
        userEntry <- getUserEntryForName userName
        groupEntry <- getGroupEntryForName groupName

        -- Set the group ID first (must be done before dropping user privileges)
        setGroupID (groupID groupEntry)

        -- Then set the user ID
        setUserID (userID userEntry)

        -- Verify the change
        newEuid <- getEffectiveUserID
        when (newEuid == 0) $
            error "Failed to drop privileges - still running as root"

-- | Regain privileges (back to root)
regainPrivileges :: IO ()
regainPrivileges = do
    -- Set effective user ID back to 0 (root)
    -- This only works if the real UID is root
    setEffectiveUserID 0

    -- Verify
    euid <- getEffectiveUserID
    when (euid /= 0) $
        error "Failed to regain privileges - not running as root"

-- | Execute action with dropped privileges
withDroppedPrivileges :: String -> String -> IO a -> IO a
withDroppedPrivileges user group action =
    bracket
        (dropPrivileges user group)
        (\_ -> regainPrivileges)
        (\_ -> action)

-- | Run a builder process as an unprivileged user
runBuilderAsUser :: FilePath -> [String] -> String -> String -> Map Text Text -> IO (ExitCode, String, String)
runBuilderAsUser program args user group env = do
    -- Get current UID to check if we're root
    uid <- getRealUserID

    if uid == 0
        then do
            -- We're root, so use privilege dropping
            withDroppedPrivileges user group $ do
                -- Execute the builder as the unprivileged user
                readProcessWithExitCode program args ""
        else do
            -- Not running as root, just execute directly
            readProcessWithExitCode program args ""

-- | Helper to set ownership of a file or directory
setOwnerAndGroup :: FilePath -> UserID -> GroupID -> IO ()
setOwnerAndGroup path uid gid = do
    -- Check if path exists
    exists <- doesPathExist path
    when exists $ do
        -- Set ownership
        setOwnerAndGroup path uid gid

-- | Bitwise OR for CInt flags
(.|.) :: CInt -> CInt -> CInt
a .|. b = a + b  -- Simple bit OR implementation for mount flags
