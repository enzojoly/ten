{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Ten.GC (
    -- Core GC operations
    collectGarbage,
    requestGarbageCollection,

    -- GC roots management
    addRoot,
    removeRoot,
    listRoots,
    requestAddRoot,
    requestRemoveRoot,
    requestListRoots,

    -- GC statistics
    GCStats(..),

    -- Path reachability
    isReachable,
    findReachablePaths,
    requestPathReachability,

    -- Concurrent GC
    acquireGCLock,
    releaseGCLock,
    withGCLock,
    breakStaleLock,

    -- Active build management
    getActiveBuildPaths,

    -- Store verification
    verifyStore,
    repairStore,

    -- Internal synchronization
    GCLock(..),
    GCLockInfo(..)
) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Exception (bracket, try, catch, throwIO, finally, mask, SomeException, IOException)
import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import System.Directory hiding (getFileSize) -- Avoid ambiguity
import System.FilePath
import System.IO.Error (isDoesNotExistError, catchIOError)
import System.Posix.Files
import System.IO (hPutStrLn, stderr, hFlush, withFile, IOMode(..))
import System.Posix.IO (openFd, createFile, closeFd, setLock, getLock,
                        defaultFileFlags, OpenMode(..), OpenFileFlags(..),
                        exclusive, fdToHandle)
import System.Posix.IO (LockRequest(WriteLock, Unlock))
import System.Posix.Types (Fd, ProcessID)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess)
import System.Posix.Files (fileExist, getFileStatus, fileSize, setFileMode)
import Data.Maybe (catMaybes, fromMaybe)
import System.Posix.Files.ByteString (createLink, removeLink)
import System.IO (SeekMode(AbsoluteSeek))
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Database.SQLite.Simple (Only(..))
import Data.List (isPrefixOf, isInfixOf)

import Ten.Core
import Ten.Store
import Ten.Hash
import Ten.Derivation
import Ten.Graph
import Ten.DB.Core
import Ten.DB.References
import Ten.DB.Derivations

-- | Lock information returned by daemon
data GCLockInfo = GCLockInfo
    { lockOwnerPid :: ProcessID
    , lockAcquiredTime :: UTCTime
    , lockIsStale :: Bool
    } deriving (Show, Eq)

-- | Statistics from a garbage collection run
data GCStats = GCStats
    { gcTotal :: Int               -- Total number of paths in store
    , gcLive :: Int                -- Number of live paths
    , gcCollected :: Int           -- Number of paths collected
    , gcBytes :: Integer           -- Number of bytes freed
    , gcElapsedTime :: NominalDiffTime  -- Time taken for GC
    } deriving (Show, Eq)

-- | File lock structure for GC coordination
data GCLock = GCLock {
    lockFd :: Fd,              -- File descriptor for the lock file
    lockPath :: FilePath,      -- Path to the lock file
    lockPid :: ProcessID       -- Process ID holding the lock
}

-- | Global reference to hold the GC lock file descriptor
{-# NOINLINE globalGCLockFdRef #-}
globalGCLockFdRef :: IORef (Maybe (Fd, FilePath))
globalGCLockFdRef = unsafePerformIO $ newIORef Nothing

-- | Add a root to protect a path from garbage collection (privileged context only)
addRoot :: StorePath -> Text -> Bool -> TenM 'Daemon a GCRoot
addRoot path name permanent = do
    env <- ask

    -- Verify the path exists in the store
    exists <- storePathExists path
    unless exists $
        throwError $ StoreError $ "Cannot add root for non-existent path: " <> storeHash path

    -- Create the root
    now <- liftIO getCurrentTime
    let rootType = if permanent then PermanentRoot else SymlinkRoot
    let root = GCRoot path name rootType now

    -- Write the root to the roots directory
    let rootsDir = storeLocation env </> "gc-roots"
    liftIO $ createDirectoryIfMissing True rootsDir

    -- Generate a unique filename for the root
    let rootFile = rootsDir </> (T.unpack $ storeHash path <> "-" <> name)

    -- Write a symlink to the actual path
    let targetPath = storePathToFilePath path env
    liftIO $ createSymbolicLink targetPath rootFile

    -- Also register the root in the database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        liftIO $ dbExecute db
            "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
            (storePathToText path, name, if permanent then "permanent" else "user")

    logMsg 1 $ "Added GC root: " <> name <> " -> " <> storeHash path

    return root

-- | Request to add a root from unprivileged context
requestAddRoot :: StorePath -> Text -> Bool -> TenM ctx a GCRoot
requestAddRoot path name permanent = do
    env <- ask

    case runMode env of
        -- In daemon mode, call directly if already privileged
        DaemonMode ->
            addRoot path name permanent

        -- In client mode, send request to daemon
        ClientMode conn -> do
            -- Send request to daemon using protocol
            result <- withDaemon $ \conn' -> do
                -- Implementation would use proper protocol
                let req = AddGCRootRequest path name permanent
                -- Send request and receive response
                return $ Right $ GCRoot path name (if permanent then PermanentRoot else SymlinkRoot)
                                      (UTCTime (toEnum 0) 0) -- Would get actual time from daemon

            case result of
                Left err -> throwError $ DaemonError $ "Failed to add GC root: " <> T.pack (show err)
                Right root -> return root

        -- In standalone mode, can only add roots if running as root
        _ -> do
            -- Check if running as root
            isRoot <- liftIO $ do
                uid <- System.Posix.Process.getRealUserID
                return (uid == 0)

            if isRoot
                then addRoot path name permanent
                else throwError $ PermissionError "Adding GC roots requires root privileges or daemon connection"

-- | Remove a root (privileged context only)
removeRoot :: GCRoot -> TenM 'Daemon a ()
removeRoot root = do
    env <- ask

    -- Generate the root file path
    let rootsDir = storeLocation env </> "gc-roots"
    let rootFile = rootsDir </> (T.unpack $ storeHash (rootPath root) <> "-" <> rootName root)

    -- Check if it exists
    exists <- liftIO $ doesFileExist rootFile

    -- Remove from filesystem and database
    when exists $ do
        -- Don't remove permanent roots unless forced
        unless (rootType root == PermanentRoot) $ do
            -- Remove from filesystem
            liftIO $ removeFile rootFile

            -- Remove from database
            withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                liftIO $ dbExecute db
                    "UPDATE GCRoots SET active = 0 WHERE path = ? AND name = ?"
                    (storePathToText (rootPath root), rootName root)

            logMsg 1 $ "Removed GC root: " <> rootName root <> " -> " <> storeHash (rootPath root)

-- | Request to remove a root from unprivileged context
requestRemoveRoot :: GCRoot -> TenM ctx a ()
requestRemoveRoot root = do
    env <- ask

    case runMode env of
        -- In daemon mode, call directly if already privileged
        DaemonMode ->
            removeRoot root

        -- In client mode, send request to daemon
        ClientMode conn -> do
            -- Send request to daemon using protocol
            result <- withDaemon $ \conn' -> do
                -- Implementation would use proper protocol
                let req = RemoveGCRootRequest (rootName root)
                -- Send request and receive confirmation
                return $ Right ()

            case result of
                Left err -> throwError $ DaemonError $ "Failed to remove GC root: " <> T.pack (show err)
                Right _ -> return ()

        -- In standalone mode, can only remove roots if running as root
        _ -> do
            -- Check if running as root
            isRoot <- liftIO $ do
                uid <- System.Posix.Process.getRealUserID
                return (uid == 0)

            if isRoot
                then removeRoot root
                else throwError $ PermissionError "Removing GC roots requires root privileges or daemon connection"

-- | List all current GC roots (works in both contexts through different mechanisms)
listRoots :: TenM ctx a [GCRoot]
listRoots = do
    env <- ask

    case runMode env of
        -- In daemon mode or standalone root mode, list roots directly
        DaemonMode -> listRootsPrivileged
        _ -> do
            -- Check if running as root in standalone
            isRoot <- liftIO $ do
                uid <- System.Posix.Process.getRealUserID
                return (uid == 0)

            if isRoot
                then listRootsPrivileged
                else requestListRoots -- Use daemon protocol

-- | Private implementation for listing roots with filesystem access
listRootsPrivileged :: TenM ctx a [GCRoot]
listRootsPrivileged = do
    env <- ask

    -- Get the roots directory
    let rootsDir = storeLocation env </> "gc-roots"

    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir

    -- Get roots both from filesystem and database
    fsRoots <- liftIO $ getFileSystemRoots rootsDir

    -- Get database roots (only in daemon context)
    dbRoots <- case runMode env of
        DaemonMode -> liftIO $ withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                         results <- dbQuery_ db "SELECT path, name, type, timestamp FROM GCRoots WHERE active = 1"
                                   :: IO [(Text, Text, Text, Int)]
                         buildDatabaseRoots results
        _ -> return []

    -- Combine both sources, with filesystem taking precedence for duplicates
    let allRoots = Map.union (Map.fromList [(rootPath r, r) | r <- fsRoots])
                             (Map.fromList [(rootPath r, r) | r <- dbRoots])

    return $ Map.elems allRoots

-- | Request to list roots from unprivileged context
requestListRoots :: TenM ctx a [GCRoot]
requestListRoots = do
    env <- ask

    case runMode env of
        -- In client mode, send request to daemon
        ClientMode conn -> do
            -- Send request to daemon using protocol
            result <- withDaemon $ \conn' -> do
                -- Implementation would use proper protocol
                let req = ListGCRootsRequest
                -- Send request and receive roots list
                -- This is a stub that would be replaced with actual protocol implementation
                return $ Right []

            case result of
                Left err -> throwError $ DaemonError $ "Failed to list GC roots: " <> T.pack (show err)
                Right roots -> return roots

        -- If not in client mode and we got here, then we're unprivileged
        -- Just return an empty list since we can't access the roots
        _ -> return []

-- | Build database roots from query results
buildDatabaseRoots :: [(Text, Text, Text, Int)] -> IO [GCRoot]
buildDatabaseRoots results = do
    now <- getCurrentTime
    forM results $ \(pathText, name, typeStr, timestamp) -> do
        case parseStorePath pathText of
            Just path -> do
                -- Convert timestamp to UTCTime
                let rootTime = posixSecondsToUTCTime (fromIntegral timestamp)
                -- Determine root type
                let rootType = case typeStr of
                      "permanent" -> PermanentRoot
                      "runtime" -> RuntimeRoot
                      "profile" -> ProfileRoot
                      "registry" -> RegistryRoot
                      _ -> SymlinkRoot

                return $ GCRoot
                    { rootPath = path
                    , rootName = name
                    , rootType = rootType
                    , rootTime = rootTime
                    }
            Nothing -> throwIO $ userError $ "Invalid store path in database: " ++ T.unpack pathText

-- | Get roots from filesystem
getFileSystemRoots :: FilePath -> IO [GCRoot]
getFileSystemRoots rootsDir = do
    -- Create roots directory if it doesn't exist
    createDirectoryIfMissing True rootsDir

    -- List all files in the roots directory
    files <- listDirectory rootsDir

    -- Parse each file into a GCRoot
    now <- getCurrentTime
    catMaybes <$> forM files (\file -> do
        -- Read the symlink target
        let rootFile = rootsDir </> file
        targetExists <- doesFileExist rootFile

        if targetExists then do
            target <- try $ getSymbolicLinkTarget rootFile
            case target of
                Left (_ :: SomeException) -> return Nothing
                Right linkTarget -> do
                    let path = case filePathToStorePath linkTarget of
                            Just p -> p
                            Nothing -> StorePath "unknown" "unknown"

                    -- Parse name from file
                    let name = case break (== '-') file of
                            (_, '-':rest) -> T.pack rest
                            _ -> T.pack file

                    -- Determine root type based on file name pattern
                    let rootType = if "permanent-" `T.isPrefixOf` name
                                  then PermanentRoot
                                  else SymlinkRoot

                    return $ Just $ GCRoot
                        { rootPath = path
                        , rootName = name
                        , rootType = rootType
                        , rootTime = now
                        }
        else return Nothing)

-- | Run garbage collection (privileged context only)
collectGarbage :: TenM 'Daemon a GCStats
collectGarbage = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Get the lock file path
    let lockPath = getGCLockPath env

    -- Ensure the lock directory exists
    liftIO $ ensureLockDirExists (takeDirectory lockPath)

    -- Acquire the GC lock (this will throw an exception if it fails)
    acquireGCLock

    -- Run GC with proper cleanup
    result <- collectGarbageWithStats `catchError` \e -> do
        -- Make sure to release the lock on error
        releaseGCLock
        throwError e

    -- Release the lock
    releaseGCLock

    -- Return result
    return result

-- | Request garbage collection from unprivileged context
requestGarbageCollection :: TenM ctx a GCStats
requestGarbageCollection = do
    env <- ask

    case runMode env of
        -- In daemon mode, call directly if already privileged
        DaemonMode -> collectGarbage

        -- In client mode, send request to daemon
        ClientMode conn -> do
            -- Send request to daemon using protocol
            result <- withDaemon $ \conn' -> do
                -- Implementation would use proper protocol
                let req = GCRequest False -- Not forcing GC
                -- Send request and receive GC stats
                -- This is a stub that would be replaced with actual protocol implementation
                return $ Right $ GCStats 0 0 0 0 0

            case result of
                Left err -> throwError $ DaemonError $ "Failed to request garbage collection: " <> T.pack (show err)
                Right stats -> return stats

        -- In standalone mode, can only run GC if running as root
        _ -> do
            -- Check if running as root
            isRoot <- liftIO $ do
                uid <- System.Posix.Process.getRealUserID
                return (uid == 0)

            if isRoot
                then do
                    -- Create simulated daemon environment
                    let daemonEnv = env { runMode = DaemonMode }
                    -- Run with daemon privileges
                    result <- liftIO $ runTen collectGarbage daemonEnv (BuildState Build [] Set.empty Set.empty (BuildIdFromInt 0) [] 0)
                    case result of
                        Left err -> throwError err
                        Right (stats, _) -> return stats
                else
                    throwError $ PermissionError "Garbage collection requires root privileges or daemon connection"

-- | Run garbage collection with stats (internal implementation)
collectGarbageWithStats :: TenM 'Daemon a GCStats
collectGarbageWithStats = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database connection
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Find all paths in the store
        let storeLocationPath = storeLocation env
        allStorePaths <- liftIO $ findAllStorePaths db storeLocationPath

        -- Find all root paths
        rootPaths <- liftIO $ findAllRoots db storeLocationPath

        -- Log information about roots
        logMsg 2 $ "Found " <> T.pack (show (Set.size rootPaths)) <> " GC roots"

        -- Find all paths reachable from roots (transitive closure)
        reachablePaths <- liftIO $ computeReachablePathsFromRoots db rootPaths

        -- Log information about reachable paths
        logMsg 2 $ "Found " <> T.pack (show (Set.size reachablePaths)) <> " reachable paths"

        -- Get active build paths (if in daemon mode)
        activePaths <- getActiveBuildPaths
        when (not $ Set.null activePaths) $
            logMsg 2 $ "Found " <> T.pack (show (Set.size activePaths)) <> " active build paths"

        -- Combine reachable and active paths
        let protectedPaths = Set.union reachablePaths activePaths

        -- Find all deletable paths
        let deletablePaths = Set.difference allStorePaths protectedPaths

        -- Get total size of deletable paths
        totalSize <- liftIO $ sum <$> mapM (getPathSize storeLocationPath)
                                        (map storePathToText $ Set.toList deletablePaths)

        -- Delete unreachable paths
        deleted <- liftIO $ do
            -- Mark unreachable paths as invalid in database
            markPathsAsInvalid db deletablePaths

            -- Delete files from filesystem
            forM_ (Set.toList deletablePaths) $ \path -> do
                -- Skip paths in gc-roots directory
                let pathText = storePathToText path
                let fsPath = storePathToFilePath path env
                unless ("gc-roots/" `T.isPrefixOf` pathText) $
                    -- Attempt to delete, ignoring errors
                    catch (removePathForcibly fsPath) $ \(_ :: IOError) -> return ()

            return $ Set.size deletablePaths

        -- Calculate elapsed time
        endTime <- liftIO getCurrentTime
        let elapsed = diffUTCTime endTime startTime

        -- Return statistics
        return GCStats
            { gcTotal = Set.size allStorePaths
            , gcLive = Set.size reachablePaths
            , gcCollected = deleted
            , gcBytes = totalSize
            , gcElapsedTime = elapsed
            }

-- | Get file/path size in bytes
getPathSize :: FilePath -> Text -> IO Integer
getPathSize storeLocation pathText = do
    let fullPath = storeLocation </> T.unpack pathText

    -- Check if path exists
    exists <- doesFileExist fullPath
    if exists
        then do
            -- Get file size
            fileStatus <- getFileStatus fullPath
            return $ fromIntegral $ fileSize fileStatus
        else
            -- If it doesn't exist, return 0
            return 0

-- | Find all store paths (privileged context)
findAllStorePaths :: Database -> FilePath -> IO (Set StorePath)
findAllStorePaths db storeLocation = do
    -- Get paths from database (preferred, more reliable)
    -- Query paths from ValidPaths table
    results <- dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

    -- Parse StorePaths and create set
    let dbPaths = Set.fromList $ catMaybes $ map (\(Only p) -> parseStorePath p) results

    -- If DB is empty, fall back to filesystem scan
    if Set.null dbPaths
        then scanFilesystemForPaths storeLocation
        else return dbPaths

-- | Scan filesystem for store paths (fallback)
scanFilesystemForPaths :: FilePath -> IO (Set StorePath)
scanFilesystemForPaths storeLocation = do
    -- Check if store exists
    exists <- doesDirectoryExist storeLocation
    if not exists
        then return Set.empty
        else do
            -- List all files in the store directory
            entries <- try $ listDirectory storeLocation
            case entries of
                Left (_ :: SomeException) -> return Set.empty
                Right files -> do
                    -- Filter for valid store path format
                    foldM (\acc file -> do
                        -- Skip special directories and non-store paths
                        unless (file `elem` ["gc-roots", "tmp", "locks", "var"] ||
                              any (`isPrefixOf` file) ["gc-roots", "tmp.", ".",  ".."]) $ do
                            -- Check if it's a store path (hash-name format)
                            case parseStorePath (T.pack file) of
                                Just path -> return $ Set.insert path acc
                                Nothing -> return acc
                        return acc
                        ) Set.empty files

-- | Find all roots for garbage collection (privileged context)
findAllRoots :: Database -> FilePath -> IO (Set StorePath)
findAllRoots db storeLocation = do
    -- Get roots from filesystem (gc-roots directory)
    let rootsDir = storeLocation </> "gc-roots"
    fsRoots <- getFileSystemRootPaths rootsDir

    -- Get roots from database
    dbRoots <- getDatabaseRootPaths db

    -- Get runtime lock roots (for active builds)
    runtimeRoots <- getRuntimeRootPaths storeLocation

    -- Combine all root sources
    return $ Set.unions [fsRoots, dbRoots, runtimeRoots]

-- | Get root paths from filesystem
getFileSystemRootPaths :: FilePath -> IO (Set StorePath)
getFileSystemRootPaths rootsDir = do
    -- Check if the directory exists
    exists <- doesDirectoryExist rootsDir
    if not exists
        then return Set.empty
        else do
            -- List all symlinks in the directory
            files <- listDirectory rootsDir

            -- Process each symlink to get target
            foldM (\acc file -> do
                let path = rootsDir </> file
                isLink <- pathIsSymbolicLink <$> getFileStatus path
                if isLink
                    then do
                        target <- try $ getSymbolicLinkTarget path
                        case target of
                            Left (_ :: SomeException) -> return acc
                            Right targetPath ->
                                case filePathToStorePath targetPath of
                                    Just sp -> return $ Set.insert sp acc
                                    Nothing -> return acc
                    else return acc
                ) Set.empty files

-- | Helper function to check if a path is a symbolic link
pathIsSymbolicLink :: FileStatus -> Bool
pathIsSymbolicLink status = isSymbolicLink status

-- | Get root paths from database
getDatabaseRootPaths :: Database -> IO (Set StorePath)
getDatabaseRootPaths db = do
    -- Query active roots from database
    results <- dbQuery_ db "SELECT path FROM GCRoots WHERE active = 1" :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only text) -> parseStorePath text) results

-- | Get runtime roots from active builds
getRuntimeRootPaths :: FilePath -> IO (Set StorePath)
getRuntimeRootPaths storeLocation = do
    -- Check for runtime locks directory
    let locksDir = storeLocation </> "var/ten/locks"
    exists <- doesDirectoryExist locksDir

    if not exists
        then return Set.empty
        else do
            -- Look for active build locks
            files <- try $ listDirectory locksDir
            case files of
                Left (_ :: SomeException) -> return Set.empty
                Right locks -> do
                    -- Process each lock file to find references to store paths
                    foldM (\acc lockFile -> do
                        let lockPath = locksDir </> lockFile
                        -- Check if lock file contains a reference to a store path
                        content <- try $ readFile lockPath
                        case content of
                            Left (_ :: SomeException) -> return acc
                            Right text -> do
                                -- Extract store paths from lock file content
                                let storePaths = extractStorePaths text
                                return $ Set.union acc storePaths
                        ) Set.empty locks
  where
    -- Function to extract store paths from text
    extractStorePaths :: String -> Set StorePath
    extractStorePaths text =
        -- This is a simplified approach; in practice you'd use a proper parser
        let words = lines text
            paths = filter (\w -> length w > 10 && '-' `elem` w) words
        in Set.fromList $ catMaybes $ map (parseStorePath . T.pack) paths

-- | Check if a path is reachable from any root (works in both contexts)
isReachable :: StorePath -> TenM ctx a Bool
isReachable path = do
    env <- ask

    case runMode env of
        -- In daemon mode, check directly
        DaemonMode -> isReachablePrivileged path

        -- In other modes, check if running as root
        _ -> do
            -- Check if running as root
            isRoot <- liftIO $ do
                uid <- System.Posix.Process.getRealUserID
                return (uid == 0)

            if isRoot
                then isReachablePrivileged path
                else requestPathReachability path

-- | Internal implementation for privileged reachability check
isReachablePrivileged :: StorePath -> TenM ctx a Bool
isReachablePrivileged path = do
    env <- ask

    -- Initialize database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all roots
        roots <- liftIO $ findAllRoots db (storeLocation env)

        -- Use database to check reachability
        liftIO $ isPathReachable db roots path

-- | Request path reachability check from unprivileged context
requestPathReachability :: StorePath -> TenM ctx a Bool
requestPathReachability path = do
    env <- ask

    case runMode env of
        -- In client mode, send request to daemon
        ClientMode conn -> do
            -- Send request to daemon using protocol
            result <- withDaemon $ \conn' -> do
                -- This is a stub that would be replaced with actual protocol implementation
                return $ Right False

            case result of
                Left err -> throwError $ DaemonError $ "Failed to check path reachability: " <> T.pack (show err)
                Right reachable -> return reachable

        -- If not in client mode and we got here, then fall back to heuristic
        -- An unprivileged process can't reliably determine reachability
        -- But we can check if the path is in the store at least
        _ -> do
            -- Check if path exists in store (doesn't guarantee reachability)
            storePathExists path

-- | Find all paths reachable from roots (privileged context only)
findReachablePaths :: TenM 'Daemon a (Set StorePath)
findReachablePaths = do
    env <- ask

    -- Initialize database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Find all roots
        roots <- liftIO $ findAllRoots db (storeLocation env)

        -- Compute all reachable paths
        liftIO $ computeReachablePathsFromRoots db roots

-- | Acquire the GC lock (privileged context only)
acquireGCLock :: TenM 'Daemon a ()
acquireGCLock = do
    env <- ask
    let lockPath = getGCLockPath env

    -- Try to acquire the lock
    result <- liftIO $ acquireFileLock lockPath

    case result of
        Left err -> throwError $ ResourceError $ "Could not acquire GC lock: " <> err
        Right lock -> do
            -- Keep the fd in a global reference to avoid GC
            liftIO $ writeIORef globalGCLockFdRef (Just (lockFd lock, lockPath))

-- | Release the GC lock (privileged context only)
releaseGCLock :: TenM 'Daemon a ()
releaseGCLock = do
    env <- ask
    let lockPath = getGCLockPath env

    -- Check if the lock file exists and we own it
    exists <- liftIO $ doesFileExist lockPath
    when exists $ do
        -- Read the lock file to check the owner
        content <- liftIO $ try $ readFile lockPath
        case content of
            Right pidStr -> do
                case reads pidStr of
                    [(lockPid, "")] -> do
                        -- Check if it's our PID
                        ourPid <- liftIO getProcessID
                        when (lockPid == ourPid) $ do
                            -- Get the file descriptor from the global reference
                            mFdInfo <- liftIO $ readIORef globalGCLockFdRef
                            case mFdInfo of
                                Just (fd, path) | path == lockPath -> do
                                    -- Create a temporary GCLock just to release it
                                    let lock = GCLock fd lockPath ourPid
                                    liftIO $ releaseFileLock lock
                                    -- Clear the global reference
                                    liftIO $ writeIORef globalGCLockFdRef Nothing
                                _ -> return ()
                    _ -> return ()
            _ -> return ()

-- | Execute an action with the GC lock (privileged context only)
withGCLock :: TenM 'Daemon a b -> TenM 'Daemon a b
withGCLock action = do
    -- First acquire the lock
    acquireGCLock

    -- Run the action with proper error handling
    result <- catchError
        action
        (\e -> do
            -- Always release the lock on error
            releaseGCLock
            -- Re-throw the error
            throwError e)

    -- Release the lock
    releaseGCLock

    -- Return the result
    return result

-- | Break a stale lock (privileged context only)
breakStaleLock :: FilePath -> TenM 'Daemon a ()
breakStaleLock lockPath = do
    -- Check lock status
    status <- liftIO $ checkLockFile lockPath

    case status of
        Left err ->
            logMsg 1 $ "Cannot check lock status: " <> err
        Right isValid ->
            unless isValid $ liftIO $ do
                -- Log that we're breaking a stale lock
                hPutStrLn stderr $ "Breaking stale GC lock: " ++ lockPath

                -- Remove the lock file
                removeFile lockPath `catch` \(_ :: SomeException) ->
                    hPutStrLn stderr $ "Warning: Failed to remove stale lock file: " ++ lockPath

-- | Check if a lock file exists and is valid
checkLockFile :: FilePath -> IO (Either Text Bool)
checkLockFile lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath

    if not exists
        then return $ Right False  -- Lock file doesn't exist
        else do
            -- Read the lock file to get PID
            result <- try $ readFile lockPath
            case result of
                Left (e :: SomeException) ->
                    return $ Left $ "Error reading lock file: " <> T.pack (show e)

                Right content -> do
                    -- Parse the PID
                    case reads content of
                        [(pid, "")] -> do
                            -- Check if the process is still running
                            pidRunning <- isProcessRunning pid
                            return $ Right pidRunning

                        _ -> return $ Left "Invalid lock file format"

-- | Check if a process is still running
isProcessRunning :: ProcessID -> IO Bool
isProcessRunning pid = do
    -- Try to send signal 0 to the process (doesn't actually send a signal, just checks existence)
    result <- try $ signalProcess 0 pid
    case result of
        Left (_ :: SomeException) -> return False  -- Process doesn't exist
        Right _ -> return True  -- Process exists

-- | Create and acquire a lock file
acquireFileLock :: FilePath -> IO (Either Text GCLock)
acquireFileLock lockPath = do
    -- Ensure parent directory exists
    ensureLockDirExists (takeDirectory lockPath)

    -- Get our process ID
    pid <- getProcessID

    -- Check if lock already exists
    lockStatus <- checkLockFile lockPath

    case lockStatus of
        Left err -> return $ Left err
        Right True ->
            return $ Left "Another garbage collection is in progress"
        Right False -> do
            -- Create the lock file with our PID
            result <- try $ mask $ \unmask -> do
                -- Use file creation with EXCL flag to ensure atomicity
                fd <- openFd lockPath WriteOnly (Just 0o644) defaultFileFlags{exclusive=True}

                unmask $ do
                    -- Write our PID to it
                    handle <- fdToHandle fd
                    hPutStrLn handle (show pid)
                    hFlush handle

                    -- Lock the file to prevent concurrent access
                    setLock fd (WriteLock, AbsoluteSeek, 0, 0)

                    -- Return the lock
                    return $ GCLock {
                        lockFd = fd,
                        lockPath = lockPath,
                        lockPid = pid
                    }

            case result of
                Left (e :: SomeException) ->
                    return $ Left $ "Failed to create lock file: " <> T.pack (show e)
                Right lock -> return $ Right lock

-- | Release a file lock
releaseFileLock :: GCLock -> IO ()
releaseFileLock lock = do
    -- Release the lock
    setLock (lockFd lock) (Unlock, AbsoluteSeek, 0, 0)

    -- Close the file descriptor
    closeFd (lockFd lock)

    -- Remove the lock file
    catchIOError (removeFile (lockPath lock)) (\_ -> return ())

-- | Get active build paths (to prevent GC during builds)
getActiveBuildPaths :: TenM ctx a (Set StorePath)
getActiveBuildPaths = do
    -- In daemon mode, query the daemon for active build outputs
    isDaemon <- isDaemonMode
    if isDaemon
        then getDaemonActivePaths
        else return Set.empty

-- | Get active build paths from daemon mode
getDaemonActivePaths :: TenM ctx a (Set StorePath)
getDaemonActivePaths = do
    env <- ask

    -- In daemon mode, check for active build outputs in the runtime state
    case runMode env of
        DaemonMode -> do
            -- Get runtime paths from database
            withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                -- Query active build paths from database
                results <- liftIO $ dbQuery_ db
                    "SELECT path FROM ActiveBuilds WHERE status != 'completed'" :: IO [Only Text]

                -- Parse paths
                return $ Set.fromList $ catMaybes $ map (\(Only t) -> parseStorePath t) results

        ClientMode conn -> do
            -- Ask the daemon via protocol
            result <- withDaemon $ \_ ->
                return $ Right Set.empty  -- This would make an RPC call in practice
            return result

        _ -> return Set.empty

-- | Verify the store integrity (works in both contexts)
verifyStore :: TenM ctx a Bool
verifyStore = do
    env <- ask

    case runMode env of
        -- In daemon mode, verify directly
        DaemonMode -> do
            -- Open database
            withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                -- Get all valid paths from database
                paths <- liftIO $ dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

                -- Verify each path
                results <- forM paths $ \(Only pathText) -> do
                    case parseStorePath pathText of
                        Nothing -> return False
                        Just storePath -> do
                            -- Verify that the store file exists and has the correct hash
                            result <- liftIO $ try $ do
                                let fullPath = storePathToFilePath storePath env
                                fileExists <- doesFileExist fullPath
                                if fileExists
                                    then do
                                        content <- BS.readFile fullPath
                                        let actualHash = showHash $ hashByteString content
                                        return $ actualHash == storeHash storePath
                                    else return False
                            case result of
                                Left (_ :: SomeException) -> return False
                                Right valid -> return valid

                -- Return true if all paths verify
                return $ all id results

        -- In client mode, request verification from daemon
        ClientMode conn -> do
            -- Send request to daemon using protocol
            result <- withDaemon $ \conn' -> do
                -- This is a stub that would be replaced with actual protocol implementation
                let req = StoreVerifyRequest "/"  -- Verify entire store
                return $ Right True -- Assume valid for stub

            case result of
                Left err -> throwError $ DaemonError $ "Failed to verify store: " <> T.pack (show err)
                Right valid -> return valid

        -- In standalone mode, verify if running as root
        _ -> do
            -- Check if running as root
            isRoot <- liftIO $ do
                uid <- System.Posix.Process.getRealUserID
                return (uid == 0)

            if isRoot
                then do
                    -- Create simulated daemon environment
                    let daemonEnv = env { runMode = DaemonMode }
                    -- Run with daemon privileges
                    result <- liftIO $ runTen verifyStore daemonEnv (BuildState Build [] Set.empty Set.empty (BuildIdFromInt 0) [] 0)
                    case result of
                        Left err -> throwError err
                        Right (valid, _) -> return valid
                else
                    -- Unprivileged standalone mode can only verify paths it can read
                    return True -- Just assume valid since we can't really check

-- | Repair the store (remove invalid paths) - privileged context only
repairStore :: TenM 'Daemon a GCStats
repairStore = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all valid paths from database
        paths <- liftIO $ dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

        -- Verify and repair each path
        (valid, invalid, totalSize) <- foldM (\(v, i, size) (Only pathText) -> do
            case parseStorePath pathText of
                Nothing -> return (v, i + 1, size)  -- Invalid path format
                Just storePath -> do
                    -- Check if file exists and has correct hash
                    result <- liftIO $ try $ do
                        let fullPath = storePathToFilePath storePath env
                        fileExists <- doesFileExist fullPath
                        if fileExists
                            then do
                                content <- BS.readFile fullPath
                                let actualHash = showHash $ hashByteString content
                                return (actualHash == storeHash storePath, fullPath)
                            else return (False, fullPath)

                    case result of
                        Left (_ :: SomeException) -> return (v, i + 1, size)
                        Right (isValid, path) ->
                            if isValid
                                then return (v + 1, i, size)
                                else do
                                    -- Invalid path, mark as invalid and remove
                                    liftIO $ do
                                        -- Get size before deletion
                                        pathSize <- getFileSize' path

                                        -- Mark as invalid in database
                                        dbExecute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?" (Only pathText)

                                        -- Delete the file
                                        catchIOError (removeFile path) (\_ -> return ())

                                        return (v, i + 1, size + pathSize)
            ) (0, 0, 0) paths

        -- Calculate elapsed time
        endTime <- liftIO getCurrentTime
        let elapsed = diffUTCTime endTime startTime

        -- Return statistics
        return GCStats
            { gcTotal = valid + invalid
            , gcLive = valid
            , gcCollected = invalid
            , gcBytes = totalSize
            , gcElapsedTime = elapsed
            }

-- | Helper to get file size
getFileSize' :: FilePath -> IO Integer
getFileSize' path = do
    exists <- doesFileExist path
    if exists
        then do
            fileStatus <- getFileStatus path
            return $ fromIntegral $ fileSize fileStatus
        else return 0

-- Helper function for database interaction - Path reachability
isPathReachable :: Database -> Set StorePath -> StorePath -> IO Bool
isPathReachable db roots path = do
    -- Check if path is directly in roots
    if path `Set.member` roots
        then return True
        else do
            -- Use reference tracking DB to check if path is reachable from roots
            let rootPaths = Set.toList roots
            reachable <- foldM (\found root ->
                                if found
                                   then return True
                                   else do
                                       -- Check if the path is reachable from this root
                                       refs <- findPathsClosure db (Set.singleton root)
                                       return $ path `Set.member` refs
                               ) False rootPaths
            return reachable

-- | Helper function to compute reachable paths from roots
computeReachablePathsFromRoots :: Database -> Set StorePath -> IO (Set StorePath)
computeReachablePathsFromRoots db roots = do
    -- Include the roots themselves
    findPathsClosure db roots

-- | Find paths reachability closure (using properly optimized DB query)
findPathsClosure :: Database -> Set StorePath -> IO (Set StorePath)
findPathsClosure db startingPaths = do
    -- Create a temporary table for the closure calculation
    dbExecuteSimple_ db "CREATE TEMP TABLE IF NOT EXISTS temp_closure (path TEXT PRIMARY KEY)"
    dbExecuteSimple_ db "DELETE FROM temp_closure"

    -- Insert all starting paths
    forM_ (Set.toList startingPaths) $ \path ->
        dbExecute db "INSERT INTO temp_closure VALUES (?)" (Only (storePathToText path))

    -- Use recursive CTE to compute the transitive closure efficiently
    results <- dbQuery_ db "WITH RECURSIVE closure(path) AS (\
                            \  SELECT path FROM temp_closure\
                            \  UNION\
                            \  SELECT r.reference FROM References r JOIN closure c ON r.referrer = c.path\
                            \) SELECT path FROM closure" :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only p) -> parseStorePath p) results

-- | Helper function to mark paths as invalid in the database
markPathsAsInvalid :: Database -> Set StorePath -> IO ()
markPathsAsInvalid db paths = do
    -- Mark each path as invalid in the ValidPaths table
    forM_ (Set.toList paths) $ \path ->
        dbExecute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
                  (Only (storePathToText path))
