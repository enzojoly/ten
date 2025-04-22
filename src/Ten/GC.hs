{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ten.GC (
    -- Core GC operations (daemon privileges only)
    collectGarbage,

    -- GC roots management (daemon privileges only)
    addRoot,
    removeRoot,
    listRoots,

    -- GC operations via protocol (builder context)
    requestGarbageCollection,
    requestAddRoot,
    requestRemoveRoot,
    requestListRoots,

    -- GC statistics
    GCStats(..),

    -- Path reachability (works in both contexts via different mechanisms)
    isReachable,
    findReachablePaths,
    requestPathReachability,

    -- Concurrent GC (daemon privileges only)
    acquireGCLock,
    releaseGCLock,
    withGCLock,
    breakStaleLock,

    -- Active build management (daemon privileges only)
    getActiveBuildPaths,

    -- Store verification (works in both contexts)
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
import Control.Monad.State (get, modify, gets)
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
import System.Directory hiding (pathIsSymbolicLink) -- Be explicit about which one we use
import qualified System.Directory as Dir
import System.FilePath
import System.IO.Error (isDoesNotExistError, catchIOError, isPermissionError)
import qualified System.Posix.Files as Posix
import System.IO (Handle, hPutStrLn, stderr, hFlush, withFile, IOMode(..))
import System.Posix.IO (openFd, createFile, closeFd, setLock, getLock,
                       defaultFileFlags, OpenMode(..), OpenFileFlags(..),
                       exclusive, fdToHandle, WriteLock(..), Unlock(..))
import System.Posix.Types (Fd, ProcessID)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess)
import qualified System.Posix.IO as PosixIO
import qualified System.Posix.Files.ByteString as ByteString
import System.IO (SeekMode(AbsoluteSeek))
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Database.SQLite.Simple (Only(..))
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Singletons
import Data.Singletons.TH
import Data.Maybe (fromMaybe, isJust, catMaybes)
import Network.Socket (Socket)

import Ten.Core

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

-- | Daemon connection for protocol communication
data DaemonConnection = DaemonConnection {
    connSocket :: Socket,            -- Socket connected to daemon
    connHandle :: Handle,            -- Handle for reading/writing to socket
    connAuthToken :: AuthToken,      -- Authentication token
    connUserId :: UserId,            -- Authenticated user ID
    connRequestId :: TVar Integer,   -- Current request ID
    connPendingRequests :: TVar (Map Integer (TMVar DaemonResponse)) -- Map of pending requests
}

-- | A daemon request to send through the protocol
data DaemonRequest
    = AddGCRootRequest StorePath Text Bool -- Path, name, permanent flag
    | RemoveGCRootRequest GCRoot
    | ListGCRootsRequest
    | GCRequest Bool -- Force flag
    | PathReachabilityRequest StorePath
    | StoreVerifyRequest
    | AbortGCRequest
    | GCStatusRequest
    deriving (Show, Eq)

-- | Response from daemon
data DaemonResponse
    = GCRootResponse GCRoot
    | GCRootListResponse [GCRoot]
    | GCResultResponse GCStats
    | PathReachabilityResponse Bool
    | StoreVerifyResponse Bool
    | SuccessResponse
    | ErrorResponse BuildError
    deriving (Show, Eq)

-- | Global reference to hold the GC lock file descriptor
{-# NOINLINE globalGCLockFdRef #-}
globalGCLockFdRef :: IORef (Maybe (Fd, FilePath))
globalGCLockFdRef = unsafePerformIO $ newIORef Nothing

-- | Add a root to protect a path from garbage collection (daemon privilege only)
addRoot :: SPrivilegeTier 'Daemon -> StorePath -> Text -> Bool -> TenM 'Build 'Daemon GCRoot
addRoot st path name permanent = do
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
    liftIO $ createFileLink targetPath rootFile

    -- Also register the root in the database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        tenExecute_ db
            "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
            (storePathToText path, name, if permanent then "permanent" else "user")

    -- Use the core logging function, not local one
    Ten.Core.logMsg 1 $ "Added GC root: " <> name <> " -> " <> storeHash path

    return root

-- | Request to add a root from builder context via protocol
requestAddRoot :: SPrivilegeTier 'Builder -> StorePath -> Text -> Bool -> TenM 'Build 'Builder GCRoot
requestAddRoot st path name permanent = do
    env <- ask

    -- Send request to daemon using protocol
    daemonConn <- getDaemonConnection
    result <- sendToDaemon st daemonConn $ AddGCRootRequest path name permanent

    case result of
        Left err ->
            throwError $ DaemonError $ "Failed to add GC root: " <> err
        Right (GCRootResponse root) ->
            return root
        Right resp ->
            throwError $ DaemonError $ "Unexpected response for add GC root: " <> T.pack (show resp)

-- | Remove a root (daemon privilege only)
removeRoot :: SPrivilegeTier 'Daemon -> GCRoot -> TenM 'Build 'Daemon ()
removeRoot st root = do
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

            -- Remove from database with proper privilege
            withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
                tenExecute_ db
                    "UPDATE GCRoots SET active = 0 WHERE path = ? AND name = ?"
                    (storePathToText (rootPath root), rootName root)

            Ten.Core.logMsg 1 $ "Removed GC root: " <> rootName root <> " -> " <> storeHash (rootPath root)

-- | Request to remove a root from builder context via protocol
requestRemoveRoot :: SPrivilegeTier 'Builder -> GCRoot -> TenM 'Build 'Builder ()
requestRemoveRoot st root = do
    env <- ask

    -- Send request to daemon using protocol
    daemonConn <- getDaemonConnection
    result <- sendToDaemon st daemonConn $ RemoveGCRootRequest root

    case result of
        Left err ->
            throwError $ DaemonError $ "Failed to remove root: " <> err
        Right SuccessResponse ->
            return ()
        Right resp ->
            throwError $ DaemonError $ "Unexpected response for remove GC root: " <> T.pack (show resp)

-- | List all current GC roots
-- Works in both contexts through different mechanisms
listRoots :: forall (t :: PrivilegeTier). SingI t => SPrivilegeTier t -> TenM 'Build t [GCRoot]
listRoots st = do
    env <- ask

    -- Dispatch based on privilege tier
    case fromSing st of
        -- In daemon context, list roots directly
        Daemon -> listRootsPrivileged st
        -- In builder context, use protocol
        Builder -> requestListRoots st

-- | Private implementation for listing roots with filesystem access
listRootsPrivileged :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon [GCRoot]
listRootsPrivileged st = do
    env <- ask

    -- Get the roots directory
    let rootsDir = storeLocation env </> "gc-roots"

    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir

    -- Get roots both from filesystem and database
    fsRoots <- liftIO $ getFileSystemRoots rootsDir

    -- Get database roots with proper privilege
    dbRoots <- withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        results <- tenQuery_ db "SELECT path, name, type, timestamp FROM GCRoots WHERE active = 1"
                  :: TenM 'Build 'Daemon [(Text, Text, Text, Int)]
        liftIO $ buildDatabaseRoots results

    -- Combine both sources, with filesystem taking precedence for duplicates
    let allRoots = Map.union (Map.fromList [(rootPath r, r) | r <- fsRoots])
                           (Map.fromList [(rootPath r, r) | r <- dbRoots])

    return $ Map.elems allRoots

-- | Request to list roots from builder context via protocol
requestListRoots :: SPrivilegeTier 'Builder -> TenM 'Build 'Builder [GCRoot]
requestListRoots st = do
    env <- ask

    -- Send request to daemon using protocol
    daemonConn <- getDaemonConnection
    result <- sendToDaemon st daemonConn ListGCRootsRequest

    case result of
        Left err ->
            throwError $ DaemonError $ "Failed to list GC roots: " <> err
        Right (GCRootListResponse roots) ->
            return roots
        Right resp ->
            throwError $ DaemonError $ "Unexpected response for list GC roots: " <> T.pack (show resp)

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
            target <- try $ Posix.readSymbolicLink rootFile
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

-- | Run garbage collection (daemon privilege only)
collectGarbage :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon GCStats
collectGarbage st = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Acquire the GC lock first
    withGCLock st $ \st' -> do
        -- Run the actual garbage collection with GC lock held
        collectGarbageWithStats st'

-- | Request garbage collection from builder context via protocol
requestGarbageCollection :: SPrivilegeTier 'Builder -> Bool -> TenM 'Build 'Builder GCStats
requestGarbageCollection st force = do
    env <- ask

    -- Send request to daemon using protocol
    daemonConn <- getDaemonConnection
    result <- sendToDaemon st daemonConn $ GCRequest force

    case result of
        Left err ->
            throwError $ DaemonError $ "Failed to request garbage collection: " <> err
        Right (GCResultResponse stats) ->
            return stats
        Right resp ->
            throwError $ DaemonError $ "Unexpected response for GC request: " <> T.pack (show resp)

-- | Run garbage collection with stats (internal implementation)
collectGarbageWithStats :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon GCStats
collectGarbageWithStats st = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database connection with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Find all paths in the store
        let storeLocationPath = storeLocation env
        allStorePaths <- liftIO $ findAllStorePaths db storeLocationPath

        -- Find all root paths
        rootPaths <- liftIO $ findAllRoots db storeLocationPath

        -- Log information about roots
        Ten.Core.logMsg 2 $ "Found " <> T.pack (show (Set.size rootPaths)) <> " GC roots"

        -- Find all paths reachable from roots (transitive closure)
        reachablePaths <- liftIO $ computeReachablePathsFromRoots db rootPaths

        -- Log information about reachable paths
        Ten.Core.logMsg 2 $ "Found " <> T.pack (show (Set.size reachablePaths)) <> " reachable paths"

        -- Get active build paths (if in daemon mode)
        activePaths <- getActiveBuildPaths st
        when (not $ Set.null activePaths) $
            Ten.Core.logMsg 2 $ "Found " <> T.pack (show (Set.size activePaths)) <> " active build paths"

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
                let fsPath = storeLocationPath </> T.unpack pathText
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
            fileStatus <- Posix.getFileStatus fullPath
            return $ fromIntegral $ Posix.fileSize fileStatus
        else
            -- If it doesn't exist, return 0
            return 0

-- | Find all store paths (daemon privilege context)
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

-- | Find all roots for garbage collection (daemon privilege context)
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
                isLink <- Posix.isSymbolicLink <$> Posix.getFileStatus path
                if isLink
                    then do
                        target <- try $ Posix.readSymbolicLink path
                        case target of
                            Left (_ :: SomeException) -> return acc
                            Right targetPath ->
                                case filePathToStorePath targetPath of
                                    Just sp -> return $ Set.insert sp acc
                                    Nothing -> return acc
                    else return acc
                ) Set.empty files

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

-- | Check if a path is reachable from any root
-- Works in both contexts through different mechanisms
isReachable :: forall (t :: PrivilegeTier). SingI t => SPrivilegeTier t -> StorePath -> TenM 'Build t Bool
isReachable st path = do
    env <- ask

    -- Dispatch based on privilege tier
    case fromSing st of
        -- In daemon context, check directly
        Daemon -> isReachablePrivileged st path
        -- In builder context, use protocol
        Builder -> requestPathReachability st path

-- | Internal implementation for privileged reachability check
isReachablePrivileged :: SPrivilegeTier 'Daemon -> StorePath -> TenM 'Build 'Daemon Bool
isReachablePrivileged st path = do
    env <- ask

    -- Initialize database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all roots
        roots <- liftIO $ findAllRoots db (storeLocation env)

        -- Use database to check reachability
        liftIO $ isPathReachable db roots path

-- | Request path reachability check from builder context via protocol
requestPathReachability :: SPrivilegeTier 'Builder -> StorePath -> TenM 'Build 'Builder Bool
requestPathReachability st path = do
    env <- ask

    -- Send request to daemon using protocol
    daemonConn <- getDaemonConnection
    result <- sendToDaemon st daemonConn $ PathReachabilityRequest path

    case result of
        Left err ->
            throwError $ DaemonError $ "Failed to check path reachability: " <> err
        Right (PathReachabilityResponse reachable) ->
            return reachable
        Right resp ->
            throwError $ DaemonError $ "Unexpected response for reachability check: " <> T.pack (show resp)

-- | Find all paths reachable from roots (daemon privilege only)
findReachablePaths :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon (Set StorePath)
findReachablePaths st = do
    env <- ask

    -- Initialize database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Find all roots
        roots <- liftIO $ findAllRoots db (storeLocation env)

        -- Compute all reachable paths
        liftIO $ computeReachablePathsFromRoots db roots

-- | Acquire the GC lock (daemon privilege only)
acquireGCLock :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon ()
acquireGCLock st = do
    env <- ask
    let lockPath = getGCLockPath env

    -- Ensure the lock directory exists
    liftIO $ ensureLockDirExists (takeDirectory lockPath)

    -- Try to acquire the lock with proper privilege verification
    result <- liftIO $ acquireFileLock lockPath

    case result of
        Left err -> throwError $ ResourceError $ "Could not acquire GC lock: " <> err
        Right lock -> do
            -- Keep the fd in a global reference to avoid GC
            liftIO $ writeIORef globalGCLockFdRef (Just (lockFd lock, lockPath))

-- | Release the GC lock (daemon privilege only)
releaseGCLock :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon ()
releaseGCLock st = do
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

-- | Execute an action with the GC lock (daemon privilege only)
withGCLock :: SPrivilegeTier 'Daemon -> (SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withGCLock st action = do
    -- First acquire the lock with privilege evidence
    acquireGCLock st

    -- Run the action with proper error handling
    action st `catchError` \e -> do
        -- Always release the lock on error
        releaseGCLock st
        -- Re-throw the error
        throwError e
      `finally` do
        -- Always release the lock on completion
        releaseGCLock st

-- | Break a stale lock (daemon privilege only)
breakStaleLock :: SPrivilegeTier 'Daemon -> FilePath -> TenM 'Build 'Daemon ()
breakStaleLock st lockPath = do
    -- Check lock status
    status <- liftIO $ checkLockFile lockPath

    case status of
        Left err ->
            Ten.Core.logMsg 1 $ "Cannot check lock status: " <> err
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

-- | Helper function for breakStaleLock that can be used from non-TenM contexts
breakStaleLock' :: FilePath -> IO ()
breakStaleLock' lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath
    when exists $ do
        -- Read the lock file
        content <- try $ readFile lockPath
        case content of
            Right pidStr -> do
                case reads pidStr of
                    [(lockPid, "")] -> do
                        -- Check if the process is still running
                        pidRunning <- isProcessRunning lockPid
                        unless pidRunning $ do
                            -- Remove the stale lock
                            hPutStrLn stderr $ "Breaking stale GC lock: " ++ lockPath
                            removeFile lockPath `catch` \(_ :: SomeException) ->
                                hPutStrLn stderr $ "Warning: Failed to remove stale lock file: " ++ lockPath
                    _ -> return ()
            _ -> return ()

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
                    setLock fd (PosixIO.WriteLock, AbsoluteSeek, 0, 0)

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
    setLock (lockFd lock) (PosixIO.Unlock, AbsoluteSeek, 0, 0)

    -- Close the file descriptor
    closeFd (lockFd lock)

    -- Remove the lock file
    catchIOError (removeFile (lockPath lock)) (\_ -> return ())

-- | Get active build paths (to prevent GC during builds)
getActiveBuildPaths :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon (Set StorePath)
getActiveBuildPaths st = do
    env <- ask

    -- Get runtime paths from database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Query active build paths from database
        results <- tenQuery_ db
            "SELECT path FROM ActiveBuilds WHERE status != 'completed'" :: TenM 'Build 'Daemon [Only Text]

        -- Parse paths
        return $ Set.fromList $ catMaybes $ map (\(Only t) -> parseStorePath t) results

-- | Verify the store integrity
-- Works in both contexts through different mechanisms
verifyStore :: forall (t :: PrivilegeTier). SingI t => SPrivilegeTier t -> FilePath -> TenM 'Build t Bool
verifyStore st storeDir = do
    env <- ask

    -- Dispatch based on privilege tier
    case fromSing st of
        -- In daemon context, verify directly
        Daemon -> verifyStorePrivileged st storeDir
        -- In builder context, use protocol
        Builder -> requestStoreVerification st

-- | Private implementation for store verification with full access
verifyStorePrivileged :: SPrivilegeTier 'Daemon -> FilePath -> TenM 'Build 'Daemon Bool
verifyStorePrivileged st storeDir = do
    env <- ask

    -- Open database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all valid paths from database
        paths <- tenQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: TenM 'Build 'Daemon [Only Text]

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

-- | Request store verification from builder context via protocol
requestStoreVerification :: SPrivilegeTier 'Builder -> TenM 'Build 'Builder Bool
requestStoreVerification st = do
    env <- ask

    -- Send request to daemon using protocol
    daemonConn <- getDaemonConnection
    result <- sendToDaemon st daemonConn StoreVerifyRequest

    case result of
        Left err ->
            throwError $ DaemonError $ "Failed to verify store: " <> err
        Right (StoreVerifyResponse valid) ->
            return valid
        Right resp ->
            throwError $ DaemonError $ "Unexpected response for store verification: " <> T.pack (show resp)

-- | Repair the store (remove invalid paths) - daemon privilege only
repairStore :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon GCStats
repairStore st = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all valid paths from database
        paths <- tenQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: TenM 'Build 'Daemon [Only Text]

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
                                        tenExecute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?" (Only pathText)

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
            fileStatus <- Posix.getFileStatus path
            return $ fromIntegral $ Posix.fileSize fileStatus
        else return 0

-- | Helper function for database interaction - Path reachability
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

-- | Helper to get a daemon connection
getDaemonConnection :: TenM p t DaemonConnection
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ ConfigError "Not in client mode, cannot access daemon"

-- | Helper to send a request to the daemon with privilege check
sendToDaemon :: SPrivilegeTier 'Builder -> DaemonConnection -> DaemonRequest -> TenM 'Build 'Builder (Either Text DaemonResponse)
sendToDaemon st conn request = do
    -- Create a new request ID
    reqId <- liftIO $ atomically $ do
        curId <- readTVar (connRequestId conn)
        writeTVar (connRequestId conn) (curId + 1)
        return curId

    -- Create a TMVar to receive the response
    responseTMVar <- liftIO $ atomically $ newEmptyTMVar

    -- Register the request in the pending map
    liftIO $ atomically $ modifyTVar' (connPendingRequests conn) $
        Map.insert reqId responseTMVar

    -- Encode and send the request
    let requestData = encodeRequest reqId (connAuthToken conn) request
    liftIO $ do
        BS.hPut (connHandle conn) requestData
        hFlush (connHandle conn)

    -- Wait for response with timeout
    result <- liftIO $ atomically $ do
        response <- takeTMVar responseTMVar
        -- Remove from pending requests
        modifyTVar' (connPendingRequests conn) $ Map.delete reqId
        return response

    case result of
        ErrorResponse err -> return $ Left $ T.pack $ show err
        _ -> return $ Right result

-- | Encode a request to binary
encodeRequest :: Integer -> AuthToken -> DaemonRequest -> BS.ByteString
encodeRequest reqId (AuthToken token) request =
    -- In a real implementation, this would properly serialize the request to binary
    -- For this example, we're just creating a simplified representation
    BS.pack $ show (reqId, token, request)

-- | The Database type for database operations
data Database = Database {
    dbConn :: Connection
}

-- | Connection type for SQLite
data Connection = Connection {
    connHandle :: Handle
}

-- | Execute a database query
dbQuery_ :: Database -> String -> IO a
dbQuery_ db query =
    -- This would be implemented to query the SQLite database
    -- For this implementation, we'll need a complete SQLite integration
    error "Implement dbQuery_ with proper SQLite access"

-- | Execute a database operation
dbExecute :: Database -> String -> a -> IO ()
dbExecute db query params =
    -- This would be implemented to execute a SQLite command
    -- For this implementation, we'll need a complete SQLite integration
    error "Implement dbExecute with proper SQLite access"

-- | Execute a simple database operation
dbExecuteSimple_ :: Database -> String -> IO ()
dbExecuteSimple_ db query =
    -- This would be implemented to execute a simple SQLite command
    -- For this implementation, we'll need a complete SQLite integration
    error "Implement dbExecuteSimple_ with proper SQLite access"

-- | Query from the database within the TenM monad
tenQuery_ :: Database -> String -> TenM p t a
tenQuery_ db query =
    -- This would be implemented to execute a query within the TenM monad
    -- For this implementation, we'll need to integrate with Ten.DB.Core
    error "Implement tenQuery_ with proper TenM database access"

-- | Execute a database operation within the TenM monad
tenExecute :: Database -> String -> a -> TenM p t b
tenExecute db query params =
    -- This would be implemented to execute a command within the TenM monad
    -- For this implementation, we'll need to integrate with Ten.DB.Core
    error "Implement tenExecute with proper TenM database access"

-- | Execute a database operation within the TenM monad (simplified version)
tenExecute_ :: Database -> String -> a -> TenM p t ()
tenExecute_ db query params =
    -- This would be implemented to execute a command within the TenM monad
    -- For this implementation, we'll need to integrate with Ten.DB.Core
    error "Implement tenExecute_ with proper TenM database access"

-- | Access the database with proper daemon privileges
withDatabase :: SPrivilegeTier 'Daemon -> FilePath -> Int -> (Database -> TenM p 'Daemon a) -> TenM p 'Daemon a
withDatabase st dbPath timeout action =
    -- This would be implemented to safely access the database with proper privileges
    -- For this implementation, we'll need to integrate with Ten.DB.Core
    error "Implement withDatabase with proper privilege control"

-- | Hash calculation function
hashByteString :: BS.ByteString -> Text
hashByteString bs =
    -- This would be implemented to calculate a secure hash of binary data
    -- For this implementation, we'll need cryptographic functions
    error "Implement hashByteString with proper cryptographic methods"

-- | Convert hash to string representation
showHash :: Text -> Text
showHash hash =
    -- This would be implemented to format a hash for display
    -- For this implementation, we'll need proper string formatting
    hash

-- | Check if a store path exists
storePathExists :: StorePath -> TenM p t Bool
storePathExists path = do
    env <- ask
    let filePath = storePathToFilePath path env
    liftIO $ doesFileExist filePath

-- | Create a symlink
createFileLink :: FilePath -> FilePath -> IO ()
createFileLink target link =
    Posix.createSymbolicLink target link
