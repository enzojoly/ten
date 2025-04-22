{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}

module Ten.GC
    ( -- Core GC operations (daemon privileges only)
      collectGarbage
    , gcLockPath
    , acquireGCLock
    , releaseGCLock
    , withGCLock
    , breakStaleLock

    -- GC statistics
    , GCStats(..)

    -- GC roots management (daemon privileges only)
    , addRoot
    , removeRoot
    , listRoots
    , isGCRoot
    , findGCRoots

    -- Path reachability analysis (daemon privileges only)
    , findPathReferences
    , computeReachablePaths
    , findPathsWithPrefix

    -- Store verification (daemon privileges only)
    , verifyStore
    , repairStore

    -- Protocol operations (builder context)
    , requestGarbageCollection
    , requestAddRoot
    , requestRemoveRoot
    , requestListRoots
    , requestPathReachability

    -- Active build management
    , getActiveBuildPaths

    -- Lock management
    , GCLock(..)
    , GCLockInfo(..)
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
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing,
                         listDirectory, removeFile, getPermissions, removePathForcibly)
import qualified System.Directory as Dir
import System.FilePath
import System.IO.Error (isDoesNotExistError, catchIOError, isPermissionError)
import qualified System.Posix.Files as Posix
import System.IO (Handle, hPutStrLn, stderr, hFlush)
import System.Posix.IO (openFd, createFile, closeFd, setLock, getLock,
                        defaultFileFlags, OpenMode(..), OpenFileFlags(..),
                        exclusive, fdToHandle, LockRequest(WriteLock, Unlock))
import System.Posix.Types (Fd, ProcessID)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess)
import qualified System.Posix.IO as PosixIO
import qualified System.Posix.Files.ByteString as ByteString
import System.IO (SeekMode(AbsoluteSeek))
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Database.SQLite.Simple (Connection, Query(..), Only(..), query, query_, execute, execute_)
import qualified Database.SQLite.Simple as SQLite
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Singletons
import Data.Singletons.TH
import Data.Maybe (fromMaybe, isJust, listToMaybe, catMaybes)
import Network.Socket (Socket)
import qualified Network.Socket as Network
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Control.Concurrent.MVar

import Ten.Core
import Ten.Daemon.Protocol (DaemonConnection, DaemonRequest(..), DaemonResponse(..),
                           sendDaemonRequest, encodeRequest, decodeResponse)

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
    liftIO $ Posix.createSymbolicLink targetPath rootFile

    -- Also register the root in the database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        execute db
            "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
            (storePathToText path, name, if permanent then "permanent" else "user")

    logMsg 1 $ "Added GC root: " <> name <> " -> " <> storeHash path

    return root

-- | Request to add a root from builder context via protocol
requestAddRoot :: SPrivilegeTier 'Builder -> StorePath -> Text -> Bool -> TenM 'Build 'Builder GCRoot
requestAddRoot st path name permanent = do
    env <- ask

    -- Send request to daemon using protocol
    case runMode env of
        ClientMode conn -> do
            -- Create a request message
            let requestMsg = AddGCRootRequest path name permanent

            -- Send the request via the daemon protocol
            response <- sendDaemonRequest conn requestMsg

            case response of
                GCRootResponse root -> return root
                ErrorResponse err -> throwError $ DaemonError $ "Failed to add GC root: " <> T.pack (show err)
                _ -> throwError $ DaemonError $ "Unexpected response for add GC root: " <> T.pack (show response)

        _ -> throwError $ DaemonError "Not connected to daemon"

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
                execute db
                    "UPDATE GCRoots SET active = 0 WHERE path = ? AND name = ?"
                    (storePathToText (rootPath root), rootName root)

            logMsg 1 $ "Removed GC root: " <> rootName root <> " -> " <> storeHash (rootPath root)

-- | Request to remove a root from builder context via protocol
requestRemoveRoot :: SPrivilegeTier 'Builder -> GCRoot -> TenM 'Build 'Builder ()
requestRemoveRoot st root = do
    env <- ask

    -- Send request to daemon using protocol
    case runMode env of
        ClientMode conn -> do
            -- Create a request message
            let requestMsg = RemoveGCRootRequest root

            -- Send the request via the daemon protocol
            response <- sendDaemonRequest conn requestMsg

            case response of
                SuccessResponse -> return ()
                ErrorResponse err -> throwError $ DaemonError $ "Failed to remove root: " <> T.pack (show err)
                _ -> throwError $ DaemonError $ "Unexpected response for remove GC root: " <> T.pack (show response)

        _ -> throwError $ DaemonError "Not connected to daemon"

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
        results <- query_ db "SELECT path, name, type, timestamp FROM GCRoots WHERE active = 1"
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
    case runMode env of
        ClientMode conn -> do
            -- Create a request message
            let requestMsg = ListGCRootsRequest

            -- Send the request via the daemon protocol
            response <- sendDaemonRequest conn requestMsg

            case response of
                GCRootListResponse roots -> return roots
                ErrorResponse err -> throwError $ DaemonError $ "Failed to list GC roots: " <> T.pack (show err)
                _ -> throwError $ DaemonError $ "Unexpected response for list GC roots: " <> T.pack (show response)

        _ -> throwError $ DaemonError "Not connected to daemon"

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
            -- Check if it's a symlink
            isLink <- Posix.isSymbolicLink <$> Posix.getFileStatus rootFile `catch` \(_ :: IOException) ->
                return False

            if isLink
                then do
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
                else return Nothing
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
    case runMode env of
        ClientMode conn -> do
            -- Create a request message
            let requestMsg = GCRequest force

            -- Send the request via the daemon protocol
            response <- sendDaemonRequest conn requestMsg

            case response of
                GCResultResponse stats -> return stats
                ErrorResponse err -> throwError $ DaemonError $ "Failed to request garbage collection: " <> T.pack (show err)
                _ -> throwError $ DaemonError $ "Unexpected response for GC request: " <> T.pack (show response)

        _ -> throwError $ DaemonError "Not connected to daemon"

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
        logMsg 2 $ "Found " <> T.pack (show (Set.size rootPaths)) <> " GC roots"

        -- Find all paths reachable from roots (transitive closure)
        reachablePaths <- liftIO $ computeReachablePathsFromRoots db rootPaths

        -- Log information about reachable paths
        logMsg 2 $ "Found " <> T.pack (show (Set.size reachablePaths)) <> " reachable paths"

        -- Get active build paths (if in daemon mode)
        activePaths <- getActiveBuildPaths st
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
findAllStorePaths :: Connection -> FilePath -> IO (Set StorePath)
findAllStorePaths db storeLocation = do
    -- Get paths from database (preferred, more reliable)
    -- Query paths from ValidPaths table
    results <- SQLite.query_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

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
            -- List all entries in store directory
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
findAllRoots :: Connection -> FilePath -> IO (Set StorePath)
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
                let srcPath = rootsDir </> file
                isLink <- Posix.isSymbolicLink <$> Posix.getFileStatus srcPath `catch`
                    \(_ :: SomeException) -> return False

                if isLink
                    then do
                        target <- try $ Posix.readSymbolicLink srcPath
                        case target of
                            Left (_ :: SomeException) -> return acc
                            Right targetPath ->
                                case filePathToStorePath targetPath of
                                    Just sp -> return $ Set.insert sp acc
                                    Nothing -> return acc
                    else return acc
                ) Set.empty files

-- | Get root paths from database
getDatabaseRootPaths :: Connection -> IO (Set StorePath)
getDatabaseRootPaths db = do
    -- Query active roots from database
    results <- SQLite.query_ db "SELECT path FROM GCRoots WHERE active = 1" :: IO [Only Text]

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
    case runMode env of
        ClientMode conn -> do
            -- Create a request message
            let requestMsg = PathReachabilityRequest path

            -- Send the request via the daemon protocol
            response <- sendDaemonRequest conn requestMsg

            case response of
                PathReachabilityResponse reachable -> return reachable
                ErrorResponse err -> throwError $ DaemonError $ "Failed to check path reachability: " <> T.pack (show err)
                _ -> throwError $ DaemonError $ "Unexpected response for reachability check: " <> T.pack (show response)

        _ -> throwError $ DaemonError "Not connected to daemon"

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
    let lockPath = gcLockPath env

    -- Ensure the lock directory exists
    liftIO $ ensureLockDirExists (takeDirectory lockPath)

    -- Try to acquire the lock with proper privilege verification
    result <- liftIO $ try $ acquireFileLock lockPath

    case result of
        Left (e :: SomeException) -> throwError $ ResourceError $ "Could not acquire GC lock: " <> T.pack (show e)
        Right lock -> do
            -- Keep the fd in a global reference to avoid GC
            liftIO $ writeIORef globalGCLockFdRef (Just (lockFd lock, lockPath))

-- | Release the GC lock (daemon privilege only)
releaseGCLock :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon ()
releaseGCLock st = do
    env <- ask
    let lockPath = gcLockPath env

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
    result <- action st `catchError` \e -> do
        -- Always release the lock on error
        releaseGCLock st
        -- Re-throw the error
        throwError e

    -- Always release the lock on completion
    releaseGCLock st

    -- Return the result
    return result

-- | Break a stale lock (daemon privilege only)
breakStaleLock :: SPrivilegeTier 'Daemon -> FilePath -> TenM 'Build 'Daemon ()
breakStaleLock st lockPath = do
    -- Check lock status
    status <- liftIO $ try $ checkLockFile lockPath

    case status of
        Left (e :: SomeException) ->
            logMsg 1 $ "Cannot check lock status: " <> T.pack (show e)
        Right isValid ->
            unless isValid $ liftIO $ do
                -- Log that we're breaking a stale lock
                hPutStrLn stderr $ "Breaking stale GC lock: " ++ lockPath

                -- Remove the lock file
                removeFile lockPath `catch` \(_ :: SomeException) ->
                    hPutStrLn stderr $ "Warning: Failed to remove stale lock file: " ++ lockPath

-- | Check if a lock file exists and is valid
checkLockFile :: FilePath -> IO Bool
checkLockFile lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath

    if not exists
        then return False  -- Lock file doesn't exist
        else do
            -- Read the lock file to get PID
            result <- try $ readFile lockPath
            case result of
                Left (e :: SomeException) ->
                    throwIO $ userError $ "Error reading lock file: " ++ show e

                Right content -> do
                    -- Parse the PID
                    case reads content of
                        [(pid, "")] -> do
                            -- Check if the process is still running
                            isProcessRunning pid
                        _ -> throwIO $ userError "Invalid lock file format"

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
acquireFileLock :: FilePath -> IO GCLock
acquireFileLock lockPath = do
    -- Ensure parent directory exists
    ensureLockDirExists (takeDirectory lockPath)

    -- Get our process ID
    pid <- getProcessID

    -- Check if lock already exists
    result <- try $ checkLockFile lockPath
    active <- case result of
        Left _ -> return False
        Right isActive -> return isActive

    when active $
        throwIO $ userError "Another garbage collection is in progress"

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
            throwIO $ userError $ "Failed to create lock file: " ++ show e
        Right lock ->
            return lock

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
getActiveBuildPaths :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon (Set StorePath)
getActiveBuildPaths st = do
    env <- ask

    -- Get runtime paths from database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Query active build paths from database
        results <- query_ db
            "SELECT path FROM ActiveBuilds WHERE status != 'completed'" :: TenM 'Build 'Daemon [Only Text]

        -- Parse paths
        return $ Set.fromList $ catMaybes $ map (\(Only t) -> parseStorePath t) results

-- | Verify the store integrity
verifyStore :: FilePath -> TenM 'Build 'Daemon Bool
verifyStore storeDir = do
    env <- ask

    -- Open database with proper privilege
    withDatabase sDaemon (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all valid paths from database
        paths <- query_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: TenM 'Build 'Daemon [Only Text]

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

-- | Repair the store (remove invalid paths) - daemon privilege only
repairStore :: SPrivilegeTier 'Daemon -> TenM 'Build 'Daemon GCStats
repairStore st = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database with proper privilege
    withDatabase st (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all valid paths from database
        paths <- query_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: TenM 'Build 'Daemon [Only Text]

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
                                        fileStatus <- Posix.getFileStatus path
                                        let pathSize = fromIntegral $ Posix.fileSize fileStatus

                                        -- Mark as invalid in database
                                        execute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?" (Only pathText)

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

-- | Helper function for database interaction - Path reachability
isPathReachable :: Connection -> Set StorePath -> StorePath -> IO Bool
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
computeReachablePathsFromRoots :: Connection -> Set StorePath -> IO (Set StorePath)
computeReachablePathsFromRoots db roots = do
    -- Include the roots themselves
    findPathsClosure db roots

-- | Find paths reachability closure (using properly optimized DB query)
findPathsClosure :: Connection -> Set StorePath -> IO (Set StorePath)
findPathsClosure db startingPaths = do
    -- Create a temporary table for the closure calculation
    execute_ db "CREATE TEMP TABLE IF NOT EXISTS temp_closure (path TEXT PRIMARY KEY)"
    execute_ db "DELETE FROM temp_closure"

    -- Insert all starting paths
    forM_ (Set.toList startingPaths) $ \path ->
        execute db "INSERT OR IGNORE INTO temp_closure VALUES (?)" (Only (storePathToText path))

    -- Use recursive CTE to compute the transitive closure efficiently
    let closureQuery = "WITH RECURSIVE closure(path) AS (\
                      \  SELECT path FROM temp_closure\
                      \  UNION\
                      \  SELECT r.reference FROM References r JOIN closure c ON r.referrer = c.path\
                      \) SELECT DISTINCT path FROM closure"

    results <- query_ db closureQuery :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only p) -> parseStorePath p) results

-- | Helper function to mark paths as invalid in the database
markPathsAsInvalid :: Connection -> Set StorePath -> IO ()
markPathsAsInvalid db paths = do
    -- Mark each path as invalid in the ValidPaths table
    forM_ (Set.toList paths) $ \path ->
        execute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
               (Only (storePathToText path))

-- | Scan a file for references to store paths
scanFileForStoreReferences :: FilePath -> TenM p t (Set StorePath)
scanFileForStoreReferences filePath = do
    -- Check if file exists
    exists <- liftIO $ doesFileExist filePath
    unless exists $
        return Set.empty

    -- Get store location
    env <- ask
    let storeDir = storeLocation env

    -- Read file content
    contentResult <- liftIO $ try $ BS.readFile filePath
    content <- case contentResult of
        Right c -> return c
        Left (_ :: IOException) -> return BS.empty

    -- Extract store paths from binary content
    let contentText = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') content
    let paths = findStorePaths contentText storeDir

    -- Filter for valid paths
    validPaths <- filterM storePathExists paths

    -- Return the set of found paths
    return $ Set.fromList validPaths

-- | Find all references in a path
-- This is a daemon-only operation
findPathReferences :: SPrivilegeTier 'Daemon -> StorePath -> TenM 'Build 'Daemon (Set StorePath)
findPathReferences _ path = do
    env <- ask

    -- First try to get references from database (faster)
    dbRefs <- getReferencesFromDB path

    if not (Set.null dbRefs)
        then return dbRefs
        else do
            -- Fall back to file scanning if database has no entries
            let filePath = storePathToFilePath path env

            -- Check file existence
            exists <- liftIO $ doesFileExist filePath
            if not exists
                then return Set.empty
                else do
                    -- Read file content
                    content <- liftIO $ BS.readFile filePath

                    -- Scan for references
                    refs <- scanForReferences content

                    -- Register references in database for future use
                    forM_ (Set.toList refs) $ \refPath ->
                        registerReference path refPath

                    return refs

-- | Get references from database
getReferencesFromDB :: StorePath -> TenM p 'Daemon (Set StorePath)
getReferencesFromDB path = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- Open database connection
    conn <- liftIO $ SQLite.open dbPath

    -- Query for references
    rows <- liftIO $ SQLite.query conn
        "SELECT reference FROM References WHERE referrer = ?"
        (Only (storePathToText path)) :: TenM p 'Daemon [Only Text]

    -- Close connection
    liftIO $ SQLite.close conn

    -- Convert to StorePath objects
    let paths = catMaybes $ map (\(Only t) -> parseStorePath t) rows
    return $ Set.fromList paths

-- | Register a reference between paths
registerReference :: StorePath -> StorePath -> TenM p 'Daemon ()
registerReference referrer reference = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- Open database connection
    conn <- liftIO $ SQLite.open dbPath

    -- Insert reference (ignore if already exists)
    liftIO $ SQLite.execute conn
        "INSERT OR IGNORE INTO References (referrer, reference, type) VALUES (?, ?, 'direct')"
        (storePathToText referrer, storePathToText reference)

    -- Close connection
    liftIO $ SQLite.close conn

-- | Scan content for references to other store paths
-- This is a daemon-only operation
scanForReferences :: BS.ByteString -> TenM p 'Daemon (Set StorePath)
scanForReferences content = do
    env <- ask
    let storeDir = storeLocation env

    -- Convert to text for easier scanning
    let contentText = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') content

    -- Find potential store paths
    let paths = findStorePaths contentText storeDir

    -- Verify each path exists
    foldM (\acc path -> do
        exists <- storePathExists path
        if exists
            then return $ Set.insert path acc
            else return acc
        ) Set.empty paths

-- | Find potential store paths in text
findStorePaths :: Text -> FilePath -> [StorePath]
findStorePaths content storeDir =
    let storeDirText = T.pack storeDir
        -- Split into lines and words
        allWords = concatMap T.words $ T.lines content
        -- Filter for words that could be store paths
        potentialPaths = filter (isStorePath storeDirText) allWords
        -- Extract and parse store paths
        parsedPaths = catMaybes $ map extractStorePath potentialPaths
    in parsedPaths
  where
    isStorePath :: Text -> Text -> Bool
    isStorePath storeDir text =
        -- Check if it's a full store path
        (storeDir `T.isPrefixOf` text && "-" `T.isInfixOf` T.drop (T.length storeDir) text) ||
        -- Or just a store path without the store dir
        (T.length text >= 10 && "-" `T.isInfixOf` text && allHex (T.takeWhile (/= '-') text))
      where
        allHex = T.all isHexDigit

    extractStorePath :: Text -> Maybe StorePath
    extractStorePath text
        | "/" `T.isPrefixOf` text =
            -- Handle full paths starting with /
            let fileName = T.pack $ takeFileName $ T.unpack text
            in parseStorePath fileName
        | otherwise =
            -- Handle just hash-name format
            parseStorePath text

-- | Check if a path is a GC root
-- This is a daemon-only operation
isGCRoot :: SPrivilegeTier 'Daemon -> StorePath -> TenM p 'Daemon Bool
isGCRoot _ path = do
    env <- ask
    let storeDir = storeLocation env
    let rootsDir = storeDir </> "gc-roots"

    -- Check database first
    isDbRoot <- isGCRootInDB path

    if isDbRoot
        then return True
        else do
            -- If not in database, check filesystem
            -- List all roots
            roots <- liftIO $ listDirectory rootsDir `catch` \(_ :: IOException) -> return []

            -- Check if any root links to this path
            isFileRoot <- liftIO $ foldM (\found root -> do
                if found
                    then return True
                    else do
                        -- Check if this root points to our path
                        let rootPath = rootsDir </> root
                        isLink <- liftIO $ Posix.isSymbolicLink <$> Posix.getFileStatus rootPath `catch`
                            \(_ :: SomeException) -> return False
                        if isLink
                            then do
                                target <- liftIO $ Posix.readSymbolicLink rootPath `catch` \(_ :: IOException) -> return ""
                                let targetPath = storePathToFilePath path env
                                return $ targetPath == target
                            else return False
                ) False roots

            -- If found in filesystem but not in DB, register it
            when (isFileRoot && not isDbRoot) $
                registerGCRoot path (T.pack "filesystem-found") "symlink"

            return isFileRoot

-- | Check if a path is a GC root in the database
isGCRootInDB :: StorePath -> TenM p 'Daemon Bool
isGCRootInDB path = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- Open database connection
    conn <- liftIO $ SQLite.open dbPath

    -- Query for roots
    rows <- liftIO $ SQLite.query conn
        "SELECT COUNT(*) FROM GCRoots WHERE path = ? AND active = 1"
        (Only (storePathToText path)) :: TenM p 'Daemon [Only Int]

    -- Close connection
    liftIO $ SQLite.close conn

    -- Check if count > 0
    case rows of
        [Only count] -> return (count > 0)
        _ -> return False

-- | Register a GC root in the database
registerGCRoot :: StorePath -> Text -> Text -> TenM p 'Daemon ()
registerGCRoot path name rootType = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- Open database connection
    conn <- liftIO $ SQLite.open dbPath

    -- Insert or update root
    liftIO $ SQLite.execute conn
        "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
        (storePathToText path, name, rootType)

    -- Close connection
    liftIO $ SQLite.close conn

-- | Access the database with proper daemon privileges
withDatabase :: SPrivilegeTier 'Daemon -> FilePath -> Int -> (Connection -> TenM p 'Daemon a) -> TenM p 'Daemon a
withDatabase st dbPath timeout action = do
    -- Initialize database and ensure schema exists
    connection <- liftIO $ initializeDatabase dbPath

    -- Set busy timeout
    liftIO $ SQLite.execute_ connection $ "PRAGMA busy_timeout = " <> Query (T.pack (show timeout))

    -- Enable foreign keys
    liftIO $ SQLite.execute_ connection "PRAGMA foreign_keys = ON"

    -- Run action with proper error handling
    result <- action connection `catchError` \e -> do
        liftIO $ SQLite.close connection
        throwError e

    -- Close the connection
    liftIO $ SQLite.close connection

    -- Return the result
    return result

-- | Initialize database with schema
initializeDatabase :: FilePath -> IO Connection
initializeDatabase dbPath = do
    -- Ensure directory exists
    createDirectoryIfMissing True (takeDirectory dbPath)

    -- Connect to database
    conn <- SQLite.open dbPath

    -- Set up schema if needed
    ensureSchema conn

    return conn

-- | Ensure database schema is set up
ensureSchema :: Connection -> IO ()
ensureSchema conn = do
    -- Check if tables exist
    tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' AND name='ValidPaths'" :: IO [Only Text]

    when (null tables) $ do
        -- Create tables
        execute_ conn "CREATE TABLE IF NOT EXISTS ValidPaths (\
                     \  path TEXT PRIMARY KEY,\
                     \  is_valid INTEGER NOT NULL DEFAULT 1,\
                     \  last_accessed INTEGER,\
                     \  registration_time INTEGER NOT NULL DEFAULT (strftime('%s','now')),\
                     \  deriver TEXT\
                     \)"

        execute_ conn "CREATE TABLE IF NOT EXISTS References (\
                     \  referrer TEXT NOT NULL,\
                     \  reference TEXT NOT NULL,\
                     \  ref_type TEXT NOT NULL,\
                     \  PRIMARY KEY (referrer, reference),\
                     \  FOREIGN KEY (referrer) REFERENCES ValidPaths(path) ON DELETE CASCADE,\
                     \  FOREIGN KEY (reference) REFERENCES ValidPaths(path) ON DELETE CASCADE\
                     \)"

        execute_ conn "CREATE TABLE IF NOT EXISTS GCRoots (\
                     \  path TEXT NOT NULL,\
                     \  name TEXT NOT NULL,\
                     \  type TEXT NOT NULL,\
                     \  timestamp INTEGER NOT NULL,\
                     \  active INTEGER NOT NULL DEFAULT 1,\
                     \  PRIMARY KEY (path, name)\
                     \)"

        execute_ conn "CREATE TABLE IF NOT EXISTS ActiveBuilds (\
                     \  build_id TEXT PRIMARY KEY,\
                     \  path TEXT NOT NULL,\
                     \  start_time INTEGER NOT NULL,\
                     \  status TEXT NOT NULL,\
                     \  progress REAL\
                     \)"

        -- Create indexes
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References (referrer)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_references_reference ON References (reference)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_gcroots_path ON GCRoots (path)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_gcroots_active ON GCRoots (active)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_activebuilds_status ON ActiveBuilds (status)"

-- | Find paths with prefix in the store
findPathsWithPrefix :: SPrivilegeTier 'Daemon -> Text -> TenM 'Build 'Daemon [StorePath]
findPathsWithPrefix st prefix = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- Open database connection
    conn <- liftIO $ SQLite.open dbPath

    -- Query for paths with prefix
    rows <- liftIO $ SQLite.query conn
        "SELECT path FROM ValidPaths WHERE path LIKE ? || '%' AND is_valid = 1"
        (Only prefix) :: TenM 'Build 'Daemon [Only Text]

    -- Close connection
    liftIO $ SQLite.close conn

    -- Convert to StorePath objects
    return $ catMaybes $ map (\(Only t) -> parseStorePath t) rows

-- | Calculate hash of ByteString content
hashByteString :: BS.ByteString -> Digest SHA256
hashByteString = Crypto.hash

-- | Convert hash to string representation
showHash :: Digest SHA256 -> Text
showHash = T.pack . show
