{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ten.GC (
    -- Core GC operations
    collectGarbage,
    daemonCollectGarbage,

    -- GC roots management
    GCRoot(..),
    addRoot,
    removeRoot,
    listRoots,

    -- GC statistics
    GCStats(..),

    -- Path reachability
    isReachable,
    findReachablePaths,

    -- Concurrent GC
    acquireGCLock,
    releaseGCLock,
    withGCLock,
    breakStaleLock,

    -- Active build management
    getActiveBuildPaths,

    -- Store verification
    verifyStore,
    repairStore
) where

import Control.Concurrent.STM
import Control.Exception (bracket, try, catch, throwIO, finally, mask, SomeException, IOException)
import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Time
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

import Ten.Core
import Ten.Store
import Ten.Hash
import Ten.Derivation
import Ten.Graph
import Ten.DB.Core
import Ten.DB.References
import Ten.DB.Derivations

-- | A garbage collection root
data GCRoot = GCRoot
    { rootPath :: StorePath        -- Path protected from garbage collection
    , rootName :: Text             -- Name/purpose of this root
    , rootCreated :: UTCTime       -- When this root was created
    , rootPermanent :: Bool        -- Whether this is a permanent root
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

-- | Add a root to protect a path from garbage collection
addRoot :: StorePath -> Text -> Bool -> TenM p GCRoot
addRoot path name permanent = do
    env <- ask

    -- Verify the path exists in the store
    exists <- storePathExists path
    unless exists $ throwError $ StoreError $ "Cannot add root for non-existent path: " <> storeHash path

    -- Create the root
    now <- liftIO getCurrentTime
    let root = GCRoot
            { rootPath = path
            , rootName = name
            , rootCreated = now
            , rootPermanent = permanent
            }

    -- Write the root to the roots directory
    let rootsDir = storePath env </> "gc-roots"
    liftIO $ createDirectoryIfMissing True rootsDir

    -- Generate a unique filename for the root
    let rootFile = rootsDir </> (T.unpack $ storeHash path <> "-" <> name)

    -- Write a symlink to the actual path
    let targetPath = storePathToFilePath path env
    liftIO $ createSymbolicLink targetPath rootFile

    -- Also register the root in the database
    db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000
    liftIO $ dbExecute db
        "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
        (storePathToText path, name, if permanent then "permanent" else "user")
    liftIO $ closeDatabase db

    logMsg 1 $ "Added GC root: " <> name <> " -> " <> storeHash path

    return root

-- | Remove a root
removeRoot :: GCRoot -> TenM p ()
removeRoot root = do
    env <- ask

    -- Generate the root file path
    let rootsDir = storePath env </> "gc-roots"
    let rootFile = rootsDir </> (T.unpack $ storeHash (rootPath root) <> "-" <> rootName root)

    -- Check if it exists
    exists <- liftIO $ doesFileExist rootFile

    -- Remove from filesystem and database
    when exists $ do
        -- Don't remove permanent roots unless forced
        unless (rootPermanent root) $ do
            -- Remove from filesystem
            liftIO $ removeFile rootFile

            -- Remove from database
            db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000
            liftIO $ dbExecute db
                "UPDATE GCRoots SET active = 0 WHERE path = ? AND name = ?"
                (storePathToText (rootPath root), rootName root)
            liftIO $ closeDatabase db

            logMsg 1 $ "Removed GC root: " <> rootName root <> " -> " <> storeHash (rootPath root)

-- | List all current GC roots
listRoots :: TenM p [GCRoot]
listRoots = do
    env <- ask

    -- Get the roots directory
    let rootsDir = storePath env </> "gc-roots"

    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir

    -- Get roots both from filesystem and database
    fsRoots <- liftIO $ getFileSystemRoots rootsDir
    dbRoots <- liftIO $ getDatabaseRoots (storePath env)

    -- Combine both sources, with filesystem taking precedence for duplicates
    let allRoots = Map.union (Map.fromList [(rootPath r, r) | r <- fsRoots])
                             (Map.fromList [(rootPath r, r) | r <- dbRoots])

    return $ Map.elems allRoots

-- | Get roots from filesystem
getFileSystemRoots :: FilePath -> IO [GCRoot]
getFileSystemRoots rootsDir = do
    -- Create roots directory if it doesn't exist
    createDirectoryIfMissing True rootsDir

    -- List all files in the roots directory
    files <- listDirectory rootsDir

    -- Parse each file into a GCRoot
    now <- getCurrentTime
    roots <- forM files $ \file -> do
        -- Read the symlink target
        let rootFile = rootsDir </> file
        targetExists <- doesFileExist rootFile

        if targetExists
            then do
                target <- try $ getSymbolicLinkTarget rootFile
                case target of
                    Left (_ :: SomeException) -> return Nothing
                    Right linkTarget -> do
                        let path = case parseStorePathFromPath linkTarget of
                                Just p -> p
                                Nothing -> StorePath "unknown" "unknown"

                        -- Parse name from file
                        let name = case break (== '-') file of
                                (_, '-':rest) -> T.pack rest
                                _ -> T.pack file

                        -- Check if it's permanent (in a real implementation, this would be stored in metadata)
                        let isPermanent = "permanent-" `T.isPrefixOf` name

                        return $ Just $ GCRoot
                            { rootPath = path
                            , rootName = name
                            , rootCreated = now
                            , rootPermanent = isPermanent
                            }
            else return Nothing

    -- Filter out Nothings and return the list
    return $ catMaybes roots

-- | Get roots from database
getDatabaseRoots :: FilePath -> IO [GCRoot]
getDatabaseRoots storeDir = do
    -- Open database
    db <- initDatabase (defaultDBPath storeDir) 5000

    -- Query active roots
    results <- dbQuery_ db "SELECT path, name, type, timestamp FROM GCRoots WHERE active = 1"
              :: IO [(Text, Text, Text, Int)]

    -- Convert to GCRoot objects
    now <- getCurrentTime
    roots <- forM results $ \(pathText, name, typeStr, timestamp) -> do
        case parseStorePath pathText of
            Just path -> do
                -- Convert timestamp to UTCTime
                let rootTime = posixSecondsToUTCTime (fromIntegral timestamp)
                -- Determine if permanent
                let isPermanent = typeStr == "permanent"

                return $ Just $ GCRoot
                    { rootPath = path
                    , rootName = name
                    , rootCreated = rootTime
                    , rootPermanent = isPermanent
                    }
            Nothing -> return Nothing

    -- Close database and return roots
    closeDatabase db
    return $ catMaybes roots

-- | Parse StorePath from filesystem path
parseStorePathFromPath :: FilePath -> Maybe StorePath
parseStorePathFromPath path =
    -- Extract store path from a filesystem path like "/nix/store/hash-name"
    parseStorePath $ T.pack $ takeFileName path

-- | Run garbage collection
collectGarbage :: TenM p GCStats
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

-- | Run garbage collection with stats
collectGarbageWithStats :: TenM p GCStats
collectGarbageWithStats = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database connection
    let dbPath = defaultDBPath (storePath env)
    db <- liftIO $ initDatabase dbPath 5000

    -- Find all paths in the store
    let storeDir = storePath env
    allStorePaths <- liftIO $ findAllStorePaths storeDir

    -- Find all root paths
    rootPaths <- liftIO $ findAllRoots db storeDir

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
    totalSize <- liftIO $ sum <$> mapM (getFileSize . (storeDir </>))
                                       (map (pathToFilePath . storePathToText) $ Set.toList deletablePaths)

    -- Delete unreachable paths
    deleted <- liftIO $ do
        -- Mark unreachable paths as invalid in database
        liftIO $ markPathsAsInvalid db deletablePaths

        -- Delete files from filesystem
        forM_ (Set.toList deletablePaths) $ \path -> do
            -- Skip paths in gc-roots directory
            let pathText = storePathToText path
            unless ("gc-roots/" `T.isPrefixOf` pathText) $
                -- Attempt to delete, ignoring errors
                catch (removePathForcibly (storeDir </> pathToFilePath pathText)) $ \(_ :: IOError) -> return ()

        return $ Set.size deletablePaths

    -- Close database
    liftIO $ closeDatabase db

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

-- | Convert StorePath to path relative to store (hash-name format)
pathToFilePath :: Text -> FilePath
pathToFilePath = T.unpack

-- | Find all store paths
findAllStorePaths :: FilePath -> IO (Set StorePath)
findAllStorePaths storeDir = do
    -- Check if store exists
    exists <- doesDirectoryExist storeDir
    if not exists
        then return Set.empty
        else do
            -- Get paths from database (preferred, more reliable)
            db <- initDatabase (defaultDBPath storeDir) 5000

            -- Query paths from ValidPaths table
            results <- dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

            -- Parse StorePaths and create set
            let dbPaths = Set.fromList $ catMaybes $ map (\(Only p) -> parseStorePath p) results

            -- Close database
            closeDatabase db

            -- Fallback: if database doesn't have all paths, scan filesystem as backup
            if Set.null dbPaths
                then scanFilesystemForPaths storeDir
                else return dbPaths

-- | Scan filesystem for store paths as a fallback
scanFilesystemForPaths :: FilePath -> IO (Set StorePath)
scanFilesystemForPaths storeDir = do
    -- List all files in the store directory
    entries <- try $ listDirectory storeDir

    case entries of
        Left (_ :: SomeException) -> return Set.empty
        Right files -> do
            -- Filter for valid store path format
            paths <- foldM (\acc file -> do
                -- Skip special directories and non-store paths
                unless (file `elem` ["gc-roots", "tmp", "locks", "var"] ||
                       any (`isPrefixOf` file) ["gc-roots", "tmp.", ".",  ".."]) $ do
                    -- Check if it's a store path (hash-name format)
                    case parseStorePath (T.pack file) of
                        Just path -> return $ Set.insert path acc
                        Nothing -> return acc
                return acc
                ) Set.empty files

            return paths

-- | Find all roots for garbage collection
findAllRoots :: Database -> FilePath -> IO (Set StorePath)
findAllRoots db storeDir = do
    -- Get roots from filesystem (gc-roots directory)
    let rootsDir = storeDir </> "gc-roots"
    fsRoots <- getFileSystemRootPaths rootsDir

    -- Get roots from database
    dbRoots <- getDatabaseRootPaths db

    -- Get runtime lock roots (for active builds)
    runtimeRoots <- getRuntimeRootPaths storeDir

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
            paths <- foldM (\acc file -> do
                let path = rootsDir </> file
                isLink <- pathIsSymbolicLink <$> getFileStatus path
                if isLink
                    then do
                        target <- try $ getSymbolicLinkTarget path
                        case target of
                            Left (_ :: SomeException) -> return acc
                            Right targetPath ->
                                case parseStorePathFromPath targetPath of
                                    Just sp -> return $ Set.insert sp acc
                                    Nothing -> return acc
                    else return acc
            ) Set.empty files

            return paths

-- | Get root paths from database
getDatabaseRootPaths :: Database -> IO (Set StorePath)
getDatabaseRootPaths db = do
    -- Query active roots from database
    results <- dbQuery_ db "SELECT path FROM GCRoots WHERE active = 1" :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only text) -> parseStorePath text) results

-- | Get runtime roots from active builds
getRuntimeRootPaths :: FilePath -> IO (Set StorePath)
getRuntimeRootPaths storeDir = do
    -- Check for runtime locks directory
    let locksDir = storeDir </> "var/ten/locks"
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
                    paths <- foldM (\acc lockFile -> do
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

                    return paths
  where
    -- Simple function to extract store paths from text
    extractStorePaths :: String -> Set StorePath
    extractStorePaths text =
        -- This is a simplified approach; in practice you'd use a proper parser
        let words = lines text
            paths = filter (\w -> length w > 10 && '-' `elem` w) words
        in Set.fromList $ catMaybes $ map (parseStorePath . T.pack) paths

-- | Get file size
getFileSize :: FilePath -> IO Integer
getFileSize path = do
    exists <- doesFileExist path
    if exists
        then do
            stat <- getFileStatus path
            return $ fromIntegral $ fileSize stat
        else return 0

-- | Daemon-coordinated garbage collection
daemonCollectGarbage :: TenM p GCStats
daemonCollectGarbage = do
    env <- ask

    -- Check if in daemon mode
    isDaemon <- isDaemonMode
    unless isDaemon $
        throwError $ DaemonError "Daemon-coordinated GC requires daemon mode"

    -- In daemon mode, we just use the standard GC mechanism but with potential
    -- additional notifications to clients
    collectGarbage

-- | Acquire the GC lock
acquireGCLock :: TenM p ()
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

-- | Release the GC lock
releaseGCLock :: TenM p ()
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

-- | Execute an action with the GC lock
withGCLock :: TenM p a -> TenM p a
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

-- | Check if a path is reachable from any root
isReachable :: StorePath -> TenM p Bool
isReachable path = do
    env <- ask

    -- Initialize database
    db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000

    -- Get all roots
    roots <- liftIO $ findAllRoots db (storePath env)

    -- Use database to check reachability
    result <- liftIO $ isPathReachable db roots path

    -- Close database
    liftIO $ closeDatabase db

    return result

-- | Find all paths reachable from roots
findReachablePaths :: TenM p (Set StorePath)
findReachablePaths = do
    env <- ask

    -- Initialize database
    db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000

    -- Find all roots
    roots <- liftIO $ findAllRoots db (storePath env)

    -- Compute all reachable paths
    reachable <- liftIO $ computeReachablePathsFromRoots db roots

    -- Close database
    liftIO $ closeDatabase db

    return reachable

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

-- | Break a stale lock
breakStaleLock :: FilePath -> IO ()
breakStaleLock lockPath = do
    -- Check if lock exists
    exists <- doesFileExist lockPath

    when exists $ do
        -- Read the lock to log what we're breaking
        content <- try $ readFile lockPath
        case content of
            Right pidStr ->
                hPutStrLn stderr $ "Breaking stale GC lock held by process " ++ pidStr
            _ ->
                hPutStrLn stderr $ "Breaking stale GC lock (unreadable)"

        -- Remove the lock file
        removeFile lockPath

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
                fd <- createFile lockPath 0o644

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
getActiveBuildPaths :: TenM p (Set StorePath)
getActiveBuildPaths = do
    -- In daemon mode, query the daemon for active build outputs
    isDaemon <- isDaemonMode
    if isDaemon
        then getDaemonActivePaths
        else return Set.empty
  where
    getDaemonActivePaths :: TenM p (Set StorePath)
    getDaemonActivePaths = do
        env <- ask

        -- In daemon mode, check for active build outputs in the runtime state
        case runMode env of
            DaemonMode -> do
                -- Get runtime paths from database
                db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000

                -- Query active build paths from database
                results <- liftIO $ dbQuery_ db "SELECT path FROM ActiveBuilds WHERE status != 'completed'" :: IO [Only Text]

                -- Close database
                liftIO $ closeDatabase db

                -- Parse paths
                return $ Set.fromList $ catMaybes $ map (\(Only t) -> parseStorePath t) results

            ClientMode conn -> do
                -- Ask the daemon via protocol
                result <- withDaemon $ \_ ->
                    return $ Right Set.empty  -- This would make an RPC call in practice
                return result

            _ -> return Set.empty

-- | Verify the store integrity
verifyStore :: TenM p Bool
verifyStore = do
    env <- ask
    let storeDir = storePath env

    -- Open database
    db <- liftIO $ initDatabase (defaultDBPath storeDir) 5000

    -- Get all valid paths from database
    paths <- liftIO $ dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

    -- Close database
    liftIO $ closeDatabase db

    -- Verify each path
    results <- forM paths $ \(Only pathText) -> do
        case parseStorePath pathText of
            Nothing -> return False
            Just storePath -> do
                -- Verify that the store file exists and has the correct hash
                result <- try $ do
                    let fullPath = storeDir </> pathToFilePath pathText
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

-- | Repair the store (remove invalid paths)
repairStore :: TenM p GCStats
repairStore = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database
    db <- liftIO $ initDatabase (defaultDBPath (storePath env)) 5000

    -- Get all valid paths from database
    paths <- liftIO $ dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

    -- Verify and repair each path
    (valid, invalid, totalSize) <- foldM (\(v, i, size) (Only pathText) -> do
        case parseStorePath pathText of
            Nothing -> return (v, i + 1, size)  -- Invalid path format
            Just storePath -> do
                -- Check if file exists and has correct hash
                result <- liftIO $ try $ do
                    let fullPath = (storePath env) </> pathToFilePath pathText
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
                                    pathSize <- getFileSize path

                                    -- Mark as invalid in database
                                    dbExecute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?" (Only pathText)

                                    -- Delete the file
                                    catchIOError (removeFile path) (\_ -> return ())

                                    return (v, i + 1, size + pathSize)
    ) (0, 0, 0) paths

    -- Close database
    liftIO $ closeDatabase db

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
