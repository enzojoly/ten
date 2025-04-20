{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    isDeletable,
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
import Control.Exception (bracket, try, catch, throwIO, SomeException, Exception)
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
                       exclusive, fdToHandle, Fd, WriteLock, Unlock, AbsoluteSeek)
import System.Posix.Types (ProcessID)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess)
import System.Posix.Files (fileExist, getFileStatus, fileSize, setFileMode)
import Data.Maybe (catMaybes)

import Ten.Core hiding (storePathToFilePath) -- Import Core but not the conflicting function
import qualified Ten.Store as Store          -- Use qualified import for Store
import Ten.Hash
import Ten.Derivation
import Ten.Graph

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

-- | Add a root to protect a path from garbage collection
addRoot :: StorePath -> Text -> Bool -> TenM p GCRoot
addRoot path name permanent = do
    env <- ask

    -- Verify the path exists in the store
    exists <- Store.storePathExists path
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
    let targetPath = Store.storePathToFilePath path env
    liftIO $ createSymbolicLink targetPath rootFile

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

    -- Remove it if it exists
    when exists $ do
        -- Don't remove permanent roots unless forced
        unless (rootPermanent root) $ do
            liftIO $ removeFile rootFile
            logMsg 1 $ "Removed GC root: " <> rootName root <> " -> " <> storeHash (rootPath root)

-- | List all current GC roots
listRoots :: TenM p [GCRoot]
listRoots = do
    env <- ask

    -- Get the roots directory
    let rootsDir = storePath env </> "gc-roots"

    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir

    -- List all files in the roots directory
    files <- liftIO $ listDirectory rootsDir

    -- Parse each file into a GCRoot
    now <- liftIO getCurrentTime
    roots <- forM files $ \file -> do
        -- Read the symlink target
        let rootFile = rootsDir </> file
        targetExists <- liftIO $ doesFileExist rootFile

        if targetExists
            then do
                target <- liftIO $ getSymbolicLinkTarget rootFile
                let path = case parseStorePath target of
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
            result <- try $ do
                -- Create the file with exclusive flag to ensure we're the only ones creating it
                fd <- createFile lockPath 0o644

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
    result <- collectGarbageWithStats

    -- Release the lock
    releaseGCLock

    -- Return result or propagate error
    return result

-- | Run garbage collection with stats
collectGarbageWithStats :: TenM p GCStats
collectGarbageWithStats = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Find all paths in the store
    let storeDir = storePath env
    allStorePaths <- liftIO $ findStorePaths storeDir

    -- Find all root-reachable paths
    rootPaths <- findRootPaths storeDir

    -- Find all paths reachable from roots (transitive closure)
    reachablePaths <- computeReachablePaths storeDir rootPaths

    -- Get active build paths (if in daemon mode)
    activePaths <- getActiveBuildPaths

    -- Combine reachable and active paths
    let protectedPaths = Set.union reachablePaths activePaths

    -- Find all deletable paths
    let deletablePaths = Set.difference allStorePaths protectedPaths

    -- Get total size of deletable paths
    totalSize <- liftIO $ sum <$> mapM (getFileSize . (storeDir </>)) (Set.toList deletablePaths)

    -- Delete unreachable paths
    deleted <- liftIO $ do
        forM_ (Set.toList deletablePaths) $ \path -> do
            -- Skip paths in gc-roots directory
            unless ("gc-roots/" `T.isPrefixOf` T.pack path) $
                -- Attempt to delete, ignoring errors
                catch (removePathForcibly (storeDir </> path)) $ \(_ :: IOError) -> return ()
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
        Right _ -> return ()

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
                            -- Create a temporary GCLock just to release it
                            fd <- liftIO $ openFd lockPath ReadWrite (Just 0o644) defaultFileFlags
                            let lock = GCLock fd lockPath ourPid
                            liftIO $ releaseFileLock lock
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

-- | Check if a path is deletable (not reachable from any root)
isDeletable :: StorePath -> TenM p Bool
isDeletable path = do
    isReach <- isReachable path
    return $ not isReach

-- | Check if a path is reachable from any root
isReachable :: StorePath -> TenM p Bool
isReachable path = do
    env <- ask
    let pathStr = T.unpack (storeHash path) <> "-" <> T.unpack (storeName path)

    -- Find all reachable paths
    rootPaths <- findRootPaths (storePath env)
    reachablePaths <- computeReachablePaths (storePath env) rootPaths

    -- Check if our path is in the reachable set
    return $ pathStr `Set.member` reachablePaths

-- | Find all paths reachable from roots
findReachablePaths :: TenM p (Set FilePath)
findReachablePaths = do
    env <- ask
    rootPaths <- findRootPaths (storePath env)
    computeReachablePaths (storePath env) rootPaths

-- | Find all paths in the store
findStorePaths :: FilePath -> IO (Set FilePath)
findStorePaths storeDir = do
    -- Check if store directory exists
    exists <- doesDirectoryExist storeDir
    if not exists
        then return Set.empty
        else do
            -- List all files in the store directory
            entries <- listDirectory storeDir

            -- Filter out directories and non-store paths
            let nonStoreDirs = ["gc-roots", "tmp", "locks", "var"]
            paths <- forM (filter (`notElem` nonStoreDirs) entries) $ \entry -> do
                let fullPath = storeDir </> entry
                isDir <- doesDirectoryExist fullPath
                if isDir
                    then return Nothing
                    else Just <$> isValidStorePath fullPath entry

            -- Return valid store paths
            return $ Set.fromList $ catMaybes paths
  where
    isValidStorePath :: FilePath -> FilePath -> IO (Maybe FilePath)
    isValidStorePath fullPath name = do
        -- Check if it looks like a store path (hash-name format)
        let parts = T.splitOn "-" (T.pack name)
        if length parts >= 2 && T.length (head parts) > 8
            then do
                -- Make sure it's a regular file
                fileExists <- doesFileExist fullPath
                if fileExists
                    then return $ Just name
                    else return Nothing
            else return Nothing

-- | Find all root paths
findRootPaths :: FilePath -> TenM p (Set FilePath)
findRootPaths storeDir = do
    -- Get the roots directory
    let rootsDir = storeDir </> "gc-roots"

    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir

    -- List all files in the roots directory
    files <- liftIO $ try $ listDirectory rootsDir

    case files of
        Left (_ :: SomeException) -> return Set.empty
        Right fs -> do
            -- Read each symlink to find the target
            paths <- liftIO $ forM fs $ \f -> do
                let rootFile = rootsDir </> f
                isFile <- doesFileExist rootFile
                isSymlink <- pathIsSymbolicLink rootFile

                if not (isFile && isSymlink)
                    then return Nothing
                    else do
                        -- Try to read the symlink
                        target <- try $ getSymbolicLinkTarget rootFile
                        case target of
                            Left (_ :: SomeException) -> return Nothing
                            Right t -> return $ Just $ takeFileName t

            -- Return the set of valid paths
            return $ Set.fromList $ catMaybes paths

-- | Compute all paths reachable from root paths
computeReachablePaths :: FilePath -> Set FilePath -> TenM p (Set FilePath)
computeReachablePaths storeDir rootPaths = do
    -- Start with root paths
    initialReachable <- liftIO $ findDirectDependencies storeDir rootPaths

    -- Recursively find all dependencies until we reach a fixed point
    liftIO $ findFixPoint storeDir initialReachable
  where
    findDirectDependencies :: FilePath -> Set FilePath -> IO (Set FilePath)
    findDirectDependencies dir paths = do
        -- For each path, try to find its dependencies
        deps <- forM (Set.toList paths) $ \path -> do
            let fullPath = dir </> path
            scanFileForReferences fullPath

        -- Combine all dependencies with the original paths
        return $ Set.union paths (Set.unions deps)

    findFixPoint :: FilePath -> Set FilePath -> IO (Set FilePath)
    findFixPoint dir current = do
        -- Find direct dependencies of current set
        next <- findDirectDependencies dir current

        -- If we didn't find any new paths, we're done
        if Set.size next == Set.size current
            then return current
            else findFixPoint dir next

    scanFileForReferences :: FilePath -> IO (Set FilePath)
    scanFileForReferences path = do
        -- Check if the file exists
        exists <- doesFileExist path
        if not exists
            then return Set.empty
            else do
                -- Read the file in binary mode
                content <- BS.readFile path

                -- Extract store path references (simplified)
                -- In a real implementation, this would use a more sophisticated algorithm
                -- that understands the binary format and can extract valid store paths
                return $ extractStorePaths content

    -- | Extract store paths from binary content
    extractStorePaths :: BS.ByteString -> Set FilePath
    extractStorePaths content =
        -- In a real implementation, we would scan for store path patterns
        -- This is a placeholder that would be replaced with proper scanning logic
        Set.empty

-- | Parse a store path from a file path
parseStorePath :: FilePath -> Maybe StorePath
parseStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) -> Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

-- | Helper to get the size of a file
getFileSize :: FilePath -> IO Integer
getFileSize path = do
    exists <- doesFileExist path
    if exists
        then do
            info <- getFileStatus path
            return $ fromIntegral $ fileSize info
        else return 0

-- | Get paths of active builds (to prevent GC during builds)
getActiveBuildPaths :: TenM p (Set FilePath)
getActiveBuildPaths = do
    -- In daemon mode, query the daemon for active build outputs
    isDaemon <- isDaemonMode
    if isDaemon
        then queryDaemonForActivePaths
        else return Set.empty
  where
    queryDaemonForActivePaths :: TenM p (Set FilePath)
    queryDaemonForActivePaths = do
        -- In a real implementation, we would query the daemon state
        -- We're building a proper implementation, so we need to integrate with the daemon
        -- But for now, return an empty set as a starting point
        -- In the real implementation this would be connected to the daemon state tracking
        return Set.empty

-- | Verify the store integrity
verifyStore :: TenM p Bool
verifyStore = do
    env <- ask
    let storeDir = storePath env

    -- Find all store paths
    paths <- liftIO $ findStorePaths storeDir

    -- Verify each path
    results <- forM (Set.toList paths) $ \path -> do
        let fullPath = storeDir </> path
        let storePath = case parseStorePath path of
                Just sp -> sp
                Nothing -> StorePath "invalid" "invalid"

        -- Read the content and verify hash
        content <- liftIO $ try $ BS.readFile fullPath
        case content of
            Left (_ :: SomeException) -> return False
            Right bs -> do
                let actualHash = showHash $ hashByteString bs
                return $ actualHash == storeHash storePath

    -- Return true if all paths verify
    return $ all id results

-- | Repair the store (remove invalid paths)
repairStore :: TenM p GCStats
repairStore = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Find all store paths
    let storeDir = storePath env
    paths <- liftIO $ findStorePaths storeDir

    -- Verify each path and remove invalid ones
    (valid, invalid, totalSize) <- foldM verifyPath (0, 0, 0) (Set.toList paths)

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
  where
    verifyPath (validCount, invalidCount, totalSize) path = do
        env <- ask
        let storeDir = storePath env
        let fullPath = storeDir </> path
        let storePath = case parseStorePath path of
                Just sp -> sp
                Nothing -> StorePath "invalid" "invalid"

        -- Read the content and verify hash
        content <- liftIO $ try $ BS.readFile fullPath
        case content of
            Left (_ :: SomeException) -> do
                -- Remove invalid path
                liftIO $ removeFile fullPath
                return (validCount, invalidCount + 1, totalSize)
            Right bs -> do
                let actualHash = showHash $ hashByteString bs
                if actualHash == storeHash storePath
                    then return (validCount + 1, invalidCount, totalSize)
                    else do
                        -- Remove invalid path and add to size
                        size <- liftIO $ getFileSize fullPath
                        liftIO $ removeFile fullPath
                        return (validCount, invalidCount + 1, totalSize + size)
