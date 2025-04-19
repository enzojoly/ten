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

    -- Active build management
    getActiveBuildPaths,

    -- Store verification
    verifyStore,
    repairStore
) where

import Control.Concurrent.STM
import Control.Exception (bracket, try, SomeException, catch)
import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
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
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
import System.IO (hPutStrLn, stderr)

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

-- | Global lock for garbage collection
{-# NOINLINE globalGCLock #-}
globalGCLock :: TMVar ()
globalGCLock = unsafePerformIO $ atomically $ newTMVar ()

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

-- | Run garbage collection
collectGarbage :: TenM p GCStats
collectGarbage = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Get the GC lock to prevent concurrent GCs
    liftIO $ atomically $ do
        -- Try to take the global GC lock
        takeTMVar globalGCLock `orElse` throwSTM (ResourceError "Another garbage collection is in progress")

    -- Ensure we release the lock even if an error occurs
    collectGarbageWithStats env `onException`
        (liftIO $ atomically $ putTMVar globalGCLock ())

-- | Run garbage collection with stats
collectGarbageWithStats :: BuildEnv -> TenM p GCStats
collectGarbageWithStats env = do
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
    totalSize <- liftIO $ sum <$> mapM (getFileSizeIO . (storeDir </>)) (Set.toList deletablePaths)

    -- Delete unreachable paths
    deleted <- liftIO $ do
        forM_ (Set.toList deletablePaths) $ \path -> do
            -- Skip paths in gc-roots directory
            unless ("gc-roots/" `isPrefixOf` path) $
                -- Attempt to delete, ignoring errors
                catch (removeFile (storeDir </> path)) $ \(_ :: IOError) -> return ()
        return $ Set.size deletablePaths

    -- Calculate elapsed time
    endTime <- liftIO getCurrentTime
    let elapsed = diffUTCTime endTime startTime

    -- Release the GC lock
    liftIO $ atomically $ putTMVar globalGCLock ()

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

    -- In a real implementation, this would coordinate with the daemon
    -- For now, just use the standard GC
    collectGarbage

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
                let isPermanent = "permanent-" `isPrefixOf` file

                return $ Just $ GCRoot
                    { rootPath = path
                    , rootName = name
                    , rootCreated = now
                    , rootPermanent = isPermanent
                    }
            else return Nothing

    -- Filter out Nothings and return the list
    return $ catMaybes roots

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

-- | Acquire the GC lock
acquireGCLock :: TenM p ()
acquireGCLock = do
    -- Try to take the global GC lock
    locked <- liftIO $ atomically $ do
        isEmpty <- isEmptyTMVar globalGCLock
        if isEmpty
            then return False
            else do
                takeTMVar globalGCLock
                return True

    unless locked $
        throwError $ ResourceError "Could not acquire GC lock"

-- | Release the GC lock
releaseGCLock :: TenM p ()
releaseGCLock = do
    -- Release the global GC lock
    liftIO $ atomically $ do
        isEmpty <- isEmptyTMVar globalGCLock
        unless isEmpty $
            putTMVar globalGCLock ()

-- | Execute an action with the GC lock
withGCLock :: TenM p a -> TenM p a
withGCLock action = do
    -- Acquire lock, run action, then release lock
    bracket
        acquireGCLock
        (\_ -> releaseGCLock)
        (\_ -> action)

-- | Get paths of active builds (to prevent GC during builds)
getActiveBuildPaths :: TenM p (Set FilePath)
getActiveBuildPaths = do
    -- In a real implementation, this would check the daemon state for active builds
    -- For now, just return an empty set
    return Set.empty

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
            let nonStoreDirs = ["gc-roots", "tmp", "locks"]
            paths <- forM (filter (`notElem` nonStoreDirs) entries) $ \entry -> do
                let fullPath = storeDir </> entry
                isDir <- doesDirectoryExist fullPath
                return $ if isDir then Nothing else Just entry

            -- Return valid store paths
            return $ Set.fromList $ catMaybes paths

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

                if not isFile
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
    -- In a full implementation, this would recursively scan references in each file
    -- For a simplified version, we'll assume all store paths are standalone

    -- In a real implementation, we would read each store path to find references to other paths
    -- and build a complete reachability graph

    -- For now, just return the root paths
    return rootPaths

-- | Parse a store path from a file path
parseStorePath :: FilePath -> Maybe StorePath
parseStorePath path =
    case break (== '-') (takeFileName path) of
        (hash, '-':name) -> Just $ StorePath (T.pack hash) (T.pack name)
        _ -> Nothing

-- | Helper to get the size of a file - renamed to avoid ambiguity
getFileSizeIO :: FilePath -> IO Integer
getFileSizeIO path = do
    exists <- doesFileExist path
    if exists
        then do
            info <- getFileStatus path
            return $ fromIntegral $ fileSize info
        else return 0

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
                        size <- liftIO $ getFileSizeIO fullPath
                        liftIO $ removeFile fullPath
                        return (validCount, invalidCount + 1, totalSize + size)

-- | Exception handler for GC
onException :: TenM p a -> TenM p () -> TenM p a
onException action handler = do
    env <- ask
    state <- get

    -- Run the action and handler if it fails
    result <- liftIO $ try $ runTen action env state
    case result of
        Left (err :: BuildError) -> do
            -- Run handler and re-throw error
            _ <- liftIO $ runTen handler env state
            throwError err
        Right (val, _) -> return val

-- | STM version of throwError - fixed implementation to avoid recursion
throwSTM :: BuildError -> STM a
throwSTM err = Control.Concurrent.STM.throwSTM $ toException err

-- | Helper functions for STM
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\x acc -> case x of
                              Just v -> v : acc
                              Nothing -> acc) []
