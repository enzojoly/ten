{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Ten.GC (
    GCRoot(..),
    GCStats(..),
    addRoot,
    removeRoot,
    collectGarbage,
    listRoots,
    isDeletable
) where

import Control.Monad
import Data.Time
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath

import Ten.Core
import Ten.Store

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
    } deriving (Show, Eq)

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
    let rootsDir = storePath env </> "gcroots"
    liftIO $ createDirectoryIfMissing True rootsDir
    
    -- Generate a unique filename for the root
    let rootFile = rootsDir </> (T.unpack $ storeHash path <> "-" <> name)
    
    -- Write a symlink to the actual path
    let targetPath = storePathToFilePath path env
    liftIO $ createFileLink targetPath rootFile
    
    logMsg 1 $ "Added GC root: " <> name <> " -> " <> storeHash path
    
    return root

-- | Remove a root
removeRoot :: GCRoot -> TenM p ()
removeRoot root = do
    env <- ask
    
    -- Generate the root file path
    let rootsDir = storePath env </> "gcroots"
    let rootFile = rootsDir </> (T.unpack $ storeHash (rootPath root) <> "-" <> rootName root)
    
    -- Check if it exists
    exists <- liftIO $ doesFileExist rootFile
    
    -- Remove it if it exists
    when exists $ do
        liftIO $ removeFile rootFile
        logMsg 1 $ "Removed GC root: " <> rootName root <> " -> " <> storeHash (rootPath root)

-- | Collect garbage
collectGarbage :: TenM p GCStats
collectGarbage = do
    env <- ask
    
    -- Find all paths in the store
    let storeDir = storePath env
    storePaths <- liftIO $ findStorePaths storeDir
    
    -- Find all root-reachable paths
    rootPaths <- findRootPaths storeDir
    
    -- Find all paths reachable from roots (transitive closure)
    reachablePaths <- computeReachablePaths rootPaths
    
    -- Find all deletable paths
    let deletable = storePaths `Set.difference` reachablePaths
    
    -- Get total size of deletable paths
    totalSize <- liftIO $ sum <$> mapM (getFileSize . (\p -> storeDir </> p)) (Set.toList deletable)
    
    -- Delete unreachable paths
    deleted <- liftIO $ do
        forM_ (Set.toList deletable) $ \path -> do
            removeFile (storeDir </> path)
        return $ Set.size deletable
    
    -- Return statistics
    return GCStats
        { gcTotal = Set.size storePaths
        , gcLive = Set.size reachablePaths
        , gcCollected = deleted
        , gcBytes = totalSize
        }

-- | List all current GC roots
listRoots :: TenM p [GCRoot]
listRoots = do
    env <- ask
    
    -- Get the roots directory
    let rootsDir = storePath env </> "gcroots"
    
    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir
    
    -- List all files in the roots directory
    files <- liftIO $ listDirectory rootsDir
    
    -- Parse each file into a GCRoot
    -- This is a simplified implementation - in reality we would need to
    -- read the symlink target and parse the root information
    now <- liftIO getCurrentTime
    let roots = map (\f -> GCRoot
                      { rootPath = StorePath (T.pack $ takeBaseName f) (T.pack "unknown")
                      , rootName = T.pack $ takeBaseName f
                      , rootCreated = now
                      , rootPermanent = False
                      })
                   files
    
    return roots

-- | Check if a path is deletable (not reachable from any root)
isDeletable :: StorePath -> TenM p Bool
isDeletable path = do
    env <- ask
    
    -- Find all root-reachable paths
    let storeDir = storePath env
    rootPaths <- findRootPaths storeDir
    
    -- Find all paths reachable from roots
    reachablePaths <- computeReachablePaths rootPaths
    
    -- Check if our path is reachable
    let pathStr = T.unpack $ storeHash path <> "-" <> storeName path
    return $ not $ pathStr `Set.member` reachablePaths

-- | Helper to find all paths in the store
findStorePaths :: FilePath -> IO (Set String)
findStorePaths storeDir = do
    -- List all files in the store directory
    files <- listDirectory storeDir
    
    -- Filter out any non-store paths (like gcroots directory)
    let storePaths = filter (\f -> not $ f `elem` ["gcroots", "tmp"]) files
    
    return $ Set.fromList storePaths

-- | Helper to find all root paths
findRootPaths :: FilePath -> TenM p (Set String)
findRootPaths storeDir = do
    -- Get the roots directory
    let rootsDir = storeDir </> "gcroots"
    
    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir
    
    -- List all files in the roots directory
    files <- liftIO $ try $ listDirectory rootsDir
    
    -- Read each symlink to find the target
    case files of
        Left _ -> return Set.empty
        Right fs -> do
            paths <- liftIO $ forM fs $ \f -> do
                target <- try $ getSymbolicLinkTarget (rootsDir </> f)
                case target of
                    Left _ -> return Nothing
                    Right t -> return $ Just $ takeFileName t
            
            -- Filter out any failures and return the set of paths
            return $ Set.fromList $ map takeFileName $ catMaybes paths
  where
    try :: IO a -> IO (Either IOError a)
    try = Control.Exception.try
    
    catMaybes :: [Maybe a] -> [a]
    catMaybes = foldr (\x acc -> case x of Nothing -> acc; Just y -> y : acc) []

-- | Helper to compute all paths reachable from a set of roots
computeReachablePaths :: Set String -> TenM p (Set String)
computeReachablePaths roots = do
    -- This is a simplified implementation
    -- In a real implementation, we would recursively scan references
    -- in store paths to build the complete transitive closure
    return roots

-- | Helper to get the size of a file
getFileSize :: FilePath -> IO Integer
getFileSize path = do
    exists <- doesFileExist path
    if exists
        then getFileSize' path
        else return 0
  where
    getFileSize' :: FilePath -> IO Integer
    getFileSize' p = do
        info <- getFileStatus p
        return $ fromIntegral $ fileSize info
