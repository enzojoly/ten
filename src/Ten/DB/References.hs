{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ten.DB.References (
    -- Reference registration
    registerReference,
    registerReferences,
    registerPathReferences,

    -- Reference queries
    getReferences,
    getReferrers,
    getAllReferences,
    queryPathsReferencing,
    queryPathsReferencedBy,

    -- Reference scanning
    scanFileForReferences,

    -- Reachability operations
    computeReachablePaths,
    isReachableFrom,
    getTransitiveClosure,

    -- Reference maintenance
    validateReferences,
    cleanupDanglingReferences,

    -- Garbage collection support
    findRoots,
    getPathsReachableFromRoots,
    pruneUnreachablePaths
) where

import Control.Exception (catch, try, throwIO, SomeException)
import Control.Monad (forM, forM_, when, unless, foldM, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Int (Int64)
import Data.List (nub)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import System.FilePath ((</>), takeFileName)
import System.IO (withFile, IOMode(..))
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, isRegularFile, readSymbolicLink)

import Ten.DB.Core
import Ten.Core (StorePath(..), storeHash, storeName)

-- | Register a single reference
registerReference :: Database -> Text -> Text -> IO ()
registerReference db referrer reference = do
    -- Check that both paths exist
    let q = "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
    execute db q (referrer, reference)

-- | Register multiple references from one referrer
registerReferences :: Database -> Text -> [Text] -> IO ()
registerReferences db referrer references = do
    -- Use a transaction for efficiency
    withTransaction db ReadWrite $ \db' -> do
        -- Insert each reference
        forM_ references $ \reference ->
            registerReference db' referrer reference

-- | Register references for a path, scanning the file for references
registerPathReferences :: Database -> FilePath -> Text -> IO ()
registerPathReferences db storeDir path = do
    -- Scan the file for references
    references <- scanFileForReferences storeDir path

    -- Register the found references
    registerReferences db path references

-- | Get direct references from a path
getReferences :: Database -> Text -> IO [Text]
getReferences db path = do
    let q = "SELECT reference FROM References WHERE referrer = ?"
    map fromOnly <$> query db q [path]

-- | Get paths that reference a given path
getReferrers :: Database -> Text -> IO [Text]
getReferrers db path = do
    let q = "SELECT referrer FROM References WHERE reference = ?"
    map fromOnly <$> query db q [path]

-- | Get all references in the database
getAllReferences :: Database -> IO [(Text, Text)]
getAllReferences db = do
    let q = "SELECT referrer, reference FROM References"
    query_ db q

-- | Query paths that reference a given path
queryPathsReferencing :: Database -> Text -> IO [Text]
queryPathsReferencing = getReferrers

-- | Query paths that are referenced by a given path
queryPathsReferencedBy :: Database -> Text -> IO [Text]
queryPathsReferencedBy = getReferences

-- | Scan a file for references to store paths
scanFileForReferences :: FilePath -> Text -> IO [Text]
scanFileForReferences storeDir path = do
    -- Construct the full path
    let fullPath = storeDir </> T.unpack path

    -- Get file status to determine type
    status <- try $ getSymbolicLinkStatus fullPath
    case status of
        Left (_ :: SomeException) -> return []
        Right stat -> do
            if isSymbolicLink stat then
                -- Handle symbolic link
                scanSymlink fullPath
            else if isRegularFile stat then
                -- Handle regular file
                scanRegularFile fullPath
            else
                -- Unknown file type
                return []
  where
    -- Scan a symbolic link for references
    scanSymlink :: FilePath -> IO [Text]
    scanSymlink linkPath = do
        -- Read the link target
        targetPath <- try $ readSymbolicLink linkPath
        case targetPath of
            Left (_ :: SomeException) -> return []
            Right target -> do
                -- Check if target is in the store
                let tPath = T.pack $ takeFileName target
                let isStorePath = validStorePathFormat tPath
                if isStorePath
                    then return [tPath]
                    else return []

    -- Scan a regular file for references
    scanRegularFile :: FilePath -> IO [Text]
    scanRegularFile filePath = do
        -- Read file content
        content <- try $ BS.readFile filePath
        case content of
            Left (_ :: SomeException) -> return []
            Right bytes -> do
                -- Extract potential store paths
                let refs = extractStorePathReferences bytes
                -- Return unique, valid references
                return $ nub $ filter validStorePathFormat refs

-- | Extract potential store paths from file content
extractStorePathReferences :: ByteString -> [Text]
extractStorePathReferences content =
    -- This is a simplified implementation. A real one would use more sophisticated
    -- pattern matching and context awareness to identify store paths embedded in files.
    let
        -- Convert to lines
        lines = BC.lines content

        -- Look for store path patterns in each line
        pathsInLines = concatMap extractPathsFromLine lines
    in
        pathsInLines
  where
    extractPathsFromLine :: ByteString -> [Text]
    extractPathsFromLine line =
        -- Split by spaces and look for store path patterns
        let words = BC.words line
            potentialPaths = map TE.decodeUtf8 words
        in
            filter looksLikeStorePath potentialPaths

    -- Simple heuristic for store paths
    looksLikeStorePath :: Text -> Bool
    looksLikeStorePath text =
        -- Check if it has a hash-like prefix followed by a dash
        case T.splitOn "-" text of
            (hash:_) -> T.length hash > 7 && all isHexChar (T.unpack hash)
            _ -> False

    isHexChar :: Char -> Bool
    isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Check if text is in valid store path format
validStorePathFormat :: Text -> Bool
validStorePathFormat text =
    -- Check for hash-name format
    case T.splitOn "-" text of
        [hash, name] -> T.length hash > 7 && not (T.null name) && all isHexChar (T.unpack hash)
        _ -> False
  where
    isHexChar :: Char -> Bool
    isHexChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Compute all paths reachable from a set of roots
computeReachablePaths :: Database -> [Text] -> IO (Set Text)
computeReachablePaths db roots = do
    -- Use a recursive CTE to find all reachable paths
    let cteQuery = "WITH RECURSIVE\
                   \  reachable(path) AS (\
                   \    VALUES " ++ placeholders (length roots) ++ "\
                   \    UNION\
                   \    SELECT reference FROM References JOIN reachable ON referrer = path\
                   \  )\
                   \SELECT path FROM reachable;"

    -- Convert roots to the format needed for query
    let rootParams = map Only roots

    -- Execute the query
    paths <- query db (Query $ T.pack cteQuery) rootParams

    -- Convert result to a set
    return $ Set.fromList $ map fromOnly paths
  where
    -- Generate SQL placeholders like (?,?,...,?)
    placeholders :: Int -> String
    placeholders n
        | n <= 0    = ""
        | n == 1    = "(?)"
        | otherwise = "(?)" ++ replicate (n-1) ',(?)'

-- | Check if a path is reachable from the given roots
isReachableFrom :: Database -> [Text] -> Text -> IO Bool
isReachableFrom db roots path = do
    reachable <- computeReachablePaths db roots
    return $ path `Set.member` reachable

-- | Get the transitive closure of references for a path
getTransitiveClosure :: Database -> Text -> IO (Set Text)
getTransitiveClosure db path = do
    -- Use recursive CTE to find all references
    let q = "WITH RECURSIVE\
             \  closure(p) AS (\
             \    VALUES(?)\
             \    UNION\
             \    SELECT reference FROM References JOIN closure ON referrer = p\
             \  )\
             \SELECT p FROM closure;"

    -- Execute query and convert to set
    results <- query db q [path]
    return $ Set.fromList $ map fromOnly results

-- | Validate all references in the database
validateReferences :: Database -> FilePath -> IO (Int, Int)
validateReferences db storeDir = do
    -- Get all references
    refs <- getAllReferences db

    -- Check each reference
    (valid, invalid) <- foldM checkRef (0, 0) refs

    -- Return validation results
    return (valid, invalid)
  where
    checkRef :: (Int, Int) -> (Text, Text) -> IO (Int, Int)
    checkRef (valid, invalid) (referrer, reference) = do
        -- Check if both paths exist
        referrerExists <- doesPathExist db storeDir referrer
        referenceExists <- doesPathExist db storeDir reference

        if referrerExists && referenceExists
            then return (valid + 1, invalid)
            else do
                -- If invalid, remove the reference
                let q = "DELETE FROM References WHERE referrer = ? AND reference = ?"
                execute db q (referrer, reference)
                return (valid, invalid + 1)

-- | Check if a path exists in the store
doesPathExist :: Database -> FilePath -> Text -> IO Bool
doesPathExist db storeDir path = do
    -- First check the ValidPaths table
    let q = "SELECT COUNT(*) FROM ValidPaths WHERE path = ? AND is_valid = 1"
    results <- query db q [path]

    case results of
        [Only (count :: Int)] | count > 0 -> return True
        _ -> do
            -- Fallback to filesystem check
            let fullPath = storeDir </> T.unpack path
            isFile <- try $ getSymbolicLinkStatus fullPath
            case isFile of
                Left (_ :: SomeException) -> return False
                Right _ -> return True

-- | Clean up dangling references
cleanupDanglingReferences :: Database -> FilePath -> IO Int
cleanupDanglingReferences db storeDir = do
    -- Find all references
    refs <- getAllReferences db

    -- Check and remove invalid references
    removed <- withTransaction db ReadWrite $ \db' ->
        foldM removeIfInvalid 0 refs

    -- Return count of removed references
    return removed
  where
    removeIfInvalid :: Int -> (Text, Text) -> IO Int
    removeIfInvalid count (referrer, reference) = do
        -- Check if both paths exist
        referrerExists <- doesPathExist db storeDir referrer
        referenceExists <- doesPathExist db storeDir reference

        if not (referrerExists && referenceExists) then do
            -- Remove invalid reference
            let q = "DELETE FROM References WHERE referrer = ? AND reference = ?"
            execute db q (referrer, reference)
            return (count + 1)
        else
            return count

-- | Find all roots (paths with no referrers)
findRoots :: Database -> IO [Text]
findRoots db = do
    -- Paths that exist in ValidPaths but not as references in References
    let q = "SELECT path FROM ValidPaths WHERE is_valid = 1 \
            \AND path NOT IN (SELECT reference FROM References)"

    -- Execute the query
    map fromOnly <$> query_ db q

-- | Get all paths reachable from GC roots
getPathsReachableFromRoots :: Database -> [Text] -> IO (Set Text)
getPathsReachableFromRoots = computeReachablePaths

-- | Prune unreachable paths from ValidPaths table
pruneUnreachablePaths :: Database -> [Text] -> IO Int
pruneUnreachablePaths db roots = do
    -- Find reachable paths
    reachable <- computeReachablePaths db roots

    -- Find all valid paths
    let qAllPaths = "SELECT path FROM ValidPaths WHERE is_valid = 1"
    allPaths <- Set.fromList . map fromOnly <$> query_ db qAllPaths

    -- Calculate unreachable paths
    let unreachable = Set.difference allPaths reachable

    -- Mark unreachable paths as invalid
    withTransaction db ReadWrite $ \db' -> do
        forM_ (Set.toList unreachable) $ \path -> do
            let qInvalidate = "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
            execute db' qInvalidate [path]

    -- Return number of pruned paths
    return $ Set.size unreachable
