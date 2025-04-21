{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Ten.DB.References (
    -- Reference registration
    registerReference,
    registerReferences,
    registerPathReferences,

    -- Reference queries
    getReferences,
    getReferrers,
    getDirectReferences,
    getDirectReferrers,

    -- Reference graph operations
    computeReachablePathsFromRoots,
    findPathsClosure,
    findPathsClosureWithLimit,

    -- Garbage collection support
    findGCRoots,
    markPathsAsValid,
    markPathsAsInvalid,

    -- Path validity
    isPathReachable,

    -- Reference scanning (for initial import only)
    scanFileForReferences,
    scanStoreForReferences,

    -- Database operations
    vacuumReferenceDb,
    validateReferenceDb,

    -- Types
    ReferenceEntry(..),
    ReferenceStats(..)
) where

import Control.Exception (try, finally, catch, SomeException)
import Control.Monad (forM, forM_, when, unless, void, foldM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf, isPrefixOf, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.SQLite.Simple (NamedParam(..), Query(..), ToRow(..), FromRow(..), Only(..))
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.FromRow as SQLite (RowParser, field)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink, isRegularFile, fileMode)
import System.IO (IOMode(..), withFile, hFileSize)
import System.IO.MMap (mmapFileByteString)

import Ten.Core (StorePath(..), storePathToText, parseStorePath, StorePathReference(..))
import Ten.DB.Core

-- | A reference from one path to another
data ReferenceEntry = ReferenceEntry {
    refFrom :: !StorePath,  -- ^ Path that contains the reference
    refTo   :: !StorePath   -- ^ Path that is being referenced
} deriving (Show, Eq)

-- | Statistics about references
data ReferenceStats = ReferenceStats {
    totalReferences :: !Int,        -- ^ Total number of references
    uniqueReferrers :: !Int,        -- ^ Number of unique referrers
    uniqueReferences :: !Int,       -- ^ Number of unique references
    avgReferencesPerPath :: !Double -- ^ Average references per path
} deriving (Show, Eq)

-- Make ReferenceEntry an instance of FromRow
instance FromRow ReferenceEntry where
    fromRow = do
        fromText <- SQLite.field :: SQLite.RowParser Text
        toText <- SQLite.field :: SQLite.RowParser Text

        -- Parse store paths safely
        let fromPath = fromMaybe (error $ "Invalid referrer path: " ++ T.unpack fromText)
                                 (parseStorePath fromText)
        let toPath = fromMaybe (error $ "Invalid reference path: " ++ T.unpack toText)
                               (parseStorePath toText)

        return $ ReferenceEntry fromPath toPath

-- Make ReferenceEntry an instance of ToRow
instance ToRow ReferenceEntry where
    toRow (ReferenceEntry from to) = SQLite.toRow (storePathToText from, storePathToText to)

-- Make StorePathReference an instance of FromRow
instance FromRow StorePathReference where
    fromRow = do
        fromText <- SQLite.field :: SQLite.RowParser Text
        toText <- SQLite.field :: SQLite.RowParser Text

        -- Parse store paths safely
        let fromPath = fromMaybe (error $ "Invalid referrer path: " ++ T.unpack fromText)
                                 (parseStorePath fromText)
        let toPath = fromMaybe (error $ "Invalid reference path: " ++ T.unpack toText)
                               (parseStorePath toText)

        return $ StorePathReference fromPath toPath

-- | Register a single reference between two store paths
registerReference :: Database -> StorePath -> StorePath -> IO ()
registerReference db from to = do
    -- Avoid self-references
    when (from /= to) $ do
        dbExecute db
            "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
            (storePathToText from, storePathToText to)

-- | Register multiple references from one referrer
registerReferences :: Database -> StorePath -> Set StorePath -> IO ()
registerReferences db referrer references = do
    -- Use a transaction for efficiency
    withTransaction db ReadWrite $ \db' -> do
        -- Insert each valid reference
        forM_ (Set.toList references) $ \ref ->
            -- Avoid self-references
            when (referrer /= ref) $
                registerReference db' referrer ref

-- | Register references for a path after scanning the file for references
registerPathReferences :: Database -> FilePath -> StorePath -> IO Int
registerPathReferences db storeDir path = do
    -- Construct the full path
    let fullPath = storeDir </> T.unpack (storePathToText path)

    -- Verify the file exists
    exists <- doesFileExist fullPath
    if not exists
        then return 0
        else do
            -- Scan for references
            references <- scanFileForReferences storeDir fullPath

            -- Register the found references (if any)
            if Set.null references
                then return 0
                else do
                    registerReferences db path references
                    return $ Set.size references

-- | Get direct references from a store path
getDirectReferences :: Database -> StorePath -> IO (Set StorePath)
getDirectReferences db path = do
    results <- dbQuery db
        "SELECT reference FROM References WHERE referrer = ?"
        (Only (storePathToText path)) :: IO [Only Text]

    -- Parse each store path and return as a set
    return $ Set.fromList $ catMaybes $
        map (\(Only p) -> parseStorePath p) results

-- | Get all references from a path (direct and indirect)
getReferences :: Database -> StorePath -> IO (Set StorePath)
getReferences db path = do
    -- Use recursive CTE to efficiently query the closure
    let query = "WITH RECURSIVE\n\
                \  closure(path) AS (\n\
                \    VALUES(?)\n\
                \    UNION\n\
                \    SELECT reference FROM References, closure \n\
                \    WHERE referrer = closure.path\n\
                \  )\n\
                \SELECT path FROM closure WHERE path != ?"

    results <- dbQuery db query (storePathToText path, storePathToText path) :: IO [Only Text]

    -- Parse each store path and return as a set
    return $ Set.fromList $ catMaybes $
        map (\(Only p) -> parseStorePath p) results

-- | Get direct referrers to a store path
getDirectReferrers :: Database -> StorePath -> IO (Set StorePath)
getDirectReferrers db path = do
    results <- dbQuery db
        "SELECT referrer FROM References WHERE reference = ?"
        (Only (storePathToText path)) :: IO [Only Text]

    -- Parse each store path and return as a set
    return $ Set.fromList $ catMaybes $
        map (\(Only p) -> parseStorePath p) results

-- | Get all referrers to a path (direct and indirect)
getReferrers :: Database -> StorePath -> IO (Set StorePath)
getReferrers db path = do
    -- Use recursive CTE to efficiently query the closure
    let query = "WITH RECURSIVE\n\
                \  closure(path) AS (\n\
                \    VALUES(?)\n\
                \    UNION\n\
                \    SELECT referrer FROM References, closure \n\
                \    WHERE reference = closure.path\n\
                \  )\n\
                \SELECT path FROM closure WHERE path != ?"

    results <- dbQuery db query (storePathToText path, storePathToText path) :: IO [Only Text]

    -- Parse each store path and return as a set
    return $ Set.fromList $ catMaybes $
        map (\(Only p) -> parseStorePath p) results

-- | Find all GC roots
findGCRoots :: Database -> FilePath -> IO (Set StorePath)
findGCRoots db storeDir = do
    -- Get roots from the file system (symlinks in gc-roots directory)
    let rootsDir = storeDir </> "gc-roots"
    fsRoots <- getGCRootsFromFS rootsDir

    -- Get explicitly registered roots from the database
    dbRoots <- getRegisteredRoots db

    -- Combine both sets
    return $ Set.union fsRoots dbRoots

-- | Get GC roots from filesystem
getGCRootsFromFS :: FilePath -> IO (Set StorePath)
getGCRootsFromFS rootsDir = do
    -- Check if the roots directory exists
    exists <- doesDirectoryExist rootsDir
    if not exists
        then return Set.empty
        else do
            -- List all files in the roots directory
            files <- listDirectory rootsDir

            -- Process each file that is a symlink
            roots <- forM files $ \file -> do
                let path = rootsDir </> file

                -- Check if it's a symlink
                isLink <- isSymbolicLink <$> getFileStatus path

                if isLink
                    then do
                        -- Read the target
                        target <- try $ readSymbolicLink path
                        case target of
                            Left (_ :: SomeException) -> return Nothing
                            Right targetPath -> do
                                -- Parse the store path from the target filename
                                return $ parseStorePath $ T.pack $ takeFileName targetPath
                    else return Nothing

            -- Return the set of valid roots
            return $ Set.fromList $ catMaybes roots

-- | Get roots registered in the database
getRegisteredRoots :: Database -> IO (Set StorePath)
getRegisteredRoots db = do
    results <- dbQuery_ db
        "SELECT path FROM GCRoots WHERE active = 1" :: IO [Only Text]

    -- Parse each store path and return as a set
    return $ Set.fromList $ catMaybes $
        map (\(Only p) -> parseStorePath p) results

-- | Compute all paths reachable from a set of roots
computeReachablePathsFromRoots :: Database -> Set StorePath -> IO (Set StorePath)
computeReachablePathsFromRoots db roots = do
    -- If no roots, return empty set
    if Set.null roots
        then return Set.empty
        else do
            -- Create temporary table for roots
            withTransaction db ReadWrite $ \db' -> do
                -- Create temp table
                dbExecute_ db' "CREATE TEMP TABLE IF NOT EXISTS temp_roots (path TEXT PRIMARY KEY)"

                -- Clear previous roots
                dbExecute_ db' "DELETE FROM temp_roots"

                -- Insert all roots
                forM_ (Set.toList roots) $ \root ->
                    dbExecute db' "INSERT INTO temp_roots VALUES (?)"
                             (Only (storePathToText root))

                -- Use recursive CTE to find all reachable paths
                let query = "WITH RECURSIVE\n\
                            \  reachable(path) AS (\n\
                            \    SELECT path FROM temp_roots\n\
                            \    UNION\n\
                            \    SELECT reference FROM References, reachable \n\
                            \    WHERE referrer = reachable.path\n\
                            \  )\n\
                            \SELECT path FROM reachable"

                results <- dbQuery_ db' query :: IO [Only Text]

                -- Parse each store path and return as a set
                return $ Set.fromList $ catMaybes $
                    map (\(Only p) -> parseStorePath p) results

-- | Find the transitive closure of paths
findPathsClosure :: Database -> Set StorePath -> IO (Set StorePath)
findPathsClosure db startingPaths =
    findPathsClosureWithLimit db startingPaths (-1)  -- No limit

-- | Find the transitive closure of paths with depth limit
findPathsClosureWithLimit :: Database -> Set StorePath -> Int -> IO (Set StorePath)
findPathsClosureWithLimit db startingPaths depthLimit = do
    -- If no starting paths, return empty set
    if Set.null startingPaths
        then return Set.empty
        else do
            -- Create temporary table for the starting set
            withTransaction db ReadWrite $ \db' -> do
                -- Create temp table
                dbExecute_ db' "CREATE TEMP TABLE IF NOT EXISTS temp_closure_start (path TEXT PRIMARY KEY)"

                -- Clear previous data
                dbExecute_ db' "DELETE FROM temp_closure_start"

                -- Insert all starting paths
                forM_ (Set.toList startingPaths) $ \path ->
                    dbExecute db' "INSERT INTO temp_closure_start VALUES (?)"
                              (Only (storePathToText path))

                -- Use recursive CTE to find the closure
                let limitClause = if depthLimit > 0
                                 then T.pack $ ", " ++ show depthLimit
                                 else ""

                let query = T.concat [
                        "WITH RECURSIVE\n",
                        "  closure(path, depth) AS (\n",
                        "    SELECT path, 0 FROM temp_closure_start\n",
                        "    UNION\n",
                        "    SELECT reference, depth + 1 FROM References, closure \n",
                        "    WHERE referrer = closure.path",
                        if depthLimit > 0
                            then T.concat [" AND depth < ", T.pack $ show depthLimit, "\n"]
                            else "\n",
                        "  )\n",
                        "SELECT path FROM closure"
                      ]

                results <- dbQuery_ db' (Query query) :: IO [Only Text]

                -- Parse each store path and return as a set
                return $ Set.fromList $ catMaybes $
                    map (\(Only p) -> parseStorePath p) results

-- | Check if a path is reachable from any of the GC roots
isPathReachable :: Database -> Set StorePath -> StorePath -> IO Bool
isPathReachable db roots path = do
    -- First check if the path is itself a root
    if path `Set.member` roots
        then return True
        else do
            -- Use SQL to efficiently check reachability
            let query = "WITH RECURSIVE\n\
                        \  reachable(path) AS (\n\
                        \    VALUES " ++ rootsValues (Set.toList roots) ++ "\n\
                        \    UNION\n\
                        \    SELECT reference FROM References, reachable \n\
                        \    WHERE referrer = reachable.path\n\
                        \  )\n\
                        \SELECT COUNT(*) FROM reachable WHERE path = ?"

            results <- dbQuery db (Query $ T.pack query) (Only (storePathToText path)) :: IO [Only Int]

            case results of
                [Only count] -> return (count > 0)
                _ -> return False
  where
    -- Helper to create SQL values clause
    rootsValues [] = "(NULL)"  -- Avoid empty VALUES clause
    rootsValues [r] = "('" ++ T.unpack (storePathToText r) ++ "')"
    rootsValues (r:rs) = "('" ++ T.unpack (storePathToText r) ++ "'), " ++ rootsValues rs

-- | Mark a set of paths as valid in the ValidPaths table
markPathsAsValid :: Database -> Set StorePath -> IO ()
markPathsAsValid db paths =
    withTransaction db ReadWrite $ \db' -> do
        forM_ (Set.toList paths) $ \path ->
            dbExecute db'
                "UPDATE ValidPaths SET is_valid = 1 WHERE path = ?"
                (Only (storePathToText path))

-- | Mark a set of paths as invalid in the ValidPaths table
markPathsAsInvalid :: Database -> Set StorePath -> IO ()
markPathsAsInvalid db paths =
    withTransaction db ReadWrite $ \db' -> do
        forM_ (Set.toList paths) $ \path ->
            dbExecute db'
                "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
                (Only (storePathToText path))

-- | Vacuum the reference database to optimize performance
vacuumReferenceDb :: Database -> IO ()
vacuumReferenceDb db = do
    -- Remove dangling references
    cleanupDanglingReferences db

    -- Analyze tables
    dbExecute_ db "ANALYZE References"

    -- Vacuum database
    dbExecute_ db "VACUUM"

-- | Validate and repair the reference database
validateReferenceDb :: Database -> FilePath -> IO (Int, Int)
validateReferenceDb db storeDir = do
    -- Count existing references
    [Only totalRefs] <- dbQuery_ db "SELECT COUNT(*) FROM References" :: IO [Only Int]

    -- Remove references to non-existent paths
    invalid <- cleanupDanglingReferences db

    -- Return statistics
    return (totalRefs, invalid)

-- | Cleanup dangling references
cleanupDanglingReferences :: Database -> IO Int
cleanupDanglingReferences db = withTransaction db ReadWrite $ \db' -> do
    -- Find references to paths that don't exist in ValidPaths
    dangling <- dbQuery_ db'
        "SELECT referrer, reference FROM References \
        \WHERE reference NOT IN (SELECT path FROM ValidPaths WHERE is_valid = 1)"
        :: IO [(Text, Text)]

    -- Delete these references
    forM_ dangling $ \(from, to) ->
        dbExecute db'
            "DELETE FROM References WHERE referrer = ? AND reference = ?"
            (from, to)

    return $ length dangling

-- | Scan a file for references to store paths
scanFileForReferences :: FilePath -> FilePath -> IO (Set StorePath)
scanFileForReferences storeDir filePath = do
    -- Check if the file exists
    exists <- doesFileExist filePath
    if not exists
        then return Set.empty
        else do
            -- Detect file type and use appropriate scanner
            fileType <- detectFileType filePath

            case fileType of
                ElfBinary -> scanElfBinary filePath storeDir
                TextFile -> scanTextFile filePath storeDir
                ScriptFile -> scanTextFile filePath storeDir
                BinaryFile -> scanBinaryFile filePath storeDir

-- | File types for scanning
data FileType = ElfBinary | TextFile | ScriptFile | BinaryFile
    deriving (Show, Eq)

-- | Detect the type of a file
detectFileType :: FilePath -> IO FileType
detectFileType path = do
    -- Check file existence
    exists <- doesFileExist path
    if not exists
        then return BinaryFile
        else do
            -- Read first bytes to identify file type
            header <- BS.take 4 <$> BS.readFile path

            let ext = takeExtension path

            if header == elfMagic
                then return ElfBinary
                else if BS.take 2 header == shebangMagic
                    then return ScriptFile
                    else if ext `elem` textExtensions
                        then return TextFile
                        else return BinaryFile
  where
    elfMagic = BS.pack [0x7F, 0x45, 0x4C, 0x46]  -- .ELF
    shebangMagic = BS.pack [0x23, 0x21]          -- #!
    textExtensions = [".txt", ".xml", ".json", ".js", ".html", ".c", ".h",
                     ".cpp", ".py", ".sh", ".bash", ".log", ".conf", ".yml",
                     ".yaml", ".hs", ".cabal", ".md", ".nix"]

-- | Scan an ELF binary for store path references
scanElfBinary :: FilePath -> FilePath -> IO (Set StorePath)
scanElfBinary filePath storeDir = do
    -- Memory map the file for efficient access
    content <- mmapFileByteString filePath Nothing

    -- Extract text segments that might contain references
    let textFragments = extractTextFromBinary content

    -- Scan each fragment for store path references
    let storeRoot = storeDir ++ "/"
    let references = concatMap (findStoreReferences storeRoot) textFragments

    -- Return unique valid references
    return $ Set.fromList $ catMaybes $
        map (\path -> parseStorePath $ T.pack path) $
        nub references

-- | Scan a text file for store path references
scanTextFile :: FilePath -> FilePath -> IO (Set StorePath)
scanTextFile filePath storeDir = do
    -- Read file content
    content <- BS.readFile filePath

    -- Convert to text, ignoring encoding errors
    let text = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') content

    -- Scan for store path references
    let storeRoot = T.pack storeDir ++ "/"
    let references = findStoreRefsInText storeRoot text

    -- Return unique valid references
    return $ Set.fromList $ catMaybes $
        map parseStorePath $ nub references

-- | Scan a binary file for possible store path references
scanBinaryFile :: FilePath -> FilePath -> IO (Set StorePath)
scanBinaryFile filePath storeDir = do
    -- Memory map the file for efficient access
    content <- mmapFileByteString filePath Nothing

    -- Extract potential text fragments
    let textFragments = extractTextFromBinary content

    -- Scan each fragment for store path references
    let storeRoot = storeDir ++ "/"
    let references = concatMap (findStoreReferences storeRoot) textFragments

    -- Return unique valid references
    return $ Set.fromList $ catMaybes $
        map (\path -> parseStorePath $ T.pack path) $
        nub references

-- | Extract text fragments from binary data
extractTextFromBinary :: ByteString -> [String]
extractTextFromBinary bs =
    -- Split by null bytes
    map BC.unpack $ filter isValidAsciiString $ BC.split '\0' bs
  where
    isValidAsciiString :: ByteString -> Bool
    isValidAsciiString s =
        -- Must be reasonable length and mostly printable ASCII
        BS.length s >= 4 &&
        BS.length s <= 2048 &&
        BS.all (\c -> c >= 32 && c < 127 || c == 9 || c == 10) s

-- | Find store path references in text
findStoreRefsInText :: Text -> Text -> [Text]
findStoreRefsInText storeRoot text =
    -- Split text into lines
    let lines = T.splitOn "\n" text

        -- For each line, find all tokens
        tokens = concatMap T.words lines

        -- Filter for potential store paths
        potentialPaths = filter isPotentialStorePath tokens

        -- Extract store paths
        storePaths = mapMaybe (extractStorePath storeRoot) potentialPaths
    in
        storePaths
  where
    isPotentialStorePath :: Text -> Bool
    isPotentialStorePath t =
        -- Quick pre-filtering
        T.length t >= 12 &&
        "-" `T.isInfixOf` t

    extractStorePath :: Text -> Text -> Maybe Text
    extractStorePath root t =
        -- Extract store paths that look like:
        -- /nix/store/hash-name or just hash-name
        if root `T.isPrefixOf` t
            then
                -- Path starts with store root, extract the filename
                let path = T.drop (T.length root) t
                in if isValidStorePathFormat path
                    then Just path
                    else Nothing
            else
                -- Check if it's a raw store path
                if isValidStorePathFormat t
                    then Just t
                    else Nothing

    isValidStorePathFormat :: Text -> Bool
    isValidStorePathFormat t =
        -- Check for hash-name format
        case T.breakOn "-" t of
            (hash, name) | not (T.null name) && T.length hash >= 8 ->
                         -- Hash should be hex digits
                         T.all isHexDigit hash &&
                         -- Name should start with - and have valid chars
                         T.all isValidNameChar (T.drop 1 name)
            _ -> False

    isHexDigit :: Char -> Bool
    isHexDigit c = (c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F')

    isValidNameChar :: Char -> Bool
    isValidNameChar c = (c >= 'a' && c <= 'z') ||
                        (c >= 'A' && c <= 'Z') ||
                        (c >= '0' && c <= '9') ||
                        c == '+' || c == '-' || c == '.' || c == '_'

-- | Find store path references in string data
findStoreReferences :: String -> String -> [String]
findStoreReferences storeRoot s =
    -- Split string into tokens
    let tokens = words s

        -- Find potential store paths
        paths = filter (isStorePath storeRoot) tokens
    in
        paths
  where
    isStorePath :: String -> String -> Bool
    isStorePath root path =
        -- Check if it starts with the store root
        (root `isPrefixOf` path &&
         '-' `elem` drop (length root) path) ||
        -- Or check if it's a valid hash-name format directly
        (length path >= 10 &&
         '-' `elem` path &&
         all isHexDigit (takeWhile (/= '-') path))

    isHexDigit :: Char -> Bool
    isHexDigit c = (c >= '0' && c <= '9') ||
                   (c >= 'a' && c <= 'f') ||
                   (c >= 'A' && c <= 'F')

-- | Scan the entire store for references between paths
scanStoreForReferences :: FilePath -> IO (Set StorePathReference)
scanStoreForReferences storeDir = do
    -- Get all store paths
    paths <- listStoreContents storeDir

    -- For each path, scan for references to other paths
    references <- foldM (\acc path -> do
        -- Get the full path in the filesystem
        let fullPath = storeDir </> T.unpack (storePathToText path)

        -- Skip if not a regular file (e.g., if it's a directory)
        isFile <- doesFileExist fullPath
        if not isFile
            then return acc
            else do
                -- Scan for references
                refs <- scanFileForReferences storeDir fullPath

                -- Add each reference to the accumulator
                let newRefs = Set.map (StorePathReference path) refs
                return $ Set.union acc newRefs
        ) Set.empty paths

    return references

-- | List all paths in the store
listStoreContents :: FilePath -> IO (Set StorePath)
listStoreContents storeDir = do
    -- Check if directory exists
    exists <- doesDirectoryExist storeDir
    if not exists
        then return Set.empty
        else do
            -- List all entries
            entries <- listDirectory storeDir

            -- Filter for those that match the store path pattern
            let validPaths = catMaybes $ map (\entry -> parseStorePath $ T.pack entry) entries
            return $ Set.fromList validPaths

-- Helper function for Maybe mapping
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
    case f x of
        Nothing -> mapMaybe f xs
        Just y -> y : mapMaybe f xs
