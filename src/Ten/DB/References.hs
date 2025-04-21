{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module Ten.DB.References (
    -- Reference registration for privileged context
    registerReference,
    registerReferences,
    registerPathReferences,

    -- Reference queries (available in both contexts)
    getReferences,
    getReferrers,
    getDirectReferences,
    getDirectReferrers,

    -- Reference graph operations for privileged context
    computeReachablePathsFromRoots,
    findPathsClosure,
    findPathsClosureWithLimit,

    -- Garbage collection support for privileged context
    findGCRoots,
    markPathsAsValid,
    markPathsAsInvalid,

    -- Path reachability (available in both contexts via appropriate implementation)
    isPathReachable,

    -- Reference scanning (for initial import only, privileged)
    scanFileForReferences,
    scanStoreForReferences,

    -- Database operations (privileged)
    vacuumReferenceDb,
    validateReferenceDb,

    -- Protocol-based operations for unprivileged context
    requestRegisterReference,
    requestRegisterReferences,
    requestComputeReachablePaths,
    requestFindPathsClosure,
    requestIsPathReachable,

    -- Types
    ReferenceEntry(..),
    ReferenceStats(..)
) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Exception (try, finally, catch, throwIO, SomeException, IOException)
import Control.Monad (forM, forM_, when, unless, void, foldM)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.SQLite.Simple (NamedParam(..), Query(..), ToRow(..), FromRow(..), Only(..))
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.FromRow as SQLite (RowParser, field)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeFileName, takeExtension, takeDirectory)
import System.Posix.Files (getFileStatus, isSymbolicLink, readSymbolicLink, isRegularFile, fileMode)
import System.IO (IOMode(..), withFile, hFileSize, stdout, stderr, hPutStrLn)
import System.IO.MMap (mmapFileByteString)
import Data.Char (isHexDigit)

import Ten.Core
import Ten.DB.Core
import Ten.Daemon.Protocol
import Ten.Daemon.Client

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

-- | Register a single reference between two store paths (privileged operation)
registerReference :: Database -> StorePath -> StorePath -> TenM p 'Privileged ()
registerReference db from to =
    -- Avoid self-references
    when (from /= to) $
        tenExecute_ db
            "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
            (storePathToText from, storePathToText to)

-- | Register multiple references from one referrer (privileged operation)
registerReferences :: Database -> StorePath -> Set StorePath -> TenM p 'Privileged ()
registerReferences db referrer references = withTenTransaction db ReadWrite $ \_ -> do
    -- Insert each valid reference
    forM_ (Set.toList references) $ \ref ->
        -- Avoid self-references
        when (referrer /= ref) $
            tenExecute_ db
                   "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
                   (storePathToText referrer, storePathToText ref)

-- | Register references for a path after scanning the file for references (privileged operation)
registerPathReferences :: Database -> FilePath -> StorePath -> TenM p 'Privileged Int
registerPathReferences db storeDir path = do
    env <- ask

    -- Construct the full path
    let fullPath = storePathToFilePath path env

    -- Verify the file exists
    exists <- liftIO $ doesFileExist fullPath
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

-- | Get direct references from a store path (available in both contexts)
getDirectReferences :: Database -> StorePath -> TenM p ctx (Set StorePath)
getDirectReferences db path = do
    env <- ask
    case runMode env of
        -- In privileged context, directly query the database
        DaemonMode -> do
            results <- tenQuery db
                "SELECT reference FROM References WHERE referrer = ?"
                (Only (storePathToText path))

            -- Parse each store path and return as a set
            return $ Set.fromList $ catMaybes $
                map (\(Only p) -> parseStorePath p) results

        -- In unprivileged context, request via protocol
        _ -> requestDirectReferences path

-- | Get all references from a path (direct and indirect) (available in both contexts)
getReferences :: Database -> StorePath -> TenM p ctx (Set StorePath)
getReferences db path = do
    env <- ask
    case runMode env of
        -- In privileged context, use recursive CTE
        DaemonMode -> do
            -- Use recursive CTE to efficiently query the closure
            let query = "WITH RECURSIVE\n\
                        \  closure(path) AS (\n\
                        \    VALUES(?)\n\
                        \    UNION\n\
                        \    SELECT reference FROM References, closure \n\
                        \    WHERE referrer = closure.path\n\
                        \  )\n\
                        \SELECT path FROM closure WHERE path != ?"

            results <- tenQuery db query (storePathToText path, storePathToText path)

            -- Parse each store path and return as a set
            return $ Set.fromList $ catMaybes $
                map (\(Only p) -> parseStorePath p) results

        -- In unprivileged context, request via protocol
        _ -> requestReferences path

-- | Get direct referrers to a store path (available in both contexts)
getDirectReferrers :: Database -> StorePath -> TenM p ctx (Set StorePath)
getDirectReferrers db path = do
    env <- ask
    case runMode env of
        -- In privileged context, directly query the database
        DaemonMode -> do
            results <- tenQuery db
                "SELECT referrer FROM References WHERE reference = ?"
                (Only (storePathToText path))

            -- Parse each store path and return as a set
            return $ Set.fromList $ catMaybes $
                map (\(Only p) -> parseStorePath p) results

        -- In unprivileged context, request via protocol
        _ -> requestDirectReferrers path

-- | Get all referrers to a path (direct and indirect) (available in both contexts)
getReferrers :: Database -> StorePath -> TenM p ctx (Set StorePath)
getReferrers db path = do
    env <- ask
    case runMode env of
        -- In privileged context, use recursive CTE
        DaemonMode -> do
            -- Use recursive CTE to efficiently query the closure
            let query = "WITH RECURSIVE\n\
                        \  closure(path) AS (\n\
                        \    VALUES(?)\n\
                        \    UNION\n\
                        \    SELECT referrer FROM References, closure \n\
                        \    WHERE reference = closure.path\n\
                        \  )\n\
                        \SELECT path FROM closure WHERE path != ?"

            results <- tenQuery db query (storePathToText path, storePathToText path)

            -- Parse each store path and return as a set
            return $ Set.fromList $ catMaybes $
                map (\(Only p) -> parseStorePath p) results

        -- In unprivileged context, request via protocol
        _ -> requestReferrers path

-- | Find all GC roots (privileged operation)
findGCRoots :: Database -> FilePath -> TenM p 'Privileged (Set StorePath)
findGCRoots db storeDir = do
    -- Get roots from the file system (symlinks in gc-roots directory)
    let rootsDir = storeDir </> "gc-roots"
    fsRoots <- liftIO $ getGCRootsFromFS rootsDir

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

-- | Get roots from database (privileged operation)
getRegisteredRoots :: Database -> TenM p 'Privileged (Set StorePath)
getRegisteredRoots db = do
    results <- tenQuery_ db
        "SELECT path FROM GCRoots WHERE active = 1"

    -- Parse each store path and return as a set
    return $ Set.fromList $ catMaybes $
        map (\(Only p) -> parseStorePath p) results

-- | Compute all paths reachable from a set of roots (privileged operation)
computeReachablePathsFromRoots :: Database -> Set StorePath -> TenM p 'Privileged (Set StorePath)
computeReachablePathsFromRoots db roots = do
    -- If no roots, return empty set
    if Set.null roots
        then return Set.empty
        else withTenTransaction db ReadWrite $ \_ -> do
            -- Create temp table
            tenExecuteSimple_ db
                "CREATE TEMP TABLE IF NOT EXISTS temp_roots (path TEXT PRIMARY KEY)"

            -- Clear previous roots
            tenExecuteSimple_ db "DELETE FROM temp_roots"

            -- Insert all roots
            forM_ (Set.toList roots) $ \root ->
                tenExecute_ db
                       "INSERT INTO temp_roots VALUES (?)"
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

            results <- tenQuery_ db query

            -- Parse each store path and return as a set
            return $ Set.fromList $ catMaybes $
                map (\(Only p) -> parseStorePath p) results

-- | Find the transitive closure of paths (privileged operation)
findPathsClosure :: Database -> Set StorePath -> TenM p 'Privileged (Set StorePath)
findPathsClosure db startingPaths =
    findPathsClosureWithLimit db startingPaths (-1)  -- No limit

-- | Find the transitive closure of paths with depth limit (privileged operation)
findPathsClosureWithLimit :: Database -> Set StorePath -> Int -> TenM p 'Privileged (Set StorePath)
findPathsClosureWithLimit db startingPaths depthLimit = do
    -- If no starting paths, return empty set
    if Set.null startingPaths
        then return Set.empty
        else withTenTransaction db ReadWrite $ \_ -> do
            -- Create temp table
            tenExecuteSimple_ db
                "CREATE TEMP TABLE IF NOT EXISTS temp_closure_start (path TEXT PRIMARY KEY)"

            -- Clear previous data
            tenExecuteSimple_ db "DELETE FROM temp_closure_start"

            -- Insert all starting paths
            forM_ (Set.toList startingPaths) $ \path ->
                tenExecute_ db
                          "INSERT INTO temp_closure_start VALUES (?)"
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

            results <- tenQuery_ db (Query query)

            -- Parse each store path and return as a set
            return $ Set.fromList $ catMaybes $
                map (\(Only p) -> parseStorePath p) results

-- | Check if a path is reachable from any of the GC roots (available in both contexts)
isPathReachable :: Database -> Set StorePath -> StorePath -> TenM p ctx Bool
isPathReachable db roots path = do
    env <- ask
    case runMode env of
        -- In privileged context, direct database query
        DaemonMode -> do
            -- First check if the path is itself a root
            if path `Set.member` roots
                then return True
                else do
                    -- Use SQL to efficiently check reachability
                    let rootValues = rootsToSqlValues (Set.toList roots)
                    let query = "WITH RECURSIVE\n\
                                \  reachable(path) AS (\n\
                                \    VALUES " ++ rootValues ++ "\n\
                                \    UNION\n\
                                \    SELECT reference FROM References, reachable \n\
                                \    WHERE referrer = reachable.path\n\
                                \  )\n\
                                \SELECT COUNT(*) FROM reachable WHERE path = ?"

                    results <- tenQuery db (Query $ T.pack query) (Only (storePathToText path)) :: TenM p 'Privileged [Only Int]

                    case results of
                        [Only count] -> return (count > 0)
                        _ -> return False

        -- In unprivileged context, request via protocol
        _ -> requestIsPathReachable roots path
  where
    -- Helper to create SQL values clause
    rootsToSqlValues [] = "(NULL)"  -- Avoid empty VALUES clause
    rootsToSqlValues [r] = "('" ++ T.unpack (storePathToText r) ++ "')"
    rootsToSqlValues (r:rs) = "('" ++ T.unpack (storePathToText r) ++ "'), " ++ rootsToSqlValues rs

-- | Mark a set of paths as valid in the ValidPaths table (privileged operation)
markPathsAsValid :: Database -> Set StorePath -> TenM p 'Privileged ()
markPathsAsValid db paths = withTenTransaction db ReadWrite $ \_ -> do
    forM_ (Set.toList paths) $ \path ->
        tenExecute_ db
            "UPDATE ValidPaths SET is_valid = 1 WHERE path = ?"
            (Only (storePathToText path))

-- | Mark a set of paths as invalid in the ValidPaths table (privileged operation)
markPathsAsInvalid :: Database -> Set StorePath -> TenM p 'Privileged ()
markPathsAsInvalid db paths = withTenTransaction db ReadWrite $ \_ -> do
    forM_ (Set.toList paths) $ \path ->
        tenExecute_ db
            "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
            (Only (storePathToText path))

-- | Vacuum the reference database to optimize performance (privileged operation)
vacuumReferenceDb :: Database -> TenM p 'Privileged ()
vacuumReferenceDb db = do
    -- Remove dangling references
    cleanupDanglingReferences db

    -- Analyze tables
    tenExecuteSimple_ db "ANALYZE References"

    -- Vacuum database
    tenExecuteSimple_ db "VACUUM"

-- | Validate and repair the reference database (privileged operation)
validateReferenceDb :: Database -> FilePath -> TenM p 'Privileged (Int, Int)
validateReferenceDb db storeDir = do
    -- Count existing references
    [Only totalRefs] <- tenQuery_ db "SELECT COUNT(*) FROM References" :: TenM p 'Privileged [Only Int]

    -- Remove references to non-existent paths
    invalid <- cleanupDanglingReferences db

    -- Return statistics
    return (totalRefs, invalid)

-- | Cleanup dangling references (privileged operation)
cleanupDanglingReferences :: Database -> TenM p 'Privileged Int
cleanupDanglingReferences db = withTenTransaction db ReadWrite $ \db' -> do
    -- Find references to paths that don't exist in ValidPaths
    dangling <- tenQuery_ db'
        "SELECT referrer, reference FROM References \
        \WHERE reference NOT IN (SELECT path FROM ValidPaths WHERE is_valid = 1)" :: TenM p 'Privileged [(Text, Text)]

    -- Delete these references
    count <- foldM (\acc (from, to) -> do
        tenExecute_ db'
            "DELETE FROM References WHERE referrer = ? AND reference = ?"
            (from, to)
        return $! acc + 1) 0 dangling

    return count

-- | Scan a file for references to store paths (available in both contexts)
scanFileForReferences :: FilePath -> FilePath -> TenM p ctx (Set StorePath)
scanFileForReferences storeDir filePath = do
    -- Check if the file exists
    exists <- liftIO $ doesFileExist filePath
    if not exists
        then return Set.empty
        else do
            -- Detect file type and use appropriate scanner
            fileType <- liftIO $ detectFileType filePath

            case fileType of
                ElfBinary -> liftIO $ scanElfBinary filePath storeDir
                TextFile -> liftIO $ scanTextFile filePath storeDir
                ScriptFile -> liftIO $ scanTextFile filePath storeDir
                BinaryFile -> liftIO $ scanBinaryFile filePath storeDir

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
    let storeRoot = T.pack storeDir
    let references = concatMap (findStoreReferences (T.unpack storeRoot)) textFragments

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
    let storeRoot = T.pack storeDir
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
    let storeRoot = T.pack storeDir
    let references = concatMap (findStoreReferences (T.unpack storeRoot)) textFragments

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

-- | Scan the entire store for references between paths (privileged operation)
scanStoreForReferences :: FilePath -> TenM p 'Privileged (Set StorePathReference)
scanStoreForReferences storeDir = do
    -- Get all store paths
    paths <- liftIO $ listStoreContents storeDir

    -- For each path, scan for references to other paths
    references <- liftIO $ foldM (\acc path -> do
        -- Get the full path in the filesystem
        let fullPath = storeDir </> T.unpack (storePathToText path)

        -- Skip if not a regular file (e.g., if it's a directory)
        isFile <- doesFileExist fullPath
        if not isFile
            then return acc
            else do
                -- Scan for references
                refs <- scanFileForReferences' storeDir fullPath

                -- Add each reference to the accumulator
                let newRefs = Set.map (StorePathReference path) refs
                return $ Set.union acc newRefs
        ) Set.empty paths

    return references
  where
    -- Internal version without TenM context
    scanFileForReferences' :: FilePath -> FilePath -> IO (Set StorePath)
    scanFileForReferences' storeDir filePath = do
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

-- | Request to register a reference (unprivileged operation)
requestRegisterReference :: StorePath -> StorePath -> TenM p 'Unprivileged ()
requestRegisterReference from to = do
    -- Build request
    let req = StoreReferenceRequest {
        referenceSource = from,
        referenceTarget = to,
        referenceType = DirectReference
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ RegisterReferenceReq req

    -- Handle response
    case resp of
        RegisterReferenceResp success -> return ()
        _ -> throwError $ DaemonError "Unexpected response from daemon when registering reference"

-- | Request to register multiple references (unprivileged operation)
requestRegisterReferences :: StorePath -> Set StorePath -> TenM p 'Unprivileged ()
requestRegisterReferences from refs = do
    -- Build request
    let req = StoreReferencesRequest {
        referencesSource = from,
        referencesTargets = refs
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ RegisterReferencesReq req

    -- Handle response
    case resp of
        RegisterReferencesResp success -> return ()
        _ -> throwError $ DaemonError "Unexpected response from daemon when registering references"

-- | Request direct references (unprivileged operation)
requestDirectReferences :: StorePath -> TenM p 'Unprivileged (Set StorePath)
requestDirectReferences path = do
    -- Build request
    let req = GetReferencesRequest {
        getReferencesPath = path,
        getReferencesDepth = Just 1  -- Direct references only
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ GetReferencesReq req

    -- Handle response
    case resp of
        GetReferencesResp paths -> return paths
        _ -> throwError $ DaemonError "Unexpected response from daemon when fetching references"

-- | Request all references (unprivileged operation)
requestReferences :: StorePath -> TenM p 'Unprivileged (Set StorePath)
requestReferences path = do
    -- Build request
    let req = GetReferencesRequest {
        getReferencesPath = path,
        getReferencesDepth = Nothing  -- All references (transitive)
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ GetReferencesReq req

    -- Handle response
    case resp of
        GetReferencesResp paths -> return paths
        _ -> throwError $ DaemonError "Unexpected response from daemon when fetching references"

-- | Request direct referrers (unprivileged operation)
requestDirectReferrers :: StorePath -> TenM p 'Unprivileged (Set StorePath)
requestDirectReferrers path = do
    -- Build request
    let req = GetReferrersRequest {
        getReferrersPath = path,
        getReferrersDepth = Just 1  -- Direct referrers only
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ GetReferrersReq req

    -- Handle response
    case resp of
        GetReferrersResp paths -> return paths
        _ -> throwError $ DaemonError "Unexpected response from daemon when fetching referrers"

-- | Request all referrers (unprivileged operation)
requestReferrers :: StorePath -> TenM p 'Unprivileged (Set StorePath)
requestReferrers path = do
    -- Build request
    let req = GetReferrersRequest {
        getReferrersPath = path,
        getReferrersDepth = Nothing  -- All referrers (transitive)
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ GetReferrersReq req

    -- Handle response
    case resp of
        GetReferrersResp paths -> return paths
        _ -> throwError $ DaemonError "Unexpected response from daemon when fetching referrers"

-- | Request computation of reachable paths (unprivileged operation)
requestComputeReachablePaths :: Set StorePath -> TenM p 'Unprivileged (Set StorePath)
requestComputeReachablePaths roots = do
    -- Build request
    let req = ComputeReachablePathsRequest {
        computeReachableRoots = roots
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ ComputeReachablePathsReq req

    -- Handle response
    case resp of
        ComputeReachablePathsResp paths -> return paths
        _ -> throwError $ DaemonError "Unexpected response from daemon when computing reachable paths"

-- | Request path closure with limit (unprivileged operation)
requestFindPathsClosure :: Set StorePath -> Maybe Int -> TenM p 'Unprivileged (Set StorePath)
requestFindPathsClosure paths depthLimit = do
    -- Build request
    let req = FindPathsClosureRequest {
        findPathsClosurePaths = paths,
        findPathsClosureDepthLimit = depthLimit
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ FindPathsClosureReq req

    -- Handle response
    case resp of
        FindPathsClosureResp resultPaths -> return resultPaths
        _ -> throwError $ DaemonError "Unexpected response from daemon when finding path closure"

-- | Request path reachability check (unprivileged operation)
requestIsPathReachable :: Set StorePath -> StorePath -> TenM p 'Unprivileged Bool
requestIsPathReachable roots path = do
    -- Build request
    let req = IsPathReachableRequest {
        isPathReachableRoots = roots,
        isPathReachablePath = path
    }

    -- Send request to daemon
    resp <- sendStoreRequest $ IsPathReachableReq req

    -- Handle response
    case resp of
        IsPathReachableResp isReachable -> return isReachable
        _ -> throwError $ DaemonError "Unexpected response from daemon when checking path reachability"

-- | Helper function to send store requests to daemon
sendStoreRequest :: StoreOperation -> TenM p 'Unprivileged StoreResponse
sendStoreRequest op = do
    -- Get daemon connection
    conn <- getDaemonConnection

    -- Create request
    let req = DaemonRequest {
        requestId = generateRequestId op,
        requestAuth = getAuthToken,
        requestOperation = StoreOp op
    }

    -- Send request and get response
    resp <- sendRequest conn req

    -- Extract store response
    case responseOperation resp of
        StoreOpResp storeResp -> return storeResp
        ErrorResp err -> throwError $ DaemonError $ "Error from daemon: " <> err
        _ -> throwError $ DaemonError "Unexpected response type from daemon"

-- | Helper function to generate a request ID
generateRequestId :: StoreOperation -> RequestId
generateRequestId = undefined  -- Implementation would depend on specific protocol details

-- | Helper function to get the current auth token
getAuthToken :: Maybe AuthToken
getAuthToken = undefined  -- Implementation would retrieve token from environment

-- | Helper function to get daemon connection
getDaemonConnection :: TenM p 'Unprivileged DaemonConnection
getDaemonConnection = undefined  -- Implementation would retrieve or establish connection
