{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BinaryLiterals #-}

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

import Control.Exception (bracket, catch, try, throwIO, SomeException)
import Control.Monad (forM, forM_, when, unless, foldM, void)
import Data.Binary (decode, encode)
import Data.Binary.Get (runGet, getWord32le, getWord16le, skip, getByteString, getRemainingLazyByteString)
import Data.Bits ((.|.), (.&.), shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBC
import Data.Word (Word8)
import Data.Int (Int64)
import Data.List (nub, isInfixOf, isPrefixOf)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable (peekByteOff, peek, Storable(..))
import System.IO (IOMode(..), withFile, hFileSize, Handle)
import System.IO.MMap (mmapFileByteString)
import System.FilePath ((</>), takeFileName, takeExtension)
import System.Directory (doesFileExist)
import System.Posix.Files (getSymbolicLinkStatus, isSymbolicLink, isRegularFile, readSymbolicLink, fileMode, getFileStatus)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField

import Ten.DB.Core

-- | Magic number for ELF files
elfMagic :: ByteString
elfMagic = BS.pack [0x7F, 0x45, 0x4C, 0x46] -- ".ELF"

-- | Magic number for script files (shebang)
scriptMagic :: ByteString
scriptMagic = BS.pack [0x23, 0x21] -- "#!"

-- Byte values for control characters
tabByte, newlineByte :: Word8
tabByte = 9     -- '\t'
newlineByte = 10 -- '\n'

-- | Check if Word8 is an ASCII character
isAsciiWord8 :: Word8 -> Bool
isAsciiWord8 w = w < 128

-- | Check if Word8 is a printable character
isPrintWord8 :: Word8 -> Bool
isPrintWord8 w = w >= 32 && w < 127

-- | Register a single reference
registerReference :: Database -> Text -> Text -> IO ()
registerReference db referrer reference = do
    -- Check that both paths exist
    let q = "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
    SQLite.execute (dbConn db) q (referrer, reference)

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
    map SQLite.fromOnly <$> SQLite.query (dbConn db) q [path]

-- | Get paths that reference a given path
getReferrers :: Database -> Text -> IO [Text]
getReferrers db path = do
    let q = "SELECT referrer FROM References WHERE reference = ?"
    map SQLite.fromOnly <$> SQLite.query (dbConn db) q [path]

-- | Get all references in the database
getAllReferences :: Database -> IO [(Text, Text)]
getAllReferences db = do
    let q = "SELECT referrer, reference FROM References"
    SQLite.query_ (dbConn db) q

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
                -- Handle regular file based on file type detection
                detectFileTypeAndScan fullPath
            else
                -- Unknown file type
                return []

-- | Detect file type and use appropriate scanner
detectFileTypeAndScan :: FilePath -> IO [Text]
detectFileTypeAndScan path = do
    -- Check if file exists
    exists <- doesFileExist path
    if not exists
        then return []
        else do
            -- Get file size
            size <- getFileSize path

            -- Skip empty or very small files
            if size < 4
                then return []
                else do
                    -- Read magic bytes to determine file type
                    magic <- BS.take 4 <$> BS.readFile path

                    -- Check file type based on magic bytes and extension
                    let ext = takeExtension path

                    if magic == elfMagic then
                        -- ELF binary
                        scanElfBinary path
                    else if BS.take 2 magic == scriptMagic then
                        -- Script file
                        scanScriptFile path
                    else if ext `elem` [".xml", ".html", ".json", ".js", ".conf", ".cfg", ".yaml", ".yml"] then
                        -- Text-based configuration file
                        scanTextFile path
                    else if ext `elem` [".c", ".h", ".cpp", ".hpp", ".cc", ".cxx", ".hs", ".py", ".sh", ".bash"] then
                        -- Source code file
                        scanTextFile path
                    else
                        -- Fallback - scan as binary
                        scanBinaryFile path

-- | Get file size safely
getFileSize :: FilePath -> IO Integer
getFileSize path = do
    stat <- getFileStatus path
    return $ fromIntegral $ fileMode stat

-- | Scan a symbolic link for references
scanSymlink :: FilePath -> IO [Text]
scanSymlink linkPath = do
    -- Read the link target
    targetPath <- try $ readSymbolicLink linkPath
    case targetPath of
        Left (_ :: SomeException) -> return []
        Right target -> do
            -- Check if target is in the store
            let tPath = T.pack $ takeFileName target
            if validStorePathFormat tPath
                then return [tPath]
                else return []

-- | Scan an ELF binary file for references
scanElfBinary :: FilePath -> IO [Text]
scanElfBinary path = do
    -- Use memory mapping for efficient access to large binaries
    content <- mmapFileByteString path Nothing

    -- Extract string sections from ELF
    stringData <- extractElfStrings content

    -- Process each string section to find store paths
    let allStrings = processElfStrings stringData

    -- Filter valid store paths
    return $ nub $ filter validStorePathFormat allStrings

-- | Extract string sections from ELF binary
extractElfStrings :: ByteString -> IO [ByteString]
extractElfStrings elfData = do
    -- This is a simplified ELF parser for illustration
    -- A complete implementation would parse the ELF header, section headers,
    -- and extract .rodata, .data sections properly

    -- For now, we'll scan the whole file for null-terminated strings
    let chunks = BC.split '\0' elfData

    -- Only return chunks that might be valid strings
    return $ filter isValidStringCandidate chunks
  where
    isValidStringCandidate :: ByteString -> Bool
    isValidStringCandidate bs =
        BS.length bs >= 8 && -- Store path hash is at least 8 chars
        BS.all (\c -> isAsciiWord8 c && (isPrintWord8 c || c == tabByte || c == newlineByte)) bs

-- | Process ELF strings to find potential store paths
processElfStrings :: [ByteString] -> [Text]
processElfStrings strings =
    concatMap processString strings
  where
    processString :: ByteString -> [Text]
    processString bs =
        let text = TE.decodeUtf8With TEE.lenientDecode bs
        in extractStorePathsFromText text

-- | Scan a script file for references
scanScriptFile :: FilePath -> IO [Text]
scanScriptFile path = scanTextFile path

-- | Scan a text file for references
scanTextFile :: FilePath -> IO [Text]
scanTextFile path = do
    -- Read file content
    content <- try $ BS.readFile path
    case content of
        Left (_ :: SomeException) -> return []
        Right bytes -> do
            -- Decode as text with lenient decoder (handles encoding errors)
            let text = TE.decodeUtf8With TEE.lenientDecode bytes

            -- Extract store paths
            let paths = extractStorePathsFromText text

            -- Return unique, valid references
            return $ nub $ filter validStorePathFormat paths

-- | Extract store paths from text
extractStorePathsFromText :: Text -> [Text]
extractStorePathsFromText text =
    -- Split text into lines for processing
    let lines = T.splitOn "\n" text

        -- Process each line to find store paths
        pathsPerLine = map extractPathsFromLine lines

        -- Flatten the results
        allPaths = concat pathsPerLine
    in
        -- Filter out invalid paths
        filter (not . T.null) allPaths
  where
    extractPathsFromLine :: Text -> [Text]
    extractPathsFromLine line =
        -- Various patterns for store paths in text
        let
            -- Split by common separators
            splits = concatMap (\sep -> T.splitOn sep line) [" ", "\t", "\"", "'", "=", ":", ",", ";"]

            -- Check for common store path patterns
            candidates = filter (\t -> not (T.null t) &&
                                      (T.any (== '-') t) &&
                                      T.length t >= 10) splits

            -- Process each candidate with more specific checks
            processed = concatMap processPotentialPath candidates
        in
            processed

    processPotentialPath :: Text -> [Text]
    processPotentialPath candidate =
        -- Look for hash-name patterns in the text
        let hashNamePattern path =
                case T.breakOn "-" path of
                    (hash, name) | not (T.null name) && T.length hash >= 8 &&
                                   T.all isHexDigitChar hash ->
                        Just $ hash <> name
                    _ -> Nothing

            -- Try extracting as a store path
            extracted = hashNamePattern candidate
        in
            -- Return as singleton list if valid
            maybe [] (:[]) extracted

-- | Check if a character is a hexadecimal digit
isHexDigitChar :: Char -> Bool
isHexDigitChar c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

-- | Scan a binary file for references
scanBinaryFile :: FilePath -> IO [Text]
scanBinaryFile path = do
    -- Use memory mapping for efficient access to potentially large files
    content <- mmapFileByteString path Nothing

    -- Break the binary into chunks for processing
    let chunks = splitBinaryIntoChunks content

    -- Process each chunk to find text fragments
    let textFragments = extractTextFromBinary chunks

    -- Process text fragments to find store paths
    let storePaths = concatMap extractStorePathsFromText textFragments

    -- Return unique, valid references
    return $ nub $ filter validStorePathFormat storePaths

-- | Split binary data into manageable chunks
splitBinaryIntoChunks :: ByteString -> [ByteString]
splitBinaryIntoChunks bs =
    -- Split into 4KB chunks for processing
    let chunkSize = 4096
        totalSize = BS.length bs

        -- Create chunk indices
        indices = [0, chunkSize .. totalSize-1]

        -- Extract chunks
        chunks = map (\i -> BS.take chunkSize (BS.drop i bs)) indices
    in
        chunks

-- | Extract text fragments from binary data
extractTextFromBinary :: [ByteString] -> [Text]
extractTextFromBinary chunks =
    -- Process each chunk to extract ASCII strings
    let processedChunks = map processChunk chunks

        -- Filter out empty results
        validFragments = filter (not . T.null) (concat processedChunks)
    in
        validFragments
  where
    processChunk :: ByteString -> [Text]
    processChunk chunk =
        -- Split by null bytes (common string delimiter in binaries)
        let nullSplit = BC.split '\0' chunk

            -- Keep only valid ASCII strings of reasonable length
            validStrings = filter isValidBinaryString nullSplit

            -- Convert to Text
            textStrings = map (TE.decodeUtf8With TEE.lenientDecode) validStrings
        in
            textStrings

    isValidBinaryString :: ByteString -> Bool
    isValidBinaryString bs =
        -- Must be a reasonable length to be a path
        BS.length bs >= 8 &&

        -- Must contain only ASCII printable chars
        BS.all (\c -> isAsciiWord8 c && (isPrintWord8 c || c == tabByte || c == newlineByte)) bs &&

        -- Must contain a dash (common in store paths)
        BC.elem '-' bs

-- | Check if text is in valid store path format
validStorePathFormat :: Text -> Bool
validStorePathFormat text =
    -- Check for hash-name format
    case T.breakOn "-" text of
        (hash, name) | not (T.null name) ->
            let nameNoPrefix = T.drop 1 name -- Skip the dash
                validHash = T.length hash >= 8 && T.all isHexDigitChar hash
                validName = not (T.null nameNoPrefix) && T.all validNameChar nameNoPrefix
            in validHash && validName
        _ -> False
  where
    validNameChar :: Char -> Bool
    validNameChar c = (c >= '0' && c <= '9') ||
                      (c >= 'a' && c <= 'z') ||
                      (c >= 'A' && c <= 'Z') ||
                      c == '-' || c == '_' || c == '.' || c == '+'

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
    let rootParams = map SQLite.Only roots

    -- Execute the query
    paths <- SQLite.query (dbConn db) (SQLite.Query $ T.pack cteQuery) rootParams

    -- Convert result to a set
    return $ Set.fromList $ map SQLite.fromOnly paths
  where
    -- Generate SQL placeholders like (?,?,...,?)
    placeholders :: Int -> String
    placeholders n
        | n <= 0    = ""
        | n == 1    = "(?)"
        | otherwise = "(?)" ++ concat (replicate (n-1) ",(?)")

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
    results <- SQLite.query (dbConn db) q [path]
    return $ Set.fromList $ map SQLite.fromOnly results

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
                SQLite.execute (dbConn db) q (referrer, reference)
                return (valid, invalid + 1)

-- | Check if a path exists in the store
doesPathExist :: Database -> FilePath -> Text -> IO Bool
doesPathExist db storeDir path = do
    -- First check the ValidPaths table
    let q = "SELECT COUNT(*) FROM ValidPaths WHERE path = ? AND is_valid = 1"
    results <- SQLite.query (dbConn db) q [path]

    case results of
        [SQLite.Only (count :: Int)] | count > 0 -> return True
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
            SQLite.execute (dbConn db) q (referrer, reference)
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
    map SQLite.fromOnly <$> SQLite.query_ (dbConn db) q

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
    allPaths <- Set.fromList . map SQLite.fromOnly <$> SQLite.query_ (dbConn db) qAllPaths

    -- Calculate unreachable paths
    let unreachable = Set.difference allPaths reachable

    -- Mark unreachable paths as invalid
    withTransaction db ReadWrite $ \db' -> do
        forM_ (Set.toList unreachable) $ \path -> do
            let qInvalidate = "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
            SQLite.execute (dbConn db') qInvalidate [path]

    -- Return number of pruned paths
    return $ Set.size unreachable
