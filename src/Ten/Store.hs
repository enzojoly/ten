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

module Ten.Store
    ( -- Store path types
      StorePath(..)
    , storePathToText
    , textToStorePath
    , parseStorePath
    , validateStorePath
    , storePathToFilePath
    , filePathToStorePath
    , makeStorePath

    -- Store reference types
    , StoreReference(..)
    , ReferenceType(..)
    , StorePathReference(..)

    -- Store initialization
    , initializeStore
    , createStoreDirectories
    , verifyStore
    , ensureStoreDirectories

    -- Store path operations
    , storePathExists
    , verifyStorePath
    , listStorePaths

    -- Daemon-only store operations
    , addToStore
    , storeFile
    , storeDirectory
    , removeFromStore

    -- Store reading (available to both tiers)
    , readFromStore
    , readPathFromStore

    -- Store protocol message types
    , StoreRequest(..)
    , StoreResponse(..)

    -- Protocol operations (for builder tier)
    , requestAddToStore
    , requestReadFromStore
    , requestVerifyPath

    -- Garbage collection support
    , collectGarbage
    , isGCRoot
    , collectGarbageCandidate
    , findPathReferences

    -- Store reference scanning
    , scanFileForStoreReferences
    , findStorePaths
    , hashPath
    , getPathHash

    -- Database operations
    , registerValidPath
    , unregisterValidPath
    , getReferencesToPath
    , getReferencesFromPath
    , findPathsWithPrefix
    , getStorePaths
    ) where

import Control.Concurrent.STM
import Control.Exception (try, throwIO, catch, bracket, finally, SomeException, IOException)
import Control.Monad (when, unless, forM_, void, forM, filterM, foldM)
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets, put, modify)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isHexDigit)
import Data.Maybe (fromMaybe, isJust, listToMaybe, catMaybes)
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing, listDirectory, removeFile)
import System.FilePath ((</>), takeDirectory, takeFileName)
import System.IO (withFile, IOMode(..), hPutStr, stderr, hPutStrLn)
import System.Posix.Files (fileExist, getFileStatus, isRegularFile, setFileMode, setOwnerAndGroup, fileSize)
import System.Posix.User (getUserEntryForName, getGroupEntryForName, userID, groupID)
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))
import System.Exit (ExitCode(..))
import Crypto.Hash (hash, SHA256(..), Digest, digestFromByteString)
import qualified Crypto.Hash as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Singletons
import Data.Singletons.TH
import Network.Socket (Socket)
import qualified Network.Socket as Network

import Ten.Core

-- | Store request message types for protocol communication
data StoreRequest
    = StoreAddRequest Text ByteString       -- Name hint and content to store
    | StoreReadRequest StorePath            -- Request to read from store
    | StoreVerifyRequest StorePath          -- Request to verify a path exists
    | StoreListRequest                      -- Request to list store paths
    | StoreDerivationRequest ByteString     -- Serialized derivation to store
    | StoreReferenceRequest StorePath       -- Request to get references of a path
    | StoreGCRequest Bool                   -- Request garbage collection (force flag)
    deriving (Show, Eq)

-- | Store response message types for protocol communication
data StoreResponse
    = StoreAddResponse StorePath            -- Path where content was stored
    | StoreReadResponse ByteString          -- Content read from store
    | StoreVerifyResponse Bool              -- Whether path exists and is valid
    | StoreListResponse [StorePath]         -- List of paths in store
    | StoreDerivationResponse StorePath     -- Path where derivation was stored
    | StoreReferenceResponse (Set StorePath) -- Set of references for a path
    | StoreGCResponse Int Int Integer       -- GC stats: collected, remaining, bytes freed
    | StoreErrorResponse Text               -- Error response
    deriving (Show, Eq)

-- | Initialize the content-addressable store
-- Requires daemon privileges
initializeStore :: SPrivilegeTier 'Daemon -> FilePath -> TenM p 'Daemon ()
initializeStore _ storeDir = do
    -- Create the basic store structure
    liftIO $ createDirectoryIfMissing True storeDir
    liftIO $ createDirectoryIfMissing True (storeDir </> "gc-roots")
    liftIO $ createDirectoryIfMissing True (storeDir </> "var/ten")

    -- Create lock directory for GC
    env <- ask
    liftIO $ createDirectoryIfMissing True (takeDirectory $ gcLockPath (storeLocation env))

    -- Set appropriate permissions
    -- Store root: read-execute for all, write for owner only
    liftIO $ setFileMode storeDir 0o755

    -- GC roots: read-write-execute for owner, read-execute for others
    liftIO $ setFileMode (storeDir </> "gc-roots") 0o755

    -- Verify the store
    verifyStore storeDir

-- | Create store directories with proper permissions
-- Requires daemon privileges
createStoreDirectories :: SPrivilegeTier 'Daemon -> FilePath -> TenM p 'Daemon ()
createStoreDirectories _ storeDir = do
    -- Core directories needed for store operation
    let coreDirs = [
            storeDir,
            storeDir </> "gc-roots",
            storeDir </> "var",
            storeDir </> "var/ten",
            storeDir </> "var/ten/db",
            storeDir </> "tmp"
            ]

    -- Create each directory with proper permissions
    forM_ coreDirs $ \dir -> liftIO $ do
        createDirectoryIfMissing True dir
        setFileMode dir 0o755

-- | Ensure all store directories exist
-- Requires daemon privileges
ensureStoreDirectories :: SPrivilegeTier 'Daemon -> FilePath -> TenM p 'Daemon ()
ensureStoreDirectories st = createStoreDirectories st

-- | Verify the store structure and permissions
-- Requires daemon privileges
verifyStore :: FilePath -> TenM p 'Daemon ()
verifyStore storeDir = do
    -- Check if store directory exists
    storeExists <- liftIO $ doesDirectoryExist storeDir
    unless storeExists $
        throwError $ StoreError $ "Store directory does not exist: " <> T.pack storeDir

    -- Check permissions on store directory
    perms <- liftIO $ getPermissions storeDir
    unless (readable perms && executable perms) $
        throwError $ StoreError $ "Store directory has incorrect permissions: " <> T.pack storeDir

    -- Log successful verification
    logMsg 1 $ "Store verified: " <> T.pack storeDir

-- | Add content to the store with a name hint
-- This is a daemon-only operation
addToStore :: SPrivilegeTier 'Daemon -> Text -> ByteString -> TenM p 'Daemon StorePath
addToStore _ nameHint content = do
    env <- ask
    let storeDir = storeLocation env

    -- Calculate hash of content
    let contentHashDigest = hashByteString content
    let contentHash = T.pack $ show contentHashDigest

    -- Sanitize name hint (remove special characters, etc.)
    let name = sanitizeName nameHint

    -- Create store path
    let path = StorePath contentHash name

    -- Create full file path
    let filePath = storePathToFilePath path env

    -- Check if the path already exists
    exists <- liftIO $ doesFileExist filePath
    if exists
        then do
            -- Verify hash of existing file matches
            existingContent <- liftIO $ BS.readFile filePath
            let existingHashDigest = hashByteString existingContent
            let existingHash = T.pack $ show existingHashDigest
            if existingHash == contentHash
                then return path  -- Path exists with correct content
                else throwError $ HashError $ "Hash collision in store for: " <> T.pack filePath
        else do
            -- Create directory structure if needed
            liftIO $ createDirectoryIfMissing True (takeDirectory filePath)

            -- Write content to temporary file
            let tempFile = filePath <> ".tmp"
            liftIO $ BS.writeFile tempFile content

            -- Set correct permissions (read-only for all, no execute)
            liftIO $ setFileMode tempFile 0o444

            -- Move to final location atomically
            liftIO $ renameFile tempFile filePath

            -- Register as valid path in database if available
            registerValidPath path Nothing

            -- Return the store path
            return path

-- | Store a file in the content-addressable store
-- This is a daemon-only operation
storeFile :: SPrivilegeTier 'Daemon -> FilePath -> TenM p 'Daemon StorePath
storeFile st filePath = do
    -- Check if file exists
    exists <- liftIO $ doesFileExist filePath
    unless exists $
        throwError $ InputNotFound filePath

    -- Read file content
    content <- liftIO $ BS.readFile filePath

    -- Use file name as hint
    let nameHint = T.pack $ takeFileName filePath

    -- Add to store
    addToStore st nameHint content

-- | Store a directory in the content-addressable store (as a tarball)
-- This is a daemon-only operation
storeDirectory :: SPrivilegeTier 'Daemon -> FilePath -> TenM p 'Daemon StorePath
storeDirectory st dirPath = do
    -- Check if directory exists
    exists <- liftIO $ doesDirectoryExist dirPath
    unless exists $
        throwError $ InputNotFound dirPath

    -- Create a temporary tarball
    env <- ask
    let tempDir = workDir env </> "tmp"
    liftIO $ createDirectoryIfMissing True tempDir

    let tarballPath = tempDir </> (takeFileName dirPath ++ ".tar.gz")

    -- Run tar to create the tarball
    (exitCode, _, stderr) <- liftIO $
        readCreateProcessWithExitCode
            (proc "tar" ["-czf", tarballPath, "-C", takeDirectory dirPath, takeFileName dirPath])
            ""

    case exitCode of
        ExitSuccess -> do
            -- Read the tarball content
            content <- liftIO $ BS.readFile tarballPath

            -- Clean up the temporary tarball
            liftIO $ removeFile tarballPath

            -- Add to store with directory name + .tar.gz as hint
            addToStore st (T.pack $ takeFileName dirPath ++ ".tar.gz") content

        ExitFailure code ->
            throwError $ StoreError $ "Failed to create tarball: " <> T.pack stderr

-- | Check if a path exists in the store
-- Available in both daemon and builder tiers
storePathExists :: StorePath -> TenM p t Bool
storePathExists path = do
    env <- ask
    let filePath = storePathToFilePath path env
    liftIO $ doesFileExist filePath

-- | Verify that a store path exists and has the correct content hash
-- Available in both daemon and builder tiers
verifyStorePath :: StorePath -> TenM p t Bool
verifyStorePath path = do
    -- First check if the path exists
    exists <- storePathExists path

    if not exists
        then return False
        else do
            -- Read the content and verify the hash
            content <- readFromStore path

            -- Calculate hash of the content
            let contentHashDigest = hashByteString content
            let contentHash = T.pack $ show contentHashDigest

            -- Compare with the expected hash from the path
            return $ contentHash == storeHash path

-- | List all paths in the store
-- This is a daemon-only operation
listStorePaths :: SPrivilegeTier 'Daemon -> TenM p 'Daemon [StorePath]
listStorePaths _ = do
    -- Get store directory
    env <- ask
    let storeDir = storeLocation env

    -- Check if directory exists
    exists <- liftIO $ doesDirectoryExist storeDir
    if not exists
        then return []
        else do
            -- List all entries
            entries <- liftIO $ listDirectory storeDir

            -- Filter and parse valid store paths
            return $ catMaybes $ map (parseStorePath . T.pack) entries

-- | Remove a path from the store
-- This is a daemon-only operation
removeFromStore :: SPrivilegeTier 'Daemon -> StorePath -> TenM p 'Daemon ()
removeFromStore _ path = do
    env <- ask
    let filePath = storePathToFilePath path env

    -- Check if path exists
    exists <- liftIO $ doesFileExist filePath
    unless exists $
        throwError $ StoreError $ "Path does not exist: " <> storePathToText path

    -- Check if path has any referrers
    refs <- getReferencesToPath path
    unless (Set.null refs) $
        throwError $ StoreError $ "Cannot remove path with referrers: " <> storePathToText path

    -- Remove the path
    liftIO $ removeFile filePath

    -- Unregister from valid paths
    unregisterValidPath path

-- | Read content from a store path
-- Available in both daemon and builder tiers
readFromStore :: StorePath -> TenM p t ByteString
readFromStore path = do
    -- Validate the path format to prevent attacks
    unless (validateStorePath path) $
        throwError $ StoreError $ "Invalid store path format: " <> storePathToText path

    -- Get the file path
    env <- ask
    let filePath = storePathToFilePath path env

    -- Check if it exists
    exists <- liftIO $ doesFileExist filePath
    unless exists $
        throwError $ StoreError $ "Store path does not exist: " <> storePathToText path

    -- Read the content
    liftIO $ BS.readFile filePath

-- | Read a file path from the store, making sure it's within the store boundaries
-- Available in both daemon and builder tiers
readPathFromStore :: FilePath -> TenM p t ByteString
readPathFromStore filePath = do
    -- Convert to store path for validation
    env <- ask
    case filePathToStorePath filePath of
        Just path -> readFromStore path
        Nothing ->
            -- Additional security check - make sure the path is within the store
            if isStoreSubPath (storeLocation env) filePath
                then liftIO $ BS.readFile filePath
                else throwError $ StoreError $ "Path is outside the store: " <> T.pack filePath

-- Helper to check if a path is within the store
isStoreSubPath :: FilePath -> FilePath -> Bool
isStoreSubPath storeDir path =
    let normalStore = normalise storeDir
        normalPath = normalise path
    in normalStore `isPrefixOf` normalPath

-- | Check if a path is a GC root
-- This is a daemon-only operation
isGCRoot :: SPrivilegeTier 'Daemon -> StorePath -> TenM p 'Daemon Bool
isGCRoot _ path = do
    env <- ask
    let storeDir = storeLocation env
    let rootsDir = storeDir </> "gc-roots"

    -- List all roots
    roots <- liftIO $ listDirectory rootsDir `catch` \(_ :: IOException) -> return []

    -- Check if any root links to this path
    foldM (\found root -> do
        if found
            then return True
            else do
                -- Check if this root points to our path
                let rootPath = rootsDir </> root
                target <- liftIO $ readSymbolicLink rootPath `catch` \(_ :: IOException) -> return ""
                let targetPath = storePathToFilePath path env
                return $ targetPath == target
        ) False roots

-- | Collect garbage candidate check
-- This is a daemon-only operation
collectGarbageCandidate :: SPrivilegeTier 'Daemon -> StorePath -> TenM p 'Daemon Bool
collectGarbageCandidate st path = do
    -- Check if path is a GC root
    isRoot <- isGCRoot st path
    if isRoot
        then return False
        else do
            -- Check if path has any referrers
            refs <- getReferencesToPath path
            return $ Set.null refs

-- | Garbage collection
-- This is a daemon-only operation
collectGarbage :: SPrivilegeTier 'Daemon -> TenM p 'Daemon (Int, Int, Integer)
collectGarbage st = do
    env <- ask
    let storeDir = storeLocation env

    -- Find all store paths
    allPaths <- listStorePaths st

    -- Find all roots
    rootPaths <- findGCRoots st

    -- Find reachable paths
    reachable <- computeReachablePaths st rootPaths

    -- Determine unreachable paths
    let unreachable = filter (\p -> not $ p `Set.member` reachable) allPaths

    -- Get size of unreachable paths
    sizes <- forM unreachable $ \path -> do
        let fullPath = storePathToFilePath path env
        fileExists <- liftIO $ doesFileExist fullPath
        if fileExists
            then do
                stat <- liftIO $ getFileStatus fullPath
                return $ fromIntegral $ fileSize stat
            else return 0

    let totalSize = sum sizes

    -- Delete unreachable paths
    forM_ unreachable $ \path -> do
        -- Skip if the path is a GC root
        isRoot <- isGCRoot st path
        unless isRoot $ do
            -- Remove from store if no referrers
            let fullPath = storePathToFilePath path env
            liftIO $ removeFile fullPath `catch` \(_ :: IOException) -> return ()
            -- Unregister from database
            unregisterValidPath path

    -- Return stats: collected, remaining, bytes freed
    return (length unreachable, length allPaths - length unreachable, totalSize)

-- | Find all GC roots
findGCRoots :: SPrivilegeTier 'Daemon -> TenM p 'Daemon (Set StorePath)
findGCRoots _ = do
    env <- ask
    let storeDir = storeLocation env
    let rootsDir = storeDir </> "gc-roots"

    -- Check if roots directory exists
    exists <- liftIO $ doesDirectoryExist rootsDir
    if not exists
        then return Set.empty
        else do
            -- List all files in the roots directory
            files <- liftIO $ listDirectory rootsDir

            -- Process each file that is a symlink
            roots <- forM files $ \file -> do
                let path = rootsDir </> file
                -- Check if it's a symlink
                isLink <- liftIO $ isSymbolicLink <$> getFileStatus path
                if isLink
                    then do
                        -- Read the target
                        targetE <- liftIO $ try $ readSymbolicLink path
                        case targetE of
                            Left (_ :: IOException) -> return Nothing
                            Right targetPath -> do
                                -- Convert to store path
                                return $ filePathToStorePath targetPath
                    else return Nothing

            -- Return set of valid roots
            return $ Set.fromList $ catMaybes roots

-- | Compute all paths reachable from a set of roots
computeReachablePaths :: SPrivilegeTier 'Daemon -> Set StorePath -> TenM p 'Daemon (Set StorePath)
computeReachablePaths st roots = do
    -- Include the roots themselves
    let reachable = roots

    -- For each root, add its references recursively
    foldM (\reached root -> do
        refs <- findPathReferences st root
        let newReached = Set.union reached refs
        if Set.size newReached > Set.size reached
            then computeReachablePaths st newReached  -- Continue if we found new paths
            else return newReached                    -- Stop if no new paths found
        ) reachable (Set.toList roots)

-- | Find all references in a path
-- This is a daemon-only operation
findPathReferences :: SPrivilegeTier 'Daemon -> StorePath -> TenM p 'Daemon (Set StorePath)
findPathReferences _ path = do
    env <- ask
    let filePath = storePathToFilePath path env

    -- Check file existence
    exists <- liftIO $ doesFileExist filePath
    if not exists
        then return Set.empty
        else do
            -- Read file content
            content <- liftIO $ BS.readFile filePath

            -- Scan for references
            scanForReferences content

-- | Scan content for references to other store paths
-- This is a daemon-only operation
scanForReferences :: ByteString -> TenM p 'Daemon (Set StorePath)
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

-- | Register a valid path in the database
-- This is a daemon-only operation
registerValidPath :: StorePath -> Maybe StorePath -> TenM p 'Daemon ()
registerValidPath path mDeriver = do
    -- In a real implementation, this would register the path in a database
    -- For now, we'll just ensure the path exists
    env <- ask
    let fullPath = storePathToFilePath path env
    exists <- liftIO $ doesFileExist fullPath
    unless exists $
        throwError $ StoreError $ "Cannot register non-existent path: " <> storePathToText path

-- | Unregister a path
-- This is a daemon-only operation
unregisterValidPath :: StorePath -> TenM p 'Daemon ()
unregisterValidPath _ = do
    -- In a full implementation, this would unregister from a database
    return ()

-- | Get references to a path (what refers to this path)
-- Available in both daemon and builder tiers with different implementations
getReferencesToPath :: StorePath -> TenM p t (Set StorePath)
getReferencesToPath path = do
    env <- ask
    case currentPrivilegeTier env of
        Daemon -> do
            -- In daemon context, query the database directly
            -- For now, return empty set as a placeholder
            return Set.empty

        Builder -> do
            -- In builder context, request via daemon protocol
            requestReferrersToPath path

-- | Get references from a path (what this path refers to)
-- Available in both daemon and builder tiers with different implementations
getReferencesFromPath :: StorePath -> TenM p t (Set StorePath)
getReferencesFromPath path = do
    env <- ask
    case currentPrivilegeTier env of
        Daemon -> do
            -- In daemon context, query the database directly
            -- For now, scan the file directly
            let st = error "This is only used to make the type checker happy"
            findPathReferences st path

        Builder -> do
            -- In builder context, request via daemon protocol
            requestReferencesFromPath path

-- | Find store paths with a specific prefix
-- This is a daemon-only operation
findPathsWithPrefix :: SPrivilegeTier 'Daemon -> Text -> TenM p 'Daemon [StorePath]
findPathsWithPrefix st prefix = do
    -- List all store paths
    allPaths <- listStorePaths st

    -- Filter by prefix
    return $ filter (\path -> prefix `T.isPrefixOf` storeName path) allPaths

-- | Get all store paths
-- This is a daemon-only operation
getStorePaths :: SPrivilegeTier 'Daemon -> TenM p 'Daemon [StorePath]
getStorePaths = listStorePaths

-- | Calculate hash for a file path
-- Available in both daemon and builder tiers
hashPath :: FilePath -> TenM p t Text
hashPath path = do
    -- Read file content
    content <- liftIO $ BS.readFile path
    -- Calculate hash
    let hashDigest = hashByteString content
    return $ T.pack $ show hashDigest

-- | Get the hash part of a store path
-- Available in both daemon and builder tiers
getPathHash :: StorePath -> Text
getPathHash = storeHash

-- | Request to add content to the store via daemon protocol
-- For use in builder tier
requestAddToStore :: Text -> ByteString -> TenM p 'Builder StorePath
requestAddToStore nameHint content = do
    -- Get daemon connection
    conn <- getDaemonConnection

    -- Send request to daemon
    response <- sendStoreRequest conn (StoreAddRequest nameHint content)

    -- Handle response
    case response of
        StoreAddResponse path -> return path
        StoreErrorResponse err -> throwError $ StoreError err
        _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Request to read from the store via daemon protocol
-- For use in builder tier when direct read is not possible
requestReadFromStore :: StorePath -> TenM p 'Builder ByteString
requestReadFromStore path = do
    -- First try direct read - this works if the file is readable
    -- by the builder tier
    env <- ask
    let filePath = storePathToFilePath path env
    fileExists <- liftIO $ doesFileExist filePath
    if fileExists
        then do
            -- Try to read the file directly
            result <- liftIO $ try $ BS.readFile filePath
            case result of
                Right content -> return content
                Left (_ :: IOException) -> do
                    -- If direct read fails, go through the daemon
                    conn <- getDaemonConnection
                    response <- sendStoreRequest conn (StoreReadRequest path)
                    case response of
                        StoreReadResponse content -> return content
                        StoreErrorResponse err -> throwError $ StoreError err
                        _ -> throwError $ ProtocolError "Unexpected response from daemon"
        else do
            -- File doesn't exist or can't be accessed, request from daemon
            conn <- getDaemonConnection
            response <- sendStoreRequest conn (StoreReadRequest path)
            case response of
                StoreReadResponse content -> return content
                StoreErrorResponse err -> throwError $ StoreError err
                _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Request to verify a path via daemon protocol
-- For use in builder tier
requestVerifyPath :: StorePath -> TenM p 'Builder Bool
requestVerifyPath path = do
    -- Try to check directly first
    env <- ask
    let filePath = storePathToFilePath path env
    fileExists <- liftIO $ doesFileExist filePath

    if fileExists
        then return True  -- File exists and is accessible
        else do
            -- If not accessible, ask daemon
            conn <- getDaemonConnection
            response <- sendStoreRequest conn (StoreVerifyRequest path)
            case response of
                StoreVerifyResponse result -> return result
                _ -> return False  -- Any other response means verification failed

-- | Request references from a path via protocol
requestReferencesFromPath :: StorePath -> TenM p 'Builder (Set StorePath)
requestReferencesFromPath path = do
    conn <- getDaemonConnection
    response <- sendStoreRequest conn (StoreReferenceRequest path)
    case response of
        StoreReferenceResponse refs -> return refs
        _ -> return Set.empty  -- Return empty set on error

-- | Request referrers to a path via protocol
requestReferrersToPath :: StorePath -> TenM p 'Builder (Set StorePath)
requestReferrersToPath _ = do
    -- In a real implementation, this would use a specific request type
    -- For now, return empty set
    return Set.empty

-- | Scan a file for references to store paths
-- Available in both daemon and builder tiers
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

-- | Calculate hash of ByteString content
hashByteString :: ByteString -> Digest SHA256
hashByteString = Crypto.hash

-- | Sanitize a name for use in store paths
sanitizeName :: Text -> Text
sanitizeName name =
    -- Remove any non-alphanumeric characters except allowed ones
    let filtered = T.filter isAllowedChar name
        -- Limit length
        limited = if T.length filtered > 100 then T.take 100 filtered else filtered
        -- Provide fallback if empty
        result = if T.null limited then "unknown" else limited
    in result
  where
    isAllowedChar c = isAlphaNum c || c == '-' || c == '_' || c == '.' || c == '+'

    isAlphaNum c = (c >= 'a' && c <= 'z') ||
                   (c >= 'A' && c <= 'Z') ||
                   (c >= '0' && c <= '9')

-- | Get daemon connection
getDaemonConnection :: TenM p 'Builder DaemonConnection
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ ConfigError "Not in client mode - no daemon connection available"

-- | Send a store request to the daemon
sendStoreRequest :: DaemonConnection -> StoreRequest -> TenM p 'Builder StoreResponse
sendStoreRequest conn request = do
    -- In a real implementation, this would serialize the request,
    -- send it through the socket, and receive+parse the response

    -- Create a daemon message
    let msg = DaemonMessage {
        msgId = 0,  -- Sequential message ID
        msgType = "store-request",
        msgPayload = Aeson.encode request
    }

    -- Send the message
    response <- sendToDaemon conn msg

    -- Parse the response
    case response of
        Right (DaemonMessage _ "store-response" payload) ->
            case Aeson.eitherDecode payload of
                Right storeResp -> return storeResp
                Left err -> throwError $ ProtocolError $ "Failed to parse store response: " <> T.pack err
        Right _ -> throwError $ ProtocolError "Invalid response type from daemon"
        Left err -> throwError $ DaemonError err

-- | Daemon message structure
data DaemonMessage = DaemonMessage {
    msgId :: Int,
    msgType :: Text,
    msgPayload :: LBS.ByteString
}

-- | Send message to daemon and get response
sendToDaemon :: DaemonConnection -> DaemonMessage -> TenM p 'Builder (Either Text DaemonMessage)
sendToDaemon conn msg = do
    -- This would be implemented with actual socket communication
    -- For now, return an error
    throwError $ ProtocolError "Daemon connection not implemented"

-- | Daemon connection type
data DaemonConnection = DaemonConnection {
    connSocket :: Socket,
    connUserId :: UserId,
    connAuthToken :: AuthToken
}

-- | Helper function to normalize file paths
normalise :: FilePath -> FilePath
normalise = id  -- Use System.FilePath.normalise in a real implementation

-- | Check if a path is a symbolic link
isSymbolicLink :: FileStatus -> Bool
isSymbolicLink status = not (isRegularFile status)

-- | Read symbolic link target
readSymbolicLink :: FilePath -> IO FilePath
readSymbolicLink path = do
    -- In a real implementation, this would use System.Posix.Files.readSymbolicLink
    -- For now, this is a stub
    return path

-- | Get file permissions
getPermissions :: FilePath -> IO (System.Directory.Permissions)
getPermissions path = do
    -- In a real implementation, this would convert System.Posix.Files.FileStatus
    -- to System.Directory.Permissions
    liftIO $ System.Directory.getPermissions path

-- | Check if a path is readable
readable :: System.Directory.Permissions -> Bool
readable perms = System.Directory.readable perms

-- | Check if a path is executable
executable :: System.Directory.Permissions -> Bool
executable perms = System.Directory.executable perms

-- | Rename a file (atomically if possible)
renameFile :: FilePath -> FilePath -> IO ()
renameFile old new = System.Directory.renameFile old new
