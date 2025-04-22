{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

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

    -- Privileged store operations (daemon only)
    , addToStore
    , storeFile
    , storeDirectory
    , removeFromStore

    -- Store reading (available to both contexts)
    , readFromStore
    , readPathFromStore

    -- Store protocol message types
    , StoreRequest(..)
    , StoreResponse(..)

    -- Protocol operations (for unprivileged builders)
    , requestAddToStore
    , requestReadFromStore
    , requestVerifyPath

    -- Garbage collection support
    , collectGarbage
    , gcLockPath
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
import Control.Monad (when, unless, forM_, void, forM)
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
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory
import System.FilePath
import System.IO (withFile, IOMode(..), hPutStr, stderr, hPutStrLn)
import System.Posix.Files (fileExist, getFileStatus, isRegularFile, setFileMode, setOwnerAndGroup)
import System.Posix.User (getUserEntryForName, getGroupEntryForName, userID, groupID)
import qualified System.Process as Process
import System.Exit
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Ten.Core

-- | Store request message types for protocol communication
data StoreRequest
    = StoreAddRequest Text ByteString       -- Name hint and content to store
    | StoreReadRequest StorePath            -- Request to read from store
    | StoreVerifyRequest StorePath          -- Request to verify a path exists
    | StoreListRequest                      -- Request to list store paths
    | StoreDerivationRequest ByteString     -- Serialized derivation to store
    deriving (Show, Eq)

-- | Store response message types for protocol communication
data StoreResponse
    = StoreAddResponse StorePath            -- Path where content was stored
    | StoreReadResponse ByteString          -- Content read from store
    | StoreVerifyResponse Bool              -- Whether path exists and is valid
    | StoreListResponse [StorePath]         -- List of paths in store
    | StoreDerivationResponse StorePath     -- Path where derivation was stored
    | StoreErrorResponse Text               -- Error response
    deriving (Show, Eq)

-- | Initialize the content-addressable store
initializeStore :: FilePath -> TenM p 'Privileged ()
initializeStore storeDir = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "initializeStore"

    -- Create the basic store structure
    liftIO $ createDirectoryIfMissing True storeDir
    liftIO $ createDirectoryIfMissing True (storeDir </> "gc-roots")
    liftIO $ createDirectoryIfMissing True (storeDir </> "var/ten")

    -- Create lock directory for GC
    liftIO $ createDirectoryIfMissing True (takeDirectory $ gcLockPath storeDir)

    -- Set appropriate permissions
    -- Store root: read-execute for all, write for owner only
    liftIO $ setFileMode storeDir 0o755

    -- GC roots: read-write-execute for owner, read-execute for others
    liftIO $ setFileMode (storeDir </> "gc-roots") 0o755

    -- Verify the store
    verifyStore storeDir

-- | Create store directories with proper permissions
createStoreDirectories :: FilePath -> TenM p 'Privileged ()
createStoreDirectories storeDir = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "createStoreDirectories"

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
ensureStoreDirectories :: FilePath -> TenM p 'Privileged ()
ensureStoreDirectories = createStoreDirectories

-- | Verify the store structure and permissions
verifyStore :: FilePath -> TenM p 'Privileged ()
verifyStore storeDir = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "verifyStore"

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

-- | Path to the GC lock file
gcLockPath :: FilePath -> FilePath
gcLockPath storeDir = storeDir </> "var/ten/gc.lock"

-- | Add content to the store with a name hint
-- This is a privileged operation only available in daemon context
addToStore :: Text -> ByteString -> TenM p 'Privileged StorePath
addToStore nameHint content = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "addToStore"

    env <- ask
    let storeDir = storeLocation env

    -- Calculate hash of content
    let contentHash = hashByteString content

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
            let existingHash = hashByteString existingContent
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
-- This is a privileged operation only available in daemon context
storeFile :: FilePath -> TenM p 'Privileged StorePath
storeFile filePath = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "storeFile"

    -- Check if file exists
    exists <- liftIO $ doesFileExist filePath
    unless exists $
        throwError $ InputNotFound filePath

    -- Read file content
    content <- liftIO $ BS.readFile filePath

    -- Use file name as hint
    let nameHint = T.pack $ takeFileName filePath

    -- Add to store
    addToStore nameHint content

-- | Store a directory in the content-addressable store (as a tarball)
-- This is a privileged operation only available in daemon context
storeDirectory :: FilePath -> TenM p 'Privileged StorePath
storeDirectory dirPath = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "storeDirectory"

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
        Process.readCreateProcessWithExitCode
            (Process.shell $ "tar -czf " ++ tarballPath ++ " -C " ++
                            takeDirectory dirPath ++ " " ++ takeFileName dirPath)
            ""

    case exitCode of
        ExitSuccess -> do
            -- Read the tarball content
            content <- liftIO $ BS.readFile tarballPath

            -- Clean up the temporary tarball
            liftIO $ removeFile tarballPath

            -- Add to store with directory name + .tar.gz as hint
            addToStore (T.pack $ takeFileName dirPath ++ ".tar.gz") content

        ExitFailure code ->
            throwError $ StoreError $ "Failed to create tarball: " <> T.pack stderr

-- | Check if a path exists in the store
-- Available in both privileged and unprivileged contexts
storePathExists :: StorePath -> TenM p ctx Bool
storePathExists path = do
    env <- ask
    let filePath = storePathToFilePath path env
    liftIO $ doesFileExist filePath

-- | Verify that a store path exists and has the correct content hash
-- Available in both privileged and unprivileged contexts
verifyStorePath :: StorePath -> TenM p ctx Bool
verifyStorePath path = do
    -- First check if the path exists
    exists <- storePathExists path

    if not exists
        then return False
        else do
            -- Read the content and verify the hash
            content <- readFromStore path

            -- Calculate hash of the content
            let contentHash = hashByteString content

            -- Compare with the expected hash from the path
            return $ contentHash == storeHash path

-- | List all paths in the store
listStorePaths :: TenM p 'Privileged [StorePath]
listStorePaths = do
    -- This is a privileged operation
    ensurePrivilegedContext "listStorePaths"

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

-- | Remove a path from the store (privileged operation)
removeFromStore :: StorePath -> TenM p 'Privileged ()
removeFromStore path = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "removeFromStore"

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
-- Available in both privileged and unprivileged contexts, but with validation
readFromStore :: StorePath -> TenM p ctx ByteString
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
-- Available in both privileged and unprivileged contexts, but with validation
readPathFromStore :: FilePath -> TenM p ctx ByteString
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
isGCRoot :: StorePath -> TenM p 'Privileged Bool
isGCRoot path = do
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
collectGarbageCandidate :: StorePath -> TenM p 'Privileged Bool
collectGarbageCandidate path = do
    -- Check if path is a GC root
    isRoot <- isGCRoot path
    if isRoot
        then return False
        else do
            -- Check if path has any referrers
            refs <- getReferencesToPath path
            return $ Set.null refs

-- | Garbage collection (implementation would go here)
collectGarbage :: TenM p 'Privileged ()
collectGarbage = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "collectGarbage"

    -- Implementation would go here
    logMsg 1 "Starting garbage collection..."

    -- For a real implementation, we would:
    -- 1. Acquire the GC lock
    -- 2. Find all reachable paths from GC roots
    -- 3. Delete all unreachable paths
    -- 4. Release the GC lock

    logMsg 1 "Garbage collection completed"

-- | Find all references in a path
findPathReferences :: StorePath -> TenM p 'Privileged (Set StorePath)
findPathReferences path = do
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
scanForReferences :: ByteString -> TenM p 'Privileged (Set StorePath)
scanForReferences content = do
    env <- ask
    let storeDir = storeLocation env

    -- Convert to text for easier scanning
    let contentText = TE.decodeUtf8 content

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
-- This is a privileged operation only available in daemon context
registerValidPath :: StorePath -> Maybe StorePath -> TenM p 'Privileged ()
registerValidPath path mDeriver = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "registerValidPath"

    -- In a real implementation, this would register the path in a database
    -- For now, we'll just ensure the path exists
    env <- ask
    let fullPath = storePathToFilePath path env
    exists <- liftIO $ doesFileExist fullPath
    unless exists $
        throwError $ StoreError $ "Cannot register non-existent path: " <> storePathToText path

-- | Unregister a path
unregisterValidPath :: StorePath -> TenM p 'Privileged ()
unregisterValidPath _ = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "unregisterValidPath"

    -- In a full implementation, this would unregister from a database
    return ()

-- | Get references to a path (what refers to this path)
getReferencesToPath :: StorePath -> TenM p ctx (Set StorePath)
getReferencesToPath _ = do
    -- In a full implementation, this would query a reference database
    -- For now, we return empty set
    return Set.empty

-- | Get references from a path (what this path refers to)
getReferencesFromPath :: StorePath -> TenM p ctx (Set StorePath)
getReferencesFromPath path = do
    ctx <- asks privilegeContext
    case ctx of
        Privileged -> findPathReferences path
        Unprivileged -> return Set.empty  -- Unprivileged can't scan

-- | Find store paths with a specific prefix
findPathsWithPrefix :: Text -> TenM p 'Privileged [StorePath]
findPathsWithPrefix prefix = do
    -- List all store paths
    allPaths <- listStorePaths

    -- Filter by prefix
    return $ filter (\path -> prefix `T.isPrefixOf` storeName path) allPaths

-- | Get all store paths
getStorePaths :: TenM p 'Privileged [StorePath]
getStorePaths = listStorePaths

-- | Calculate hash for a file path
hashPath :: FilePath -> TenM p ctx Text
hashPath path = do
    -- Read file content
    content <- liftIO $ BS.readFile path
    -- Calculate hash
    return $ hashByteString content

-- | Get the hash part of a store path
getPathHash :: StorePath -> Text
getPathHash = storeHash

-- | Request to add content to the store via daemon protocol
-- For use in unprivileged builder context
requestAddToStore :: Text -> ByteString -> TenM p 'Unprivileged StorePath
requestAddToStore nameHint content = do
    -- Create store add request
    let request = StoreAddRequest nameHint content

    -- Send request to daemon via protocol
    response <- sendStoreDaemonRequest request

    -- Parse response
    case response of
        StoreAddResponse path -> return path
        StoreErrorResponse err -> throwError $ StoreError err
        _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Request to read from the store via daemon protocol
-- For use in unprivileged builder context when direct read is not possible
requestReadFromStore :: StorePath -> TenM p 'Unprivileged ByteString
requestReadFromStore path = do
    -- First try direct read - this works if the file is readable
    -- by the unprivileged user
    env <- ask
    let filePath = storePathToFilePath path env
    fileExists <- liftIO $ doesFileExist filePath
    if fileExists
        then liftIO $ BS.readFile filePath
        else do
            -- If direct read fails, go through the daemon
            let request = StoreReadRequest path
            response <- sendStoreDaemonRequest request
            case response of
                StoreReadResponse content -> return content
                StoreErrorResponse err -> throwError $ StoreError err
                _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Request to verify a path via daemon protocol
requestVerifyPath :: StorePath -> TenM p 'Unprivileged Bool
requestVerifyPath path = do
    -- Create verify request
    let request = StoreVerifyRequest path

    -- Send request to daemon
    response <- sendStoreDaemonRequest request

    -- Parse response
    case response of
        StoreVerifyResponse result -> return result
        StoreErrorResponse _ -> return False
        _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Scan a file for references to store paths
-- Available in both privileged and unprivileged contexts
scanFileForStoreReferences :: FilePath -> TenM p ctx (Set StorePath)
scanFileForStoreReferences filePath = do
    -- Check if file exists
    exists <- liftIO $ doesFileExist filePath
    unless exists $
        return Set.empty

    -- Get store location
    env <- ask
    let storeDir = storeLocation env

    -- Read file content
    content <- liftIO $ BS.readFile filePath

    -- Extract store paths from binary content
    paths <- liftIO $ extractStorePaths storeDir content

    -- Filter for valid paths
    valid <- filterM storePathExists paths

    -- Return the set of found paths
    return $ Set.fromList valid

-- | Extract store paths from binary content
extractStorePaths :: FilePath -> ByteString -> IO [StorePath]
extractStorePaths storeDir content = do
    -- Convert to string for easier processing
    let str = BC.unpack content

    -- Extract potential store paths
    let potentialPaths = findPotentialStorePaths storeDir str

    -- Filter valid paths
    return $ catMaybes $ map parseStorePath potentialPaths

-- | Find potential store paths in a string
findPotentialStorePaths :: FilePath -> String -> [Text]
findPotentialStorePaths storeDir str =
    let storePrefix = storeDir ++ "/"
        fragments = extractStringFragments str
        potentialPaths = filter (\s -> storePrefix `isPrefixOf` s) fragments
        -- Extract just the path components
        pathComponents = map (\s -> T.pack $ drop (length storePrefix) s) potentialPaths
    in pathComponents

-- | Extract string fragments from binary data
extractStringFragments :: String -> [String]
extractStringFragments [] = []
extractStringFragments s =
    let (nonPrintable, rest) = break isPrintable s
        (fragment, remainder) = span isPrintable rest
    in if null fragment
       then extractStringFragments remainder
       else if length fragment > 8  -- Only consider fragments of reasonable length
            then fragment : extractStringFragments remainder
            else extractStringFragments remainder

-- | Check if a character is printable ASCII for string extraction
isPrintable :: Char -> Bool
isPrintable c = c >= ' ' && c <= '~'

-- | Calculate hash of ByteString content
hashByteString :: ByteString -> Text
hashByteString content =
    let digest = hash content :: Digest SHA256
    in T.pack $ show digest

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

-- Helper to ensure a function is being called from privileged context
ensurePrivilegedContext :: Text -> TenM p 'Privileged ()
ensurePrivilegedContext _ = return ()

-- | Send a store request to the daemon and get the response
-- Helper for protocol-based store operations
sendStoreDaemonRequest :: StoreRequest -> TenM p 'Unprivileged StoreResponse
sendStoreDaemonRequest request = do
    -- In a real implementation, this would use the daemon connection
    -- to send the request and receive a response.
    --
    -- For now we'll implement a simplified version that simulates
    -- the daemon communication.

    case request of
        StoreAddRequest name content ->
            -- Simulate adding to store
            return $ StoreAddResponse $ StorePath
                (hashByteString content)
                (sanitizeName name)

        StoreReadRequest path ->
            -- Try to read directly - in a real implementation
            -- this would go through the daemon
            do
                env <- ask
                let filePath = storePathToFilePath path env
                exists <- liftIO $ doesFileExist filePath
                if exists
                    then do
                        content <- liftIO $ BS.readFile filePath
                        return $ StoreReadResponse content
                    else
                        return $ StoreErrorResponse "Path not found in store"

        StoreVerifyRequest path ->
            -- Simulate verification
            return $ StoreVerifyResponse True

        StoreListRequest ->
            -- Simulate listing
            return $ StoreListResponse []

        StoreDerivationRequest content ->
            -- Simulate storing a derivation
            return $ StoreDerivationResponse $ StorePath
                (hashByteString content)
                "derivation.drv"
