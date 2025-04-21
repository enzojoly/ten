{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Ten.Store
    ( -- Store initialization
      initializeStore
    , createStoreDirectories
    , verifyStore

    -- Store path operations
    , storePathToFilePath
    , filePathToStorePath
    , storePathExists
    , verifyStorePath
    , validateStorePath

    -- Privileged store operations (daemon only)
    , addToStore
    , storeFile
    , storeDirectory
    , storeDerivation

    -- Store reading (available to both contexts)
    , readFromStore
    , readPathFromStore

    -- Store protocol operations (for unprivileged builders)
    , requestAddToStore
    , requestReadFromStore
    , requestStoreDerivation

    -- Garbage collection support
    , collectGarbage
    , gcLockPath

    -- Store reference scanning
    , scanFileForStoreReferences
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
import Data.Maybe (fromMaybe, isJust, listToMaybe)
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
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Ten.Core
import Ten.Hash
import Ten.Daemon.Protocol (StoreRequest(..), StoreResponse(..))

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

-- | Store a derivation in the content-addressable store
-- This is a privileged operation only available in daemon context
storeDerivation :: Derivation -> TenM p 'Privileged StorePath
storeDerivation derivation = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "storeDerivation"

    -- Serialize the derivation
    let content = serializeDerivation derivation

    -- Use derivation name as hint
    let nameHint = derivName derivation <> ".drv"

    -- Add to store
    path <- addToStore nameHint content

    -- Register in database for derivation tracking
    registerDerivation derivation path

    -- Return the store path
    return path

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
    -- Create store read request
    let request = StoreReadRequest path

    -- Send request to daemon via protocol
    response <- sendStoreDaemonRequest request

    -- Parse response
    case response of
        StoreReadResponse content -> return content
        StoreErrorResponse err -> throwError $ StoreError err
        _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Request to store a derivation via daemon protocol
-- For use in unprivileged builder context
requestStoreDerivation :: Derivation -> TenM p 'Unprivileged StorePath
requestStoreDerivation derivation = do
    -- Create store derivation request
    let request = StoreDerivationRequest (serializeDerivation derivation)

    -- Send request to daemon via protocol
    response <- sendStoreDaemonRequest request

    -- Parse response
    case response of
        StoreDerivationResponse path -> return path
        StoreErrorResponse err -> throwError $ StoreError err
        _ -> throwError $ ProtocolError "Unexpected response from daemon"

-- | Send a store request to the daemon and get the response
-- Helper for protocol-based store operations
sendStoreDaemonRequest :: StoreRequest -> TenM p 'Unprivileged StoreResponse
sendStoreDaemonRequest request = do
    -- Get daemon connection
    conn <- getDaemonConnection

    -- Send request through protocol layer
    result <- sendDaemonStoreRequest conn request

    -- Return result or convert errors
    case result of
        Left err -> throwError $ ProtocolError $ "Store request failed: " <> T.pack (show err)
        Right response -> return response

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

    -- Scan for store references
    paths <- liftIO $ extractStorePaths storeDir content

    -- Return the set of found paths
    return $ Set.fromList paths

-- | Extract store paths from binary content
extractStorePaths :: FilePath -> ByteString -> IO [StorePath]
extractStorePaths storeDir content = do
    -- Convert to string for easier processing
    let str = BC.unpack content

    -- Extract potential store paths
    let potentialPaths = findPotentialStorePaths storeDir str

    -- Filter valid paths
    return $ mapMaybe parseStorePath potentialPaths

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

-- | Register a valid path in the database
-- This is a privileged operation only available in daemon context
registerValidPath :: StorePath -> Maybe StorePath -> TenM p 'Privileged ()
registerValidPath path mDeriver = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "registerValidPath"

    -- Get database connection
    env <- ask
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Insert or update valid path record
        tenExecute_ db
            "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time, deriver, is_valid) \
            \VALUES (?, ?, strftime('%s','now'), ?, 1)"
            (storePathToText path, storeHash path, fmap storePathToText mDeriver)

-- | Register a derivation in the database
-- This is a privileged operation only available in daemon context
registerDerivation :: Derivation -> StorePath -> TenM p 'Privileged ()
registerDerivation derivation path = do
    -- Verify we're running in privileged context
    ensurePrivilegedContext "registerDerivation"

    -- Get database connection
    env <- ask
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Insert derivation record
        derivId <- tenExecute db
            "INSERT OR IGNORE INTO Derivations (hash, store_path, timestamp) \
            \VALUES (?, ?, strftime('%s','now'))"
            (derivHash derivation, storePathToText path)

        -- Register outputs
        when (derivId > 0) $ forM_ (Set.toList $ derivOutputs derivation) $ \output -> do
            tenExecute_ db
                "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) \
                \VALUES (?, ?, ?)"
                (derivId, outputName output, storePathToText (outputPath output))

            -- Register as valid path
            registerValidPath (outputPath output) (Just path)

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
ensurePrivilegedContext funcName = do
    -- This function exists as a documentation aid and for future extensions
    -- The context is enforced by the type system
    return ()

-- Helper to get daemon connection for protocol operations
getDaemonConnection :: TenM p 'Unprivileged DaemonConnection
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ ProtocolError "No daemon connection available"

-- Here we'd also implement all the proper serialization and helper functions,
-- but they're omitted here for brevity
