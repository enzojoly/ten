{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

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

    -- Type classes for store operations with privilege awareness
    , StoreAccessOps(..)
    , StoreContentOps(..)
    , StoreModificationOps(..)
    , StoreQueryOps(..)
    , StoreScanOps(..)
    , GCManagementOps(..)

    -- Store initialization (daemon only)
    , initializeStore
    , createStoreDirectories
    , verifyStore
    , ensureStoreDirectories

    -- Store path operations (available to both)

    -- Content operations (available to both through appropriate tier)
    , addToStore
    , storeFile
    , storeDirectory
    , removeFromStore
    , readPathFromStore

    -- Garbage collection (daemon only)
    , collectGarbage
    , isGCRoot
    , findGCRoots
    , registerGCRoot

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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, catMaybes)
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing,
                         listDirectory, removeFile, getPermissions, Permissions,
                         readable, executable, renameFile)
import System.FilePath ((</>), takeDirectory, takeFileName, normalise)
import System.IO (withFile, IOMode(..), hPutStr, stderr, hPutStrLn, Handle, hFlush)
import System.Posix.Files (fileExist, getFileStatus, isRegularFile, setFileMode,
                          setOwnerAndGroup, fileSize, FileStatus,
                          isSymbolicLink, readSymbolicLink)
import System.Posix.User (getUserEntryForName, getGroupEntryForName, userID, groupID)
import System.Process (readCreateProcessWithExitCode, proc, CreateProcess(..))
import System.Exit (ExitCode(..))
import Crypto.Hash (hash, SHA256(..), Digest, digestFromByteString)
import qualified Crypto.Hash as Crypto
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import Data.Singletons
import Network.Socket (Socket)
import qualified Network.Socket as Network
import Database.SQLite.Simple (Connection, Query(..), Only(..), execute, execute_, query, query_)
import qualified Database.SQLite.Simple as SQL

import Ten.Core
import Ten.DB.Core (HasDirectQueryOps(..), HasTransactionOps(..),
                    withTransaction, withReadTransaction, withWriteTransaction,
                    Database, TransactionMode(..))

-- | Type class for basic store access operations
-- Available to both privilege tiers through appropriate implementations
class StoreAccessOps (t :: PrivilegeTier) where
    -- | Read content from a store path
    readStoreContent :: StorePath -> TenM p t ByteString

    -- | Check if a store path exists
    checkStorePathExists :: StorePath -> TenM p t Bool

    -- | Verify a store path's integrity
    verifyStoreIntegrity :: StorePath -> TenM p t Bool

-- | Type class for store content operations
class StoreContentOps (t :: PrivilegeTier) where
    -- | Add content to the store
    addToStore_impl :: Text -> ByteString -> TenM p t StorePath

    -- | Store a file in the store
    storeFile_impl :: FilePath -> TenM p t StorePath

    -- | Store a directory in the store
    storeDirectory_impl :: FilePath -> TenM p t StorePath

-- | Type class for store modification operations
-- Only available to Daemon tier
class (t ~ 'Daemon) => StoreModificationOps (t :: PrivilegeTier) where
    -- | Add content to the store (internal daemon implementation)
    addContentToStore :: Text -> ByteString -> TenM p t StorePath

    -- | Store a file in the store (internal daemon implementation)
    storeFileToStore :: FilePath -> TenM p t StorePath

    -- | Store a directory in the store (as a tarball)
    storeDirectoryToStore :: FilePath -> TenM p t StorePath

    -- | Remove a path from the store
    removePathFromStore :: StorePath -> TenM p t ()

-- | Type class for store query operations
-- Available to both privilege tiers through appropriate implementations
class StoreQueryOps (t :: PrivilegeTier) where
    -- | List all paths in the store
    listAllStorePaths :: TenM p t [StorePath]

    -- | Find paths with a specific prefix
    findPathsWithPrefix_ :: Text -> TenM p t [StorePath]

    -- | Get details about a store path
    getStorePathInfo :: StorePath -> TenM p t (Maybe StorePath)

-- | Type class for store scanning operations
-- Available to both privilege tiers through appropriate implementations
class StoreScanOps (t :: PrivilegeTier) where
    -- | Scan content for references to other store paths
    scanForReferences_ :: ByteString -> TenM p t (Set StorePath)

    -- | Get references to a path (what refers to this path)
    getReferencesTo :: StorePath -> TenM p t (Set StorePath)

    -- | Get references from a path (what this path refers to)
    getReferencesFrom :: StorePath -> TenM p t (Set StorePath)

-- | Type class for garbage collection operations
-- Only available to Daemon tier
class (t ~ 'Daemon) => GCManagementOps (t :: PrivilegeTier) where
    -- | Collect garbage in the store
    performGC :: TenM p t (Int, Int, Integer)

    -- | Check if a path is a GC root
    checkGCRoot :: StorePath -> TenM p t Bool

    -- | Register a path as a GC root
    registerGCRoot_ :: StorePath -> Text -> Text -> TenM p t ()

    -- | Find all GC roots
    findAllGCRoots :: TenM p t (Set StorePath)

    -- | Check if a path can be garbage collected
    canCollectPath :: StorePath -> TenM p t Bool

-- Daemon implementation of StoreAccessOps
instance StoreAccessOps 'Daemon where
    readStoreContent path = do
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

    checkStorePathExists path = do
        env <- ask
        let filePath = storePathToFilePath path env
        liftIO $ doesFileExist filePath

    verifyStoreIntegrity path = do
        -- First check if the path exists
        exists <- checkStorePathExists path
        if not exists
            then return False
            else do
                -- Read the content and verify the hash
                content <- readStoreContent path
                -- Calculate hash of the content
                let contentHashDigest = hashByteString content
                let contentHash = T.pack $ show contentHashDigest
                -- Compare with the expected hash from the path
                return $ contentHash == storeHash path

-- Builder implementation of StoreAccessOps (via protocol)
instance StoreAccessOps 'Builder where
    readStoreContent path = do
        -- First try direct read - this works if the file is readable by the builder tier
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
                        requestReadViaProtocol path
            else do
                -- File doesn't exist or can't be accessed, request from daemon
                requestReadViaProtocol path

    checkStorePathExists path = do
        -- Try direct check first
        env <- ask
        let filePath = storePathToFilePath path env
        directExists <- liftIO $ doesFileExist filePath
        if directExists
            then return True
            else do
                -- If direct check fails, go through the daemon
                conn <- getDaemonConnection
                let req = Request {
                        reqId = 0,
                        reqType = "store-verify",
                        reqParams = Map.singleton "path" (storePathToText path),
                        reqPayload = Nothing
                    }
                response <- liftIO $ sendRequestSync conn req 10000000
                case response of
                    Left _ -> return False
                    Right (StoreVerifyResponse exists) -> return exists
                    Right _ -> return False

    verifyStoreIntegrity path = do
        -- Try to verify directly first
        env <- ask
        let filePath = storePathToFilePath path env
        fileExists <- liftIO $ doesFileExist filePath
        if fileExists
            then do
                -- Try to read and verify directly
                contentResult <- liftIO $ try $ BS.readFile filePath
                case contentResult of
                    Right content -> do
                        let contentHashDigest = hashByteString content
                        let contentHash = T.pack $ show contentHashDigest
                        return $ contentHash == storeHash path
                    Left (_ :: IOException) -> requestVerifyViaProtocol path
            else requestVerifyViaProtocol path

-- Daemon implementation of StoreModificationOps
instance StoreModificationOps 'Daemon where
    addContentToStore nameHint content = do
        env <- ask
        let storeDir = storeLocation env

        -- Calculate hash of content
        let contentHashDigest = hashByteString content
        let contentHash = T.pack $ show contentHashDigest

        -- Sanitize name hint
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
                    then do
                        -- Path exists with correct content, register it
                        registerValidPath path Nothing
                        return path
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

                -- Register as valid path in database
                registerValidPath path Nothing

                -- Return the store path
                return path

    storeFileToStore filePath = do
        -- Check if file exists
        exists <- liftIO $ doesFileExist filePath
        unless exists $
            throwError $ InputNotFound filePath

        -- Read file content
        content <- liftIO $ BS.readFile filePath

        -- Use file name as hint
        let nameHint = T.pack $ takeFileName filePath

        -- Add to store
        addContentToStore nameHint content

    storeDirectoryToStore dirPath = do
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
                addContentToStore (T.pack $ takeFileName dirPath ++ ".tar.gz") content

            ExitFailure code ->
                throwError $ StoreError $ "Failed to create tarball: " <> T.pack stderr

    removePathFromStore path = do
        env <- ask
        let filePath = storePathToFilePath path env

        -- Check if path exists
        exists <- liftIO $ doesFileExist filePath
        unless exists $
            throwError $ StoreError $ "Path does not exist: " <> storePathToText path

        -- Check if path has any referrers
        refs <- getReferencesTo path
        unless (Set.null refs) $
            throwError $ StoreError $ "Cannot remove path with referrers: " <> storePathToText path

        -- Check if path is a GC root
        isRoot <- checkGCRoot path
        when isRoot $
            throwError $ StoreError $ "Cannot remove GC root: " <> storePathToText path

        -- Remove the path
        liftIO $ removeFile filePath

        -- Unregister from valid paths
        unregisterValidPath path

-- Daemon implementation of StoreQueryOps
instance StoreQueryOps 'Daemon where
    listAllStorePaths = do
        -- Get store directory
        env <- ask
        let storeDir = storeLocation env

        -- Check if directory exists
        exists <- liftIO $ doesDirectoryExist storeDir
        if not exists
            then return []
            else do
                -- Try to get paths from database first (more reliable)
                dbPaths <- getStorePathsFromDB

                if null dbPaths
                    then do
                        -- Fall back to filesystem scan if database is empty
                        entries <- liftIO $ listDirectory storeDir

                        -- Filter and parse valid store paths
                        let paths = catMaybes $ map (parseStorePath . T.pack) $
                                    filter (\e -> not $ e `elem` ["gc-roots", "tmp", "var", "locks"]) entries

                        -- Register any found paths in the database
                        forM_ paths $ \path -> registerValidPath path Nothing

                        return paths
                    else return dbPaths

    findPathsWithPrefix_ prefix = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        db <- getDatabaseConn
        -- Query for paths with prefix
        rows <- tenQuery db
            "SELECT path FROM ValidPaths WHERE name LIKE ? || '%' AND is_valid = 1"
            (Only prefix) :: TenM p 'Daemon [Only Text]

        -- Convert to StorePath objects
        return $ catMaybes $ map (\(Only t) -> parseStorePath t) rows

    getStorePathInfo path = do
        exists <- checkStorePathExists path
        if exists
            then return $ Just path
            else return Nothing

-- | Builder implementation of StoreQueryOps (via protocol)
instance StoreQueryOps 'Builder where
    listAllStorePaths = do
        conn <- getDaemonConnection
        let req = Request {
                reqId = 0,
                reqType = "store-list",
                reqParams = Map.empty,
                reqPayload = Nothing
            }
        response <- liftIO $ sendRequestSync conn req 60000000
        case response of
            Left _ -> return []
            Right (StoreListResponse paths) -> return paths
            Right _ -> return []

    findPathsWithPrefix_ prefix = do
        conn <- getDaemonConnection
        let req = Request {
                reqId = 0,
                reqType = "find-paths-with-prefix",
                reqParams = Map.singleton "prefix" prefix,
                reqPayload = Nothing
            }
        response <- liftIO $ sendRequestSync conn req 30000000
        case response of
            Left _ -> return []
            Right (StoreListResponse paths) -> return paths
            Right _ -> return []

    getStorePathInfo path = do
        exists <- checkStorePathExists path
        if exists
            then return $ Just path
            else return Nothing

-- Daemon implementation of StoreScanOps
instance StoreScanOps 'Daemon where
    scanForReferences_ content = do
        env <- ask
        let storeDir = storeLocation env

        -- Convert to text for easier scanning
        let contentText = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') content

        -- Find potential store paths
        let paths = findStorePaths contentText storeDir

        -- Verify each path exists
        foldM (\acc path -> do
            exists <- checkStorePathExists path
            if exists
                then return $ Set.insert path acc
                else return acc
            ) Set.empty paths

    getReferencesTo path = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        db <- getDatabaseConn
        -- Query for referrers
        rows <- tenQuery db
            "SELECT referrer FROM References WHERE reference = ?"
            (Only (storePathToText path)) :: TenM p 'Daemon [Only Text]

        -- Convert to StorePath objects
        let paths = catMaybes $ map (\(Only t) -> parseStorePath t) rows
        return $ Set.fromList paths

    getReferencesFrom path = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- First try to get references from database (faster)
        db <- getDatabaseConn
        rows <- tenQuery db
            "SELECT reference FROM References WHERE referrer = ?"
            (Only (storePathToText path)) :: TenM p 'Daemon [Only Text]

        let dbRefs = Set.fromList $ catMaybes $ map (\(Only t) -> parseStorePath t) rows

        if not (Set.null dbRefs)
            then return dbRefs
            else do
                -- Fall back to file scanning if database has no entries
                let filePath = storePathToFilePath path env

                -- Check file existence
                exists <- liftIO $ doesFileExist filePath
                if not exists
                    then return Set.empty
                    else do
                        -- Read file content
                        content <- liftIO $ BS.readFile filePath

                        -- Scan for references
                        refs <- scanForReferences_ content

                        -- Register references in database for future use
                        forM_ (Set.toList refs) $ \refPath ->
                            registerReference path refPath

                        return refs

-- Builder implementation of StoreScanOps (via protocol)
instance StoreScanOps 'Builder where
    scanForReferences_ content = do
        env <- ask
        let storeDir = storeLocation env

        -- Try to scan locally first
        let contentText = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') content
        let potentialPaths = findStorePaths contentText storeDir

        -- Verify each path exists through daemon
        foldM (\acc path -> do
            exists <- checkStorePathExists path
            if exists
                then return $ Set.insert path acc
                else return acc
            ) Set.empty potentialPaths

    getReferencesTo path = do
        conn <- getDaemonConnection
        let req = Request {
                reqId = 0,
                reqType = "store-referrers",
                reqParams = Map.singleton "path" (storePathToText path),
                reqPayload = Nothing
            }
        response <- liftIO $ sendRequestSync conn req 30000000
        case response of
            Left _ -> return Set.empty
            Right (DerivationOutputResponse paths) -> return paths
            Right _ -> return Set.empty

    getReferencesFrom path = do
        conn <- getDaemonConnection
        let req = Request {
                reqId = 0,
                reqType = "store-references",
                reqParams = Map.singleton "path" (storePathToText path),
                reqPayload = Nothing
            }
        response <- liftIO $ sendRequestSync conn req 30000000
        case response of
            Left _ -> return Set.empty
            Right (DerivationOutputResponse paths) -> return paths
            Right _ -> return Set.empty

-- Daemon implementation of GCManagementOps
instance GCManagementOps 'Daemon where
    performGC = do
        env <- ask
        let storeDir = storeLocation env

        -- Find all store paths
        allPaths <- listAllStorePaths

        -- Find all roots
        rootPaths <- findAllGCRoots

        -- Find reachable paths
        reachable <- computeReachablePaths rootPaths

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
            isRoot <- checkGCRoot path
            unless isRoot $ do
                -- Remove from store if no referrers
                let fullPath = storePathToFilePath path env
                liftIO $ removeFile fullPath `catch` \(_ :: IOException) -> return ()
                -- Unregister from database
                unregisterValidPath path

        -- Return stats: collected, remaining, bytes freed
        return (length unreachable, length allPaths - length unreachable, totalSize)

    checkGCRoot path = do
        env <- ask
        let storeDir = storeLocation env
        let rootsDir = storeDir </> "gc-roots"

        -- Check database first
        isDbRoot <- isGCRootInDB path

        if isDbRoot
            then return True
            else do
                -- If not in database, check filesystem
                -- List all roots
                roots <- liftIO $ listDirectory rootsDir `catch` \(_ :: IOException) -> return []

                -- Check if any root links to this path
                isFileRoot <- liftIO $ foldM (\found root -> do
                    if found
                        then return True
                        else do
                            -- Check if this root points to our path
                            let rootPath = rootsDir </> root
                            linkStatus <- try $ getFileStatus rootPath
                            let isLink = case linkStatus of
                                          Right stat -> isSymbolicLink stat
                                          Left (_ :: SomeException) -> False
                            if isLink
                                then do
                                    target <- readSymbolicLink rootPath `catch` \(_ :: IOException) -> return ""
                                    let targetPath = storePathToFilePath path env
                                    return $ targetPath == target
                                else return False
                    ) False roots

                -- If found in filesystem but not in DB, register it
                when (isFileRoot && not isDbRoot) $
                    registerGCRoot_ path (T.pack "filesystem-found") "symlink"

                return isFileRoot

    registerGCRoot_ path name rootType = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        db <- getDatabaseConn
        -- Insert or update root
        tenExecute db
            "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
            (storePathToText path, name, rootType)

    findAllGCRoots = do
        env <- ask
        let storeDir = storeLocation env
        let rootsDir = storeDir </> "gc-roots"

        -- Get roots from database
        dbRoots <- getGCRootsFromDB

        -- Check if roots directory exists
        exists <- liftIO $ doesDirectoryExist rootsDir
        if not exists
            then return dbRoots
            else do
                -- List all files in the roots directory
                files <- liftIO $ listDirectory rootsDir

                -- Process each file that is a symlink
                fsRoots <- liftIO $ foldM (\acc file -> do
                    let path = rootsDir </> file
                    -- Check if it's a symlink
                    linkStatus <- try $ getFileStatus path
                    let isLink = case linkStatus of
                                  Right stat -> isSymbolicLink stat
                                  Left (_ :: SomeException) -> False
                    if isLink
                        then do
                            -- Read the target
                            targetE <- try $ readSymbolicLink path
                            case targetE of
                                Left (_ :: IOException) -> return acc
                                Right targetPath -> do
                                    -- Convert to store path
                                    case filePathToStorePath targetPath of
                                        Just sp -> return $ Set.insert sp acc
                                        Nothing -> return acc
                        else return acc
                    ) Set.empty files

                -- Combine both sets of roots
                return $ Set.union dbRoots fsRoots

    canCollectPath path = do
        -- Check if path is a GC root
        isRoot <- checkGCRoot path
        if isRoot
            then return False
            else do
                -- Check if path has any referrers
                refs <- getReferencesTo path
                return $ Set.null refs

-- Instances for StoreContentOps
instance StoreContentOps 'Daemon where
    addToStore_impl = addContentToStore
    storeFile_impl = storeFileToStore
    storeDirectory_impl = storeDirectoryToStore

instance StoreContentOps 'Builder where
    addToStore_impl = addToStore_builder
    storeFile_impl = storeFile_builder
    storeDirectory_impl = storeDirectory_builder

-- | Add content to the store - public API function
addToStore :: forall p t. (StoreContentOps t) => Text -> ByteString -> TenM p t StorePath
addToStore = addToStore_impl

-- | Builder implementation to request content addition via protocol
addToStore_builder :: Text -> ByteString -> TenM p 'Builder StorePath
addToStore_builder nameHint content = do
    conn <- getDaemonConnection
    let req = Request {
            reqId = 0,
            reqType = "store-add",
            reqParams = Map.singleton "name" nameHint,
            reqPayload = Just content
        }
    response <- liftIO $ sendRequestSync conn req 60000000
    case response of
        Left err ->
            throwError $ StoreError $ "Failed to add to store: " <> case err of
                DaemonError m -> m
                _ -> T.pack (show err)
        Right (StoreAddResponse path) ->
            return path
        Right resp ->
            throwError $ StoreError $ "Unexpected response type: " <> T.pack (show resp)

-- | Builder implementation to request store file operation via protocol
storeFile_builder :: FilePath -> TenM p 'Builder StorePath
storeFile_builder filePath = do
    -- Check if file exists
    exists <- liftIO $ doesFileExist filePath
    unless exists $
        throwError $ InputNotFound filePath

    -- Read file content
    content <- liftIO $ BS.readFile filePath

    -- Get file name as hint
    let nameHint = T.pack $ takeFileName filePath

    -- Request store operation via protocol using the common helper
    requestAddToStore nameHint content

-- | Helper function to request store addition via protocol
requestAddToStore :: Text -> ByteString -> TenM p 'Builder StorePath
requestAddToStore nameHint content = do
    -- Get daemon connection
    conn <- getDaemonConnection

    -- Create store-add request with content
    let req = Request {
            reqId = 0,
            reqType = "store-add",
            reqParams = Map.singleton "name" nameHint,
            reqPayload = Just content
        }

    -- Send request and wait for response (larger timeout for bigger files)
    response <- liftIO $ sendRequestSync conn req 120000000  -- 2 minute timeout

    -- Handle response types
    case response of
        Left err ->
            throwError $ StoreError $ "Failed to add content to store: " <>
                case err of
                    DaemonError m -> m
                    _ -> T.pack (show err)

        Right (StoreAddResponse path) ->
            return path

        Right (ErrorResponse err) ->
            throwError err

        Right resp ->
            throwError $ StoreError $
                "Unexpected response type for store-add request: " <> T.pack (show resp)

-- | Store a file in the store
storeFile :: forall p t. (StoreContentOps t) => FilePath -> TenM p t StorePath
storeFile = storeFile_impl

-- | Store a directory in the store - Builder implementation via protocol
storeDirectory_builder :: FilePath -> TenM p 'Builder StorePath
storeDirectory_builder dirPath = do
    -- For Builder, we need to tarball locally and then send the content
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
            -- Add to store via protocol
            addToStore_builder (T.pack $ takeFileName dirPath ++ ".tar.gz") content

        ExitFailure code ->
            throwError $ StoreError $ "Failed to create tarball: " <> T.pack stderr

-- | Store a directory in the store - public API function
storeDirectory :: forall p t. (StoreContentOps t) => FilePath -> TenM p t StorePath
storeDirectory = storeDirectory_impl

-- | Remove a path from the store - restricted to Daemon tier
removeFromStore :: StorePath -> TenM p 'Daemon ()
removeFromStore = removePathFromStore

-- | Read a file path from the store
readPathFromStore :: forall p t. (StoreAccessOps t) => FilePath -> TenM p t ByteString
readPathFromStore filePath = do
    -- Convert to store path for validation
    env <- ask
    case filePathToStorePath filePath of
        Just path -> readStoreContent path
        Nothing ->
            -- Additional security check - make sure the path is within the store
            if isStoreSubPath (storeLocation env) filePath
                then liftIO $ BS.readFile filePath
                else throwError $ StoreError $ "Path is outside the store: " <> T.pack filePath

-- | Check if a path is a GC root
isGCRoot :: StorePath -> TenM p 'Daemon Bool
isGCRoot = checkGCRoot

-- | Garbage collection
collectGarbage :: TenM p 'Daemon (Int, Int, Integer)
collectGarbage = performGC

-- | Find all GC roots
findGCRoots :: TenM p 'Daemon (Set StorePath)
findGCRoots = findAllGCRoots

-- | Register a GC root
registerGCRoot :: StorePath -> Text -> Text -> TenM p 'Daemon ()
registerGCRoot = registerGCRoot_

    -- | Scan a file for references to store paths
scanFileForStoreReferences :: forall p t. (StoreScanOps t) => FilePath -> TenM p t (Set StorePath)
scanFileForStoreReferences filePath = do
    -- Check if file exists
    exists <- liftIO $ doesFileExist filePath
    if not exists
        then return Set.empty
        else do
            -- Get store location
            env <- ask
            let storeDir = storeLocation env

            -- Read file content
            contentResult <- liftIO $ try $ BS.readFile filePath
            content <- case contentResult of
                Right c -> return c
                Left (_ :: IOException) -> return BS.empty

            -- Extract store paths
            scanForReferences_ content

-- | Get references to a path (what refers to this path)
getReferencesToPath :: forall p t. (StoreScanOps t) => StorePath -> TenM p t (Set StorePath)
getReferencesToPath = getReferencesTo

-- | Get references from a path (what this path refers to)
getReferencesFromPath :: forall p t. (StoreScanOps t) => StorePath -> TenM p t (Set StorePath)
getReferencesFromPath = getReferencesFrom

-- | Find store paths with a specific prefix
findPathsWithPrefix :: forall p t. (StoreQueryOps t) => Text -> TenM p t [StorePath]
findPathsWithPrefix = findPathsWithPrefix_

-- | Get all store paths
getStorePaths :: TenM p 'Daemon [StorePath]
getStorePaths = listAllStorePaths

-- | Calculate hash for a file path
hashPath :: FilePath -> TenM p t Text
hashPath path = do
    -- Read file content
    content <- liftIO $ BS.readFile path
    -- Calculate hash
    let hashDigest = hashByteString content
    return $ T.pack $ show hashDigest

-- | Get the hash part of a store path
getPathHash :: StorePath -> Text
getPathHash = storeHash

-- | Helper function to get a database connection
getDatabaseConn :: TenM p 'Daemon (Database 'Daemon)
getDatabaseConn = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- In a real implementation, this would use a connection pool
    -- For simplicity, we're creating a new connection each time
    conn <- liftIO $ SQL.open dbPath
    liftIO $ SQL.execute_ conn "PRAGMA foreign_keys = ON"

    -- Return the database connection wrapped in our Database type
    return $ Database conn dbPath True 5000 3 sDaemon

-- | Execute a query against the database
tenExecute :: (SQL.ToRow q) => Database 'Daemon -> Query -> q -> TenM p 'Daemon ()
tenExecute db query params = do
    -- For a real implementation, this would handle retries and error cases
    liftIO $ SQL.execute (dbConn db) query params

-- | Execute a query against the database and return results
tenQuery :: (SQL.ToRow q, SQL.FromRow r) => Database 'Daemon -> Query -> q -> TenM p 'Daemon [r]
tenQuery db query params = do
    -- For a real implementation, this would handle retries and error cases
    liftIO $ SQL.query (dbConn db) query params

-- | Public API functions that use the type classes

-- | Initialize the content-addressable store
-- Requires daemon privileges
initializeStore :: FilePath -> TenM p 'Daemon ()
initializeStore storeDir = do
    -- Create the basic store structure
    liftIO $ createDirectoryIfMissing True storeDir
    liftIO $ createDirectoryIfMissing True (storeDir </> "gc-roots")
    liftIO $ createDirectoryIfMissing True (storeDir </> "var/ten")

    -- Create lock directory for GC
    storeEnv <- ask
    liftIO $ createDirectoryIfMissing True (takeDirectory $ getGCLockPath storeEnv)

    -- Set appropriate permissions
    -- Store root: read-execute for all, write for owner only
    liftIO $ setFileMode storeDir 0o755

    -- GC roots: read-write-execute for owner, read-execute for others
    liftIO $ setFileMode (storeDir </> "gc-roots") 0o755

    -- Initialize SQLite database for path tracking
    initializeDatabase (defaultDBPath storeDir)

    -- Verify the store
    verifyStore storeDir

-- | Create store directories with proper permissions
-- Requires daemon privileges
createStoreDirectories :: FilePath -> TenM p 'Daemon ()
createStoreDirectories storeDir = do
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
ensureStoreDirectories :: FilePath -> TenM p 'Daemon ()
ensureStoreDirectories = createStoreDirectories

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

    -- Verify database exists and is accessible
    let dbPath = defaultDBPath storeDir
    dbExists <- liftIO $ doesFileExist dbPath
    unless dbExists $ do
        logMsg 1 $ "Store database doesn't exist, creating at: " <> T.pack dbPath
        initializeDatabase dbPath

    -- Log successful verification
    logMsg 1 $ "Store verified: " <> T.pack storeDir

-- | Initialize the store database
initializeDatabase :: FilePath -> TenM p 'Daemon ()
initializeDatabase dbPath = do
    -- Create the directory if it doesn't exist
    liftIO $ createDirectoryIfMissing True (takeDirectory dbPath)

    -- Open database connection
    conn <- liftIO $ SQL.open dbPath

    -- Create schema if it doesn't exist
    liftIO $ do
        -- Create ValidPaths table for tracking store paths
        SQL.execute_ conn "CREATE TABLE IF NOT EXISTS ValidPaths (\
                          \path TEXT PRIMARY KEY, \
                          \hash TEXT NOT NULL, \
                          \name TEXT NOT NULL, \
                          \deriver TEXT, \
                          \is_valid INTEGER NOT NULL DEFAULT 1, \
                          \timestamp INTEGER NOT NULL)"

        -- Create References table for tracking dependencies
        SQL.execute_ conn "CREATE TABLE IF NOT EXISTS References (\
                          \referrer TEXT NOT NULL, \
                          \reference TEXT NOT NULL, \
                          \type TEXT NOT NULL DEFAULT 'direct', \
                          \PRIMARY KEY (referrer, reference), \
                          \FOREIGN KEY (referrer) REFERENCES ValidPaths(path), \
                          \FOREIGN KEY (reference) REFERENCES ValidPaths(path))"

        -- Create GCRoots table
        SQL.execute_ conn "CREATE TABLE IF NOT EXISTS GCRoots (\
                          \path TEXT NOT NULL, \
                          \name TEXT NOT NULL, \
                          \type TEXT NOT NULL, \
                          \timestamp INTEGER NOT NULL, \
                          \active INTEGER NOT NULL DEFAULT 1, \
                          \PRIMARY KEY (path, name), \
                          \FOREIGN KEY (path) REFERENCES ValidPaths(path))"

        -- Create indexes for efficient queries
        SQL.execute_ conn "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References(referrer)"
        SQL.execute_ conn "CREATE INDEX IF NOT EXISTS idx_references_reference ON References(reference)"
        SQL.execute_ conn "CREATE INDEX IF NOT EXISTS idx_gcroots_path ON GCRoots(path) WHERE active = 1"

        -- Close connection
        SQL.close conn

-- | Check if a path is a GC root in the database
isGCRootInDB :: StorePath -> TenM p 'Daemon Bool
isGCRootInDB path = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    db <- getDatabaseConn
    -- Query for roots with explicit type annotation
    rows <- tenQuery db
        "SELECT COUNT(*) FROM GCRoots WHERE path = ? AND active = 1"
        (Only (storePathToText path)) :: TenM p 'Daemon [Only Int]

    -- Check if count > 0
    case rows of
        [Only count] -> return (count > 0)
        _ -> return False

-- | Get GC roots from database
getGCRootsFromDB :: TenM p 'Daemon (Set StorePath)
getGCRootsFromDB = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    db <- getDatabaseConn
    -- Query for roots
    rows <- tenQuery db
        "SELECT path FROM GCRoots WHERE active = 1"
        () :: TenM p 'Daemon [Only Text]

    -- Convert to StorePath objects
    let paths = catMaybes $ map (\(Only t) -> parseStorePath t) rows
    return $ Set.fromList paths

-- | Compute all paths reachable from a set of roots
computeReachablePaths :: Set StorePath -> TenM p 'Daemon (Set StorePath)
computeReachablePaths roots = do
    -- Include the roots themselves
    let reachable = roots

    -- For each root, add its references recursively
    foldM (\reached root -> do
        refs <- getReferencesFrom root
        let newReached = Set.union reached (Set.insert root refs)
        if Set.size newReached > Set.size reached
            then computeReachablePaths newReached  -- Continue if we found new paths
            else return newReached                 -- Stop if no new paths found
        ) reachable (Set.toList roots)

-- | Register a reference between paths
registerReference :: StorePath -> StorePath -> TenM p 'Daemon ()
registerReference referrer reference = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    db <- getDatabaseConn
    -- Insert reference (ignore if already exists)
    tenExecute db
        "INSERT OR IGNORE INTO References (referrer, reference, type) VALUES (?, ?, 'direct')"
        (storePathToText referrer, storePathToText reference)

-- | Register a valid path in the database
-- This is a daemon-only operation
registerValidPath :: StorePath -> Maybe StorePath -> TenM p 'Daemon ()
registerValidPath path mDeriver = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- First check if path exists
    let fullPath = storePathToFilePath path env
    exists <- liftIO $ doesFileExist fullPath
    unless exists $
        throwError $ StoreError $ "Cannot register non-existent path: " <> storePathToText path

    db <- getDatabaseConn
    -- Insert path into ValidPaths
    tenExecute db
        "INSERT OR IGNORE INTO ValidPaths (path, hash, name, deriver, is_valid, timestamp) VALUES (?, ?, ?, ?, 1, strftime('%s','now'))"
        (storePathToText path, storeHash path, storeName path, fmap storePathToText mDeriver)

    -- If there's a deriver, register the reference
    forM_ mDeriver $ \deriver ->
        tenExecute db
            "INSERT OR IGNORE INTO References (referrer, reference, type) VALUES (?, ?, 'derivation')"
            (storePathToText path, storePathToText deriver)

-- | Unregister a path
-- This is a daemon-only operation
unregisterValidPath :: StorePath -> TenM p 'Daemon ()
unregisterValidPath path = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    db <- getDatabaseConn
    -- Mark path as invalid
    tenExecute db
        "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
        (Only $ storePathToText path)

-- | Get paths from database
getStorePathsFromDB :: TenM p 'Daemon [StorePath]
getStorePathsFromDB = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    db <- getDatabaseConn
    -- Query valid paths
    rows <- tenQuery db
        "SELECT path FROM ValidPaths WHERE is_valid = 1"
        () :: TenM p 'Daemon [Only Text]

    -- Convert to StorePath objects
    return $ catMaybes $ map (\(Only t) -> parseStorePath t) rows

-- | Request store content via protocol
requestReadViaProtocol :: StorePath -> TenM p 'Builder ByteString
requestReadViaProtocol path = do
    -- Get daemon connection
    conn <- getDaemonConnection

    -- Create the request
    let req = Request {
            reqId = 0,
            reqType = "store-read",
            reqParams = Map.singleton "path" (storePathToText path),
            reqPayload = Nothing
        }

    -- Send request and wait for response
    response <- liftIO $ sendRequestSync conn req 30000000

    -- Handle response
    case response of
        Left err ->
            throwError $ StoreError $ "Failed to read from store: " <> case err of
                DaemonError m -> m
                _ -> T.pack (show err)

        -- Direct pattern matching on the expected response type
        Right (StoreReadResponse content) ->
            return content

        -- Handle unexpected response types with a clear error
        Right resp ->
            throwError $ StoreError $ "Unexpected response type: " <> T.pack (show resp)

-- | Request to verify a path via protocol
requestVerifyViaProtocol :: StorePath -> TenM p 'Builder Bool
requestVerifyViaProtocol path = do
    conn <- getDaemonConnection

    -- Create the request
    let req = Request {
            reqId = 0,
            reqType = "store-verify-integrity",
            reqParams = Map.singleton "path" (storePathToText path),
            reqPayload = Nothing
        }

    -- Send request and wait for response
    response <- liftIO $ sendRequestSync conn req 10000000

    -- Handle response
    case response of
        Left _ ->
            return False  -- Any error means verification failed
        Right (StoreVerifyResponse valid) ->
            return valid
        Right _ ->
            return False

-- | Get daemon connection from environment
getDaemonConnection :: TenM p 'Builder (DaemonConnection 'Builder)
getDaemonConnection = do
    env <- ask
    case runMode env of
        ClientMode conn -> return conn
        _ -> throwError $ ConfigError "Not in client mode - no daemon connection available"

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

-- | Helper function to check if a path is within the store
isStoreSubPath :: FilePath -> FilePath -> Bool
isStoreSubPath storeDir path =
    let normalStore = normalise storeDir
        normalPath = normalise path
    in normalStore `isPrefixOf` normalPath

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
