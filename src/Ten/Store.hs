{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ten.Store (
    -- Core store operations
    addToStore,
    ensureInStore,
    readFromStore,
    verifyStorePath,
    storePathExists,

    -- Path utilities
    storePathToFilePath,
    filePathToStorePath,
    makeStorePath,

    -- Atomic store operations
    lockStorePath,
    unlockStorePath,
    withLockedStorePath,

    -- Batch operations
    addMultipleToStore,

    -- Store management
    initializeStore,
    createStoreDirectories,
    repairStore,
    verifyStore,

    -- Store queries
    listStorePaths,
    findPathsByPrefix,

    -- Cache management
    invalidateCache,
    clearStoreCache
) where

import Control.Concurrent.STM
import Control.Exception (bracket, catch, try, SomeException)
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad.State (modify)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory
import System.FilePath
import System.IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files
import System.Posix.IO (openFd, closeFd, defaultFileFlags, OpenMode(..))
import System.Posix.Types (Fd)
import qualified System.Posix.IO.ByteString as PosixBS

import Ten.Core
import Ten.Hash

-- | Global store cache for store paths - implemented as a TVar for thread safety
{-# NOINLINE globalStoreCache #-}
globalStoreCache :: TVar (Map Text (TMVar ()))
globalStoreCache = unsafePerformIO $ atomically $ newTVar Map.empty

-- | Add content to the store, returning its StorePath
addToStore :: Text -> ByteString -> TenM p StorePath
addToStore name content = do
    env <- ask

    -- Generate the store path
    let contentHash = showHash $ hashByteString content
    let path = StorePath contentHash name
    let fullPath = storePathToFilePath path env

    -- Check if it already exists (store is immutable)
    exists <- liftIO $ doesFileExist fullPath

    unless exists $ do
        logMsg 2 $ "Adding to store: " <> contentHash <> ":" <> name

        -- Create store directory if needed
        liftIO $ createDirectoryIfMissing True (storePath env)

        -- Create temporary file
        tempPath <- liftIO $ do
            let tempDir = storePath env </> "tmp"
            createDirectoryIfMissing True tempDir
            let tempFile = tempDir </> (T.unpack contentHash ++ "-" ++ T.unpack name ++ ".tmp")
            BS.writeFile tempFile content

            -- Set file permissions (read-only)
            setFileMode tempFile $ ownerReadMode

            return tempFile

        -- Atomically move the file into place
        liftIO $ renameFile tempPath fullPath

    -- Build-specific operations
    case currentPhase <$> getBuildState of
        Just Build -> do
            -- Record this as an output in build state
            modify $ \s -> s { buildOutputs = Set.insert path (buildOutputs s) }

            -- Add the proof that we've created a valid store path
            addProof BuildProof

        _ -> return ()

    -- Add to cache
    atomicallyTen $ do
        cache <- readTVar globalStoreCache
        case Map.lookup (storeHash path) cache of
            Nothing -> do
                lock <- newTMVar ()
                writeTVar globalStoreCache (Map.insert (storeHash path) lock cache)
            Just _ -> return ()

    return path

-- | Ensure a file is in the store, adding it if necessary
ensureInStore :: Text -> FilePath -> TenM p StorePath
ensureInStore name sourceFile = do
    -- Check if source file exists
    exists <- liftIO $ doesFileExist sourceFile
    unless exists $ throwError $ InputNotFound sourceFile

    -- Read the content
    content <- liftIO $ BS.readFile sourceFile

    -- Add to store
    addToStore name content

-- | Read content from a store path
readFromStore :: StorePath -> TenM p ByteString
readFromStore path = do
    env <- ask
    let fullPath = storePathToFilePath path env

    -- Check if it exists
    exists <- liftIO $ doesFileExist fullPath
    unless exists $ throwError $ StoreError $ "Path not in store: " <> storeHash path

    -- Read the content
    liftIO $ BS.readFile fullPath

-- | Verify a store path matches its hash
verifyStorePath :: StorePath -> TenM p Bool
verifyStorePath path = do
    env <- ask
    let fullPath = storePathToFilePath path env

    -- Check file exists
    exists <- liftIO $ doesFileExist fullPath
    if not exists
        then return False
        else do
            -- Read and hash content
            content <- liftIO $ BS.readFile fullPath
            let actualHash = showHash $ hashByteString content

            -- Compare with expected hash
            return $ actualHash == storeHash path

-- | Check if a store path exists
storePathExists :: StorePath -> TenM p Bool
storePathExists path = do
    env <- ask
    liftIO $ doesFileExist $ storePathToFilePath path env

-- | Convert a store path to a filesystem path
storePathToFilePath :: StorePath -> BuildEnv -> FilePath
storePathToFilePath sp env = storePath env </> T.unpack (storeHash sp) ++ "-" ++ T.unpack (storeName sp)

-- | Try to parse a store path from a file path
filePathToStorePath :: FilePath -> Maybe StorePath
filePathToStorePath path =
    case break (== '-') (takeFileName path) of
        (hashPart, '-':namePart) ->
            Just $ StorePath (T.pack hashPart) (T.pack namePart)
        _ -> Nothing

-- | Create a store path from hash and name
makeStorePath :: Text -> Text -> StorePath
makeStorePath hash name = StorePath hash name

-- | Lock a store path to prevent concurrent modification
lockStorePath :: StorePath -> TenM p ()
lockStorePath path = do
    -- Get or create a lock for this path
    lock <- atomicallyTen $ do
        cache <- readTVar globalStoreCache
        case Map.lookup (storeHash path) cache of
            Just lock -> return lock
            Nothing -> do
                lock <- newTMVar ()
                writeTVar globalStoreCache (Map.insert (storeHash path) lock cache)
                return lock

    -- Acquire the lock (will block if already locked)
    atomicallyTen $ takeTMVar lock

-- | Unlock a store path
unlockStorePath :: StorePath -> TenM p ()
unlockStorePath path = do
    -- Release the lock if it exists
    atomicallyTen $ do
        cache <- readTVar globalStoreCache
        case Map.lookup (storeHash path) cache of
            Just lock -> do
                empty <- isEmptyTMVar lock
                unless empty $ putTMVar lock ()
            Nothing -> return ()

-- | Execute an action with a locked store path
withLockedStorePath :: StorePath -> TenM p a -> TenM p a
withLockedStorePath path action = do
    -- Lock the path, execute the action, then unlock
    bracket
        (lockStorePath path)
        (\_ -> unlockStorePath path)
        (\_ -> action)

-- | Add multiple items to the store in one operation
addMultipleToStore :: [(Text, ByteString)] -> TenM p (Set StorePath)
addMultipleToStore items = do
    paths <- forM items $ \(name, content) ->
        addToStore name content
    return $ Set.fromList paths

-- | Initialize a new store
initializeStore :: FilePath -> IO ()
initializeStore path = do
    -- Create the basic directory structure
    createStoreDirectories path

    -- Initialize the cache
    atomically $ writeTVar globalStoreCache Map.empty

    -- Set up permissions
    setFileMode path (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)

-- | Create the directory structure for a store
createStoreDirectories :: FilePath -> IO ()
createStoreDirectories path = do
    -- Create main store directory
    createDirectoryIfMissing True path

    -- Create subdirectories
    let dirs = [
            path </> "tmp",     -- For temporary files during atomic operations
            path </> "locks",   -- For lock files
            path </> "gc-roots" -- For garbage collection roots
            ]

    forM_ dirs $ \dir -> do
        createDirectoryIfMissing True dir
        -- Set appropriate permissions
        setFileMode dir (ownerReadMode .|. ownerWriteMode .|. ownerExecuteMode)

-- | Repair a possibly corrupted store
repairStore :: FilePath -> IO (Int, Int)
repairStore path = do
    -- Find all files in the store
    files <- findStoreFiles path

    -- Verify each file
    (corrupted, valid) <- foldM (\(c, v) file -> do
        let storePath = filePathToStorePath file
        case storePath of
            Nothing -> return (c + 1, v)  -- Not a valid store path format
            Just sp -> do
                content <- try $ BS.readFile file
                case content of
                    Left (_ :: SomeException) -> return (c + 1, v)
                    Right bytes -> do
                        let actualHash = showHash $ hashByteString bytes
                        if actualHash == storeHash sp
                            then return (c, v + 1)
                            else do
                                -- Corrupted file - remove it
                                removeFile file
                                return (c + 1, v)
        ) (0, 0) files

    return (corrupted, valid)

-- | Find all files in the store directory
findStoreFiles :: FilePath -> IO [FilePath]
findStoreFiles path = do
    exists <- doesDirectoryExist path
    if not exists
        then return []
        else do
            entries <- listDirectory path
            let fullPaths = map (path </>) entries
            files <- filterM doesFileExist fullPaths
            return $ filter (\f -> not $ "tmp" `isPrefixOf` takeFileName f) files

-- | Verify the entire store
verifyStore :: FilePath -> IO (Bool, Int, Int)
verifyStore path = do
    -- Find all files in the store
    files <- findStoreFiles path

    -- Verify each file
    results <- forM files $ \file -> do
        let storePath = filePathToStorePath file
        case storePath of
            Nothing -> return False  -- Not a valid store path format
            Just sp -> do
                content <- try $ BS.readFile file
                case content of
                    Left (_ :: SomeException) -> return False
                    Right bytes -> do
                        let actualHash = showHash $ hashByteString bytes
                        return $ actualHash == storeHash sp

    let valid = length $ filter id results
    let invalid = length results - valid

    return (invalid == 0, valid, invalid)

-- | List all paths in the store
listStorePaths :: FilePath -> IO [StorePath]
listStorePaths path = do
    -- Find all files in the store
    files <- findStoreFiles path

    -- Convert to StorePath objects
    return $ foldr (\file acc ->
        case filePathToStorePath file of
            Just sp -> sp : acc
            Nothing -> acc
        ) [] files

-- | Find paths matching a prefix
findPathsByPrefix :: FilePath -> Text -> IO [StorePath]
findPathsByPrefix path prefix = do
    -- List all paths
    allPaths <- listStorePaths path

    -- Filter by prefix
    return $ filter (\sp -> prefix `T.isPrefixOf` storeHash sp) allPaths

-- | Invalidate the cache for a specific path
invalidateCache :: StorePath -> TenM p ()
invalidateCache path =
    atomicallyTen $ do
        cache <- readTVar globalStoreCache
        writeTVar globalStoreCache (Map.delete (storeHash path) cache)

-- | Clear the entire store cache
clearStoreCache :: TenM p ()
clearStoreCache =
    atomicallyTen $ writeTVar globalStoreCache Map.empty

-- | Internal helper to get the current build state (for context-aware operations)
getBuildState :: TenM p (Maybe BuildState)
getBuildState = do
    -- This would be implemented to access the current state
    -- in a real implementation, but we're just returning Nothing as a stub.
    return Nothing

-- | Combine POSIX permission bits
(.|.) :: FileMode -> FileMode -> FileMode
a .|. b = a + b

-- | POSIX permission bits
ownerReadMode, ownerWriteMode, ownerExecuteMode :: FileMode
ownerReadMode = 0o400
ownerWriteMode = 0o200
ownerExecuteMode = 0o100
