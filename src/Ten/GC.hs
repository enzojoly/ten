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
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Ten.GC
    ( -- Core GC operations (daemon privileges only)
      collectGarbage
    , gcLockPath
    , acquireGCLock
    , releaseGCLock
    , withGCLock
    , breakStaleLock

    -- GC statistics
    , GCStats(..)

    -- GC roots management (daemon privileges only)
    , addRoot
    , removeRoot
    , listRoots
    , isGCRoot
    , findAllRoots

    -- Path reachability analysis (daemon privileges only)
    , findPathReferences
    , computeReachablePaths
    , findPathsWithPrefix

    -- Store verification (daemon privileges only)
    , verifyStore
    , repairStore

    -- Protocol operations (builder context)
    , requestGarbageCollection
    , requestAddRoot
    , requestRemoveRoot
    , requestListRoots
    , requestPathReachability

    -- Active build management
    , getActiveBuildPaths

    -- Lock management
    , GCLock(..)
    , GCLockInfo(..)
    ) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Exception (bracket, try, catch, throwIO, finally, mask, SomeException, IOException)
import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, modify, gets)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Time
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Set (Set)
import System.Directory (doesFileExist, removeFile)
import Data.Char (isHexDigit)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, getPermissions, removePathForcibly)
import qualified System.Directory as Dir
import System.FilePath
import System.IO.Error (isDoesNotExistError, catchIOError, isPermissionError)
import qualified System.Posix.Files as Posix
import System.IO (Handle, hPutStrLn, stderr, hFlush, hGetContents)
import System.Posix.IO (openFd, closeFd, setLock, getLock,
                        defaultFileFlags, OpenMode(..), OpenFileFlags(..),
                        fdToHandle, LockRequest(WriteLock, Unlock))
import System.Posix.Types (Fd, ProcessID, FileMode)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess)
import qualified System.Posix.IO as PosixIO
import qualified System.Posix.Files.ByteString as ByteString
import System.IO (SeekMode(AbsoluteSeek))
import qualified Data.ByteString.Char8 as BC
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Database.SQLite.Simple (Connection, Query(..), Only(..), query, query_, execute, execute_)
import qualified Database.SQLite.Simple as SQLite
import Control.Exception (try, IOException)
import Data.List (isPrefixOf, isInfixOf, sort)
import Data.Singletons
import Data.Singletons.TH
import Data.Kind (Type)
import Data.Maybe (fromMaybe, isJust, listToMaybe, catMaybes)
import Network.Socket (Socket)
import qualified Network.Socket as Network
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)

-- Import Ten.Core with explicit import list to avoid ambiguity
import Ten.Core (
    -- Core types
    TenM(..), Phase(..), PrivilegeTier(..), BuildEnv(..), BuildError(..), BuildId(..),
    StorePath(..), storeHash, storeName, storePathToText, storePathToFilePath,
    GCStats(..), GCRoot(..), rootPath, rootName, rootType, rootTime,
    Request(..), DaemonResponse(..), RootType(..), SPhase(..), SPrivilegeTier(..),
    sDaemon, sBuilder, sBuild,
    RunMode(..), daemonStatus,
    ensureLockDirExists, hashByteString, buildErrorToText,
    -- Functions
    parseStorePath, sendRequest, sendRequestSync, receiveResponse,
    currentBuildId, logMsg, defaultDBPath, filePathToStorePath, gcLockPath,
    -- Error handling
    throwError, catchError
    )

-- | DaemonResponse helper functions
isDaemonResponseOk :: DaemonResponse -> Bool
isDaemonResponseOk (ErrorResponse _) = False
isDaemonResponseOk _ = True

getDaemonResponseMessage :: DaemonResponse -> Text
getDaemonResponseMessage (ErrorResponse err) = buildErrorToText err
getDaemonResponseMessage (SuccessResponse) = "Success"
getDaemonResponseMessage _ = "Unknown response"

getGCStatsFromResponse :: DaemonResponse -> GCStats
getGCStatsFromResponse (GCResultResponse stats) = stats
getGCStatsFromResponse _ = GCStats 0 0 0 0 0.0

extractRootsFromResponse :: DaemonResponse -> [GCRoot]
extractRootsFromResponse (GCRootListResponse roots) = roots
extractRootsFromResponse _ = []

getReachabilityFromResponse :: DaemonResponse -> Bool
getReachabilityFromResponse (StatusResponse status) =
    T.pack (show $ daemonStatus status) == "true"
getReachabilityFromResponse _ = False

getDaemonResponseMetadata :: DaemonResponse -> Map Text Text
getDaemonResponseMetadata (GCResultResponse stats) =
    Map.fromList [
        ("total", T.pack $ show $ gcTotal stats),
        ("live", T.pack $ show $ gcLive stats),
        ("collected", T.pack $ show $ gcCollected stats),
        ("bytes", T.pack $ show $ gcBytes stats),
        ("elapsedTime", T.pack $ show $ gcElapsedTime stats)
    ]
getDaemonResponseMetadata _ = Map.empty

getRootCountFromResponse :: DaemonResponse -> Int
getRootCountFromResponse (GCRootListResponse roots) = length roots
getRootCountFromResponse _ = 0

getRootPathFromResponse :: DaemonResponse -> Int -> Maybe Text
getRootPathFromResponse (GCRootListResponse roots) idx =
    if idx >= 0 && idx < length roots
    then Just $ storePathToText $ rootPath $ roots !! idx
    else Nothing
getRootPathFromResponse _ _ = Nothing

getRootNameFromResponse :: DaemonResponse -> Int -> Maybe Text
getRootNameFromResponse (GCRootListResponse roots) idx =
    if idx >= 0 && idx < length roots
    then Just $ rootName $ roots !! idx
    else Nothing
getRootNameFromResponse _ _ = Nothing

getRootTypeFromResponse :: DaemonResponse -> Int -> Maybe Text
getRootTypeFromResponse (GCRootListResponse roots) idx =
    if idx >= 0 && idx < length roots
    then Just $ case rootType (roots !! idx) of
        Ten.Core.PermanentRoot -> "permanent"
        Ten.Core.ProfileRoot -> "profile"
        Ten.Core.RuntimeRoot -> "runtime"
        Ten.Core.RegistryRoot -> "registry"
        _ -> "symlink"
    else Nothing
getRootTypeFromResponse _ _ = Nothing

getRootTimeFromResponse :: DaemonResponse -> Int -> Maybe Text
getRootTimeFromResponse (GCRootListResponse roots) idx =
    if idx >= 0 && idx < length roots
    then Just $ T.pack $ show $ rootTime $ roots !! idx
    else Nothing
getRootTimeFromResponse _ _ = Nothing

-- | Database helper functions for daemon context

-- | Execute a query with parameters and return results
dbQuery :: (SQLite.ToRow q, SQLite.FromRow r) =>
           Connection -> Query -> q -> TenM 'Build 'Daemon [r]
dbQuery conn query params = liftIO $ SQLite.query conn query params

-- | Execute a parameterless query and return results
dbQuery_ :: SQLite.FromRow r =>
            Connection -> Query -> TenM 'Build 'Daemon [r]
dbQuery_ conn query = liftIO $ SQLite.query_ conn query

-- | Execute a query with parameters (no results)
dbExecute :: SQLite.ToRow q =>
             Connection -> Query -> q -> TenM 'Build 'Daemon ()
dbExecute conn query params = liftIO $ SQLite.execute conn query params

-- | Execute a parameterless query (no results)
dbExecute_ :: Connection -> Query -> TenM 'Build 'Daemon ()
dbExecute_ conn query = liftIO $ SQLite.execute_ conn query

-- | Execute a database transaction with the default mode
withTransaction :: Connection -> TenM 'Build 'Daemon a -> TenM 'Build 'Daemon a
withTransaction conn action = do
    -- Begin transaction
    liftIO $ SQLite.execute_ conn "BEGIN TRANSACTION"

    -- Run the action and handle errors
    result <- action `catchError` \e -> do
        -- Rollback on error
        liftIO $ SQLite.execute_ conn "ROLLBACK"
        throwError e

    -- Commit transaction if successful
    liftIO $ SQLite.execute_ conn "COMMIT"
    return result

-- | Execute a database transaction with a specific mode
withTransactionMode :: Connection -> Text -> TenM 'Build 'Daemon a -> TenM 'Build 'Daemon a
withTransactionMode conn mode action = do
    -- Begin transaction with specified mode
    liftIO $ SQLite.execute_ conn $ "BEGIN " <> Query mode <> " TRANSACTION"

    -- Run the action and handle errors
    result <- action `catchError` \e -> do
        -- Rollback on error
        liftIO $ SQLite.execute_ conn "ROLLBACK"
        throwError e

    -- Commit transaction if successful
    liftIO $ SQLite.execute_ conn "COMMIT"
    return result

-- | Lock information returned by daemon
data GCLockInfo = GCLockInfo
    { lockOwnerPid :: ProcessID
    , lockAcquiredTime :: UTCTime
    , lockIsStale :: Bool
    } deriving (Show, Eq)

-- | File lock structure for GC coordination
data GCLock = GCLock {
    lockFd :: Fd,              -- File descriptor for the lock file
    lockPath :: FilePath,      -- Path to the lock file
    lockPid :: ProcessID       -- Process ID holding the lock
}

-- | Global reference to hold the GC lock file descriptor
{-# NOINLINE globalGCLockFdRef #-}
globalGCLockFdRef :: IORef (Maybe (Fd, FilePath))
globalGCLockFdRef = unsafePerformIO $ newIORef Nothing

-- | Add a root to protect a path from garbage collection (daemon privilege only)
addRoot :: StorePath -> Text -> Bool -> TenM 'Build 'Daemon GCRoot
addRoot path name permanent = do
    env <- ask

    -- Verify the path exists in the store
    exists <- storePathExists path
    unless exists $
        throwError $ Ten.Core.StoreError $ "Cannot add root for non-existent path: " <> storeHash path

    -- Create the root
    now <- liftIO getCurrentTime
    let rootType = if permanent then Ten.Core.PermanentRoot else Ten.Core.SymlinkRoot
    let root = Ten.Core.GCRoot path name rootType now

    -- Write the root to the roots directory
    let rootsDir = storeLocation env </> "gc-roots"
    liftIO $ createDirectoryIfMissing True rootsDir

    -- Generate a unique filename for the root
    let rootFile = rootsDir </> (T.unpack $ storeHash path <> "-" <> name)

    -- Write a symlink to the actual path
    let targetPath = storePathToFilePath path env
    liftIO $ Posix.createSymbolicLink targetPath rootFile

    -- Also register the root in the database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db ->
        dbExecute db
            "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
            (storePathToText path, name, (if permanent then "permanent" else "user") :: Text)

    logMsg 1 $ "Added GC root: " <> name <> " -> " <> storeHash path

    return root

-- | Request to add a root from builder context via protocol
requestAddRoot :: StorePath -> Text -> Bool -> TenM 'Build 'Builder GCRoot
requestAddRoot path name permanent = do
    env <- ask

    -- Send request to daemon using protocol
    case Ten.Core.runMode env of
        ClientMode conn -> do
            -- Create a request
            let request = Ten.Core.Request {
                    Ten.Core.reqId = 0,  -- Will be set by sendRequest
                    Ten.Core.reqType = "add-gc-root",
                    Ten.Core.reqParams = Map.fromList [
                        ("path", storePathToText path),
                        ("name", name),
                        ("permanent", if permanent then "true" else "false")
                    ],
                    Ten.Core.reqPayload = Nothing
                }

            -- Send request through daemon connection
            reqId <- liftIO $ Ten.Core.sendRequest conn request
            responseResult <- liftIO $ Ten.Core.receiveResponse conn reqId 30000000 -- 30 seconds timeout

            -- Handle response
            case responseResult of
                Left err -> throwError err
                Right response ->
                    if isDaemonResponseOk response
                        then do
                            -- Construct a GC root from the response
                            now <- liftIO getCurrentTime
                            let rootType = if permanent then Ten.Core.PermanentRoot else Ten.Core.SymlinkRoot
                            return $ Ten.Core.GCRoot path name rootType now
                        else throwError $ Ten.Core.DaemonError $ getDaemonResponseMessage response

        _ -> throwError $ Ten.Core.DaemonError "Not connected to daemon"

-- | Remove a root (daemon privilege only)
removeRoot :: GCRoot -> TenM 'Build 'Daemon ()
removeRoot root = do
    env <- ask

    -- Generate the root file path
    let rootsDir = storeLocation env </> "gc-roots"
    let rootFile = rootsDir </> (T.unpack $ storeHash (Ten.Core.rootPath root) <> "-" <> Ten.Core.rootName root)

    -- Check if it exists
    exists <- liftIO $ doesFileExist rootFile
    when exists $ do
        -- Don't remove permanent roots unless forced
        unless (Ten.Core.rootType root == Ten.Core.PermanentRoot) $ do
            -- Remove from filesystem
            liftIO $ removeFile rootFile

            -- Remove from database
            withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db ->
                dbExecute db
                    "UPDATE GCRoots SET active = 0 WHERE path = ? AND name = ?"
                    (storePathToText (Ten.Core.rootPath root), Ten.Core.rootName root)

            logMsg 1 $ "Removed GC root: " <> Ten.Core.rootName root <> " -> " <> storeHash (Ten.Core.rootPath root)

-- | Request to remove a root from builder context via protocol
requestRemoveRoot :: GCRoot -> TenM 'Build 'Builder ()
requestRemoveRoot root = do
    env <- ask

    -- Send request to daemon using protocol
    case Ten.Core.runMode env of
        ClientMode conn -> do
            -- Create a request
            let request = Ten.Core.Request {
                    Ten.Core.reqId = 0,  -- Will be set by sendRequest
                    Ten.Core.reqType = "remove-gc-root",
                    Ten.Core.reqParams = Map.singleton "name" (Ten.Core.rootName root),
                    Ten.Core.reqPayload = Nothing
                }

            -- Send request through daemon connection
            reqId <- liftIO $ Ten.Core.sendRequest conn request
            responseResult <- liftIO $ Ten.Core.receiveResponse conn reqId 30000000 -- 30 seconds timeout

            -- Handle response
            case responseResult of
                Left err -> throwError err
                Right response ->
                    unless (isDaemonResponseOk response) $
                        throwError $ Ten.Core.DaemonError $ getDaemonResponseMessage response

        _ -> throwError $ Ten.Core.DaemonError "Not connected to daemon"

-- | List all current GC roots
-- Works in both contexts through different mechanisms
listRoots :: forall (t :: PrivilegeTier). SingI t => TenM 'Build t [GCRoot]
listRoots = case sing @t of
    SDaemon -> listRootsPrivileged
    SBuilder -> requestListRoots

-- | Private implementation for listing roots with filesystem access
listRootsPrivileged :: TenM 'Build 'Daemon [GCRoot]
listRootsPrivileged = do
    env <- ask

    -- Get the roots directory
    let rootsDir = storeLocation env </> "gc-roots"

    -- Create it if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir

    -- Get roots both from filesystem and database
    fsRoots <- getFileSystemRoots rootsDir

    -- Get database roots
    dbRootsResult <- withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        results <- dbQuery_ db "SELECT path, name, type, timestamp FROM GCRoots WHERE active = 1"
                :: TenM 'Build 'Daemon [(Text, Text, Text, Int)]
        return results

    -- Build database roots
    dbRoots <- liftIO $ buildDatabaseRoots dbRootsResult

    -- Combine both sources, with filesystem taking precedence for duplicates
    let allRoots = Map.union (Map.fromList [(Ten.Core.rootPath r, r) | r <- fsRoots])
                           (Map.fromList [(Ten.Core.rootPath r, r) | r <- dbRoots])

    return $ Map.elems allRoots

-- | Get file system roots
getFileSystemRoots :: FilePath -> TenM 'Build 'Daemon [GCRoot]
getFileSystemRoots rootsDir = do
    -- Create roots directory if it doesn't exist
    liftIO $ createDirectoryIfMissing True rootsDir

    -- List all files in the roots directory
    paths <- liftIO $ listDirectoryBS (BC.pack rootsDir)

    -- Parse each file into a GCRoot
    now <- liftIO getCurrentTime
    liftIO $ catMaybes <$> forM paths (\filePath -> do
        -- Read the symlink target
        let rootFile = BC.unpack filePath
        targetExists <- ByteString.fileExist filePath

        if targetExists then do
            -- Check if it's a symlink
            fileStatus <- ByteString.getFileStatus filePath `catch` \(_ :: IOException) ->
                return undefined
            let isLink = isSymbolicLink fileStatus

            if isLink
                then do
                    target <- try @IOException $ ByteString.readSymbolicLink filePath
                    case target of
                        Left (_ :: IOException) -> return Nothing
                        Right linkTarget -> do
                            let path = case Ten.Core.filePathToStorePath (BC.unpack linkTarget) of
                                    Just p -> p
                                    Nothing -> StorePath "unknown" "unknown"

                            -- Parse name from file
                            let fileName = takeFileName (BC.unpack filePath)
                            let name = case break (== '-') fileName of
                                    (_, '-':rest) -> T.pack rest
                                    _ -> T.pack fileName

                            -- Determine root type based on file name pattern
                            let rootType = if "permanent-" `T.isPrefixOf` name
                                          then Ten.Core.PermanentRoot
                                          else Ten.Core.SymlinkRoot

                            return $ Just $ Ten.Core.GCRoot
                                { Ten.Core.rootPath = path
                                , Ten.Core.rootName = name
                                , Ten.Core.rootType = rootType
                                , Ten.Core.rootTime = now
                                }
                else return Nothing
        else return Nothing)
  where
    isSymbolicLink fileStatus = ByteString.isSymbolicLink fileStatus

    -- ByteString version of listDirectory
    listDirectoryBS :: ByteString -> IO [ByteString]
    listDirectoryBS dir = do
        -- Convert to FilePath for standard directory listing
        let dirPath = BC.unpack dir
        fileNames <- Dir.listDirectory dirPath
        -- Convert back to ByteString
        return $ map (BC.pack . (dirPath </>)) fileNames

-- | Request to list roots from builder context via protocol
requestListRoots :: TenM 'Build 'Builder [GCRoot]
requestListRoots = do
    env <- ask

    -- Send request to daemon using protocol
    case Ten.Core.runMode env of
        ClientMode conn -> do
            -- Create a request
            let request = Ten.Core.Request {
                    Ten.Core.reqId = 0,  -- Will be set by sendRequest
                    Ten.Core.reqType = "list-gc-roots",
                    Ten.Core.reqParams = Map.empty,
                    Ten.Core.reqPayload = Nothing
                }

            -- Send request through daemon connection
            reqId <- liftIO $ Ten.Core.sendRequest conn request
            responseResult <- liftIO $ Ten.Core.receiveResponse conn reqId 30000000 -- 30 seconds timeout

            -- Handle response
            case responseResult of
                Left err -> throwError err
                Right response ->
                    if isDaemonResponseOk response
                        then do
                            -- Get roots directly from response
                            return $ extractRootsFromResponse response
                        else throwError $ Ten.Core.DaemonError $ getDaemonResponseMessage response

        _ -> throwError $ Ten.Core.DaemonError "Not connected to daemon"

-- | Build database roots from query results
buildDatabaseRoots :: [(Text, Text, Text, Int)] -> IO [GCRoot]
buildDatabaseRoots results = do
    now <- getCurrentTime
    forM results $ \(pathText, name, typeStr, timestamp) -> do
        case Ten.Core.parseStorePath pathText of
            Just path -> do
                -- Convert timestamp to UTCTime
                let rootTime = posixSecondsToUTCTime (fromIntegral timestamp)
                -- Determine root type
                let rootType = case typeStr of
                      "permanent" -> Ten.Core.PermanentRoot
                      "runtime" -> Ten.Core.RuntimeRoot
                      "profile" -> Ten.Core.ProfileRoot
                      "registry" -> Ten.Core.RegistryRoot
                      _ -> Ten.Core.SymlinkRoot

                return $ Ten.Core.GCRoot
                    { Ten.Core.rootPath = path
                    , Ten.Core.rootName = name
                    , Ten.Core.rootType = rootType
                    , Ten.Core.rootTime = rootTime
                    }
            Nothing -> throwIO $ userError $ "Invalid store path in database: " ++ T.unpack pathText

-- | Run garbage collection (daemon privilege only)
collectGarbage :: TenM 'Build 'Daemon GCStats
collectGarbage = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Acquire the GC lock first
    withGCLock $ collectGarbageWithStats

-- | Request garbage collection from builder context via protocol
requestGarbageCollection :: Bool -> TenM 'Build 'Builder GCStats
requestGarbageCollection force = do
    env <- ask

    -- Send request to daemon using protocol
    case Ten.Core.runMode env of
        ClientMode conn -> do
            -- Create a request
            let request = Ten.Core.Request {
                    Ten.Core.reqId = 0,  -- Will be set by sendRequest
                    Ten.Core.reqType = "gc",
                    Ten.Core.reqParams = Map.singleton "force" (if force then "true" else "false"),
                    Ten.Core.reqPayload = Nothing
                }

            -- Send request through daemon connection
            reqId <- liftIO $ Ten.Core.sendRequest conn request
            responseResult <- liftIO $ Ten.Core.receiveResponse conn reqId 300000000 -- 5 minutes timeout

            -- Handle response
            case responseResult of
                Left err -> throwError err
                Right response ->
                    if isDaemonResponseOk response
                        then return $ getGCStatsFromResponse response
                        else throwError $ Ten.Core.DaemonError $ getDaemonResponseMessage response

        _ -> throwError $ Ten.Core.DaemonError "Not connected to daemon"

-- | Run garbage collection with stats (internal implementation)
collectGarbageWithStats :: TenM 'Build 'Daemon GCStats
collectGarbageWithStats = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database connection
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Find all paths in the store
        let storeLocationPath = storeLocation env
        allStorePaths <- liftIO $ findAllStorePaths db storeLocationPath

        -- Find all root paths
        rootPaths <- liftIO $ findAllRoots db storeLocationPath

        -- Log information about roots
        logMsg 2 $ "Found " <> T.pack (show (Set.size rootPaths)) <> " GC roots"

        -- Find all paths reachable from roots (transitive closure)
        reachablePaths <- liftIO $ computeReachablePathsFromRoots db rootPaths

        -- Log information about reachable paths
        logMsg 2 $ "Found " <> T.pack (show (Set.size reachablePaths)) <> " reachable paths"

        -- Get active build paths
        activePaths <- getActiveBuildPaths
        unless (Set.null activePaths) $
            logMsg 2 $ "Found " <> T.pack (show (Set.size activePaths)) <> " active build paths"

        -- Combine reachable and active paths
        let protectedPaths = Set.union reachablePaths activePaths

        -- Find all deletable paths
        let deletablePaths = Set.difference allStorePaths protectedPaths

        -- Get total size of deletable paths
        totalSize <- liftIO $ sum <$> mapM (getPathSize storeLocationPath)
                                        (map storePathToText $ Set.toList deletablePaths)

        -- Delete unreachable paths
        deleted <- liftIO $ do
            -- Mark unreachable paths as invalid in database
            markPathsAsInvalid db deletablePaths

            -- Delete files from filesystem
            forM_ (Set.toList deletablePaths) $ \path -> do
                -- Skip paths in gc-roots directory
                let pathText = storePathToText path
                let fsPath = storeLocationPath </> T.unpack pathText
                unless ("gc-roots/" `T.isPrefixOf` pathText) $ do
                    -- Attempt to delete, catching and logging any errors but continuing
                    -- This is critical for Nix-like robustness during GC
                    catch (removePathForcibly fsPath) $ \(e :: SomeException) -> do
                        -- Log the error but continue with GC
                        hPutStrLn stderr $ "Warning: Could not remove path: " ++ fsPath
                        hPutStrLn stderr $ "  Error: " ++ show e
                        return ()

            return $ Set.size deletablePaths

        -- Calculate elapsed time
        endTime <- liftIO getCurrentTime
        let elapsed = realToFrac $ diffUTCTime endTime startTime

        -- Return statistics
        return Ten.Core.GCStats
            { Ten.Core.gcTotal = Set.size allStorePaths
            , Ten.Core.gcLive = Set.size reachablePaths
            , Ten.Core.gcCollected = deleted
            , Ten.Core.gcBytes = totalSize
            , Ten.Core.gcElapsedTime = elapsed
            }

-- | Get file/path size in bytes
getPathSize :: FilePath -> Text -> IO Integer
getPathSize storeLocation pathText = do
    let fullPath = BC.pack $ storeLocation </> T.unpack pathText

    -- Check if path exists
    exists <- ByteString.fileExist fullPath
    if exists
        then do
            -- Get file size
            fileStatus <- ByteString.getFileStatus fullPath
            return $ fromIntegral $ ByteString.fileSize fileStatus
        else
            -- If it doesn't exist, return 0
            return 0

-- | Find all store paths (daemon context)
findAllStorePaths :: Connection -> FilePath -> IO (Set StorePath)
findAllStorePaths db storeLocation = do
    -- Get paths from database (preferred, more reliable)
    -- Query paths from ValidPaths table
    results <- SQLite.query_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: IO [Only Text]

    -- Parse StorePaths and create set
    let dbPaths = Set.fromList $ catMaybes $ map (\(Only p) -> Ten.Core.parseStorePath p) results

    -- If DB is empty, fall back to filesystem scan
    if Set.null dbPaths
        then scanFilesystemForPaths storeLocation
        else return dbPaths

-- | Scan filesystem for store paths (fallback)
scanFilesystemForPaths :: FilePath -> IO (Set StorePath)
scanFilesystemForPaths storeLocation = do
    -- Convert to ByteString path
    let storePathBS = BC.pack storeLocation

    -- Check if store exists
    exists <- ByteString.fileExist storePathBS
    if not exists
        then return Set.empty
        else do
            -- List all entries in store directory
            files <- listFilesBS storePathBS `catch` \(_ :: IOException) -> return []

            -- Filter for valid store path format and handle return type
            Set.fromList . catMaybes <$> mapM (\file -> do
                let fileName = BC.unpack $ getFileName file
                -- Skip special directories and non-store paths
                if (fileName `elem` ["gc-roots", "tmp", "locks", "var"] ||
                    any (`isPrefixOf` fileName) ["gc-roots", "tmp.", ".", ".."])
                then return Nothing
                else return $ Ten.Core.parseStorePath (T.pack fileName)) files
  where
    -- List files in a directory, returning ByteString paths
    listFilesBS :: ByteString -> IO [ByteString]
    listFilesBS dirPath = do
        let dirPathStr = BC.unpack dirPath
        entries <- Dir.listDirectory dirPathStr
        return $ map (BC.pack . (dirPathStr </>)) entries

    -- Get the file name part of a path
    getFileName :: ByteString -> ByteString
    getFileName = BC.pack . takeFileName . BC.unpack

-- | Find all roots for garbage collection (daemon context)
findAllRoots :: Connection -> FilePath -> IO (Set StorePath)
findAllRoots db storeLocation = do
    -- Get roots from filesystem (gc-roots directory)
    let rootsDir = storeLocation </> "gc-roots"
    fsRoots <- getFileSystemRootPaths rootsDir

    -- Get roots from database
    dbRoots <- getDatabaseRootPaths db

    -- Get runtime lock roots (for active builds)
    runtimeRoots <- getRuntimeRootPaths storeLocation

    -- Combine all root sources
    return $ Set.unions [fsRoots, dbRoots, runtimeRoots]

-- | Get root paths from filesystem
getFileSystemRootPaths :: FilePath -> IO (Set StorePath)
getFileSystemRootPaths rootsDir = do
    -- Check if the directory exists
    let rootsDirBS = BC.pack rootsDir
    exists <- ByteString.fileExist rootsDirBS
    if not exists
        then return Set.empty
        else do
            -- List all symlinks in the directory
            files <- listFilesBS rootsDirBS `catch` \(_ :: IOException) -> return []

            -- Process each symlink to get target
            foldM (\acc file -> do
                let srcPath = file
                isLink <- (ByteString.isSymbolicLink <$> ByteString.getFileStatus srcPath) `catch`
                    \(_ :: SomeException) -> pure False

                if isLink
                    then do
                        target <- try @SomeException $ ByteString.readSymbolicLink srcPath
                        case target of
                            Left (_ :: SomeException) -> return acc
                            Right targetPath ->
                                case Ten.Core.filePathToStorePath (BC.unpack targetPath) of
                                    Just sp -> return $ Set.insert sp acc
                                    Nothing -> return acc
                    else return acc
                ) Set.empty files
  where
    -- List files in a directory, returning ByteString paths
    listFilesBS :: ByteString -> IO [ByteString]
    listFilesBS dirPath = do
        let dirPathStr = BC.unpack dirPath
        entries <- Dir.listDirectory dirPathStr
        return $ map (BC.pack . (dirPathStr </>)) entries

-- | Get root paths from database
getDatabaseRootPaths :: Connection -> IO (Set StorePath)
getDatabaseRootPaths db = do
    -- Query active roots from database
    results <- SQLite.query_ db "SELECT path FROM GCRoots WHERE active = 1" :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only text) -> Ten.Core.parseStorePath text) results

-- | Get runtime roots from active builds
getRuntimeRootPaths :: FilePath -> IO (Set StorePath)
getRuntimeRootPaths storeLocation = do
    -- Check for runtime locks directory
    let locksDir = storeLocation </> "var/ten/locks"
    let locksDirBS = BC.pack locksDir
    exists <- ByteString.fileExist locksDirBS

    if not exists
        then return Set.empty
        else do
            -- Look for active build locks - robust against all kinds of errors
            files <- listFilesBS locksDirBS `catch` \(e :: SomeException) -> do
                -- Log but continue - GC should be maximally robust
                hPutStrLn stderr $ "Warning: Error accessing lock directory: " ++ show e
                return []

            -- Process each lock file to find references to store paths
            foldM (\acc lockFile -> do
                -- Read lock file
                content <- try @SomeException $ BS.readFile (BC.unpack lockFile)
                case content of
                    Left e -> do
                        -- Log error but continue GC process, following Nix philosophy
                        liftIO $ hPutStrLn stderr $ "Warning: Error reading lock file: " ++ BC.unpack lockFile
                        return acc
                    Right fileContent -> do
                        -- Extract store paths from lock file content
                        let storePaths = extractStorePaths (BC.unpack fileContent)
                        return $ Set.union acc storePaths
                ) Set.empty files
  where
    -- List files in a directory, returning ByteString paths
    listFilesBS :: ByteString -> IO [ByteString]
    listFilesBS dirPath = do
        let dirPathStr = BC.unpack dirPath
        entries <- Dir.listDirectory dirPathStr
        return $ map (BC.pack . (dirPathStr </>)) entries

    -- Function to extract store paths from text
    extractStorePaths :: String -> Set StorePath
    extractStorePaths text =
        -- This is a simplified approach; in practice you'd use a proper parser
        let words = lines text
            paths = filter (\w -> length w > 10 && '-' `elem` w) words
        in Set.fromList $ catMaybes $ map (Ten.Core.parseStorePath . T.pack) paths

-- | Check if a path is reachable from any root
-- Works in both contexts through different mechanisms
tsReachable :: forall (t :: PrivilegeTier). SingI t => StorePath -> TenM 'Build t Bool
tsReachable path = case sing @t of
    SDaemon -> isReachablePrivileged path
    SBuilder -> requestPathReachability path

-- | Internal implementation for privileged reachability check
isReachablePrivileged :: StorePath -> TenM 'Build 'Daemon Bool
isReachablePrivileged path = do
    env <- ask

    -- Initialize database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all roots
        roots <- liftIO $ findAllRoots db (storeLocation env)

        -- Use database to check reachability
        liftIO $ isPathReachable db roots path

-- | Check if a path is reachable from roots
isPathReachable :: Connection -> Set StorePath -> StorePath -> IO Bool
isPathReachable db roots path = do
    -- Check if path is directly in roots
    if path `Set.member` roots
        then return True
        else do
            -- Use reference tracking DB to check if path is reachable from roots
            let rootPaths = Set.toList roots
            reachable <- foldM (\found root ->
                                if found
                                   then return True
                                   else do
                                       -- Check if the path is reachable from this root
                                       refs <- findPathsClosure db (Set.singleton root)
                                       return $ path `Set.member` refs
                               ) False rootPaths
            return reachable

-- | Request path reachability check from builder context via protocol
requestPathReachability :: StorePath -> TenM 'Build 'Builder Bool
requestPathReachability path = do
    env <- ask

    -- Send request to daemon using protocol
    case Ten.Core.runMode env of
        ClientMode conn -> do
            -- Create a request
            let request = Ten.Core.Request {
                    Ten.Core.reqId = 0,  -- Will be set by sendRequest
                    Ten.Core.reqType = "path-reachability",
                    Ten.Core.reqParams = Map.singleton "path" (storePathToText path),
                    Ten.Core.reqPayload = Nothing
                }

            -- Send request through daemon connection
            reqId <- liftIO $ Ten.Core.sendRequest conn request
            responseResult <- liftIO $ Ten.Core.receiveResponse conn reqId 30000000 -- 30 seconds timeout

            -- Handle response
            case responseResult of
                Left err -> throwError err
                Right response ->
                    if isDaemonResponseOk response
                        then
                            return $ getReachabilityFromResponse response
                        else throwError $ Ten.Core.DaemonError $ getDaemonResponseMessage response

        _ -> throwError $ Ten.Core.DaemonError "Not connected to daemon"

-- | Find all paths reachable from roots (daemon privilege only)
findReachablePaths :: TenM 'Build 'Daemon (Set StorePath)
findReachablePaths = do
    env <- ask

    -- Initialize database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Find all roots
        roots <- liftIO $ findAllRoots db (storeLocation env)

        -- Compute all reachable paths
        liftIO $ computeReachablePathsFromRoots db roots

-- | Acquire the GC lock (daemon privilege only)
acquireGCLock :: TenM 'Build 'Daemon ()
acquireGCLock = do
    env <- ask
    let lockPath = Ten.Core.gcLockPath (storeLocation env)

    -- Ensure the lock directory exists
    liftIO $ Ten.Core.ensureLockDirExists (takeDirectory lockPath)

    -- Try to acquire the lock with proper privilege verification
    result <- liftIO $ try @SomeException $ acquireFileLock lockPath

    case result of
        Left e -> throwError $ Ten.Core.ResourceError $ "Could not acquire GC lock: " <> T.pack (show e)
        Right lock -> do
            -- Keep the fd in a global reference to avoid GC
            liftIO $ writeIORef globalGCLockFdRef (Just (lockFd lock, lockPath))

-- | Release the GC lock (daemon privilege only)
releaseGCLock :: TenM 'Build 'Daemon ()
releaseGCLock = do
    env <- ask
    let lockPath = Ten.Core.gcLockPath (storeLocation env)

    -- Check if the lock file exists and we own it
    exists <- liftIO $ doesFileExist lockPath
    when exists $ do
        -- Read the lock file to check the owner
        content <- liftIO $ try @IOException $ readFile lockPath
        case content of
            Right pidStr -> do
                case reads pidStr of
                    [(lockPid, "")] -> do
                        -- Check if it's our PID
                        ourPid <- liftIO getProcessID
                        when (lockPid == ourPid) $ do
                            -- Get the file descriptor from the global reference
                            mFdInfo <- liftIO $ readIORef globalGCLockFdRef
                            case mFdInfo of
                                Just (fd, path) | path == lockPath -> do
                                    -- Create a temporary GCLock just to release it
                                    let lock = GCLock fd lockPath ourPid
                                    liftIO $ releaseFileLock lock
                                    -- Clear the global reference
                                    liftIO $ writeIORef globalGCLockFdRef Nothing
                                _ -> return ()
                    _ -> return ()
            _ -> return ()

-- | Execute an action with the GC lock (daemon privilege only)
withGCLock :: TenM 'Build 'Daemon a -> TenM 'Build 'Daemon a
withGCLock action = do
    -- First acquire the lock with privilege evidence
    acquireGCLock

    -- Run the action with proper error handling
    result <- action `Ten.Core.catchError` \e -> do
        -- Always release the lock on error
        releaseGCLock
        -- Re-throw the error
        Ten.Core.throwError e

    -- Always release the lock on completion
    releaseGCLock

    -- Return the result
    return result

-- | Break a stale lock (daemon privilege only)
breakStaleLock :: FilePath -> TenM 'Build 'Daemon ()
breakStaleLock lockPath = do
    -- Check lock status
    status <- liftIO $ try @SomeException $ checkLockFile lockPath

    case status of
        Left e ->
            logMsg 1 $ "Cannot check lock status: " <> T.pack (show e)
        Right isValid ->
            unless isValid $ liftIO $ do
                -- Log that we're breaking a stale lock
                hPutStrLn stderr $ "Breaking stale GC lock: " ++ lockPath

                -- Remove the lock file - catch any exception but log it
                -- This ensures GC can proceed even when lock removal fails
                removeFile lockPath `catch` \(e :: SomeException) -> do
                    hPutStrLn stderr $ "Warning: Failed to remove stale lock file: " ++ lockPath
                    hPutStrLn stderr $ "  Error: " ++ show e

-- | Check if a lock file exists and is valid
checkLockFile :: FilePath -> IO Bool
checkLockFile lockPath = do
    -- Convert to ByteString path
    let lockPathBS = BC.pack lockPath

    -- Check if lock file exists
    exists <- ByteString.fileExist lockPathBS

    if not exists
        then return False  -- Lock file doesn't exist
        else do
            -- Read the lock file to get PID
            result <- try @IOException $ BS.readFile lockPath
            case result of
                Left _ ->
                    return False  -- Error reading lock file, treat as invalid

                Right content -> do
                    -- Parse the PID
                    case reads (BC.unpack content) of
                        [(pid, "")] -> do
                            -- Check if the process is still running
                            isProcessRunning pid
                        _ -> return False  -- Invalid lock file format

-- | Helper function for breakStaleLock that can be used from non-TenM contexts
breakStaleLock' :: FilePath -> IO ()
breakStaleLock' lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath
    when exists $ do
        -- Read the lock file
        content <- try @SomeException $ readFile lockPath
        case content of
            Right pidStr -> do
                case reads pidStr of
                    [(lockPid, "")] -> do
                        -- Check if the process is still running
                        pidRunning <- isProcessRunning lockPid
                        unless pidRunning $ do
                            -- Remove the stale lock
                            hPutStrLn stderr $ "Breaking stale GC lock: " ++ lockPath
                            removeFile lockPath `catch` \(_ :: SomeException) ->
                                hPutStrLn stderr $ "Warning: Failed to remove stale lock file: " ++ lockPath
                    _ -> return ()
            _ -> return ()

-- | Check if a process is still running
isProcessRunning :: ProcessID -> IO Bool
isProcessRunning pid = do
    -- Try to send signal 0 to the process (doesn't actually send a signal, just checks existence)
    result <- try @SomeException $ signalProcess 0 pid
    case result of
        Left _ -> return False  -- Process doesn't exist
        Right _ -> return True  -- Process exists

-- | Create and acquire a lock file
acquireFileLock :: FilePath -> IO GCLock
acquireFileLock lockPath = do
    -- Ensure parent directory exists
    Ten.Core.ensureLockDirExists (takeDirectory lockPath)

    -- Get our process ID
    pid <- getProcessID

    -- Check if lock already exists
    result <- try @SomeException $ checkLockFile lockPath
    active <- case result of
        Left _ -> return False
        Right isActive -> return isActive

    when active $
        throwIO $ userError "Another garbage collection is in progress"

    -- Create the lock file with our PID
    result <- try @SomeException $ mask $ \unmask -> do
        -- Create and open the lock file with proper flags for locking
        fd <- openFd lockPath WriteOnly defaultFileFlags{creat = Just 0o644, exclusive = True}

        unmask $ do
            -- Write our PID to it
            handle <- fdToHandle fd
            hPutStrLn handle (show pid)
            hFlush handle

            -- Lock the file to prevent concurrent access
            setLock fd (WriteLock, AbsoluteSeek, 0, 0)

            -- Return the lock
            return $ GCLock {
                lockFd = fd,
                lockPath = lockPath,
                lockPid = pid
            }

    case result of
        Left e ->
            throwIO $ userError $ "Failed to create lock file: " ++ show e
        Right lock ->
            return lock

-- | Release a file lock
releaseFileLock :: GCLock -> IO ()
releaseFileLock lock = do
    -- Release the lock - catch any errors but always try to continue
    catch (setLock (lockFd lock) (Unlock, AbsoluteSeek, 0, 0)) $ \(e :: SomeException) -> do
        hPutStrLn stderr $ "Warning: Error releasing lock: " ++ show e
        return ()

    -- Close the file descriptor - again, be robust against errors
    catch (closeFd (lockFd lock)) $ \(e :: SomeException) -> do
        hPutStrLn stderr $ "Warning: Error closing lock file descriptor: " ++ show e
        return ()

    -- Remove the lock file - if this fails, log but continue
    -- This approach ensures maximum resilience during GC, following Nix's philosophy
    catchIOError (removeFile (lockPath lock)) $ \err -> do
        hPutStrLn stderr $ "Warning: Error removing lock file: " ++ lockPath lock
        hPutStrLn stderr $ "  Error: " ++ show err
        return ()

-- | Get active build paths (to prevent GC during builds)
getActiveBuildPaths :: TenM 'Build 'Daemon (Set StorePath)
getActiveBuildPaths = do
    env <- ask

    -- Get runtime paths from database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Query active build paths from database
        results <- dbQuery_ db
            "SELECT path FROM ActiveBuilds WHERE status != 'completed'" :: TenM 'Build 'Daemon [Only Text]

        -- Parse paths
        return $ Set.fromList $ catMaybes $ map (\(Only t) -> Ten.Core.parseStorePath t) results

-- | Verify the store integrity
verifyStore :: FilePath -> TenM 'Build 'Daemon Bool
verifyStore storeDir = do
    env <- ask

    -- Open database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all valid paths from database
        paths <- dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: TenM 'Build 'Daemon [Only Text]

        -- Verify each path
        results <- liftIO $ forM paths $ \(Only pathText) -> do
            case Ten.Core.parseStorePath pathText of
                Nothing -> return False
                Just storePath -> do
                    -- Verify that the store file exists and has the correct hash
                    result <- try @SomeException $ do
                        let fullPath = storePathToFilePath storePath env
                        let fullPathBS = BC.pack fullPath
                        fileExists <- ByteString.fileExist fullPathBS
                        if fileExists
                            then do
                                content <- BS.readFile fullPath
                                let actualHash = showHash $ Ten.Core.hashByteString content
                                return $ actualHash == storeHash storePath
                            else return False
                    case result of
                        Left _ -> return False
                        Right valid -> return valid

        -- Return true if all paths verify
        return $ all id results

-- | Convert hash to string representation
showHash :: Digest SHA256 -> Text
showHash = T.pack . show

-- | Repair the store (remove invalid paths) - daemon privilege only
repairStore :: TenM 'Build 'Daemon GCStats
repairStore = do
    env <- ask
    startTime <- liftIO getCurrentTime

    -- Open database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Get all valid paths from database
        paths <- dbQuery_ db "SELECT path FROM ValidPaths WHERE is_valid = 1" :: TenM 'Build 'Daemon [Only Text]

        -- Verify and repair each path
        (valid, invalid, totalSize) <- liftIO $ foldM (\(v, i, size) (Only pathText) -> do
            case Ten.Core.parseStorePath pathText of
                Nothing -> return (v, i + 1, size)  -- Invalid path format
                Just storePath -> do
                    -- Check if file exists and has correct hash
                    result <- try @SomeException $ do
                        let fullPath = storePathToFilePath storePath env
                        let fullPathBS = BC.pack fullPath
                        fileExists <- ByteString.fileExist fullPathBS
                        if fileExists
                            then do
                                content <- BS.readFile fullPath
                                let actualHash = showHash $ Ten.Core.hashByteString content
                                return (actualHash == storeHash storePath, fullPath)
                            else return (False, fullPath)

                    case result of
                        Left _ -> return (v, i + 1, size)
                        Right (isValid, path) ->
                            if isValid
                                then return (v + 1, i, size)
                                else do
                                    -- Invalid path, mark as invalid and remove
                                    -- Get size before deletion
                                    fileStatus <- try @IOException $ do
                                        status <- ByteString.getFileStatus (BC.pack path)
                                        return $ fromIntegral $ ByteString.fileSize status
                                    let pathSize = case fileStatus of
                                            Right size -> size
                                            Left _ -> 0

                                    -- Mark as invalid in database
                                    SQLite.execute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?" (Only pathText)

                                    -- Delete the file - robust against any kind of error
                                    catchIOError (removeFile path) $ \err -> do
                                        -- Log but continue with GC process
                                        hPutStrLn stderr $ "Warning: Could not remove invalid path: " ++ path
                                        hPutStrLn stderr $ "  Error: " ++ show err
                                        return ()

                                    return (v, i + 1, size + pathSize)
            ) (0, 0, 0) paths

        -- Calculate elapsed time
        endTime <- liftIO getCurrentTime
        let elapsed = realToFrac $ diffUTCTime endTime startTime

        -- Return statistics
        return Ten.Core.GCStats
            { Ten.Core.gcTotal = valid + invalid
            , Ten.Core.gcLive = valid
            , Ten.Core.gcCollected = invalid
            , Ten.Core.gcBytes = totalSize
            , Ten.Core.gcElapsedTime = elapsed
            }

-- | Helper function for database with proper monad handling
withDatabase :: FilePath -> Int -> (Connection -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withDatabase dbPath timeout action = do
    -- Initialize database and ensure schema exists
    connection <- liftIO $ initializeDatabase dbPath

    -- Set busy timeout
    liftIO $ SQLite.execute_ connection $ "PRAGMA busy_timeout = " <> Query (T.pack (show timeout))

    -- Enable foreign keys
    liftIO $ SQLite.execute_ connection "PRAGMA foreign_keys = ON"

    -- Run action with proper error handling in TenM monad
    result <- action connection `catchError` \e -> do
        -- Close the connection
        liftIO $ SQLite.close connection
        -- Re-throw the error
        throwError e

    -- Close the connection
    liftIO $ SQLite.close connection

    -- Return the result
    return result

-- | Initialize database with schema
initializeDatabase :: FilePath -> IO Connection
initializeDatabase dbPath = do
    -- Ensure directory exists
    createDirectoryIfMissing True (takeDirectory dbPath)

    -- Connect to database
    conn <- SQLite.open dbPath

    -- Set up schema if needed
    ensureSchema conn

    return conn

-- | Ensure database schema is set up
ensureSchema :: Connection -> IO ()
ensureSchema conn = do
    -- Check if tables exist
    tables <- query_ conn "SELECT name FROM sqlite_master WHERE type='table' AND name='ValidPaths'" :: IO [Only Text]

    when (null tables) $ do
        -- Create tables
        execute_ conn "CREATE TABLE IF NOT EXISTS ValidPaths (\
                     \  path TEXT PRIMARY KEY,\
                     \  is_valid INTEGER NOT NULL DEFAULT 1,\
                     \  last_accessed INTEGER,\
                     \  registration_time INTEGER NOT NULL DEFAULT (strftime('%s','now')),\
                     \  deriver TEXT\
                     \)"

        execute_ conn "CREATE TABLE IF NOT EXISTS References (\
                     \  referrer TEXT NOT NULL,\
                     \  reference TEXT NOT NULL,\
                     \  ref_type TEXT NOT NULL,\
                     \  PRIMARY KEY (referrer, reference),\
                     \  FOREIGN KEY (referrer) REFERENCES ValidPaths(path) ON DELETE CASCADE,\
                     \  FOREIGN KEY (reference) REFERENCES ValidPaths(path) ON DELETE CASCADE\
                     \)"

        execute_ conn "CREATE TABLE IF NOT EXISTS GCRoots (\
                     \  path TEXT NOT NULL,\
                     \  name TEXT NOT NULL,\
                     \  type TEXT NOT NULL,\
                     \  timestamp INTEGER NOT NULL,\
                     \  active INTEGER NOT NULL DEFAULT 1,\
                     \  PRIMARY KEY (path, name)\
                     \)"

        execute_ conn "CREATE TABLE IF NOT EXISTS ActiveBuilds (\
                     \  build_id TEXT PRIMARY KEY,\
                     \  path TEXT NOT NULL,\
                     \  start_time INTEGER NOT NULL,\
                     \  status TEXT NOT NULL,\
                     \  progress REAL\
                     \)"

        -- Create indexes
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References (referrer)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_references_reference ON References (reference)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_gcroots_path ON GCRoots (path)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_gcroots_active ON GCRoots (active)"
        execute_ conn "CREATE INDEX IF NOT EXISTS idx_activebuilds_status ON ActiveBuilds (status)"

-- | Find paths with prefix in the store
findPathsWithPrefix :: Text -> TenM 'Build 'Daemon [StorePath]
findPathsWithPrefix prefix = do
    env <- ask

    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Query for paths with prefix
        rows <- dbQuery db
            "SELECT path FROM ValidPaths WHERE path LIKE ? || '%' AND is_valid = 1"
            (Only prefix) :: TenM 'Build 'Daemon [Only Text]

        -- Convert to StorePath objects
        return $ catMaybes $ map (\(Only t) -> Ten.Core.parseStorePath t) rows

-- | Helper function to check if a path exists in store
storePathExists :: StorePath -> TenM p t Bool
storePathExists path = do
    env <- ask
    let fullPath = storePathToFilePath path env
    let fullPathBS = BC.pack fullPath
    liftIO $ ByteString.fileExist fullPathBS

-- | Compute reachable paths from a set of roots
computeReachablePaths :: Set StorePath -> TenM 'Build 'Daemon (Set StorePath)
computeReachablePaths roots = do
    env <- ask

    -- Initialize database
    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Compute all reachable paths
        liftIO $ computeReachablePathsFromRoots db roots

-- | Helper function to compute reachable paths from roots
computeReachablePathsFromRoots :: Connection -> Set StorePath -> IO (Set StorePath)
computeReachablePathsFromRoots db roots = do
    -- Include the roots themselves
    findPathsClosure db roots

-- | Find paths reachability closure (using properly optimized DB query)
findPathsClosure :: Connection -> Set StorePath -> IO (Set StorePath)
findPathsClosure db startingPaths = do
    -- Create a temporary table for the closure calculation
    execute_ db "CREATE TEMP TABLE IF NOT EXISTS temp_closure (path TEXT PRIMARY KEY)"
    execute_ db "DELETE FROM temp_closure"

    -- Insert all starting paths
    forM_ (Set.toList startingPaths) $ \path ->
        execute db "INSERT OR IGNORE INTO temp_closure VALUES (?)" (Only (storePathToText path))

    -- Use recursive CTE to compute the transitive closure efficiently
    let closureQuery = "WITH RECURSIVE closure(path) AS (\
                      \  SELECT path FROM temp_closure\
                      \  UNION\
                      \  SELECT r.reference FROM References r JOIN closure c ON r.referrer = c.path\
                      \) SELECT DISTINCT path FROM closure"

    results <- query_ db closureQuery :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only p) -> Ten.Core.parseStorePath p) results

-- | Helper function to mark paths as invalid in the database
markPathsAsInvalid :: Connection -> Set StorePath -> IO ()
markPathsAsInvalid db paths = do
    -- Mark each path as invalid in the ValidPaths table
    forM_ (Set.toList paths) $ \path ->
        execute db "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
               (Only (storePathToText path))

-- | Find all references in a path
-- This is a daemon-only operation
findPathReferences :: StorePath -> TenM 'Build 'Daemon (Set StorePath)
findPathReferences path = do
    env <- ask

    -- First try to get references from database (faster)
    dbRefs <- getReferencesFromDB path

    if not (Set.null dbRefs)
        then return dbRefs
        else do
            -- Fall back to file scanning if database has no entries
            let filePath = storePathToFilePath path env
            let filePathBS = BC.pack filePath

            -- Check file existence
            exists <- liftIO $ ByteString.fileExist filePathBS
            if not exists
                then return Set.empty
                else do
                    -- Read file content
                    content <- liftIO $ BS.readFile filePath

                    -- Scan for references
                    refs <- scanForReferences content

                    -- Register references in database for future use
                    forM_ (Set.toList refs) $ \refPath ->
                        registerReference path refPath

                    return refs

-- | Get references from database
getReferencesFromDB :: StorePath -> TenM 'Build 'Daemon (Set StorePath)
getReferencesFromDB path = do
    env <- ask

    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Query for references
        rows <- dbQuery db
            "SELECT reference FROM References WHERE referrer = ?"
            (Only (storePathToText path)) :: TenM 'Build 'Daemon [Only Text]

        -- Convert to StorePath objects
        return $ Set.fromList $ catMaybes $ map (\(Only t) -> Ten.Core.parseStorePath t) rows

-- | Register a reference between paths
registerReference :: StorePath -> StorePath -> TenM 'Build 'Daemon ()
registerReference referrer reference = do
    env <- ask

    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Insert reference (ignore if already exists)
        dbExecute db
            "INSERT OR IGNORE INTO References (referrer, reference, type) VALUES (?, ?, 'direct')"
            (storePathToText referrer, storePathToText reference)

-- | Scan content for references to other store paths
-- This is a daemon-only operation
scanForReferences :: ByteString -> TenM 'Build 'Daemon (Set StorePath)
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
            in Ten.Core.parseStorePath fileName
        | otherwise =
            -- Handle just hash-name format
            Ten.Core.parseStorePath text

-- | Check if a path is a GC root
-- This is a daemon-only operation
isGCRoot :: StorePath -> TenM 'Build 'Daemon Bool
isGCRoot path = do
    env <- ask
    let storeDir = storeLocation env
    let rootsDir = storeDir </> "gc-roots"

    -- Check database first
    isDbRoot <- isGCRootInDB path

    if isDbRoot
        then return True
        else do
            -- If not in database, check filesystem
            -- List all roots - catch any exception for Nix-like robustness
            roots <- liftIO $ listFilesBS (BC.pack rootsDir) `catch` \(e :: SomeException) -> do
                -- Log but continue with GC
                hPutStrLn stderr $ "Warning: Error listing GC roots: " ++ show e
                return []

            -- Check if any root links to this path
            isFileRoot <- liftIO $ foldM (\found root -> do
                if found
                    then return True
                    else do
                        -- Check if this root points to our path
                        let rootPath = root
                        isLink <- (ByteString.isSymbolicLink <$> ByteString.getFileStatus rootPath) `catch`
                            \(_ :: SomeException) -> pure False
                        if isLink
                            then do
                                target <- ByteString.readSymbolicLink rootPath `catch` \(_ :: IOException) -> return ""
                                let targetPath = BC.pack $ storePathToFilePath path env
                                return $ targetPath == target
                            else return False
                ) False roots

            -- If found in filesystem but not in DB, register it
            when (isFileRoot && not isDbRoot) $
                registerGCRoot path (T.pack "filesystem-found") ("symlink" :: Text)

            return isFileRoot
  where
    -- List files in a directory, returning ByteString paths
    listFilesBS :: ByteString -> IO [ByteString]
    listFilesBS dirPath = do
        let dirPathStr = BC.unpack dirPath
        entries <- Dir.listDirectory dirPathStr
        return $ map (BC.pack . (dirPathStr </>)) entries

-- | Check if a path is a GC root in the database
isGCRootInDB :: StorePath -> TenM 'Build 'Daemon Bool
isGCRootInDB path = do
    env <- ask

    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Query for roots
        rows <- dbQuery db
            "SELECT COUNT(*) FROM GCRoots WHERE path = ? AND active = 1"
            (Only (storePathToText path)) :: TenM 'Build 'Daemon [Only Int]

        -- Check if count > 0
        return $ case rows of
            [Only count] -> count > 0
            _ -> False

-- | Register a GC root in the database
registerGCRoot :: StorePath -> Text -> Text -> TenM 'Build 'Daemon ()
registerGCRoot path name rootType = do
    env <- ask

    withDatabase (defaultDBPath (storeLocation env)) 5000 $ \db -> do
        -- Insert or update root
        dbExecute db
            "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, ?, strftime('%s','now'), 1)"
            (storePathToText path, name, rootType)
