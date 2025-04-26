{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Ten.DB.References (
    -- Type classes for reference operations
    CanRegisterReferences(..),
    CanQueryReferences(..),
    CanManageReachability(..),
    CanManageGCRoots(..),
    CanScanReferences(..),
    CanManageValidPaths(..),
    CanManageDatabase(..),

    -- Reference registration functions (context-aware)
    registerReference,
    registerReferences,
    registerPathReferences,

    -- Reference queries (context-aware)
    getReferences,
    getReferrers,
    getDirectReferences,
    getDirectReferrers,

    -- Reachability operations (context-aware)
    computeReachablePathsFromRoots,
    findPathsClosure,
    findPathsClosureWithLimit,
    isPathReachable,

    -- GC support (context-aware)
    findGCRoots,
    getRegisteredRoots,

    -- Path validity management (context-aware)
    markPathsAsValid,
    markPathsAsInvalid,

    -- Reference scanning (context-aware)
    scanFileForReferences,
    scanStoreForReferences,

    -- Database operations (context-aware)
    vacuumReferenceDb,
    validateReferenceDb,
    cleanupDanglingReferences,

    -- Types
    ReferenceEntry(..),
    ReferenceStats(..),
    StorePathReference(..)
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
import Data.Int (Int64)
import Data.List (isInfixOf, isPrefixOf, nub, sort)
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
import qualified Ten.Daemon.Client as Client

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

-- | Type class for reference registration operations
class CanRegisterReferences (t :: PrivilegeTier) where
    -- | Register a single reference between two store paths
    registerReferenceImpl :: Database t -> StorePath -> StorePath -> TenM p t ()

    -- | Register multiple references from one referrer
    registerReferencesImpl :: Database t -> StorePath -> Set StorePath -> TenM p t ()

    -- | Register references for a path after scanning the file
    registerPathReferencesImpl :: Database t -> FilePath -> StorePath -> TenM p t Int

-- | Daemon implementation for reference registration
instance CanRegisterReferences 'Daemon where
    registerReferenceImpl db from to =
        -- Avoid self-references
        when (from /= to) $
            tenExecute_ db
                "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
                (storePathToText from, storePathToText to)

    registerReferencesImpl db referrer references = withTenTransaction db ReadWrite $ \_ -> do
        -- Insert each valid reference
        forM_ (Set.toList references) $ \ref ->
            -- Avoid self-references
            when (referrer /= ref) $
                tenExecute_ db
                       "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
                       (storePathToText referrer, storePathToText ref)

    registerPathReferencesImpl db storeDir path = do
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

-- | Builder implementation for reference registration (via protocol)
instance CanRegisterReferences 'Builder where
    registerReferenceImpl _ from to =
        -- Avoid self-references
        when (from /= to) $ do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create request
                    let request = Request {
                        reqId = 0,
                        reqType = "register-reference",
                        reqParams = Map.fromList [
                            ("referrer", storePathToText from),
                            ("reference", storePathToText to)
                        ],
                        reqPayload = Nothing
                    }

                    -- Send request
                    response <- liftIO $ Client.sendRequestSync conn request 30000000
                    case response of
                        Left err -> throwError err
                        Right resp ->
                            if respStatus resp == "ok"
                                then return ()
                                else throwError $ DBError $ respMessage resp

                _ -> throwError $ privilegeError "Cannot register reference without daemon connection"

    registerReferencesImpl _ from refs =
        -- Skip if empty
        unless (Set.null refs) $ do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Filter out self-references
                    let validRefs = Set.filter (/= from) refs

                    -- Build paths list
                    let pathsList = T.intercalate "," (map storePathToText (Set.toList validRefs))

                    -- Create request
                    let request = Request {
                        reqId = 0,
                        reqType = "register-references",
                        reqParams = Map.fromList [
                            ("referrer", storePathToText from),
                            ("references", pathsList)
                        ],
                        reqPayload = Nothing
                    }

                    -- Send request
                    response <- liftIO $ Client.sendRequestSync conn request 30000000
                    case response of
                        Left err -> throwError err
                        Right resp ->
                            if respStatus resp == "ok"
                                then return ()
                                else throwError $ DBError $ respMessage resp

                _ -> throwError $ privilegeError "Cannot register references without daemon connection"

    registerPathReferencesImpl _ storeDir path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Construct the full path
                let fullPath = storePathToFilePath path env

                -- Verify the file exists
                exists <- liftIO $ doesFileExist fullPath
                if not exists
                    then return 0
                    else do
                        -- Create request
                        let request = Request {
                            reqId = 0,
                            reqType = "register-path-references",
                            reqParams = Map.fromList [
                                ("path", storePathToText path),
                                ("filePath", T.pack fullPath),
                                ("storeDir", T.pack storeDir)
                            ],
                            reqPayload = Nothing
                        }

                        -- Send request
                        response <- liftIO $ Client.sendRequestSync conn request 30000000
                        case response of
                            Left err -> throwError err
                            Right resp ->
                                if respStatus resp == "ok"
                                    then case Map.lookup "count" (respData resp) of
                                        Just countText -> case reads (T.unpack countText) of
                                            [(count, "")] -> return count
                                            _ -> return 0
                                        Nothing -> return 0
                                    else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot register path references without daemon connection"

-- | Type class for reference query operations
class CanQueryReferences (t :: PrivilegeTier) where
    -- | Get direct references from a store path
    getDirectReferencesImpl :: Database t -> StorePath -> TenM p t (Set StorePath)

    -- | Get all references from a path (direct and indirect)
    getReferencesImpl :: Database t -> StorePath -> TenM p t (Set StorePath)

    -- | Get direct referrers to a store path
    getDirectReferrersImpl :: Database t -> StorePath -> TenM p t (Set StorePath)

    -- | Get all referrers to a path (direct and indirect)
    getReferrersImpl :: Database t -> StorePath -> TenM p t (Set StorePath)

-- | Daemon implementation for reference queries
instance CanQueryReferences 'Daemon where
    getDirectReferencesImpl db path = do
        results <- tenQuery db
            "SELECT reference FROM References WHERE referrer = ?"
            (Only (storePathToText path))

        -- Parse each store path and return as a set
        return $ Set.fromList $ catMaybes $
            map (\(Only p) -> parseStorePath p) results

    getReferencesImpl db path = do
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

    getDirectReferrersImpl db path = do
        results <- tenQuery db
            "SELECT referrer FROM References WHERE reference = ?"
            (Only (storePathToText path))

        -- Parse each store path and return as a set
        return $ Set.fromList $ catMaybes $
            map (\(Only p) -> parseStorePath p) results

    getReferrersImpl db path = do
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

-- | Builder implementation for reference queries (via protocol)
instance CanQueryReferences 'Builder where
    getDirectReferencesImpl _ path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "get-direct-references",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "references" (respData resp) of
                                    Just refsText -> do
                                        let refsList = T.splitOn "," refsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath refsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot get references without daemon connection"

    getReferencesImpl _ path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "get-references",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "references" (respData resp) of
                                    Just refsText -> do
                                        let refsList = T.splitOn "," refsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath refsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot get references without daemon connection"

    getDirectReferrersImpl _ path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "get-direct-referrers",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "referrers" (respData resp) of
                                    Just refsText -> do
                                        let refsList = T.splitOn "," refsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath refsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot get referrers without daemon connection"

    getReferrersImpl _ path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "get-referrers",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "referrers" (respData resp) of
                                    Just refsText -> do
                                        let refsList = T.splitOn "," refsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath refsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot get referrers without daemon connection"

-- | Type class for reachability operations
class CanManageReachability (t :: PrivilegeTier) where
    -- | Compute all paths reachable from a set of roots
    computeReachablePathsImpl :: Database t -> Set StorePath -> TenM p t (Set StorePath)

    -- | Find the transitive closure of paths
    findPathsClosureImpl :: Database t -> Set StorePath -> TenM p t (Set StorePath)

    -- | Find the transitive closure of paths with depth limit
    findPathsClosureWithLimitImpl :: Database t -> Set StorePath -> Int -> TenM p t (Set StorePath)

    -- | Check if a path is reachable from any of the roots
    isPathReachableImpl :: Database t -> Set StorePath -> StorePath -> TenM p t Bool

-- | Daemon implementation for reachability operations
instance CanManageReachability 'Daemon where
    computeReachablePathsImpl db roots = do
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

    findPathsClosureImpl db startingPaths =
        findPathsClosureWithLimitImpl db startingPaths (-1)  -- No limit

    findPathsClosureWithLimitImpl db startingPaths depthLimit = do
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

    isPathReachableImpl db roots path = do
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

                results <- tenQuery db (Query $ T.pack query) (Only (storePathToText path)) :: TenM p 'Daemon [Only Int]

                case results of
                    [Only count] -> return (count > 0)
                    _ -> return False
      where
        -- Helper to create SQL values clause
        rootsToSqlValues [] = "(NULL)"  -- Avoid empty VALUES clause
        rootsToSqlValues [r] = "('" ++ T.unpack (storePathToText r) ++ "')"
        rootsToSqlValues (r:rs) = "('" ++ T.unpack (storePathToText r) ++ "'), " ++ rootsToSqlValues rs

-- | Builder implementation for reachability operations (via protocol)
instance CanManageReachability 'Builder where
    computeReachablePathsImpl _ roots = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Build roots list
                let rootsList = T.intercalate "," (map storePathToText (Set.toList roots))

                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "compute-reachable-paths",
                    reqParams = Map.singleton "roots" rootsList,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "paths" (respData resp) of
                                    Just pathsText -> do
                                        let pathsList = T.splitOn "," pathsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath pathsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot compute reachable paths without daemon connection"

    findPathsClosureImpl db paths =
        findPathsClosureWithLimitImpl db paths (-1)  -- No limit

    findPathsClosureWithLimitImpl _ paths depthLimit = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Build paths list
                let pathsList = T.intercalate "," (map storePathToText (Set.toList paths))

                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "find-paths-closure",
                    reqParams = Map.fromList [
                        ("paths", pathsList),
                        ("depthLimit", T.pack $ show depthLimit)
                    ],
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "paths" (respData resp) of
                                    Just pathsText -> do
                                        let pathsList = T.splitOn "," pathsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath pathsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot find paths closure without daemon connection"

    isPathReachableImpl _ roots path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Build roots list
                let rootsList = T.intercalate "," (map storePathToText (Set.toList roots))

                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "is-path-reachable",
                    reqParams = Map.fromList [
                        ("roots", rootsList),
                        ("path", storePathToText path)
                    ],
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "reachable" (respData resp) of
                                    Just "true" -> return True
                                    _ -> return False
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot check path reachability without daemon connection"

-- | Type class for GC root operations
class CanManageGCRoots (t :: PrivilegeTier) where
    -- | Find all GC roots
    findGCRootsImpl :: Database t -> FilePath -> TenM p t (Set StorePath)

    -- | Get registered roots from the database
    getRegisteredRootsImpl :: Database t -> TenM p t (Set StorePath)

-- | Daemon implementation for GC root operations
instance CanManageGCRoots 'Daemon where
    findGCRootsImpl db storeDir = do
        -- Get roots from the file system (symlinks in gc-roots directory)
        let rootsDir = storeDir </> "gc-roots"
        fsRoots <- liftIO $ getGCRootsFromFS rootsDir

        -- Get explicitly registered roots from the database
        dbRoots <- getRegisteredRoots db

        -- Combine both sets
        return $ Set.union fsRoots dbRoots

    getRegisteredRootsImpl db = do
        results <- tenQuery_ db
            "SELECT path FROM GCRoots WHERE active = 1"

        -- Parse each store path and return as a set
        return $ Set.fromList $ catMaybes $
            map (\(Only p) -> parseStorePath p) results

-- | Builder implementation for GC root operations (via protocol)
instance CanManageGCRoots 'Builder where
    findGCRootsImpl _ storeDir = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "find-gc-roots",
                    reqParams = Map.singleton "storeDir" (T.pack storeDir),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "roots" (respData resp) of
                                    Just rootsText -> do
                                        let rootsList = T.splitOn "," rootsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath rootsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot find GC roots without daemon connection"

    getRegisteredRootsImpl _ = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "get-registered-roots",
                    reqParams = Map.empty,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then
                                case Map.lookup "roots" (respData resp) of
                                    Just rootsText -> do
                                        let rootsList = T.splitOn "," rootsText
                                        return $ Set.fromList $ catMaybes $ map parseStorePath rootsList
                                    Nothing -> return Set.empty
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot get registered roots without daemon connection"

-- | Helper function to get GC roots from filesystem
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

-- | Type class for reference scanning operations
class CanScanReferences (t :: PrivilegeTier) where
    -- | Scan a file for references to store paths
    scanFileForReferencesImpl :: FilePath -> FilePath -> TenM p t (Set StorePath)

    -- | Scan the entire store for references between paths
    scanStoreForReferencesImpl :: FilePath -> TenM p t (Set StorePathReference)

-- | Implementation for scanning operations (shared between tiers)
instance CanScanReferences t where
    scanFileForReferencesImpl storeDir filePath = do
        -- Check if the file exists
        exists <- liftIO $ doesFileExist filePath
        if not exists
            then return Set.empty
            else do
                -- Detect file type and use appropriate scanner
                fileType <- liftIO $ detectFileType filePath

                liftIO $ case fileType of
                    ElfBinary -> scanElfBinary filePath storeDir
                    TextFile -> scanTextFile filePath storeDir
                    ScriptFile -> scanTextFile filePath storeDir
                    BinaryFile -> scanBinaryFile filePath storeDir

    scanStoreForReferencesImpl storeDir = do
        env <- ask
        -- Only allow this operation on daemon tier
        case currentPrivilegeTier env of
            Daemon -> do
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
                            refs <- scanFile storeDir fullPath

                            -- Add each reference to the accumulator
                            let newRefs = Set.map (StorePathReference path) refs
                            return $ Set.union acc newRefs
                    ) Set.empty paths

                return references

            Builder -> throwError $ privilegeError "Cannot scan entire store without daemon privileges"
        where
            -- Internal version without TenM context
            scanFile :: FilePath -> FilePath -> IO (Set StorePath)
            scanFile storeDir filePath = do
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

-- | Type class for path validity management
class CanManageValidPaths (t :: PrivilegeTier) where
    -- | Mark a set of paths as valid in the ValidPaths table
    markPathsAsValidImpl :: Database t -> Set StorePath -> TenM p t ()

    -- | Mark a set of paths as invalid in the ValidPaths table
    markPathsAsInvalidImpl :: Database t -> Set StorePath -> TenM p t ()

-- | Daemon implementation for path validity management
instance CanManageValidPaths 'Daemon where
    markPathsAsValidImpl db paths = withTenTransaction db ReadWrite $ \_ -> do
        forM_ (Set.toList paths) $ \path ->
            tenExecute_ db
                "UPDATE ValidPaths SET is_valid = 1 WHERE path = ?"
                (Only (storePathToText path))

    markPathsAsInvalidImpl db paths = withTenTransaction db ReadWrite $ \_ -> do
        forM_ (Set.toList paths) $ \path ->
            tenExecute_ db
                "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
                (Only (storePathToText path))

-- | Builder implementation for path validity management (via protocol)
instance CanManageValidPaths 'Builder where
    markPathsAsValidImpl _ paths = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Skip if empty
                unless (Set.null paths) $ do
                    -- Build paths list
                    let pathsList = T.intercalate "," (map storePathToText (Set.toList paths))

                    -- Create request
                    let request = Request {
                        reqId = 0,
                        reqType = "mark-paths-valid",
                        reqParams = Map.singleton "paths" pathsList,
                        reqPayload = Nothing
                    }

                    -- Send request
                    response <- liftIO $ Client.sendRequestSync conn request 30000000
                    case response of
                        Left err -> throwError err
                        Right resp ->
                            if respStatus resp == "ok"
                                then return ()
                                else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot mark paths as valid without daemon connection"

    markPathsAsInvalidImpl _ paths = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Skip if empty
                unless (Set.null paths) $ do
                    -- Build paths list
                    let pathsList = T.intercalate "," (map storePathToText (Set.toList paths))

                    -- Create request
                    let request = Request {
                        reqId = 0,
                        reqType = "mark-paths-invalid",
                        reqParams = Map.singleton "paths" pathsList,
                        reqPayload = Nothing
                    }

                    -- Send request
                    response <- liftIO $ Client.sendRequestSync conn request 30000000
                    case response of
                        Left err -> throwError err
                        Right resp ->
                            if respStatus resp == "ok"
                                then return ()
                                else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot mark paths as invalid without daemon connection"

-- | Type class for database management operations
class CanManageDatabase (t :: PrivilegeTier) where
    -- | Vacuum the reference database to optimize performance
    vacuumReferenceDbImpl :: Database t -> TenM p t ()

    -- | Validate and repair the reference database
    validateReferenceDbImpl :: Database t -> FilePath -> TenM p t (Int, Int)

    -- | Cleanup dangling references
    cleanupDanglingReferencesImpl :: Database t -> TenM p t Int

-- | Daemon implementation for database management
instance CanManageDatabase 'Daemon where
    vacuumReferenceDbImpl db = do
        -- Remove dangling references
        cleanupDanglingReferences db

        -- Analyze tables
        tenExecuteSimple_ db "ANALYZE References"

        -- Vacuum database
        tenExecuteSimple_ db "VACUUM"

    validateReferenceDbImpl db storeDir = do
        -- Count existing references
        [Only totalRefs] <- tenQuery_ db "SELECT COUNT(*) FROM References" :: TenM p 'Daemon [Only Int]

        -- Remove references to non-existent paths
        invalid <- cleanupDanglingReferences db

        -- Return statistics
        return (totalRefs, invalid)

    cleanupDanglingReferencesImpl db = withTenTransaction db ReadWrite $ \db' -> do
        -- Find references to paths that don't exist in ValidPaths
        dangling <- tenQuery_ db'
            "SELECT referrer, reference FROM References \
            \WHERE reference NOT IN (SELECT path FROM ValidPaths WHERE is_valid = 1)" :: TenM p 'Daemon [(Text, Text)]

        -- Delete these references
        count <- foldM (\acc (from, to) -> do
            tenExecute_ db'
                "DELETE FROM References WHERE referrer = ? AND reference = ?"
                (from, to)
            return $! acc + 1) 0 dangling

        return count

-- | Builder implementation for database management (via protocol)
instance CanManageDatabase 'Builder where
    vacuumReferenceDbImpl _ = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "vacuum-reference-db",
                    reqParams = Map.empty,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then return ()
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot vacuum database without daemon connection"

    validateReferenceDbImpl _ storeDir = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "validate-reference-db",
                    reqParams = Map.singleton "storeDir" (T.pack storeDir),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then case (Map.lookup "totalRefs" (respData resp),
                                       Map.lookup "invalid" (respData resp)) of
                                (Just totalText, Just invalidText) ->
                                    case (reads (T.unpack totalText), reads (T.unpack invalidText)) of
                                        ([(total, "")], [(invalid, "")]) -> return (total, invalid)
                                        _ -> return (0, 0)
                                _ -> return (0, 0)
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot validate database without daemon connection"

    cleanupDanglingReferencesImpl _ = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,
                    reqType = "cleanup-dangling-references",
                    reqParams = Map.empty,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ Client.sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right resp ->
                        if respStatus resp == "ok"
                            then case Map.lookup "count" (respData resp) of
                                Just countText -> case reads (T.unpack countText) of
                                    [(count, "")] -> return count
                                    _ -> return 0
                                Nothing -> return 0
                            else throwError $ DBError $ respMessage resp

            _ -> throwError $ privilegeError "Cannot cleanup dangling references without daemon connection"

-- | Context-aware reference registration
registerReference :: (CanRegisterReferences t) => Database t -> StorePath -> StorePath -> TenM p t ()
registerReference = registerReferenceImpl

-- | Context-aware reference registration (multiple)
registerReferences :: (CanRegisterReferences t) => Database t -> StorePath -> Set StorePath -> TenM p t ()
registerReferences = registerReferencesImpl

-- | Context-aware path reference registration
registerPathReferences :: (CanRegisterReferences t) => Database t -> FilePath -> StorePath -> TenM p t Int
registerPathReferences = registerPathReferencesImpl

-- | Context-aware direct reference query
getDirectReferences :: (CanQueryReferences t) => Database t -> StorePath -> TenM p t (Set StorePath)
getDirectReferences = getDirectReferencesImpl

-- | Context-aware full reference query
getReferences :: (CanQueryReferences t) => Database t -> StorePath -> TenM p t (Set StorePath)
getReferences = getReferencesImpl

-- | Context-aware direct referrer query
getDirectReferrers :: (CanQueryReferences t) => Database t -> StorePath -> TenM p t (Set StorePath)
getDirectReferrers = getDirectReferrersImpl

-- | Context-aware full referrer query
getReferrers :: (CanQueryReferences t) => Database t -> StorePath -> TenM p t (Set StorePath)
getReferrers = getReferrersImpl

-- | Context-aware compute reachable paths
computeReachablePathsFromRoots :: (CanManageReachability t) => Database t -> Set StorePath -> TenM p t (Set StorePath)
computeReachablePathsFromRoots = computeReachablePathsImpl

-- | Context-aware path closure
findPathsClosure :: (CanManageReachability t) => Database t -> Set StorePath -> TenM p t (Set StorePath)
findPathsClosure = findPathsClosureImpl

-- | Context-aware path closure with limit
findPathsClosureWithLimit :: (CanManageReachability t) => Database t -> Set StorePath -> Int -> TenM p t (Set StorePath)
findPathsClosureWithLimit = findPathsClosureWithLimitImpl

-- | Context-aware path reachability check
isPathReachable :: (CanManageReachability t) => Database t -> Set StorePath -> StorePath -> TenM p t Bool
isPathReachable = isPathReachableImpl

-- | Context-aware GC roots finder
findGCRoots :: (CanManageGCRoots t) => Database t -> FilePath -> TenM p t (Set StorePath)
findGCRoots = findGCRootsImpl

-- | Context-aware get registered roots
getRegisteredRoots :: (CanManageGCRoots t) => Database t -> TenM p t (Set StorePath)
getRegisteredRoots = getRegisteredRootsImpl

-- | Context-aware file reference scanner
scanFileForReferences :: (CanScanReferences t) => FilePath -> FilePath -> TenM p t (Set StorePath)
scanFileForReferences = scanFileForReferencesImpl

-- | Context-aware store reference scanner
scanStoreForReferences :: (CanScanReferences t) => FilePath -> TenM p t (Set StorePathReference)
scanStoreForReferences = scanStoreForReferencesImpl

-- | Context-aware mark paths as valid
markPathsAsValid :: (CanManageValidPaths t) => Database t -> Set StorePath -> TenM p t ()
markPathsAsValid = markPathsAsValidImpl

-- | Context-aware mark paths as invalid
markPathsAsInvalid :: (CanManageValidPaths t) => Database t -> Set StorePath -> TenM p t ()
markPathsAsInvalid = markPathsAsInvalidImpl

-- | Context-aware vacuum reference database
vacuumReferenceDb :: (CanManageDatabase t) => Database t -> TenM p t ()
vacuumReferenceDb = vacuumReferenceDbImpl

-- | Context-aware validate reference database
validateReferenceDb :: (CanManageDatabase t) => Database t -> FilePath -> TenM p t (Int, Int)
validateReferenceDb = validateReferenceDbImpl

-- | Context-aware cleanup dangling references
cleanupDanglingReferences :: (CanManageDatabase t) => Database t -> TenM p t Int
cleanupDanglingReferences = cleanupDanglingReferencesImpl
