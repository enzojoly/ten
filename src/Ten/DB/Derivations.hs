{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Ten.DB.Derivations (
    -- Store/retrieve derivations
    storeDerivation,
    retrieveDerivation,
    registerDerivationFile,
    storeDerivationInDB,
    retrieveDerivationByHash,
    requestDerivationByHash,

    -- Output mapping
    registerDerivationOutput,
    getOutputsForDerivation,
    getDerivationForOutput,
    findDerivationsByOutputs,

    -- Reference tracking
    addDerivationReference,
    getDerivationReferences,
    getReferrers,
    getTransitiveReferences,
    getTransitiveReferrers,

    -- Query functions
    isDerivationRegistered,
    listRegisteredDerivations,
    getDerivationPath,

    -- Path validity
    registerValidPath,
    invalidatePath,
    isPathValid,
    getPathInfo,

    -- Bulk operations
    registerDerivationWithOutputs,
    bulkRegisterReferences,

    -- Types
    DerivationInfo(..),
    OutputInfo(..),
    PathInfo(..)
) where

import Control.Exception (try, catch, throwIO, finally, SomeException)
import Control.Monad (forM, forM_, when, unless, void, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Except (throwError, catchError)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Typeable (typeOf)
import Database.SQLite.Simple (NamedParam(..), Query(..), ToRow(..), FromRow(..), Only(..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import System.FilePath ((</>), takeFileName)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (withFile, IOMode(..))
import System.Posix.Files (getFileStatus, fileSize)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector

import Ten.Core

-- | Information about a stored derivation
data DerivationInfo = DerivationInfo {
    derivInfoId :: Int64,            -- ^ Database ID
    derivInfoHash :: Text,           -- ^ Derivation hash
    derivInfoStorePath :: StorePath, -- ^ Path in the store
    derivInfoTimestamp :: Int64      -- ^ Unix timestamp when registered
} deriving (Show, Eq)

-- | Information about a derivation output
data OutputInfo = OutputInfo {
    outputInfoDerivationId :: Int64,  -- ^ Database ID of the derivation
    outputInfoName :: Text,           -- ^ Output name
    outputInfoPath :: StorePath       -- ^ Path in the store
} deriving (Show, Eq)

-- | Information about a store path
data PathInfo = PathInfo {
    pathInfoPath :: StorePath,          -- ^ The store path
    pathInfoHash :: Text,               -- ^ Hash part of path
    pathInfoDeriver :: Maybe StorePath, -- ^ Derivation that produced this path (if known)
    pathInfoRegistrationTime :: UTCTime, -- ^ When it was registered
    pathInfoIsValid :: Bool             -- ^ Whether path is still valid
} deriving (Show, Eq)

-- Make DerivationInfo an instance of FromRow
instance FromRow DerivationInfo where
    fromRow = DerivationInfo
        <$> field
        <*> field
        <*> parseStorePathField
        <*> field

-- Make OutputInfo an instance of FromRow
instance FromRow OutputInfo where
    fromRow = OutputInfo
        <$> field
        <*> field
        <*> parseStorePathField

-- Make StorePathReference an instance of FromRow
instance FromRow StorePathReference where
    fromRow = StorePathReference
        <$> parseStorePathField
        <*> parseStorePathField

-- Make PathInfo an instance of FromRow
instance FromRow PathInfo where
    fromRow = do
        path <- parseStorePathField
        hash <- field :: RowParser Text
        deriverText <- field :: RowParser (Maybe Text)
        timestamp <- field :: RowParser Int64
        isValid <- field :: RowParser Int

        -- Parse deriver if present
        let deriver = deriverText >>= parseStorePath

        -- Convert timestamp to UTCTime
        let regTime = posixSecondsToUTCTime (fromIntegral timestamp)

        return $ PathInfo path hash deriver regTime (isValid == 1)

-- Helper function to parse StorePath from a database field
parseStorePathField :: RowParser StorePath
parseStorePathField = do
    text <- field :: RowParser Text
    case parseStorePath text of
        Just path -> return path
        Nothing -> fail $ "Invalid store path: " ++ T.unpack text

-- | Store a derivation in the database - daemon operation
storeDerivation :: Database -> Derivation -> StorePath -> TenM p 'Daemon Int64
storeDerivation db derivation storePath = do
    -- Check if derivation already exists
    existing <- isDerivationRegistered db (derivHash derivation)

    if existing
        then do
            -- Get the ID
            rows <- tenQuery db
                "SELECT id FROM Derivations WHERE hash = ?"
                (Only (derivHash derivation))
            case rows of
                [Only derivId] -> return derivId
                _ -> throwError $ DBError "Failed to retrieve derivation ID"
        else do
            -- Insert new derivation
            derivId <- tenExecute db
                "INSERT INTO Derivations (hash, store_path, timestamp) VALUES (?, ?, strftime('%s','now'))"
                (derivHash derivation, storePathToText storePath)

            -- Register the derivation path as valid
            registerValidPath db storePath (Just storePath)

            return derivId

-- | New function to handle database operations for storing a derivation
-- This replaces Ten.Derivation.storeDerivationFile's database operations
storeDerivationInDB :: Derivation -> StorePath -> FilePath -> TenM p 'Daemon Int64
storeDerivationInDB drv derivPath dbPath = do
    -- Initialize database
    db <- liftIO $ initDatabase dbPath 5000

    -- Register the derivation file using the existing function
    derivId <- registerDerivationFile db drv derivPath

    -- Close the database
    liftIO $ closeDatabase db

    return derivId

-- | Register a derivation file in the database with proper reference tracking - daemon operation
registerDerivationFile :: Database -> Derivation -> StorePath -> TenM p 'Daemon Int64
registerDerivationFile db derivation storePath = withTenTransaction db ReadWrite $ \_ -> do
    -- Store in database
    derivId <- storeDerivation db derivation storePath

    -- Register outputs
    outputPaths <- forM (Set.toList $ derivOutputs derivation) $ \output -> do
        let outPath = outputPath output
        registerDerivationOutput db derivId (outputName output) outPath

        -- Register this output path as valid
        registerValidPath db outPath (Just storePath)

        -- Return the output path for reference tracking
        return outPath

    -- Register references from derivation to inputs
    forM_ (Set.toList $ derivInputs derivation) $ \input -> do
        addDerivationReference db storePath (inputPath input)

    -- Register references from outputs to derivation (metadata)
    forM_ outputPaths $ \outPath -> do
        addDerivationReference db outPath storePath

    -- Register references from outputs to inputs (direct dependencies)
    forM_ outputPaths $ \outPath -> do
        forM_ (Set.toList $ derivInputs derivation) $ \input -> do
            addDerivationReference db outPath (inputPath input)

    return derivId

-- | Retrieve a derivation from the database - daemon operation
retrieveDerivation :: Database -> Text -> TenM p 'Daemon (Maybe Derivation)
retrieveDerivation db hash = do
    env <- ask

    -- Query for derivation
    derivRows <- tenQuery db
        "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d WHERE d.hash = ?"
        (Only hash)

    case derivRows of
        [] -> return Nothing
        [derivInfo] -> do
            -- Get the derivation store path
            let storePath = derivInfoStorePath derivInfo

            -- Read and deserialize the derivation file
            derivContent <- liftIO $ readDerivationFile env storePath

            case derivContent of
                Left err -> do
                    -- Log error
                    logMsg 1 $ "Error reading derivation file: " <> err
                    return Nothing
                Right drv -> return $ Just drv
        _ -> throwError $ DBError "Multiple derivations with same hash - database corruption"

-- | Retrieve a derivation by hash - this is now properly context-aware
retrieveDerivationByHash :: FilePath -> Text -> TenM p 'Daemon (Maybe Derivation)
retrieveDerivationByHash dbPath hash = do
    env <- ask

    -- Open database
    db <- liftIO $ initDatabase dbPath 5000

    -- Query for store path - explicitly in the daemon context
    storePaths <- tenQuery db
        "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
        (Only hash)

    -- Get the derivation
    result <- case storePaths of
        [Only pathText] ->
            case parseStorePath pathText of
                Just sp -> readDerivationFromStore env sp
                Nothing -> return Nothing
        _ -> return Nothing

    -- Close database
    liftIO $ closeDatabase db

    -- Return the result
    return result

-- | Request derivation by hash - builder operation using protocol
requestDerivationByHash :: Text -> TenM p 'Builder (Maybe Derivation)
requestDerivationByHash hash = do
    env <- ask

    -- Must use daemon connection to request derivation
    case runMode env of
        ClientMode conn -> do
            -- Create a request
            let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "retrieve-derivation",
                    reqParams = Map.fromList [
                        ("hash", hash),
                        ("includeOutputs", "true")
                    ],
                    reqPayload = Nothing
                }

            -- Send request through daemon connection
            reqId <- liftIO $ sendRequest conn request
            responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

            -- Handle response
            case responseResult of
                Left err -> throwError err
                Right response ->
                    if respStatus response == "ok"
                        then case respPayload response of
                            Just content ->
                                -- Deserialize derivation
                                case Ten.Core.deserializeDerivation content of
                                    Left err -> throwError $ SerializationError $ "Failed to deserialize derivation: " <> errorToText err
                                    Right drv -> return $ Just drv
                            Nothing -> return Nothing
                        else throwError $ DaemonError $ respMessage response

        _ -> throwError $ BuildFailed "Cannot request derivation: not connected to daemon"

-- | Helper to read derivation from store - available in both contexts with different implementations
readDerivationFromStore :: BuildEnv -> StorePath -> TenM p t (Maybe Derivation)
readDerivationFromStore env sp = do
    -- Implementation depends on privilege tier
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context can access store directly
        Daemon -> do
            -- Direct store access
            let fullPath = storePathToFilePath sp env

            -- Check if file exists
            exists <- liftIO $ doesFileExist fullPath
            if not exists
                then return Nothing
                else do
                    -- Read and deserialize
                    content <- liftIO $ BS.readFile fullPath
                    case Ten.Core.deserializeDerivation content of
                        Left _ -> return Nothing
                        Right drv -> return $ Just drv

        -- Builder context must use protocol
        Builder -> do
            -- Must use daemon connection
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "retrieve-derivation",
                            reqParams = Map.singleton "path" (storePathToText sp),
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then case respPayload response of
                                    Just content ->
                                        -- Deserialize derivation
                                        case Ten.Core.deserializeDerivation content of
                                            Left err -> return Nothing
                                            Right drv -> return $ Just drv
                                    Nothing -> return Nothing
                                else return Nothing

                _ -> return Nothing  -- Cannot access without daemon connection

-- | Local helper function to deserialize a derivation
-- This avoids the ambiguous import issue
localDeserializeDerivation :: ByteString -> Either Text Derivation
localDeserializeDerivation bs =
    Ten.Core.deserializeDerivation bs

-- | Read and deserialize a derivation file - daemon operation
readDerivationFile :: BuildEnv -> StorePath -> IO (Either Text Derivation)
readDerivationFile env path = do
    -- Construct the full path
    let filePath = storePathToFilePath path env

    -- Check if file exists
    exists <- doesFileExist filePath
    if not exists
        then return $ Left $ "Derivation file does not exist: " <> T.pack filePath
        else do
            -- Read the file
            content <- BS.readFile filePath
            -- Return the result of deserialization
            return $ Ten.Core.deserializeDerivation content

-- | Register an output for a derivation - daemon operation
registerDerivationOutput :: Database -> Int64 -> Text -> StorePath -> TenM p 'Daemon ()
registerDerivationOutput db derivId outputName outPath = do
    -- Insert output record
    void $ tenExecute db
        "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) VALUES (?, ?, ?)"
        (derivId, outputName, storePathToText outPath)

-- | Get all outputs for a derivation - available in both contexts with different implementations
getOutputsForDerivation :: Database -> Int64 -> TenM p t [OutputInfo]
getOutputsForDerivation db derivId = do
    -- Implementation depends on privilege tier
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon ->
            tenQuery db
                "SELECT derivation_id, output_name, path FROM Outputs WHERE derivation_id = ?"
                (Only derivId)

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "query-derivation",
                            reqParams = Map.fromList [
                                ("queryType", "outputs"),
                                ("queryValue", T.pack $ show derivId)
                            ],
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    -- Parse outputs from response data
                                    let outputTexts = T.splitOn "," $
                                            Map.findWithDefault "" "outputs" (respData response)
                                        paths = catMaybes $ map parseStorePath $
                                            filter (not . T.null) outputTexts
                                    in
                                        -- Convert paths to OutputInfo (simplified)
                                        return $ zipWith
                                            (\i path -> OutputInfo derivId
                                                (T.pack $ "output" ++ show i) path)
                                            [1..] paths
                                else return []

                _ -> throwError $ BuildFailed "Cannot query outputs: not connected to daemon"

-- | Find the derivation that produced a particular output - daemon operation
getDerivationForOutput :: Database -> StorePath -> TenM p 'Daemon (Maybe DerivationInfo)
getDerivationForOutput db outputPath = do
    results <- tenQuery db
        "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
        \JOIN Outputs o ON d.id = o.derivation_id \
        \WHERE o.path = ?"
        (Only (storePathToText outputPath))

    return $ listToMaybe results

-- | Find derivations that produced the given outputs - appropriately context-aware
findDerivationsByOutputs :: Database -> [StorePath] -> TenM p t (Map String Derivation)
findDerivationsByOutputs db [] = return Map.empty
findDerivationsByOutputs db outputPaths = do
    env <- ask
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            -- Convert StorePath to text for query
            let pathTexts = map storePathToText outputPaths

            -- Use a better approach for handling the IN clause
            let placeholders = "(" ++ intercalate ", " (replicate (length pathTexts) "?") ++ ")"
            let queryStr = "SELECT DISTINCT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
                          \JOIN Outputs o ON d.id = o.derivation_id \
                          \WHERE o.path IN " ++ placeholders

            derivInfos <- tenQuery db (Query $ T.pack queryStr) pathTexts

            -- Now load each derivation
            derivations <- forM derivInfos $ \derivInfo -> do
                mDrv <- retrieveDerivation db (derivInfoHash derivInfo)
                case mDrv of
                    Nothing -> return Nothing
                    Just drv -> return $ Just (T.unpack (derivInfoHash derivInfo), drv)

            -- Filter out Nothings and convert to Map
            return $ Map.fromList $ catMaybes derivations

        -- Builder context must use protocol
        Builder -> do
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let pathText = T.intercalate "," $ map storePathToText outputPaths
                        request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "get-derivation-for-output",
                            reqParams = Map.singleton "paths" pathText,
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    -- If payload contains derivations
                                    case respPayload response of
                                        Just content ->
                                            -- Deserialize derivations (assuming a single derivation for simplicity)
                                            -- A real implementation would handle multiple derivations
                                            case Ten.Core.deserializeDerivation content of
                                                Left _ -> return Map.empty
                                                Right drv -> return $ Map.singleton (T.unpack $ derivHash drv) drv
                                        Nothing -> return Map.empty
                                else return Map.empty

                _ -> throwError $ BuildFailed "Cannot find derivations: not connected to daemon"

-- | Add a reference between two paths - daemon operation
addDerivationReference :: Database -> StorePath -> StorePath -> TenM p 'Daemon ()
addDerivationReference db referrer reference =
    when (referrer /= reference) $ do
        -- Skip self-references
        void $ tenExecute db
            "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
            (storePathToText referrer, storePathToText reference)

-- | Bulk register multiple references for efficiency - daemon operation
bulkRegisterReferences :: Database -> [(StorePath, StorePath)] -> TenM p 'Daemon Int
bulkRegisterReferences db [] = return 0
bulkRegisterReferences db references = withTenTransaction db ReadWrite $ \_ -> do
    -- Filter out self-references
    let validRefs = filter (\(from, to) -> from /= to) references

    -- Insert each reference
    count <- foldM (\acc (from, to) -> do
        void $ tenExecute db
            "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
            (storePathToText from, storePathToText to)
        return $! acc + 1) 0 validRefs

    return count

-- | Get all direct references from a path - available in both contexts
getDerivationReferences :: Database -> StorePath -> TenM p t [StorePath]
getDerivationReferences db path = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            -- Direct database access
            results <- tenQuery db
                "SELECT reference FROM References WHERE referrer = ?"
                (Only (storePathToText path))

            -- Parse each path
            return $ catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "query-derivation",
                            reqParams = Map.fromList [
                                ("queryType", "references"),
                                ("queryValue", storePathToText path)
                            ],
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    -- Parse paths from response data
                                    let refTexts = T.splitOn "," $
                                            Map.findWithDefault "" "references" (respData response)
                                    in
                                        return $ catMaybes $ map parseStorePath $
                                            filter (not . T.null) refTexts
                                else return []

                _ -> throwError $ BuildFailed "Cannot get references: not connected to daemon"

-- | Get all direct referrers to a path - available in both contexts
getReferrers :: Database -> StorePath -> TenM p t [StorePath]
getReferrers db path = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            results <- tenQuery db
                "SELECT referrer FROM References WHERE reference = ?"
                (Only (storePathToText path))

            -- Parse each path
            return $ catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "query-derivation",
                            reqParams = Map.fromList [
                                ("queryType", "referrers"),
                                ("queryValue", storePathToText path)
                            ],
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    -- Parse paths from response data
                                    let refTexts = T.splitOn "," $
                                            Map.findWithDefault "" "referrers" (respData response)
                                    in
                                        return $ catMaybes $ map parseStorePath $
                                            filter (not . T.null) refTexts
                                else return []

                _ -> throwError $ BuildFailed "Cannot get referrers: not connected to daemon"

-- | Get all transitive references (closure) - available in both contexts
getTransitiveReferences :: Database -> StorePath -> TenM p t (Set StorePath)
getTransitiveReferences db path = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            -- Use recursive CTE to efficiently query the closure
            let query = "WITH RECURSIVE\n\
                        \  closure(p) AS (\n\
                        \    VALUES(?)\n\
                        \    UNION\n\
                        \    SELECT reference FROM References JOIN closure ON referrer = p\n\
                        \  )\n\
                        \SELECT p FROM closure WHERE p != ?"

            results <- tenQuery db query (storePathToText path, storePathToText path)

            -- Parse paths and return as set
            return $ Set.fromList $ catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "query-derivation",
                            reqParams = Map.fromList [
                                ("queryType", "transitive-references"),
                                ("queryValue", storePathToText path)
                            ],
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    -- Parse paths from response data
                                    let refTexts = T.splitOn "," $
                                            Map.findWithDefault "" "references" (respData response)
                                        paths = catMaybes $ map parseStorePath $
                                            filter (not . T.null) refTexts
                                    in
                                        return $ Set.fromList paths
                                else return Set.empty

                _ -> throwError $ BuildFailed "Cannot get transitive references: not connected to daemon"

-- | Get all transitive referrers (reverse closure) - available in both contexts
getTransitiveReferrers :: Database -> StorePath -> TenM p t (Set StorePath)
getTransitiveReferrers db path = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            -- Use recursive CTE to efficiently query the closure
            let query = "WITH RECURSIVE\n\
                        \  closure(p) AS (\n\
                        \    VALUES(?)\n\
                        \    UNION\n\
                        \    SELECT referrer FROM References JOIN closure ON reference = p\n\
                        \  )\n\
                        \SELECT p FROM closure WHERE p != ?"

            results <- tenQuery db query (storePathToText path, storePathToText path)

            -- Parse paths and return as set
            return $ Set.fromList $ catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "query-derivation",
                            reqParams = Map.fromList [
                                ("queryType", "transitive-referrers"),
                                ("queryValue", storePathToText path)
                            ],
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    -- Parse paths from response data
                                    let refTexts = T.splitOn "," $
                                            Map.findWithDefault "" "referrers" (respData response)
                                        paths = catMaybes $ map parseStorePath $
                                            filter (not . T.null) refTexts
                                    in
                                        return $ Set.fromList paths
                                else return Set.empty

                _ -> throwError $ BuildFailed "Cannot get transitive referrers: not connected to daemon"

-- | Check if a derivation is already registered - available in both contexts
isDerivationRegistered :: Database -> Text -> TenM p t Bool
isDerivationRegistered db hash = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            results <- tenQuery db
                "SELECT 1 FROM Derivations WHERE hash = ? LIMIT 1"
                (Only hash)
            return $ not (null results)

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "query-derivation",
                            reqParams = Map.fromList [
                                ("queryType", "exists"),
                                ("queryValue", hash)
                            ],
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            return $ Map.findWithDefault "false" "exists" (respData response) == "true"

                _ -> throwError $ BuildFailed "Cannot check derivation: not connected to daemon"

-- | List all registered derivations - available in both contexts
listRegisteredDerivations :: Database -> TenM p t [DerivationInfo]
listRegisteredDerivations db = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon ->
            tenQuery_ db "SELECT id, hash, store_path, timestamp FROM Derivations ORDER BY timestamp DESC"

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "list-derivations",
                            reqParams = Map.singleton "limit" "100",  -- Reasonable limit
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    -- Parse derivation paths from response data
                                    let pathTexts = T.splitOn "," $
                                            Map.findWithDefault "" "paths" (respData response)
                                        paths = catMaybes $ map parseStorePath $
                                            filter (not . T.null) pathTexts
                                    in
                                        -- Convert paths to basic DerivationInfo objects
                                        return $ zipWith
                                            (\i p -> DerivationInfo i
                                                (storeHash p)
                                                p
                                                0)
                                            [1..] paths
                                else return []

                _ -> throwError $ BuildFailed "Cannot list derivations: not connected to daemon"

-- | Get the store path for a derivation by hash - available in both contexts
getDerivationPath :: Database -> Text -> TenM p t (Maybe StorePath)
getDerivationPath db hash = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            results <- tenQuery db
                "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
                (Only hash)

            case results of
                [Only path] -> return $ parseStorePath path
                _ -> return Nothing

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "query-derivation",
                            reqParams = Map.fromList [
                                ("queryType", "path"),
                                ("queryValue", hash)
                            ],
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    let pathText = Map.findWithDefault "" "path" (respData response)
                                    in
                                        if T.null pathText
                                            then return Nothing
                                            else return $ parseStorePath pathText
                                else return Nothing

                _ -> throwError $ BuildFailed "Cannot get derivation path: not connected to daemon"

-- | Register a valid path in the store - daemon operation
registerValidPath :: Database -> StorePath -> Maybe StorePath -> TenM p 'Daemon ()
registerValidPath db path mDeriver = do
    let deriverText = case mDeriver of
            Just deriver -> Just (storePathToText deriver)
            Nothing -> Nothing

    void $ tenExecute db
        "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time, deriver, is_valid) \
        \VALUES (?, ?, strftime('%s','now'), ?, 1)"
        (storePathToText path, storeHash path, deriverText)

-- | Mark a path as invalid - daemon operation
invalidatePath :: Database -> StorePath -> TenM p 'Daemon ()
invalidatePath db path =
    void $ tenExecute db
        "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
        (Only (storePathToText path))

-- | Check if a path is valid - available in both contexts
isPathValid :: Database -> StorePath -> TenM p t Bool
isPathValid db path = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            results <- tenQuery db
                "SELECT is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
                (Only (storePathToText path))

            case results of
                [Only valid] -> return (valid == 1)
                _ -> return False

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "store-verify",
                            reqParams = Map.singleton "path" (storePathToText path),
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            return $ respStatus response == "ok" &&
                                     Map.findWithDefault "false" "valid" (respData response) == "true"

                _ -> throwError $ BuildFailed "Cannot check path validity: not connected to daemon"

-- | Get detailed information about a path - available in both contexts
getPathInfo :: Database -> StorePath -> TenM p t (Maybe PathInfo)
getPathInfo db path = do
    tier <- asks currentPrivilegeTier

    case tier of
        -- Daemon context has direct database access
        Daemon -> do
            results <- tenQuery db
                "SELECT path, hash, deriver, registration_time, is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
                (Only (storePathToText path))

            return $ listToMaybe results

        -- Builder context must use protocol
        Builder -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create a request
                    let request = Request {
                            reqId = 0,  -- Will be set by sendRequest
                            reqType = "store-verify",
                            reqParams = Map.singleton "path" (storePathToText path),
                            reqPayload = Nothing
                        }

                    -- Send request through daemon connection
                    reqId <- liftIO $ sendRequest conn request
                    responseResult <- liftIO $ receiveResponse conn reqId 30000000 -- 30 seconds timeout

                    -- Handle response - simplified implementation
                    case responseResult of
                        Left err -> throwError err
                        Right response ->
                            if respStatus response == "ok"
                                then
                                    let valid = Map.findWithDefault "false" "valid" (respData response) == "true"
                                    in
                                        if valid
                                            then return $ Just (PathInfo
                                                    path
                                                    (storeHash path)
                                                    Nothing
                                                    (posixSecondsToUTCTime 0)
                                                    True)
                                            else return Nothing
                                else return Nothing

                _ -> throwError $ BuildFailed "Cannot get path info: not connected to daemon"

-- | Register a derivation with all its outputs in a single transaction - daemon operation
registerDerivationWithOutputs :: Database -> Derivation -> StorePath -> TenM p 'Daemon Int64
registerDerivationWithOutputs db derivation storePath = withTenTransaction db ReadWrite $ \_ -> do
    -- Register the derivation
    derivId <- storeDerivation db derivation storePath

    -- Get all outputs and inputs for reference tracking
    let outputs = Set.map outputPath $ derivOutputs derivation
    let inputs = Set.map inputPath $ derivInputs derivation

    -- Register all outputs
    forM_ (Set.toList $ derivOutputs derivation) $ \output -> do
        let outPath = outputPath output
        registerDerivationOutput db derivId (outputName output) outPath

        -- Register this as a valid path
        registerValidPath db outPath (Just storePath)

    -- Register all input references
    forM_ (Set.toList $ derivInputs derivation) $ \input -> do
        addDerivationReference db storePath (inputPath input)

    -- Register references from outputs to inputs (direct dependencies)
    forM_ (Set.toList outputs) $ \outPath -> do
        -- Reference from output to derivation
        addDerivationReference db outPath storePath

        -- References from output to each input
        forM_ (Set.toList inputs) $ \input -> do
            addDerivationReference db outPath input

    return derivId

-- | Helper function to send a daemon request in builder context
sendDaemonRequest :: DaemonConnection t -> Request -> TenM p 'Builder Response
sendDaemonRequest conn request = do
    -- Generate a request ID
    reqId <- liftIO $ atomically $ do
        currentId <- readTVar (connNextReqId conn)
        writeTVar (connNextReqId conn) (currentId + 1)
        return currentId

    -- Create a response MVar
    responseMVar <- liftIO $ newEmptyMVar

    -- Register the request
    liftIO $ atomically $ modifyTVar' (connRequestMap conn) $
        Map.insert reqId responseMVar

    -- Update the request with the ID
    let request' = request { reqId = reqId }

    -- Serialize and send the request
    liftIO $ do
        BS.hPut (connHandle conn) (encodeRequest request')
        hFlush (connHandle conn)

    -- Wait for response with a reasonable timeout (30 seconds)
    result <- liftIO $ timeout 30000000 $ takeMVar responseMVar

    -- Clean up the request entry
    liftIO $ atomically $ modifyTVar' (connRequestMap conn) $
        Map.delete reqId

    -- Return the response or error
    case result of
        Just resp -> return resp
        Nothing -> throwError $ DaemonError "Timeout waiting for daemon response"
