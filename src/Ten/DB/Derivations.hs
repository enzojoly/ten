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
import Ten.DB.Core
import Ten.Daemon.Protocol (DaemonRequest(..), DaemonResponse(..))

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
parseStorePathField = field

-- | Store a derivation in the database - privileged operation
storeDerivation :: Database -> Derivation -> StorePath -> TenM p 'Privileged Int64
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
storeDerivationInDB :: Derivation -> StorePath -> FilePath -> TenM p 'Privileged Int64
storeDerivationInDB drv derivPath dbPath = do
    -- Initialize database
    db <- liftIO $ initDatabase dbPath 5000

    -- Register the derivation file using the existing function
    derivId <- registerDerivationFile db drv derivPath

    -- Close the database
    liftIO $ closeDatabase db

    return derivId

-- | Register a derivation file in the database with proper reference tracking - privileged operation
registerDerivationFile :: Database -> Derivation -> StorePath -> TenM p 'Privileged Int64
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

-- | Retrieve a derivation from the database - privileged operation
retrieveDerivation :: Database -> Text -> TenM p 'Privileged (Maybe Derivation)
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
retrieveDerivationByHash :: FilePath -> Text -> TenM p 'Privileged (Maybe Derivation)
retrieveDerivationByHash dbPath hash = do
    env <- ask

    -- Open database
    db <- liftIO $ initDatabase dbPath 5000

    -- Query for store path - explicitly in the privileged context
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

-- | Request derivation by hash - unprivileged operation using protocol
requestDerivationByHash :: Text -> TenM p 'Unprivileged (Maybe Derivation)
requestDerivationByHash hash = do
    env <- ask

    -- Must use daemon connection to request derivation
    case runMode env of
        ClientMode conn -> do
            -- Send request through daemon protocol
            result <- sendDaemonRequest conn (RetrieveDerivationCmd $ RetrieveDerivationRequest {
                derivationHash = hash,
                includeOutputs = True
            })

            -- Process response
            case result of
                DerivationRetrievedResponse mDrv -> return mDrv
                ErrorResponse err -> throwError err
                _ -> throwError $ ProtocolError $ "Unexpected response type for derivation request"

        _ -> throwError $ BuildFailed "Cannot request derivation: not connected to daemon"

-- | Helper to read derivation from store - available in both contexts with different implementations
readDerivationFromStore :: BuildEnv -> StorePath -> TenM p ctx (Maybe Derivation)
readDerivationFromStore env sp = do
    -- Implementation depends on privilege context
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
            -- Privileged context can access store directly
            let fullPath = storePathToFilePath sp env

            -- Check if file exists
            exists <- liftIO $ doesFileExist fullPath
            if not exists
                then return Nothing
                else do
                    -- Read and deserialize
                    content <- liftIO $ BS.readFile fullPath
                    case deserializeDerivation content of
                        Left _ -> return Nothing
                        Right drv -> return $ Just drv

        Unprivileged -> do
            -- Unprivileged context must use protocol
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (RetrieveDerivationCmd $ RetrieveDerivationRequest {
                        derivationPath = sp,
                        includeOutputs = False
                    })

                    -- Process response
                    case result of
                        DerivationRetrievedResponse mDrv -> return mDrv
                        _ -> return Nothing

                _ -> return Nothing  -- Cannot access without daemon connection

-- | Helper function to deserialize a derivation
deserializeDerivation :: ByteString -> Either Text Derivation
deserializeDerivation bs =
    case Aeson.eitherDecode (LBS.fromStrict bs) of
        Left err -> Left $ T.pack $ "JSON parse error: " ++ err
        Right json -> derivationFromJSON json

-- | Convert JSON to Derivation
derivationFromJSON :: Aeson.Value -> Either Text Derivation
derivationFromJSON value = case Aeson.parseEither parseDerivation value of
    Left err -> Left $ T.pack err
    Right drv -> Right drv
  where
    parseDerivation = Aeson.withObject "Derivation" $ \o -> do
        name <- o Aeson..: "name"
        hash <- o Aeson..: "hash"
        builderObj <- o Aeson..: "builder"
        builder <- parseStorePath' builderObj
        args <- o Aeson..: "args"
        inputsArray <- o Aeson..: "inputs"
        inputs <- parseInputs inputsArray
        outputsArray <- o Aeson..: "outputs"
        outputs <- parseOutputs outputsArray
        envObj <- o Aeson..: "env"
        env <- parseEnvMap envObj
        system <- o Aeson..: "system"
        strategyText <- o Aeson..: "strategy"
        let strategy = if strategyText == ("monadic" :: Text) then MonadicStrategy else ApplicativeStrategy
        metaObj <- o Aeson..: "meta"
        meta <- parseEnvMap metaObj

        return $ Derivation name hash builder args
                           (Set.fromList inputs) (Set.fromList outputs)
                           env system strategy meta

    parseStorePath' = Aeson.withObject "StorePath" $ \o -> do
        hash <- o Aeson..: "hash"
        name <- o Aeson..: "name"
        return $ StorePath hash name

    parseInputs = Aeson.withArray "Inputs" $ \arr ->
        mapM parseInput (Vector.toList arr)

    parseInput = Aeson.withObject "DerivationInput" $ \o -> do
        pathObj <- o Aeson..: "path"
        path <- parseStorePath' pathObj
        name <- o Aeson..: "name"
        return $ DerivationInput path name

    parseOutputs = Aeson.withArray "Outputs" $ \arr ->
        mapM parseOutput (Vector.toList arr)

    parseOutput = Aeson.withObject "DerivationOutput" $ \o -> do
        name <- o Aeson..: "name"
        pathObj <- o Aeson..: "path"
        path <- parseStorePath' pathObj
        return $ DerivationOutput name path

    parseEnvMap :: Aeson.Object -> Aeson.Parser (Map Text Text)
    parseEnvMap o =
        return $ Map.fromList [(Key.toText k, v) | (k, Aeson.String v) <- KeyMap.toList o]

-- | Read and deserialize a derivation file - privileged operation
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
            return $ deserializeDerivation content

-- | Register an output for a derivation - privileged operation
registerDerivationOutput :: Database -> Int64 -> Text -> StorePath -> TenM p 'Privileged ()
registerDerivationOutput db derivId outputName outPath = do
    -- Insert output record
    void $ tenExecute db
        "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) VALUES (?, ?, ?)"
        (derivId, outputName, storePathToText outPath)

-- | Get all outputs for a derivation - available in both contexts with different implementations
getOutputsForDerivation :: Database -> Int64 -> TenM p ctx [OutputInfo]
getOutputsForDerivation db derivId = do
    -- Implementation depends on privilege context
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged ->
            -- Direct database access
            tenQuery db
                "SELECT derivation_id, output_name, path FROM Outputs WHERE derivation_id = ?"
                (Only derivId)

        Unprivileged -> do
            -- Must use daemon protocol
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (QueryDerivationCmd $ QueryDerivationRequest {
                        queryType = "outputs",
                        queryValue = T.pack $ show derivId,
                        queryLimit = Nothing
                    })

                    -- Process response and convert to OutputInfo
                    case result of
                        DerivationOutputResponse paths ->
                            -- Convert paths to OutputInfo (simplified)
                            return $ map (\path -> OutputInfo derivId (storeName path) path) (Set.toList paths)
                        _ -> return []

                _ -> throwError $ BuildFailed "Cannot query outputs: not connected to daemon"

-- | Find the derivation that produced a particular output - privileged operation
getDerivationForOutput :: Database -> StorePath -> TenM p 'Privileged (Maybe DerivationInfo)
getDerivationForOutput db outputPath = do
    results <- tenQuery db
        "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
        \JOIN Outputs o ON d.id = o.derivation_id \
        \WHERE o.path = ?"
        (Only (storePathToText outputPath))

    return $ listToMaybe results

-- | Find derivations that produced the given outputs - appropriately context-aware
findDerivationsByOutputs :: Database -> [StorePath] -> TenM p ctx (Map String Derivation)
findDerivationsByOutputs db [] = return Map.empty
findDerivationsByOutputs db outputPaths = do
    env <- ask
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
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

        Unprivileged -> do
            -- Must use daemon protocol
            case runMode env of
                ClientMode conn -> do
                    -- Request derivations for outputs through protocol
                    let pathText = T.intercalate "," $ map storePathToText outputPaths
                    result <- sendDaemonRequest conn (GetDerivationForOutputRequest pathText)

                    -- Process response
                    case result of
                        DerivationQueryResponse drvs ->
                            return $ Map.fromList $ zip (map (T.unpack . derivHash) drvs) drvs
                        _ -> return Map.empty

                _ -> throwError $ BuildFailed "Cannot find derivations: not connected to daemon"

-- | Add a reference between two paths - privileged operation
addDerivationReference :: Database -> StorePath -> StorePath -> TenM p 'Privileged ()
addDerivationReference db referrer reference =
    when (referrer /= reference) $ do
        -- Skip self-references
        void $ tenExecute db
            "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
            (storePathToText referrer, storePathToText reference)

-- | Bulk register multiple references for efficiency - privileged operation
bulkRegisterReferences :: Database -> [(StorePath, StorePath)] -> TenM p 'Privileged Int
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
getDerivationReferences :: Database -> StorePath -> TenM p ctx [StorePath]
getDerivationReferences db path = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
            -- Direct database access
            results <- tenQuery db
                "SELECT reference FROM References WHERE referrer = ?"
                (Only (storePathToText path))

            -- Parse each path
            return $ catMaybes $ map (\(Only text) -> parseStorePath text) results

        Unprivileged -> do
            -- Must use protocol
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (QueryDerivationCmd $ QueryDerivationRequest {
                        queryType = "references",
                        queryValue = storePathToText path,
                        queryLimit = Nothing
                    })

                    -- Process response
                    case result of
                        DerivationOutputResponse paths -> return $ Set.toList paths
                        _ -> return []

                _ -> throwError $ BuildFailed "Cannot get references: not connected to daemon"

-- | Get all direct referrers to a path
getReferrers :: Database -> StorePath -> TenM p ctx [StorePath]
getReferrers db path = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
            -- Direct database access
            results <- tenQuery db
                "SELECT referrer FROM References WHERE reference = ?"
                (Only (storePathToText path))

            -- Parse each path
            return $ catMaybes $ map (\(Only text) -> parseStorePath text) results

        Unprivileged -> do
            -- Must use protocol
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (QueryDerivationCmd $ QueryDerivationRequest {
                        queryType = "referrers",
                        queryValue = storePathToText path,
                        queryLimit = Nothing
                    })

                    -- Process response
                    case result of
                        DerivationOutputResponse paths -> return $ Set.toList paths
                        _ -> return []

                _ -> throwError $ BuildFailed "Cannot get referrers: not connected to daemon"

-- | Get all transitive references (closure)
getTransitiveReferences :: Database -> StorePath -> TenM p ctx (Set StorePath)
getTransitiveReferences db path = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
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

        Unprivileged -> do
            -- Must use protocol
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (QueryDerivationCmd $ QueryDerivationRequest {
                        queryType = "transitive-references",
                        queryValue = storePathToText path,
                        queryLimit = Nothing
                    })

                    -- Process response
                    case result of
                        DerivationOutputResponse paths -> return paths
                        _ -> return Set.empty

                _ -> throwError $ BuildFailed "Cannot get transitive references: not connected to daemon"

-- | Get all transitive referrers (reverse closure)
getTransitiveReferrers :: Database -> StorePath -> TenM p ctx (Set StorePath)
getTransitiveReferrers db path = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
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

        Unprivileged -> do
            -- Must use protocol
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (QueryDerivationCmd $ QueryDerivationRequest {
                        queryType = "transitive-referrers",
                        queryValue = storePathToText path,
                        queryLimit = Nothing
                    })

                    -- Process response
                    case result of
                        DerivationOutputResponse paths -> return paths
                        _ -> return Set.empty

                _ -> throwError $ BuildFailed "Cannot get transitive referrers: not connected to daemon"

-- | Check if a derivation is already registered
isDerivationRegistered :: Database -> Text -> TenM p ctx Bool
isDerivationRegistered db hash = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
            results <- tenQuery db
                "SELECT 1 FROM Derivations WHERE hash = ? LIMIT 1"
                (Only hash)
            return $ not (null results)

        Unprivileged -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (QueryDerivationCmd $ QueryDerivationRequest {
                        queryType = "exists",
                        queryValue = hash,
                        queryLimit = Nothing
                    })

                    -- Process response - simplified, assumes boolean in some response field
                    return $ case result of
                        StoreVerifyResponse exists -> exists
                        _ -> False

                _ -> throwError $ BuildFailed "Cannot check derivation: not connected to daemon"

-- | List all registered derivations
listRegisteredDerivations :: Database -> TenM p ctx [DerivationInfo]
listRegisteredDerivations db = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged ->
            tenQuery_ db "SELECT id, hash, store_path, timestamp FROM Derivations ORDER BY timestamp DESC"

        Unprivileged -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (ListDerivationsRequest (Just 100))

                    -- Process response - simplified implementation
                    case result of
                        DerivationListResponse paths ->
                            -- Convert paths to basic DerivationInfo objects
                            return $ zipWith (\i p -> DerivationInfo i
                                                             (storeHash p)
                                                             p
                                                             0) [1..] paths
                        _ -> return []

                _ -> throwError $ BuildFailed "Cannot list derivations: not connected to daemon"

-- | Get the store path for a derivation by hash
getDerivationPath :: Database -> Text -> TenM p ctx (Maybe StorePath)
getDerivationPath db hash = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
            results <- tenQuery db
                "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
                (Only hash)

            case results of
                [Only path] -> return $ parseStorePath path
                _ -> return Nothing

        Unprivileged -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (QueryDerivationCmd $ QueryDerivationRequest {
                        queryType = "path",
                        queryValue = hash,
                        queryLimit = Nothing
                    })

                    -- Process response
                    case result of
                        StorePathResponse path -> return $ Just path
                        _ -> return Nothing

                _ -> throwError $ BuildFailed "Cannot get derivation path: not connected to daemon"

-- | Register a valid path in the store - privileged operation
registerValidPath :: Database -> StorePath -> Maybe StorePath -> TenM p 'Privileged ()
registerValidPath db path mDeriver = do
    let deriverText = case mDeriver of
            Just deriver -> Just (storePathToText deriver)
            Nothing -> Nothing

    void $ tenExecute db
        "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time, deriver, is_valid) \
        \VALUES (?, ?, strftime('%s','now'), ?, 1)"
        (storePathToText path, storeHash path, deriverText)

-- | Mark a path as invalid - privileged operation
invalidatePath :: Database -> StorePath -> TenM p 'Privileged ()
invalidatePath db path =
    void $ tenExecute db
        "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
        (Only (storePathToText path))

-- | Check if a path is valid - available in both contexts
isPathValid :: Database -> StorePath -> TenM p ctx Bool
isPathValid db path = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
            results <- tenQuery db
                "SELECT is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
                (Only (storePathToText path))

            case results of
                [Only valid] -> return (valid == 1)
                _ -> return False

        Unprivileged -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (StoreVerifyRequest (storePathToText path))

                    -- Process response
                    case result of
                        StoreVerifyResponse valid -> return valid
                        _ -> return False

                _ -> throwError $ BuildFailed "Cannot check path validity: not connected to daemon"

-- | Get detailed information about a path - available in both contexts
getPathInfo :: Database -> StorePath -> TenM p ctx (Maybe PathInfo)
getPathInfo db path = do
    ctxType <- asks privilegeContext

    case ctxType of
        Privileged -> do
            results <- tenQuery db
                "SELECT path, hash, deriver, registration_time, is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
                (Only (storePathToText path))

            return $ listToMaybe results

        Unprivileged -> do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Request through protocol
                    result <- sendDaemonRequest conn (StoreVerifyRequest (storePathToText path))

                    -- Process response - simplified implementation
                    case result of
                        StoreVerifyResponse valid ->
                            if valid
                                then return $ Just (PathInfo path (storeHash path) Nothing (posixSecondsToUTCTime 0) True)
                                else return Nothing
                        _ -> return Nothing

                _ -> throwError $ BuildFailed "Cannot get path info: not connected to daemon"

-- | Register a derivation with all its outputs in a single transaction - privileged operation
registerDerivationWithOutputs :: Database -> Derivation -> StorePath -> TenM p 'Privileged Int64
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

-- | Helper function to send a daemon request in unprivileged context
sendDaemonRequest :: DaemonConnection -> DaemonRequest -> TenM p 'Unprivileged DaemonResponse
sendDaemonRequest conn request = do
    -- This would use the actual daemon client protocol
    -- For now, a placeholder that would be implemented in Ten.Daemon.Client
    throwError $ BuildFailed "Daemon client protocol not implemented"
