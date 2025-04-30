{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Ten.DB.Derivations (
    -- Type classes for derivation operations
    CanStoreDerivation(..),
    CanRetrieveDerivation(..),
    CanManageOutputs(..),
    CanManageReferences(..),
    CanQueryDerivations(..),
    CanManagePathValidity(..),

    -- Helper functions (phase and context aware)
    storeDerivationInDB,
    readDerivationFromStore,
    listDerivations,

    -- Information types
    OutputInfo(..),
    PathInfo(..)
) where

import Control.Exception (try, catch, throwIO, finally, SomeException)
import Control.Monad (forM, forM_, when, unless, void, foldM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Control.Monad.Except (throwError, catchError)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe, catMaybes, isJust)
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
import Database.SQLite.Simple (NamedParam(..), Query(..), ToRow(..), FromRow(..), Only(..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import System.FilePath ((</>), takeFileName, takeDirectory)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO (withFile, IOMode(..))
import System.Posix.Files (getFileStatus, fileSize, setFileMode)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector

import Ten.Core
import Ten.DB.Core
import qualified Ten.Daemon.Protocol as Protocol
import qualified Ten.Derivation as Deriv

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


-- Helper function to parse StorePath from a database field without validation
parseStorePathField :: RowParser StorePath
parseStorePathField = do
    pathText <- field :: RowParser Text
    -- Extract components without validation - pure data extraction following Nix pattern
    let (hash, namePart) = T.breakOn "-" pathText
    let name = T.drop 1 namePart
    return $ StorePath hash name

-- Validate a single StorePath after extraction
validateAndLogPath :: StorePath -> TenM p t StorePath
validateAndLogPath path = do
    valid <- liftIO $ validateStorePathWithContext path "database-extraction"
    unless valid $
        logMsg 1 $ "Warning: Invalid store path from database: " <> storePathToText path
    return path

-- Validate a collection of StorePath objects after extraction
validateAndLogPaths :: [StorePath] -> TenM p t [StorePath]
validateAndLogPaths paths = do
    forM paths $ \path -> validateAndLogPath path

-- Validate DerivationInfo objects, focusing on their contained StorePath
validateAndLogDerivationInfos :: [DerivationInfo] -> TenM p t [DerivationInfo]
validateAndLogDerivationInfos derivInfos = do
    forM derivInfos $ \info -> do
        _ <- validateAndLogPath (derivationStorePath info)
        return info

listDerivations :: Int -> TenM p 'Daemon [DerivationInfo]
listDerivations limit = do
    db <- getDatabaseConn
    tenQuery db
        "SELECT id, hash, store_path, timestamp FROM Derivations ORDER BY timestamp DESC LIMIT ?"
        (Only limit)

-- Validate OutputInfo objects, focusing on their contained StorePath
validateAndLogOutputInfos :: [OutputInfo] -> TenM p t [OutputInfo]
validateAndLogOutputInfos outputInfos = do
    forM outputInfos $ \info -> do
        _ <- validateAndLogPath (outputInfoPath info)
        return info

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

-- | Extract metadata from daemon responses
daemonResponseMeta :: Either BuildError DaemonResponse -> Map Text Text
daemonResponseMeta (Left _) = Map.empty  -- No metadata for error responses
daemonResponseMeta (Right response) =
    case response of
        DerivationStoredResponse path ->
            Map.fromList [
                -- Use the existing storeHash function to get the hash part
                ("derivationId", storeHash path),
                ("path", storePathToText path)
            ]
        SuccessResponse ->
            Map.empty
        StoreAddResponse path ->
            Map.singleton "path" (storePathToText path)
        DerivationQueryResponse derivs ->
            Map.singleton "count" (T.pack $ show $ length derivs)
        DerivationRetrievedResponse mDrv ->
            let hash = maybe "" derivHash mDrv
                deriver = maybe "" (storePathToText . derivBuilder) mDrv
            in Map.fromList [
                ("hash", hash),
                ("deriver", deriver),
                ("found", T.pack $ show $ isJust mDrv)
            ]
        StoreVerifyResponse valid ->
            Map.fromList [
                ("valid", if valid then "true" else "false"),
                ("hash", "")  -- Placeholder for hash if needed
            ]
        -- Add additional response types as needed
        _ -> Map.empty  -- Default empty metadata for other responses

-- Type class definition - keep this clean
class CanStoreDerivation (t :: PrivilegeTier) where
    -- | Store a derivation in the content-addressed store and return its path
    storeDerivation :: Derivation -> TenM p t StorePath

    -- | Register a derivation in the database and return its database ID
    registerDerivationInDB :: Derivation -> StorePath -> TenM p t Int64

    -- | Register a derivation file with its dependencies
    registerDerivationFile :: Derivation -> StorePath -> TenM p t Int64

-- Then add this as a standalone function OUTSIDE the typeclass
-- | Register a derivation in the database - convenience wrapper around registerDerivationInDB
registerDerivation :: CanStoreDerivation 'Daemon => Derivation -> StorePath -> TenM p 'Daemon Int64
registerDerivation derivation storePath = registerDerivationInDB derivation storePath

-- | Daemon instance for storing derivations - direct database access
instance CanStoreDerivation 'Daemon where
    -- This should store the derivation in the content store, not just register in DB
    storeDerivation derivation = do
        env <- ask

        -- Serialize the derivation
        let serialized = Deriv.serializeDerivation derivation

        -- Calculate hash for the content
        let contentHashDigest = hashByteString serialized
        let contentHash = T.pack $ show contentHashDigest

        -- Create store path
        let name = derivName derivation <> ".drv"
        let storePath = StorePath contentHash name

        -- Create full file path
        let filePath = storePathToFilePath storePath env

        -- Check if the path already exists
        exists <- liftIO $ doesFileExist filePath
        if exists
            then return storePath
            else do
                -- Create directory structure if needed
                liftIO $ createDirectoryIfMissing True (takeDirectory filePath)

                -- Write content to file
                liftIO $ BS.writeFile filePath serialized

                -- Set correct permissions (read-only for all)
                liftIO $ setFileMode filePath 0o444

                -- Return the store path
                return storePath

    -- This registers in the database, returns ID
    registerDerivationInDB derivation storePath = do
        db <- getDatabaseConn

        -- Check if derivation already exists
        existing <- isDerivationRegistered (derivHash derivation)

        if existing
            then do
                -- Get the ID
                results <- tenQuery db
                    "SELECT id FROM Derivations WHERE hash = ?"
                    (Only (derivHash derivation)) :: TenM p 'Daemon [Only Int64]
                case results of
                    [Only derivId] -> return derivId
                    _ -> throwError $ DBError "Failed to retrieve derivation ID"
            else do
                -- Insert new derivation
                liftIO $ SQLite.execute (dbConn db)
                    "INSERT INTO Derivations (hash, store_path, timestamp) VALUES (?, ?, strftime('%s','now'))"
                    (derivHash derivation, storePathToText storePath)

                -- Get the last inserted row ID
                derivId <- liftIO $ SQLite.lastInsertRowId (dbConn db)

                -- Register the derivation path as valid
                registerValidPath storePath (Just storePath)

                return derivId

    registerDerivationFile derivation storePath = do
        db <- getDatabaseConn

        withTenWriteTransaction db $ \txDb -> do
            -- Register in database first
            derivId <- registerDerivationInDB derivation storePath

            -- Register outputs
            outputPaths <- forM (Set.toList $ derivOutputs derivation) $ \output -> do
                let outPath = outputPath output
                registerDerivationOutput derivId (outputName output) outPath

                -- Register this output path as valid
                registerValidPath outPath (Just storePath)

                -- Return the output path for reference tracking
                return outPath

            -- Register references from derivation to inputs
            forM_ (Set.toList $ derivInputs derivation) $ \input -> do
                addDerivationReference storePath (inputPath input)

            -- Register references from outputs to derivation (metadata)
            forM_ outputPaths $ \outPath -> do
                addDerivationReference outPath storePath

            -- Register references from outputs to inputs (direct dependencies)
            forM_ outputPaths $ \outPath -> do
                forM_ (Set.toList $ derivInputs derivation) $ \input -> do
                    addDerivationReference outPath (inputPath input)

            return derivId

-- | Builder implementation of CanStoreDerivation via protocol
instance CanStoreDerivation 'Builder where
    -- Store the derivation in the content store
    storeDerivation derivation = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create store request with derivation payload
                let serialized = Deriv.serializeDerivation derivation
                let request = Request {
                    reqId = 0,
                    reqType = "store-derivation",
                    reqParams = Map.singleton "name" (derivName derivation <> ".drv"),
                    reqPayload = Just serialized
                }

                -- Send request and wait for response
                response <- liftIO $ sendRequestSync conn request 30000000  -- 30 second timeout
                case response of
                    Left err -> throwError err
                    Right (DerivationStoredResponse path) -> return path
                    Right _ -> throwError $ DBError "Unexpected response from daemon"

            _ -> throwError $ privilegeError "Cannot store derivation without daemon connection"

    -- Register in the database
    registerDerivationInDB derivation storePath = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request to register in DB
                let serialized = Deriv.serializeDerivation derivation
                let request = Request {
                    reqId = 0,
                    reqType = "register-derivation-in-db",
                    reqParams = Map.singleton "storePath" (storePathToText storePath),
                    reqPayload = Just serialized
                }

                -- Send request and wait for response
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right response ->
                        -- Extract derivation ID from response metadata
                        case Map.lookup "derivationId" (daemonResponseMeta (Right response)) of
                            Just idText -> case reads (T.unpack idText) of
                                [(derivId, "")] -> return derivId
                                _ -> throwError $ DBError "Invalid derivation ID format"
                            Nothing -> throwError $ DBError "Missing derivation ID in response"

            _ -> throwError $ privilegeError "Cannot register derivation without daemon connection"

    registerDerivationFile derivation storePath = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Serialize and prepare request
                let serialized = Deriv.serializeDerivation derivation
                let request = Request {
                    reqId = 0,
                    reqType = "register-derivation-file",
                    reqParams = Map.singleton "storePath" (storePathToText storePath),
                    reqPayload = Just serialized
                }

                -- Send request and wait for response
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right response ->
                        -- Extract derivation ID from response metadata
                        case Map.lookup "derivationId" (daemonResponseMeta (Right response)) of
                            Just idText -> case reads (T.unpack idText) of
                                [(derivId, "")] -> return derivId
                                _ -> throwError $ DBError "Invalid derivation ID format"
                            Nothing -> throwError $ DBError "Missing derivation ID in response"

            _ -> throwError $ privilegeError "Cannot register derivation file without daemon connection"

-- | Type class for derivation retrieval operations
class CanRetrieveDerivation (p :: Phase) (t :: PrivilegeTier) where
    -- | Retrieve a derivation from the database
    retrieveDerivation :: Text -> TenM p t (Maybe Derivation)

    -- | Retrieve a derivation by hash
    retrieveDerivationByHash :: Text -> TenM p t (Maybe Derivation)

    -- | Find the derivation that produced a particular output
    getDerivationForOutput :: StorePath -> TenM p t (Maybe DerivationInfo)

    -- | Find derivations that produced the given outputs
    findDerivationsByOutputs :: [StorePath] -> TenM p t (Map String Derivation)

-- | Implementation for Daemon in Eval phase - direct database access
instance CanRetrieveDerivation 'Eval 'Daemon where
    retrieveDerivation hash = do
        db <- getDatabaseConn
        env <- ask

        -- Query for derivation
        derivRows <- tenQuery db
            "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d WHERE d.hash = ?"
            (Only hash) :: TenM 'Eval 'Daemon [DerivationInfo]

        -- Validate returned paths
        validatedRows <- validateAndLogDerivationInfos derivRows

        case validatedRows of
            [] -> return Nothing
            [derivInfo] -> do
                -- Get the derivation store path
                let storePath = derivationStorePath derivInfo

                -- Read and deserialize the derivation file
                derivContent <- liftIO $ readDerivationFile env storePath

                case derivContent of
                    Left err -> do
                        -- Log error
                        logMsg 1 $ "Error reading derivation file: " <> err
                        return Nothing
                    Right drv -> return $ Just drv
            _ -> throwError $ DBError "Multiple derivations with same hash - database corruption"

    retrieveDerivationByHash hash = retrieveDerivation hash

    getDerivationForOutput outputPath = do
        db <- getDatabaseConn

        results <- tenQuery db
            "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
            \JOIN Outputs o ON d.id = o.derivation_id \
            \WHERE o.path = ?"
            (Only (storePathToText outputPath)) :: TenM 'Eval 'Daemon [DerivationInfo]

        -- Validate returned paths
        validatedResults <- validateAndLogDerivationInfos results

        return $ listToMaybe validatedResults

    findDerivationsByOutputs [] = return Map.empty
    findDerivationsByOutputs outputPaths = do
        db <- getDatabaseConn
        env <- ask

        -- Convert StorePath to text for query
        let pathTexts = map storePathToText outputPaths

        -- Use a better approach for handling the IN clause
        let placeholders = "(" ++ intercalate ", " (replicate (length pathTexts) "?") ++ ")"
        let queryStr = "SELECT DISTINCT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
                      \JOIN Outputs o ON d.id = o.derivation_id \
                      \WHERE o.path IN " ++ placeholders

        derivInfos <- tenQuery db (Query $ T.pack queryStr) pathTexts :: TenM 'Eval 'Daemon [DerivationInfo]

        -- Validate store paths
        validatedDerivInfos <- validateAndLogDerivationInfos derivInfos

        -- Now load each derivation
        derivations <- forM validatedDerivInfos $ \derivInfo -> do
            mDrv <- retrieveDerivation (derivationHash derivInfo)
            case mDrv of
                Nothing -> return Nothing
                Just drv -> return $ Just (T.unpack (derivationHash derivInfo), drv)

        -- Filter out Nothings and convert to Map
        return $ Map.fromList $ catMaybes derivations

-- | Implementation for Daemon in Build phase - direct database access
instance CanRetrieveDerivation 'Build 'Daemon where
    retrieveDerivation hash = do
        db <- getDatabaseConn
        env <- ask

        -- Query for derivation
        derivRows <- tenQuery db
            "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d WHERE d.hash = ?"
            (Only hash) :: TenM 'Build 'Daemon [DerivationInfo]

        -- Validate returned paths
        validatedRows <- validateAndLogDerivationInfos derivRows

        case validatedRows of
            [] -> return Nothing
            [derivInfo] -> do
                -- Get the derivation store path
                let storePath = derivationStorePath derivInfo

                -- Read and deserialize the derivation file
                derivContent <- liftIO $ readDerivationFile env storePath

                case derivContent of
                    Left err -> do
                        -- Log error
                        logMsg 1 $ "Error reading derivation file: " <> err
                        return Nothing
                    Right drv -> return $ Just drv
            _ -> throwError $ DBError "Multiple derivations with same hash - database corruption"

    retrieveDerivationByHash hash = retrieveDerivation hash

    getDerivationForOutput outputPath = do
        db <- getDatabaseConn

        results <- tenQuery db
            "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
            \JOIN Outputs o ON d.id = o.derivation_id \
            \WHERE o.path = ?"
            (Only (storePathToText outputPath)) :: TenM 'Build 'Daemon [DerivationInfo]

        -- Validate returned paths
        validatedResults <- validateAndLogDerivationInfos results

        return $ listToMaybe validatedResults

    findDerivationsByOutputs [] = return Map.empty
    findDerivationsByOutputs outputPaths = do
        db <- getDatabaseConn
        env <- ask

        -- Convert StorePath to text for query
        let pathTexts = map storePathToText outputPaths

        -- Use a better approach for handling the IN clause
        let placeholders = "(" ++ intercalate ", " (replicate (length pathTexts) "?") ++ ")"
        let queryStr = "SELECT DISTINCT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
                      \JOIN Outputs o ON d.id = o.derivation_id \
                      \WHERE o.path IN " ++ placeholders

        derivInfos <- tenQuery db (Query $ T.pack queryStr) pathTexts :: TenM 'Build 'Daemon [DerivationInfo]

        -- Validate store paths
        validatedDerivInfos <- validateAndLogDerivationInfos derivInfos

        -- Now load each derivation
        derivations <- forM validatedDerivInfos $ \derivInfo -> do
            mDrv <- retrieveDerivation (derivationHash derivInfo)
            case mDrv of
                Nothing -> return Nothing
                Just drv -> return $ Just (T.unpack (derivationHash derivInfo), drv)

        -- Filter out Nothings and convert to Map
        return $ Map.fromList $ catMaybes derivations

-- | Implementation for Builder in Eval phase via protocol
instance CanRetrieveDerivation 'Eval 'Builder where
    retrieveDerivation hash = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Send request through daemon protocol
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "retrieve-derivation",
                    reqParams = Map.singleton "hash" hash,
                    reqPayload = Nothing
                }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationRetrievedResponse mDrv) -> return mDrv
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return Nothing  -- Unexpected response

            _ -> throwError $ privilegeError "Cannot retrieve derivation without daemon connection"

    retrieveDerivationByHash hash = retrieveDerivation hash

    getDerivationForOutput path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Send request through daemon protocol
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-derivation-for-output",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationStoredResponse storePath) ->
                        -- Construct a basic DerivationInfo from response
                        case Map.lookup "hash" (daemonResponseMeta response) of
                            Just hash ->
                                return $ Just $ DerivationInfo 0 hash storePath 0
                            Nothing -> return Nothing
                    Right (ErrorResponse _) -> return Nothing
                    Right _ -> return Nothing  -- Unexpected response

            _ -> throwError $ privilegeError "Cannot get derivation for output without daemon connection"

    findDerivationsByOutputs [] = return Map.empty
    findDerivationsByOutputs outputPaths = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Request derivations for outputs through protocol
                let pathsParam = T.intercalate "," (map storePathToText outputPaths)
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "find-derivations-by-outputs",
                    reqParams = Map.singleton "paths" pathsParam,
                    reqPayload = Nothing
                }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationQueryResponse drvs) ->
                        -- Construct a map from the derivations
                        return $ Map.fromList $ map (\d -> (T.unpack (derivHash d), d)) drvs
                    Right (ErrorResponse _) -> return Map.empty
                    Right _ -> return Map.empty  -- Unexpected response

            _ -> throwError $ privilegeError "Cannot find derivations by outputs without daemon connection"

-- | Implementation for Builder in Build phase via protocol
instance CanRetrieveDerivation 'Build 'Builder where
    retrieveDerivation hash = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Send request through daemon protocol
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "retrieve-derivation",
                    reqParams = Map.singleton "hash" hash,
                    reqPayload = Nothing
                }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationRetrievedResponse mDrv) -> return mDrv
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return Nothing  -- Unexpected response

            _ -> throwError $ privilegeError "Cannot retrieve derivation without daemon connection"

    retrieveDerivationByHash hash = retrieveDerivation hash

    getDerivationForOutput path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Send request through daemon protocol
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-derivation-for-output",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationStoredResponse storePath) ->
                        -- Construct a basic DerivationInfo from response
                        case Map.lookup "hash" (daemonResponseMeta response) of
                            Just hash ->
                                return $ Just $ DerivationInfo 0 hash storePath 0
                            Nothing -> return Nothing
                    Right (ErrorResponse _) -> return Nothing
                    Right _ -> return Nothing  -- Unexpected response

            _ -> throwError $ privilegeError "Cannot get derivation for output without daemon connection"

    findDerivationsByOutputs [] = return Map.empty
    findDerivationsByOutputs outputPaths = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Request derivations for outputs through protocol
                let pathsParam = T.intercalate "," (map storePathToText outputPaths)
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "find-derivations-by-outputs",
                    reqParams = Map.singleton "paths" pathsParam,
                    reqPayload = Nothing
                }

                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationQueryResponse drvs) ->
                        -- Construct a map from the derivations
                        return $ Map.fromList $ map (\d -> (T.unpack (derivHash d), d)) drvs
                    Right (ErrorResponse _) -> return Map.empty
                    Right _ -> return Map.empty  -- Unexpected response

            _ -> throwError $ privilegeError "Cannot find derivations by outputs without daemon connection"

-- | Type class for output management operations
class CanManageOutputs (t :: PrivilegeTier) where
    -- | Register an output for a derivation
    registerDerivationOutput :: Int64 -> Text -> StorePath -> TenM p t ()

    -- | Get all outputs for a derivation
    getOutputsForDerivation :: Int64 -> TenM p t [OutputInfo]

-- | Daemon implementation of CanManageOutputs with direct database access
instance CanManageOutputs 'Daemon where
    registerDerivationOutput derivId outputName outPath = do
        db <- getDatabaseConn

        -- Insert output record
        void $ tenExecute db
            "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) VALUES (?, ?, ?)"
            (derivId, outputName, storePathToText outPath)

    getOutputsForDerivation derivId = do
        db <- getDatabaseConn

        -- Query outputs
        outputs <- tenQuery db
            "SELECT derivation_id, output_name, path FROM Outputs WHERE derivation_id = ?"
            (Only derivId)

        -- Validate all paths after extraction
        validateAndLogOutputInfos outputs

-- | Builder implementation of CanManageOutputs via protocol
instance CanManageOutputs 'Builder where
    registerDerivationOutput derivId outputName outPath = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "register-derivation-output",
                    reqParams = Map.fromList [
                        ("derivId", T.pack $ show derivId),
                        ("outputName", outputName),
                        ("path", storePathToText outPath)
                    ],
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (SuccessResponse) -> return ()
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> throwError $ DBError "Unexpected response from daemon"

            _ -> throwError $ privilegeError "Cannot register derivation output without daemon connection"

    getOutputsForDerivation derivId = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-outputs-for-derivation",
                    reqParams = Map.singleton "derivId" (T.pack $ show derivId),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationOutputResponse paths) ->
                        -- Convert output paths to OutputInfo objects and validate
                        let outputs = map (\p -> OutputInfo derivId "" p) (Set.toList paths)
                        in validateAndLogOutputInfos outputs
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return []  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot get outputs without daemon connection"

-- | Type class for reference management operations
class CanManageReferences (t :: PrivilegeTier) where
    -- | Add a reference between two paths
    addDerivationReference :: StorePath -> StorePath -> TenM p t ()

    -- | Get all direct references from a path
    getDerivationReferences :: StorePath -> TenM p t [StorePath]

    -- | Get all direct referrers to a path
    getReferrers :: StorePath -> TenM p t [StorePath]

    -- | Get all transitive references (closure)
    getTransitiveReferences :: StorePath -> TenM p t (Set StorePath)

    -- | Get all transitive referrers (reverse closure)
    getTransitiveReferrers :: StorePath -> TenM p t (Set StorePath)

    -- | Bulk register multiple references for efficiency
    bulkRegisterReferences :: [(StorePath, StorePath)] -> TenM p t Int

-- | Daemon implementation of CanManageReferences with direct database access
instance CanManageReferences 'Daemon where
    addDerivationReference referrer reference =
        when (referrer /= reference) $ do
            db <- getDatabaseConn

            -- Skip self-references
            void $ tenExecute db
                "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
                (storePathToText referrer, storePathToText reference)

    getDerivationReferences path = do
        db <- getDatabaseConn

        results <- tenQuery db
            "SELECT reference FROM References WHERE referrer = ?"
            (Only (storePathToText path)) :: TenM p 'Daemon [Only Text]

        -- Parse each path
        let parsedPaths = catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Validate parsed paths
        validateAndLogPaths parsedPaths

    getReferrers path = do
        db <- getDatabaseConn

        results <- tenQuery db
            "SELECT referrer FROM References WHERE reference = ?"
            (Only (storePathToText path)) :: TenM p 'Daemon [Only Text]

        -- Parse each path
        let parsedPaths = catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Validate parsed paths
        validateAndLogPaths parsedPaths

    getTransitiveReferences path = do
        db <- getDatabaseConn

        -- Use recursive CTE to efficiently query the closure
        let query = "WITH RECURSIVE\n\
                    \  closure(p) AS (\n\
                    \    VALUES(?)\n\
                    \    UNION\n\
                    \    SELECT reference FROM References JOIN closure ON referrer = p\n\
                    \  )\n\
                    \SELECT p FROM closure WHERE p != ?"

        results <- tenQuery db query (storePathToText path, storePathToText path) :: TenM p 'Daemon [Only Text]

        -- Parse paths
        let parsedPaths = catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Validate parsed paths and convert to set
        validatedPaths <- validateAndLogPaths parsedPaths
        return $ Set.fromList validatedPaths

    getTransitiveReferrers path = do
        db <- getDatabaseConn

        -- Use recursive CTE to efficiently query the closure
        let query = "WITH RECURSIVE\n\
                    \  closure(p) AS (\n\
                    \    VALUES(?)\n\
                    \    UNION\n\
                    \    SELECT referrer FROM References JOIN closure ON reference = p\n\
                    \  )\n\
                    \SELECT p FROM closure WHERE p != ?"

        results <- tenQuery db query (storePathToText path, storePathToText path) :: TenM p 'Daemon [Only Text]

        -- Parse paths
        let parsedPaths = catMaybes $ map (\(Only text) -> parseStorePath text) results

        -- Validate parsed paths and convert to set
        validatedPaths <- validateAndLogPaths parsedPaths
        return $ Set.fromList validatedPaths

    bulkRegisterReferences [] = return 0
    bulkRegisterReferences references = do
        db <- getDatabaseConn

        withTenWriteTransaction db $ \_ -> do
            -- Filter out self-references
            let validRefs = filter (\(from, to) -> from /= to) references

            -- Insert each reference
            count <- foldM (\acc (from, to) -> do
                void $ tenExecute db
                    "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
                    (storePathToText from, storePathToText to)
                return $! acc + 1) 0 validRefs

            return count

-- | Builder implementation of CanManageReferences via protocol
instance CanManageReferences 'Builder where
    addDerivationReference referrer reference =
        when (referrer /= reference) $ do
            env <- ask
            case runMode env of
                ClientMode conn -> do
                    -- Create request
                    let request = Request {
                        reqId = 0,  -- Will be set by sendRequest
                        reqType = "add-derivation-reference",
                        reqParams = Map.fromList [
                            ("referrer", storePathToText referrer),
                            ("reference", storePathToText reference)
                        ],
                        reqPayload = Nothing
                    }

                    -- Send request
                    response <- liftIO $ sendRequestSync conn request 30000000
                    case response of
                        Left err -> throwError err
                        Right (SuccessResponse) -> return ()
                        Right (ErrorResponse err) -> throwError err
                        Right _ -> throwError $ DBError "Unexpected response from daemon"

                _ -> throwError $ privilegeError "Cannot add reference without daemon connection"

    getDerivationReferences path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-derivation-references",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (StoreListResponse paths) -> validateAndLogPaths paths
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return []  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot get references without daemon connection"

    getReferrers path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-referrers",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (StoreListResponse paths) -> validateAndLogPaths paths
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return []  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot get referrers without daemon connection"

    getTransitiveReferences path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-transitive-references",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (StoreListResponse paths) -> do
                        validatedPaths <- validateAndLogPaths paths
                        return $ Set.fromList validatedPaths
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return Set.empty  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot get transitive references without daemon connection"

    getTransitiveReferrers path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-transitive-referrers",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (StoreListResponse paths) -> do
                        validatedPaths <- validateAndLogPaths paths
                        return $ Set.fromList validatedPaths
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return Set.empty  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot get transitive referrers without daemon connection"

    bulkRegisterReferences refs = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Filter out self-references
                let validRefs = filter (\(from, to) -> from /= to) refs

                -- Build references list param
                let refsText = T.intercalate ";" $ map
                      (\(from, to) -> storePathToText from <> "," <> storePathToText to) validRefs

                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "bulk-register-references",
                    reqParams = Map.singleton "references" refsText,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (SuccessResponse) ->
                        -- The count should be in the metadata
                        case Map.lookup "count" (daemonResponseMeta response) of
                            Just countText -> case reads (T.unpack countText) of
                                [(count, "")] -> return count
                                _ -> return (length validRefs)  -- Return the number we sent
                            Nothing -> return (length validRefs)  -- Return the number we sent
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return 0  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot bulk register references without daemon connection"

-- | Type class for derivation query operations
class CanQueryDerivations (t :: PrivilegeTier) where
    -- | Check if a derivation is already registered
    isDerivationRegistered :: Text -> TenM p t Bool

    -- | List all registered derivations
    listRegisteredDerivations :: TenM p t [DerivationInfo]

    -- | Get the store path for a derivation by hash
    getDerivationPath :: Text -> TenM p t (Maybe StorePath)

-- | Daemon implementation of CanQueryDerivations with direct database access
instance CanQueryDerivations 'Daemon where
    isDerivationRegistered hash = do
        db <- getDatabaseConn

        results <- tenQuery db
            "SELECT 1 FROM Derivations WHERE hash = ? LIMIT 1"
            (Only hash) :: TenM p 'Daemon [Only Int]

        return $ not (null results)

    listRegisteredDerivations = do
        db <- getDatabaseConn

        derivInfos <- liftIO $ SQLite.query_ (dbConn db) "SELECT id, hash, store_path, timestamp FROM Derivations ORDER BY timestamp DESC"

        -- Validate paths
        validateAndLogDerivationInfos derivInfos

    getDerivationPath hash = do
        db <- getDatabaseConn

        results <- tenQuery db
            "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
            (Only hash) :: TenM p 'Daemon [Only Text]

        case results of
            [Only path] ->
                case parseStorePath path of
                    Just sp -> do
                        -- Validate after parsing
                        validatedPath <- validateAndLogPath sp
                        return $ Just validatedPath
                    Nothing -> do
                        -- Log invalid path
                        logMsg 1 $ "Invalid store path in database: " <> path
                        return Nothing
            _ -> return Nothing

-- | Builder implementation of CanQueryDerivations via protocol
instance CanQueryDerivations 'Builder where
    isDerivationRegistered hash = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "is-derivation-registered",
                    reqParams = Map.singleton "hash" hash,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (SuccessResponse) -> return True
                    Right (ErrorResponse _) -> return False
                    Right _ -> return False  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot check derivation registration without daemon connection"

    listRegisteredDerivations = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "list-registered-derivations",
                    reqParams = Map.empty,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (DerivationListResponse paths) -> do
                        -- Convert to DerivationInfo objects with minimal data
                        let derivInfos = map (\p -> DerivationInfo 0 "" p 0) paths
                        validateAndLogDerivationInfos derivInfos
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> return []  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot list derivations without daemon connection"

    getDerivationPath hash = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-derivation-path",
                    reqParams = Map.singleton "hash" hash,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (StorePathResponse path) -> do
                        validatedPath <- validateAndLogPath path
                        return $ Just validatedPath
                    Right (ErrorResponse _) -> return Nothing
                    Right _ -> return Nothing  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot get derivation path without daemon connection"

-- | Type class for path validity operations
class CanManagePathValidity (t :: PrivilegeTier) where
    -- | Register a valid path in the store
    registerValidPath :: StorePath -> Maybe StorePath -> TenM p t ()

    -- | Mark a path as invalid
    invalidatePath :: StorePath -> TenM p t ()

    -- | Check if a path is valid
    isPathValid :: StorePath -> TenM p t Bool

    -- | Get detailed information about a path
    getPathInfo :: StorePath -> TenM p t (Maybe PathInfo)

-- | Daemon implementation of CanManagePathValidity with direct database access
instance CanManagePathValidity 'Daemon where
    registerValidPath path mDeriver = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- First check if path exists
        let fullPath = storePathToFilePath path env
        exists <- liftIO $ doesFileExist fullPath
        unless exists $
            throwError $ StoreError $ "Cannot register non-existent path: " <> storePathToText path

        -- Open database connection
        conn <- liftIO $ SQLite.open dbPath

        -- Insert path into ValidPaths
        liftIO $ SQLite.execute conn
            "INSERT OR IGNORE INTO ValidPaths (path, hash, name, deriver, is_valid, timestamp) VALUES (?, ?, ?, ?, 1, strftime('%s','now'))"
            (storePathToText path, storeHash path, storeName path, fmap storePathToText mDeriver)

        -- If there's a deriver, register the reference
        forM_ mDeriver $ \deriver ->
            liftIO $ SQLite.execute conn
                "INSERT OR IGNORE INTO References (referrer, reference, type) VALUES (?, ?, 'derivation')"
                (storePathToText path, storePathToText deriver)

        -- Close connection
        liftIO $ SQLite.close conn

    invalidatePath path = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- Open database connection
        conn <- liftIO $ SQLite.open dbPath

        -- Mark path as invalid
        liftIO $ SQLite.execute conn
            "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
            (Only $ storePathToText path)

        -- Close connection
        liftIO $ SQLite.close conn

    isPathValid path = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- Open database connection
        conn <- liftIO $ SQLite.open dbPath

        -- Query for validity
        rows <- liftIO $ SQLite.query conn
            "SELECT is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
            (Only (storePathToText path)) :: TenM p 'Daemon [Only Int]

        -- Close connection
        liftIO $ SQLite.close conn

        -- Check if valid
        case rows of
            [Only valid] -> return (valid == 1)
            _ -> return False

    getPathInfo path = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- Open database connection
        conn <- liftIO $ SQLite.open dbPath

        -- Query for path info
        pathInfos <- liftIO $ SQLite.query conn
            "SELECT path, hash, deriver, registration_time, is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
            (Only (storePathToText path)) :: TenM p 'Daemon [PathInfo]

        -- Close connection
        liftIO $ SQLite.close conn

        -- Validate the returned paths
        validatedPathInfos <- forM pathInfos $ \info -> do
            _ <- validateAndLogPath (pathInfoPath info)
            case pathInfoDeriver info of
                Just deriver -> do
                    _ <- validateAndLogPath deriver
                    return info
                Nothing -> return info

        return $ listToMaybe validatedPathInfos

-- | Builder implementation of CanManagePathValidity via protocol
instance CanManagePathValidity 'Builder where
    registerValidPath path mDeriver = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let deriverParam = case mDeriver of
                        Just deriver -> Map.singleton "deriver" (storePathToText deriver)
                        Nothing -> Map.empty

                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "register-valid-path",
                    reqParams = Map.insert "path" (storePathToText path) deriverParam,
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (SuccessResponse) -> return ()
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> throwError $ DBError "Unexpected response from daemon"

            _ -> throwError $ privilegeError "Cannot register valid path without daemon connection"

    invalidatePath path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "invalidate-path",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (SuccessResponse) -> return ()
                    Right (ErrorResponse err) -> throwError err
                    Right _ -> throwError $ DBError "Unexpected response from daemon"

            _ -> throwError $ privilegeError "Cannot invalidate path without daemon connection"

    isPathValid path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "is-path-valid",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (SuccessResponse) -> return True
                    Right (ErrorResponse _) -> return False
                    Right _ -> return False  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot check path validity without daemon connection"

    getPathInfo path = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create request
                let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = "get-path-info",
                    reqParams = Map.singleton "path" (storePathToText path),
                    reqPayload = Nothing
                }

                -- Send request
                response <- liftIO $ sendRequestSync conn request 30000000
                case response of
                    Left err -> throwError err
                    Right (SuccessResponse) ->
                        case (Map.lookup "hash" (daemonResponseMeta response),
                              Map.lookup "valid" (daemonResponseMeta response)) of
                            (Just hash, Just valid) -> do
                                let deriver = Map.lookup "deriver" (daemonResponseMeta response) >>= parseStorePath
                                    isValid = valid == "true"
                                    time = posixSecondsToUTCTime 0 -- Default time
                                pathInfo <- validateAndLogPath path
                                case deriver of
                                    Just d -> do
                                        _ <- validateAndLogPath d
                                        return $ Just $ PathInfo pathInfo hash deriver time isValid
                                    Nothing ->
                                        return $ Just $ PathInfo pathInfo hash deriver time isValid
                            _ -> return Nothing
                    Right (ErrorResponse _) -> return Nothing
                    Right _ -> return Nothing  -- Default fallback for unexpected responses

            _ -> throwError $ privilegeError "Cannot get path info without daemon connection"


-- | Helper function to store a derivation in the database (context-aware)
storeDerivationInDB :: (CanStoreDerivation t) => Derivation -> StorePath -> TenM p t Int64
storeDerivationInDB = registerDerivationInDB

-- | Helper function to read a derivation from the store - available in both contexts
readDerivationFromStore :: (CanRetrieveDerivation p t) => StorePath -> TenM p t (Maybe Derivation)
readDerivationFromStore path = do
    env <- ask

    -- Try to read directly from the file system first
    let filePath = storePathToFilePath path env
    fileExists <- liftIO $ doesFileExist filePath

    if fileExists
        then do
            -- Try to read and parse the file directly
            content <- liftIO $ try $ BS.readFile filePath
            case content of
                Right bs ->
                    case Deriv.deserializeDerivation bs of
                        Left _ -> return Nothing
                        Right drv -> return $ Just drv
                Left (_ :: SomeException) -> do
                    -- If direct file read fails, try protocol or DB based on context
                    fetchDerivationFromDB path
        else
            -- File doesn't exist locally, try protocol or DB based on context
            fetchDerivationFromDB path
  where
    -- Implementation depends on tier
    fetchDerivationFromDB :: (CanRetrieveDerivation p t) => StorePath -> TenM p t (Maybe Derivation)
    fetchDerivationFromDB sp = do
        -- Try to find the derivation hash from the store path
        let name = storeName sp
        if T.isSuffixOf ".drv" name
            then do
                -- This is a derivation path, extract the hash part
                let hash = storeHash sp
                -- Look up the derivation by hash
                retrieveDerivationByHash hash
            else
                -- Not a derivation path, try to find the derivation that produced this output
                do
                    mDerivInfo <- getDerivationForOutput sp
                    case mDerivInfo of
                        Just info -> retrieveDerivation (derivationHash info)
                        Nothing -> return Nothing

-- | Read and deserialize a derivation file
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
            return $ case Deriv.deserializeDerivation content of
                     Left err -> Left $ "Failed to deserialize derivation: " <> T.pack (show err)
                     Right drv -> Right drv

-- | Helper function to get a database connection
getDatabaseConn :: TenM p 'Daemon (Database 'Daemon)
getDatabaseConn = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)

    -- In a real implementation, this would use a connection pool
    -- For simplicity, we're creating a new connection each time
    conn <- liftIO $ SQLite.open dbPath
    liftIO $ SQLite.execute_ conn "PRAGMA foreign_keys = ON"

    -- Return the database connection wrapped in our Database type
    return $ Database conn dbPath True 5000 3 sDaemon

-- | Execute a query against the database
tenExecute :: (SQLite.ToRow q) => Database 'Daemon -> Query -> q -> TenM p 'Daemon ()
tenExecute db query params = do
    -- For a real implementation, this would handle retries and error cases
    liftIO $ SQLite.execute (dbConn db) query params

-- | Execute a query against the database and return results
tenQuery :: (SQLite.ToRow q, SQLite.FromRow r) => Database 'Daemon -> Query -> q -> TenM p 'Daemon [r]
tenQuery db query params = do
    -- For a real implementation, this would handle retries and error cases
    liftIO $ SQLite.query (dbConn db) query params

-- | Execute a simple query without parameters and return results
tenQuery_ :: (SQLite.FromRow r) => Database 'Daemon -> Query -> TenM p 'Daemon [r]
tenQuery_ db query = do
    -- For a real implementation, this would handle retries and error cases
    liftIO $ SQLite.query_ (dbConn db) query
