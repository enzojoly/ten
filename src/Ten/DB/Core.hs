{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Ten.DB.Core (
    -- Database types
    Database(..),
    DBError(..),
    TransactionMode(..),
    Only(..),

    -- Type classes for database operations
    HasStoreOps(..),
    HasDerivationOps(..),
    HasBuildOps(..),
    HasGCOps(..),
    HasSchemaOps(..),

    -- Daemon-only type classes for direct database access
    HasDirectQueryOps(..),
    HasTransactionOps(..),

    -- Evidence passing
    withDbPrivileges,

    -- Database initialization and access
    initDatabaseDaemon,
    closeDatabaseDaemon,
    getDatabaseFromEnv,

    -- Daemon-only transaction handling
    withTenTransaction,
    withTenReadTransaction,
    withTenWriteTransaction,

    -- Utility functions
    retryOnBusy,
    ensureDBDirectories,

    -- Re-exported types for convenience
    Query(..)
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch, throwIO, finally, Exception, SomeException)
import Control.Monad (when, void, unless, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, local, ReaderT, runReaderT, asks)
import Control.Monad.Except (MonadError, throwError, catchError, ExceptT, runExceptT)
import Control.Monad.State (StateT, runStateT, get, put, gets)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (Connection, Query(..), ToRow(..), FromRow(..), Only(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.IO (withFile, IOMode(..), hPutStrLn, stderr)
import qualified Data.Aeson as Aeson
import qualified Data.Binary as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.Binary.Get as Binary

-- Import from Ten.Core, including all necessary items for database operations
import Ten.Core (
    TenM, BuildEnv(..), BuildError(..), privilegeError, StorePath, Phase(..),
    PrivilegeTier(..), SPrivilegeTier(..), sDaemon, sBuilder, SPhase(..), sBuild, sEval,
    fromSing, defaultDBPath, BuildState(..),
    Database(..), TransactionMode(..),
    CanAccessDatabase, outputPath, outputName,
    currentBuildId, initBuildState, runTen, verbosity, logMsg,
    runMode, RunMode(..), DaemonConnection, sendRequestSync, Request(..),
    DaemonResponse(..), buildErrorToText, serializeDerivation, deserializeDerivation,
    daemonStatus, brOutputPaths, dbInitialized, -- Add record field
    liftTenIO, Derivation(..), BuildResult(..), hashByteString, Response(..), storePathToText,
    parseStorePath, BuildId(..), GCRoot(..), rootName, storePathToFilePath
    )

-- | Database error types with explicit categorization
data DBError
    = DBConnectionError Text     -- Errors with connection creation/management
    | DBQueryError Text          -- Errors executing queries
    | DBSchemaError Text         -- Errors with database schema
    | DBTransactionError Text    -- Errors with transaction handling
    | DBLockError Text           -- Database locking errors
    | DBResourceError Text       -- Resource allocation/deallocation errors
    | DBMigrationError Text      -- Schema migration errors
    | DBInvalidStateError Text   -- Invalid database state
    | DBPrivilegeError Text      -- Privilege violations
    | DBProtocolError Text       -- Protocol communication errors
    | DBDeserializationError Text -- Data deserialization errors
    | DBSerializationError Text  -- Data serialization errors
    deriving (Show, Eq)

instance Exception DBError

-- | Convert DBError to BuildError for consistent error handling
toBuilderError :: DBError -> BuildError
toBuilderError (DBConnectionError msg) = DBError msg
toBuilderError (DBQueryError msg) = DBError msg
toBuilderError (DBSchemaError msg) = DBError msg
toBuilderError (DBTransactionError msg) = DBError msg
toBuilderError (DBLockError msg) = DBError msg
toBuilderError (DBResourceError msg) = DBError msg
toBuilderError (DBMigrationError msg) = DBError msg
toBuilderError (DBInvalidStateError msg) = DBError msg
toBuilderError (DBPrivilegeError msg) = PrivilegeError msg
toBuilderError (DBProtocolError msg) = ProtocolError msg
toBuilderError (DBDeserializationError msg) = SerializationError msg
toBuilderError (DBSerializationError msg) = SerializationError msg

-- | Helper to unwrap the evidence from a Database
getDbPrivEvidence :: Database t -> SPrivilegeTier t
getDbPrivEvidence = dbPrivEvidence

-- | Evidence passing function for Database operations
withDbPrivileges :: Database t -> (SPrivilegeTier t -> TenM p t a) -> TenM p t a
withDbPrivileges db f = f (getDbPrivEvidence db)

-- Protocol message types for high-level operations
data DbRequestType
    = StoreDerivationRequest Derivation
    | RetrieveDerivationRequest StorePath
    | RegisterBuildResultRequest BuildId BuildResult
    | GetBuildResultRequest StorePath
    | GetSchemaVersionRequest
    | SetSchemaVersionRequest Int
    | CreateGCRootRequest StorePath Text
    | RemoveGCRootRequest StorePath Text
    | ListGCRootsRequest Bool
    deriving (Show, Eq)

data DbResponseType
    = StoreDerivationResponse StorePath
    | RetrieveDerivationResponse (Maybe Derivation)
    | RegisterBuildResultResponse StorePath
    | GetBuildResultResponse (Maybe BuildResult)
    | GetSchemaVersionResponse Int
    | SetSchemaVersionResponse
    | CreateGCRootResponse
    | RemoveGCRootResponse
    | ListGCRootsResponse [Text]
    | DbErrorResponse DBError   -- Renamed from ErrorResponse to avoid conflict
    deriving (Show, Eq)

-- | Base typeclass for store operations (common to both Daemon and Builder)
class HasStoreOps (t :: PrivilegeTier) where
    -- | Register a valid path in the store
    registerPath :: Database t -> StorePath -> TenM p t ()

    -- | Check if a path exists in the store
    pathExists :: Database t -> StorePath -> TenM p t Bool

    -- | Get all paths in the store
    getAllPaths :: Database t -> TenM p t [StorePath]

-- | Base typeclass for derivation operations (common to both Daemon and Builder)
class HasDerivationOps (t :: PrivilegeTier) where
    -- | Store a derivation
    storeDerivation :: Database t -> Derivation -> TenM p t StorePath

    -- | Retrieve a derivation by store path
    retrieveDerivation :: Database t -> StorePath -> TenM p t (Maybe Derivation)

-- | Base typeclass for build operations (common to both Daemon and Builder)
class HasBuildOps (t :: PrivilegeTier) where
    -- | Register a build result
    registerBuildResult :: Database t -> BuildId -> BuildResult -> TenM p t StorePath

    -- | Get a build result by store path
    getBuildResult :: Database t -> StorePath -> TenM p t (Maybe BuildResult)

-- | Base typeclass for garbage collection operations (common to both Daemon and Builder)
class HasGCOps (t :: PrivilegeTier) where
    -- | Create a GC root
    createGCRoot :: Database t -> StorePath -> Text -> TenM p t ()

    -- | Remove a GC root
    removeGCRoot :: Database t -> StorePath -> Text -> TenM p t ()

    -- | List GC roots
    listGCRoots :: Database t -> Bool -> TenM p t [Text]

-- | Base typeclass for schema operations (common to both Daemon and Builder)
class HasSchemaOps (t :: PrivilegeTier) where
    -- | Get the current schema version
    getSchemaVersion :: Database t -> TenM p t Int

    -- | Update the schema version
    updateSchemaVersion :: Database t -> Int -> TenM p t ()

-- | Daemon-only typeclass for direct SQL query operations
class HasDirectQueryOps (t :: PrivilegeTier) where
    -- | Execute a query with parameters
    query :: (ToRow q, FromRow r) => Database t -> Query -> q -> TenM p t [r]

    -- | Execute a simple query without parameters
    query_ :: (FromRow r) => Database t -> Query -> TenM p t [r]

    -- | Execute a statement with parameters
    execute :: (ToRow q) => Database t -> Query -> q -> TenM p t Int64

    -- | Execute a statement with parameters without returning result
    execute_ :: (ToRow q) => Database t -> Query -> q -> TenM p t ()

    -- | Execute a simple statement without parameters
    executeSimple_ :: Database t -> Query -> TenM p t ()

-- | Daemon-only typeclass for transaction management
class HasTransactionOps (t :: PrivilegeTier) where

    -- | Execute an action within a transaction
    withTenTransaction :: Database t -> TransactionMode -> (Database t -> TenM p t a) -> TenM p t a

    -- | Execute an action within a read-only transaction
    withTenReadTransaction :: Database t -> (Database t -> TenM p t a) -> TenM p t a

    -- | Execute an action within a read-write transaction
    withTenWriteTransaction :: Database t -> (Database t -> TenM p t a) -> TenM p t a

-- | Bracket-like function that works in the TenM monad
bracketTenM :: TenM p t a -> (a -> TenM p t b) -> (a -> TenM p t c) -> TenM p t c
bracketTenM acquire release action = do
    resource <- acquire
    result <- catchError (action resource) $ \e -> do
        -- On error, still release the resource, then rethrow
        _ <- release resource
        throwError e
    _ <- release resource
    return result

-- Daemon instances for direct database operations

-- | HasStoreOps instance for Daemon (direct database access)
instance HasStoreOps 'Daemon where
    registerPath db path = withTenWriteTransaction db $ \db' -> do
        -- Insert into ValidPaths table
        execute_ db' "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time) VALUES (?, ?, strftime('%s','now'))"
            (storePathToText path, T.unpack (storePathToText path))

    pathExists db path = withTenReadTransaction db $ \db' -> do
        results <- query db' "SELECT 1 FROM ValidPaths WHERE path = ? AND is_valid = 1" [storePathToText path]
        return $ not (null (results :: [Only Int]))

    getAllPaths db = withTenReadTransaction db $ \db' -> do
        results <- query_ db' "SELECT path FROM ValidPaths WHERE is_valid = 1"
        return $ map (\(Only p) -> case parseStorePath p of
                                     Just path -> path
                                     Nothing -> error "Invalid path in database") (results :: [Only Text])

-- | HasDerivationOps instance for Daemon (direct database access)
instance HasDerivationOps 'Daemon where
    storeDerivation db drv = withTenWriteTransaction db $ \db' -> do
        -- Serialize the derivation
        let serialized = serializeDerivation drv
        let contentHash = hashByteString serialized
        let hashText = T.pack $ show contentHash
        let name = derivName drv <> ".drv"
        let storePath = case parseStorePath (hashText <> "-" <> name) of
                          Just path -> path
                          Nothing -> error "Failed to create valid store path"

        -- Store in Derivations table
        execute_ db' "INSERT OR REPLACE INTO Derivations (hash, store_path) VALUES (?, ?)"
            (derivHash drv, storePathToText storePath)

        -- Get the ID of the inserted derivation
        [Only derivId] <- query db' "SELECT id FROM Derivations WHERE hash = ?" [derivHash drv]

        -- Store outputs
        forM_ (Set.toList $ derivOutputs drv) $ \output -> do
            execute_ db' "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) VALUES (?, ?, ?)"
                (derivId :: Int64, outputName output, storePathToText (outputPath output))

        -- Register the path
        registerPath db' storePath

        return storePath

    retrieveDerivation db path = withTenReadTransaction db $ \db' -> do
        -- Check if derivation exists
        pathExists <- query db' "SELECT 1 FROM Derivations WHERE store_path = ?" [storePathToText path]
        if null (pathExists :: [Only Int])
            then return Nothing
            else do
                -- Get the environment to determine store location
                env <- ask

                -- Use the proper store path determination function
                let derivPath = storePathToFilePath path env

                fileExists <- liftIO $ doesFileExist derivPath
                if not fileExists
                    then return Nothing
                    else do
                        content <- liftIO $ BS.readFile derivPath
                        case deserializeDerivation content of
                            Left _ -> return Nothing
                            Right drv -> return (Just drv)

-- | HasBuildOps instance for Daemon (direct database access)
instance HasBuildOps 'Daemon where
    registerBuildResult db buildId result = withTenWriteTransaction db $ \db' -> do
        -- Serialize the build result
        let json = Aeson.encode result
        let contentHash = hashByteString (LBS.toStrict json)
        let hashText = T.pack $ show contentHash
        let name = T.pack (show buildId) <> "-result.json"
        let storePath = case parseStorePath (hashText <> "-" <> name) of
                          Just path -> path
                          Nothing -> error "Failed to create valid store path"

        -- Register the path
        registerPath db' storePath

        -- Register references
        forM_ (Set.toList $ brOutputPaths result) $ \outputPath -> do
            forM_ (Set.toList $ brReferences result) $ \refPath -> do
                execute_ db' "INSERT OR REPLACE INTO References (referrer, reference) VALUES (?, ?)"
                    (storePathToText outputPath, storePathToText refPath)

        return storePath

    getBuildResult db path = withTenReadTransaction db $ \db' -> do
        -- Check if path exists
        exists <- pathExists db' path
        if not exists
            then return Nothing
            else do
                -- Get the environment to determine store location
                env <- ask

                -- Use the proper store path determination function
                let resultPath = storePathToFilePath path env

                fileExists <- liftIO $ doesFileExist resultPath
                if not fileExists
                    then return Nothing
                    else do
                        content <- liftIO $ BS.readFile resultPath
                        case Aeson.eitherDecodeStrict content of
                            Left _ -> return Nothing
                            Right result -> return (Just result)

-- | HasGCOps instance for Daemon (direct database access)
instance HasGCOps 'Daemon where
    createGCRoot db path name = withTenWriteTransaction db $ \db' -> do
        execute_ db' "INSERT OR REPLACE INTO GCRoots (path, name, type, timestamp, active) VALUES (?, ?, 'registry', strftime('%s','now'), 1)"
            (storePathToText path, name)

    removeGCRoot db path name = withTenWriteTransaction db $ \db' -> do
        execute_ db' "UPDATE GCRoots SET active = 0 WHERE path = ? AND name = ?"
            (storePathToText path, name)

    listGCRoots db onlyActive = withTenReadTransaction db $ \db' -> do
        let query = if onlyActive
                      then "SELECT name FROM GCRoots WHERE active = 1"
                      else "SELECT name FROM GCRoots"
        results <- query_ db' (Query query)
        return $ map (\(Only name) -> name) (results :: [Only Text])

-- | HasSchemaOps instance for Daemon (direct database access)
instance HasSchemaOps 'Daemon where
    getSchemaVersion db = withTenReadTransaction db $ \db' -> do
        -- Check if the version table exists
        hasVersionTable <- query_ db' "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM p 'Daemon [Only Int]
        case hasVersionTable of
            [Only (count :: Int)] | count > 0 -> do
                -- Table exists, get the version
                results <- query_ db' "SELECT version FROM SchemaVersion LIMIT 1;" :: TenM p 'Daemon [Only Int]
                case results of
                    [Only version] -> return version
                    _ -> return 0  -- No version record found
            _ -> return 0  -- Table doesn't exist

    updateSchemaVersion db version = withTenWriteTransaction db $ \db' -> do
        -- Check if the version table exists
        hasVersionTable <- query_ db' "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM p 'Daemon [Only Int]
        case hasVersionTable of
            [Only (count :: Int)] | count > 0 -> do
                -- Table exists, update the version
                execute_ db' "UPDATE SchemaVersion SET version = ?, updated_at = CURRENT_TIMESTAMP" [version]
            _ -> do
                -- Table doesn't exist, create it
                executeSimple_ db' "CREATE TABLE IF NOT EXISTS SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"
                execute_ db' "INSERT INTO SchemaVersion (version, updated_at) VALUES (?, CURRENT_TIMESTAMP)" [version]

-- | HasDirectQueryOps instance for Daemon (direct database access)
instance HasDirectQueryOps 'Daemon where
    query db q params = do
        -- Execute query with retry
        liftIO $ retryOnBusy db $ SQLite.query (dbConn db) q params

    query_ db q = do
        -- Execute simple query with retry
        liftIO $ retryOnBusy db $ SQLite.query_ (dbConn db) q

    execute db query params = do
        -- Execute the query with retry on busy
        liftIO $ retryOnBusy db $ do
            -- Execute the query
            SQLite.execute (dbConn db) query params
            -- Get the number of changes (rows affected) and convert from Int to Int64
            fromIntegral <$> SQLite.changes (dbConn db)

    execute_ db query params = void $ execute db query params

    executeSimple_ db query = do
        -- Execute simple query
        liftIO $ retryOnBusy db $ SQLite.execute_ (dbConn db) query

-- | HasTransactionOps instance for Daemon (direct transaction management)
instance HasTransactionOps 'Daemon where
    withTenTransaction db mode action =
        -- Start transaction with appropriate mode
        let beginStmt = case mode of
                ReadOnly -> "BEGIN TRANSACTION READONLY;"
                ReadWrite -> "BEGIN TRANSACTION;"
                Exclusive -> "BEGIN EXCLUSIVE TRANSACTION;"
        in
        -- Execute transaction with proper error handling
        catchError
            (do
                -- Begin transaction
                liftIO $ retryOnBusy db $ SQLite.execute_ (dbConn db) (Query beginStmt)

                -- Run the action
                result <- action db

                -- Commit transaction
                liftIO $ retryOnBusy db $ SQLite.execute_ (dbConn db) "COMMIT;"

                return result)
            (\e -> do
                -- Try to roll back on error
                liftIO $ rollbackOnError db
                throwError e)

    withTenReadTransaction db = withTenTransaction db ReadOnly
    withTenWriteTransaction db = withTenTransaction db ReadWrite

-- Builder instances for protocol-based operations

-- | Helper function to encode a DbRequestType to protocol format
encodeDbRequest :: DbRequestType -> (Text, Map.Map Text Text, Maybe ByteString)
encodeDbRequest (StoreDerivationRequest drv) =
    ("store-derivation", Map.empty, Just (serializeDerivation drv))

encodeDbRequest (RetrieveDerivationRequest path) =
    ("retrieve-derivation", Map.singleton "path" (storePathToText path), Nothing)

encodeDbRequest (RegisterBuildResultRequest buildId result) =
    ("register-build-result",
     Map.singleton "buildId" (T.pack $ show buildId),
     Just (LBS.toStrict $ Aeson.encode result))

encodeDbRequest (GetBuildResultRequest path) =
    ("get-build-result", Map.singleton "path" (storePathToText path), Nothing)

encodeDbRequest GetSchemaVersionRequest =
    ("get-schema-version", Map.empty, Nothing)

encodeDbRequest (SetSchemaVersionRequest version) =
    ("set-schema-version", Map.singleton "version" (T.pack $ show version), Nothing)

encodeDbRequest (CreateGCRootRequest path name) =
    ("create-gc-root",
     Map.fromList [("path", storePathToText path), ("name", name)],
     Nothing)

encodeDbRequest (RemoveGCRootRequest path name) =
    ("remove-gc-root",
     Map.fromList [("path", storePathToText path), ("name", name)],
     Nothing)

encodeDbRequest (ListGCRootsRequest onlyActive) =
    ("list-gc-roots",
     Map.singleton "onlyActive" (if onlyActive then "true" else "false"),
     Nothing)

-- | Helper function to parse DaemonResponse into DbResponseType
parseDbResponse :: DaemonResponse -> Either BuildError DbResponseType
parseDbResponse (DerivationStoredResponse path) =
    Right (StoreDerivationResponse path)

parseDbResponse (DerivationRetrievedResponse mDrv) =
    Right (RetrieveDerivationResponse mDrv)

parseDbResponse (BuildResultResponse result) =
    Right (RegisterBuildResultResponse (head $ Set.toList $ brOutputPaths result))

parseDbResponse (StoreReadResponse content) =
    case Aeson.eitherDecodeStrict content of
        Left err -> Left $ SerializationError $ "Failed to parse build result: " <> T.pack err
        Right result -> Right (GetBuildResultResponse (Just result))

parseDbResponse (StatusResponse status) =
    Right (GetSchemaVersionResponse (read $ T.unpack $ daemonStatus status))

parseDbResponse SuccessResponse =
    Right SetSchemaVersionResponse

parseDbResponse (GCRootAddedResponse _) =
    Right CreateGCRootResponse

parseDbResponse (GCRootRemovedResponse _) =
    Right RemoveGCRootResponse

parseDbResponse (GCRootListResponse gcRoots) =
    -- Extract the name field from each GCRoot
    Right (ListGCRootsResponse (map rootName gcRoots))

parseDbResponse (Ten.Core.ErrorResponse err) =
    Left err

parseDbResponse resp =
    Left $ ProtocolError $ "Unexpected response: " <> T.pack (show resp)

-- | Helper function to send a database request through the protocol
sendDbRequest :: DbRequestType -> TenM p 'Builder (Either BuildError DbResponseType)
sendDbRequest reqData = do
    env <- ask
    case runMode env of
        ClientMode conn -> do
            let (reqType, params, payload) = encodeDbRequest reqData
            let request = Request {
                    reqId = 0,  -- Will be set by sendRequest
                    reqType = reqType,
                    reqParams = params,
                    reqPayload = payload
                }

            -- Send request and wait for response
            respEither <- liftIO $ sendRequestSync conn request 30000000  -- 30 second timeout
            case respEither of
                Left err -> return $ Left err
                Right resp -> return $ parseDbResponse resp
        _ -> return $ Left $ privilegeError "Cannot send database request without daemon connection"

-- | HasStoreOps instance for Builder (protocol-based)
instance HasStoreOps 'Builder where
    registerPath _ _ =
        throwError $ privilegeError "Builder cannot directly register paths"

    pathExists db path = do
        -- For path existence, we can use the retrieve-derivation endpoint and check for response
        respEither <- sendDbRequest (RetrieveDerivationRequest path)
        case respEither of
            Left _ -> return False  -- If error, assume path doesn't exist
            Right (RetrieveDerivationResponse Nothing) -> return False
            Right _ -> return True  -- Any other successful response means path exists

    getAllPaths _ =
        throwError $ privilegeError "Builder cannot list all paths"

-- | HasDerivationOps instance for Builder (protocol-based)
instance HasDerivationOps 'Builder where
    storeDerivation db drv = do
        respEither <- sendDbRequest (StoreDerivationRequest drv)
        case respEither of
            Left err -> throwError err
            Right (StoreDerivationResponse path) -> return path
            Right _ -> throwError $ toBuilderError $ DBProtocolError "Unexpected response type"

    retrieveDerivation db path = do
        respEither <- sendDbRequest (RetrieveDerivationRequest path)
        case respEither of
            Left _ -> return Nothing
            Right (RetrieveDerivationResponse mDrv) -> return mDrv
            Right _ -> return Nothing

-- | HasBuildOps instance for Builder (protocol-based)
instance HasBuildOps 'Builder where
    registerBuildResult db buildId result = do
        respEither <- sendDbRequest (RegisterBuildResultRequest buildId result)
        case respEither of
            Left err -> throwError err
            Right (RegisterBuildResultResponse path) -> return path
            Right _ -> throwError $ toBuilderError $ DBProtocolError "Unexpected response type"

    getBuildResult db path = do
        respEither <- sendDbRequest (GetBuildResultRequest path)
        case respEither of
            Left _ -> return Nothing
            Right (GetBuildResultResponse mResult) -> return mResult
            Right _ -> return Nothing

-- | HasGCOps instance for Builder (protocol-based)
instance HasGCOps 'Builder where
    createGCRoot db path name = do
        respEither <- sendDbRequest (CreateGCRootRequest path name)
        case respEither of
            Left err -> throwError err
            Right CreateGCRootResponse -> return ()
            Right _ -> throwError $ toBuilderError $ DBProtocolError "Unexpected response type"

    removeGCRoot db path name = do
        respEither <- sendDbRequest (RemoveGCRootRequest path name)
        case respEither of
            Left err -> throwError err
            Right RemoveGCRootResponse -> return ()
            Right _ -> throwError $ toBuilderError $ DBProtocolError "Unexpected response type"

    listGCRoots db onlyActive = do
        respEither <- sendDbRequest (ListGCRootsRequest onlyActive)
        case respEither of
            Left err -> throwError err
            Right (ListGCRootsResponse names) -> return names
            Right _ -> throwError $ toBuilderError $ DBProtocolError "Unexpected response type"

-- | HasSchemaOps instance for Builder (protocol-based)
instance HasSchemaOps 'Builder where
    getSchemaVersion db = do
        respEither <- sendDbRequest GetSchemaVersionRequest
        case respEither of
            Left err -> throwError err
            Right (GetSchemaVersionResponse version) -> return version
            Right _ -> throwError $ toBuilderError $ DBProtocolError "Unexpected response type"

    updateSchemaVersion db version = do
        respEither <- sendDbRequest (SetSchemaVersionRequest version)
        case respEither of
            Left err -> throwError err
            Right SetSchemaVersionResponse -> return ()
            Right _ -> throwError $ toBuilderError $ DBProtocolError "Unexpected response type"

-- | Initialize the database - this is a daemon operation
initDatabaseDaemon :: SPrivilegeTier 'Daemon -> FilePath -> Int -> IO (Database 'Daemon)
initDatabaseDaemon st dbPath busyTimeout = do
    -- Create directory if needed
    createDirectoryIfMissing True (takeDirectory dbPath)

    -- Check if database exists
    exists <- doesFileExist dbPath
    let newDB = not exists

    -- Open or create database
    conn <- catch
        (SQLite.open dbPath)
        (\(e :: SomeException) ->
            throwIO $ DBConnectionError $ T.pack $ "Failed to open database: " ++ show e)

    -- Set pragmas for performance and correctness
    SQLite.execute_ conn "PRAGMA journal_mode = WAL;"
    SQLite.execute_ conn "PRAGMA synchronous = NORMAL;"
    SQLite.execute_ conn "PRAGMA foreign_keys = ON;"
    SQLite.execute_ conn $ Query $ "PRAGMA busy_timeout = " <> T.pack (show busyTimeout) <> ";"

    -- Create the database structure
    let db = Database {
        dbConn = conn,
        dbPath = dbPath,
        dbInitialized = False,
        dbBusyTimeout = busyTimeout,
        dbMaxRetries = 5,
        dbPrivEvidence = st
    }

    -- Initialize schema if new database
    when newDB $ do
        initializeSchema db

    -- Set file permissions (0600 - owner read/write only)
    setFileMode dbPath 0o600

    -- Update db to indicate initialization complete
    let db' = db { dbInitialized = True }

    -- Return the initialized database
    return db'

-- | Close the database connection
closeDatabaseDaemon :: Database 'Daemon -> IO ()
closeDatabaseDaemon db = catch
    (SQLite.close (dbConn db))
    (\(e :: SomeException) ->
        hPutStrLn stderr $ "Warning: Error closing database: " ++ show e)

-- | Get the database connection from environment
getDatabaseFromEnv :: Database t -> TenM p t (Database t)
getDatabaseFromEnv = return

-- | Roll back transaction on error
rollbackOnError :: Database 'Daemon -> IO ()
rollbackOnError db = catch
    (void $ SQLite.execute_ (dbConn db) "ROLLBACK;")
    (\(e :: SomeException) ->
        hPutStrLn stderr $ "Warning: Error rolling back transaction: " ++ show e)

-- | Initialize the database schema
initializeSchema :: Database 'Daemon -> IO ()
initializeSchema db = do
    -- Use IO-level transaction for initial schema setup
    withTenTransactionIO db Exclusive $ \_ -> do
        -- Create schema version tracking table
        SQLite.execute_ (dbConn db) "CREATE TABLE IF NOT EXISTS SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"

        -- Create Derivations table
        SQLite.execute_ (dbConn db) "CREATE TABLE IF NOT EXISTS Derivations (\
                \id INTEGER PRIMARY KEY, \
                \hash TEXT NOT NULL UNIQUE, \
                \store_path TEXT NOT NULL UNIQUE, \
                \timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now')));"

        -- Create Outputs table
        SQLite.execute_ (dbConn db) "CREATE TABLE IF NOT EXISTS Outputs (\
                \derivation_id INTEGER NOT NULL, \
                \output_name TEXT NOT NULL, \
                \path TEXT NOT NULL UNIQUE, \
                \PRIMARY KEY (derivation_id, output_name), \
                \FOREIGN KEY (derivation_id) REFERENCES Derivations(id));"

        -- Create References table
        SQLite.execute_ (dbConn db) "CREATE TABLE IF NOT EXISTS References (\
                \referrer TEXT NOT NULL, \
                \reference TEXT NOT NULL, \
                \PRIMARY KEY (referrer, reference));"

        -- Create ValidPaths table
        SQLite.execute_ (dbConn db) "CREATE TABLE IF NOT EXISTS ValidPaths (\
                \path TEXT PRIMARY KEY, \
                \hash TEXT NOT NULL, \
                \registration_time INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
                \deriver TEXT, \
                \is_valid INTEGER NOT NULL DEFAULT 1);"

        -- Create GCRoots table
        SQLite.execute_ (dbConn db) "CREATE TABLE IF NOT EXISTS GCRoots (\
                \path TEXT NOT NULL, \
                \name TEXT NOT NULL, \
                \type TEXT NOT NULL, \
                \timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
                \active INTEGER NOT NULL DEFAULT 1, \
                \PRIMARY KEY (path, name));"

        -- Create indices for performance
        SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_derivations_hash ON Derivations(hash);"
        SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_outputs_path ON Outputs(path);"
        SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References(referrer);"
        SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_references_reference ON References(reference);"
        SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_validpaths_hash ON ValidPaths(hash);"
        SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_gcroots_active ON GCRoots(active);"

        -- Set schema version
        SQLite.execute (dbConn db) "INSERT OR REPLACE INTO SchemaVersion (version, updated_at) VALUES (?, CURRENT_TIMESTAMP)" [1 :: Int]
        return ()

-- | Helper function for direct IO-level transactions
withTenTransactionIO :: Database 'Daemon -> TransactionMode -> (Database 'Daemon -> IO a) -> IO a
withTenTransactionIO db mode action = do
    -- Start transaction with appropriate mode
    let beginStmt = case mode of
            ReadOnly -> "BEGIN TRANSACTION READONLY;"
            ReadWrite -> "BEGIN TRANSACTION;"
            Exclusive -> "BEGIN EXCLUSIVE TRANSACTION;"

    -- Execute transaction with proper error handling
    bracket
        (do SQLite.execute_ (dbConn db) (Query beginStmt)
            return db)
        (\_ -> catch
                (SQLite.execute_ (dbConn db) "ROLLBACK;")
                (\(_ :: SomeException) -> return ()))
        (\_ -> do
            result <- action db
            SQLite.execute_ (dbConn db) "COMMIT;"
            return result)

-- | Ensure database directories exist (privileged daemon-only operation)
ensureDBDirectories :: FilePath -> TenM p 'Daemon ()
ensureDBDirectories storeDir = do
    -- Use defaultDBPath from Ten.Core to calculate the path
    let dbDir = takeDirectory (defaultDBPath storeDir)

    -- Create the directory with appropriate permissions
    liftIO $ do
        createDirectoryIfMissing True dbDir
        -- Set appropriate permissions (0700 - rwx------ for owner only)
        setFileMode dbDir 0o700

    logMsg 2 $ "Created database directory: " <> T.pack dbDir

-- | Retry an operation if the database is busy
retryOnBusy :: Database 'Daemon -> IO a -> IO a
retryOnBusy db action = retryWithCount 0
  where
    retryWithCount attempt = catch
        action
        (\(e :: SQLite.SQLError) ->
            case SQLite.sqlError e of
                SQLite.ErrorBusy | attempt < dbMaxRetries db -> do
                    -- Wait with exponential backoff
                    let delayMicros = 10000 * (2 ^ attempt) -- 10ms, 20ms, 40ms, etc.
                    threadDelay delayMicros
                    retryWithCount (attempt + 1)
                SQLite.ErrorLocked | attempt < dbMaxRetries db -> do
                    -- Wait with exponential backoff
                    let delayMicros = 10000 * (2 ^ attempt) -- 10ms, 20ms, 40ms, etc.
                    threadDelay delayMicros
                    retryWithCount (attempt + 1)
                _ -> throwIO e)

-- | Get database connection from the database object (daemon-only)
-- This function is used by all direct database operations
tenQuery :: (ToRow q, FromRow r) => Database 'Daemon -> Query -> q -> TenM p 'Daemon [r]
tenQuery db q params = liftIO $ retryOnBusy db $ SQLite.query (dbConn db) q params

-- | Execute a statement with parameters
tenExecute :: (ToRow q) => Database 'Daemon -> Query -> q -> TenM p 'Daemon ()
tenExecute db q params = liftIO $ retryOnBusy db $ void $ SQLite.execute (dbConn db) q params
