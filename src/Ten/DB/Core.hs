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
    Only(..),  -- Re-export the Only type needed by modules like Ten.GC

    -- Type classes for database capabilities
    CanAccessDatabaseConn(..),
    CanManageTransactions(..),
    CanExecuteQuery(..),
    CanExecuteStatement(..),
    CanManageSchema(..),

    -- Evidence passing
    withDbPrivileges,

    -- Database initialization and access
    initDatabaseDaemon,
    closeDatabaseDaemon,
    getDatabaseFromEnv,

    -- Transaction handling
    withTransaction,
    withReadTransaction,
    withWriteTransaction,

    -- Utility functions
    retryOnBusy,
    ensureDBDirectories,

    -- Helper functions for implementing protocol-based operations
    sendDbRequest,
    receiveDbResponse,
    serializeQueryParams,
    deserializeQueryResults,

    -- Re-exported types for convenience
    Query(..)
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch, throwIO, finally, Exception, SomeException)
import Control.Monad (when, void, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, local, ReaderT, runReaderT, asks)
import Control.Monad.Except (MonadError, throwError, catchError, ExceptT, runExceptT)
import Control.Monad.State (StateT, runStateT, get, put, gets)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (Connection, Query(..), ToRow(..), FromRow(..), Only(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.IO (withFile, IOMode(..), hPutStrLn, stderr)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson

import qualified Data.Binary as Binary

-- Import from Ten.Core, including all necessary items for database operations
import Ten.Core (
    TenM, BuildEnv(..), BuildError(..), privilegeError, StorePath, Phase(..),
    PrivilegeTier(..), SPrivilegeTier(..), sDaemon, sBuilder, SPhase(..), sBuild, sEval,
    fromSing, defaultDBPath, BuildState(..),
    Database(..), TransactionMode(..),
    CanAccessDatabase, -- Not using direct methods
    currentBuildId, initBuildState, runTen, verbosity, logMsg,
    runMode, RunMode(..), DaemonConnection, sendRequestSync, Request(..),
    DaemonResponse(..), buildErrorToText, serializeDerivation, deserializeDerivation,
    daemonStatus, brOutputPaths, dbInitialized, -- Add record field
    liftTenIO
    )

-- | Database error types
data DBError
    = DBConnectionError Text
    | DBQueryError Text
    | DBSchemaError Text
    | DBTransactionError Text
    | DBLockError Text
    | DBResourceError Text
    | DBMigrationError Text
    | DBInvalidStateError Text
    | DBPhaseError Text
    | DBPermissionError Text
    | DBPrivilegeError Text  -- Error type for privilege violations
    | DBProtocolError Text   -- Error with client-daemon protocol
    deriving (Show, Eq)

instance Exception DBError

-- | Convert DBError to BuildError
toBuilderError :: DBError -> BuildError
toBuilderError (DBConnectionError msg) = DBError msg
toBuilderError (DBQueryError msg) = DBError msg
toBuilderError (DBSchemaError msg) = DBError msg
toBuilderError (DBTransactionError msg) = DBError msg
toBuilderError (DBLockError msg) = DBError msg
toBuilderError (DBResourceError msg) = DBError msg
toBuilderError (DBMigrationError msg) = DBError msg
toBuilderError (DBInvalidStateError msg) = DBError msg
toBuilderError (DBPhaseError msg) = DBError msg
toBuilderError (DBPermissionError msg) = DBError msg
toBuilderError (DBPrivilegeError msg) = DBError msg
toBuilderError (DBProtocolError msg) = DBError msg

-- | Helper to unwrap the evidence from a Database
getDbPrivEvidence :: Database t -> SPrivilegeTier t
getDbPrivEvidence = dbPrivEvidence

-- | Evidence passing function for Database operations
withDbPrivileges :: Database t -> (SPrivilegeTier t -> TenM p t a) -> TenM p t a
withDbPrivileges db f = f (getDbPrivEvidence db)

-- | DaemonResponse helper functions
isDaemonResponseOk :: DaemonResponse -> Bool
isDaemonResponseOk (ErrorResponse _) = False
isDaemonResponseOk _ = True

getDaemonResponseMessage :: DaemonResponse -> Text
getDaemonResponseMessage (ErrorResponse err) = buildErrorToText err
getDaemonResponseMessage SuccessResponse = "Success"
getDaemonResponseMessage _ = "Unknown response"

getDaemonResponsePayload :: DaemonResponse -> Maybe ByteString
getDaemonResponsePayload (StoreReadResponse content) = Just content
getDaemonResponsePayload _ = Nothing

getRowsAffected :: DaemonResponse -> Maybe Int64
getRowsAffected SuccessResponse = Just 0  -- Default to 0 affected rows
getRowsAffected _ = Nothing

getSchemaVersionFromResponse :: DaemonResponse -> Maybe Int
getSchemaVersionFromResponse (StatusResponse status) = Just (read (T.unpack (daemonStatus status)))
getSchemaVersionFromResponse _ = Nothing

-- | Type class for database connection operations
class CanAccessDatabaseConn (t :: PrivilegeTier) where
    getDatabaseConn :: TenM p t (Database t)
    withDatabaseAccess :: (Database t -> TenM p t a) -> TenM p t a

-- | Type class for transaction management
class CanManageTransactions (t :: PrivilegeTier) where
    withTransaction :: Database t -> TransactionMode -> (Database t -> TenM p t a) -> TenM p t a
    withReadTransaction :: Database t -> (Database t -> TenM p t a) -> TenM p t a
    withWriteTransaction :: Database t -> (Database t -> TenM p t a) -> TenM p t a

-- | Type class for query execution
class CanExecuteQuery (t :: PrivilegeTier) where
    query :: (ToRow q, FromRow r, Aeson.FromJSON r) => Database t -> Query -> q -> TenM p t [r]
    query_ :: (FromRow r, Aeson.FromJSON r) => Database t -> Query -> TenM p t [r]

-- | Type class for statement execution
class CanExecuteStatement (t :: PrivilegeTier) where
    execute :: (ToRow q) => Database t -> Query -> q -> TenM p t Int64
    execute_ :: (ToRow q) => Database t -> Query -> q -> TenM p t ()
    executeSimple_ :: Database t -> Query -> TenM p t ()

-- | Type class for schema management
class CanManageSchema (t :: PrivilegeTier) where
    -- | Get the current schema version
    getSchemaVersion :: Database t -> TenM p t Int

    -- | Update the schema version
    updateSchemaVersion :: Database t -> Int -> TenM p t ()

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

-- | Instance for Daemon (direct database access)
instance CanAccessDatabaseConn 'Daemon where
    getDatabaseConn = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)
        db <- liftIO $ initDatabaseDaemon sDaemon dbPath 5000
        return db

    withDatabaseAccess action = do
        env <- ask
        let dbPath = defaultDBPath (storeLocation env)

        -- Use bracketTenM instead of bracket
        bracketTenM
            (liftIO $ initDatabaseDaemon sDaemon dbPath 5000)  -- acquire
            (\db -> liftIO $ closeDatabaseDaemon db)          -- release
            action                                            -- action

-- | Instance for Builder (protocol-based access)
instance CanAccessDatabaseConn 'Builder where
    getDatabaseConn = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Create a Database with Builder evidence but no actual connection
                let dbPath = defaultDBPath (storeLocation env)
                return $ Database {
                    dbConn = error "Cannot access direct DB connection in Builder context",
                    dbPath = dbPath,
                    dbInitialized = True,
                    dbBusyTimeout = 5000,
                    dbMaxRetries = 5,
                    dbPrivEvidence = sBuilder
                }
            _ -> throwError $ privilegeError "Database access requires daemon connection"

    withDatabaseAccess action = do
        -- For Builder, just get the virtual Database and pass it to the action
        db <- getDatabaseConn
        action db

-- | Daemon instance for direct transaction management
instance CanManageTransactions 'Daemon where
    withTransaction db mode action =
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

    withReadTransaction db = withTransaction db ReadOnly
    withWriteTransaction db = withTransaction db ReadWrite

-- | Builder instance for transaction management via protocol
instance CanManageTransactions 'Builder where
    withTransaction db mode action = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Send transaction begin request
                let txnType = case mode of
                        ReadOnly -> "readonly"
                        ReadWrite -> "readwrite"
                        Exclusive -> "exclusive"

                let beginReq = Request {
                        reqId = 0,
                        reqType = "db-begin-transaction",
                        reqParams = Map.singleton "mode" txnType,
                        reqPayload = Nothing
                    }

                beginResp <- liftIO $ sendRequestSync conn beginReq 30000000
                case beginResp of
                    Left err -> throwError err
                    Right resp ->
                        if isDaemonResponseOk resp
                            then do
                                -- Run the action
                                result <- catchError (action db) $ \e -> do
                                    -- Roll back on error
                                    let rollbackReq = Request {
                                            reqId = 0,
                                            reqType = "db-rollback-transaction",
                                            reqParams = Map.empty,
                                            reqPayload = Nothing
                                        }
                                    void $ liftIO $ sendRequestSync conn rollbackReq 30000000
                                    throwError e

                                -- Commit transaction
                                let commitReq = Request {
                                        reqId = 0,
                                        reqType = "db-commit-transaction",
                                        reqParams = Map.empty,
                                        reqPayload = Nothing
                                    }

                                commitResp <- liftIO $ sendRequestSync conn commitReq 30000000
                                case commitResp of
                                    Left err -> throwError err
                                    Right resp' ->
                                        if isDaemonResponseOk resp'
                                            then return result
                                            else throwError $ toBuilderError $ DBTransactionError $ getDaemonResponseMessage resp'
                            else throwError $ toBuilderError $ DBTransactionError $ getDaemonResponseMessage resp

            _ -> throwError $ privilegeError "Transaction management requires daemon connection"

    withReadTransaction db = withTransaction db ReadOnly
    withWriteTransaction db = withTransaction db ReadWrite

-- | Daemon instance for direct query execution
instance CanExecuteQuery 'Daemon where
    query db q params = do
        -- Execute query with retry
        liftIO $ retryOnBusy db $ SQLite.query (dbConn db) q params

    query_ db q = do
        -- Execute simple query with retry
        liftIO $ retryOnBusy db $ SQLite.query_ (dbConn db) q

-- | Builder instance for query execution via protocol
instance CanExecuteQuery 'Builder where
    query db q params = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Serialize query and parameters
                let queryStr = case q of
                        Query qs -> qs

                -- Convert params to JSON
                let serializedParams = serializeQueryParams params

                let queryReq = Request {
                        reqId = 0,
                        reqType = "db-query",
                        reqParams = Map.fromList [
                            ("query", queryStr),
                            ("params", serializedParams)
                        ],
                        reqPayload = Nothing
                    }

                queryResp <- liftIO $ sendRequestSync conn queryReq 30000000
                case queryResp of
                    Left err -> throwError err
                    Right resp ->
                        if isDaemonResponseOk resp
                            then case getDaemonResponsePayload resp of
                                Just payload ->
                                    deserializeQueryResults payload
                                Nothing -> throwError $ toBuilderError $ DBQueryError "Missing query results"
                            else throwError $ toBuilderError $ DBQueryError $ getDaemonResponseMessage resp

            _ -> throwError $ privilegeError "Query execution requires daemon connection"

    query_ db q = query db q ()

-- | Daemon instance for direct statement execution
instance CanExecuteStatement 'Daemon where
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

-- | Builder instance for statement execution via protocol
instance CanExecuteStatement 'Builder where
    execute db query params = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                -- Serialize query and parameters
                let queryStr = case query of
                        Query qs -> qs

                -- Convert params to JSON
                let serializedParams = serializeQueryParams params

                let execReq = Request {
                        reqId = 0,
                        reqType = "db-execute",
                        reqParams = Map.fromList [
                            ("query", queryStr),
                            ("params", serializedParams)
                        ],
                        reqPayload = Nothing
                    }

                execResp <- liftIO $ sendRequestSync conn execReq 30000000
                case execResp of
                    Left err -> throwError err
                    Right resp ->
                        if isDaemonResponseOk resp
                            then case getRowsAffected resp of
                                Just count -> return count
                                Nothing -> throwError $ toBuilderError $ DBQueryError "Missing row count"
                            else throwError $ toBuilderError $ DBQueryError $ getDaemonResponseMessage resp

            _ -> throwError $ privilegeError "Statement execution requires daemon connection"

    execute_ db query params = void $ execute db query params

    executeSimple_ db query = execute_ db query ()

instance Aeson.FromJSON (Only Int) where
    parseJSON = Aeson.withArray "Only Int" $ \arr ->
        if length arr == 1
            then do
                val <- Aeson.parseJSON (arr Aeson.! 0)
                return (Only val)
            else fail "Expected array of length 1 for Only Int"

-- | Daemon instance for direct schema management
instance CanManageSchema 'Daemon where
    getSchemaVersion db = do
        -- Check if the version table exists
        hasVersionTable <- query_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM p 'Daemon [Only Int]

        case hasVersionTable of
            [Only count] | count > 0 -> do
                -- Table exists, get the version
                results <- query_ db "SELECT version FROM SchemaVersion LIMIT 1;" :: TenM p 'Daemon [Only Int]
                case results of
                    [Only version] -> return version
                    _ -> return 0  -- No version record found
            _ -> return 0  -- Table doesn't exist

    updateSchemaVersion db version = do
        -- Check if the version table exists
        hasVersionTable <- query_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM p 'Daemon [Only Int]

        case hasVersionTable of
            [Only count] | count > 0 -> do
                -- Table exists, update the version
                execute_ db "UPDATE SchemaVersion SET version = ?, updated_at = CURRENT_TIMESTAMP" [version]
            _ -> do
                -- Table doesn't exist, create it
                executeSimple_ db "CREATE TABLE SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"
                execute_ db "INSERT INTO SchemaVersion (version, updated_at) VALUES (?, CURRENT_TIMESTAMP)" [version]

-- | Builder instance for schema management via protocol
instance CanManageSchema 'Builder where
    getSchemaVersion db = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let versionReq = Request {
                        reqId = 0,
                        reqType = "db-get-schema-version",
                        reqParams = Map.empty,
                        reqPayload = Nothing
                    }

                versionResp <- liftIO $ sendRequestSync conn versionReq 30000000
                case versionResp of
                    Left err -> throwError err
                    Right resp ->
                        if isDaemonResponseOk resp
                            then case getSchemaVersionFromResponse resp of
                                Just version -> return version
                                Nothing -> throwError $ toBuilderError $ DBSchemaError "Missing version"
                            else throwError $ toBuilderError $ DBSchemaError $ getDaemonResponseMessage resp

            _ -> throwError $ privilegeError "Schema management requires daemon connection"

    updateSchemaVersion db version = do
        env <- ask
        case runMode env of
            ClientMode conn -> do
                let updateReq = Request {
                        reqId = 0,
                        reqType = "db-update-schema-version",
                        reqParams = Map.singleton "version" (T.pack $ show version),
                        reqPayload = Nothing
                    }

                updateResp <- liftIO $ sendRequestSync conn updateReq 30000000
                case updateResp of
                    Left err -> throwError err
                    Right resp ->
                        if isDaemonResponseOk resp
                            then return ()
                            else throwError $ toBuilderError $ DBSchemaError $ getDaemonResponseMessage resp

            _ -> throwError $ privilegeError "Schema management requires daemon connection"

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

-- | Get the database from environment
getDatabaseFromEnv :: (CanAccessDatabaseConn t) => TenM p t (Database t)
getDatabaseFromEnv = getDatabaseConn

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
    withTransactionIO db Exclusive $ \_ -> do
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
withTransactionIO :: Database 'Daemon -> TransactionMode -> (Database 'Daemon -> IO a) -> IO a
withTransactionIO db mode action = do
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

-- | Helper function for sending database requests to daemon
sendDbRequest :: Text -> Map.Map Text Text -> Maybe ByteString -> TenM p 'Builder DaemonResponse
sendDbRequest reqType params payload = do
    env <- ask
    case runMode env of
        ClientMode conn -> do
            let req = Request {
                    reqId = 0,
                    reqType = reqType,
                    reqParams = params,
                    reqPayload = payload
                }

            respEither <- liftIO $ sendRequestSync conn req 30000000
            case respEither of
                Left err -> throwError err
                Right resp -> return resp

        _ -> throwError $ privilegeError "Cannot send database request without daemon connection"

-- | Helper function to receive and process database responses
receiveDbResponse :: DaemonResponse -> (DaemonResponse -> Either BuildError a) -> TenM p 'Builder a
receiveDbResponse resp parser =
    case parser resp of
        Left err -> throwError err
        Right result -> return result

-- | Serialize query parameters
serializeQueryParams :: (ToRow q) => q -> Text
serializeQueryParams params =
    -- Convert the parameters to SQLite data and manually serialize them
    let paramValues = toRow params
        -- Custom serialization for each SQLite data type
        serializeSQLData :: SQLite.SQLData -> Text
        serializeSQLData SQLite.SQLNull = "null"
        serializeSQLData (SQLite.SQLText txt) = "\"" <> T.replace "\"" "\\\"" txt <> "\""
        serializeSQLData (SQLite.SQLInteger i) = T.pack (show i)
        serializeSQLData (SQLite.SQLFloat f) = T.pack (show f)
        serializeSQLData (SQLite.SQLBlob b) = "\"" <> T.pack (show b) <> "\"" -- Base64 would be better

        -- Build a simple JSON array manually
        serializedValues = map serializeSQLData paramValues
        jsonArray = "[" <> T.intercalate "," serializedValues <> "]"
    in jsonArray

-- | Deserialize query results
deserializeQueryResults :: (FromRow r, Aeson.FromJSON r) => ByteString -> TenM p 'Builder [r]
deserializeQueryResults payload = do
    -- Properly decode the binary response payload
    case Aeson.eitherDecodeStrict payload of
        Left err -> throwError $ toBuilderError $ DBQueryError $ "Failed to parse query results: " <> T.pack err
        Right (rows :: Aeson.Value) -> do
            -- Process the JSON value as an array of rows
            case Aeson.fromJSON rows of
                Aeson.Error err -> throwError $ toBuilderError $ DBQueryError $ "Failed to convert query results: " <> T.pack err
                Aeson.Success result -> return result
