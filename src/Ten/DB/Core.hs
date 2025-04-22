{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Ten.DB.Core (
    -- Database types
    Database(..),
    DBError(..),
    TransactionMode(..),
    Only(..),  -- Re-export the Only type needed by modules like Ten.GC

    -- Database initialization
    initDatabase,
    closeDatabase,
    withDatabase,

    -- Transaction management
    dbWithTransaction,
    withTransaction,
    withTenTransaction,

    -- Low-level query functions
    dbExecute,
    dbExecute_,
    dbExecuteSimple_,
    dbQuery,
    dbQuery_,

    -- TenM-integrated query functions
    tenQuery,
    tenQuery_,
    tenExecute,
    tenExecute_,
    tenExecuteSimple_,

    -- Database metadata
    getSchemaVersion,
    updateSchemaVersion,

    -- Utility functions
    runDBAction,
    retryOnBusy,
    withDB
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch, throwIO, finally, Exception, SomeException)
import Control.Monad (when, void, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Except (MonadError, throwError)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (Connection, Query(..), ToRow(..), FromRow(..), Only(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)

import Ten.Core

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
    deriving (Show, Eq)

instance Exception DBError

-- | Transaction modes
data TransactionMode
    = ReadOnly     -- ^ Read-only transaction
    | ReadWrite    -- ^ Read-write transaction
    | Exclusive    -- ^ Exclusive access
    deriving (Show, Eq)

-- | Database connection wrapper
data Database = Database {
    dbConn :: Connection,         -- ^ SQLite connection
    dbPath :: FilePath,           -- ^ Path to database file
    dbInitialized :: Bool,        -- ^ Whether schema is initialized
    dbBusyTimeout :: Int,         -- ^ Busy timeout in milliseconds
    dbMaxRetries :: Int           -- ^ Maximum number of retries for busy operations
}

-- | Initialize the database - this is a daemon operation
initDatabase :: FilePath -> Int -> IO Database
initDatabase dbPath busyTimeout = do
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
    executeRaw conn "PRAGMA journal_mode = WAL;"
    executeRaw conn "PRAGMA synchronous = NORMAL;"
    executeRaw conn "PRAGMA foreign_keys = ON;"
    executeRaw conn $ "PRAGMA busy_timeout = " <> T.pack (show busyTimeout) <> ";"

    -- Create the database structure
    let db = Database {
        dbConn = conn,
        dbPath = dbPath,
        dbInitialized = False,
        dbBusyTimeout = busyTimeout,
        dbMaxRetries = 5
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

-- | Close the database
closeDatabase :: Database -> IO ()
closeDatabase db = catch
    (SQLite.close (dbConn db))
    (\(e :: SomeException) ->
        putStrLn $ "Warning: Error closing database: " ++ show e)

-- | Run an action with a database connection - requires daemon privileges
withDatabase :: FilePath -> Int -> (Database -> TenM p 'Daemon a) -> TenM p 'Daemon a
withDatabase dbPath busyTimeout action = do
    -- Create a bracket-like operation in the daemon TenM context
    daemonBracket
        (liftIO $ initDatabase dbPath busyTimeout)
        (liftIO . closeDatabase)
        action

-- | Run an action with a database in the daemon TenM monad
withDB :: (Database -> TenM p 'Daemon a) -> TenM p 'Daemon a
withDB action = do
    env <- ask
    let dbPath = defaultDBPath (storeLocation env)
    let busyTimeout = 5000  -- 5 seconds

    -- Create a bracket-like operation in the daemon TenM monad
    daemonBracket
        (liftIO $ initDatabase dbPath busyTimeout)
        (liftIO . closeDatabase)
        action

-- | Bracket operation for daemon context
daemonBracket :: TenM p 'Daemon a -> (a -> TenM p 'Daemon b) -> (a -> TenM p 'Daemon c) -> TenM p 'Daemon c
daemonBracket acquire release action = do
    resource <- acquire
    result <- action resource `onError` release resource
    _ <- release resource
    return result
  where
    onError :: TenM p 'Daemon a -> TenM p 'Daemon b -> TenM p 'Daemon a
    onError op cleanup =
      catchError op (\e -> cleanup >> throwError e)

-- | Run a transaction with the specified mode - requires daemon privileges
dbWithTransaction :: Database -> TransactionMode -> (Database -> TenM p 'Daemon a) -> TenM p 'Daemon a
dbWithTransaction db mode action = do
    -- Start transaction with appropriate mode
    let beginStmt = case mode of
            ReadOnly -> "BEGIN TRANSACTION READONLY;"
            ReadWrite -> "BEGIN TRANSACTION;"
            Exclusive -> "BEGIN EXCLUSIVE TRANSACTION;"

    -- Execute transaction with proper error handling
    (flip catchError) (\e -> do
            -- Try to roll back on error
            liftIO $ rollbackOnError db
            throwError e) $ do

        -- Begin transaction
        tenExecuteSimple_ db (Query beginStmt)

        -- Run the action
        result <- action db

        -- Commit transaction
        tenExecuteSimple_ db "COMMIT;"

        return result

-- | Alias for dbWithTransaction for backward compatibility
withTransaction :: Database -> TransactionMode -> (Database -> TenM p 'Daemon a) -> TenM p 'Daemon a
withTransaction = dbWithTransaction

-- | Run a transaction in the TenM monad - requires daemon privileges
withTenTransaction :: Database -> TransactionMode -> (Database -> TenM p 'Daemon a) -> TenM p 'Daemon a
withTenTransaction = dbWithTransaction

-- | Roll back transaction on error
rollbackOnError :: Database -> IO ()
rollbackOnError db = catch
    (void $ SQLite.execute_ (dbConn db) "ROLLBACK;")
    (\(e :: SomeException) ->
        putStrLn $ "Warning: Error rolling back transaction: " ++ show e)

-- | Execute a statement with parameters, returning the number of rows affected - requires daemon privileges
dbExecute :: ToRow q => Database -> Query -> q -> TenM p 'Daemon Int64
dbExecute db query params = do
    -- Execute the query with retry on busy
    rows <- liftIO $ retryOnBusy db $ do
        -- Execute the query
        SQLite.execute (dbConn db) query params
        -- Get the number of changes (rows affected) and convert from Int to Int64
        fromIntegral <$> SQLite.changes (dbConn db)
    return rows

-- | Execute a statement with parameters, discarding the result - requires daemon privileges
dbExecute_ :: ToRow q => Database -> Query -> q -> TenM p 'Daemon ()
dbExecute_ db query params = void $ dbExecute db query params

-- | Execute a simple SQL statement without parameters - requires daemon privileges
dbExecuteSimple_ :: Database -> Query -> TenM p 'Daemon ()
dbExecuteSimple_ db query = liftIO $
    retryOnBusy db $ SQLite.execute_ (dbConn db) query

-- | Execute a query with parameters - requires daemon privileges
dbQuery :: (ToRow q, FromRow r) => Database -> Query -> q -> TenM p 'Daemon [r]
dbQuery db q params = liftIO $
    retryOnBusy db $ SQLite.query (dbConn db) q params

-- | Execute a query without parameters - requires daemon privileges
dbQuery_ :: FromRow r => Database -> Query -> TenM p 'Daemon [r]
dbQuery_ db q = liftIO $
    retryOnBusy db $ SQLite.query_ (dbConn db) q

-- | TenM-integrated query with parameters - requires daemon privileges
tenQuery :: (ToRow q, FromRow r) => Database -> Query -> q -> TenM p 'Daemon [r]
tenQuery db query params = do
    result <- liftIO $ try $ runTenDaemonDB $ dbQuery db query params
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database query error: " ++ show e
        Right rows -> return rows
  where
    runTenDaemonDB :: TenM p 'Daemon a -> IO a
    runTenDaemonDB action = do
        env <- initDaemonEnv "/tmp" "/nix/store" Nothing
        let state = initBuildState (case undefined :: p of
                      'Eval -> Eval
                      'Build -> Build) (BuildIdFromInt 0)
        result <- runTenDaemon action env state
        case result of
            Left err -> throwIO err
            Right (val, _) -> return val

-- | TenM-integrated query without parameters - requires daemon privileges
tenQuery_ :: FromRow r => Database -> Query -> TenM p 'Daemon [r]
tenQuery_ db query = do
    result <- liftIO $ try $ runTenDaemonDB $ dbQuery_ db query
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database query error: " ++ show e
        Right rows -> return rows
  where
    runTenDaemonDB :: TenM p 'Daemon a -> IO a
    runTenDaemonDB action = do
        env <- initDaemonEnv "/tmp" "/nix/store" Nothing
        let state = initBuildState (case undefined :: p of
                      'Eval -> Eval
                      'Build -> Build) (BuildIdFromInt 0)
        result <- runTenDaemon action env state
        case result of
            Left err -> throwIO err
            Right (val, _) -> return val

-- | TenM-integrated execute with parameters - requires daemon privileges
tenExecute :: (ToRow q) => Database -> Query -> q -> TenM p 'Daemon Int64
tenExecute db query params = do
    result <- liftIO $ try $ runTenDaemonDB $ dbExecute db query params
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database execute error: " ++ show e
        Right count -> return count
  where
    runTenDaemonDB :: TenM p 'Daemon a -> IO a
    runTenDaemonDB action = do
        env <- initDaemonEnv "/tmp" "/nix/store" Nothing
        let state = initBuildState (case undefined :: p of
                      'Eval -> Eval
                      'Build -> Build) (BuildIdFromInt 0)
        result <- runTenDaemon action env state
        case result of
            Left err -> throwIO err
            Right (val, _) -> return val

-- | TenM-integrated execute with parameters, discarding result - requires daemon privileges
tenExecute_ :: (ToRow q) => Database -> Query -> q -> TenM p 'Daemon ()
tenExecute_ db query params = void $ tenExecute db query params

-- | TenM-integrated simple execute - requires daemon privileges
tenExecuteSimple_ :: Database -> Query -> TenM p 'Daemon ()
tenExecuteSimple_ db query = do
    result <- liftIO $ try $ runTenDaemonDB $ dbExecuteSimple_ db query
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database execute error: " ++ show e
        Right () -> return ()
  where
    runTenDaemonDB :: TenM p 'Daemon a -> IO a
    runTenDaemonDB action = do
        env <- initDaemonEnv "/tmp" "/nix/store" Nothing
        let state = initBuildState (case undefined :: p of
                      'Eval -> Eval
                      'Build -> Build) (BuildIdFromInt 0)
        result <- runTenDaemon action env state
        case result of
            Left err -> throwIO err
            Right (val, _) -> return val

-- | Execute a raw SQL statement (for pragmas)
executeRaw :: Connection -> Text -> IO ()
executeRaw conn sql = do
    -- Using SQLite.execute_ directly instead of withStatement to avoid unused variable
    void $ SQLite.execute_ conn (Query sql)

-- | Run a database action with proper error handling
runDBAction :: Database -> IO a -> IO a
runDBAction db action = catch
    action
    (\(e :: SQLite.SQLError) ->
        case SQLite.sqlError e of
            SQLite.ErrorBusy -> throwIO $ DBLockError $ T.pack $ "Database is busy: " ++ show e
            SQLite.ErrorLocked -> throwIO $ DBLockError $ T.pack $ "Database is locked: " ++ show e
            SQLite.ErrorNoMemory -> throwIO $ DBResourceError $ T.pack $ "Out of memory: " ++ show e
            SQLite.ErrorIO -> throwIO $ DBConnectionError $ T.pack $ "I/O error: " ++ show e
            _ -> throwIO $ DBQueryError $ T.pack $ "Database error: " ++ show e)

-- | Retry an operation if the database is busy
retryOnBusy :: Database -> IO a -> IO a
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

-- | Get the current schema version
getSchemaVersion :: Database -> TenM p 'Daemon Int
getSchemaVersion db = do
    -- Check if the version table exists
    hasVersionTable <- tenQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM p 'Daemon [Only Int]

    case hasVersionTable of
        [Only count] | count > 0 -> do
            -- Table exists, get the version
            results <- tenQuery_ db "SELECT version FROM SchemaVersion LIMIT 1;" :: TenM p 'Daemon [Only Int]
            case results of
                [Only version] -> return version
                _ -> return 0  -- No version record found
        _ -> return 0  -- Table doesn't exist

-- | Update the schema version
updateSchemaVersion :: Database -> Int -> TenM p 'Daemon ()
updateSchemaVersion db version = do
    -- Check if the version table exists
    hasVersionTable <- tenQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM p 'Daemon [Only Int]

    case hasVersionTable of
        [Only count] | count > 0 -> do
            -- Table exists, update the version
            tenExecute_ db "UPDATE SchemaVersion SET version = ?, updated_at = CURRENT_TIMESTAMP" [version]
        _ -> do
            -- Table doesn't exist, create it
            tenExecuteSimple_ db "CREATE TABLE SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"
            tenExecute_ db "INSERT INTO SchemaVersion (version, updated_at) VALUES (?, CURRENT_TIMESTAMP)" [version]

-- | Initialize the database schema
initializeSchema :: Database -> IO ()
initializeSchema db = runDBAction db $ withTransactionIO db Exclusive $ \_ -> do
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

    -- Create ActiveBuilds table
    SQLite.execute_ (dbConn db) "CREATE TABLE IF NOT EXISTS ActiveBuilds (\
                \build_id TEXT PRIMARY KEY, \
                \path TEXT NOT NULL, \
                \status TEXT NOT NULL, \
                \start_time INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
                \update_time INTEGER NOT NULL DEFAULT (strftime('%s','now')));"

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
    SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_activebuilds_status ON ActiveBuilds(status);"
    SQLite.execute_ (dbConn db) "CREATE INDEX IF NOT EXISTS idx_gcroots_active ON GCRoots(active);"

    -- Set schema version (using direct DB access since we're in IO)
    SQLite.execute (dbConn db) "INSERT OR REPLACE INTO SchemaVersion (version, updated_at) VALUES (?, CURRENT_TIMESTAMP)" [1 :: Int]
    return ()

-- | Helper function for direct IO-level transactions
withTransactionIO :: Database -> TransactionMode -> (Database -> IO a) -> IO a
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
