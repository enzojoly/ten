{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

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

-- | Initialize the database
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

-- | Run an action with a database connection
withDatabase :: FilePath -> Int -> (Database -> IO a) -> IO a
withDatabase dbPath busyTimeout = bracket
    (initDatabase dbPath busyTimeout)
    closeDatabase

-- | Run an action with a database in the TenM monad
withDB :: (MonadIO m, MonadError BuildError m, MonadReader BuildEnv m) => (Database -> m a) -> m a
withDB action = do
    env <- ask
    let dbPath = defaultDBPath (storeDir env)
    let busyTimeout = 5000  -- 5 seconds

    -- Create a bracket-like operation in the TenM monad
    bracket
        (liftIO $ initDatabase dbPath busyTimeout)
        (liftIO . closeDatabase)
        action
  where
    -- Implementation of bracket for the TenM monad
    bracket :: MonadIO m => IO a -> (a -> IO b) -> (a -> m c) -> m c
    bracket acquire release action' = do
        resource <- liftIO acquire
        result <- action' resource `onError` liftIO (release resource)
        liftIO $ release resource
        return result

    -- Helper for error handling
    onError :: MonadIO m => m a -> IO b -> m a
    onError action' onErr = do
        result <- action'
        return result

-- | Run a transaction with the specified mode
dbWithTransaction :: Database -> TransactionMode -> (Database -> IO a) -> IO a
dbWithTransaction db mode action = do
    -- Start transaction with appropriate mode
    let beginStmt = case mode of
            ReadOnly -> "BEGIN TRANSACTION READONLY;"
            ReadWrite -> "BEGIN TRANSACTION;"
            Exclusive -> "BEGIN EXCLUSIVE TRANSACTION;"

    -- Execute transaction in a bracket for safety
    catch
        (bracket
            (dbExecuteSimple_ db (Query beginStmt))
            (\_ -> rollbackOnError db)
            (\_ -> do
                result <- action db
                dbExecuteSimple_ db "COMMIT;"
                return result))
        (\(e :: SomeException) -> do
            -- Try to roll back if anything went wrong
            void $ catch
                (dbExecuteSimple_ db "ROLLBACK;")
                (\(_ :: SomeException) -> return ())
            throwIO $ DBTransactionError $ T.pack $ "Transaction failed: " ++ show e)

-- | Alias for dbWithTransaction for backward compatibility
withTransaction :: Database -> TransactionMode -> (Database -> IO a) -> IO a
withTransaction = dbWithTransaction

-- | Run a transaction in the TenM monad
withTenTransaction :: (MonadIO m, MonadError BuildError m) => Database -> TransactionMode -> (Database -> m a) -> m a
withTenTransaction db mode action = do
    -- Start transaction
    let beginStmt = case mode of
            ReadOnly -> "BEGIN TRANSACTION READONLY;"
            ReadWrite -> "BEGIN TRANSACTION;"
            Exclusive -> "BEGIN EXCLUSIVE TRANSACTION;"

    -- Execute the transaction in a bracket-like pattern
    bracketTen
        (liftIO $ dbExecuteSimple_ db (Query beginStmt))
        (\_ -> liftIO $ rollbackTx db)
        (\_ -> do
            result <- action db
            liftIO $ dbExecuteSimple_ db "COMMIT;"
            return result)
  where
    -- Implementation of bracket for TenM monad
    bracketTen :: MonadIO m => IO a -> (a -> IO b) -> (a -> m c) -> m c
    bracketTen acquire release action' = do
        resource <- liftIO acquire
        result <- action' resource `onError` liftIO (release resource)
        liftIO $ release resource
        return result

    -- Helper for error handling
    onError :: MonadIO m => m a -> IO b -> m a
    onError action' onErr = do
        result <- action'
        return result

    -- Roll back transaction
    rollbackTx :: Database -> IO ()
    rollbackTx db' = catch
        (dbExecuteSimple_ db' "ROLLBACK;")
        (\(e :: SomeException) ->
            putStrLn $ "Warning: Error rolling back transaction: " ++ show e)

-- | Roll back transaction on error
rollbackOnError :: Database -> IO ()
rollbackOnError db = catch
    (dbExecuteSimple_ db "ROLLBACK;")
    (\(e :: SomeException) ->
        putStrLn $ "Warning: Error rolling back transaction: " ++ show e)

-- | Execute a statement with parameters, returning the number of rows affected
dbExecute :: ToRow q => Database -> Query -> q -> IO Int64
dbExecute db query params = retryOnBusy db $ do
    -- Execute the query
    SQLite.execute (dbConn db) query params
    -- Get the number of changes (rows affected) and convert from Int to Int64
    fromIntegral <$> SQLite.changes (dbConn db)

-- | Execute a statement with parameters, discarding the result
dbExecute_ :: ToRow q => Database -> Query -> q -> IO ()
dbExecute_ db query params = void $ dbExecute db query params

-- | Execute a simple SQL statement without parameters
dbExecuteSimple_ :: Database -> Query -> IO ()
dbExecuteSimple_ db query = retryOnBusy db $
    SQLite.execute_ (dbConn db) query

-- | Execute a query with parameters
dbQuery :: (ToRow q, FromRow r) => Database -> Query -> q -> IO [r]
dbQuery db q params = retryOnBusy db $
    SQLite.query (dbConn db) q params

-- | Execute a query without parameters
dbQuery_ :: FromRow r => Database -> Query -> IO [r]
dbQuery_ db q = retryOnBusy db $
    SQLite.query_ (dbConn db) q

-- | TenM-integrated query with parameters
tenQuery :: (MonadIO m, MonadError BuildError m, ToRow q, FromRow r) => Database -> Query -> q -> m [r]
tenQuery db query params = do
    result <- liftIO $ try $ dbQuery db query params
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database query error: " ++ show e
        Right rows -> return rows
  where
    try :: IO a -> IO (Either SomeException a)
    try action = catch (Right <$> action) (return . Left)

-- | TenM-integrated query without parameters
tenQuery_ :: (MonadIO m, MonadError BuildError m, FromRow r) => Database -> Query -> m [r]
tenQuery_ db query = do
    result <- liftIO $ try $ dbQuery_ db query
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database query error: " ++ show e
        Right rows -> return rows
  where
    try :: IO a -> IO (Either SomeException a)
    try action = catch (Right <$> action) (return . Left)

-- | TenM-integrated execute with parameters
tenExecute :: (MonadIO m, MonadError BuildError m, ToRow q) => Database -> Query -> q -> m Int64
tenExecute db query params = do
    result <- liftIO $ try $ dbExecute db query params
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database execute error: " ++ show e
        Right count -> return count
  where
    try :: IO a -> IO (Either SomeException a)
    try action = catch (Right <$> action) (return . Left)

-- | TenM-integrated execute with parameters, discarding result
tenExecute_ :: (MonadIO m, MonadError BuildError m, ToRow q) => Database -> Query -> q -> m ()
tenExecute_ db query params = void $ tenExecute db query params

-- | TenM-integrated simple execute
tenExecuteSimple_ :: (MonadIO m, MonadError BuildError m) => Database -> Query -> m ()
tenExecuteSimple_ db query = do
    result <- liftIO $ try $ dbExecuteSimple_ db query
    case result of
        Left e -> throwError $ DBError $ T.pack $ "Database execute error: " ++ show e
        Right () -> return ()
  where
    try :: IO a -> IO (Either SomeException a)
    try action = catch (Right <$> action) (return . Left)

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
getSchemaVersion :: Database -> IO Int
getSchemaVersion db = do
    -- Check if the version table exists
    hasVersionTable <- dbQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: IO [Only Int]

    case hasVersionTable of
        [Only count] | count > 0 -> do
            -- Table exists, get the version
            results <- dbQuery_ db "SELECT version FROM SchemaVersion LIMIT 1;" :: IO [Only Int]
            case results of
                [Only version] -> return version
                _ -> return 0  -- No version record found
        _ -> return 0  -- Table doesn't exist

-- | Update the schema version
updateSchemaVersion :: Database -> Int -> IO ()
updateSchemaVersion db version = do
    -- Check if the version table exists
    hasVersionTable <- dbQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: IO [Only Int]

    case hasVersionTable of
        [Only count] | count > 0 -> do
            -- Table exists, update the version
            dbExecute_ db "UPDATE SchemaVersion SET version = ?, updated_at = CURRENT_TIMESTAMP" [version]
        _ -> do
            -- Table doesn't exist, create it
            dbExecuteSimple_ db "CREATE TABLE SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"
            dbExecute_ db "INSERT INTO SchemaVersion (version, updated_at) VALUES (?, CURRENT_TIMESTAMP)" [version]

-- | Initialize the database schema
initializeSchema :: Database -> IO ()
initializeSchema db = dbWithTransaction db Exclusive $ \_ -> do
    -- Create schema version tracking table
    dbExecuteSimple_ db "CREATE TABLE IF NOT EXISTS SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"

    -- Create Derivations table
    dbExecuteSimple_ db "CREATE TABLE IF NOT EXISTS Derivations (\
                \id INTEGER PRIMARY KEY, \
                \hash TEXT NOT NULL UNIQUE, \
                \store_path TEXT NOT NULL UNIQUE, \
                \timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now')));"

    -- Create Outputs table
    dbExecuteSimple_ db "CREATE TABLE IF NOT EXISTS Outputs (\
                \derivation_id INTEGER NOT NULL, \
                \output_name TEXT NOT NULL, \
                \path TEXT NOT NULL UNIQUE, \
                \PRIMARY KEY (derivation_id, output_name), \
                \FOREIGN KEY (derivation_id) REFERENCES Derivations(id));"

    -- Create References table
    dbExecuteSimple_ db "CREATE TABLE IF NOT EXISTS References (\
                \referrer TEXT NOT NULL, \
                \reference TEXT NOT NULL, \
                \PRIMARY KEY (referrer, reference));"

    -- Create ValidPaths table
    dbExecuteSimple_ db "CREATE TABLE IF NOT EXISTS ValidPaths (\
                \path TEXT PRIMARY KEY, \
                \hash TEXT NOT NULL, \
                \registration_time INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
                \deriver TEXT, \
                \is_valid INTEGER NOT NULL DEFAULT 1);"

    -- Create ActiveBuilds table
    dbExecuteSimple_ db "CREATE TABLE IF NOT EXISTS ActiveBuilds (\
                \build_id TEXT PRIMARY KEY, \
                \path TEXT NOT NULL, \
                \status TEXT NOT NULL, \
                \start_time INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
                \update_time INTEGER NOT NULL DEFAULT (strftime('%s','now')));"

    -- Create GCRoots table
    dbExecuteSimple_ db "CREATE TABLE IF NOT EXISTS GCRoots (\
                \path TEXT NOT NULL, \
                \name TEXT NOT NULL, \
                \type TEXT NOT NULL, \
                \timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
                \active INTEGER NOT NULL DEFAULT 1, \
                \PRIMARY KEY (path, name));"

    -- Create indices for performance
    dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_derivations_hash ON Derivations(hash);"
    dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_outputs_path ON Outputs(path);"
    dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References(referrer);"
    dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_references_reference ON References(reference);"
    dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_validpaths_hash ON ValidPaths(hash);"
    dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_activebuilds_status ON ActiveBuilds(status);"
    dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_gcroots_active ON GCRoots(active);"

    -- Set schema version
    updateSchemaVersion db 1
