{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ten.DB.Core (
    -- Database types
    Database(..),
    DBError(..),
    TransactionMode(..),

    -- Database initialization
    initDatabase,
    closeDatabase,
    withDatabase,

    -- Transaction management
    dbWithTransaction,
    withTransaction,

    -- Low-level query functions
    dbExecute,
    dbExecute_,
    dbQuery,
    dbQuery_,

    -- Database metadata
    getSchemaVersion,
    updateSchemaVersion,

    -- Path for database
    defaultDBPath,

    -- Utility functions
    runDBAction,
    retryOnBusy
) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket, catch, finally, throwIO, Exception, SomeException)
import Control.Monad (when, unless, void, forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (Connection, Query(..), ToRow(..), FromRow(..), Only(..))
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite
import qualified Database.SQLite.Simple.Types as SQLite
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO.Error (catchIOError)
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

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

-- | Default path for the database
defaultDBPath :: FilePath -> FilePath
defaultDBPath storeDir = storeDir </> "var/ten/db/ten.db"

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
            (dbExecute_ db beginStmt)
            (\_ -> rollbackOnError db)
            (\_ -> do
                result <- action db
                dbExecute_ db "COMMIT;"
                return result))
        (\(e :: SomeException) -> do
            -- Try to roll back if anything went wrong
            void $ catch
                (dbExecute_ db "ROLLBACK;")
                (\(_ :: SomeException) -> return ())
            throwIO $ DBTransactionError $ T.pack $ "Transaction failed: " ++ show e)

-- | Alias for dbWithTransaction for backward compatibility
withTransaction :: Database -> TransactionMode -> (Database -> IO a) -> IO a
withTransaction = dbWithTransaction

-- | Roll back transaction on error
rollbackOnError :: Database -> IO ()
rollbackOnError db = catch
    (dbExecute_ db "ROLLBACK;")
    (\(e :: SomeException) ->
        putStrLn $ "Warning: Error rolling back transaction: " ++ show e)

-- | Execute a statement with parameters, returning the number of rows affected
dbExecute :: ToRow q => Database -> Query -> q -> IO Int64
dbExecute db query params = retryOnBusy db $
    SQLite.execute (dbConn db) query params

-- | Execute a statement with parameters, discarding the result
dbExecute_ :: ToRow q => Database -> Query -> q -> IO ()
dbExecute_ db query params = void $ dbExecute db query params

-- | Execute a query with parameters
dbQuery :: (ToRow q, FromRow r) => Database -> Query -> q -> IO [r]
dbQuery db q params = retryOnBusy db $
    SQLite.query (dbConn db) q params

-- | Execute a query without parameters
dbQuery_ :: FromRow r => Database -> Query -> IO [r]
dbQuery_ db q = retryOnBusy db $
    SQLite.query_ (dbConn db) q

-- | Execute a raw SQL statement (for pragmas)
executeRaw :: Connection -> Text -> IO ()
executeRaw conn sql = do
    -- Using SQLite.withStatement to properly manage statement lifecycle
    SQLite.withStatement conn (Query sql) $ \stmt -> do
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
    hasVersionTable <- dbQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';"

    case hasVersionTable of
        [(count :: Int)] | count > 0 -> do
            -- Table exists, get the version
            results <- dbQuery_ db "SELECT version FROM SchemaVersion LIMIT 1;"
            case results of
                [(version :: Int)] -> return version
                _ -> return 0  -- No version record found
        _ -> return 0  -- Table doesn't exist

-- | Update the schema version
updateSchemaVersion :: Database -> Int -> IO ()
updateSchemaVersion db version = do
    -- Check if the version table exists
    hasVersionTable <- dbQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';"

    case hasVersionTable of
        [(count :: Int)] | count > 0 -> do
            -- Table exists, update the version
            dbExecute_ db "UPDATE SchemaVersion SET version = ?, updated_at = CURRENT_TIMESTAMP" [version]
        _ -> do
            -- Table doesn't exist, create it
            dbExecute_ db "CREATE TABLE SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"
            dbExecute_ db "INSERT INTO SchemaVersion (version, updated_at) VALUES (?, CURRENT_TIMESTAMP)" [version]

-- | Initialize the database schema
initializeSchema :: Database -> IO ()
initializeSchema db = dbWithTransaction db Exclusive $ \_ -> do
    -- Create schema version tracking table
    dbExecute_ db "CREATE TABLE IF NOT EXISTS SchemaVersion (version INTEGER NOT NULL, updated_at TEXT NOT NULL);"

    -- Create Derivations table
    dbExecute_ db "CREATE TABLE IF NOT EXISTS Derivations (\
                \id INTEGER PRIMARY KEY, \
                \hash TEXT NOT NULL UNIQUE, \
                \store_path TEXT NOT NULL UNIQUE, \
                \timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now')));"

    -- Create Outputs table
    dbExecute_ db "CREATE TABLE IF NOT EXISTS Outputs (\
                \derivation_id INTEGER NOT NULL, \
                \output_name TEXT NOT NULL, \
                \path TEXT NOT NULL UNIQUE, \
                \PRIMARY KEY (derivation_id, output_name), \
                \FOREIGN KEY (derivation_id) REFERENCES Derivations(id));"

    -- Create References table
    dbExecute_ db "CREATE TABLE IF NOT EXISTS References (\
                \referrer TEXT NOT NULL, \
                \reference TEXT NOT NULL, \
                \PRIMARY KEY (referrer, reference));"

    -- Create ValidPaths table
    dbExecute_ db "CREATE TABLE IF NOT EXISTS ValidPaths (\
                \path TEXT PRIMARY KEY, \
                \hash TEXT NOT NULL, \
                \registration_time INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
                \deriver TEXT, \
                \is_valid INTEGER NOT NULL DEFAULT 1);"

    -- Create indices for performance
    dbExecute_ db "CREATE INDEX IF NOT EXISTS idx_derivations_hash ON Derivations(hash);"
    dbExecute_ db "CREATE INDEX IF NOT EXISTS idx_outputs_path ON Outputs(path);"
    dbExecute_ db "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References(referrer);"
    dbExecute_ db "CREATE INDEX IF NOT EXISTS idx_references_reference ON References(reference);"
    dbExecute_ db "CREATE INDEX IF NOT EXISTS idx_validpaths_hash ON ValidPaths(hash);"

    -- Set schema version
    updateSchemaVersion db 1
