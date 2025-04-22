{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Ten.DB.Core (
    -- Database types
    Database(..),
    DBError(..),
    TransactionMode(..),
    Only(..),  -- Re-export the Only type needed by modules like Ten.GC

    -- Database initialization (daemon-only)
    initDatabase,
    closeDatabase,
    withDatabase,

    -- Transaction management (daemon-only)
    withTransaction,
    withReadTransaction,
    withWriteTransaction,

    -- Low-level query functions (daemon-only)
    dbExecute,
    dbExecute_,
    dbQuery,
    dbQuery_,
    dbExecuteSimple_,

    -- Database metadata (daemon-only)
    getSchemaVersion,
    updateSchemaVersion,

    -- Utility functions for database access
    retryOnBusy,
    ensureDBDirectories
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
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple (Connection, Query(..), ToRow(..), FromRow(..), Only(..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.IO (withFile, IOMode(..), hPutStrLn, stderr)

-- Import from Ten.Core, including all necessary items for database operations
import Ten.Core (
    TenM, BuildEnv(..), BuildError(..), privilegeError, StorePath, Phase(..),
    PrivilegeTier(..), SPrivilegeTier(..), sDaemon, defaultDBPath, BuildState(..),
    currentBuildId, initBuildState_Build, runTen, sBuild, verbosity, logMsg)

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
initDatabase :: SPrivilegeTier 'Daemon -> FilePath -> Int -> IO Database
initDatabase _ dbPath busyTimeout = do
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

-- | Close the database connection
closeDatabase :: Database -> IO ()
closeDatabase db = catch
    (SQLite.close (dbConn db))
    (\(e :: SomeException) ->
        hPutStrLn stderr $ "Warning: Error closing database: " ++ show e)

-- | Run an action with a database connection - requires daemon privileges
withDatabase :: SPrivilegeTier 'Daemon -> FilePath -> Int -> (Database -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withDatabase st dbPath busyTimeout action = do
    -- Verify daemon privileges
    env <- ask
    when (currentPrivilegeTier env /= Daemon) $
        throwError $ privilegeError "Database operations require daemon privileges"

    -- Get the current build ID from the TenM context BEFORE entering IO
    bid <- gets currentBuildId

    -- Create a bracket operation in the daemon TenM context
    liftIO (bracket
        (initDatabase st dbPath busyTimeout)
        closeDatabase
        (\db -> do
            -- Run the action with privilege verification inside IO
            let env' = env { currentPrivilegeTier = Daemon }
            let state = initBuildState_Build bid  -- Use the bid we extracted before
            result <- runTen sBuild st (action db) env' state
            case result of
                Left err -> throwIO err
                Right (val, _) -> return val))

-- | Run a transaction with the specified mode - requires daemon privileges
withTransaction :: Database -> TransactionMode -> (Database -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withTransaction db mode action = do
    -- Verify daemon privileges
    env <- ask
    when (currentPrivilegeTier env /= Daemon) $
        throwError $ privilegeError "Transaction operations require daemon privileges"

    -- Start transaction with appropriate mode
    let beginStmt = case mode of
            ReadOnly -> "BEGIN TRANSACTION READONLY;"
            ReadWrite -> "BEGIN TRANSACTION;"
            Exclusive -> "BEGIN EXCLUSIVE TRANSACTION;"

    -- Execute transaction with proper error handling
    catchError
        (do
            -- Begin transaction
            dbExecuteSimple_ db (Query beginStmt)

            -- Run the action
            result <- action db

            -- Commit transaction
            dbExecuteSimple_ db "COMMIT;"

            return result)
        (\e -> do
            -- Try to roll back on error
            liftIO $ rollbackOnError db
            throwError e)

-- | Shorthand for read-only transaction
withReadTransaction :: Database -> (Database -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withReadTransaction db = withTransaction db ReadOnly

-- | Shorthand for read-write transaction
withWriteTransaction :: Database -> (Database -> TenM 'Build 'Daemon a) -> TenM 'Build 'Daemon a
withWriteTransaction db = withTransaction db ReadWrite

-- | Roll back transaction on error
rollbackOnError :: Database -> IO ()
rollbackOnError db = catch
    (void $ SQLite.execute_ (dbConn db) "ROLLBACK;")
    (\(e :: SomeException) ->
        hPutStrLn stderr $ "Warning: Error rolling back transaction: " ++ show e)

-- | Execute a statement with parameters, returning the number of rows affected - requires daemon privileges
dbExecute :: ToRow q => Database -> Query -> q -> TenM 'Build 'Daemon Int64
dbExecute db query params = do
    -- Verify daemon privileges
    env <- ask
    when (currentPrivilegeTier env /= Daemon) $
        throwError $ privilegeError "Database execute operations require daemon privileges"

    -- Execute the query with retry on busy
    liftIO $ retryOnBusy db $ do
        -- Execute the query
        SQLite.execute (dbConn db) query params
        -- Get the number of changes (rows affected) and convert from Int to Int64
        fromIntegral <$> SQLite.changes (dbConn db)

-- | Execute a statement with parameters, discarding the result - requires daemon privileges
dbExecute_ :: ToRow q => Database -> Query -> q -> TenM 'Build 'Daemon ()
dbExecute_ db query params = void $ dbExecute db query params

-- | Execute a simple SQL statement without parameters - requires daemon privileges
dbExecuteSimple_ :: Database -> Query -> TenM 'Build 'Daemon ()
dbExecuteSimple_ db query = do
    -- Verify daemon privileges
    env <- ask
    when (currentPrivilegeTier env /= Daemon) $
        throwError $ privilegeError "Database execute operations require daemon privileges"

    -- Execute simple query
    liftIO $ retryOnBusy db $ SQLite.execute_ (dbConn db) query

-- | Execute a query with parameters - requires daemon privileges
dbQuery :: (ToRow q, FromRow r) => Database -> Query -> q -> TenM 'Build 'Daemon [r]
dbQuery db q params = do
    -- Verify daemon privileges
    env <- ask
    when (currentPrivilegeTier env /= Daemon) $
        throwError $ privilegeError "Database query operations require daemon privileges"

    -- Execute query with retry
    liftIO $ retryOnBusy db $ SQLite.query (dbConn db) q params

-- | Execute a query without parameters - requires daemon privileges
dbQuery_ :: FromRow r => Database -> Query -> TenM 'Build 'Daemon [r]
dbQuery_ db q = do
    -- Verify daemon privileges
    env <- ask
    when (currentPrivilegeTier env /= Daemon) $
        throwError $ privilegeError "Database query operations require daemon privileges"

    -- Execute simple query with retry
    liftIO $ retryOnBusy db $ SQLite.query_ (dbConn db) q

-- | Get the current schema version
getSchemaVersion :: Database -> TenM 'Build 'Daemon Int
getSchemaVersion db = do
    -- Check if the version table exists
    hasVersionTable <- dbQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM 'Build 'Daemon [Only Int]

    case hasVersionTable of
        [Only count] | count > 0 -> do
            -- Table exists, get the version
            results <- dbQuery_ db "SELECT version FROM SchemaVersion LIMIT 1;" :: TenM 'Build 'Daemon [Only Int]
            case results of
                [Only version] -> return version
                _ -> return 0  -- No version record found
        _ -> return 0  -- Table doesn't exist

-- | Update the schema version
updateSchemaVersion :: Database -> Int -> TenM 'Build 'Daemon ()
updateSchemaVersion db version = do
    -- Check if the version table exists
    hasVersionTable <- dbQuery_ db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name='SchemaVersion';" :: TenM 'Build 'Daemon [Only Int]

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

        -- Set schema version
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

-- | Ensure database directories exist (privileged daemon-only operation)
-- This is now the ONLY implementation of this function in the codebase
ensureDBDirectories :: FilePath -> TenM 'Build 'Daemon ()
ensureDBDirectories storeDir = do
    -- Verify daemon privileges
    env <- ask
    when (currentPrivilegeTier env /= Daemon) $
        throwError $ privilegeError "Database directory creation requires daemon privileges"

    -- Use defaultDBPath from Ten.Core to calculate the path
    let dbDir = takeDirectory (defaultDBPath storeDir)

    -- Create the directory with appropriate permissions
    liftIO $ do
        createDirectoryIfMissing True dbDir
        -- Set appropriate permissions (0700 - rwx------ for owner only)
        setFileMode dbDir 0o700

    logMsg 2 $ "Created database directory: " <> T.pack dbDir

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
