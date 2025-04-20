{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Ten.DB.Schema (
    -- Schema versions
    SchemaVersion,
    currentSchemaVersion,

    -- Schema initialization and management
    ensureSchema,
    validateSchema,
    migrateSchema,

    -- Schema operations
    createTables,
    createIndices,

    -- Schema definitions
    derivationsTableDef,
    outputsTableDef,
    referencesTableDef,
    validPathsTableDef,

    -- Schema migrations
    Migration(..),
    migrations,

    -- Schema queries
    tableExists,
    columnExists,
    indexExists,

    -- Schema metadata
    SchemaElement(..),
    getSchemaElements,

    -- Schema errors
    SchemaError(..)
) where

import Control.Exception (catch, throwIO, try, SomeException, Exception)
import Control.Monad (forM_, when, unless, void)
import Data.List (sort, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Types

import qualified Ten.DB.Core as DBCore

-- | Schema version type
type SchemaVersion = Int

-- | Current schema version
currentSchemaVersion :: SchemaVersion
currentSchemaVersion = 1

-- | Migration definition
data Migration = Migration {
    migrationVersion :: SchemaVersion,
    migrationDescription :: Text,
    migrationUp :: DBCore.Database -> IO (),
    migrationDown :: DBCore.Database -> IO ()
}

-- | Schema element types
data SchemaElement =
    Table Text           -- ^ A database table
  | Index Text           -- ^ A database index
  | Column Text Text     -- ^ A column in a table (table, column)
  | Constraint Text Text -- ^ A constraint (table, constraint name)
  | Trigger Text         -- ^ A database trigger
  deriving (Show, Eq)

-- | Schema-related errors
data SchemaError =
    SchemaMissingTable Text
  | SchemaMissingColumn Text Text
  | SchemaMissingIndex Text
  | SchemaInvalidVersion SchemaVersion SchemaVersion
  | SchemaMigrationFailed SchemaVersion Text
  | SchemaValidationFailed Text
  deriving (Show, Eq)

instance Exception SchemaError

-- | Ensure the database schema is properly set up
ensureSchema :: DBCore.Database -> IO ()
ensureSchema db = do
    -- Get current schema version
    currentVersion <- DBCore.getSchemaVersion db

    if currentVersion == 0
        then do
            -- New database, create schema from scratch
            createTables db
            createIndices db
            DBCore.updateSchemaVersion db currentSchemaVersion
        else if currentVersion < currentSchemaVersion
            then do
                -- Existing database needs migration
                migrateSchema db currentVersion currentSchemaVersion
            else if currentVersion > currentSchemaVersion
                then do
                    -- Database is newer than our code!
                    throwIO $ SchemaInvalidVersion currentVersion currentSchemaVersion
                else do
                    -- Database is current, just validate
                    validateSchema db

-- | Create all database tables
createTables :: DBCore.Database -> IO ()
createTables db = DBCore.withTransaction db DBCore.Exclusive $ \_ -> do
    -- Create Derivations table
    execute_ (DBCore.dbConn db) derivationsTableDef

    -- Create Outputs table
    execute_ (DBCore.dbConn db) outputsTableDef

    -- Create References table
    execute_ (DBCore.dbConn db) referencesTableDef

    -- Create ValidPaths table
    execute_ (DBCore.dbConn db) validPathsTableDef

-- | Create all indices for performance
createIndices :: DBCore.Database -> IO ()
createIndices db = DBCore.withTransaction db DBCore.Exclusive $ \_ -> do
    -- Derivations indices
    execute_ (DBCore.dbConn db) "CREATE INDEX IF NOT EXISTS idx_derivations_hash ON Derivations(hash);"

    -- Outputs indices
    execute_ (DBCore.dbConn db) "CREATE INDEX IF NOT EXISTS idx_outputs_path ON Outputs(path);"
    execute_ (DBCore.dbConn db) "CREATE INDEX IF NOT EXISTS idx_outputs_derivation ON Outputs(derivation_id);"

    -- References indices
    execute_ (DBCore.dbConn db) "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References(referrer);"
    execute_ (DBCore.dbConn db) "CREATE INDEX IF NOT EXISTS idx_references_reference ON References(reference);"

    -- ValidPaths indices
    execute_ (DBCore.dbConn db) "CREATE INDEX IF NOT EXISTS idx_validpaths_hash ON ValidPaths(hash);"
    execute_ (DBCore.dbConn db) "CREATE INDEX IF NOT EXISTS idx_validpaths_deriver ON ValidPaths(deriver);"

-- | Validate the schema is correct
validateSchema :: DBCore.Database -> IO ()
validateSchema db = do
    -- Get all required tables
    let requiredTables = [
            "Derivations",
            "Outputs",
            "References",
            "ValidPaths"
          ]

    -- Check each table exists
    forM_ requiredTables $ \tableName -> do
        exists <- tableExists db tableName
        unless exists $ throwIO $ SchemaMissingTable tableName

    -- Check key columns
    validateDerivationsTable db
    validateOutputsTable db
    validateReferencesTable db
    validateValidPathsTable db

-- | Validate Derivations table structure
validateDerivationsTable :: DBCore.Database -> IO ()
validateDerivationsTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "Derivations" col
  where
    requiredColumns = ["id", "hash", "store_path", "timestamp"]

-- | Validate Outputs table structure
validateOutputsTable :: DBCore.Database -> IO ()
validateOutputsTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "Outputs" col
  where
    requiredColumns = ["derivation_id", "output_name", "path"]

-- | Validate References table structure
validateReferencesTable :: DBCore.Database -> IO ()
validateReferencesTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "References" col
  where
    requiredColumns = ["referrer", "reference"]

-- | Validate ValidPaths table structure
validateValidPathsTable :: DBCore.Database -> IO ()
validateValidPathsTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "ValidPaths" col
  where
    requiredColumns = ["path", "hash", "registration_time", "deriver", "is_valid"]

-- | Ensure a column exists in a table
ensureColumnExists :: DBCore.Database -> Text -> Text -> IO ()
ensureColumnExists db tableName columnName = do
    exists <- columnExists db tableName columnName
    unless exists $ throwIO $ SchemaMissingColumn tableName columnName

-- | Check if a table exists
tableExists :: DBCore.Database -> Text -> IO Bool
tableExists db tableName = do
    results <- query (DBCore.dbConn db) "SELECT count(*) FROM sqlite_master WHERE type='table' AND name=?" [tableName]
    case results of
        [(count :: Int)] -> return $ count > 0
        _ -> return False

-- | Check if a column exists in a table
columnExists :: DBCore.Database -> Text -> Text -> IO Bool
columnExists db tableName columnName = do
    -- First check table exists
    tableExists' <- tableExists db tableName
    if not tableExists'
        then return False
        else do
            -- Query table schema and look for column
            results <- try $ query (DBCore.dbConn db) ("PRAGMA table_info(" <> tableName <> ")") ()
                      :: IO (Either SomeException [(Int, Text, Text, Int, Maybe Text, Int)])
            case results of
                Left _ -> return False
                Right rows -> return $ any (\(_, name, _, _, _, _) -> name == columnName) rows

-- | Check if an index exists
indexExists :: DBCore.Database -> Text -> IO Bool
indexExists db indexName = do
    results <- query (DBCore.dbConn db) "SELECT count(*) FROM sqlite_master WHERE type='index' AND name=?" [indexName]
    case results of
        [(count :: Int)] -> return $ count > 0
        _ -> return False

-- | Get all schema elements (tables, indices, etc.)
getSchemaElements :: DBCore.Database -> IO [SchemaElement]
getSchemaElements db = do
    -- Get all tables
    tables <- query_ (DBCore.dbConn db) "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
        :: IO [[Text]]
    let tableElements = map (Table . head) tables

    -- Get all indices
    indices <- query_ (DBCore.dbConn db) "SELECT name FROM sqlite_master WHERE type='index' ORDER BY name"
        :: IO [[Text]]
    let indexElements = map (Index . head) indices

    -- Get all triggers
    triggers <- query_ (DBCore.dbConn db) "SELECT name FROM sqlite_master WHERE type='trigger' ORDER BY name"
        :: IO [[Text]]
    let triggerElements = map (Trigger . head) triggers

    -- Get columns for each table
    columnElements <- concat <$> mapM getTableColumns (map (\(Table t) -> t) tableElements)

    -- Combine all elements
    return $ tableElements ++ indexElements ++ triggerElements ++ columnElements
  where
    getTableColumns :: Text -> IO [SchemaElement]
    getTableColumns tableName = do
        columns <- try $ query (DBCore.dbConn db) ("PRAGMA table_info(" <> tableName <> ")") ()
                   :: IO (Either SomeException [(Int, Text, Text, Int, Maybe Text, Int)])
        case columns of
            Left _ -> return []
            Right cols -> return $ map (\(_, name, _, _, _, _) -> Column tableName name) cols

-- | Migrate the schema from one version to another
migrateSchema :: DBCore.Database -> SchemaVersion -> SchemaVersion -> IO ()
migrateSchema db fromVersion toVersion =
    if fromVersion >= toVersion
        then return () -- Nothing to do
        else do
            -- Find migrations we need to apply
            let requiredMigrations = filter (\m ->
                    migrationVersion m > fromVersion &&
                    migrationVersion m <= toVersion) migrations

            -- Apply each migration in a transaction
            forM_ requiredMigrations $ \migration ->
                DBCore.withTransaction db DBCore.Exclusive $ \_ -> do
                    -- Run the migration
                    catch
                        (migrationUp migration db)
                        (\(e :: SomeException) ->
                            throwIO $ SchemaMigrationFailed
                                (migrationVersion migration)
                                (T.pack $ "Migration failed: " ++ show e))

                    -- Update schema version
                    DBCore.updateSchemaVersion db (migrationVersion migration)

-- | Migration definitions
migrations :: [Migration]
migrations = [
    -- Version 1: Initial schema
    Migration {
        migrationVersion = 1,
        migrationDescription = "Initial schema creation",
        migrationUp = \db -> do
            createTables db
            createIndices db,
        migrationDown = \db -> do
            execute_ (DBCore.dbConn db) "DROP TABLE IF EXISTS Outputs;"
            execute_ (DBCore.dbConn db) "DROP TABLE IF EXISTS References;"
            execute_ (DBCore.dbConn db) "DROP TABLE IF EXISTS ValidPaths;"
            execute_ (DBCore.dbConn db) "DROP TABLE IF EXISTS Derivations;"
    }

    -- Add new migrations here as the schema evolves
    ]

-- | SQL for creating Derivations table
derivationsTableDef :: Query
derivationsTableDef = Query $
    "CREATE TABLE IF NOT EXISTS Derivations (\
    \id INTEGER PRIMARY KEY, \
    \hash TEXT NOT NULL UNIQUE, \
    \store_path TEXT NOT NULL UNIQUE, \
    \timestamp INTEGER NOT NULL DEFAULT (strftime('%s','now')));"

-- | SQL for creating Outputs table
outputsTableDef :: Query
outputsTableDef = Query $
    "CREATE TABLE IF NOT EXISTS Outputs (\
    \derivation_id INTEGER NOT NULL, \
    \output_name TEXT NOT NULL, \
    \path TEXT NOT NULL UNIQUE, \
    \PRIMARY KEY (derivation_id, output_name), \
    \FOREIGN KEY (derivation_id) REFERENCES Derivations(id));"

-- | SQL for creating References table
referencesTableDef :: Query
referencesTableDef = Query $
    "CREATE TABLE IF NOT EXISTS References (\
    \referrer TEXT NOT NULL, \
    \reference TEXT NOT NULL, \
    \PRIMARY KEY (referrer, reference));"

-- | SQL for creating ValidPaths table
validPathsTableDef :: Query
validPathsTableDef = Query $
    "CREATE TABLE IF NOT EXISTS ValidPaths (\
    \path TEXT PRIMARY KEY, \
    \hash TEXT NOT NULL, \
    \registration_time INTEGER NOT NULL DEFAULT (strftime('%s','now')), \
    \deriver TEXT, \
    \is_valid INTEGER NOT NULL DEFAULT 1);"
