{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}

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

import Control.Monad (forM_, when, unless, void)
import Control.Monad.Except (throwError, catchError)
import Data.List (sort, intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (Query(..))
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.Types
import Control.Exception (SomeException)

import qualified Ten.DB.Core as DBCore
import Ten.Core (TenM, BuildError(..), Phase(..), PrivilegeTier(..), logMsg)

-- | Schema version type
type SchemaVersion = Int

-- | Current schema version
currentSchemaVersion :: SchemaVersion
currentSchemaVersion = 1

-- | Migration definition
data Migration = Migration {
    migrationVersion :: SchemaVersion,
    migrationDescription :: Text,
    migrationUp :: DBCore.Database -> TenM 'Build 'Daemon (),
    migrationDown :: DBCore.Database -> TenM 'Build 'Daemon ()
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

-- | Convert SchemaError to BuildError for the TenM monad
schemaErrorToBuildError :: SchemaError -> BuildError
schemaErrorToBuildError (SchemaMissingTable tableName) =
    DBError $ "Missing table: " <> tableName
schemaErrorToBuildError (SchemaMissingColumn tableName colName) =
    DBError $ "Missing column " <> colName <> " in table " <> tableName
schemaErrorToBuildError (SchemaMissingIndex idxName) =
    DBError $ "Missing index: " <> idxName
schemaErrorToBuildError (SchemaInvalidVersion current required) =
    DBError $ "Schema version mismatch: current " <> T.pack (show current) <> ", required " <> T.pack (show required)
schemaErrorToBuildError (SchemaMigrationFailed version reason) =
    DBError $ "Migration to version " <> T.pack (show version) <> " failed: " <> reason
schemaErrorToBuildError (SchemaValidationFailed reason) =
    DBError $ "Schema validation failed: " <> reason

-- | Ensure the database schema is properly set up
ensureSchema :: DBCore.Database -> TenM 'Build 'Daemon ()
ensureSchema db = do
    -- Get current schema version
    currentVersion <- DBCore.getSchemaVersion db

    if currentVersion == 0
        then do
            -- New database, create schema from scratch
            logMsg 1 "Creating new database schema"
            createTables db
            createIndices db
            DBCore.updateSchemaVersion db currentSchemaVersion
        else if currentVersion < currentSchemaVersion
            then do
                -- Existing database needs migration
                logMsg 1 $ "Migrating database schema from version " <> T.pack (show currentVersion) <> " to " <> T.pack (show currentSchemaVersion)
                migrateSchema db currentVersion currentSchemaVersion
            else if currentVersion > currentSchemaVersion
                then do
                    -- Database is newer than our code!
                    throwError $ schemaErrorToBuildError $ SchemaInvalidVersion currentVersion currentSchemaVersion
                else do
                    -- Database is current, just validate
                    logMsg 2 "Database schema is current, validating"
                    validateSchema db

-- | Create all database tables
createTables :: DBCore.Database -> TenM 'Build 'Daemon ()
createTables db = DBCore.withTransaction db DBCore.Exclusive $ \_ -> do
    -- Create Derivations table
    DBCore.dbExecuteSimple_ db derivationsTableDef

    -- Create Outputs table
    DBCore.dbExecuteSimple_ db outputsTableDef

    -- Create References table
    DBCore.dbExecuteSimple_ db referencesTableDef

    -- Create ValidPaths table
    DBCore.dbExecuteSimple_ db validPathsTableDef

-- | Create all indices for performance
createIndices :: DBCore.Database -> TenM 'Build 'Daemon ()
createIndices db = DBCore.withTransaction db DBCore.Exclusive $ \_ -> do
    -- Derivations indices
    DBCore.dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_derivations_hash ON Derivations(hash);"

    -- Outputs indices
    DBCore.dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_outputs_path ON Outputs(path);"
    DBCore.dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_outputs_derivation ON Outputs(derivation_id);"

    -- References indices
    DBCore.dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_references_referrer ON References(referrer);"
    DBCore.dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_references_reference ON References(reference);"

    -- ValidPaths indices
    DBCore.dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_validpaths_hash ON ValidPaths(hash);"
    DBCore.dbExecuteSimple_ db "CREATE INDEX IF NOT EXISTS idx_validpaths_deriver ON ValidPaths(deriver);"

-- | Validate the schema is correct
validateSchema :: DBCore.Database -> TenM 'Build 'Daemon ()
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
        unless exists $
            throwError $ schemaErrorToBuildError $ SchemaMissingTable tableName

    -- Check key columns
    validateDerivationsTable db
    validateOutputsTable db
    validateReferencesTable db
    validateValidPathsTable db

-- | Validate Derivations table structure
validateDerivationsTable :: DBCore.Database -> TenM 'Build 'Daemon ()
validateDerivationsTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "Derivations" col
  where
    requiredColumns = ["id", "hash", "store_path", "timestamp"]

-- | Validate Outputs table structure
validateOutputsTable :: DBCore.Database -> TenM 'Build 'Daemon ()
validateOutputsTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "Outputs" col
  where
    requiredColumns = ["derivation_id", "output_name", "path"]

-- | Validate References table structure
validateReferencesTable :: DBCore.Database -> TenM 'Build 'Daemon ()
validateReferencesTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "References" col
  where
    requiredColumns = ["referrer", "reference"]

-- | Validate ValidPaths table structure
validateValidPathsTable :: DBCore.Database -> TenM 'Build 'Daemon ()
validateValidPathsTable db = do
    forM_ requiredColumns $ \col ->
        ensureColumnExists db "ValidPaths" col
  where
    requiredColumns = ["path", "hash", "registration_time", "deriver", "is_valid"]

-- | Ensure a column exists in a table
ensureColumnExists :: DBCore.Database -> Text -> Text -> TenM 'Build 'Daemon ()
ensureColumnExists db tableName columnName = do
    exists <- columnExists db tableName columnName
    unless exists $
        throwError $ schemaErrorToBuildError $ SchemaMissingColumn tableName columnName

-- | Check if a table exists
tableExists :: DBCore.Database -> Text -> TenM 'Build 'Daemon Bool
tableExists db tableName = do
    results <- DBCore.dbQuery db "SELECT count(*) FROM sqlite_master WHERE type='table' AND name=?" [tableName] :: TenM 'Build 'Daemon [DBCore.Only Int]
    case results of
        [DBCore.Only count] -> return $ count > 0
        _ -> return False

-- | Check if a column exists in a table
columnExists :: DBCore.Database -> Text -> Text -> TenM 'Build 'Daemon Bool
columnExists db tableName columnName = do
    -- First check table exists
    tableExists' <- tableExists db tableName
    if not tableExists'
        then return False
        else do
            -- Query table schema and look for column
            let pragmaQuery = Query $ "PRAGMA table_info(" <> tableName <> ")"
            catchError
                (do
                    rows <- DBCore.dbQuery_ db pragmaQuery :: TenM 'Build 'Daemon [(Int, Text, Text, Int, Maybe Text, Int)]
                    return $ any (\(_, name, _, _, _, _) -> name == columnName) rows)
                (\_ -> return False)

-- | Check if an index exists
indexExists :: DBCore.Database -> Text -> TenM 'Build 'Daemon Bool
indexExists db indexName = do
    results <- DBCore.dbQuery db "SELECT count(*) FROM sqlite_master WHERE type='index' AND name=?" [indexName] :: TenM 'Build 'Daemon [DBCore.Only Int]
    case results of
        [DBCore.Only count] -> return $ count > 0
        _ -> return False

-- | Get all schema elements (tables, indices, etc.)
getSchemaElements :: DBCore.Database -> TenM 'Build 'Daemon [SchemaElement]
getSchemaElements db = do
    -- Get all tables
    tables <- DBCore.dbQuery_ db "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name"
        :: TenM 'Build 'Daemon [[Text]]
    let tableElements = map (Table . head) tables

    -- Get all indices
    indices <- DBCore.dbQuery_ db "SELECT name FROM sqlite_master WHERE type='index' ORDER BY name"
        :: TenM 'Build 'Daemon [[Text]]
    let indexElements = map (Index . head) indices

    -- Get all triggers
    triggers <- DBCore.dbQuery_ db "SELECT name FROM sqlite_master WHERE type='trigger' ORDER BY name"
        :: TenM 'Build 'Daemon [[Text]]
    let triggerElements = map (Trigger . head) triggers

    -- Get columns for each table
    columnElements <- concat <$> mapM getTableColumns (map (\(Table t) -> t) tableElements)

    -- Combine all elements
    return $ tableElements ++ indexElements ++ triggerElements ++ columnElements
  where
    getTableColumns :: Text -> TenM 'Build 'Daemon [SchemaElement]
    getTableColumns tableName = do
        let pragmaQuery = Query $ "PRAGMA table_info(" <> tableName <> ")"
        catchError
            (do
                cols <- DBCore.dbQuery_ db pragmaQuery :: TenM 'Build 'Daemon [(Int, Text, Text, Int, Maybe Text, Int)]
                return $ map (\(_, name, _, _, _, _) -> Column tableName name) cols)
            (\_ -> return [])

-- | Migrate the schema from one version to another
migrateSchema :: DBCore.Database -> SchemaVersion -> SchemaVersion -> TenM 'Build 'Daemon ()
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
                    catchError
                        (migrationUp migration db)
                        (\err ->
                            throwError $ schemaErrorToBuildError $ SchemaMigrationFailed
                                (migrationVersion migration)
                                (T.pack $ "Migration failed: " ++ show err))

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
            DBCore.dbExecuteSimple_ db "DROP TABLE IF EXISTS Outputs;"
            DBCore.dbExecuteSimple_ db "DROP TABLE IF EXISTS References;"
            DBCore.dbExecuteSimple_ db "DROP TABLE IF EXISTS ValidPaths;"
            DBCore.dbExecuteSimple_ db "DROP TABLE IF EXISTS Derivations;"
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
