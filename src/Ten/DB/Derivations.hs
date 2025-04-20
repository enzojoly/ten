{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ten.DB.Derivations (
    -- Store/retrieve derivations
    storeDerivation,
    retrieveDerivation,
    registerDerivationFile,
    storeDerivationInDB,

    -- Output mapping
    registerDerivationOutput,
    getOutputsForDerivation,
    getDerivationForOutput,
    findDerivationsByOutputs,

    -- Reference tracking
    addDerivationReference,
    getDerivationReferences,
    getReferrers,

    -- Query functions
    isDerivationRegistered,
    listRegisteredDerivations,
    getDerivationPath,
    retrieveDerivationByHash,

    -- Path validity
    registerValidPath,
    invalidatePath,
    isPathValid,

    -- Bulk operations
    registerDerivationWithOutputs,

    -- Types
    DerivationInfo(..),
    OutputInfo(..),
    DerivationReference(..)
) where

import Control.Exception (try, catch, throwIO, SomeException)
import Control.Monad (forM, forM_, when, unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
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
import System.Time.Extra (offsetTime, showDuration)

import Ten.DB.Core
import Ten.Core
import qualified Ten.Derivation as TenDeriv

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

-- | Reference between store paths
data DerivationReference = DerivationReference {
    refReferrer :: StorePath,  -- ^ Path that refers to another
    refReference :: StorePath  -- ^ Path being referred to
} deriving (Show, Eq)

-- | Custom RowParser for StorePath
parseStorePath :: SQLite.RowParser StorePath
parseStorePath = do
  path <- SQLite.field :: SQLite.RowParser Text
  case parseStorePathText path of
    Just sp -> return sp
    Nothing -> fail $ "Invalid store path format: " ++ T.unpack path

-- Make DerivationInfo an instance of FromRow
instance FromRow DerivationInfo where
    fromRow = DerivationInfo <$> SQLite.field <*> SQLite.field <*> parseStorePath <*> SQLite.field

-- Make OutputInfo an instance of FromRow
instance FromRow OutputInfo where
    fromRow = OutputInfo <$> SQLite.field <*> SQLite.field <*> parseStorePath

-- Make DerivationReference an instance of FromRow
instance FromRow DerivationReference where
    fromRow = DerivationReference <$> parseStorePath <*> parseStorePath

-- Helper function to parse StorePath from Text
parseStorePathText :: Text -> Maybe StorePath
parseStorePathText path = case T.breakOn "-" path of
    (hash, name) | not (T.null name) -> Just $ StorePath hash (T.drop 1 name)
    _ -> Nothing

-- Helper function to convert StorePath to Text
storePathToText :: StorePath -> Text
storePathToText (StorePath hash name) = hash <> "-" <> name

-- | Store a derivation in the database
storeDerivation :: Database -> TenDeriv.Derivation -> StorePath -> IO Int64
storeDerivation db derivation storePath = dbWithTransaction db ReadWrite $ \_ -> do
    -- Check if derivation already exists
    existing <- isDerivationRegistered db (TenDeriv.derivHash derivation)

    if existing
        then do
            -- Get the ID
            [Only derivId] <- dbQuery db
                "SELECT id FROM Derivations WHERE hash = ?"
                (Only (TenDeriv.derivHash derivation)) :: IO [Only Int64]
            return derivId
        else do
            -- Insert new derivation
            dbExecute db
                "INSERT INTO Derivations (hash, store_path, timestamp) VALUES (?, ?, strftime('%s','now'))"
                (TenDeriv.derivHash derivation, storePathToText storePath)

            -- Get the last inserted ID
            [Only derivId] <- dbQuery_ db "SELECT last_insert_rowid()" :: IO [Only Int64]

            -- Register the derivation path as valid
            registerValidPath db storePath (Just storePath)

            return derivId

-- | New function to handle database operations for storing a derivation
-- This replaces Ten.Derivation.storeDerivationFile's database operations
storeDerivationInDB :: TenDeriv.Derivation -> StorePath -> FilePath -> IO Int64
storeDerivationInDB drv derivPath dbPath = do
    -- Initialize database
    db <- initDatabase dbPath 5000

    -- Register the derivation file using the existing function
    derivId <- registerDerivationFile db drv derivPath

    -- Close the database
    closeDatabase db

    return derivId

-- | Register a derivation file in the database
registerDerivationFile :: Database -> TenDeriv.Derivation -> StorePath -> IO Int64
registerDerivationFile db derivation storePath = do
    -- Store in database
    derivId <- storeDerivation db derivation storePath

    -- Register outputs
    forM_ (Set.toList $ TenDeriv.derivOutputs derivation) $ \output -> do
        registerDerivationOutput db derivId (TenDeriv.outputName output) (TenDeriv.outputPath output)

    -- Register input references
    forM_ (Set.toList $ TenDeriv.derivInputs derivation) $ \input -> do
        addDerivationReference db storePath (TenDeriv.inputPath input)

    return derivId

-- | Retrieve a derivation from the database
retrieveDerivation :: Database -> Text -> IO (Maybe TenDeriv.Derivation)
retrieveDerivation db hash = do
    -- Query for derivation
    derivRows <- dbQuery db
        "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d WHERE d.hash = ?"
        (Only hash) :: IO [DerivationInfo]

    case derivRows of
        [] -> return Nothing
        [derivInfo] -> do
            -- Get the derivation store path
            let storePath = derivInfoStorePath derivInfo

            -- Read and deserialize the derivation file
            derivContent <- readDerivationFile storePath

            case derivContent of
                Left err -> do
                    -- Log error
                    putStrLn $ "Error reading derivation file: " ++ T.unpack err
                    return Nothing
                Right drv -> return $ Just drv
        _ -> error "Multiple derivations with same hash - database corruption"

-- | Retrieve a derivation by hash
retrieveDerivationByHash :: FilePath -> Text -> IO (Maybe TenDeriv.Derivation)
retrieveDerivationByHash dbPath hash = do
    -- Open database
    db <- initDatabase dbPath 5000

    -- Query for store path
    storePaths <- dbQuery db
        "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
        (Only hash) :: IO [Only Text]

    -- Get the derivation
    result <- case storePaths of
        [Only pathText] ->
            case parseStorePathText pathText of
                Just sp -> do
                    derivMaybe <- retrieveDerivationFromStore sp
                    return derivMaybe
                Nothing -> return Nothing
        _ -> return Nothing

    -- Close the database
    closeDatabase db

    return result
  where
    retrieveDerivationFromStore :: StorePath -> IO (Maybe TenDeriv.Derivation)
    retrieveDerivationFromStore sp = do
        -- Construct the full path
        let fullPath = "/var/lib/ten/store/" ++ T.unpack (storeHash sp) ++ "-" ++ T.unpack (storeName sp)

        -- Check if it exists
        exists <- doesFileExist fullPath
        if not exists
            then return Nothing
            else do
                -- Read and deserialize
                content <- BS.readFile fullPath
                case TenDeriv.deserializeDerivation content of
                    Left _ -> return Nothing
                    Right drv -> return $ Just drv

-- | Read and deserialize a derivation file
readDerivationFile :: StorePath -> IO (Either Text TenDeriv.Derivation)
readDerivationFile path = do
    -- Construct the full path
    let filePath = "/var/lib/ten/store/" ++ T.unpack (storeHash path) ++ "-" ++ T.unpack (storeName path)

    -- Check if file exists
    exists <- doesFileExist filePath
    if not exists
        then return $ Left $ "Derivation file does not exist: " <> T.pack filePath
        else do
            -- Read the file
            content <- BS.readFile filePath

            -- Deserialize
            case TenDeriv.deserializeDerivation content of
                Left err -> return $ Left $ "Failed to deserialize derivation: " <> err
                Right drv -> return $ Right drv

-- | Register an output for a derivation
registerDerivationOutput :: Database -> Int64 -> Text -> StorePath -> IO ()
registerDerivationOutput db derivId outputName outputPath = do
    -- Insert output record
    dbExecute db
        "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) VALUES (?, ?, ?)"
        (derivId, outputName, storePathToText outputPath)

    -- Register the output path as valid with its deriver
    registerValidPath db outputPath (Just $ StorePath (T.pack $ show derivId) "drv")

-- | Get all outputs for a derivation
getOutputsForDerivation :: Database -> Int64 -> IO [OutputInfo]
getOutputsForDerivation db derivId = do
    dbQuery db
        "SELECT derivation_id, output_name, path FROM Outputs WHERE derivation_id = ?"
        (Only derivId) :: IO [OutputInfo]

-- | Find the derivation that produced a particular output
getDerivationForOutput :: Database -> StorePath -> IO (Maybe DerivationInfo)
getDerivationForOutput db outputPath = do
    results <- dbQuery db
        "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
        \JOIN Outputs o ON d.id = o.derivation_id \
        \WHERE o.path = ?"
        (Only (storePathToText outputPath)) :: IO [DerivationInfo]

    return $ listToMaybe results

-- | Find derivations that produced the given outputs
findDerivationsByOutputs :: Database -> [StorePath] -> IO (Map String TenDeriv.Derivation)
findDerivationsByOutputs db [] = return Map.empty
findDerivationsByOutputs db outputPaths = do
    -- Convert StorePath to text for query
    let pathTexts = map storePathToText outputPaths

    -- First get the derivation info
    -- We use a more robust approach for handling the IN clause
    placeholders <- placeholdersForIn (length pathTexts)
    let queryStr = "SELECT DISTINCT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
                  \JOIN Outputs o ON d.id = o.derivation_id \
                  \WHERE o.path IN " ++ placeholders

    derivInfos <- dbQuery db (Query $ T.pack queryStr) pathTexts :: IO [DerivationInfo]

    -- Now load each derivation
    derivations <- forM derivInfos $ \derivInfo -> do
        mDrv <- retrieveDerivation db (derivInfoHash derivInfo)
        case mDrv of
            Nothing -> return Nothing
            Just drv -> return $ Just (T.unpack (derivInfoHash derivInfo), drv)

    -- Filter out Nothings and convert to Map
    return $ Map.fromList $ catMaybes derivations

-- Helper function to create placeholders for SQL IN clause
placeholdersForIn :: Int -> IO String
placeholdersForIn n = do
    let params = replicate n "?"
    return $ "(" ++ intercalate ", " params ++ ")"

-- Helper function for string intercalation
intercalate :: String -> [String] -> String
intercalate sep [] = ""
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Add a reference between two paths
addDerivationReference :: Database -> StorePath -> StorePath -> IO ()
addDerivationReference db referrer reference = do
    dbExecute db
        "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
        (storePathToText referrer, storePathToText reference)

-- | Get all references from a path
getDerivationReferences :: Database -> StorePath -> IO [StorePath]
getDerivationReferences db path = do
    results <- dbQuery db
        "SELECT reference FROM References WHERE referrer = ?"
        (Only (storePathToText path)) :: IO [Only Text]

    return $ map (\(Only t) ->
        fromMaybe (error $ "Invalid store path: " ++ T.unpack t) (parseStorePathText t)) results

-- | Get all paths that refer to a given path
getReferrers :: Database -> StorePath -> IO [StorePath]
getReferrers db path = do
    results <- dbQuery db
        "SELECT referrer FROM References WHERE reference = ?"
        (Only (storePathToText path)) :: IO [Only Text]

    return $ map (\(Only t) ->
        fromMaybe (error $ "Invalid store path: " ++ T.unpack t) (parseStorePathText t)) results

-- | Check if a derivation is already registered
isDerivationRegistered :: Database -> Text -> IO Bool
isDerivationRegistered db hash = do
    results <- dbQuery db
        "SELECT 1 FROM Derivations WHERE hash = ? LIMIT 1"
        (Only hash) :: IO [Only Int]
    return $ not (null results)

-- | List all registered derivations
listRegisteredDerivations :: Database -> IO [DerivationInfo]
listRegisteredDerivations db = do
    dbQuery_ db "SELECT id, hash, store_path, timestamp FROM Derivations ORDER BY timestamp DESC" :: IO [DerivationInfo]

-- | Get the store path for a derivation by hash
getDerivationPath :: Database -> Text -> IO (Maybe StorePath)
getDerivationPath db hash = do
    results <- dbQuery db
        "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
        (Only hash) :: IO [Only Text]

    case results of
        [Only path] -> return $ parseStorePathText path
        _ -> return Nothing

-- | Register a valid path in the store
registerValidPath :: Database -> StorePath -> Maybe StorePath -> IO ()
registerValidPath db path mDeriver = do
    let deriverText = case mDeriver of
            Just deriver -> Just (storePathToText deriver)
            Nothing -> Nothing

    dbExecute db
        "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time, deriver, is_valid) \
        \VALUES (?, ?, strftime('%s','now'), ?, 1)"
        (storePathToText path, storeHash path, deriverText)

-- | Mark a path as invalid
invalidatePath :: Database -> StorePath -> IO ()
invalidatePath db path = do
    dbExecute db
        "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
        (Only (storePathToText path))

-- | Check if a path is valid
isPathValid :: Database -> StorePath -> IO Bool
isPathValid db path = do
    results <- dbQuery db
        "SELECT is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
        (Only (storePathToText path)) :: IO [Only Int]

    case results of
        [Only valid] -> return (valid == 1)
        _ -> return False

-- | Register a derivation with all its outputs in a single transaction
registerDerivationWithOutputs :: Database -> TenDeriv.Derivation -> StorePath -> IO Int64
registerDerivationWithOutputs db derivation storePath = dbWithTransaction db ReadWrite $ \_ -> do
    -- Register the derivation
    derivId <- storeDerivation db derivation storePath

    -- Register all outputs
    forM_ (Set.toList $ TenDeriv.derivOutputs derivation) $ \output -> do
        registerDerivationOutput db derivId (TenDeriv.outputName output) (TenDeriv.outputPath output)

    -- Register all input references
    forM_ (Set.toList $ TenDeriv.derivInputs derivation) $ \input -> do
        addDerivationReference db storePath (TenDeriv.inputPath input)

    return derivId
