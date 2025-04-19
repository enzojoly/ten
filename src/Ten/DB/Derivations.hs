{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Ten.DB.Derivations (
    -- Store/retrieve derivations
    storeDerivation,
    retrieveDerivation,
    registerDerivationFile,

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
import Ten.Derivation
import Ten.Hash
import Ten.Store (storePathToFilePath)

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

-- Make DerivationInfo an instance of FromRow
instance FromRow DerivationInfo where
    fromRow = DerivationInfo <$> field <*> field <*> parseStorePath <*> field
      where
        parseStorePath :: RowParser StorePath
        parseStorePath = do
          path <- field :: RowParser Text
          case parseStorePathText path of
            Just sp -> return sp
            Nothing -> SQLite.returnError SQLite.ConversionFailed
                       "StorePath" "Invalid store path format"

-- Make OutputInfo an instance of FromRow
instance FromRow OutputInfo where
    fromRow = OutputInfo <$> field <*> field <*> parseStorePath
      where
        parseStorePath :: RowParser StorePath
        parseStorePath = do
          path <- field :: RowParser Text
          case parseStorePathText path of
            Just sp -> return sp
            Nothing -> SQLite.returnError SQLite.ConversionFailed
                       "StorePath" "Invalid store path format"

-- Make DerivationReference an instance of FromRow
instance FromRow DerivationReference where
    fromRow = DerivationReference <$> parseStorePath <*> parseStorePath
      where
        parseStorePath :: RowParser StorePath
        parseStorePath = do
          path <- field :: RowParser Text
          case parseStorePathText path of
            Just sp -> return sp
            Nothing -> SQLite.returnError SQLite.ConversionFailed
                       "StorePath" "Invalid store path format"

-- | Parse a store path from text
parseStorePathText :: Text -> Maybe StorePath
parseStorePathText path =
    case T.breakOn "-" path of
        (hash, name) | not (T.null name) ->
            Just $ StorePath hash (T.drop 1 name)
        _ -> Nothing

-- | Convert a store path to text
storePathToText :: StorePath -> Text
storePathToText (StorePath hash name) = hash <> "-" <> name

-- | Store a derivation in the database
storeDerivation :: Database -> Derivation -> StorePath -> IO Int64
storeDerivation db derivation storePath = withTransaction db ReadWrite $ \_ -> do
    -- Check if derivation already exists
    existing <- isDerivationRegistered db (derivHash derivation)

    if existing
        then do
            -- Get the ID
            [Only derivId] <- query db
                "SELECT id FROM Derivations WHERE hash = ?"
                (Only (derivHash derivation))
            return derivId
        else do
            -- Insert new derivation
            execute db
                "INSERT INTO Derivations (hash, store_path, timestamp) VALUES (?, ?, strftime('%s','now'))"
                (derivHash derivation, storePathToText storePath)

            -- Get the last inserted ID
            [Only derivId] <- query_ db "SELECT last_insert_rowid()"

            -- Register the derivation path as valid
            registerValidPath db storePath (Just storePath)

            return derivId

-- | Register a derivation file in the database
registerDerivationFile :: Database -> Derivation -> StorePath -> IO Int64
registerDerivationFile db derivation storePath = do
    -- Store in database
    derivId <- storeDerivation db derivation storePath

    -- Register outputs
    forM_ (Set.toList $ derivOutputs derivation) $ \output -> do
        registerDerivationOutput db derivId (outputName output) (outputPath output)

    -- Register input references
    forM_ (Set.toList $ derivInputs derivation) $ \input -> do
        addDerivationReference db storePath (inputPath input)

    return derivId

-- | Retrieve a derivation from the database
retrieveDerivation :: Database -> Text -> IO (Maybe Derivation)
retrieveDerivation db hash = do
    -- Query for derivation
    derivRows <- query db
        "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d WHERE d.hash = ?"
        (Only hash)

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

-- | Read and deserialize a derivation file
readDerivationFile :: StorePath -> IO (Either Text Derivation)
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
            case deserializeDerivation content of
                Left err -> return $ Left $ "Failed to deserialize derivation: " <> err
                Right drv -> return $ Right drv

-- | Register an output for a derivation
registerDerivationOutput :: Database -> Int64 -> Text -> StorePath -> IO ()
registerDerivationOutput db derivId outputName outputPath = do
    -- Insert output record
    execute db
        "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) VALUES (?, ?, ?)"
        (derivId, outputName, storePathToText outputPath)

    -- Register the output path as valid with its deriver
    registerValidPath db outputPath (Just $ StorePath (T.pack $ show derivId) "drv")

-- | Get all outputs for a derivation
getOutputsForDerivation :: Database -> Int64 -> IO [OutputInfo]
getOutputsForDerivation db derivId = do
    query db
        "SELECT derivation_id, output_name, path FROM Outputs WHERE derivation_id = ?"
        (Only derivId)

-- | Find the derivation that produced a particular output
getDerivationForOutput :: Database -> StorePath -> IO (Maybe DerivationInfo)
getDerivationForOutput db outputPath = do
    results <- query db
        "SELECT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
        \JOIN Outputs o ON d.id = o.derivation_id \
        \WHERE o.path = ?"
        (Only (storePathToText outputPath))

    return $ listToMaybe results

-- | Find derivations that produced the given outputs
findDerivationsByOutputs :: Database -> [StorePath] -> IO (Map String Derivation)
findDerivationsByOutputs db [] = return Map.empty
findDerivationsByOutputs db outputPaths = do
    -- Convert StorePath to text for query
    let pathTexts = map storePathToText outputPaths

    -- First get the derivation info
    derivInfos <- query db
        "SELECT DISTINCT d.id, d.hash, d.store_path, d.timestamp FROM Derivations d \
        \JOIN Outputs o ON d.id = o.derivation_id \
        \WHERE o.path IN (?);"
        (SQLite.In pathTexts)

    -- Now load each derivation
    derivations <- forM derivInfos $ \derivInfo -> do
        mDrv <- retrieveDerivation db (derivInfoHash derivInfo)
        case mDrv of
            Nothing -> return Nothing
            Just drv -> return $ Just (T.unpack (derivInfoHash derivInfo), drv)

    -- Filter out Nothings and convert to Map
    return $ Map.fromList $ catMaybes derivations

-- | Add a reference between two paths
addDerivationReference :: Database -> StorePath -> StorePath -> IO ()
addDerivationReference db referrer reference = do
    execute db
        "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
        (storePathToText referrer, storePathToText reference)

-- | Get all references from a path
getDerivationReferences :: Database -> StorePath -> IO [StorePath]
getDerivationReferences db path = do
    results <- query db
        "SELECT reference FROM References WHERE referrer = ?"
        (Only (storePathToText path))

    return $ map (\(Only t) ->
        fromMaybe (error $ "Invalid store path: " ++ T.unpack t) (parseStorePathText t)) results

-- | Get all paths that refer to a given path
getReferrers :: Database -> StorePath -> IO [StorePath]
getReferrers db path = do
    results <- query db
        "SELECT referrer FROM References WHERE reference = ?"
        (Only (storePathToText path))

    return $ map (\(Only t) ->
        fromMaybe (error $ "Invalid store path: " ++ T.unpack t) (parseStorePathText t)) results

-- | Check if a derivation is already registered
isDerivationRegistered :: Database -> Text -> IO Bool
isDerivationRegistered db hash = do
    results <- query db
        "SELECT 1 FROM Derivations WHERE hash = ? LIMIT 1"
        (Only hash)
    return $ not (null results)

-- | List all registered derivations
listRegisteredDerivations :: Database -> IO [DerivationInfo]
listRegisteredDerivations db = do
    query_ db "SELECT id, hash, store_path, timestamp FROM Derivations ORDER BY timestamp DESC"

-- | Get the store path for a derivation by hash
getDerivationPath :: Database -> Text -> IO (Maybe StorePath)
getDerivationPath db hash = do
    results <- query db
        "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
        (Only hash)

    case results of
        [Only path] -> return $ parseStorePathText path
        _ -> return Nothing

-- | Register a valid path in the store
registerValidPath :: Database -> StorePath -> Maybe StorePath -> IO ()
registerValidPath db path mDeriver = do
    let deriverText = case mDeriver of
            Just deriver -> Just (storePathToText deriver)
            Nothing -> Nothing

    execute db
        "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time, deriver, is_valid) \
        \VALUES (?, ?, strftime('%s','now'), ?, 1)"
        (storePathToText path, storeHash path, deriverText)

-- | Mark a path as invalid
invalidatePath :: Database -> StorePath -> IO ()
invalidatePath db path = do
    execute db
        "UPDATE ValidPaths SET is_valid = 0 WHERE path = ?"
        (Only (storePathToText path))

-- | Check if a path is valid
isPathValid :: Database -> StorePath -> IO Bool
isPathValid db path = do
    results <- query db
        "SELECT is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
        (Only (storePathToText path))

    case results of
        [Only valid] -> return (valid == (1 :: Int))
        _ -> return False

-- | Register a derivation with all its outputs in a single transaction
registerDerivationWithOutputs :: Database -> Derivation -> StorePath -> IO Int64
registerDerivationWithOutputs db derivation storePath = withTransaction db ReadWrite $ \_ -> do
    -- Register the derivation
    derivId <- storeDerivation db derivation storePath

    -- Register all outputs
    forM_ (Set.toList $ derivOutputs derivation) $ \output -> do
        registerDerivationOutput db derivId (outputName output) (outputPath output)

    -- Register all input references
    forM_ (Set.toList $ derivInputs derivation) $ \input -> do
        addDerivationReference db storePath (inputPath input)

    return derivId
