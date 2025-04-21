{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

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
    getTransitiveReferences,
    getTransitiveReferrers,

    -- Query functions
    isDerivationRegistered,
    listRegisteredDerivations,
    getDerivationPath,
    retrieveDerivationByHash,

    -- Path validity
    registerValidPath,
    invalidatePath,
    isPathValid,
    getPathInfo,

    -- Bulk operations
    registerDerivationWithOutputs,
    bulkRegisterReferences,

    -- Types
    DerivationInfo(..),
    OutputInfo(..),
    PathInfo(..)
) where

import Control.Exception (try, catch, throwIO, SomeException, finally)
import Control.Monad (forM, forM_, when, unless, void, foldM)
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
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Vector as Vector

import Ten.DB.Core
import Ten.Core

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

-- | Information about a store path
data PathInfo = PathInfo {
    pathInfoPath :: StorePath,          -- ^ The store path
    pathInfoHash :: Text,               -- ^ Hash part of path
    pathInfoDeriver :: Maybe StorePath, -- ^ Derivation that produced this path (if known)
    pathInfoRegistrationTime :: UTCTime, -- ^ When it was registered
    pathInfoIsValid :: Bool             -- ^ Whether path is still valid
} deriving (Show, Eq)

-- Make DerivationInfo an instance of FromRow
instance FromRow DerivationInfo where
    fromRow = DerivationInfo
        <$> field
        <*> field
        <*> parseStorePathField
        <*> field

-- Make OutputInfo an instance of FromRow
instance FromRow OutputInfo where
    fromRow = OutputInfo
        <$> field
        <*> field
        <*> parseStorePathField

-- Make StorePathReference an instance of FromRow
instance FromRow StorePathReference where
    fromRow = StorePathReference
        <$> parseStorePathField
        <*> parseStorePathField

-- Make PathInfo an instance of FromRow
instance FromRow PathInfo where
    fromRow = do
        path <- parseStorePathField
        hash <- field :: RowParser Text
        deriverText <- field :: RowParser (Maybe Text)
        timestamp <- field :: RowParser Int64
        isValid <- field :: RowParser Int

        -- Parse deriver if present
        let deriver = deriverText >>= parseStorePath

        -- Convert timestamp to UTCTime
        let regTime = posixSecondsToUTCTime (fromIntegral timestamp)

        return $ PathInfo path hash deriver regTime (isValid == 1)

-- Helper function to parse StorePath from a database field
parseStorePathField :: RowParser StorePath
parseStorePathField = do
    path <- field
    case parseStorePath path of
        Just sp -> return sp
        Nothing -> fail $ "Invalid store path format: " ++ T.unpack path

-- | Store a derivation in the database
storeDerivation :: Database -> Derivation -> StorePath -> IO Int64
storeDerivation db derivation storePath = dbWithTransaction db ReadWrite $ \txn -> do
    -- Check if derivation already exists
    existing <- isDerivationRegistered txn (derivHash derivation)

    if existing
        then do
            -- Get the ID
            [Only derivId] <- dbQuery txn
                "SELECT id FROM Derivations WHERE hash = ?"
                (Only (derivHash derivation)) :: IO [Only Int64]
            return derivId
        else do
            -- Insert new derivation
            dbExecute txn
                "INSERT INTO Derivations (hash, store_path, timestamp) VALUES (?, ?, strftime('%s','now'))"
                (derivHash derivation, storePathToText storePath)

            -- Get the last inserted ID
            [Only derivId] <- dbQuery_ txn "SELECT last_insert_rowid()" :: IO [Only Int64]

            -- Register the derivation path as valid
            registerValidPath txn storePath (Just storePath)

            return derivId

-- | New function to handle database operations for storing a derivation
-- This replaces Ten.Derivation.storeDerivationFile's database operations
storeDerivationInDB :: Derivation -> StorePath -> FilePath -> IO Int64
storeDerivationInDB drv derivPath dbPath = do
    -- Initialize database
    db <- initDatabase dbPath 5000

    -- Register the derivation file using the existing function
    derivId <- registerDerivationFile db drv derivPath

    -- Close the database
    closeDatabase db

    return derivId

-- | Register a derivation file in the database with proper reference tracking
registerDerivationFile :: Database -> Derivation -> StorePath -> IO Int64
registerDerivationFile db derivation storePath = dbWithTransaction db ReadWrite $ \txn -> do
    -- Store in database
    derivId <- storeDerivation txn derivation storePath

    -- Register outputs
    outputPaths <- forM (Set.toList $ derivOutputs derivation) $ \output -> do
        let outputPath = outputPath output
        registerDerivationOutput txn derivId (outputName output) outputPath

        -- Register this output path as valid
        registerValidPath txn outputPath (Just storePath)

        -- Return the output path for reference tracking
        return outputPath

    -- Register references from derivation to inputs
    forM_ (Set.toList $ derivInputs derivation) $ \input -> do
        addDerivationReference txn storePath (inputPath input)

    -- Register references from outputs to derivation (metadata)
    forM_ outputPaths $ \output -> do
        addDerivationReference txn output storePath

    -- Register references from outputs to inputs (direct dependencies)
    forM_ outputPaths $ \output -> do
        forM_ (Set.toList $ derivInputs derivation) $ \input -> do
            addDerivationReference txn output (inputPath input)

    return derivId

-- | Retrieve a derivation from the database
retrieveDerivation :: Database -> Text -> IO (Maybe Derivation)
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
        _ -> throwIO $ DBError "Multiple derivations with same hash - database corruption"

-- | Retrieve a derivation by hash
retrieveDerivationByHash :: FilePath -> Text -> IO (Maybe Derivation)
retrieveDerivationByHash dbPath hash = do
    -- Open database
    db <- initDatabase dbPath 5000

    -- Query for store path
    result <- try $ do
        storePaths <- dbQuery db
            "SELECT store_path FROM Derivations WHERE hash = ? LIMIT 1"
            (Only hash) :: IO [Only Text]

        -- Get the derivation
        case storePaths of
            [Only pathText] ->
                case parseStorePath pathText of
                    Just sp -> do
                        derivMaybe <- retrieveDerivationFromStore sp
                        return derivMaybe
                    Nothing -> return Nothing
            _ -> return Nothing

    -- Close the database
    closeDatabase db

    -- Handle any exceptions
    case result of
        Left (e :: SomeException) -> do
            putStrLn $ "Error retrieving derivation: " ++ show e
            return Nothing
        Right derivation -> return derivation
  where
    retrieveDerivationFromStore :: StorePath -> IO (Maybe Derivation)
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
                case deserializeDerivation content of
                    Left _ -> return Nothing
                    Right drv -> return $ Just drv

-- | Helper function to deserialize a derivation
deserializeDerivation :: ByteString -> Either Text Derivation
deserializeDerivation bs =
    case Aeson.eitherDecode (LBS.fromStrict bs) of
        Left err -> Left $ T.pack $ "JSON parse error: " ++ err
        Right json -> derivationFromJSON json

-- | Convert JSON to Derivation
derivationFromJSON :: Aeson.Value -> Either Text Derivation
derivationFromJSON value = case Aeson.parseEither parseDerivation value of
    Left err -> Left $ T.pack err
    Right drv -> Right drv
  where
    parseDerivation = Aeson.withObject "Derivation" $ \o -> do
        name <- o Aeson..: "name"
        hash <- o Aeson..: "hash"
        builderObj <- o Aeson..: "builder"
        builder <- parseStorePath' builderObj
        args <- o Aeson..: "args"
        inputsArray <- o Aeson..: "inputs"
        inputs <- parseInputs inputsArray
        outputsArray <- o Aeson..: "outputs"
        outputs <- parseOutputs outputsArray
        envObj <- o Aeson..: "env"
        env <- parseEnvMap envObj
        system <- o Aeson..: "system"
        strategyText <- o Aeson..: "strategy"
        let strategy = if strategyText == ("monadic" :: Text) then MonadicStrategy else ApplicativeStrategy
        metaObj <- o Aeson..: "meta"
        meta <- parseEnvMap metaObj

        return $ Derivation name hash builder args
                           (Set.fromList inputs) (Set.fromList outputs)
                           env system strategy meta

    parseStorePath' = Aeson.withObject "StorePath" $ \o -> do
        hash <- o Aeson..: "hash"
        name <- o Aeson..: "name"
        return $ StorePath hash name

    parseInputs = Aeson.withArray "Inputs" $ \arr ->
        mapM parseInput (Vector.toList arr)

    parseInput = Aeson.withObject "DerivationInput" $ \o -> do
        pathObj <- o Aeson..: "path"
        path <- parseStorePath' pathObj
        name <- o Aeson..: "name"
        return $ DerivationInput path name

    parseOutputs = Aeson.withArray "Outputs" $ \arr ->
        mapM parseOutput (Vector.toList arr)

    parseOutput = Aeson.withObject "DerivationOutput" $ \o -> do
        name <- o Aeson..: "name"
        pathObj <- o Aeson..: "path"
        path <- parseStorePath' pathObj
        return $ DerivationOutput name path

    parseEnvMap :: Aeson.Object -> Aeson.Parser (Map Text Text)
    parseEnvMap o =
        return $ Map.fromList [(Key.toText k, v) | (k, Aeson.String v) <- KeyMap.toList o]

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
            deserializeDerivation content

-- | Register an output for a derivation
registerDerivationOutput :: Database -> Int64 -> Text -> StorePath -> IO ()
registerDerivationOutput db derivId outputName outputPath = do
    -- Insert output record
    void $ dbExecute db
        "INSERT OR REPLACE INTO Outputs (derivation_id, output_name, path) VALUES (?, ?, ?)"
        (derivId, outputName, storePathToText outputPath)

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
findDerivationsByOutputs :: Database -> [StorePath] -> IO (Map String Derivation)
findDerivationsByOutputs db [] = return Map.empty
findDerivationsByOutputs db outputPaths = do
    -- Convert StorePath to text for query
    let pathTexts = map storePathToText outputPaths

    -- Use a better approach for handling the IN clause
    let placeholders = "(" ++ intercalate ", " (replicate (length pathTexts) "?") ++ ")"
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

-- Helper function for string intercalation
intercalate :: String -> [String] -> String
intercalate sep [] = ""
intercalate sep [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Add a reference between two paths
addDerivationReference :: Database -> StorePath -> StorePath -> IO ()
addDerivationReference db referrer reference = do
    when (referrer /= reference) $ do
        -- Skip self-references
        void $ dbExecute db
            "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
            (storePathToText referrer, storePathToText reference)

-- | Bulk register multiple references for efficiency
bulkRegisterReferences :: Database -> [(StorePath, StorePath)] -> IO Int
bulkRegisterReferences db [] = return 0
bulkRegisterReferences db references = dbWithTransaction db ReadWrite $ \txn -> do
    -- Filter out self-references
    let validRefs = filter (\(from, to) -> from /= to) references

    -- Insert each reference
    count <- foldM (\acc (from, to) -> do
        void $ dbExecute txn
            "INSERT OR IGNORE INTO References (referrer, reference) VALUES (?, ?)"
            (storePathToText from, storePathToText to)
        return $! acc + 1) 0 validRefs

    return count

-- | Get all direct references from a path
getDerivationReferences :: Database -> StorePath -> IO [StorePath]
getDerivationReferences db path = do
    results <- dbQuery db
        "SELECT reference FROM References WHERE referrer = ?"
        (Only (storePathToText path)) :: IO [Only Text]

    -- Parse each path
    return $ catMaybes $ map (\(Only text) -> parseStorePath text) results

-- | Get all direct referrers to a path
getReferrers :: Database -> StorePath -> IO [StorePath]
getReferrers db path = do
    results <- dbQuery db
        "SELECT referrer FROM References WHERE reference = ?"
        (Only (storePathToText path)) :: IO [Only Text]

    -- Parse each path
    return $ catMaybes $ map (\(Only text) -> parseStorePath text) results

-- | Get all transitive references (closure)
getTransitiveReferences :: Database -> StorePath -> IO (Set StorePath)
getTransitiveReferences db path = do
    results <- dbQuery db
        "WITH RECURSIVE\n\
        \  closure(p) AS (\n\
        \    VALUES(?)\n\
        \    UNION\n\
        \    SELECT reference FROM References JOIN closure ON referrer = p\n\
        \  )\n\
        \SELECT p FROM closure WHERE p != ?"
        (storePathToText path, storePathToText path) :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only text) -> parseStorePath text) results

-- | Get all transitive referrers (reverse closure)
getTransitiveReferrers :: Database -> StorePath -> IO (Set StorePath)
getTransitiveReferrers db path = do
    results <- dbQuery db
        "WITH RECURSIVE\n\
        \  closure(p) AS (\n\
        \    VALUES(?)\n\
        \    UNION\n\
        \    SELECT referrer FROM References JOIN closure ON reference = p\n\
        \  )\n\
        \SELECT p FROM closure WHERE p != ?"
        (storePathToText path, storePathToText path) :: IO [Only Text]

    -- Parse paths and return as set
    return $ Set.fromList $ catMaybes $ map (\(Only text) -> parseStorePath text) results

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
        [Only path] -> return $ parseStorePath path
        _ -> return Nothing

-- | Register a valid path in the store
registerValidPath :: Database -> StorePath -> Maybe StorePath -> IO ()
registerValidPath db path mDeriver = do
    let deriverText = case mDeriver of
            Just deriver -> Just (storePathToText deriver)
            Nothing -> Nothing

    void $ dbExecute db
        "INSERT OR REPLACE INTO ValidPaths (path, hash, registration_time, deriver, is_valid) \
        \VALUES (?, ?, strftime('%s','now'), ?, 1)"
        (storePathToText path, storeHash path, deriverText)

-- | Mark a path as invalid
invalidatePath :: Database -> StorePath -> IO ()
invalidatePath db path = do
    void $ dbExecute db
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

-- | Get detailed information about a path
getPathInfo :: Database -> StorePath -> IO (Maybe PathInfo)
getPathInfo db path = do
    results <- dbQuery db
        "SELECT path, hash, deriver, registration_time, is_valid FROM ValidPaths WHERE path = ? LIMIT 1"
        (Only (storePathToText path)) :: IO [PathInfo]

    return $ listToMaybe results

-- | Register a derivation with all its outputs in a single transaction
registerDerivationWithOutputs :: Database -> Derivation -> StorePath -> IO Int64
registerDerivationWithOutputs db derivation storePath = dbWithTransaction db ReadWrite $ \txn -> do
    -- Register the derivation
    derivId <- storeDerivation txn derivation storePath

    -- Get all outputs and inputs for reference tracking
    let outputs = Set.map outputPath $ derivOutputs derivation
    let inputs = Set.map inputPath $ derivInputs derivation

    -- Register all outputs
    forM_ (Set.toList $ derivOutputs derivation) $ \output -> do
        let outputPath = outputPath output
        registerDerivationOutput txn derivId (outputName output) outputPath

        -- Register this as a valid path
        registerValidPath txn outputPath (Just storePath)

    -- Register all input references
    forM_ (Set.toList $ derivInputs derivation) $ \input -> do
        addDerivationReference txn storePath (inputPath input)

    -- Register references from outputs to inputs (direct dependencies)
    forM_ (Set.toList outputs) $ \output -> do
        -- Reference from output to derivation
        addDerivationReference txn output storePath

        -- References from output to each input
        forM_ (Set.toList inputs) $ \input -> do
            addDerivationReference txn output input

    return derivId
