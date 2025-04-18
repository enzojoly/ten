{-# LANGUAGE OverloadedStrings #-}

module Ten.Hash (
    -- Main Hash types and operations
    Hash,
    hashByteString,
    hashLazyByteString,
    hashText,
    hashFile,
    hashStorePath,

    -- Hash verification
    verifyHash,

    -- Conversion functions
    showHash,
    hashFromText,

    -- Higher-level hashing functions
    hashDerivation,
    hashEnvironment,

    -- Utilities
    hashFilePath,
    isValidHash
) where

import qualified Crypto.Hash as Crypto
import qualified Crypto.Hash.Algorithms as Crypto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.List (intercalate)
import System.IO (IOMode(..), withFile)
import System.FilePath (takeFileName)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (liftIO)

-- | Hash type - SHA256 for content addressing
type Hash = Crypto.Digest Crypto.SHA256

-- | Hash a strict ByteString
hashByteString :: BS.ByteString -> Hash
hashByteString = Crypto.hash

-- | Hash a lazy ByteString
hashLazyByteString :: LBS.ByteString -> Hash
hashLazyByteString = Crypto.hashlazy

-- | Hash a Text value
hashText :: Text -> Hash
hashText = hashByteString . TE.encodeUtf8

-- | Hash a file's contents
hashFile :: FilePath -> IO (Either String Hash)
hashFile path = do
    result <- try $ withFile path ReadMode $ \h -> do
        contents <- LBS.hGetContents h
        let hash = hashLazyByteString contents
        -- Force evaluation to make sure file is read
        return $! hash
    case result of
        Left (e :: SomeException) ->
            return $ Left $ "Failed to hash file " ++ path ++ ": " ++ show e
        Right hash ->
            return $ Right hash

-- | Hash a store path (combines hash and name)
hashStorePath :: Text -> Text -> Hash
hashStorePath hash name = hashText (hash <> "-" <> name)

-- | Verify that a ByteString hashes to the expected hash
verifyHash :: BS.ByteString -> Hash -> Bool
verifyHash content expectedHash = hashByteString content == expectedHash

-- | Convert a hash to a text representation
showHash :: Hash -> Text
showHash = T.pack . show

-- | Parse a hash from its text representation
hashFromText :: Text -> Maybe Hash
hashFromText text =
    case Crypto.digestFromByteString (TE.encodeUtf8 text) of
        Just digest -> Just digest
        Nothing -> Nothing

-- | Check if a text is a valid hash representation
isValidHash :: Text -> Bool
isValidHash = isJust . hashFromText

-- | Hash a derivation's basic attributes
hashDerivation :: Text          -- ^ Derivation name
               -> [Text]        -- ^ Arguments
               -> Map Text Text -- ^ Environment variables
               -> Hash
hashDerivation name args env =
    hashText $ T.intercalate ":"
        [ name
        , T.intercalate "," args
        , T.intercalate ";" $ map (\(k, v) -> k <> "=" <> v) $ Map.toList env
        ]

-- | Hash an environment mapping
hashEnvironment :: Map Text Text -> Hash
hashEnvironment env =
    hashText $ T.intercalate ";" $ map (\(k, v) -> k <> "=" <> v) $ Map.toList env

-- | Hash a file path (used for debugging, not for content addressing)
hashFilePath :: FilePath -> Hash
hashFilePath = hashText . T.pack . takeFileName

-- | Higher-level function to hash a file and handle errors in IO monad
hashFileIO :: FilePath -> IO Hash
hashFileIO path = do
    result <- hashFile path
    case result of
        Left err -> error err  -- In a real app, this would use proper error handling
        Right hash -> return hash
