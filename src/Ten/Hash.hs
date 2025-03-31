{-# LANGUAGE OverloadedStrings #-}

module Ten.Hash (
    Hash,
    hashByteString,
    hashFile,
    hashStorePath,
    hashDerivation,
    hashText,
    showHash
) where

import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import System.IO (IOMode(..), withFile)

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
hashFile :: FilePath -> IO Hash
hashFile path = withFile path ReadMode $ \h -> do
    contents <- LBS.hGetContents h
    return $ hashLazyByteString contents

-- | Convert a hash to a text representation
showHash :: Hash -> Text
showHash = T.pack . show

-- | Hash a store path (forward declaration - implemented in Store module)
hashStorePath :: FilePath -> IO Hash
hashStorePath = hashFile

-- | Hash a derivation (forward declaration - implemented in Derivation module)
hashDerivation :: Text -> [Text] -> [(Text, Text)] -> Hash
hashDerivation name inputs env = 
    hashText $ T.intercalate ":" 
        [ name
        , T.intercalate "," inputs
        , T.intercalate ";" $ map (\(k, v) -> k <> "=" <> v) env
        ]
