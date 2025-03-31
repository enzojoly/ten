{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ten.Store (
    StorePath(..),
    storePathToFilePath,
    addToStore,
    ensureInStore,
    readFromStore,
    verifyStorePath,
    hashStorePath,
    storePathExists
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad.State (modify)
import Control.Exception (try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import System.IO.Error

import Ten.Core
import qualified Ten.Hash as Hash  -- Use qualified import

-- | Add content to the store, returning its StorePath
addToStore :: Text -> ByteString -> TenM 'Build StorePath
addToStore name content = do
    env <- ask

    -- Generate the store path
    let hash = Hash.showHash $ Hash.hashByteString content  -- Use qualified names
    let path = StorePath hash name
    let fullPath = storePathToFilePath path env

    -- Check if it already exists
    exists <- liftIO $ doesFileExist fullPath

    -- Only write if it doesn't exist (store is immutable)
    unless exists $ do
        logMsg 1 $ "Adding to store: " <> hash <> "-" <> name

        -- Create store directory if needed
        liftIO $ createDirectoryIfMissing True (storePath env)

        -- Write the content
        result <- liftIO $ try $ BS.writeFile fullPath content
        case result of
            Left err -> throwError $ StoreError $ "Failed to write to store: " <> T.pack (show err)
            Right () -> return ()

    -- Add proof that this path is now in the store
    addProof $ BuildProof

    -- Record this as an output
    modify $ \s -> s { buildOutputs = Set.insert path (buildOutputs s) }

    return path
  where
    try :: IO a -> IO (Either IOError a)
    try = Control.Exception.try

-- | Ensure a file is in the store, adding it if necessary
ensureInStore :: Text -> FilePath -> TenM 'Build StorePath
ensureInStore name sourceFile = do
    env <- ask

    -- Check if source file exists
    exists <- liftIO $ doesFileExist sourceFile
    unless exists $ throwError $ InputNotFound sourceFile

    -- Read the content
    content <- liftIO $ BS.readFile sourceFile

    -- Add to store
    addToStore name content

-- | Read content from a store path
readFromStore :: StorePath -> TenM p ByteString
readFromStore path = do
    env <- ask
    let fullPath = storePathToFilePath path env

    -- Check if it exists
    exists <- liftIO $ doesFileExist fullPath
    unless exists $ throwError $ StoreError $ "Path not in store: " <> storeHash path

    -- Read the content
    liftIO $ BS.readFile fullPath

-- | Verify a store path matches its hash
verifyStorePath :: StorePath -> TenM p Bool
verifyStorePath path = do
    env <- ask
    let fullPath = storePathToFilePath path env

    -- Check file exists
    exists <- liftIO $ doesFileExist fullPath
    if not exists
        then return False
        else do
            -- Read and hash content
            content <- liftIO $ BS.readFile fullPath
            let actualHash = Hash.showHash $ Hash.hashByteString content  -- Use qualified names

            -- Compare with expected hash
            return $ actualHash == storeHash path

-- | Check if a store path exists
storePathExists :: StorePath -> TenM p Bool
storePathExists path = do
    env <- ask
    liftIO $ doesFileExist $ storePathToFilePath path env

-- | Reimplementation of hashStorePath now that we have the full context
hashStorePath :: StorePath -> TenM p Hash.Hash  -- Update the return type to use the qualified name
hashStorePath path = do
    content <- readFromStore path
    return $ Hash.hashByteString content  -- Use qualified name
