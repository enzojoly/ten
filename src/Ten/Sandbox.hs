{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ten.Sandbox (
    withSandbox,
    SandboxConfig(..),
    defaultSandboxConfig
) where

import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import qualified Control.Exception as E
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO.Temp (withSystemTempDirectory)

import Ten.Core
import Ten.Store

-- | Configuration for a build sandbox
data SandboxConfig = SandboxConfig
    { sandboxAllowNetwork :: Bool          -- Allow network access
    , sandboxExtraPaths :: Set FilePath    -- Additional paths to make available
    , sandboxEnv :: Map Text Text          -- Environment variables
    , sandboxReadOnlyBindMounts :: Map FilePath FilePath -- Read-only bind mounts
    } deriving (Show, Eq)

-- | Default sandbox configuration (restrictive)
defaultSandboxConfig :: SandboxConfig
defaultSandboxConfig = SandboxConfig
    { sandboxAllowNetwork = False
    , sandboxExtraPaths = Set.empty
    , sandboxEnv = Map.empty
    , sandboxReadOnlyBindMounts = Map.empty
    }

-- | Run an action within a build sandbox
withSandbox :: Set StorePath -> SandboxConfig -> (FilePath -> TenM 'Build a) -> TenM 'Build a
withSandbox inputs config action = do
    env <- ask

    -- Only buildable in the Build phase
    state <- get
    when (currentPhase state /= Build) $
        throwError $ BuildFailed "Sandbox can only be created in Build phase"

    -- Create a temporary sandbox directory
    let sandboxFunc sandboxDir = do
            -- Create the basic directory structure with proper permissions
            liftIO $ createAndSetupDir sandboxDir
            liftIO $ createAndSetupDir (sandboxDir </> "tmp")
            liftIO $ createAndSetupDir (sandboxDir </> "build")
            liftIO $ createAndSetupDir (sandboxDir </> "out")
            liftIO $ createAndSetupDir (sandboxDir </> "store")

            -- Map all inputs into the sandbox
            mapM_ (setupInput sandboxDir) (Set.toList inputs)

            -- Add read-only bind mounts from config
            mapM_ (setupBindMount sandboxDir) (Map.toList $ sandboxReadOnlyBindMounts config)

            -- Set up system paths for gcc if needed
            mapM_ (setupSystemPath sandboxDir) (Set.toList $ sandboxExtraPaths config)

            -- Log sandbox creation
            logMsg 1 $ "Created sandbox at: " <> T.pack sandboxDir

            -- Add sandbox proof
            addProof $ BuildProof

            -- Run the action inside the sandbox
            let buildDir = sandboxDir
            result <- action buildDir

            -- Log sandbox cleanup
            logMsg 1 $ "Cleaning up sandbox: " <> T.pack sandboxDir

            return result

    -- Create the temporary directory and ensure cleanup
    liftIO (withSystemTempDirectory "ten-sandbox" (runSandboxed env state sandboxFunc))
        >>= either throwError return

-- | Helper to create a directory and set proper permissions
createAndSetupDir :: FilePath -> IO ()
createAndSetupDir dir = do
    createDirectoryIfMissing True dir
    perms <- getPermissions dir
    setPermissions dir (setOwnerExecutable True $
                       setOwnerWritable True $
                       setOwnerReadable True $ perms)

-- | Helper to run a sandbox action with the Ten monad context
runSandboxed :: BuildEnv -> BuildState -> (FilePath -> TenM 'Build a) -> FilePath -> IO (Either BuildError a)
runSandboxed env state action sandboxDir = do
    result <- runTen (action sandboxDir) env state
    return $ case result of
        Left err -> Left err
        Right (val, _) -> Right val

-- | Set up an input in the sandbox
setupInput :: FilePath -> StorePath -> TenM 'Build ()
setupInput sandboxDir input = do
    env <- ask

    -- Determine source and destination paths
    let sourcePath = storePathToFilePath input env

    -- Create two links:
    -- 1. One with the full store path format
    let storeDest = sandboxDir </> "store" </> T.unpack (storeHash input) ++ "-" ++ T.unpack (storeName input)
    -- 2. One with just the name (what the builder expects to find)
    let nameDest = sandboxDir </> T.unpack (storeName input)

    -- Create the files with appropriate permissions
    result1 <- liftIO $ E.try $ createFileLink sourcePath storeDest
    case result1 of
        Left (_ :: E.SomeException) -> do
            -- Fallback to copying if linking fails
            liftIO $ copyFile sourcePath storeDest
            perms <- liftIO $ getPermissions storeDest
            liftIO $ setPermissions storeDest (setOwnerReadable True $ perms)
            logMsg 2 $ "Copied input to sandbox store: " <> T.pack storeDest
        Right () ->
            logMsg 2 $ "Linked input to sandbox store: " <> T.pack storeDest

    -- Then create the simple name link (THIS IS THE KEY PART)
    result2 <- liftIO $ E.try $ createFileLink sourcePath nameDest
    case result2 of
        Left (_ :: E.SomeException) -> do
            -- Fallback to copying if linking fails
            liftIO $ copyFile sourcePath nameDest
            perms <- liftIO $ getPermissions nameDest
            liftIO $ setPermissions nameDest (setOwnerReadable True $ perms)
            logMsg 2 $ "Copied input to sandbox: " <> T.pack nameDest
        Right () ->
            logMsg 2 $ "Linked input to sandbox: " <> T.pack nameDest

-- | Set up a bind mount in the sandbox
setupBindMount :: FilePath -> (FilePath, FilePath) -> TenM 'Build ()
setupBindMount sandboxDir (source, dest) = do
    let fullDest = sandboxDir </> dest

    -- Create parent directory with appropriate permissions
    liftIO $ createAndSetupDir (takeDirectory fullDest)

    -- In a real implementation, this would use mount --bind
    -- For now, we simulate with symlinks for portability
    liftIO $ createFileLink source fullDest
    logMsg 2 $ "Bind mounted (simulated): " <> T.pack source <> " -> " <> T.pack fullDest

-- | Helper to make system paths available in the sandbox
setupSystemPath :: FilePath -> FilePath -> TenM 'Build ()
setupSystemPath sandboxDir systemPath = do
    -- Skip if path doesn't exist
    pathExists <- liftIO $ doesPathExist systemPath
    unless pathExists $ return ()

    -- Create parent dirs if needed
    let sandboxPath = sandboxDir </> makeRelative "/" systemPath
    liftIO $ createAndSetupDir (takeDirectory sandboxPath)

    -- Skip if it's already a link or file
    destExists <- liftIO $ doesPathExist sandboxPath
    when destExists $ return ()

    -- Check if the source is a directory
    isDir <- liftIO $ doesDirectoryExist systemPath

    -- Link the system path into the sandbox
    if isDir
        then do
            -- For directories, create the directory and set permissions
            liftIO $ createAndSetupDir sandboxPath
            logMsg 2 $ "Created directory in sandbox: " <> T.pack sandboxPath
        else do
            -- For files, create a link and ensure it's accessible
            result <- liftIO $ E.try $ createFileLink systemPath sandboxPath
            case result of
                Left (_ :: E.SomeException) -> do
                    -- Fallback to copying for files
                    liftIO $ copyFile systemPath sandboxPath
                    perms <- liftIO $ getPermissions sandboxPath
                    liftIO $ setPermissions sandboxPath (setOwnerExecutable True $
                                                       setOwnerReadable True $ perms)
                    logMsg 2 $ "Copied file to sandbox: " <> T.pack sandboxPath
                Right () ->
                    logMsg 2 $ "Linked file to sandbox: " <> T.pack systemPath
