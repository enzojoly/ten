{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Ten.Sandbox (
    withSandbox,
    SandboxConfig(..),
    defaultSandboxConfig
) where

import Control.Monad
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
        -- Create the basic structure
        liftIO $ createDirectoryIfMissing True (sandboxDir </> "tmp")
        liftIO $ createDirectoryIfMissing True (sandboxDir </> "build")
        
        -- Map all inputs into the sandbox
        mapM_ (setupInput sandboxDir) (Set.toList inputs)
        
        -- Add read-only bind mounts from config
        mapM_ (setupBindMount sandboxDir) (Map.toList $ sandboxReadOnlyBindMounts config)
        
        -- Log sandbox creation
        logMsg 1 $ "Created sandbox at: " <> T.pack sandboxDir
        
        -- Add sandbox proof
        addProof $ BuildProof
        
        -- Run the action inside the sandbox
        let buildDir = sandboxDir </> "build"
        result <- action buildDir
        
        -- Log sandbox cleanup
        logMsg 1 $ "Cleaning up sandbox: " <> T.pack sandboxDir
        
        return result
    
    -- Create the temporary directory and ensure cleanup
    liftIO (withSystemTempDirectory "ten-sandbox" (runSandboxed env state sandboxFunc))
        >>= either throwError return

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
    let destPath = sandboxDir </> "store" </> T.unpack (storeHash input) ++ "-" ++ T.unpack (storeName input)
    
    -- Create parent directory
    liftIO $ createDirectoryIfMissing True (takeDirectory destPath)
    
    -- Create a hard link (or copy if cross-device)
    result <- liftIO $ E.try $ createFileLink sourcePath destPath
    case result of
        Left (_ :: E.SomeException) -> do
            -- Fallback to copying if linking fails
            liftIO $ copyFile sourcePath destPath
            logMsg 2 $ "Copied input to sandbox: " <> T.pack destPath
        Right () ->
            logMsg 2 $ "Linked input to sandbox: " <> T.pack destPath

-- | Set up a bind mount in the sandbox
setupBindMount :: FilePath -> (FilePath, FilePath) -> TenM 'Build ()
setupBindMount sandboxDir (source, dest) = do
    let fullDest = sandboxDir </> dest
    
    -- Create parent directory
    liftIO $ createDirectoryIfMissing True (takeDirectory fullDest)
    
    -- In a real implementation, this would use mount --bind
    -- For now, we simulate with symlinks for portability
    liftIO $ createFileLink source fullDest
    logMsg 2 $ "Bind mounted (simulated): " <> T.pack source <> " -> " <> T.pack fullDest
