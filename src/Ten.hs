{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Ten (
    -- Re-exports from Core
    TenM,
    Phase(..),
    BuildEnv(..),
    BuildError(..),
    StorePath(..),
    Proof(..),
    runTen,
    evalTen,
    buildTen,
    initBuildEnv,

    -- Re-exports from Store
    addToStore,
    ensureInStore,
    readFromStore,
    verifyStorePath,
    storePathExists,

    -- Re-exports from Derivation
    Derivation(..),
    DerivationInput(..),
    DerivationOutput(..),
    mkDerivation,
    instantiateDerivation,

    -- Re-exports from Build
    BuildResult(..),
    buildDerivation,

    -- Re-exports from Sandbox
    SandboxConfig(..),
    defaultSandboxConfig,
    withSandbox,

    -- Re-exports from Graph
    BuildGraph(..),
    BuildNode(..),
    createBuildGraph,
    validateGraph,
    topologicalSort,

    -- Re-exports from GC
    GCRoot(..),
    GCStats(..),
    addRoot,
    removeRoot,
    collectGarbage,

    -- Basic utilities
    getStorePath,  -- Renamed from storePath to avoid name clash
    evalFile,
    buildFile,
    buildExpression
) where

import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import System.FilePath
import System.Environment (getEnv)

import Ten.Core
import Ten.Store
import Ten.Derivation
import Ten.Build
import Ten.Sandbox
import Ten.Graph
import Ten.GC
import qualified Ten.Hash as Hash

-- | Get a standard store path
getStorePath :: FilePath -> IO FilePath  -- Renamed from storePath
getStorePath override
    | not (null override) = return override
    | otherwise = do
        home <- getEnv "HOME"
        return $ home </> ".ten/store"

-- | Evaluate a Ten expression file
evalFile :: FilePath -> IO (Either BuildError Derivation)
evalFile file = do
    store <- getStorePath ""  -- Updated to use the renamed function
    let env = initBuildEnv "/tmp/ten-build" store
    content <- TIO.readFile file
    result <- evalTen (evalExpression content) env
    return $ case result of
        Left err -> Left err
        Right (drv, _) -> Right drv

-- | Build a derivation file
buildFile :: FilePath -> IO (Either BuildError BuildResult)
buildFile file = do
    store <- getStorePath ""  -- Updated to use the renamed function
    let env = initBuildEnv "/tmp/ten-build" store
    content <- BS.readFile file
    -- In a real implementation, we would parse the derivation file
    -- For now, we simply return an error
    return $ Left $ BuildFailed "Not implemented"

-- | Build a Ten expression
buildExpression :: Text -> IO (Either BuildError BuildResult)
buildExpression expr = do
    store <- getStorePath ""  -- Updated to use the renamed function
    let env = initBuildEnv "/tmp/ten-build" store

    -- Step 1: Evaluate the expression to get a derivation
    evalResult <- evalTen (evalExpression expr) env
    case evalResult of
        Left err -> return $ Left err
        Right (drv, _) -> do
            -- Step 2: Build the derivation
            buildResult <- buildTen (buildDerivation drv) env
            return $ case buildResult of
                Left err -> Left err
                Right (result, _) -> Right result

-- | Evaluate a Ten expression (stub implementation)
evalExpression :: Text -> TenM 'Eval Derivation
evalExpression expr = do
    -- In a real implementation, we would:
    -- 1. Parse the expression into an AST
    -- 2. Interpret the AST to produce a derivation
    -- For now, we just create a dummy derivation

    throwError $ EvalError "Expression evaluation not implemented"
