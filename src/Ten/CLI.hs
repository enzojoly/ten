{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Ten.CLI (
    Command(..),
    Options(..),
    defaultOptions,
    parseArgs,
    runCommand
) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Set (Set)
import qualified Data.Set as Set
import System.Directory
import System.FilePath
import System.Environment
import System.Exit

import qualified Ten.Core as Core  -- Use qualified import to avoid name clashes
import Ten.Core (BuildEnv(..), BuildError(..), TenM) -- Import specific non-conflicting names
import Ten.Build
import Ten.Store
import Ten.GC
import Ten.Derivation

-- | CLI commands
data Command
    = Build FilePath           -- Build a derivation file
    | Eval FilePath            -- Evaluate a Ten expression file
    | GC                       -- Garbage collect
    | Info FilePath            -- Show info about a store path
    | ListRoots                -- List garbage collection roots
    | Help                     -- Show help
    deriving (Show, Eq)

-- | CLI options
data Options = Options
    { optVerbosity :: Int      -- Verbosity level (0-3)
    , optStorePath :: FilePath -- Path to the store
    , optKeepFailed :: Bool    -- Keep failed build outputs
    } deriving (Show, Eq)

-- | Default options
defaultOptions :: Options
defaultOptions = Options
    { optVerbosity = 1
    , optStorePath = "/var/ten/store"
    , optKeepFailed = False
    }

-- | Parse command line arguments
parseArgs :: [String] -> Either String (Command, Options)
parseArgs [] = Right (Help, defaultOptions)
parseArgs (cmd:args) =
    case cmd of
        "build" ->
            case args of
                (file:rest) -> parseOptions rest (Build file)
                [] -> Left "build: missing file argument"
        "eval" ->
            case args of
                (file:rest) -> parseOptions rest (Eval file)
                [] -> Left "eval: missing file argument"
        "gc" -> parseOptions args GC
        "info" ->
            case args of
                (path:rest) -> parseOptions rest (Info path)
                [] -> Left "info: missing path argument"
        "list-roots" -> parseOptions args ListRoots
        "help" -> Right (Help, defaultOptions)
        _ -> Left $ "unknown command: " ++ cmd

-- | Parse options from arguments
parseOptions :: [String] -> Command -> Either String (Command, Options)
parseOptions args cmd = go args (cmd, defaultOptions)
  where
    go [] result = Right result
    go ("--verbose":rest) (c, opts) =
        go rest (c, opts { optVerbosity = min 3 (optVerbosity opts + 1) })
    go ("--quiet":rest) (c, opts) =
        go rest (c, opts { optVerbosity = max 0 (optVerbosity opts - 1) })
    go ("--store":path:rest) (c, opts) =
        go rest (c, opts { optStorePath = path })
    go ("--keep-failed":rest) (c, opts) =
        go rest (c, opts { optKeepFailed = True })
    go (unknown:_) _ =
        Left $ "unknown option: " ++ unknown

-- | Run a command with the given options
runCommand :: Command -> Options -> IO ()
runCommand cmd options = do
    -- Create the store directory if it doesn't exist
    createDirectoryIfMissing True (optStorePath options)

    -- Create the build environment
    let env = BuildEnv
            { workDir = "/tmp/ten-build"
            , storePath = optStorePath options
            , verbosity = optVerbosity options
            , allowedPaths = Set.empty
            }

    -- Execute the command
    result <- case cmd of
        Build file -> buildDerivationFile env file
        Eval file -> evalExpressionFile env file
        GC -> runGC env
        Info path -> showPathInfo env path
        ListRoots -> showRoots env
        Help -> showHelp >> return (Right ())

    -- Handle the result
    case result of
        Left err -> do
            TIO.putStrLn $ "Error: " <> T.pack (show err)
            exitFailure
        Right _ ->
            exitSuccess

-- | Build a derivation file
buildDerivationFile :: BuildEnv -> FilePath -> IO (Either BuildError ())
buildDerivationFile env file = do
    -- Check if the file exists
    exists <- doesFileExist file
    -- Fixed: Use if/then/else instead of unless
    if not exists
        then return $ Left $ InputNotFound file
        else do
            -- In a real implementation, we would:
            -- 1. Parse the derivation file
            -- 2. Instantiate the derivation
            -- 3. Build it
            -- For now, we just print a message
            putStrLn $ "Building derivation: " ++ file
            return $ Right ()

-- | Evaluate a Ten expression file
evalExpressionFile :: BuildEnv -> FilePath -> IO (Either BuildError ())
evalExpressionFile env file = do
    -- Check if the file exists
    exists <- doesFileExist file
    -- Fixed: Use if/then/else instead of unless
    if not exists
        then return $ Left $ InputNotFound file
        else do
            -- In a real implementation, we would:
            -- 1. Parse the Ten expression file
            -- 2. Evaluate it to produce a derivation
            -- 3. Instantiate the derivation
            -- For now, we just print a message
            putStrLn $ "Evaluating expression: " ++ file
            return $ Right ()

-- | Run garbage collection
runGC :: BuildEnv -> IO (Either BuildError ())
runGC env = do
    -- In a real implementation, we would:
    -- 1. Find all GC roots
    -- 2. Build the reachability graph
    -- 3. Delete unreachable paths
    -- For now, we just print a message

    putStrLn "Running garbage collection"
    return $ Right ()

-- | Show info about a store path
showPathInfo :: BuildEnv -> FilePath -> IO (Either BuildError ())
showPathInfo env path = do
    -- In a real implementation, we would:
    -- 1. Parse the path to get the hash and name
    -- 2. Check if it exists in the store
    -- 3. Display information about it
    -- For now, we just print a message

    putStrLn $ "Path info: " ++ path
    return $ Right ()

-- | Show all GC roots
showRoots :: BuildEnv -> IO (Either BuildError ())
showRoots env = do
    -- In a real implementation, we would:
    -- 1. List all roots
    -- 2. Display information about each
    -- For now, we just print a message

    putStrLn "Listing GC roots"
    return $ Right ()

-- | Show help text
showHelp :: IO ()
showHelp = do
    putStrLn "Ten - A pure functional build system"
    putStrLn ""
    putStrLn "Usage: ten COMMAND [OPTIONS]"
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "  build FILE      Build a derivation file"
    putStrLn "  eval FILE       Evaluate a Ten expression file"
    putStrLn "  gc              Run garbage collection"
    putStrLn "  info PATH       Show information about a store path"
    putStrLn "  list-roots      List garbage collection roots"
    putStrLn "  help            Show this help"
    putStrLn ""
    putStrLn "Options:"
    putStrLn "  --verbose       Increase verbosity"
    putStrLn "  --quiet         Decrease verbosity"
    putStrLn "  --store PATH    Set store path (default: /var/ten/store)"
    putStrLn "  --keep-failed   Keep outputs from failed builds"
