module Main where

import System.Environment
import System.Exit

import Ten.CLI

main :: IO ()
main = do
    -- Get command line arguments
    args <- getArgs
    
    -- Parse arguments
    case parseArgs args of
        Left err -> do
            putStrLn $ "Error: " ++ err
            putStrLn "Run 'ten help' for usage information"
            exitFailure
        
        Right (cmd, opts) -> do
            -- Run the command
            runCommand cmd opts
