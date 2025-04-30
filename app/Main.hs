{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (void)
import Control.Exception (try, SomeException)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr, stdout)

import Ten -- Import the main facade

-- | Main entry point for ten executable
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showHelp >> exitSuccess
        ["help"] -> showHelp >> exitSuccess
        ["version"] -> showVersion >> exitSuccess
        ["daemon", "start"] -> handleDaemonStart
        ["daemon", "stop"] -> handleDaemonStop
        ["daemon", "status"] -> handleDaemonStatus
        ["build", path] -> handleBuild path
        ["eval", path] -> handleEval path
        _ -> do
            hPutStrLn stderr "Unknown command or invalid arguments"
            showHelp
            exitFailure

-- | Show help text
showHelp :: IO ()
showHelp = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " COMMAND [ARGS]"
    putStrLn ""
    putStrLn "Commands:"
    putStrLn "  build FILE      Build the specified derivation file"
    putStrLn "  eval FILE       Evaluate the specified file"
    putStrLn "  daemon start    Start the Ten daemon"
    putStrLn "  daemon stop     Stop the Ten daemon"
    putStrLn "  daemon status   Check daemon status"
    putStrLn "  help            Show this help"
    putStrLn "  version         Show version information"

-- | Show version information
showVersion :: IO ()
showVersion = putStrLn "Ten version 0.1.0"

-- | Handle starting the daemon
handleDaemonStart :: IO ()
handleDaemonStart = do
    socketPath <- getDefaultSocketPath
    result <- startDaemonIfNeeded socketPath
    case result of
        Left err -> do
            hPutStrLn stderr $ "Failed to start daemon: " ++ show err
            exitFailure
        Right _ -> do
            putStrLn "Daemon started successfully (or was already running)."
            exitSuccess

-- | Handle stopping the daemon
handleDaemonStop :: IO ()
handleDaemonStop = do
    socketPath <- getDefaultSocketPath
    -- Attempt to connect (implicitly requires Builder privilege)
    -- A real implementation would need proper credentials
    let creds = UserCredentials "cli-user" "dummy-token" Builder
    connResult <- connectToDaemon socketPath creds
    case connResult of
        Left err -> do
            -- Check if the error is simply "daemon not running"
            running <- isDaemonRunning socketPath
            if not running
                then putStrLn "Daemon is not running." >> exitSuccess
                else hPutStrLn stderr ("Failed to connect to daemon: " ++ show err) >> exitFailure
        Right conn -> do
            -- Send shutdown request
            shutdownResult <- shutdownDaemon conn
            -- Disconnect regardless of shutdown result
            disconnectFromDaemon conn
            -- Handle shutdown result
            case shutdownResult of
                Left err -> do
                    hPutStrLn stderr $ "Failed to stop daemon: " ++ show err
                    exitFailure
                Right _ -> do
                    putStrLn "Daemon stopped successfully."
                    exitSuccess

-- | Handle checking daemon status
handleDaemonStatus :: IO ()
handleDaemonStatus = do
    socketPath <- getDefaultSocketPath
    -- Attempt to connect
    let creds = UserCredentials "cli-user" "dummy-token" Builder
    connResult <- connectToDaemon socketPath creds
    case connResult of
        Left err -> do
            running <- isDaemonRunning socketPath
            if not running
                then putStrLn "Daemon is not running." >> exitSuccess
                else hPutStrLn stderr ("Failed to connect to daemon: " ++ show err) >> exitFailure
        Right conn -> do
            -- Get status
            statusResult <- getDaemonStatus conn
            -- Disconnect
            disconnectFromDaemon conn
            -- Handle status result
            case statusResult of
                Left err -> do
                    hPutStrLn stderr $ "Failed to get daemon status: " ++ show err
                    exitFailure
                Right status -> do
                    putStrLn $ "Daemon status: " ++ T.unpack (daemonStatus status)
                    putStrLn $ "Uptime: " ++ show (daemonUptime status) ++ " seconds"
                    putStrLn $ "Active builds: " ++ show (daemonActiveBuilds status)
                    putStrLn $ "Store size: " ++ show (daemonStoreSize status) ++ " bytes"
                    putStrLn $ "Store paths: " ++ show (daemonStorePaths status)
                    exitSuccess

-- | Handle build command
handleBuild :: FilePath -> IO ()
handleBuild path = do
    socketPath <- getDefaultSocketPath
    let creds = UserCredentials "cli-user" "dummy-token" Builder
    connResult <- connectToDaemon socketPath creds
    case connResult of
        Left err -> handleConnectionError err
        Right conn -> do
            buildResult <- buildFile conn path
            disconnectFromDaemon conn
            handleResult buildResult

-- | Handle eval command
handleEval :: FilePath -> IO ()
handleEval path = do
    socketPath <- getDefaultSocketPath
    let creds = UserCredentials "cli-user" "dummy-token" Builder
    connResult <- connectToDaemon socketPath creds
    case connResult of
        Left err -> handleConnectionError err
        Right conn -> do
            evalResult <- evalFile conn path
            disconnectFromDaemon conn
            handleResult evalResult

-- | Handle connection errors
handleConnectionError :: BuildError -> IO ()
handleConnectionError err = do
    hPutStrLn stderr $ "Error connecting to daemon: " ++ show err
    hPutStrLn stderr "Ensure the daemon is running ('ten daemon start') and the socket path is correct."
    exitFailure

-- | Handle build/eval result
handleResult :: Show a => Either BuildError a -> IO ()
handleResult (Left err) = do
    hPutStrLn stderr $ "Error: " ++ show err
    exitFailure
handleResult (Right result) = do
    putStrLn "Operation completed successfully:"
    print result -- Simple print for demonstration
    exitSuccess
