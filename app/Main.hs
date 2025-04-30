module Main where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hPutStrLn, stderr)

import Ten

-- | Main entry point for ten executable
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> showHelp >> exitSuccess
        ["help"] -> showHelp >> exitSuccess
        ["version"] -> showVersion >> exitSuccess
        ["daemon", "start"] -> startDaemon [] >> exitSuccess
        ["daemon", "stop"] -> shutdownDaemon >> exitSuccess
        ["daemon", "status"] -> checkDaemonStatus >> exitSuccess
        ["build", path] -> buildFile path Nothing >>= handleResult
        ["eval", path] -> evalFile path Nothing >>= handleResult
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

-- | Start the daemon
startDaemon :: [String] -> IO ()
startDaemon args = do
    running <- isDaemonRunning =<< getDefaultSocketPath
    if running
        then putStrLn "Daemon is already running"
        else do
            result <- startDaemonIfNeeded
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Failed to start daemon: " ++ show err
                    exitFailure
                Right _ -> putStrLn "Daemon started successfully"

-- | Shutdown the daemon
shutdownDaemon :: IO ()
shutdownDaemon = do
    socketPath <- getDefaultSocketPath
    running <- isDaemonRunning socketPath
    if not running
        then putStrLn "Daemon is not running"
        else do
            result <- withDaemonConnection' socketPath $ \conn -> do
                sendRequestSync conn (Request 0 "shutdown" Map.empty Nothing) 5000000
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Failed to stop daemon: " ++ show err
                    exitFailure
                Right _ -> putStrLn "Daemon stopped successfully"

-- | Check daemon status
checkDaemonStatus :: IO ()
checkDaemonStatus = do
    socketPath <- getDefaultSocketPath
    running <- isDaemonRunning socketPath
    if not running
        then putStrLn "Daemon is not running"
        else do
            result <- withDaemonConnection' socketPath $ \conn -> do
                sendRequestSync conn (Request 0 "status" Map.empty Nothing) 5000000
            case result of
                Left err -> do
                    hPutStrLn stderr $ "Failed to get daemon status: " ++ show err
                    exitFailure
                Right (StatusResponse status) -> do
                    putStrLn $ "Daemon status: " ++ T.unpack (daemonStatus status)
                    putStrLn $ "Uptime: " ++ show (daemonUptime status) ++ " seconds"
                    putStrLn $ "Active builds: " ++ show (daemonActiveBuilds status)
                    putStrLn $ "Store size: " ++ show (daemonStoreSize status) ++ " bytes"
                    putStrLn $ "Store paths: " ++ show (daemonStorePaths status)
                Right _ -> do
                    hPutStrLn stderr "Unexpected response from daemon"
                    exitFailure

-- | Handle build result
handleResult :: Either BuildError a -> IO ()
handleResult (Left err) = do
    hPutStrLn stderr $ "Error: " ++ show err
    exitFailure
handleResult (Right _) = do
    putStrLn "Build completed successfully"
    exitSuccess
