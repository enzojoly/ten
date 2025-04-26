{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Ten.Daemon.Client (
    -- Socket management with privilege awareness
    connectToDaemon,
    disconnectFromDaemon,
    getDefaultSocketPath,
    withDaemonConnection,

    -- Client communication with privilege checks
    sendRequest,
    receiveResponse,
    sendRequestSync,

    -- Status checking
    isDaemonRunning,
    getDaemonStatus,

    -- High-level build operations
    buildFile,
    evalFile,
    buildDerivation,
    cancelBuild,
    getBuildStatus,
    getBuildOutput,
    listBuilds,

    -- Store operations (with proper privilege handling)
    addFileToStore,
    verifyStorePath,
    getStorePathForFile,
    listStore,

    -- Derivation operations
    storeDerivation,
    retrieveDerivation,
    queryDerivationForOutput,
    queryOutputsForDerivation,
    listDerivations,
    getDerivationInfo,

    -- GC operations (daemon-only, via protocol)
    collectGarbage,
    getGCStatus,
    addGCRoot,
    removeGCRoot,
    listGCRoots,

    -- Daemon management
    startDaemonIfNeeded,
    shutdownDaemon,
    getDaemonConfig,

    -- Authentication types re-exports
    UserCredentials(..),

    -- Internal utilities
    createSocketAndConnect,
    readResponseWithTimeout
) where

import Control.Concurrent (forkIO, ThreadId, threadDelay, myThreadId, killThread)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar, newMVar, readMVar, withMVar)
import Control.Concurrent.STM
import Control.Exception (catch, finally, bracketOnError, bracket, throwIO, SomeException, try, IOException)
import Control.Monad (void, when, forever, unless)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Unique (hashUnique)
import Data.Word (Word8, Word32, Word64)
import Data.Bits (shiftL, (.|.))
import Network.Socket (Socket, Family(..), SocketType(..), SockAddr(..), socket, connect, close, socketToHandle)
import Network.Socket.ByteString (sendAll, recv)
import System.Directory (doesFileExist, createDirectoryIfMissing, getHomeDirectory, getXdgDirectory, XdgDirectory(..), findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeDirectory)
import System.IO (Handle, IOMode(..), hClose, hFlush, hPutStrLn, stderr, hPutStr, BufferMode(..), hSetBuffering)
import System.Process (createProcess, proc, waitForProcess, StdStream(..), CreateProcess(..), ProcessHandle)
import qualified System.Timeout as SystemTimeout

-- Import Ten modules
import Ten.Core hiding (sendRequest, receiveResponse, sendRequestSync, timeout)
import qualified Ten.Core as Core
import Ten.Daemon.Protocol
import Ten.Daemon.Config (getDefaultSocketPath)
import qualified Ten.Derivation as Derivation

-- | Connect to the Ten daemon - always in Builder context
connectToDaemon :: FilePath -> UserCredentials -> IO (Either BuildError (DaemonConnection 'Builder))
connectToDaemon socketPath credentials = try $ do
    -- Check if daemon is running
    running <- isDaemonRunning socketPath

    -- If not running, try to start it if autostart is enabled
    unless running $ do
        startResult <- startDaemonIfNeeded socketPath
        case startResult of
            Left err -> throwIO err
            Right _ ->
                -- Brief pause to allow daemon to initialize
                threadDelay 500000 -- 0.5 seconds

    -- Create socket and connect
    (sock, handle) <- createSocketAndConnect socketPath

    -- Set up proper handle buffering
    hSetBuffering handle (BlockBuffering Nothing)

    -- Create authentication request using Protocol's AuthRequestContent
    let authContent = AuthRequestContent {
            authVersion = currentProtocolVersion,
            authUser = username credentials,
            authToken = token credentials,
            authRequestedTier = Builder -- Always request Builder tier
        }

    -- Create the auth request and serialize it
    let authReq = AuthRequest authContent
    let (reqData, _) = serializeDaemonRequest authReq
    let framedReq = createRequestFrame reqData
    sendAll sock framedReq

    -- Receive and parse authentication response
    respResult <- try $ receiveFramedResponse sock
    case respResult of
        Left (e :: SomeException) ->
            throwIO $ AuthError "Connection error during authentication"
        Right (respData, mRespPayload) ->
            case deserializeDaemonResponse respData mRespPayload of
                Left err ->
                    throwIO $ AuthError $ "Authentication failed: " <> err
                Right (AuthResponse authResult) ->
                    case authResult of
                        AuthAccepted userId authToken _ -> do
                            -- Create daemon connection with proper privilege tier evidence
                            conn <- createDaemonConnection sock handle userId authToken sBuilder
                            return conn
                        AuthRejected reason ->
                            throwIO $ AuthError $ "Authentication rejected: " <> reason
                        AuthSuccess userId authToken -> do
                            -- Legacy format support
                            conn <- createDaemonConnection sock handle userId authToken sBuilder
                            return conn
                Right _ ->
                    throwIO $ AuthError "Invalid response type for authentication"

-- | Disconnect from the Ten daemon
disconnectFromDaemon :: DaemonConnection 'Builder -> IO ()
disconnectFromDaemon = Core.closeDaemonConnection

-- | Check if the daemon is running
isDaemonRunning :: FilePath -> IO Bool
isDaemonRunning socketPath = do
    -- Check if socket file exists
    socketExists <- doesFileExist socketPath

    if not socketExists
        then return False
        else do
            -- Try to connect to the socket
            result <- try $ createSocketAndConnect socketPath
            case result of
                Left (_ :: SomeException) ->
                    -- Connection failed, daemon not running
                    return False

                Right (sock, handle) -> do
                    -- Connection succeeded, daemon is running
                    hClose handle
                    close sock
                    return True


-- | Create a socket and connect to the daemon
createSocketAndConnect :: FilePath -> IO (Socket, Handle)
createSocketAndConnect socketPath = do
    -- Ensure parent directory exists
    createDirectoryIfMissing True (takeDirectory socketPath)

    -- Create socket
    sock <- socket AF_UNIX Stream 0

    -- Connect to socket with error handling
    bracketOnError
        (return sock)
        close
        (\s -> do
            connect s (SockAddrUnix socketPath)
            handle <- socketToHandle s ReadWriteMode
            return (s, handle))

-- | Convert a DaemonConnection to a ProtocolHandle for Protocol functions
asDaemonProtocolHandle :: DaemonConnection t -> IO ProtocolHandle
asDaemonProtocolHandle conn = do
    mvar <- newMVar ()
    return ProtocolHandle {
        protocolSocket = connSocket conn,
        protocolLock = mvar,
        protocolPrivilegeTier = fromSing (connPrivEvidence conn)
    }

-- | Send a request to the daemon
sendRequest :: DaemonConnection 'Builder -> DaemonRequest -> IO (Either BuildError Int)
sendRequest conn request = do
    -- Using Protocol's request serialization
    let (reqData, mPayload) = serializeDaemonRequest request

    -- Frame the request
    let framedReq = createRequestFrame reqData
    let framedPayload = case mPayload of
            Just payload -> createRequestFrame payload
            Nothing -> BS.empty

    -- Send the request and optional payload
    result <- try $ do
        mvar <- newMVar ()
        withMVar mvar $ \_ -> do
            sendAll (connSocket conn) framedReq

            unless (BS.null framedPayload) $
                sendAll (connSocket conn) framedPayload

            -- Store request ID for tracking
            reqId <- atomically $ do
                rid <- readTVar (connNextReqId conn)
                writeTVar (connNextReqId conn) (rid + 1)
                return rid

            -- Create response variable
            respVar <- newEmptyMVar

            -- Register the pending request
            atomically $ modifyTVar' (connRequestMap conn) $ Map.insert reqId respVar

            return reqId

    case result of
        Left err -> return $ Left $ DaemonError $ T.pack $ show err
        Right reqId -> return $ Right reqId

-- | Receive a response for a specific request
receiveResponse :: DaemonConnection 'Builder -> Int -> Int -> IO (Either BuildError DaemonResponse)
receiveResponse conn reqId timeoutMicros = do
    -- Get response MVar
    reqMap <- readTVarIO (connRequestMap conn)
    case Map.lookup reqId reqMap of
        Nothing ->
            -- No such request ID
            return $ Left $ DaemonError $ "Unknown request ID: " <> T.pack (show reqId)

        Just respVar -> do
            -- Wait for response with timeout
            mResult <- SystemTimeout.timeout timeoutMicros $ takeMVar respVar

            -- Clean up request map
            atomically $ modifyTVar' (connRequestMap conn) $ Map.delete reqId

            -- Return response or error
            case mResult of
                Nothing -> return $ Left $ DaemonError "Timeout waiting for daemon response"
                Just resp -> return $ Right resp

-- | Send a request and wait for response (synchronous)
sendRequestSync :: DaemonConnection 'Builder -> DaemonRequest -> Int -> IO (Either BuildError DaemonResponse)
sendRequestSync conn request timeoutMicros = do
    -- Send the request
    reqIdResult <- sendRequest conn request

    -- Wait for response if request was sent successfully
    case reqIdResult of
        Left err -> return $ Left err
        Right reqId -> receiveResponse conn reqId timeoutMicros

-- | Read a message with timeout
readResponseWithTimeout :: Handle -> Int -> IO BS.ByteString
readResponseWithTimeout handle timeoutMicros = do
    -- Use Protocol's framing to read a response
    result <- SystemTimeout.timeout timeoutMicros $ do
        -- Read length prefix (4 bytes)
        lenBytes <- BS.hGet handle 4
        when (BS.length lenBytes /= 4) $
            throwIO $ DaemonError "Disconnected from daemon while reading message length"

        -- Decode message length - using Word32 consistently for binary protocol
        let len = fromIntegral $
                  (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
                  (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
                  (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
                  (fromIntegral (BS.index lenBytes 3) :: Word32)

        -- Sanity check on message length
        when (len > 100 * 1024 * 1024) $ -- 100 MB limit
            throwIO $ DaemonError $ "Message too large: " <> T.pack (show len) <> " bytes"

        -- Read message body
        msgBytes <- BS.hGet handle len
        when (BS.length msgBytes /= len) $
            throwIO $ DaemonError "Disconnected from daemon while reading message body"

        -- Return the complete message
        return $ BS.append lenBytes msgBytes

    case result of
        Nothing -> throwIO $ DaemonError "Timeout waiting for daemon response"
        Just msg -> return msg

-- | Receive a framed response from socket
receiveFramedResponse :: Socket -> IO (BS.ByteString, Maybe BS.ByteString)
receiveFramedResponse sock = do
    -- Read response frame using Protocol's framing
    responseData <- recvFrame sock

    -- Check if response indicates a payload using Protocol's logic
    case Aeson.decodeStrict responseData of
        Nothing -> return (responseData, Nothing)
        Just obj -> do
            let hasPayload = case Aeson.fromJSON obj of
                    Aeson.Success (Aeson.Object o) ->
                        case KeyMap.lookup "hasContent" o of
                            Just (Aeson.Bool True) -> True
                            _ -> False
                    _ -> False

            if hasPayload
                then do
                    -- Read payload frame
                    payloadData <- recvFrame sock
                    return (responseData, Just payloadData)
                else
                    return (responseData, Nothing)
  where
    recvFrame :: Socket -> IO BS.ByteString
    recvFrame s = do
        -- Read length prefix (4 bytes)
        lenBytes <- recv s 4
        when (BS.length lenBytes /= 4) $
            throwIO ConnectionClosed

        -- Parse length - using Word32 consistently for binary protocol
        let len = (fromIntegral (BS.index lenBytes 0) :: Word32) `shiftL` 24 .|.
                 (fromIntegral (BS.index lenBytes 1) :: Word32) `shiftL` 16 .|.
                 (fromIntegral (BS.index lenBytes 2) :: Word32) `shiftL` 8 .|.
                 (fromIntegral (BS.index lenBytes 3) :: Word32)

        -- Check if message is too large
        when (len > 100 * 1024 * 1024) $ -- 100 MB limit
            throwIO $ MessageTooLarge len

        -- Read message data
        recvExactly s (fromIntegral len)

    recvExactly :: Socket -> Int -> IO BS.ByteString
    recvExactly s len = go len []
      where
        go 0 chunks = return $ BS.concat $ reverse chunks
        go n chunks = do
            chunk <- recv s n
            let chunkLen = BS.length chunk
            if chunkLen == 0
                then throwIO ConnectionClosed
                else go (n - chunkLen) (chunk : chunks)

-- | Start the daemon if it's not running
startDaemonIfNeeded :: FilePath -> IO (Either BuildError ())
startDaemonIfNeeded socketPath = try $ do
    -- Check if daemon is already running
    running <- isDaemonRunning socketPath

    unless running $ do
        -- Find the ten-daemon executable
        mDaemonPath <- findExecutable "ten-daemon"
        case mDaemonPath of
            Nothing ->
                throwIO $ DaemonError "Could not find ten-daemon executable in PATH"

            Just daemonPath -> do
                -- Start daemon in background
                (_, _, _, processHandle) <- createProcess $
                    (proc daemonPath ["start", "--socket=" ++ socketPath])
                        { std_out = CreatePipe
                        , std_err = CreatePipe
                        , detach_console = True  -- Run detached on Windows
                        }

                -- Wait for daemon to start with timeout
                let maxWaitTime = 15.0 -- 15 seconds

                -- Check with retries
                let checkWithRetry :: Int -> Double -> IO ()
                    checkWithRetry retries timeElapsed = do
                        if timeElapsed >= maxWaitTime
                            then throwIO $ DaemonError "Timeout waiting for daemon to start"
                            else do
                                -- Check if daemon is now running
                                running <- isDaemonRunning socketPath
                                if running
                                    then return ()
                                    else do
                                        -- Check if process has exited
                                        exitCode <- getProcessExitCode processHandle
                                        case exitCode of
                                            Just ExitSuccess -> do
                                                -- Process exited successfully but daemon not running yet
                                                -- Wait and retry
                                                threadDelay 500000 -- 0.5 seconds
                                                checkWithRetry (retries-1) (timeElapsed + 0.5)

                                            Just (ExitFailure code) ->
                                                -- Process failed
                                                throwIO $ DaemonError $ "Daemon process exited with error code: " <> T.pack (show code)

                                            Nothing -> do
                                                -- Process still running
                                                threadDelay 500000 -- 0.5 seconds
                                                checkWithRetry (retries-1) (timeElapsed + 0.5)

                -- Start checking
                checkWithRetry 20 0 -- 20 retries, 0 seconds elapsed

                -- Verify daemon is now running
                running <- isDaemonRunning socketPath
                unless running $
                    throwIO $ DaemonError "Daemon process started but daemon is not responding"

-- | Get process exit code (non-blocking)
getProcessExitCode :: ProcessHandle -> IO (Maybe ExitCode)
getProcessExitCode ph = do
    -- Try to get exit code without waiting
    result <- try $ waitForProcess ph
    case result of
        Left (_ :: SomeException) -> return Nothing
        Right exitCode -> return $ Just exitCode

-- | Build a file using the daemon
buildFile :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError BuildResult)
buildFile conn filePath = do
    -- Check file existence first and return early if it doesn't exist
    fileExists <- doesFileExist filePath
    if not fileExists
        then return $ Left $ InputNotFound filePath
        else do
            -- Read file content
            content <- BS.readFile filePath

            -- Create build request with positional arguments
            let request = BuildRequest (T.pack filePath) (Just content) defaultBuildRequestInfo

            -- Send request and wait for response
            respResult <- sendRequestSync conn request (120 * 1000000) -- 120 seconds timeout

            -- Process response
            case respResult of
                Left err ->
                    return $ Left err

                Right (BuildResultResponse result) ->
                    return $ Right result

                Right resp ->
                    return $ Left $ DaemonError $
                        "Invalid response type for build request: " <> T.pack (show resp)

-- | Evaluate a file using the daemon
evalFile :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError Derivation)
evalFile conn filePath = do
    -- Check file existence first and return early if it doesn't exist
    fileExists <- doesFileExist filePath
    if not fileExists
        then return $ Left $ InputNotFound filePath
        else do
            -- Read file content
            content <- BS.readFile filePath

            -- Create eval request with positional arguments
            let request = EvalRequest (T.pack filePath) (Just content) defaultBuildRequestInfo

            -- Send request and wait for response
            respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

            -- Process response
            case respResult of
                Left err ->
                    return $ Left err

                Right (DerivationResponse derivation) ->
                    return $ Right derivation

                Right (EvalResponse derivation) ->
                    return $ Right derivation

                Right resp ->
                    return $ Left $ DaemonError $
                        "Invalid response type for eval request: " <> T.pack (show resp)

-- | Build a derivation using the daemon
buildDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError BuildResult)
buildDerivation conn derivation = do
    -- Create build derivation request with positional arguments
    let request = BuildDerivationRequest derivation defaultBuildRequestInfo

    -- Send request and wait for response (longer timeout for builds)
    respResult <- sendRequestSync conn request (3600 * 1000000) -- 1 hour timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildResultResponse result) ->
            return $ Right result

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build derivation request: " <> T.pack (show resp)

-- | Cancel a build
cancelBuild :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError ())
cancelBuild conn buildId = do
    -- Create cancel build request with positional argument
    let request = CancelBuildRequest buildId

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (CancelBuildResponse _) ->
            return $ Right ()

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for cancel build request: " <> T.pack (show resp)

-- | Get status of a build
getBuildStatus :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError BuildStatusUpdate)
getBuildStatus conn buildId = do
    -- Create build status request with positional argument
    let request = BuildStatusRequest buildId

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildStatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build status request: " <> T.pack (show resp)

-- | Get output of a build
getBuildOutput :: DaemonConnection 'Builder -> BuildId -> IO (Either BuildError Text)
getBuildOutput conn buildId = do
    -- Create build output request with positional argument
    let request = QueryBuildOutputRequest buildId

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildOutputResponse output) ->
            return $ Right output

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for build output request: " <> T.pack (show resp)

-- | List all builds
listBuilds :: DaemonConnection 'Builder -> Maybe Int -> IO (Either BuildError [(BuildId, BuildStatus, Float)])
listBuilds conn limit = do
    -- Create list builds request with positional argument
    let request = ListBuildsRequest limit

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (BuildListResponse builds) ->
            return $ Right builds

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list builds request: " <> T.pack (show resp)

-- | Add a file to the store (requires daemon privileges via protocol)
addFileToStore :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError StorePath)
addFileToStore conn filePath = do
    -- Check file existence first and return early if it doesn't exist
    fileExists <- doesFileExist filePath
    if not fileExists
        then return $ Left $ InputNotFound filePath
        else do
            -- Read file content
            content <- BS.readFile filePath

            -- Create store add request with positional arguments
            let request = StoreAddRequest (T.pack filePath) content

            -- Send request and wait for response
            respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

            -- Process response
            case respResult of
                Left err ->
                    return $ Left err

                Right (StoreAddResponse path) ->
                    return $ Right path

                Right resp ->
                    return $ Left $ DaemonError $
                        "Invalid response type for store add request: " <> T.pack (show resp)

-- | Verify a store path
verifyStorePath :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError Bool)
verifyStorePath conn path = do
    -- Create verify request with positional argument
    let request = StoreVerifyRequest path

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StoreVerifyResponse valid) ->
            return $ Right valid

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store verify request: " <> T.pack (show resp)

-- | Get store path for a file
getStorePathForFile :: DaemonConnection 'Builder -> FilePath -> IO (Either BuildError StorePath)
getStorePathForFile conn filePath = do
    -- Check file existence first and return early if it doesn't exist
    fileExists <- doesFileExist filePath
    if not fileExists
        then return $ Left $ InputNotFound filePath
        else do
            -- Read file content
            content <- BS.readFile filePath

            -- Create store path request with positional arguments
            let request = StorePathRequest (T.pack filePath) content

            -- Send request and wait for response
            respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

            -- Process response
            case respResult of
                Left err ->
                    return $ Left err

                Right (StorePathResponse path) ->
                    return $ Right path

                Right resp ->
                    return $ Left $ DaemonError $
                        "Invalid response type for store path request: " <> T.pack (show resp)

-- | List store contents
listStore :: DaemonConnection 'Builder -> IO (Either BuildError [StorePath])
listStore conn = do
    -- Create list request
    let request = StoreListRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StoreListResponse paths) ->
            return $ Right paths

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store list request: " <> T.pack (show resp)

-- | Store a derivation in the daemon store (requires daemon privileges via protocol)
storeDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError StorePath)
storeDerivation conn derivation = do
    -- Serialize the derivation
    let serialized = Derivation.serializeDerivation derivation

    -- Create store derivation request with positional argument
    let request = StoreDerivationRequest serialized

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationStoredResponse path) ->
            return $ Right path

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for store derivation request: " <> T.pack (show resp)

-- | Retrieve a derivation from the store
retrieveDerivation :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError (Maybe Derivation))
retrieveDerivation conn path = do
    -- Create retrieve derivation request with positional argument
    let request = RetrieveDerivationRequest path

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationRetrievedResponse mDrv) ->
            return $ Right mDrv

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for retrieve derivation request: " <> T.pack (show resp)

-- | Query which derivation produced a particular output
queryDerivationForOutput :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError (Maybe Derivation))
queryDerivationForOutput conn outputPath = do
    -- Create get derivation for output request with positional argument
    let request = GetDerivationForOutputRequest (storePathToText outputPath)

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationResponse derivation) ->
            return $ Right $ Just derivation

        Right (DerivationRetrievedResponse mDrv) ->
            return $ Right mDrv

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for query derivation for output request: " <> T.pack (show resp)

-- | Query all outputs produced by a derivation
queryOutputsForDerivation :: DaemonConnection 'Builder -> Derivation -> IO (Either BuildError (Set StorePath))
queryOutputsForDerivation conn derivation = do
    -- Create query derivation outputs request with positional arguments
    let request = QueryDerivationRequest "outputs" (derivHash derivation) Nothing

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationOutputResponse outputs) ->
            return $ Right outputs

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for query outputs for derivation request: " <> T.pack (show resp)

-- | List all derivations in the store
listDerivations :: DaemonConnection 'Builder -> IO (Either BuildError [StorePath])
listDerivations conn = do
    -- Create list derivations request with positional argument
    let request = ListDerivationsRequest Nothing

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (60 * 1000000) -- 60 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (DerivationListResponse paths) ->
            return $ Right paths

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list derivations request: " <> T.pack (show resp)

-- | Get detailed information about a derivation
getDerivationInfo :: DaemonConnection 'Builder -> StorePath -> IO (Either BuildError DerivationInfo)
getDerivationInfo conn path = do
    -- Create query derivation info request with positional arguments
    let request = QueryDerivationRequest "info" (storePathToText path) (Just 1)

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right resp ->
            -- Extract DerivationInfo from response type
            case resp of
                DerivationInfoResponse drv outputs inputs storePath metadata ->
                    return $ Right $ DerivationInfo {
                        derivationId = 0,  -- ID is assigned by database
                        derivationHash = derivHash drv,
                        derivationStorePath = storePath,
                        derivationTimestamp = 0  -- Timestamp from database
                    }

                _ -> return $ Left $ DaemonError $
                    "Invalid response type for get derivation info request: " <> T.pack (show resp)

-- | Run garbage collection (requires daemon privileges via protocol)
collectGarbage :: DaemonConnection 'Builder -> Bool -> IO (Either BuildError GCStats)
collectGarbage conn force = do
    -- Create GC request with positional argument
    let params = GCRequestParams force
    let request = GCRequest params

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (300 * 1000000) -- 5 minutes timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCResultResponse stats) ->
            return $ Right stats

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for GC request: " <> T.pack (show resp)

-- | Get GC status
getGCStatus :: DaemonConnection 'Builder -> Bool -> IO (Either BuildError GCStatusInfo)
getGCStatus conn forceCheck = do
    -- Create GC status request with positional argument
    let params = GCStatusRequestParams forceCheck
    let request = GCStatusRequest params

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCStatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for GC status request: " <> T.pack (show resp)

-- | Add a GC root (requires daemon privileges via protocol)
addGCRoot :: DaemonConnection 'Builder -> StorePath -> Text -> Bool -> IO (Either BuildError Text)
addGCRoot conn path name permanent = do
    -- Create add GC root request with positional arguments
    let request = AddGCRootRequest path name permanent

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCRootAddedResponse rootName) ->
            return $ Right rootName

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for add GC root request: " <> T.pack (show resp)

-- | Remove a GC root (requires daemon privileges via protocol)
removeGCRoot :: DaemonConnection 'Builder -> Text -> IO (Either BuildError Text)
removeGCRoot conn name = do
    -- Create remove GC root request with positional argument
    let request = RemoveGCRootRequest name

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCRootRemovedResponse rootName) ->
            return $ Right rootName

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for remove GC root request: " <> T.pack (show resp)

-- | List all GC roots
listGCRoots :: DaemonConnection 'Builder -> IO (Either BuildError [(StorePath, Text, Bool)])
listGCRoots conn = do
    -- Create list GC roots request
    let request = ListGCRootsRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (30 * 1000000) -- 30 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (GCRootListResponse roots) ->
            return $ Right $ map (\(GCRoot path name _ _) -> (path, name, False)) roots

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for list GC roots request: " <> T.pack (show resp)

-- | Shutdown the daemon (requires daemon privileges via protocol)
shutdownDaemon :: DaemonConnection 'Builder -> IO (Either BuildError ())
shutdownDaemon conn = do
    -- Create shutdown request
    let request = ShutdownRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right ShutdownResponse ->
            return $ Right ()

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for shutdown request: " <> T.pack (show resp)

-- | Get daemon status
getDaemonStatus :: DaemonConnection 'Builder -> IO (Either BuildError DaemonStatus)
getDaemonStatus conn = do
    -- Create status request
    let request = StatusRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (StatusResponse status) ->
            return $ Right status

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for status request: " <> T.pack (show resp)

-- | Get daemon configuration
getDaemonConfig :: DaemonConnection 'Builder -> IO (Either BuildError DaemonConfig)
getDaemonConfig conn = do
    -- Create config request
    let request = ConfigRequest

    -- Send request and wait for response
    respResult <- sendRequestSync conn request (10 * 1000000) -- 10 seconds timeout

    -- Process response
    case respResult of
        Left err ->
            return $ Left err

        Right (ConfigResponse config) ->
            return $ Right config

        Right resp ->
            return $ Left $ DaemonError $
                "Invalid response type for config request: " <> T.pack (show resp)

-- | Render BuildId to Text
renderBuildId :: BuildId -> Text
renderBuildId (BuildId u) = "build-" <> T.pack (show (hashUnique u))
renderBuildId (BuildIdFromInt n) = "build-" <> T.pack (show n)
