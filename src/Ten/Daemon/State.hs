{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Ten.Daemon.State (
    -- Core state types
    DaemonState(..),
    StateError(..),

    -- State initialization and persistence
    initDaemonState,
    loadStateFromFile,
    saveStateToFile,

    -- Active build tracking
    ActiveBuild(..),
    BuildQueue(..),
    registerBuild,
    unregisterBuild,
    updateBuildStatus,
    appendBuildLog,
    getBuildStatus,
    getBuildLog,
    listActiveBuilds,
    listQueuedBuilds,

    -- Return-continuation tracking
    registerReturnedDerivation,
    getDerivationChain,
    isDerivationInChain,

    -- Store path locking
    acquirePathLock,
    releasePathLock,
    withPathLock,

    -- Path reachability tracking
    markPathAsReachable,
    isPathReachable,
    getReachablePaths,

    -- Build graph management
    registerDerivationGraph,
    getDerivationGraph,
    getTransitiveDependencies,

    -- Garbage collection coordination
    acquireGCLock,
    releaseGCLock,
    withGCLock,

    -- System statistics
    DaemonStats(..),
    updateSystemStats,
    getDaemonStats,

    -- State query functions
    hasPendingBuilds,
    hasActiveBuild,
    getNextBuildToSchedule,

    -- Background maintenance
    pruneCompletedBuilds,
    cleanupStaleBuilds,
    scheduledMaintenance,

    -- Atomic operations
    atomicallyState,

    -- State management utilities
    withState,
    modifyState,
    readState,

    -- Build status reporting
    captureStatus,
    calculateBuildProgress
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (Exception, throwIO, try, catch, finally, bracket, SomeException, IOException)
import Control.Monad (when, unless, forM, forM_, void, foldM)
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.List (sort, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import System.Directory (doesFileExist, createDirectoryIfMissing, renameFile)
import System.FilePath (takeDirectory)
import System.IO (Handle, withFile, IOMode(..), hClose)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Resource (ResourceLimit(..), Resource(..), getResourceLimit, setResourceLimit)
import System.Posix.Signals (installHandler, Handler(..), sigTERM)
import System.Posix.Process (getProcessID)
import Data.List (sortBy)
import Data.Ord (comparing)
import System.Posix.Signals (Signal)
import System.Posix.Types (ProcessID)
import qualified System.Posix.Signals as Signals
import System.Posix.Sysconf (getSysconfVar, SysconfVar(..))
import System.Posix.Files (fileSize, getFileStatus)
import System.Posix.Memory (sysconfPageSize)
import Control.Concurrent.Async (Async, async, cancel)

-- Import Ten modules
import Ten.Core (BuildId(..), BuildStatus(..), BuildError(..), StorePath(..), UserId(..),
                 AuthToken(..), BuildState(..), BuildStrategy(..),
                 BuildEnv(..), runTen, Phase(..))
import Ten.Derivation (Derivation, DerivationInput, DerivationOutput,
                      derivationEquals, derivHash, hashDerivation)
import Ten.Build (BuildResult(..))
import qualified Ten.Graph as Graph

-- | State errors
data StateError
    = StateBuildNotFound BuildId
    | StatePathNotFound StorePath
    | StateDerivationNotFound Text
    | StateIOError IOException
    | StateLockError Text
    | StateFormatError Text
    | StateVersionError Text
    | StateResourceError Text
    deriving (Show, Eq)

instance Exception StateError

-- | Active build information
data ActiveBuild = ActiveBuild {
    abBuildId :: BuildId,
    abDerivation :: Derivation,
    abOwner :: UserId,
    abStatus :: TVar BuildStatus,
    abStartTime :: UTCTime,
    abUpdateTime :: TVar UTCTime,
    abLogBuffer :: TVar Text,
    abResult :: TMVar (Either BuildError BuildResult),
    abResourceUsage :: TVar (Map Text Double),
    abDerivationChain :: TVar [Derivation],  -- Chain for return-continuation
    abProcessId :: TVar (Maybe ProcessID),
    abTimeout :: Maybe Int,  -- Timeout in seconds
    abThread :: TVar (Maybe (Async ()))  -- Reference to build thread
} deriving (Eq)

instance Show ActiveBuild where
    show ActiveBuild{..} = "ActiveBuild { id = " ++ show abBuildId ++
                          ", derivation = " ++ show (derivHash abDerivation) ++
                          ", owner = " ++ show abOwner ++ " }"

-- | Build queue entry
data BuildQueueEntry = BuildQueueEntry {
    bqBuildId :: BuildId,
    bqDerivation :: Derivation,
    bqOwner :: UserId,
    bqPriority :: Int,
    bqEnqueueTime :: UTCTime,
    bqDependencies :: Set BuildId  -- Dependencies that must complete first
} deriving (Show, Eq)

-- | Build queue
data BuildQueue = BuildQueue {
    bqEntries :: TVar [BuildQueueEntry],
    bqMaxPending :: Int  -- Maximum number of pending builds
} deriving (Eq)

instance Show BuildQueue where
    show BuildQueue{..} = "BuildQueue { maxPending = " ++ show bqMaxPending ++ " }"

-- | Comprehensive daemon state
data DaemonState = DaemonState {
    -- Build tracking
    dsActiveBuilds :: TVar (Map BuildId ActiveBuild),
    dsBuildQueue :: BuildQueue,
    dsCompletedBuilds :: TVar (Map BuildId (UTCTime, Either BuildError BuildResult)),
    dsFailedBuilds :: TVar (Map BuildId (UTCTime, BuildError)),

    -- Store management
    dsPathLocks :: TVar (Map StorePath (TMVar ())),
    dsReachablePaths :: TVar (Set StorePath),

    -- Derivation tracking
    dsKnownDerivations :: TVar (Map Text Derivation),
    dsDerivationGraphs :: TVar (Map Text Graph.BuildGraph),

    -- Garbage collection
    dsGCLock :: TMVar (),
    dsLastGC :: TVar (Maybe UTCTime),
    dsGCStats :: TVar (UTCTime, Int, Int, Integer),  -- Last GC time, collected, kept, bytes freed

    -- System statistics
    dsDaemonStats :: TVar DaemonStats,
    dsStartTime :: UTCTime,

    -- Background tasks
    dsMaintenanceThread :: TVar (Maybe (Async ())),

    -- Configuration
    dsStateFilePath :: FilePath,
    dsMaxCompletedBuilds :: Int,
    dsMaxFailedBuilds :: Int,
    dsMaxConcurrentBuilds :: Int
} deriving (Eq)

-- | System statistics
data DaemonStats = DaemonStats {
    statsBuildsTotalCompleted :: Int,
    statsBuildsTotalFailed :: Int,
    statsCPUUsage :: Double,  -- CPU usage (0-100%)
    statsMemoryUsage :: Integer,  -- Memory usage in bytes
    statsStoreSize :: Integer,  -- Store size in bytes
    statsStoreEntries :: Int,  -- Number of store entries
    statsSystemLoad :: [Double],  -- Load averages (1, 5, 15 min)
    statsLastUpdated :: UTCTime
} deriving (Show, Eq)

-- | Initialize a new daemon state
initDaemonState :: FilePath -> Int -> Int -> IO DaemonState
initDaemonState stateFile maxJobs maxHistory = do
    now <- getCurrentTime

    activeBuildsVar <- newTVarIO Map.empty
    buildQueueEntriesVar <- newTVarIO []
    completedBuildsVar <- newTVarIO Map.empty
    failedBuildsVar <- newTVarIO Map.empty

    pathLocksVar <- newTVarIO Map.empty
    reachablePathsVar <- newTVarIO Set.empty

    knownDerivationsVar <- newTVarIO Map.empty
    derivationGraphsVar <- newTVarIO Map.empty

    gcLockVar <- newTMVarIO ()
    lastGCVar <- newTVarIO Nothing
    gcStatsVar <- newTVarIO (now, 0, 0, 0)

    let buildQueue = BuildQueue {
            bqEntries = buildQueueEntriesVar,
            bqMaxPending = maxJobs * 10  -- Allow 10x max jobs in queue
        }

    -- Initialize system stats
    initialStats <- captureSystemStats
    statsVar <- newTVarIO initialStats

    maintenanceThreadVar <- newTVarIO Nothing

    -- Create state object
    let state = DaemonState {
            dsActiveBuilds = activeBuildsVar,
            dsBuildQueue = buildQueue,
            dsCompletedBuilds = completedBuildsVar,
            dsFailedBuilds = failedBuildsVar,

            dsPathLocks = pathLocksVar,
            dsReachablePaths = reachablePathsVar,

            dsKnownDerivations = knownDerivationsVar,
            dsDerivationGraphs = derivationGraphsVar,

            dsGCLock = gcLockVar,
            dsLastGC = lastGCVar,
            dsGCStats = gcStatsVar,

            dsDaemonStats = statsVar,
            dsStartTime = now,

            dsMaintenanceThread = maintenanceThreadVar,

            dsStateFilePath = stateFile,
            dsMaxCompletedBuilds = maxHistory,
            dsMaxFailedBuilds = maxHistory,
            dsMaxConcurrentBuilds = maxJobs
        }

    -- Set up maintenance thread
    setupMaintenanceThread state

    -- Set up signal handlers
    setupSignalHandlers state

    return state

-- | Load daemon state from a file
loadStateFromFile :: FilePath -> Int -> Int -> IO DaemonState
loadStateFromFile stateFile maxJobs maxHistory = do
    -- Check if the file exists
    exists <- doesFileExist stateFile
    if not exists
        then do
            -- Create a new state if the file doesn't exist
            initDaemonState stateFile maxJobs maxHistory
        else do
            -- Try to load the state from the file
            result <- try $ do
                content <- BS.readFile stateFile
                case Aeson.eitherDecodeStrict content of
                    Left err -> throwIO $ StateError $ StateFormatError $ T.pack err
                    Right stateData -> do
                        -- Initialize a new state
                        state <- initDaemonState stateFile maxJobs maxHistory

                        -- Populate it with the loaded data
                        populateState state stateData

                        return state

            case result of
                Left (err :: SomeException) -> do
                    -- If anything goes wrong, create a fresh state
                    putStrLn $ "Error loading state: " ++ show err
                    initDaemonState stateFile maxJobs maxHistory
                Right state -> return state

-- | Populate state from loaded data
populateState :: DaemonState -> Aeson.Value -> IO ()
populateState state jsonData = do
    -- Parse completed builds
    case Aeson.parseEither extractCompletedBuilds jsonData of
        Left _ -> return ()  -- Skip if not found or invalid
        Right completedBuilds -> do
            atomically $ writeTVar (dsCompletedBuilds state) completedBuilds

    -- Parse failed builds
    case Aeson.parseEither extractFailedBuilds jsonData of
        Left _ -> return ()  -- Skip if not found or invalid
        Right failedBuilds -> do
            atomically $ writeTVar (dsFailedBuilds state) failedBuilds

    -- Parse reachable paths
    case Aeson.parseEither extractReachablePaths jsonData of
        Left _ -> return ()  -- Skip if not found or invalid
        Right reachablePaths -> do
            atomically $ writeTVar (dsReachablePaths state) reachablePaths

    -- Parse known derivations
    case Aeson.parseEither extractKnownDerivations jsonData of
        Left _ -> return ()  -- Skip if not found or invalid
        Right knownDerivations -> do
            atomically $ writeTVar (dsKnownDerivations state) knownDerivations

    -- Parse last GC time
    case Aeson.parseEither extractLastGC jsonData of
        Left _ -> return ()  -- Skip if not found or invalid
        Right lastGC -> do
            atomically $ writeTVar (dsLastGC state) lastGC

    -- Parse GC stats
    case Aeson.parseEither extractGCStats jsonData of
        Left _ -> return ()  -- Skip if not found or invalid
        Right gcStats -> do
            atomically $ writeTVar (dsGCStats state) gcStats
  where
    extractCompletedBuilds = Aeson.withObject "StateData" $ \v -> do
        completedMap <- v .: "completedBuilds"
        -- In a real implementation, parse the build results properly
        return completedMap

    extractFailedBuilds = Aeson.withObject "StateData" $ \v -> do
        failedMap <- v .: "failedBuilds"
        -- In a real implementation, parse the failed builds properly
        return failedMap

    extractReachablePaths = Aeson.withObject "StateData" $ \v -> do
        paths <- v .: "reachablePaths"
        -- In a real implementation, parse the paths properly
        return paths

    extractKnownDerivations = Aeson.withObject "StateData" $ \v -> do
        derivations <- v .: "knownDerivations"
        -- In a real implementation, parse the derivations properly
        return derivations

    extractLastGC = Aeson.withObject "StateData" $ \v -> do
        lastGC <- v .: "lastGC"
        return lastGC

    extractGCStats = Aeson.withObject "StateData" $ \v -> do
        gcStats <- v .: "gcStats"
        return gcStats

-- | Save daemon state to a file
saveStateToFile :: DaemonState -> IO ()
saveStateToFile state = do
    -- Create the directory if it doesn't exist
    createDirectoryIfMissing True (takeDirectory (dsStateFilePath state))

    -- Capture the current state
    stateData <- captureStateData state

    -- Write to a temporary file first
    let tempFile = dsStateFilePath state ++ ".tmp"
    BS.writeFile tempFile (LBS.toStrict $ Aeson.encode stateData)

    -- Atomically rename to the target file
    renameFile tempFile (dsStateFilePath state)

-- | Capture the current state as JSON-serializable data
captureStateData :: DaemonState -> IO Aeson.Value
captureStateData state = do
    -- Read all relevant state data atomically
    (completedBuilds, failedBuilds, reachablePaths, knownDerivations, lastGC, gcStats) <-
        atomically $ do
            completedBuilds <- readTVar (dsCompletedBuilds state)
            failedBuilds <- readTVar (dsFailedBuilds state)
            reachablePaths <- readTVar (dsReachablePaths state)
            knownDerivations <- readTVar (dsKnownDerivations state)
            lastGC <- readTVar (dsLastGC state)
            gcStats <- readTVar (dsGCStats state)
            return (completedBuilds, failedBuilds, reachablePaths, knownDerivations, lastGC, gcStats)

    -- Build the state data object
    return $ Aeson.object [
            "version" .= ("1.0" :: Text),
            "timestamp" .= getCurrentTime,
            "completedBuilds" .= completedBuilds,
            "failedBuilds" .= failedBuilds,
            "reachablePaths" .= reachablePaths,
            "knownDerivations" .= knownDerivations,
            "lastGC" .= lastGC,
            "gcStats" .= gcStats
        ]

-- | Register a new build
registerBuild :: DaemonState -> Derivation -> UserId -> Int -> Maybe Int -> IO BuildId
registerBuild state derivation owner priority timeout = do
    -- Generate a new build ID
    buildId <- BuildId <$> newUnique

    now <- getCurrentTime

    -- Check if we're at capacity for concurrent builds
    activeBuildCount <- atomically $ Map.size <$> readTVar (dsActiveBuilds state)

    if activeBuildCount >= dsMaxConcurrentBuilds state
        then do
            -- Queue the build
            queueBuild state buildId derivation owner priority now Set.empty
            return buildId
        else do
            -- Create a new active build
            statusVar <- newTVarIO BuildPending
            updateTimeVar <- newTVarIO now
            logBufferVar <- newTVarIO ""
            resultVar <- newEmptyTMVarIO
            resourceUsageVar <- newTVarIO Map.empty
            derivationChainVar <- newTVarIO [derivation]
            processIdVar <- newTVarIO Nothing
            threadVar <- newTVarIO Nothing

            let activeBuild = ActiveBuild {
                    abBuildId = buildId,
                    abDerivation = derivation,
                    abOwner = owner,
                    abStatus = statusVar,
                    abStartTime = now,
                    abUpdateTime = updateTimeVar,
                    abLogBuffer = logBufferVar,
                    abResult = resultVar,
                    abResourceUsage = resourceUsageVar,
                    abDerivationChain = derivationChainVar,
                    abProcessId = processIdVar,
                    abTimeout = timeout,
                    abThread = threadVar
                }

            -- Register the build
            atomically $ modifyTVar' (dsActiveBuilds state) $ Map.insert buildId activeBuild

            -- Register the derivation
            atomically $ modifyTVar' (dsKnownDerivations state) $
                Map.insert (derivHash derivation) derivation

            return buildId

-- | Queue a build for later execution
queueBuild :: DaemonState -> BuildId -> Derivation -> UserId -> Int -> UTCTime -> Set BuildId -> IO ()
queueBuild state buildId derivation owner priority timestamp dependencies = do
    -- Check if we're at queue capacity
    queueSize <- atomically $ length <$> readTVar (bqEntries $ dsBuildQueue state)

    if queueSize >= bqMaxPending (dsBuildQueue state)
        then throwIO $ StateError $ StateResourceError "Build queue is full"
        else do
            -- Create a queue entry
            let entry = BuildQueueEntry {
                    bqBuildId = buildId,
                    bqDerivation = derivation,
                    bqOwner = owner,
                    bqPriority = priority,
                    bqEnqueueTime = timestamp,
                    bqDependencies = dependencies
                }

            -- Add to the queue
            atomically $ modifyTVar' (bqEntries $ dsBuildQueue state) $ \entries ->
                sortQueueEntries (entry : entries)

-- | Sort queue entries by priority and enqueue time
sortQueueEntries :: [BuildQueueEntry] -> [BuildQueueEntry]
sortQueueEntries = sortBy (comparing (\e -> (-bqPriority e, bqEnqueueTime e)))

-- | Unregister a build
unregisterBuild :: DaemonState -> BuildId -> IO ()
unregisterBuild state buildId = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> do
            -- Check if it's in the queue
            entries <- atomically $ readTVar (bqEntries $ dsBuildQueue state)
            let queueEntry = filter (\e -> bqBuildId e == buildId) entries

            if null queueEntry
                then throwIO $ StateError $ StateBuildNotFound buildId
                else do
                    -- Remove from queue
                    atomically $ modifyTVar' (bqEntries $ dsBuildQueue state) $
                        filter (\e -> bqBuildId e /= buildId)

        Just build -> do
            -- Cancel the build thread if it's running
            threadRef <- atomically $ readTVar (abThread build)
            case threadRef of
                Just thread -> cancel thread
                Nothing -> return ()

            -- Remove from active builds
            atomically $ modifyTVar' (dsActiveBuilds state) $ Map.delete buildId

            -- Check for build result
            resultEmpty <- atomically $ isEmptyTMVar (abResult build)

            unless resultEmpty $ do
                -- Get the result
                result <- atomically $ readTMVar (abResult build)
                now <- getCurrentTime

                case result of
                    Right buildResult -> do
                        -- Add to completed builds
                        atomically $ modifyTVar' (dsCompletedBuilds state) $
                            Map.insert buildId (now, result)

                        -- Update stats
                        atomically $ modifyTVar' (dsDaemonStats state) $ \stats ->
                            stats { statsBuildsTotalCompleted = statsBuildsTotalCompleted stats + 1 }

                    Left err -> do
                        -- Add to failed builds
                        atomically $ modifyTVar' (dsFailedBuilds state) $
                            Map.insert buildId (now, err)

                        -- Update stats
                        atomically $ modifyTVar' (dsDaemonStats state) $ \stats ->
                            stats { statsBuildsTotalFailed = statsBuildsTotalFailed stats + 1 }

            -- Prune old builds if necessary
            pruneCompletedBuilds state

            -- Schedule next build from queue if any
            tryScheduleNextBuild state

-- | Update a build's status
updateBuildStatus :: DaemonState -> BuildId -> BuildStatus -> IO ()
updateBuildStatus state buildId status = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Update the status and timestamp
            now <- getCurrentTime
            atomically $ do
                writeTVar (abStatus build) status
                writeTVar (abUpdateTime build) now

-- | Append to a build's log
appendBuildLog :: DaemonState -> BuildId -> Text -> IO ()
appendBuildLog state buildId logText = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Append to the log
            atomically $ modifyTVar' (abLogBuffer build) $ \log -> log <> logText

-- | Get a build's status
getBuildStatus :: DaemonState -> BuildId -> IO BuildStatus
getBuildStatus state buildId = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> do
            -- Check if it's a completed build
            completedBuilds <- atomically $ readTVar (dsCompletedBuilds state)
            case Map.lookup buildId completedBuilds of
                Just _ -> return BuildCompleted
                Nothing -> do
                    -- Check if it's a failed build
                    failedBuilds <- atomically $ readTVar (dsFailedBuilds state)
                    case Map.lookup buildId failedBuilds of
                        Just _ -> return BuildFailed'
                        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId

        Just build -> atomically $ readTVar (abStatus build)

-- | Get a build's log
getBuildLog :: DaemonState -> BuildId -> IO Text
getBuildLog state buildId = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> do
            -- Check if it's a completed or failed build
            -- In a real implementation, we would store the logs persistently
            -- For now, just return a message
            completedBuilds <- atomically $ readTVar (dsCompletedBuilds state)
            failedBuilds <- atomically $ readTVar (dsFailedBuilds state)

            if Map.member buildId completedBuilds
                then return "Build completed successfully. Log no longer available."
                else if Map.member buildId failedBuilds
                    then return "Build failed. Log no longer available."
                    else throwIO $ StateError $ StateBuildNotFound buildId

        Just build -> atomically $ readTVar (abLogBuffer build)

-- | List active builds
listActiveBuilds :: DaemonState -> IO [(BuildId, Derivation, BuildStatus, UTCTime, Double)]
listActiveBuilds state = do
    -- Get all active builds
    activeBuilds <- atomically $ readTVar (dsActiveBuilds state)

    -- Collect build information
    mapM collectBuildInfo (Map.toList activeBuilds)
  where
    collectBuildInfo (buildId, build) = do
        status <- atomically $ readTVar (abStatus build)
        updateTime <- atomically $ readTVar (abUpdateTime build)

        -- Calculate progress
        progress <- calculateBuildProgress build

        return (buildId, abDerivation build, status, updateTime, progress)

-- | List queued builds
listQueuedBuilds :: DaemonState -> IO [(BuildId, Derivation, Int, UTCTime)]
listQueuedBuilds state = do
    -- Get all queued builds
    queueEntries <- atomically $ readTVar (bqEntries $ dsBuildQueue state)

    -- Return information
    return $ map (\entry -> (bqBuildId entry, bqDerivation entry, bqPriority entry, bqEnqueueTime entry)) queueEntries

-- | Register a returned derivation for a build
registerReturnedDerivation :: DaemonState -> BuildId -> Derivation -> IO Bool
registerReturnedDerivation state buildId innerDerivation = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Check for cycle
            inChain <- isDerivationInChain state buildId innerDerivation

            if inChain
                then return False  -- Cycle detected
                else do
                    -- Add derivation to the chain
                    atomically $ modifyTVar' (abDerivationChain build) (innerDerivation :)

                    -- Register the derivation
                    atomically $ modifyTVar' (dsKnownDerivations state) $
                        Map.insert (derivHash innerDerivation) innerDerivation

                    -- Update build status
                    let innerBuildId = BuildId $ error "Inner build ID not available yet"
                    updateBuildStatus state buildId (BuildRecursing innerBuildId)

                    return True

-- | Get the derivation chain for a build
getDerivationChain :: DaemonState -> BuildId -> IO [Derivation]
getDerivationChain state buildId = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> atomically $ readTVar (abDerivationChain build)

-- | Check if a derivation is in a build's chain
isDerivationInChain :: DaemonState -> BuildId -> Derivation -> IO Bool
isDerivationInChain state buildId derivation = do
    -- Get the chain
    chain <- getDerivationChain state buildId

    -- Check if the derivation is in the chain
    return $ any (derivationEquals derivation) chain

-- | Acquire a lock for a store path
acquirePathLock :: DaemonState -> StorePath -> IO ()
acquirePathLock state path = do
    -- Get or create a lock for this path
    lock <- atomically $ do
        locks <- readTVar (dsPathLocks state)
        case Map.lookup path locks of
            Just lock -> return lock
            Nothing -> do
                lock <- newTMVar ()
                modifyTVar' (dsPathLocks state) $ Map.insert path lock
                return lock

    -- Try to acquire the lock
    taken <- atomically $ isEmptyTMVar lock
    unless taken $ do
        atomically $ takeTMVar lock

-- | Release a lock for a store path
releasePathLock :: DaemonState -> StorePath -> IO ()
releasePathLock state path = do
    -- Get the lock
    mLock <- atomically $ Map.lookup path <$> readTVar (dsPathLocks state)

    case mLock of
        Nothing -> return ()  -- Nothing to release
        Just lock -> do
            -- Check if the lock is taken
            empty <- atomically $ isEmptyTMVar lock

            unless empty $ do
                -- Release the lock
                atomically $ putTMVar lock ()

-- | Execute an action with a locked store path
withPathLock :: DaemonState -> StorePath -> IO a -> IO a
withPathLock state path action = do
    bracket
        (acquirePathLock state path)
        (\_ -> releasePathLock state path)
        (\_ -> action)

-- | Mark a path as reachable
markPathAsReachable :: DaemonState -> StorePath -> IO ()
markPathAsReachable state path = do
    atomically $ modifyTVar' (dsReachablePaths state) $ Set.insert path

-- | Check if a path is reachable
isPathReachable :: DaemonState -> StorePath -> IO Bool
isPathReachable state path = do
    atomically $ Set.member path <$> readTVar (dsReachablePaths state)

-- | Get all reachable paths
getReachablePaths :: DaemonState -> IO (Set StorePath)
getReachablePaths state = do
    atomically $ readTVar (dsReachablePaths state)

-- | Register a derivation's build graph
registerDerivationGraph :: DaemonState -> Derivation -> Graph.BuildGraph -> IO ()
registerDerivationGraph state derivation graph = do
    atomically $ modifyTVar' (dsDerivationGraphs state) $
        Map.insert (derivHash derivation) graph

-- | Get a derivation's build graph
getDerivationGraph :: DaemonState -> Derivation -> IO (Maybe Graph.BuildGraph)
getDerivationGraph state derivation = do
    atomically $ Map.lookup (derivHash derivation) <$> readTVar (dsDerivationGraphs state)

-- | Get transitive dependencies of a derivation
getTransitiveDependencies :: DaemonState -> Derivation -> IO (Set Text)
getTransitiveDependencies state derivation = do
    -- Get the build graph
    mGraph <- getDerivationGraph state derivation

    case mGraph of
        Nothing -> return Set.empty
        Just graph -> do
            -- In a real implementation, we would use the graph to compute
            -- the transitive closure of dependencies
            -- For now, just return an empty set
            return Set.empty

-- | Acquire the GC lock
acquireGCLock :: DaemonState -> IO ()
acquireGCLock state = do
    -- Try to take the GC lock
    taken <- atomically $ tryTakeTMVar (dsGCLock state)
    unless taken $
        throwIO $ StateError $ StateLockError "GC already in progress"

-- | Release the GC lock
releaseGCLock :: DaemonState -> IO ()
releaseGCLock state = do
    -- Release the GC lock
    atomically $ putTMVar (dsGCLock state) ()

-- | Execute an action with the GC lock
withGCLock :: DaemonState -> IO a -> IO a
withGCLock state action = do
    bracket
        (acquireGCLock state)
        (\_ -> releaseGCLock state)
        (\_ -> action)

-- | Update system statistics
updateSystemStats :: DaemonState -> IO ()
updateSystemStats state = do
    stats <- captureSystemStats
    atomically $ writeTVar (dsDaemonStats state) stats

-- | Get daemon statistics
getDaemonStats :: DaemonState -> IO DaemonStats
getDaemonStats state = do
    atomically $ readTVar (dsDaemonStats state)

-- | Capture current system statistics
captureSystemStats :: IO DaemonStats
captureSystemStats = do
    now <- getCurrentTime

    -- In a real implementation, we would use system calls to get actual stats
    -- For now, just provide placeholder values

    -- Try to get CPU usage
    cpuUsage <- getCPUUsage

    -- Try to get memory usage
    memoryUsage <- getMemoryUsage

    -- Try to get store size
    (storeSize, storeEntries) <- (0, 0) -- In a real implementation, query the store

    -- Try to get load averages
    loadAvg <- getLoadAverages

    return DaemonStats {
        statsBuildsTotalCompleted = 0,
        statsBuildsTotalFailed = 0,
        statsCPUUsage = cpuUsage,
        statsMemoryUsage = memoryUsage,
        statsStoreSize = storeSize,
        statsStoreEntries = storeEntries,
        statsSystemLoad = loadAvg,
        statsLastUpdated = now
    }

-- | Get CPU usage (0-100%)
getCPUUsage :: IO Double
getCPUUsage = do
    -- In a real implementation, this would read /proc/stat
    -- For now, just return a placeholder
    return 0.0

-- | Get memory usage in bytes
getMemoryUsage :: IO Integer
getMemoryUsage = do
    -- In a real implementation, this would read /proc/self/status
    -- For now, just return a placeholder
    return 0

-- | Get load averages
getLoadAverages :: IO [Double]
getLoadAverages = do
    -- In a real implementation, this would use getloadavg()
    -- For now, just return placeholders
    return [0.0, 0.0, 0.0]

-- | Check if there are pending builds
hasPendingBuilds :: DaemonState -> IO Bool
hasPendingBuilds state = do
    queueEntries <- atomically $ readTVar (bqEntries $ dsBuildQueue state)
    return $ not $ null queueEntries

-- | Check if a specific build is active
hasActiveBuild :: DaemonState -> BuildId -> IO Bool
hasActiveBuild state buildId = do
    activeBuilds <- atomically $ readTVar (dsActiveBuilds state)
    return $ Map.member buildId activeBuilds

-- | Get the next build to schedule
getNextBuildToSchedule :: DaemonState -> IO (Maybe BuildQueueEntry)
getNextBuildToSchedule state = do
    -- Check if we're at capacity
    activeBuildCount <- atomically $ Map.size <$> readTVar (dsActiveBuilds state)

    if activeBuildCount >= dsMaxConcurrentBuilds state
        then return Nothing
        else do
            -- Get queue entries
            queueEntries <- atomically $ readTVar (bqEntries $ dsBuildQueue state)

            if null queueEntries
                then return Nothing
                else do
                    -- Find the first entry with all dependencies satisfied
                    findReadyEntry queueEntries
  where
    findReadyEntry [] = return Nothing
    findReadyEntry (entry:rest) = do
        -- Check if all dependencies are satisfied
        deps <- forM (Set.toList $ bqDependencies entry) $ \depId -> do
            status <- try $ getBuildStatus state depId
            case status of
                Left (_ :: StateError) -> return False
                Right BuildCompleted -> return True
                Right _ -> return False

        if all id deps
            then return $ Just entry
            else findReadyEntry rest

-- | Try to schedule the next build from the queue
tryScheduleNextBuild :: DaemonState -> IO ()
tryScheduleNextBuild state = do
    -- Get the next build to schedule
    mEntry <- getNextBuildToSchedule state

    case mEntry of
        Nothing -> return ()  -- No ready builds
        Just entry -> do
            -- Remove from queue
            atomically $ modifyTVar' (bqEntries $ dsBuildQueue state) $
                filter (\e -> bqBuildId e /= bqBuildId entry)

            -- Register as active build
            now <- getCurrentTime
            statusVar <- newTVarIO BuildPending
            updateTimeVar <- newTVarIO now
            logBufferVar <- newTVarIO ""
            resultVar <- newEmptyTMVarIO
            resourceUsageVar <- newTVarIO Map.empty
            derivationChainVar <- newTVarIO [bqDerivation entry]
            processIdVar <- newTVarIO Nothing
            threadVar <- newTVarIO Nothing

            let activeBuild = ActiveBuild {
                    abBuildId = bqBuildId entry,
                    abDerivation = bqDerivation entry,
                    abOwner = bqOwner entry,
                    abStatus = statusVar,
                    abStartTime = now,
                    abUpdateTime = updateTimeVar,
                    abLogBuffer = logBufferVar,
                    abResult = resultVar,
                    abResourceUsage = resourceUsageVar,
                    abDerivationChain = derivationChainVar,
                    abProcessId = processIdVar,
                    abTimeout = Nothing,
                    abThread = threadVar
                }

            -- Register the build
            atomically $ modifyTVar' (dsActiveBuilds state) $
                Map.insert (bqBuildId entry) activeBuild

-- | Prune old completed builds to stay within limits
pruneCompletedBuilds :: DaemonState -> IO ()
pruneCompletedBuilds state = do
    -- Get current completed and failed builds
    completedBuilds <- atomically $ readTVar (dsCompletedBuilds state)
    failedBuilds <- atomically $ readTVar (dsFailedBuilds state)

    -- Check if we need to prune
    when (Map.size completedBuilds > dsMaxCompletedBuilds state) $ do
        -- Sort by timestamp
        let sorted = sortBy (comparing $ \(_, (time, _)) -> time) $ Map.toList completedBuilds

        -- Keep only the most recent ones
        let toKeep = take (dsMaxCompletedBuilds state) sorted
        let kept = Map.fromList toKeep

        -- Update the state
        atomically $ writeTVar (dsCompletedBuilds state) kept

    -- Same for failed builds
    when (Map.size failedBuilds > dsMaxFailedBuilds state) $ do
        -- Sort by timestamp
        let sorted = sortBy (comparing $ \(_, (time, _)) -> time) $ Map.toList failedBuilds

        -- Keep only the most recent ones
        let toKeep = take (dsMaxFailedBuilds state) sorted
        let kept = Map.fromList toKeep

        -- Update the state
        atomically $ writeTVar (dsFailedBuilds state) kept

-- | Clean up stale builds (e.g., timed out or crashed)
cleanupStaleBuilds :: DaemonState -> IO ()
cleanupStaleBuilds state = do
    now <- getCurrentTime

    -- Get all active builds
    activeBuilds <- atomically $ readTVar (dsActiveBuilds state)

    -- Check each build for staleness
    forM_ (Map.toList activeBuilds) $ \(buildId, build) -> do
        -- Check if the build has timed out
        case abTimeout build of
            Just timeout -> do
                let timeoutTime = addUTCTime (fromIntegral timeout) (abStartTime build)
                when (now > timeoutTime) $ do
                    -- Mark as failed and remove
                    let err = BuildError $ BuildFailed "Build timed out"
                    atomically $ putTMVar (abResult build) (Left err)
                    unregisterBuild state buildId

            Nothing -> do
                -- Check for last update time
                lastUpdate <- atomically $ readTVar (abUpdateTime build)
                let idleTime = diffUTCTime now lastUpdate

                -- If idle for more than 1 hour, consider it stale
                when (idleTime > 3600) $ do
                    -- Mark as failed and remove
                    let err = BuildError $ BuildFailed "Build stalled or crashed"
                    atomically $ putTMVar (abResult build) (Left err)
                    unregisterBuild state buildId

-- | Setup periodic maintenance
setupMaintenanceThread :: DaemonState -> IO ()
setupMaintenanceThread state = do
    -- Create a thread that runs maintenance tasks periodically
    thread <- async $ maintenanceLoop state

    -- Store the thread
    atomically $ writeTVar (dsMaintenanceThread state) (Just thread)

-- | Maintenance loop
maintenanceLoop :: DaemonState -> IO ()
maintenanceLoop state = do
    -- Run forever
    forever $ do
        -- Run maintenance tasks
        scheduledMaintenance state

        -- Sleep for a while
        threadDelay (60 * 1000 * 1000)  -- 60 seconds

-- | Run scheduled maintenance tasks
scheduledMaintenance :: DaemonState -> IO ()
scheduledMaintenance state = do
    -- Update system stats
    updateSystemStats state

    -- Clean up stale builds
    cleanupStaleBuilds state

    -- Prune old completed builds
    pruneCompletedBuilds state

    -- Save state to file
    saveStateToFile state

-- | Set up signal handlers
setupSignalHandlers :: DaemonState -> IO ()
setupSignalHandlers state = do
    -- Handle SIGTERM to gracefully shut down
    void $ installHandler Signals.sigTERM (Catch $ handleTermSignal state) Nothing

    -- Handle SIGINT (Ctrl+C)
    void $ installHandler Signals.sigINT (Catch $ handleTermSignal state) Nothing

    -- Handle SIGHUP for config reload
    void $ installHandler Signals.sigHUP (Catch $ handleHupSignal state) Nothing

-- | Handle SIGTERM signal
handleTermSignal :: DaemonState -> IO ()
handleTermSignal state = do
    -- Save state
    putStrLn "Received termination signal, shutting down..."
    saveStateToFile state

    -- Cancel maintenance thread
    mThread <- atomically $ readTVar (dsMaintenanceThread state)
    case mThread of
        Just thread -> cancel thread
        Nothing -> return ()

    -- Exit
    putStrLn "Shutdown complete."
    exitSuccess

-- | Handle SIGHUP signal
handleHupSignal :: DaemonState -> IO ()
handleHupSignal state = do
    -- Save and reload state
    putStrLn "Received HUP signal, reloading..."
    saveStateToFile state

    -- Update system stats
    updateSystemStats state

    putStrLn "Reload complete."

-- | Execute an STM transaction in the context of daemon state
atomicallyState :: STM a -> IO a
atomicallyState = atomically

-- | Execute an action with daemon state
withState :: DaemonState -> (DaemonState -> IO a) -> IO a
withState state action = action state

-- | Modify daemon state
modifyState :: DaemonState -> (DaemonState -> IO (DaemonState, a)) -> IO a
modifyState state f = do
    (newState, result) <- f state
    return result

-- | Read daemon state
readState :: DaemonState -> (DaemonState -> IO a) -> IO a
readState state f = f state

-- | Capture build status information
captureStatus :: DaemonState -> BuildId -> IO (BuildStatus, Double, UTCTime, UTCTime)
captureStatus state buildId = do
    -- Get the build
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Get status
            status <- atomically $ readTVar (abStatus build)

            -- Get update time
            updateTime <- atomically $ readTVar (abUpdateTime build)

            -- Calculate progress
            progress <- calculateBuildProgress build

            return (status, progress, abStartTime build, updateTime)

-- | Calculate build progress
calculateBuildProgress :: ActiveBuild -> IO Double
calculateBuildProgress build = do
    -- Get the status
    status <- atomically $ readTVar (abStatus build)

    case status of
        BuildPending -> return 0.0
        BuildRunning progress -> return progress
        BuildRecursing _ -> return 0.5  -- Assume 50% for returned derivations
        BuildCompleted -> return 1.0
        BuildFailed' -> return 1.0

-- Utility functions and helpers

-- | Generate a unique identifier
newUnique :: IO Integer
newUnique = do
    -- Get the current time as microseconds
    now <- getCurrentTime
    let micros = floor $ 1000000 * realToFrac (diffUTCTime now (read "1970-01-01 00:00:00 UTC"))

    -- Add some randomness
    r <- randomIO :: IO Int

    -- Combine time and random number
    return $ micros * 1000 + fromIntegral (r `mod` 1000)

-- | Microsecond delay
threadDelay :: Int -> IO ()
threadDelay = Control.Concurrent.threadDelay

-- | Exit successfully
exitSuccess :: IO a
exitSuccess = do
    -- Exit the process with success status
    System.Posix.Process.exitImmediately 0
