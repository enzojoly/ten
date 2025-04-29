{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ten.Daemon.State (
    -- Core state types
    DaemonState(..),
    StateError(..),

    -- State initialization and persistence
    initDaemonState,
    loadStateFromFile,
    saveStateToFile,
    populateState,

    -- Privilege-aware state transitions
    withDaemonState,
    asDaemonState,
    asBuilderState,
    transitionStatePrivilege,

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
    checkGCLock,
    tryAcquireGCLock,

    -- System statistics
    DaemonStats(..),
    updateSystemStats,
    getDaemonStats,

    -- State query functions
    hasPendingBuilds,
    hasActiveBuild,
    getNextBuildToSchedule,
    hasBuildCapacity,

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

-- Standard libraries
import Control.Concurrent (forkIO, killThread, ThreadId, threadDelay, myThreadId, MVar, newMVar, takeMVar, putMVar, withMVar)
import Control.Concurrent.STM
import Control.Concurrent.Async (Async, async, cancel, wait, waitCatch)
import Control.Exception (Exception, throwIO, try, catch, finally, bracket, mask, SomeException, IOException, ErrorCall(..))
import Control.Monad (when, unless, forM, forM_, void, foldM, forever)

-- Data structure imports
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BC
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.List (sort, sortOn, sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing, catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Data.Kind (Type)
import Data.Singletons
import Data.Singletons.TH
import Data.Unique (Unique, newUnique)
import System.IO.Unsafe (unsafePerformIO)

-- File system and IO imports
import System.Directory (doesFileExist, createDirectoryIfMissing, renameFile, removeFile)
import System.FilePath (takeDirectory, (</>), takeFileName, normalise)
import System.IO (Handle, withFile, IOMode(..), hClose, hPutStrLn, stderr, stdout, hFlush, BufferMode(..), hSetBuffering)
import System.IO.Error (isDoesNotExistError, catchIOError)

-- POSIX-specific imports
import System.Posix.Types (ProcessID, Fd, FileMode, UserID, GroupID)
import System.Posix.Files (fileExist, getFileStatus, fileSize, setFileMode,
                          setOwnerAndGroup, fileMode, ownerReadMode, ownerWriteMode,
                          ownerExecuteMode, groupReadMode, groupWriteMode, groupExecuteMode)
import System.Posix.Files.ByteString (createLink, removeLink)
import System.Posix.IO (openFd, createFile, closeFd, setLock, getLock,
                       OpenMode(..), defaultFileFlags, FdOption(..),
                       OpenFileFlags(..), trunc, fdToHandle,
                       LockRequest(WriteLock, Unlock))
import System.Posix.Types (FileOffset)
import System.Posix.Process (getProcessID, forkProcess, executeFile,
                           getProcessStatus, ProcessStatus(..), exitImmediately)
import System.Posix.Resource (ResourceLimit(..), Resource(..), getResourceLimit, setResourceLimit)
import System.Posix.Signals (installHandler, Handler(..), sigTERM, sigHUP, sigUSR1, sigINT,
                           signalProcess, Signal, blockSignals, unblockSignals, SignalSet,
                           emptySignalSet, addSignal)
import System.Posix.User (getUserEntryForID, getUserEntryForName, getGroupEntryForName,
                         setUserID, setGroupID, getEffectiveUserID, getRealUserID, userID, groupID)

import Ten.Core (BuildId(..), BuildStatus(..), BuildError(..), StorePath(..),
                UserId(..), AuthToken(..), BuildState(..), BuildStrategy(..),
                Phase(..), PrivilegeTier(..), SPrivilegeTier(..), BuildEnv(..),
                runTen, getGCLockPath, ensureLockDirExists,
                CanAccessStore, CanCreateSandbox, CanModifyStore, CanDropPrivileges, CanAccessDatabase,
                storePathToText, storePathToFilePath,
                -- Add these types directly from Core:
                Derivation(..), DerivationInput(..), DerivationOutput(..), BuildResult(..),
                -- Access AbsoluteSeek via System.IO:
                fromSing, sing)
import qualified Ten.Core as Core
import Ten.Derivation (derivationEquals)
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
    | StatePrivilegeError Text  -- Error when privilege constraints violated
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
    abThread :: TVar (Maybe (Async ())),  -- Reference to build thread
    abPrivilegeTier :: PrivilegeTier  -- Which tier initiated this build
} deriving (Eq)

instance Show ActiveBuild where
    show ActiveBuild{..} = "ActiveBuild { id = " ++ show abBuildId ++
                          ", derivation = " ++ show (derivHash abDerivation) ++
                          ", owner = " ++ show abOwner ++ ", tier = " ++ show abPrivilegeTier ++ " }"

-- | Build queue entry
data BuildQueueEntry = BuildQueueEntry {
    bqBuildId :: BuildId,
    bqDerivation :: Derivation,
    bqOwner :: UserId,
    bqPriority :: Int,
    bqEnqueueTime :: UTCTime,
    bqDependencies :: Set BuildId,  -- Dependencies that must complete first
    bqPrivilegeTier :: PrivilegeTier -- Which tier queued this build
} deriving (Show, Eq)

-- | Build queue
data BuildQueue = BuildQueue {
    bqEntries :: TVar [BuildQueueEntry],
    bqMaxPending :: Int  -- Maximum number of pending builds
} deriving (Eq)

instance Show BuildQueue where
    show BuildQueue{..} = "BuildQueue { maxPending = " ++ show bqMaxPending ++ " }"

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

-- | Comprehensive daemon state with privilege phantom type parameter
data DaemonState (t :: PrivilegeTier) = DaemonState {
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
    dsGCLock :: TMVar (),  -- For backwards compatibility
    dsGCLockOwner :: TVar (Maybe ProcessID),  -- Process ID that owns the GC lock
    dsGCLockPath :: FilePath,  -- Path to the GC lock file
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
    dsMaxConcurrentBuilds :: Int,

    -- Runtime privilege evidence
    dsPrivilegeEvidence :: SPrivilegeTier t  -- Singleton evidence of privilege tier
}

-- Custom equality instance that ignores the privilege evidence
instance Eq (DaemonState t) where
    a == b = (dsStateFilePath a == dsStateFilePath b) &&
             (dsMaxCompletedBuilds a == dsMaxCompletedBuilds b) &&
             (dsMaxFailedBuilds a == dsMaxFailedBuilds b) &&
             (dsMaxConcurrentBuilds a == dsMaxConcurrentBuilds b)

-- | Initialize a new daemon state with explicit privilege tier
initDaemonState :: SPrivilegeTier t -> FilePath -> Int -> Int -> IO (DaemonState t)
initDaemonState st stateFile maxJobs maxHistory = do
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
    gcLockOwnerVar <- newTVarIO Nothing
    let gcLockPath = takeDirectory stateFile </> "gc.lock"
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
            dsGCLockOwner = gcLockOwnerVar,
            dsGCLockPath = gcLockPath,
            dsLastGC = lastGCVar,
            dsGCStats = gcStatsVar,

            dsDaemonStats = statsVar,
            dsStartTime = now,

            dsMaintenanceThread = maintenanceThreadVar,

            dsStateFilePath = stateFile,
            dsMaxCompletedBuilds = maxHistory,
            dsMaxFailedBuilds = maxHistory,
            dsMaxConcurrentBuilds = maxJobs,
            dsPrivilegeEvidence = st
        }

    -- Ensure the lock directory exists
    ensureLockDirExists gcLockPath

    -- Set up maintenance thread (only for Daemon privilege)
    case fromSing st of
        Daemon -> setupMaintenanceThread state
        Builder -> return () -- Builder can't set up maintenance

    -- Set up signal handlers (only for Daemon privilege)
    case fromSing st of
        Daemon -> setupSignalHandlers state
        Builder -> return () -- Builder can't set up signal handlers

    return state

-- | Helper function to work with daemon state with a specific privilege tier
withDaemonState ::
    SPrivilegeTier t -> (forall s. SPrivilegeTier s -> DaemonState s -> IO a) -> DaemonState t -> IO a
withDaemonState st f state = f st state

-- | Convert state to daemon privilege tier if possible (errors if not possible)
asDaemonState :: DaemonState t -> IO (DaemonState 'Daemon)
asDaemonState state = case dsPrivilegeEvidence state of
    SDaemon -> return state
    SBuilder -> throwIO $ StatePrivilegeError "Cannot treat Builder state as Daemon state"

-- | Convert state to builder privilege tier (always safe since we can drop privileges)
asBuilderState :: DaemonState t -> DaemonState 'Builder
asBuilderState state = DaemonState {
    dsActiveBuilds = dsActiveBuilds state,
    dsBuildQueue = dsBuildQueue state,
    dsCompletedBuilds = dsCompletedBuilds state,
    dsFailedBuilds = dsFailedBuilds state,
    dsPathLocks = dsPathLocks state,
    dsReachablePaths = dsReachablePaths state,
    dsKnownDerivations = dsKnownDerivations state,
    dsDerivationGraphs = dsDerivationGraphs state,
    dsGCLock = dsGCLock state,
    dsGCLockOwner = dsGCLockOwner state,
    dsGCLockPath = dsGCLockPath state,
    dsLastGC = dsLastGC state,
    dsGCStats = dsGCStats state,
    dsDaemonStats = dsDaemonStats state,
    dsStartTime = dsStartTime state,
    dsMaintenanceThread = dsMaintenanceThread state,
    dsStateFilePath = dsStateFilePath state,
    dsMaxCompletedBuilds = dsMaxCompletedBuilds state,
    dsMaxFailedBuilds = dsMaxFailedBuilds state,
    dsMaxConcurrentBuilds = dsMaxConcurrentBuilds state,
    dsPrivilegeEvidence = SBuilder
}

-- | Transition state from one privilege tier to another (can only drop privileges)
transitionStatePrivilege :: DaemonState 'Daemon -> DaemonState 'Builder
transitionStatePrivilege = asBuilderState

-- | Load daemon state from a file with proper privilege tier
loadStateFromFile ::
    SPrivilegeTier t -> FilePath -> Int -> Int -> IO (DaemonState t)
loadStateFromFile st stateFile maxJobs maxHistory = do
    -- Check if the file exists
    exists <- doesFileExist stateFile
    if not exists
        then do
            -- Create a new state if the file doesn't exist
            initDaemonState st stateFile maxJobs maxHistory
        else do
            -- Try to load the state from the file
            result <- try $ do
                content <- BS.readFile stateFile
                case Aeson.eitherDecodeStrict content of
                    Left err -> throwIO $ StateError $ StateFormatError $ T.pack err
                    Right stateData -> do
                        -- Initialize a new state
                        state <- initDaemonState st stateFile maxJobs maxHistory

                        -- Populate it with the loaded data (respecting privilege tier)
                        populateState state stateData

                        return state

            case result of
                Left (err :: SomeException) -> do
                    -- If anything goes wrong, create a fresh state
                    putStrLn $ "Error loading state: " ++ show err
                    initDaemonState st stateFile maxJobs maxHistory
                Right state -> return state

-- | Populate state from loaded data (same implementation, but now takes a state with phantom type)
populateState :: DaemonState t -> Aeson.Value -> IO ()
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

-- | Save daemon state to a file - requires daemon privilege for file operations
saveStateToFile :: (CanAccessStore t ~ 'True, CanModifyStore t ~ 'True) => DaemonState t -> IO ()
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
captureStateData :: DaemonState t -> IO Aeson.Value
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

-- | Register a new build - available for both privilege tiers
registerBuild :: DaemonState t -> Derivation -> UserId -> Int -> Maybe Int -> IO BuildId
registerBuild state derivation owner priority timeout = do
    -- Generate a new build ID
    buildId <- BuildId <$> newUnique

    now <- getCurrentTime

    -- Record the privilege tier that initiated this build
    let tier = fromSing $ dsPrivilegeEvidence state

    -- Check if we're at capacity for concurrent builds
    activeBuildCount <- atomically $ Map.size <$> readTVar (dsActiveBuilds state)

    if activeBuildCount >= dsMaxConcurrentBuilds state
        then do
            -- Queue the build
            queueBuild state buildId derivation owner priority now Set.empty tier
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
                    abThread = threadVar,
                    abPrivilegeTier = tier
                }

            -- Register the build
            atomically $ modifyTVar' (dsActiveBuilds state) $ Map.insert buildId activeBuild

            -- Register the derivation
            atomically $ modifyTVar' (dsKnownDerivations state) $
                Map.insert (derivHash derivation) derivation

            return buildId

-- | Queue a build for later execution
queueBuild :: DaemonState t -> BuildId -> Derivation -> UserId -> Int -> UTCTime -> Set BuildId -> PrivilegeTier -> IO ()
queueBuild state buildId derivation owner priority timestamp dependencies tier = do
    -- Check if we're at queue capacity
    queueSize <- atomically $ length <$> readTVar (bqEntries $ dsBuildQueue state)

    if queueSize >= bqMaxPending (dsBuildQueue state)
        then throwIO $ StateError $ StateResourceError "Build queue is full"
        else do
            -- Create a queue entry with privilege tier information
            let entry = BuildQueueEntry {
                    bqBuildId = buildId,
                    bqDerivation = derivation,
                    bqOwner = owner,
                    bqPriority = priority,
                    bqEnqueueTime = timestamp,
                    bqDependencies = dependencies,
                    bqPrivilegeTier = tier
                }

            -- Add to the queue
            atomically $ modifyTVar' (bqEntries $ dsBuildQueue state) $ \entries ->
                sortQueueEntries (entry : entries)

-- | Sort queue entries by priority and enqueue time
sortQueueEntries :: [BuildQueueEntry] -> [BuildQueueEntry]
sortQueueEntries = sortBy (comparing (\e -> (-bqPriority e, bqEnqueueTime e)))

-- | Unregister a build
unregisterBuild :: DaemonState t -> BuildId -> IO ()
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
            -- Verify privilege tier - we can only cancel builds initiated at our tier or lower
            let stateTier = fromSing $ dsPrivilegeEvidence state
            let buildTier = abPrivilegeTier build

            -- Builder can't cancel Daemon-initiated builds
            when (stateTier == Builder && buildTier == Daemon) $
                throwIO $ StateError $ StatePrivilegeError
                    "Builder privilege tier cannot cancel Daemon-initiated builds"

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
updateBuildStatus :: DaemonState t -> BuildId -> BuildStatus -> IO ()
updateBuildStatus state buildId status = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Verify privilege tier - we can only update builds initiated at our tier or lower
            let stateTier = fromSing $ dsPrivilegeEvidence state
            let buildTier = abPrivilegeTier build

            -- Builder can't update Daemon-initiated builds
            when (stateTier == Builder && buildTier == Daemon) $
                throwIO $ StateError $ StatePrivilegeError
                    "Builder privilege tier cannot update Daemon-initiated builds"

            -- Update the status and timestamp
            now <- getCurrentTime
            atomically $ do
                writeTVar (abStatus build) status
                writeTVar (abUpdateTime build) now

-- | Append to a build's log
appendBuildLog :: DaemonState t -> BuildId -> Text -> IO ()
appendBuildLog state buildId logText = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Verify privilege tier - we can only append to builds initiated at our tier or lower
            let stateTier = fromSing $ dsPrivilegeEvidence state
            let buildTier = abPrivilegeTier build

            -- Builder can't append to Daemon-initiated builds
            when (stateTier == Builder && buildTier == Daemon) $
                throwIO $ StateError $ StatePrivilegeError
                    "Builder privilege tier cannot append to logs of Daemon-initiated builds"

            -- Append to the log
            atomically $ modifyTVar' (abLogBuffer build) $ \log -> log <> logText

-- | Get a build's status
getBuildStatus :: DaemonState t -> BuildId -> IO BuildStatus
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
getBuildLog :: DaemonState t -> BuildId -> IO Text
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
listActiveBuilds :: DaemonState t -> IO [(BuildId, Derivation, BuildStatus, UTCTime, Double)]
listActiveBuilds state = do
    -- Get all active builds
    activeBuilds <- atomically $ readTVar (dsActiveBuilds state)

    -- Get the privilege tier of the state
    let stateTier = fromSing $ dsPrivilegeEvidence state

    -- Filter builds based on privilege tier (Builder can only see Builder builds)
    let filteredBuilds = case stateTier of
            Daemon -> activeBuilds -- Daemon can see all builds
            Builder -> Map.filter (\build -> abPrivilegeTier build == Builder) activeBuilds

    -- Collect build information
    mapM collectBuildInfo (Map.toList filteredBuilds)
  where
    collectBuildInfo (buildId, build) = do
        status <- atomically $ readTVar (abStatus build)
        updateTime <- atomically $ readTVar (abUpdateTime build)

        -- Calculate progress
        progress <- calculateBuildProgress build

        return (buildId, abDerivation build, status, updateTime, progress)

-- | List queued builds
listQueuedBuilds :: DaemonState t -> IO [(BuildId, Derivation, Int, UTCTime)]
listQueuedBuilds state = do
    -- Get all queued builds
    queueEntries <- atomically $ readTVar (bqEntries $ dsBuildQueue state)

    -- Get the privilege tier of the state
    let stateTier = fromSing $ dsPrivilegeEvidence state

    -- Filter queue entries based on privilege tier (Builder can only see Builder entries)
    let filteredEntries = case stateTier of
            Daemon -> queueEntries -- Daemon can see all entries
            Builder -> filter (\entry -> bqPrivilegeTier entry == Builder) queueEntries

    -- Return information
    return $ map (\entry -> (bqBuildId entry, bqDerivation entry, bqPriority entry, bqEnqueueTime entry))
             filteredEntries

-- | Register a returned derivation for a build
registerReturnedDerivation :: DaemonState t -> BuildId -> Derivation -> IO Bool
registerReturnedDerivation state buildId innerDerivation = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Verify privilege tier - we can only register for builds initiated at our tier or lower
            let stateTier = fromSing $ dsPrivilegeEvidence state
            let buildTier = abPrivilegeTier build

            -- Builder can't register for Daemon-initiated builds
            when (stateTier == Builder && buildTier == Daemon) $
                throwIO $ StateError $ StatePrivilegeError
                    "Builder privilege tier cannot register derivations for Daemon-initiated builds"

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
getDerivationChain :: DaemonState t -> BuildId -> IO [Derivation]
getDerivationChain state buildId = do
    -- Check if the build exists
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Verify privilege tier - we can only get chains for builds initiated at our tier or lower
            let stateTier = fromSing $ dsPrivilegeEvidence state
            let buildTier = abPrivilegeTier build

            -- Builder can't get chains for Daemon-initiated builds
            when (stateTier == Builder && buildTier == Daemon) $
                throwIO $ StateError $ StatePrivilegeError
                    "Builder privilege tier cannot access derivation chains for Daemon-initiated builds"

            atomically $ readTVar (abDerivationChain build)

-- | Check if a derivation is in a build's chain
isDerivationInChain :: DaemonState t -> BuildId -> Derivation -> IO Bool
isDerivationInChain state buildId derivation = do
    -- Get the chain
    chain <- getDerivationChain state buildId

    -- Check if the derivation is in the chain
    return $ any (derivationEquals derivation) chain

-- | Acquire a lock for a store path
acquirePathLock :: DaemonState t -> StorePath -> IO ()
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
releasePathLock :: DaemonState t -> StorePath -> IO ()
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
withPathLock :: DaemonState t -> StorePath -> IO a -> IO a
withPathLock state path action = do
    bracket
        (acquirePathLock state path)
        (\_ -> releasePathLock state path)
        (\_ -> action)

-- | Mark a path as reachable - requires CanModifyStore privilege
markPathAsReachable :: (CanModifyStore t ~ 'True) => DaemonState t -> StorePath -> IO ()
markPathAsReachable state path = do
    atomically $ modifyTVar' (dsReachablePaths state) $ Set.insert path

-- | Check if a path is reachable
isPathReachable :: DaemonState t -> StorePath -> IO Bool
isPathReachable state path = do
    atomically $ Set.member path <$> readTVar (dsReachablePaths state)

-- | Get all reachable paths
getReachablePaths :: DaemonState t -> IO (Set StorePath)
getReachablePaths state = do
    atomically $ readTVar (dsReachablePaths state)

-- | Register a derivation's build graph
registerDerivationGraph :: DaemonState t -> Derivation -> Graph.BuildGraph -> IO ()
registerDerivationGraph state derivation graph = do
    atomically $ modifyTVar' (dsDerivationGraphs state) $
        Map.insert (derivHash derivation) graph

-- | Get a derivation's build graph
getDerivationGraph :: DaemonState t -> Derivation -> IO (Maybe Graph.BuildGraph)
getDerivationGraph state derivation = do
    atomically $ Map.lookup (derivHash derivation) <$> readTVar (dsDerivationGraphs state)

-- | Get transitive dependencies of a derivation
getTransitiveDependencies :: DaemonState t -> Derivation -> IO (Set Text)
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

-- | Check if a process is still running
isProcessRunning :: ProcessID -> IO Bool
isProcessRunning pid = do
    -- Try to send signal 0 to the process
    result <- try $ signalProcess 0 pid
    case result of
        Left (_ :: SomeException) -> return False  -- Process doesn't exist
        Right _ -> return True  -- Process exists

-- | Check if the GC lock file exists and is valid
checkGCLockFile :: FilePath -> IO (Either Text Bool)
checkGCLockFile lockPath = do
    -- Check if lock file exists
    exists <- doesFileExist lockPath

    if not exists
        then return $ Right False  -- Lock file doesn't exist
        else do
            -- Read the lock file to get PID
            result <- try $ readFile lockPath
            case result of
                Left (e :: SomeException) ->
                    return $ Left $ "Error reading lock file: " <> T.pack (show e)

                Right content -> do
                    -- Parse the PID
                    case reads content of
                        [(pid, "")] -> do
                            -- Check if the process is still running
                            pidRunning <- isProcessRunning pid
                            return $ Right pidRunning

                        _ -> return $ Left "Invalid lock file format"

-- | Create and acquire a GC lock file
acquireGCLockFile :: FilePath -> IO (Either Text (Fd, ProcessID))
acquireGCLockFile lockPath = do
    -- Ensure parent directory exists
    ensureLockDirExists (takeDirectory lockPath)

    -- Get our process ID
    pid <- getProcessID

    -- Check if lock already exists
    lockStatus <- checkGCLockFile lockPath

    case lockStatus of
        Left err -> return $ Left err
        Right True ->
            return $ Left "Another garbage collection is in progress"
        Right False -> do
            -- Create the lock file with our PID
            result <- try $ do
                -- Open or create the file
                fd <- openFd lockPath ReadWrite (Just 0o644) (defaultFileFlags {trunc = True})

                -- Write our PID to it
                handle <- fdToHandle fd
                hPutStrLn handle (show pid)
                hFlush handle

                -- Set exclusive lock (non-blocking)
                setLock fd (WriteLock, AbsoluteSeek, 0, 0)

                -- Return file descriptor and PID
                return (fd, pid)

            case result of
                Left (e :: SomeException) ->
                    return $ Left $ "Failed to create lock file: " <> T.pack (show e)
                Right lockInfo -> return $ Right lockInfo

-- | Release a GC lock file
releaseGCLockFile :: FilePath -> Fd -> IO ()
releaseGCLockFile lockPath fd = do
    -- Release the lock
    setLock fd (Unlock, AbsoluteSeek, 0, 0)

    -- Close the file descriptor
    closeFd fd

    -- Remove the lock file
    catchIOError (removeFile lockPath) (\_ -> return ())

-- | Check if GC lock is available (can be acquired)
checkGCLock :: DaemonState t -> STM Bool
checkGCLock state = do
    -- Check if we have a recorded owner
    mOwner <- readTVar (dsGCLockOwner state)
    case mOwner of
        Nothing -> return True  -- No owner, lock is available
        Just _ -> return False  -- Owned by some process

-- | Try to acquire the GC lock without throwing
tryAcquireGCLock :: (CanModifyStore t ~ 'True) => DaemonState t -> IO Bool
tryAcquireGCLock state = do
    -- Try to get the file-based lock
    result <- acquireGCLockFile (dsGCLockPath state)

    case result of
        Left _ -> return False
        Right (fd, pid) -> do
            -- Record the lock state in memory too
            atomically $ do
                -- Update the lock owner
                writeTVar (dsGCLockOwner state) (Just pid)

                -- For backwards compatibility, try to take the TMVar lock too
                empty <- isEmptyTMVar (dsGCLock state)
                unless empty $ do
                    void $ takeTMVar (dsGCLock state)

            -- Keep the fd in a global reference to avoid GC
            -- (This is a bit of a hack, but we don't have a place to store it in the state)
            -- In a real implementation you'd want to track this properly
            writeIORef globalGCLockFdRef (Just (fd, dsGCLockPath state))
            return True

-- | Acquire the GC lock - requires CanModifyStore privilege
acquireGCLock :: (CanModifyStore t ~ 'True) => DaemonState t -> IO ()
acquireGCLock state = do
    -- Try to get the file-based lock
    result <- acquireGCLockFile (dsGCLockPath state)

    case result of
        Left err ->
            throwIO $ StateError $ StateLockError $ "Failed to acquire GC lock: " <> err
        Right (fd, pid) -> do
            -- Record the lock state in memory too
            atomically $ do
                -- Update the lock owner
                writeTVar (dsGCLockOwner state) (Just pid)

                -- For backwards compatibility, try to take the TMVar lock too
                empty <- isEmptyTMVar (dsGCLock state)
                unless empty $ do
                    void $ takeTMVar (dsGCLock state)

            -- Keep the fd in a global reference to avoid GC
            -- (This is a bit of a hack, but we don't have a place to store it in the state)
            -- In a real implementation you'd want to track this properly
            writeIORef globalGCLockFdRef (Just (fd, dsGCLockPath state))

-- | Release the GC lock - requires CanModifyStore privilege
releaseGCLock :: (CanModifyStore t ~ 'True) => DaemonState t -> IO ()
releaseGCLock state = do
    -- Get the file descriptor from the global reference
    mFdInfo <- readIORef globalGCLockFdRef

    case mFdInfo of
        Just (fd, path) | path == dsGCLockPath state -> do
            -- Release the file lock
            releaseGCLockFile path fd

            -- Clear the global reference
            writeIORef globalGCLockFdRef Nothing

            -- Update memory state
            atomically $ do
                -- Clear the lock owner
                writeTVar (dsGCLockOwner state) Nothing

                -- For backwards compatibility, release the TMVar lock too
                empty <- isEmptyTMVar (dsGCLock state)
                when empty $ do
                    putTMVar (dsGCLock state) ()

        _ ->
            -- Not our lock or not found
            return ()

-- | Global reference to hold the GC lock file descriptor
{-# NOINLINE globalGCLockFdRef #-}
globalGCLockFdRef :: IORef (Maybe (Fd, FilePath))
globalGCLockFdRef = unsafePerformIO $ newIORef Nothing

-- | Execute an action with the GC lock - requires CanModifyStore privilege
withGCLock :: (CanModifyStore t ~ 'True) => DaemonState t -> IO a -> IO a
withGCLock state action = do
    bracket
        (acquireGCLock state)
        (\_ -> releaseGCLock state)
        (\_ -> action)

-- | Update system statistics
updateSystemStats :: DaemonState t -> IO ()
updateSystemStats state = do
    stats <- captureSystemStats
    atomically $ writeTVar (dsDaemonStats state) stats

-- | Get daemon statistics
getDaemonStats :: DaemonState t -> IO DaemonStats
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
    let (storeSize, storeEntries) = (0, 0)

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
hasPendingBuilds :: DaemonState t -> IO Bool
hasPendingBuilds state = do
    queueEntries <- atomically $ readTVar (bqEntries $ dsBuildQueue state)

    -- Get the privilege tier of the state
    let stateTier = fromSing $ dsPrivilegeEvidence state

    -- Filter entries by privilege (Builder can only see Builder entries)
    let relevantEntries = case stateTier of
            Daemon -> queueEntries
            Builder -> filter (\e -> bqPrivilegeTier e == Builder) queueEntries

    return $ not $ null relevantEntries

-- | Check if a specific build is active
hasActiveBuild :: DaemonState t -> BuildId -> IO Bool
hasActiveBuild state buildId = do
    activeBuilds <- atomically $ readTVar (dsActiveBuilds state)

    -- Check if the build exists
    case Map.lookup buildId activeBuilds of
        Nothing -> return False
        Just build -> do
            -- Verify privilege tier
            let stateTier = fromSing $ dsPrivilegeEvidence state
            let buildTier = abPrivilegeTier build

            -- Builder can only see Builder builds
            case stateTier of
                Daemon -> return True
                Builder -> return (buildTier == Builder)

-- | Check if there is capacity for more builds
hasBuildCapacity :: DaemonState t -> IO Bool
hasBuildCapacity state = do
    activeBuildCount <- atomically $ Map.size <$> readTVar (dsActiveBuilds state)
    return $ activeBuildCount < dsMaxConcurrentBuilds state

-- | Get the next build to schedule
getNextBuildToSchedule :: DaemonState t -> IO (Maybe BuildQueueEntry)
getNextBuildToSchedule state = do
    -- Check if we're at capacity
    activeBuildCount <- atomically $ Map.size <$> readTVar (dsActiveBuilds state)

    if activeBuildCount >= dsMaxConcurrentBuilds state
        then return Nothing
        else do
            -- Get queue entries
            queueEntries <- atomically $ readTVar (bqEntries $ dsBuildQueue state)

            -- Get the privilege tier of the state
            let stateTier = fromSing $ dsPrivilegeEvidence state

            -- Filter entries by privilege (Builder can only see Builder entries)
            let relevantEntries = case stateTier of
                    Daemon -> queueEntries
                    Builder -> filter (\e -> bqPrivilegeTier e == Builder) queueEntries

            if null relevantEntries
                then return Nothing
                else do
                    -- Find the first entry with all dependencies satisfied
                    findReadyEntry relevantEntries
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
tryScheduleNextBuild :: DaemonState t -> IO ()
tryScheduleNextBuild state = do
    -- Get the next build to schedule
    mEntry <- getNextBuildToSchedule state

    case mEntry of
        Nothing -> return ()  -- No ready builds
        Just entry -> do
            -- Remove from queue
            atomically $ modifyTVar' (bqEntries $ dsBuildQueue state) $
                filter (\e -> bqBuildId e /= bqBuildId entry)

            -- Get the privilege tier that queued this build
            let tier = bqPrivilegeTier entry

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
                    abThread = threadVar,
                    abPrivilegeTier = tier
                }

            -- Register the build
            atomically $ modifyTVar' (dsActiveBuilds state) $
                Map.insert (bqBuildId entry) activeBuild

-- | Prune old completed builds to stay within limits
pruneCompletedBuilds :: DaemonState t -> IO ()
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
cleanupStaleBuilds :: DaemonState t -> IO ()
cleanupStaleBuilds state = do
    now <- getCurrentTime

    -- Get all active builds
    activeBuilds <- atomically $ readTVar (dsActiveBuilds state)

    -- Check each build for staleness
    forM_ (Map.toList activeBuilds) $ \(buildId, build) -> do
        -- Check if we have the privilege to clean up this build
        let stateTier = fromSing $ dsPrivilegeEvidence state
        let buildTier = abPrivilegeTier build

        -- Only clean up builds we have the privilege to clean
        when (stateTier == Daemon || buildTier == Builder) $ do
            -- Check if the build has timed out
            case abTimeout build of
                Just timeout -> do
                    let timeoutTime = addUTCTime (fromIntegral timeout) (abStartTime build)
                    when (now > timeoutTime) $ do
                        -- Mark as failed and remove
                        let err = Core.BuildFailed "Build timed out"
                        atomically $ putTMVar (abResult build) (Left err)
                        unregisterBuild state buildId

                Nothing -> do
                    -- Check for last update time
                    lastUpdate <- atomically $ readTVar (abUpdateTime build)
                    let idleTime = diffUTCTime now lastUpdate

                    -- If idle for more than 1 hour, consider it stale
                    when (idleTime > 3600) $ do
                        -- Mark as failed and remove
                        let err = Core.BuildFailed "Build stalled or crashed"
                        atomically $ putTMVar (abResult build) (Left err)
                        unregisterBuild state buildId

-- | Setup periodic maintenance - requires CanModifyStore privilege
setupMaintenanceThread :: DaemonState 'Daemon -> IO ()
setupMaintenanceThread state = do
    -- Create a thread that runs maintenance tasks periodically
    thread <- async $ maintenanceLoop state

    -- Store the thread
    atomically $ writeTVar (dsMaintenanceThread state) (Just thread)

-- | Maintenance loop - requires CanModifyStore privilege
maintenanceLoop :: (CanModifyStore t ~ 'True) => DaemonState t -> IO ()
maintenanceLoop state = do
    -- Run forever
    forever $ do
        -- Run maintenance tasks
        scheduledMaintenance state

        -- Sleep for a while
        threadDelay (60 * 1000 * 1000)  -- 60 seconds

-- | Run scheduled maintenance tasks - requires CanModifyStore privilege
scheduledMaintenance :: (CanModifyStore t ~ 'True, CanAccessStore t ~ 'True) => DaemonState t -> IO ()
scheduledMaintenance state = do
    -- Update system stats
    updateSystemStats state

    -- Clean up stale builds
    cleanupStaleBuilds state

    -- Prune old completed builds
    pruneCompletedBuilds state

    -- Save state to file
    saveStateToFile state

-- | Set up signal handlers - requires Daemon privileges
setupSignalHandlers :: (CanModifyStore t ~ 'True) => DaemonState t -> IO ()
setupSignalHandlers state = do
    -- Handle SIGTERM to gracefully shut down
    void $ installHandler sigTERM (Catch $ handleTermSignal state) Nothing

    -- Handle SIGINT (Ctrl+C)
    void $ installHandler sigINT (Catch $ handleTermSignal state) Nothing

    -- Handle SIGHUP for config reload
    void $ installHandler sigHUP (Catch $ handleHupSignal state) Nothing

-- | Handle SIGTERM signal - requires CanModifyStore privilege
handleTermSignal :: (CanModifyStore t ~ 'True, CanAccessStore t ~ 'True) => DaemonState t -> IO ()
handleTermSignal state = do
    -- Save state
    putStrLn "Received termination signal, shutting down..."
    saveStateToFile state

    -- Cancel maintenance thread
    mThread <- atomically $ readTVar (dsMaintenanceThread state)
    case mThread of
        Just thread -> cancel thread
        Nothing -> return ()

    -- Release GC lock if we have it
    mOwner <- atomically $ readTVar (dsGCLockOwner state)
    case mOwner of
        Just _ -> releaseGCLock state
        Nothing -> return ()

    -- Exit
    putStrLn "Shutdown complete."
    exitImmediately 0

-- | Handle SIGHUP signal - requires CanModifyStore privilege
handleHupSignal :: (CanModifyStore t ~ 'True, CanAccessStore t ~ 'True) => DaemonState t -> IO ()
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
withState :: DaemonState t -> (DaemonState t -> IO a) -> IO a
withState state action = action state

-- | Modify daemon state
modifyState :: DaemonState t -> (DaemonState t -> IO (DaemonState t, a)) -> IO a
modifyState state f = do
    (newState, result) <- f state
    return result

-- | Read daemon state
readState :: DaemonState t -> (DaemonState t -> IO a) -> IO a
readState state f = f state

-- | Capture build status information
captureStatus :: DaemonState t -> BuildId -> IO (BuildStatus, Double, UTCTime, UTCTime)
captureStatus state buildId = do
    -- Get the build
    mBuild <- atomically $ Map.lookup buildId <$> readTVar (dsActiveBuilds state)

    case mBuild of
        Nothing -> throwIO $ StateError $ StateBuildNotFound buildId
        Just build -> do
            -- Verify privilege tier - we can only capture status for builds initiated at our tier or lower
            let stateTier = fromSing $ dsPrivilegeEvidence state
            let buildTier = abPrivilegeTier build

            -- Builder can't capture status for Daemon-initiated builds
            when (stateTier == Builder && buildTier == Daemon) $
                throwIO $ StateError $ StatePrivilegeError
                    "Builder privilege tier cannot capture status of Daemon-initiated builds"

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

-- | Random number generation
randomIO :: IO Int
randomIO = do
    now <- getCurrentTime
    let ns = floor (realToFrac (diffUTCTime now (read "1970-01-01 00:00:00 UTC")) * 1000000000)
    return $ fromIntegral (ns `mod` maxBound)

-- | Exit successfully
exitSuccess :: IO a
exitSuccess = do
    -- Exit the process with success status
    System.Posix.Process.exitImmediately 0
