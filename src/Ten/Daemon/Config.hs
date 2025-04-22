{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Ten.Daemon.Config (
    -- Core configuration types
    DaemonConfig(..),
    LogLevel(..),
    PrivilegeModel(..),

    -- Configuration management
    defaultDaemonConfig,
    loadDaemonConfig,
    parseConfigArgs,
    validateConfig,

    -- Environment variable handling
    loadConfigFromEnv,

    -- File-based configuration
    loadConfigFromFile,
    saveConfigToFile,

    -- Accessors and utilities
    getDefaultSocketPath,
    getDefaultStatePath,
    getDefaultConfig,
    getDefaultStoreLocation,
    getDefaultGcInterval,

    -- Command-line argument parsing
    configOptionDescriptions,
    parseConfigFromArgs,

    -- Privilege-aware configuration operations
    withConfigPrivilege,
    validatePrivilegeConfig,
    requiresPrivilegeFor
) where

import Control.Exception (try, SomeException)
import Control.Monad (when, unless)
import Control.Applicative ((<|>))
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe, isNothing, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)
import System.Directory (doesFileExist, getHomeDirectory, createDirectoryIfMissing, getXdgDirectory, XdgDirectory(..))
import System.Environment (getEnv, lookupEnv)
import System.FilePath ((</>), takeDirectory)
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName)
import qualified System.IO.Error as IOE

-- Import singletons properly
import Data.Singletons
import Data.Singletons.TH
import Data.Kind (Type)

import Ten.Core (BuildError(..), Phase(..), PrivilegeTier(..), SPrivilegeTier(..),
                 CanAccessStore, CanModifyStore, CanDropPrivileges, CanCreateSandbox,
                 TenM(..), withSPrivilegeTier, sDaemon, sBuilder, privilegeError)

-- | Privilege model for daemon operations - aligned with Nix architecture
data PrivilegeModel
    = PrivilegeAlwaysDrop     -- ^ Always drop privileges (safest, like Nix)
    | PrivilegeDropSelective  -- ^ Drop privileges selectively based on operation
    | PrivilegeNoDrop         -- ^ Never drop privileges (dangerous, only for special cases)
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Log levels for daemon operations
data LogLevel
    = LogQuiet     -- ^ Almost no logging (only critical errors)
    | LogNormal    -- ^ Normal logging (warnings and important events)
    | LogVerbose   -- ^ Verbose logging (detailed operation information)
    | LogDebug     -- ^ Debug logging (low-level details for troubleshooting)
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Configuration for the Ten daemon
data DaemonConfig = DaemonConfig {
    -- Network settings
    daemonSocketPath    :: FilePath,      -- ^ Path to the daemon socket
    daemonPort          :: Maybe Int,     -- ^ Optional TCP port for remote connections
    daemonBindAddress   :: Maybe String,  -- ^ Optional bind address for TCP connections

    -- Storage settings
    daemonStoreLocation :: FilePath,      -- ^ Path to the content-addressable store directory
    daemonStateFile     :: FilePath,      -- ^ Path to the daemon state file
    daemonTmpDir        :: FilePath,      -- ^ Path for temporary files

    -- Build settings
    daemonMaxJobs       :: Int,           -- ^ Maximum concurrent build jobs
    daemonBuildTimeout  :: Int,           -- ^ Build timeout in seconds (0 = no timeout)
    daemonKeepFailed    :: Bool,          -- ^ Keep failed build outputs for inspection

    -- Garbage collection settings
    daemonGcInterval    :: Maybe Int,     -- ^ GC interval in seconds (Nothing = manual only)
    daemonGcKeepLast    :: Int,           -- ^ Number of recent builds to protect from GC

    -- Security settings
    daemonUser          :: Maybe Text,    -- ^ User to run daemon as (if running as root)
    daemonGroup         :: Maybe Text,    -- ^ Group to run daemon as (if running as root)
    daemonAllowedUsers  :: Set Text,      -- ^ Users allowed to connect to daemon
    daemonRequireAuth   :: Bool,          -- ^ Whether authentication is required

    -- Privilege model settings
    daemonAllowPrivilegeEscalation :: Bool,          -- ^ Allow users to escalate privileges (dangerous)
    daemonPrivilegeModel :: PrivilegeModel,          -- ^ How to handle privileges
    daemonDefaultPrivilegeTier :: PrivilegeTier,     -- ^ Default privilege tier for connections

    -- Logging settings
    daemonLogFile       :: Maybe FilePath, -- ^ Path to log file (Nothing = stdout)
    daemonLogLevel      :: LogLevel,       -- ^ Logging verbosity

    -- Daemon behavior settings
    daemonForeground    :: Bool,          -- ^ Run in foreground (don't daemonize)
    daemonAutoStart     :: Bool,          -- ^ Auto-start daemon when client connects if not running
    daemonRestrictive   :: Bool           -- ^ Use more restrictive sandbox settings
} deriving (Show, Eq)

-- | Load daemon configuration from a file (alias for loadConfigFromFile)
loadDaemonConfig :: FilePath -> IO (Either String DaemonConfig)
loadDaemonConfig = loadConfigFromFile

-- | Default daemon configuration
defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig {
    -- Network defaults
    daemonSocketPath    = "/var/run/ten/daemon.sock",
    daemonPort          = Nothing,
    daemonBindAddress   = Nothing,

    -- Storage defaults - will be updated by getDefaultConfig
    daemonStoreLocation = "/var/lib/ten/store",
    daemonStateFile     = "/var/lib/ten/daemon-state.json",
    daemonTmpDir        = "/var/lib/ten/tmp",

    -- Build defaults
    daemonMaxJobs       = 4,
    daemonBuildTimeout  = 3600,  -- 1 hour
    daemonKeepFailed    = False,

    -- Garbage collection defaults
    daemonGcInterval    = Just (3600 * 24),  -- Daily
    daemonGcKeepLast    = 10,

    -- Security defaults
    daemonUser          = Nothing,
    daemonGroup         = Nothing,
    daemonAllowedUsers  = Set.empty,  -- Empty means all users are allowed
    daemonRequireAuth   = True,

    -- Privilege model defaults - aligned with Nix
    daemonAllowPrivilegeEscalation = False,  -- Default to disallowing privilege escalation
    daemonPrivilegeModel = PrivilegeAlwaysDrop,  -- Default to safest model, like Nix
    daemonDefaultPrivilegeTier = Builder,  -- Default to unprivileged, like Nix

    -- Logging defaults
    daemonLogFile       = Just "/var/log/ten/daemon.log",
    daemonLogLevel      = LogNormal,

    -- Behavior defaults
    daemonForeground    = False,
    daemonAutoStart     = True,
    daemonRestrictive   = False
}

-- | Get default configuration considering user permissions
getDefaultConfig :: IO DaemonConfig
getDefaultConfig = do
    -- Get current user information
    uid <- getEffectiveUserID
    userEntry <- getUserEntryForID uid
    let currentUser = T.pack $ userName userEntry

    -- Determine if we're running as root
    let isRoot = uid == 0

    -- Get home directory for non-root paths
    homeDir <- getHomeDirectory

    -- Get XDG config and data directories
    xdgConfigDir <- getXdgDirectory XdgConfig "ten"
    xdgDataDir <- getXdgDirectory XdgData "ten"
    xdgStateDir <- getXdgDirectory XdgState "ten"

    -- Create directories if they don't exist
    mapM_ (createDirectoryIfMissing True)
        [xdgConfigDir, xdgDataDir, xdgStateDir]

    -- Modify default config based on whether we're root or not
    let config = if isRoot
            then defaultDaemonConfig {
                -- When running as root, allow privilege escalation but default to Builder
                daemonAllowPrivilegeEscalation = False,  -- Still default to False for safety
                daemonDefaultPrivilegeTier = Builder  -- Default to unprivileged
            }
            else defaultDaemonConfig {
                -- Non-root appropriate paths
                daemonSocketPath    = xdgStateDir </> "daemon.sock",
                daemonStoreLocation = xdgDataDir </> "store",
                daemonStateFile     = xdgStateDir </> "daemon-state.json",
                daemonTmpDir        = xdgStateDir </> "tmp",
                daemonLogFile       = Just (xdgStateDir </> "daemon.log"),
                -- Always run in foreground when not root
                daemonForeground    = True,
                -- Set current user as default allowed user
                daemonAllowedUsers  = Set.singleton currentUser,
                -- Non-root users can't escalate privileges
                daemonAllowPrivilegeEscalation = False,
                daemonDefaultPrivilegeTier = Builder
            }

    -- Create directories for the updated paths
    let dirsToCreate = [
            takeDirectory (daemonSocketPath config),
            daemonStoreLocation config,
            daemonTmpDir config,
            takeDirectory (daemonStateFile config),
            maybe "" takeDirectory (daemonLogFile config)
            ]

    mapM_ (createDirectoryIfMissing True) dirsToCreate

    return config

-- | Get the default socket path based on current user
getDefaultSocketPath :: IO FilePath
getDefaultSocketPath = do
    config <- getDefaultConfig
    return $ daemonSocketPath config

-- | Get the default state file path
getDefaultStatePath :: IO FilePath
getDefaultStatePath = do
    config <- getDefaultConfig
    return $ daemonStateFile config

-- | Get the default store location
getDefaultStoreLocation :: IO FilePath
getDefaultStoreLocation = do
    -- Check environment variable first
    envPath <- lookupEnv "TEN_STORE_LOCATION"
    case envPath of
        Just path -> return path
        Nothing -> do
            -- Check legacy environment variable
            legacyPath <- lookupEnv "TEN_STORE_PATH"
            case legacyPath of
                Just path -> return path
                Nothing -> do
                    -- Use config default
                    config <- getDefaultConfig
                    return $ daemonStoreLocation config

-- | Get the default garbage collection interval
getDefaultGcInterval :: IO (Maybe Int)
getDefaultGcInterval = do
    config <- getDefaultConfig
    return $ daemonGcInterval config

-- | Load daemon configuration from a file
loadConfigFromFile :: FilePath -> IO (Either String DaemonConfig)
loadConfigFromFile path = do
    exists <- doesFileExist path
    if not exists
        then return $ Left $ "Configuration file not found: " ++ path
        else do
            result <- try $ BS.readFile path
            case result of
                Left (ex :: SomeException) ->
                    return $ Left $ "Error reading configuration file: " ++ show ex
                Right content ->
                    case Aeson.eitherDecodeStrict content of
                        Left err -> return $ Left $ "Error parsing configuration: " ++ err
                        Right config -> do
                            -- Validate the loaded configuration
                            case validateConfig config of
                                Left err -> return $ Left err
                                Right validConfig -> return $ Right validConfig

-- | Save daemon configuration to a file
-- This operation requires Daemon privileges
saveConfigToFile :: FilePath -> DaemonConfig -> TenM 'Build 'Daemon (Either String ())
saveConfigToFile path config = TenM $ \sp st -> do
    let dir = takeDirectory path
    result <- try $ createDirectoryIfMissing True dir
    case result of
        Left (ex :: SomeException) ->
            return $ Left $ "Error creating directory: " ++ show ex
        Right () -> do
            result' <- try $ LBS.writeFile path (Aeson.encode config)
            case result' of
                Left (ex :: SomeException) ->
                    return $ Left $ "Error writing configuration: " ++ show ex
                Right () -> return $ Right ()

-- | Load configuration from environment variables
loadConfigFromEnv :: IO DaemonConfig
loadConfigFromEnv = do
    -- Get default config first
    defaultConfig <- getDefaultConfig

    -- Try to override with environment variables
    mSocketPath <- lookupEnv "TEN_DAEMON_SOCKET"
    mPort <- lookupEnvInt "TEN_DAEMON_PORT"
    mBindAddr <- lookupEnv "TEN_DAEMON_BIND_ADDRESS"
    mStoreLocation <- lookupEnv "TEN_STORE_LOCATION"
    -- Check legacy environment variable if the new one is not set
    mStoreLocation' <- if isJust mStoreLocation
                       then return mStoreLocation
                       else lookupEnv "TEN_STORE_PATH"
    mStateFile <- lookupEnv "TEN_DAEMON_STATE_FILE"
    mTmpDir <- lookupEnv "TEN_DAEMON_TMP_DIR"
    mMaxJobs <- lookupEnvInt "TEN_DAEMON_MAX_JOBS"
    mTimeout <- lookupEnvInt "TEN_DAEMON_BUILD_TIMEOUT"
    mKeepFailed <- lookupEnvBool "TEN_DAEMON_KEEP_FAILED"
    mGcInterval <- lookupEnvInt "TEN_DAEMON_GC_INTERVAL"
    mGcKeepLast <- lookupEnvInt "TEN_DAEMON_GC_KEEP_LAST"
    mUser <- lookupEnvText "TEN_DAEMON_USER"
    mGroup <- lookupEnvText "TEN_DAEMON_GROUP"
    mAllowedUsers <- lookupEnvTextList "TEN_DAEMON_ALLOWED_USERS"
    mRequireAuth <- lookupEnvBool "TEN_DAEMON_REQUIRE_AUTH"
    mAllowPrivEsc <- lookupEnvBool "TEN_DAEMON_ALLOW_PRIVILEGE_ESCALATION"
    mPrivModel <- lookupEnvPrivModel "TEN_DAEMON_PRIVILEGE_MODEL"
    mDefaultTier <- lookupEnvPrivilegeTier "TEN_DAEMON_DEFAULT_PRIVILEGE_TIER"
    mLogFile <- lookupEnv "TEN_DAEMON_LOG_FILE"
    mLogLevel <- lookupEnvLogLevel "TEN_DAEMON_LOG_LEVEL"
    mForeground <- lookupEnvBool "TEN_DAEMON_FOREGROUND"
    mAutoStart <- lookupEnvBool "TEN_DAEMON_AUTO_START"
    mRestrictive <- lookupEnvBool "TEN_DAEMON_RESTRICTIVE"

    -- Apply overrides
    let config = defaultConfig {
            daemonSocketPath = fromMaybe (daemonSocketPath defaultConfig) mSocketPath,
            daemonPort = mPort <|> daemonPort defaultConfig,
            daemonBindAddress = mBindAddr <|> daemonBindAddress defaultConfig,
            daemonStoreLocation = fromMaybe (daemonStoreLocation defaultConfig) mStoreLocation',
            daemonStateFile = fromMaybe (daemonStateFile defaultConfig) mStateFile,
            daemonTmpDir = fromMaybe (daemonTmpDir defaultConfig) mTmpDir,
            daemonMaxJobs = fromMaybe (daemonMaxJobs defaultConfig) mMaxJobs,
            daemonBuildTimeout = fromMaybe (daemonBuildTimeout defaultConfig) mTimeout,
            daemonKeepFailed = fromMaybe (daemonKeepFailed defaultConfig) mKeepFailed,
            daemonGcInterval = if mGcInterval == Just 0 then Nothing else mGcInterval <|> daemonGcInterval defaultConfig,
            daemonGcKeepLast = fromMaybe (daemonGcKeepLast defaultConfig) mGcKeepLast,
            daemonUser = mUser <|> daemonUser defaultConfig,
            daemonGroup = mGroup <|> daemonGroup defaultConfig,
            daemonAllowedUsers = maybe (daemonAllowedUsers defaultConfig) Set.fromList mAllowedUsers,
            daemonRequireAuth = fromMaybe (daemonRequireAuth defaultConfig) mRequireAuth,
            daemonAllowPrivilegeEscalation = fromMaybe (daemonAllowPrivilegeEscalation defaultConfig) mAllowPrivEsc,
            daemonPrivilegeModel = fromMaybe (daemonPrivilegeModel defaultConfig) mPrivModel,
            daemonDefaultPrivilegeTier = fromMaybe (daemonDefaultPrivilegeTier defaultConfig) mDefaultTier,
            daemonLogFile = (if mLogFile == Just "stdout" then Nothing else mLogFile) <|> daemonLogFile defaultConfig,
            daemonLogLevel = fromMaybe (daemonLogLevel defaultConfig) mLogLevel,
            daemonForeground = fromMaybe (daemonForeground defaultConfig) mForeground,
            daemonAutoStart = fromMaybe (daemonAutoStart defaultConfig) mAutoStart,
            daemonRestrictive = fromMaybe (daemonRestrictive defaultConfig) mRestrictive
        }

    -- Validate final config
    case validateConfig config of
        Left err -> error $ "Invalid configuration from environment: " ++ err
        Right validConfig -> return validConfig
  where
    lookupEnvInt :: String -> IO (Maybe Int)
    lookupEnvInt name = do
        mValue <- lookupEnv name
        case mValue of
            Nothing -> return Nothing
            Just value -> case reads value of
                [(num, "")] -> return $ Just num
                _ -> return Nothing

    lookupEnvBool :: String -> IO (Maybe Bool)
    lookupEnvBool name = do
        mValue <- lookupEnv name
        case mValue of
            Nothing -> return Nothing
            Just value -> case toLower value of
                "true" -> return $ Just True
                "yes" -> return $ Just True
                "1" -> return $ Just True
                "false" -> return $ Just False
                "no" -> return $ Just False
                "0" -> return $ Just False
                _ -> return Nothing

    lookupEnvText :: String -> IO (Maybe Text)
    lookupEnvText name = do
        mValue <- lookupEnv name
        return $ T.pack <$> mValue

    lookupEnvTextList :: String -> IO (Maybe [Text])
    lookupEnvTextList name = do
        mValue <- lookupEnv name
        return $ case mValue of
            Nothing -> Nothing
            Just value -> Just $ map T.strip $ map T.pack $ splitOn ',' value

    lookupEnvLogLevel :: String -> IO (Maybe LogLevel)
    lookupEnvLogLevel name = do
        mValue <- lookupEnv name
        case mValue of
            Nothing -> return Nothing
            Just value -> case toLower value of
                "quiet" -> return $ Just LogQuiet
                "normal" -> return $ Just LogNormal
                "verbose" -> return $ Just LogVerbose
                "debug" -> return $ Just LogDebug
                _ -> case reads value of
                    [(num, "")] | num >= 0 && num <= 3 ->
                        return $ Just $ toEnum num
                    _ -> return Nothing

    lookupEnvPrivModel :: String -> IO (Maybe PrivilegeModel)
    lookupEnvPrivModel name = do
        mValue <- lookupEnv name
        case mValue of
            Nothing -> return Nothing
            Just value -> case toLower value of
                "always-drop" -> return $ Just PrivilegeAlwaysDrop
                "selective" -> return $ Just PrivilegeDropSelective
                "no-drop" -> return $ Just PrivilegeNoDrop
                _ -> return Nothing

    lookupEnvPrivilegeTier :: String -> IO (Maybe PrivilegeTier)
    lookupEnvPrivilegeTier name = do
        mValue <- lookupEnv name
        case mValue of
            Nothing -> return Nothing
            Just value -> case toLower value of
                "daemon" -> return $ Just Daemon
                "builder" -> return $ Just Builder
                _ -> return Nothing

    toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

    splitOn :: Char -> String -> [String]
    splitOn c s = case break (== c) s of
        (chunk, []) -> [chunk]
        (chunk, _:rest) -> chunk : splitOn c rest

-- | Command-line option descriptions for daemon configuration
configOptionDescriptions :: [OptDescr (DaemonConfig -> DaemonConfig)]
configOptionDescriptions = [
    Option ['s'] ["socket"] (ReqArg (\s cfg -> cfg { daemonSocketPath = s }) "PATH")
        "Path to daemon socket",
    Option ['p'] ["port"] (ReqArg (\p cfg -> cfg { daemonPort = Just (read p) }) "PORT")
        "TCP port for remote connections",
    Option ['b'] ["bind"] (ReqArg (\b cfg -> cfg { daemonBindAddress = Just b }) "ADDRESS")
        "Bind address for TCP connections",
    Option [] ["store"] (ReqArg (\s cfg -> cfg { daemonStoreLocation = s }) "PATH")
        "Path to store directory",
    Option [] ["state-file"] (ReqArg (\s cfg -> cfg { daemonStateFile = s }) "PATH")
        "Path to daemon state file",
    Option [] ["tmp-dir"] (ReqArg (\t cfg -> cfg { daemonTmpDir = t }) "PATH")
        "Path for temporary files",
    Option ['j'] ["jobs"] (ReqArg (\j cfg -> cfg { daemonMaxJobs = read j }) "N")
        "Maximum concurrent build jobs",
    Option [] ["timeout"] (ReqArg (\t cfg -> cfg { daemonBuildTimeout = read t }) "SECONDS")
        "Build timeout in seconds (0 = no timeout)",
    Option ['k'] ["keep-failed"] (NoArg (\cfg -> cfg { daemonKeepFailed = True }))
        "Keep failed build outputs",
    Option [] ["gc-interval"] (ReqArg (\i cfg -> cfg {
            daemonGcInterval = if read i == 0 then Nothing else Just (read i)
        }) "SECONDS")
        "Garbage collection interval (0 = manual only)",
    Option [] ["gc-keep-last"] (ReqArg (\n cfg -> cfg { daemonGcKeepLast = read n }) "N")
        "Number of recent builds to protect from GC",
    Option ['u'] ["user"] (ReqArg (\u cfg -> cfg { daemonUser = Just (T.pack u) }) "USER")
        "User to run daemon as",
    Option ['g'] ["group"] (ReqArg (\g cfg -> cfg { daemonGroup = Just (T.pack g) }) "GROUP")
        "Group to run daemon as",
    Option [] ["allowed-users"] (ReqArg (\us cfg -> cfg {
            daemonAllowedUsers = Set.fromList $ map T.strip $ map T.pack $ splitOn ',' us
        }) "USER1,USER2,...")
        "Users allowed to connect to daemon",
    Option [] ["no-auth"] (NoArg (\cfg -> cfg { daemonRequireAuth = False }))
        "Disable authentication requirement",
    Option [] ["allow-privilege-escalation"] (NoArg (\cfg -> cfg { daemonAllowPrivilegeEscalation = True }))
        "Allow users to escalate privileges (dangerous)",
    Option [] ["privilege-model"] (ReqArg (\m cfg -> cfg {
            daemonPrivilegeModel = parsePrivilegeModel m
        }) "always-drop|selective|no-drop")
        "Privilege model (how to handle privilege dropping)",
    Option [] ["default-tier"] (ReqArg (\t cfg -> cfg {
            daemonDefaultPrivilegeTier = parsePrivilegeTier t
        }) "daemon|builder")
        "Default privilege tier for connections",
    Option [] ["log-file"] (ReqArg (\f cfg -> cfg {
            daemonLogFile = if f == "stdout" then Nothing else Just f
        }) "PATH")
        "Path to log file (stdout = log to standard output)",
    Option ['q'] ["quiet"] (NoArg (\cfg -> cfg { daemonLogLevel = LogQuiet }))
        "Minimal logging",
    Option ['v'] ["verbose"] (NoArg (\cfg -> cfg { daemonLogLevel = LogVerbose }))
        "Verbose logging",
    Option ['d'] ["debug"] (NoArg (\cfg -> cfg { daemonLogLevel = LogDebug }))
        "Debug logging",
    Option ['f'] ["foreground"] (NoArg (\cfg -> cfg { daemonForeground = True }))
        "Run in foreground (don't daemonize)",
    Option [] ["no-auto-start"] (NoArg (\cfg -> cfg { daemonAutoStart = False }))
        "Don't auto-start daemon when client connects",
    Option ['r'] ["restrictive"] (NoArg (\cfg -> cfg { daemonRestrictive = True }))
        "Use more restrictive sandbox settings"
    ]
  where
    splitOn :: Char -> String -> [String]
    splitOn c s = case break (== c) s of
        (chunk, []) -> [chunk]
        (chunk, _:rest) -> chunk : splitOn c rest

    parsePrivilegeModel :: String -> PrivilegeModel
    parsePrivilegeModel s = case toLower s of
        "always-drop" -> PrivilegeAlwaysDrop
        "selective" -> PrivilegeDropSelective
        "no-drop" -> PrivilegeNoDrop
        _ -> PrivilegeAlwaysDrop  -- Default to safest option

    parsePrivilegeTier :: String -> PrivilegeTier
    parsePrivilegeTier s = case toLower s of
        "daemon" -> Daemon
        "builder" -> Builder
        _ -> Builder  -- Default to safer option

    toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

-- | Parse daemon configuration from command-line arguments
parseConfigFromArgs :: [String] -> IO (Either String DaemonConfig)
parseConfigFromArgs args = do
    -- Get default configuration
    defaultCfg <- getDefaultConfig

    -- Parse command-line arguments
    case getOpt Permute configOptionDescriptions args of
        (options, _, []) -> do
            -- Apply options to default configuration
            let config = foldl (flip id) defaultCfg options
            -- Validate the resulting configuration
            return $ validateConfig config
        (_, _, errors) -> return $ Left $ concat errors

-- | Parse simple config arguments (subset for basic usage)
parseConfigArgs :: [String] -> IO DaemonConfig
parseConfigArgs args = do
    result <- parseConfigFromArgs args
    case result of
        Left err -> error $ "Invalid configuration arguments: " ++ err
        Right config -> return config

-- | Validate a daemon configuration
validateConfig :: DaemonConfig -> Either String DaemonConfig
validateConfig config = do
    -- Check for required fields
    when (null $ daemonSocketPath config) $
        Left "Socket path cannot be empty"

    when (null $ daemonStoreLocation config) $
        Left "Store location cannot be empty"

    when (daemonMaxJobs config <= 0) $
        Left "Maximum jobs must be positive"

    when (daemonBuildTimeout config < 0) $
        Left "Build timeout cannot be negative"

    when (daemonGcKeepLast config < 0) $
        Left "GC keep-last value cannot be negative"

    -- Check port range if specified
    case daemonPort config of
        Just port | port <= 0 || port > 65535 ->
            Left "Port number must be between 1 and 65535"
        _ -> Right ()

    -- Check privilege-related settings
    when (daemonAllowPrivilegeEscalation config && not (daemonRequireAuth config)) $
        Left "Cannot allow privilege escalation without authentication"

    -- No-drop privilege model is dangerous
    when (daemonPrivilegeModel config == PrivilegeNoDrop) $
        Left "PrivilegeNoDrop model is dangerous and not recommended"

    -- Return the validated config
    return config

-- | Execute a configuration operation with the required privilege level
-- Now properly typed with explicit singleton evidence
withConfigPrivilege ::
    forall (p :: Phase) (a :: Type).
    SingI p =>
    SPrivilegeTier 'Daemon ->
    DaemonConfig ->
    (DaemonConfig -> TenM p 'Daemon a) ->
    TenM p 'Daemon a
withConfigPrivilege st config operation = TenM $ \sp _ -> do
    -- Run the operation with daemon privileges
    let (TenM m) = operation config
    m sp st

-- | Validate whether an operation is allowed with the current privilege model
validatePrivilegeConfig ::
    forall (p :: Phase) (t :: PrivilegeTier).
    SingI p =>
    SPrivilegeTier t ->
    DaemonConfig ->
    PrivilegeTier ->
    TenM p t Bool
validatePrivilegeConfig st config requiredTier = TenM $ \sp _ -> do
    -- Check if the current privilege tier is sufficient
    case (fromSing st, requiredTier) of
        (Daemon, _) ->
            -- Daemon tier can do anything
            return True
        (Builder, Daemon) ->
            -- Builder tier can only escalate if allowed
            return $ daemonAllowPrivilegeEscalation config
        (Builder, Builder) ->
            -- Builder tier can perform Builder operations
            return True

-- | Check if an operation requires a specific privilege tier
requiresPrivilegeFor ::
    forall (p :: Phase) (t :: PrivilegeTier).
    SingI p =>
    SPrivilegeTier t ->
    DaemonConfig ->
    Text ->
    PrivilegeTier ->
    TenM p t ()
requiresPrivilegeFor st config operation requiredTier = TenM $ \sp _ -> do
    -- Check if we have sufficient privileges
    allowed <- case (fromSing st, requiredTier) of
        (Daemon, _) -> return True  -- Daemon can do anything
        (Builder, Builder) -> return True  -- Builder can do Builder operations
        (Builder, Daemon) ->
            -- Check if privilege escalation is allowed
            return $ daemonAllowPrivilegeEscalation config

    -- Throw an error if not allowed
    unless allowed $
        throwError $ privilegeError $ "Operation '" <> operation <> "' requires " <>
                                     T.pack (show requiredTier) <> " privileges"

-- | Make the DaemonConfig type an instance of ToJSON and FromJSON
instance Aeson.ToJSON DaemonConfig where
    toJSON DaemonConfig{..} = Aeson.object [
            "socketPath" .= daemonSocketPath,
            "port" .= daemonPort,
            "bindAddress" .= daemonBindAddress,
            "storeLocation" .= daemonStoreLocation,
            "stateFile" .= daemonStateFile,
            "tmpDir" .= daemonTmpDir,
            "maxJobs" .= daemonMaxJobs,
            "buildTimeout" .= daemonBuildTimeout,
            "keepFailed" .= daemonKeepFailed,
            "gcInterval" .= daemonGcInterval,
            "gcKeepLast" .= daemonGcKeepLast,
            "user" .= daemonUser,
            "group" .= daemonGroup,
            "allowedUsers" .= Set.toList daemonAllowedUsers,
            "requireAuth" .= daemonRequireAuth,
            "allowPrivilegeEscalation" .= daemonAllowPrivilegeEscalation,
            "privilegeModel" .= privilegeModelToText daemonPrivilegeModel,
            "defaultPrivilegeTier" .= privilegeTierToText daemonDefaultPrivilegeTier,
            "logFile" .= daemonLogFile,
            "logLevel" .= fromEnum daemonLogLevel,
            "foreground" .= daemonForeground,
            "autoStart" .= daemonAutoStart,
            "restrictive" .= daemonRestrictive
        ]
      where
        privilegeModelToText :: PrivilegeModel -> Text
        privilegeModelToText PrivilegeAlwaysDrop = "always-drop"
        privilegeModelToText PrivilegeDropSelective = "selective"
        privilegeModelToText PrivilegeNoDrop = "no-drop"

        privilegeTierToText :: PrivilegeTier -> Text
        privilegeTierToText Daemon = "daemon"
        privilegeTierToText Builder = "builder"

instance Aeson.FromJSON DaemonConfig where
    parseJSON = Aeson.withObject "DaemonConfig" $ \v -> do
        daemonSocketPath <- v .: "socketPath"
        daemonPort <- v .: "port"
        daemonBindAddress <- v .: "bindAddress"
        -- Try new field name first, fall back to old field name for backward compatibility
        daemonStoreLocation <- (v .: "storeLocation") <|> (v .: "storePath")
        daemonStateFile <- v .: "stateFile"
        daemonTmpDir <- v .: "tmpDir"
        daemonMaxJobs <- v .: "maxJobs"
        daemonBuildTimeout <- v .: "buildTimeout"
        daemonKeepFailed <- v .: "keepFailed"
        daemonGcInterval <- v .: "gcInterval"
        daemonGcKeepLast <- v .: "gcKeepLast"
        daemonUser <- v .: "user"
        daemonGroup <- v .: "group"
        allowedUsersList <- v .: "allowedUsers"
        daemonRequireAuth <- v .: "requireAuth"

        -- New privilege model fields, with fallbacks for older config files
        daemonAllowPrivilegeEscalation <- v .: "allowPrivilegeEscalation" <|> pure False
        privilegeModelText <- v .: "privilegeModel" <|> pure "always-drop"
        defaultTierText <- v .: "defaultPrivilegeTier" <|> pure "builder"

        daemonLogFile <- v .: "logFile"
        logLevelInt <- v .: "logLevel"
        daemonForeground <- v .: "foreground"
        daemonAutoStart <- v .: "autoStart"
        daemonRestrictive <- v .: "restrictive"

        let daemonAllowedUsers = Set.fromList allowedUsersList
        let daemonLogLevel = toEnum $ min 3 $ max 0 logLevelInt

        -- Parse privilege model
        let daemonPrivilegeModel = case privilegeModelText of
                "always-drop" -> PrivilegeAlwaysDrop
                "selective" -> PrivilegeDropSelective
                "no-drop" -> PrivilegeNoDrop
                _ -> PrivilegeAlwaysDrop  -- Default to safest

        -- Parse default privilege tier
        let daemonDefaultPrivilegeTier = case defaultTierText of
                "daemon" -> Daemon
                "builder" -> Builder
                _ -> Builder  -- Default to safer

        return DaemonConfig{..}
