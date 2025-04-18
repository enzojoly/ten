{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ten.Daemon.Config (
    -- Core configuration types
    DaemonConfig(..),
    LogLevel(..),

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
    getDefaultStorePath,
    getDefaultGcInterval,

    -- Command-line argument parsing
    configOptionDescriptions,
    parseConfigFromArgs
) where

import Control.Exception (try, SomeException)
import Control.Monad (when, unless)
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
import System.FilePath ((</>))
import System.Posix.User (getEffectiveUserID, getUserEntryForID, userName)
import qualified System.IO.Error as IOE

import Ten.Core (BuildError(..))

-- | Log levels for daemon operations
data LogLevel
    = LogQuiet     -- ^ Almost no logging (only critical errors)
    | LogNormal    -- ^ Normal logging level (warnings and important events)
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
    daemonStorePath     :: FilePath,      -- ^ Path to the content-addressable store
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

    -- Logging settings
    daemonLogFile       :: Maybe FilePath, -- ^ Path to log file (Nothing = stdout)
    daemonLogLevel      :: LogLevel,       -- ^ Logging verbosity

    -- Daemon behavior settings
    daemonForeground    :: Bool,          -- ^ Run in foreground (don't daemonize)
    daemonAutoStart     :: Bool,          -- ^ Auto-start daemon when client connects if not running
    daemonRestrictive   :: Bool           -- ^ Use more restrictive sandbox settings
} deriving (Show, Eq)

-- | Default daemon configuration
defaultDaemonConfig :: DaemonConfig
defaultDaemonConfig = DaemonConfig {
    -- Network defaults
    daemonSocketPath    = "/var/run/ten/daemon.sock",
    daemonPort          = Nothing,
    daemonBindAddress   = Nothing,

    -- Storage defaults - will be updated by getDefaultConfig
    daemonStorePath     = "/var/lib/ten/store",
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
            then defaultDaemonConfig
            else defaultDaemonConfig {
                -- Non-root appropriate paths
                daemonSocketPath    = xdgStateDir </> "daemon.sock",
                daemonStorePath     = xdgDataDir </> "store",
                daemonStateFile     = xdgStateDir </> "daemon-state.json",
                daemonTmpDir        = xdgStateDir </> "tmp",
                daemonLogFile       = Just (xdgStateDir </> "daemon.log"),
                -- Always run in foreground when not root
                daemonForeground    = True,
                -- Set current user as default allowed user
                daemonAllowedUsers  = Set.singleton currentUser
            }

    -- Create directories for the updated paths
    let dirsToCreate = [
            takeDirectory (daemonSocketPath config),
            daemonStorePath config,
            daemonTmpDir config,
            takeDirectory (daemonStateFile config),
            maybe "" takeDirectory (daemonLogFile config)
            ]

    mapM_ (createDirectoryIfMissing True) dirsToCreate

    return config
  where
    takeDirectory path =
        case break (== '/') (reverse path) of
            (_, []) -> "."
            (_, (_:rest)) -> reverse rest

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

-- | Get the default store path
getDefaultStorePath :: IO FilePath
getDefaultStorePath = do
    -- Check environment variable first
    envPath <- lookupEnv "TEN_STORE_PATH"
    case envPath of
        Just path -> return path
        Nothing -> do
            -- Use config default
            config <- getDefaultConfig
            return $ daemonStorePath config

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
saveConfigToFile :: FilePath -> DaemonConfig -> IO (Either String ())
saveConfigToFile path config = do
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
    mStorePath <- lookupEnv "TEN_STORE_PATH"
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
            daemonStorePath = fromMaybe (daemonStorePath defaultConfig) mStorePath,
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

    toLower = map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

    splitOn :: Char -> String -> [String]
    splitOn c s = case break (== c) s of
        (chunk, []) -> [chunk]
        (chunk, _:rest) -> chunk : splitOn c rest

    (<|>) :: Maybe a -> Maybe a -> Maybe a
    Nothing <|> x = x
    x <|> _ = x

-- | Command-line option descriptions for daemon configuration
configOptionDescriptions :: [OptDescr (DaemonConfig -> DaemonConfig)]
configOptionDescriptions = [
    Option ['s'] ["socket"] (ReqArg (\s cfg -> cfg { daemonSocketPath = s }) "PATH")
        "Path to daemon socket",
    Option ['p'] ["port"] (ReqArg (\p cfg -> cfg { daemonPort = Just (read p) }) "PORT")
        "TCP port for remote connections",
    Option ['b'] ["bind"] (ReqArg (\b cfg -> cfg { daemonBindAddress = Just b }) "ADDRESS")
        "Bind address for TCP connections",
    Option [] ["store"] (ReqArg (\s cfg -> cfg { daemonStorePath = s }) "PATH")
        "Path to store",
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

    when (daemonMaxJobs config <= 0) $
        Left "Maximum jobs must be positive"

    when (daemonBuildTimeout config < 0) $
        Left "Build timeout cannot be negative"

    when (daemonGcKeepLast config < 0) $
        Left "GC keep-last value cannot be negative"

    -- Check for valid port number if specified
    case daemonPort config of
        Just port | port <= 0 || port > 65535 ->
            Left "Port number must be between 1 and 65535"
        _ -> Right ()

    -- More validations could be added here

    -- Return the validated config
    return config

-- | Make the DaemonConfig type an instance of ToJSON and FromJSON
instance Aeson.ToJSON DaemonConfig where
    toJSON DaemonConfig{..} = Aeson.object [
            "socketPath" .= daemonSocketPath,
            "port" .= daemonPort,
            "bindAddress" .= daemonBindAddress,
            "storePath" .= daemonStorePath,
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
            "logFile" .= daemonLogFile,
            "logLevel" .= fromEnum daemonLogLevel,
            "foreground" .= daemonForeground,
            "autoStart" .= daemonAutoStart,
            "restrictive" .= daemonRestrictive
        ]

instance Aeson.FromJSON DaemonConfig where
    parseJSON = Aeson.withObject "DaemonConfig" $ \v -> do
        daemonSocketPath <- v .: "socketPath"
        daemonPort <- v .: "port"
        daemonBindAddress <- v .: "bindAddress"
        daemonStorePath <- v .: "storePath"
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
        daemonLogFile <- v .: "logFile"
        logLevelInt <- v .: "logLevel"
        daemonForeground <- v .: "foreground"
        daemonAutoStart <- v .: "autoStart"
        daemonRestrictive <- v .: "restrictive"

        let daemonAllowedUsers = Set.fromList allowedUsersList
        let daemonLogLevel = toEnum $ min 3 $ max 0 logLevelInt

        return DaemonConfig{..}
