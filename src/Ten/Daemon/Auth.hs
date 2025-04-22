{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ten.Daemon.Auth (
    -- Core authentication types
    UserCredentials(..),
    AuthToken(..),

    -- Authentication functions with privilege awareness
    authenticateUser,
    validateToken,
    generateToken,

    -- User management with privilege tier constraints
    getUserInfo,
    checkUserPermission,
    addUser,
    removeUser,
    changeUserPassword,
    changeUserPrivilegeTiers,

    -- Password functions
    hashPassword,
    verifyPassword,

    -- Permission types and functions
    Permission(..),
    UserPermissionLevel(..),
    PrivilegeRequirement(..),
    permissionToText,
    textToPermission,
    permissionLevelToText,
    textToPermissionLevel,
    defaultPermissions,
    permissionRequiresDaemon,

    -- System user integration
    getSystemUserInfo,
    systemUserExists,
    integrateWithSystemUser,

    -- Token management with privilege tier tracking
    TokenInfo(..),
    createToken,
    revokeToken,
    listUserTokens,
    refreshToken,
    tokenExpired,

    -- Privilege transitions
    withDaemonPrivilegeAsBuilder,
    hasPrivilegeForPermission,

    -- Auth file management
    loadAuthFile,
    saveAuthFile,
    migrateAuthFile,
    AuthDb(..),
    UserInfo(..),
    PasswordHash(..),

    -- Authentication errors
    AuthError(..)
) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (catch, throwIO, try, bracket, SomeException, Exception, finally)
import Control.Monad (void, when, unless, forM, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State (StateT, runStateT, get, put, modify)
import Control.Applicative (Alternative(..))
import Crypto.Hash (hash, digestFromByteString, Digest, SHA256, SHA512)
import qualified Crypto.Hash as Crypto
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Crypto.Random as CryptoRandom
import Data.Aeson ((.:), (.=), eitherDecode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef')
import Data.Kind (Type)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromMaybe, catMaybes, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Singletons
import Data.Singletons.TH
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime, diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale, parseTimeM)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import System.Directory (doesFileExist, createDirectoryIfMissing, renameFile, getPermissions, setPermissions)
import System.FilePath (takeDirectory)
import System.IO (withFile, IOMode(..), hClose, stderr, hPutStrLn)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (setFileMode)
import System.Posix.Types (UserID, GroupID)
import qualified System.Posix.User as PosixUser
import System.Random (randomIO, randomRIO)

-- Import Ten modules
import Ten.Core

-- | Authentication errors
data AuthError
    = InvalidCredentials
    | UserNotFound
    | TokenNotFound
    | TokenExpired
    | InsufficientPrivileges PrivilegeTier PrivilegeRequirement
    | AuthFileError Text
    | InvalidPassphrase
    | CryptoError Text
    | SystemError Text
    | PrivilegeEscalationDenied Text
    deriving (Show, Eq)

instance Exception AuthError

-- | User credentials for authentication with privilege tier request
data UserCredentials = UserCredentials {
    ucUsername :: Text,
    ucToken :: Text,
    ucRequestedTier :: PrivilegeTier  -- Requested privilege tier
} deriving (Show, Eq)

-- | User permission levels
data UserPermissionLevel
    = UserLevelNone       -- No access
    | UserLevelBasic      -- Basic access (read-only)
    | UserLevelStandard   -- Standard user (can build)
    | UserLevelAdvanced   -- Advanced user (can manage own builds)
    | UserLevelAdmin      -- Administrator (full access)
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Privilege requirement for operations
data PrivilegeRequirement
    = DaemonRequired      -- Requires daemon privileges
    | BuilderSufficient   -- Can be done with builder privileges
    deriving (Show, Eq)

-- | Specific permissions
data Permission
    = PermBuild              -- Can build derivations
    | PermCancelBuild        -- Can cancel builds
    | PermQueryBuild         -- Can query build status
    | PermQueryStore         -- Can query store contents
    | PermModifyStore        -- Can add to store
    | PermRunGC              -- Can run garbage collection
    | PermShutdown           -- Can shut down daemon
    | PermManageUsers        -- Can manage users
    | PermAdmin              -- Administrative access
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Token information with privilege tier tracking
data TokenInfo (t :: PrivilegeTier) = TokenInfo {
    tiToken :: Text,           -- The actual token value
    tiCreated :: UTCTime,      -- When the token was created
    tiExpires :: Maybe UTCTime, -- When the token expires (Nothing = no expiry)
    tiClientInfo :: Text,      -- Information about the client
    tiLastUsed :: UTCTime,     -- When the token was last used
    tiPermissions :: Set Permission, -- Specific permissions for this token
    tiPrivilegeEvidence :: SPrivilegeTier t  -- Runtime evidence of privilege tier
} deriving (Eq)

-- Implement show for TokenInfo to hide the SPrivilegeTier
instance Show (TokenInfo t) where
    show TokenInfo{..} = "TokenInfo { token = " ++ show tiToken ++
                         ", created = " ++ show tiCreated ++
                         ", expires = " ++ show tiExpires ++
                         ", clientInfo = " ++ show tiClientInfo ++
                         ", lastUsed = " ++ show tiLastUsed ++
                         ", permissions = " ++ show tiPermissions ++
                         ", privilegeTier = " ++ show (fromSing tiPrivilegeEvidence) ++ " }"

-- | Token wrapper that erases the phantom type
data SomeTokenInfo = forall t. SomeTokenInfo (TokenInfo t)

instance Show SomeTokenInfo where
    show (SomeTokenInfo token) = show token

instance Eq SomeTokenInfo where
    (SomeTokenInfo t1) == (SomeTokenInfo t2) = tiToken t1 == tiToken t2

-- | Stored password hash
data PasswordHash = PasswordHash {
    phAlgorithm :: Text,      -- Hash algorithm (e.g., "pbkdf2-sha512")
    phSalt :: BS.ByteString,  -- Salt value
    phHash :: BS.ByteString,  -- Actual hash value
    phIterations :: Int,      -- Number of iterations
    phCreated :: UTCTime      -- When this hash was created
} deriving (Show, Eq)

-- | User information with allowed privilege tiers
data UserInfo = UserInfo {
    uiUserId :: UserId,
    uiUsername :: Text,
    uiPasswordHash :: Maybe PasswordHash,
    uiPermissionLevel :: UserPermissionLevel,
    uiSpecificPermissions :: Set Permission,
    uiAllowedPrivilegeTiers :: Set PrivilegeTier, -- Which privilege tiers are allowed
    uiSystemUser :: Maybe Text,  -- Associated system user, if any
    uiTokens :: Map Text SomeTokenInfo,  -- Store tokens with erased types
    uiLastLogin :: Maybe UTCTime,
    uiCreated :: UTCTime
} deriving (Show, Eq)

-- | Authentication database
data AuthDb = AuthDb {
    adUsers :: Map Text UserInfo,  -- Keyed by username
    adSystemUserMap :: Map Text Text,  -- System username to Ten username
    adTokenMap :: Map Text Text,  -- Token to username
    adLastModified :: UTCTime,
    adAllowPrivilegeEscalation :: Bool  -- Whether to allow privilege escalation
} deriving (Show, Eq)

-- | Get permission requirement for a specific permission
getPermissionRequirement :: Permission -> PrivilegeRequirement
getPermissionRequirement PermModifyStore = DaemonRequired
getPermissionRequirement PermRunGC = DaemonRequired
getPermissionRequirement PermShutdown = DaemonRequired
getPermissionRequirement PermManageUsers = DaemonRequired
getPermissionRequirement PermAdmin = DaemonRequired
getPermissionRequirement _ = BuilderSufficient

-- | Check if a permission requires daemon privileges
permissionRequiresDaemon :: Permission -> Bool
permissionRequiresDaemon perm = getPermissionRequirement perm == DaemonRequired

-- | Convert permission level to text
permissionLevelToText :: UserPermissionLevel -> Text
permissionLevelToText UserLevelNone = "none"
permissionLevelToText UserLevelBasic = "basic"
permissionLevelToText UserLevelStandard = "standard"
permissionLevelToText UserLevelAdvanced = "advanced"
permissionLevelToText UserLevelAdmin = "admin"

-- | Parse permission level from text
textToPermissionLevel :: Text -> Maybe UserPermissionLevel
textToPermissionLevel "none" = Just UserLevelNone
textToPermissionLevel "basic" = Just UserLevelBasic
textToPermissionLevel "standard" = Just UserLevelStandard
textToPermissionLevel "advanced" = Just UserLevelAdvanced
textToPermissionLevel "admin" = Just UserLevelAdmin
textToPermissionLevel _ = Nothing

-- | Convert permission to text
permissionToText :: Permission -> Text
permissionToText PermBuild = "build"
permissionToText PermCancelBuild = "cancel-build"
permissionToText PermQueryBuild = "query-build"
permissionToText PermQueryStore = "query-store"
permissionToText PermModifyStore = "modify-store"
permissionToText PermRunGC = "run-gc"
permissionToText PermShutdown = "shutdown"
permissionToText PermManageUsers = "manage-users"
permissionToText PermAdmin = "admin"

-- | Parse permission from text
textToPermission :: Text -> Maybe Permission
textToPermission "build" = Just PermBuild
textToPermission "cancel-build" = Just PermCancelBuild
textToPermission "query-build" = Just PermQueryBuild
textToPermission "query-store" = Just PermQueryStore
textToPermission "modify-store" = Just PermModifyStore
textToPermission "run-gc" = Just PermRunGC
textToPermission "shutdown" = Just PermShutdown
textToPermission "manage-users" = Just PermManageUsers
textToPermission "admin" = Just PermAdmin
textToPermission _ = Nothing

-- | Default permissions for permission levels
defaultPermissions :: UserPermissionLevel -> Set Permission
defaultPermissions UserLevelNone = Set.empty
defaultPermissions UserLevelBasic = Set.fromList [
    PermQueryBuild,
    PermQueryStore
    ]
defaultPermissions UserLevelStandard = Set.fromList [
    PermBuild,
    PermCancelBuild,
    PermQueryBuild,
    PermQueryStore
    ]
defaultPermissions UserLevelAdvanced = Set.fromList [
    PermBuild,
    PermCancelBuild,
    PermQueryBuild,
    PermQueryStore,
    PermModifyStore,
    PermRunGC
    ]
defaultPermissions UserLevelAdmin = Set.fromList [
    PermBuild,
    PermCancelBuild,
    PermQueryBuild,
    PermQueryStore,
    PermModifyStore,
    PermRunGC,
    PermShutdown,
    PermManageUsers,
    PermAdmin
    ]

-- | Default allowed privilege tiers for permission levels
defaultAllowedTiers :: UserPermissionLevel -> Set PrivilegeTier
defaultAllowedTiers UserLevelNone = Set.singleton Builder
defaultAllowedTiers UserLevelBasic = Set.singleton Builder
defaultAllowedTiers UserLevelStandard = Set.singleton Builder
defaultAllowedTiers UserLevelAdvanced = Set.singleton Builder
defaultAllowedTiers UserLevelAdmin = Set.fromList [Builder, Daemon]

-- | Check if a password matches a hash
verifyPassword :: Text -> PasswordHash -> Bool
verifyPassword password PasswordHash{..} =
    case phAlgorithm of
        "pbkdf2-sha512" ->
            let calculatedHash = pbkdf2Hash (TE.encodeUtf8 password) phSalt phIterations 64
            in calculatedHash == phHash
        _ -> False  -- Unknown algorithm

-- | Hash a password using PBKDF2-SHA512
hashPassword :: Text -> IO PasswordHash
hashPassword password = do
    -- Generate a random 16-byte salt
    salt <- BS.pack <$> replicateM 16 (randomRIO (0, 255))
    now <- getCurrentTime

    -- Use 100,000 iterations (can be adjusted based on security requirements)
    -- NIST SP 800-132 recommends at least 10,000 iterations
    let iterations = 100000
    let hash = pbkdf2Hash (TE.encodeUtf8 password) salt iterations 64

    return PasswordHash {
        phAlgorithm = "pbkdf2-sha512",
        phSalt = salt,
        phHash = hash,
        phIterations = iterations,
        phCreated = now
    }

-- | Generate PBKDF2 hash
pbkdf2Hash :: BS.ByteString -> BS.ByteString -> Int -> Int -> BS.ByteString
pbkdf2Hash password salt iterations keyLen =
    PBKDF2.fastPBKDF2_SHA512
        (PBKDF2.Parameters iterations keyLen)
        password
        salt

-- | Generate a random token with high entropy
generateToken :: IO Text
generateToken = do
    -- Generate UUIDs for randomness
    uuid1 <- UUID.toString <$> UUID.nextRandom
    uuid2 <- UUID.toString <$> UUID.nextRandom

    -- Add timestamp for uniqueness
    timestamp <- formatTime defaultTimeLocale "%Y%m%d%H%M%S%q" <$> getCurrentTime

    -- Add more cryptographic randomness (32 bytes = 256 bits of entropy)
    randomBytes <- CryptoRandom.getRandomBytes 32 :: IO BS.ByteString

    -- Combine all sources of randomness and hash
    let tokenInput = T.pack uuid1 <> T.pack uuid2 <> T.pack timestamp <> TE.decodeUtf8 (Base64.encode randomBytes)
    let tokenHash = hashText tokenInput

    -- Return prefixed token
    return $ "ten_" <> encodeBase64 tokenHash

-- | Generate a Base64-encoded hash of text
hashText :: Text -> BS.ByteString
hashText text =
    BA.convert (Crypto.hash (TE.encodeUtf8 text) :: Digest SHA256)

-- | Encode ByteString as Base64
encodeBase64 :: BS.ByteString -> Text
encodeBase64 = TE.decodeUtf8 . Base64.encode

-- | Decode Base64 to ByteString
decodeBase64 :: Text -> Maybe BS.ByteString
decodeBase64 text =
    case Base64.decode (TE.encodeUtf8 text) of
        Left _ -> Nothing
        Right bs -> Just bs

-- | Create a new token for a user with specific privilege tier
createToken :: forall (t :: PrivilegeTier). SingI t =>
    UserInfo -> Text -> Maybe Int -> Set Permission -> IO (TokenInfo t)
createToken user clientInfo expirySeconds permissions = do
    token <- generateToken
    now <- getCurrentTime

    -- Calculate expiry time if provided
    let expires = case expirySeconds of
            Just seconds -> Just $ addUTCTime (fromIntegral seconds) now
            Nothing -> Nothing

    -- Get singleton evidence for the privilege tier
    let evidence = sing @t

    -- Create token info with specified permissions
    return TokenInfo {
        tiToken = token,
        tiCreated = now,
        tiExpires = expires,
        tiClientInfo = clientInfo,
        tiLastUsed = now,
        tiPermissions = permissions,
        tiPrivilegeEvidence = evidence
    }

-- | Create a builder token from a daemon token (privilege downgrade only)
withDaemonPrivilegeAsBuilder :: forall a.
    TokenInfo 'Daemon -> (TokenInfo 'Builder -> IO a) -> IO a
withDaemonPrivilegeAsBuilder token action = do
    -- Create a new token with Builder privilege
    let builderEvidence = sBuilder
    let builderToken = TokenInfo {
            tiToken = tiToken token,
            tiCreated = tiCreated token,
            tiExpires = tiExpires token,
            tiClientInfo = tiClientInfo token,
            tiLastUsed = tiLastUsed token,
            tiPermissions = Set.filter (not . permissionRequiresDaemon) (tiPermissions token),
            tiPrivilegeEvidence = builderEvidence
        }
    -- Run the action with the builder token
    action builderToken

-- | Check if a token has expired
tokenExpired :: TokenInfo t -> IO Bool
tokenExpired TokenInfo{..} = do
    now <- getCurrentTime
    return $ case tiExpires of
        Just expiry -> now > expiry
        Nothing -> False

-- | Get information about a system user
getSystemUserInfo :: Text -> IO (Maybe (UserID, Text, GroupID))
getSystemUserInfo username = do
    -- First get the user entry
    userResult <- try $ PosixUser.getUserEntryForName (T.unpack username)
    case userResult of
        Left (_ :: SomeException) -> return Nothing
        Right userEntry -> do
            -- Then get the primary group for this user
            let groupName = T.pack $ PosixUser.userName userEntry
            groupResult <- try $ PosixUser.getGroupEntryForName (PosixUser.userName userEntry)
            case groupResult of
                Left (_ :: SomeException) ->
                    -- Fallback to a default group if needed
                    return $ Just (PosixUser.userID userEntry, groupName, 0)
                Right groupEntry ->
                    return $ Just (PosixUser.userID userEntry, groupName, PosixUser.groupID groupEntry)

-- | Check if a system user exists
systemUserExists :: Text -> IO Bool
systemUserExists username = do
    userInfo <- getSystemUserInfo username
    return $ isJust userInfo

-- | Integrate a Ten user with a system user
integrateWithSystemUser :: AuthDb -> Text -> Text -> IO AuthDb
integrateWithSystemUser db tenUsername sysUsername = do
    -- Check if both users exist
    let tenUser = Map.lookup tenUsername (adUsers db)
    sysUserExists <- systemUserExists sysUsername

    case (tenUser, sysUserExists) of
        (Nothing, _) -> throwIO $ AuthError $ AuthFileError $ "Ten user does not exist: " <> tenUsername
        (_, False) -> throwIO $ AuthError $ SystemError $ "System user does not exist: " <> sysUsername
        (Just user, True) -> do
            -- Update the user info
            let updatedUser = user { uiSystemUser = Just sysUsername }

            -- Update the database
            now <- getCurrentTime
            return db {
                adUsers = Map.insert tenUsername updatedUser (adUsers db),
                adSystemUserMap = Map.insert sysUsername tenUsername (adSystemUserMap db),
                adLastModified = now
            }

-- | Check if a user has permission for a specific privilege tier
hasPrivilegeForPermission :: UserInfo -> Permission -> PrivilegeTier -> Bool
hasPrivilegeForPermission user permission requestedTier =
    -- Check if user has the permission
    permission `Set.member` uiSpecificPermissions user &&
    -- Check if user is allowed to use the requested tier
    requestedTier `Set.member` uiAllowedPrivilegeTiers user &&
    -- Check if permission is compatible with requested tier
    (requestedTier == Daemon || not (permissionRequiresDaemon permission))

-- | Load the authentication database
loadAuthFile :: FilePath -> IO AuthDb
loadAuthFile path = do
    exists <- doesFileExist path
    if not exists
        then do
            -- Create a new empty database
            now <- getCurrentTime
            return AuthDb {
                adUsers = Map.empty,
                adSystemUserMap = Map.empty,
                adTokenMap = Map.empty,
                adLastModified = now,
                adAllowPrivilegeEscalation = False  -- Default to secure setting
            }
        else do
            -- Load the database from file
            content <- BS.readFile path
            case Aeson.eitherDecodeStrict content of
                Left err -> throwIO $ AuthError $ AuthFileError $ "Failed to parse auth file: " <> T.pack err
                Right db -> return db

-- | Save the authentication database
saveAuthFile :: FilePath -> AuthDb -> IO ()
saveAuthFile path db = do
    -- Create the directory if it doesn't exist
    createDirectoryIfMissing True (takeDirectory path)

    -- Write the database to a temporary file first
    let tempPath = path <> ".tmp"
    BS.writeFile tempPath (LBS.toStrict $ Aeson.encode db)

    -- Set appropriate permissions (600 - owner read/write only)
    setFileMode tempPath 0o600

    -- Atomically replace the original file
    renameFile tempPath path

    -- Ensure proper permissions on the final file
    setFileMode path 0o600

-- | Migrate an authentication file to a new format
migrateAuthFile :: FilePath -> FilePath -> IO ()
migrateAuthFile oldPath newPath = do
    -- Load the old database
    oldDb <- loadAuthFile oldPath

    -- Save it in the new format
    saveAuthFile newPath oldDb

-- | Authenticate a user and generate a token with privilege validation
authenticateUser ::
    AuthDb ->
    Text ->
    Text ->
    Text ->
    PrivilegeTier ->
    IO (AuthDb, UserId, AuthToken, SPrivilegeTier t)
authenticateUser db username password clientInfo requestedTier = do
    -- Check if the user exists
    user <- case Map.lookup username (adUsers db) of
        Just u -> return u
        Nothing -> throwIO InvalidCredentials

    -- Verify the requested privilege tier is allowed for this user
    unless (requestedTier `Set.member` uiAllowedPrivilegeTiers user) $
        throwIO $ AuthError $ InsufficientPrivileges requestedTier DaemonRequired

    -- Check for privilege escalation policy
    when (requestedTier == Daemon && not (adAllowPrivilegeEscalation db)) $
        throwIO $ AuthError $ PrivilegeEscalationDenied "Privilege escalation is disabled globally"

    -- Authenticate the user
    now <- getCurrentTime
    case uiPasswordHash user of
        -- If no password is set (e.g., for system users), skip password check
        Nothing -> do
            -- For system users, ensure they're allowed
            sysUser <- case uiSystemUser user of
                Just sysName -> do
                    exists <- systemUserExists sysName
                    if exists then return sysName else throwIO InvalidCredentials
                Nothing -> throwIO InvalidCredentials

            -- Create token based on requested privilege tier
            case requestedTier of
                Daemon -> do
                    let (newDb, userId, authToken, privEvidence) = createUserToken @'Daemon db username user clientInfo now
                    return (newDb, userId, authToken, privEvidence)
                Builder -> do
                    let (newDb, userId, authToken, privEvidence) = createUserToken @'Builder db username user clientInfo now
                    return (newDb, userId, authToken, privEvidence)

        -- If a password is set, validate it
        Just passwordHash -> do
            unless (verifyPassword password passwordHash) $
                throwIO InvalidCredentials

            -- Create token based on requested privilege tier
            case requestedTier of
                Daemon -> do
                    let (newDb, userId, authToken, privEvidence) = createUserToken @'Daemon db username user clientInfo now
                    return (newDb, userId, authToken, privEvidence)
                Builder -> do
                    let (newDb, userId, authToken, privEvidence) = createUserToken @'Builder db username user clientInfo now
                    return (newDb, userId, authToken, privEvidence)

-- Helper to create a token with proper privilege tier
createUserToken :: forall (t :: PrivilegeTier).
    (SingI t) =>
    AuthDb ->
    Text ->
    UserInfo ->
    Text ->
    UTCTime ->
    (AuthDb, UserId, AuthToken, SPrivilegeTier t)
createUserToken db username user clientInfo now = do
    -- Filter permissions based on privilege tier
    let permissions = if (fromSing (sing @t)) == Builder
                        then Set.filter (not . permissionRequiresDaemon) (uiSpecificPermissions user)
                        else uiSpecificPermissions user

    -- Create token with appropriate permissions
    let tokenStr = "ten_" <> T.pack (show (hash (TE.encodeUtf8 (username <> T.pack (show now)))))

    -- Create token info with runtime evidence
    let tokenInfo = TokenInfo {
            tiToken = tokenStr,
            tiCreated = now,
            tiExpires = Just $ addUTCTime (24 * 3600) now, -- 24 hours
            tiClientInfo = clientInfo,
            tiLastUsed = now,
            tiPermissions = permissions,
            tiPrivilegeEvidence = sing @t
        }

    -- Wrap in SomeTokenInfo to erase phantom type
    let someToken = SomeTokenInfo tokenInfo

    -- Update user with new token
    let updatedUser = user {
            uiTokens = Map.insert tokenStr someToken (uiTokens user),
            uiLastLogin = Just now
        }

    -- Update database
    let updatedDb = db {
            adUsers = Map.insert username updatedUser (adUsers db),
            adTokenMap = Map.insert tokenStr username (adTokenMap db),
            adLastModified = now
        }

    -- Return updated database, user ID, auth token, and privilege evidence
    (updatedDb, uiUserId user, AuthToken tokenStr, sing @t)

-- | Validate a token and return user ID with privilege evidence
validateToken :: forall t. SingI t =>
    AuthDb ->
    AuthToken ->
    IO (Maybe (UserId, Set Permission, SPrivilegeTier t))
validateToken db (AuthToken token) = do
    -- Check if the token exists
    case Map.lookup token (adTokenMap db) of
        Nothing -> return Nothing
        Just username -> do
            -- Check if the user exists
            case Map.lookup username (adUsers db) of
                Nothing -> return Nothing
                Just user -> do
                    -- Check if the token exists for this user
                    case Map.lookup token (uiTokens user) of
                        Nothing -> return Nothing
                        Just (SomeTokenInfo tokenInfo) -> do
                            -- Check if the token has expired
                            expired <- tokenExpired tokenInfo
                            if expired
                                then return Nothing
                                else do
                                    -- Extract privilege tier and match with requested tier
                                    case fromSing (tiPrivilegeEvidence tokenInfo) of
                                        tier | tier == fromSing (sing @t) ->
                                            -- Token has matching privilege tier, cast and return
                                            return $ Just (uiUserId user, tiPermissions tokenInfo, sing @t)
                                        _ ->
                                            -- Token has different privilege tier, return Nothing
                                            return Nothing

-- | Refresh an auth token preserving privilege tier
refreshToken :: forall (t :: PrivilegeTier). SingI t =>
    AuthDb ->
    AuthToken ->
    IO (AuthDb, AuthToken)
refreshToken db (AuthToken token) = do
    -- Check if the token exists
    case Map.lookup token (adTokenMap db) of
        Nothing -> throwIO $ AuthError TokenNotFound
        Just username -> do
            -- Check if the user exists
            case Map.lookup username (adUsers db) of
                Nothing -> throwIO $ AuthError UserNotFound
                Just user -> do
                    -- Check if the token exists for this user
                    case Map.lookup token (uiTokens user) of
                        Nothing -> throwIO $ AuthError TokenNotFound
                        Just (SomeTokenInfo tokenInfo) -> do
                            -- Check if the token has expired
                            expired <- tokenExpired tokenInfo
                            if expired
                                then throwIO $ AuthError TokenExpired
                                else do
                                    -- Create a new token with the same permissions and privilege
                                    now <- getCurrentTime

                                    -- Generate new token string
                                    newTokenStr <- generateToken

                                    -- Preserve the privilege tier
                                    let tier = fromSing (tiPrivilegeEvidence tokenInfo)

                                    -- Create new token based on privilege tier
                                    case tier of
                                        Daemon -> do
                                            updatedDb <- refreshTokenWithTier @'Daemon db username user now newTokenStr tokenInfo
                                            return (updatedDb, AuthToken newTokenStr)
                                        Builder -> do
                                            updatedDb <- refreshTokenWithTier @'Builder db username user now newTokenStr tokenInfo
                                            return (updatedDb, AuthToken newTokenStr)

-- Helper to refresh token with specific privilege tier
refreshTokenWithTier :: forall (t :: PrivilegeTier) (s :: PrivilegeTier). SingI t =>
    AuthDb ->
    Text ->
    UserInfo ->
    UTCTime ->
    Text ->
    TokenInfo s ->
    IO AuthDb
refreshTokenWithTier db username user now newTokenStr oldToken = do
    -- Create new token info preserving permissions but with new expiry
    let oldExpiry = tiExpires oldToken
    let newExpiry = case oldExpiry of
            Nothing -> Nothing
            Just expiry ->
                let remaining = diffUTCTime expiry now
                in if remaining > 0
                    then Just $ addUTCTime remaining now
                    else Just $ addUTCTime (24 * 3600) now  -- Default to 24 hours if expired

    -- Create new token info with proper privilege evidence
    let newTokenInfo = TokenInfo {
            tiToken = newTokenStr,
            tiCreated = now,
            tiExpires = newExpiry,
            tiClientInfo = tiClientInfo oldToken,
            tiLastUsed = now,
            tiPermissions = tiPermissions oldToken,
            tiPrivilegeEvidence = sing @t
        }

    -- Wrap in SomeTokenInfo
    let someToken = SomeTokenInfo newTokenInfo

    -- Update user tokens
    let updatedTokens = Map.delete (tiToken oldToken) (uiTokens user)
    let updatedTokens' = Map.insert newTokenStr someToken updatedTokens
    let updatedUser = user { uiTokens = updatedTokens' }

    -- Update token map
    let updatedTokenMap = Map.delete (tiToken oldToken) (adTokenMap db)
    let updatedTokenMap' = Map.insert newTokenStr username updatedTokenMap

    -- Update database
    return db {
        adUsers = Map.insert username updatedUser (adUsers db),
        adTokenMap = updatedTokenMap',
        adLastModified = now
    }

-- | Revoke a token
revokeToken :: AuthDb -> AuthToken -> IO AuthDb
revokeToken db (AuthToken token) = do
    -- Check if the token exists
    case Map.lookup token (adTokenMap db) of
        Nothing -> return db  -- Token doesn't exist, nothing to do
        Just username -> do
            -- Check if the user exists
            case Map.lookup username (adUsers db) of
                Nothing -> return db  -- User doesn't exist, nothing to do
                Just user -> do
                    -- Check if the token exists for this user
                    if not (Map.member token (uiTokens user))
                        then return db  -- Token doesn't exist for this user
                        else do
                            -- Update the database
                            now <- getCurrentTime
                            let updatedTokens = Map.delete token (uiTokens user)
                            let updatedUser = user { uiTokens = updatedTokens }

                            let updatedUsers = Map.insert username updatedUser (adUsers db)
                            let updatedTokenMap = Map.delete token (adTokenMap db)

                            return db {
                                adUsers = updatedUsers,
                                adTokenMap = updatedTokenMap,
                                adLastModified = now
                            }

-- | List all tokens for a user
listUserTokens :: AuthDb -> Text -> IO [SomeTokenInfo]
listUserTokens db username = do
    -- Check if the user exists
    case Map.lookup username (adUsers db) of
        Nothing -> throwIO $ AuthError UserNotFound
        Just user -> return $ Map.elems (uiTokens user)

-- | Get user information
getUserInfo :: AuthDb -> Text -> IO UserInfo
getUserInfo db username = do
    -- Check if the user exists
    case Map.lookup username (adUsers db) of
        Nothing -> throwIO $ AuthError UserNotFound
        Just user -> return user

-- | Add a new user
addUser :: AuthDb -> Text -> Text -> UserPermissionLevel -> IO AuthDb
addUser db username password permLevel = do
    -- Check if the user already exists
    when (Map.member username (adUsers db)) $
        throwIO $ AuthError $ AuthFileError $ "User already exists: " <> username

    -- Create the user
    now <- getCurrentTime
    passwordHash <- hashPassword password
    let userId = UserId $ "user_" <> username <> "_" <> T.pack (formatTime defaultTimeLocale "%Y%m%d%H%M%S" now)

    let user = UserInfo {
            uiUserId = userId,
            uiUsername = username,
            uiPasswordHash = Just passwordHash,
            uiPermissionLevel = permLevel,
            uiSpecificPermissions = defaultPermissions permLevel,
            uiAllowedPrivilegeTiers = defaultAllowedTiers permLevel,
            uiSystemUser = Nothing,
            uiTokens = Map.empty,
            uiLastLogin = Nothing,
            uiCreated = now
        }

    -- Update the database
    return db {
        adUsers = Map.insert username user (adUsers db),
        adLastModified = now
    }

-- | Remove a user
removeUser :: AuthDb -> Text -> IO AuthDb
removeUser db username = do
    -- Check if the user exists
    case Map.lookup username (adUsers db) of
        Nothing -> return db  -- User doesn't exist, nothing to do
        Just user -> do
            -- Update the database
            now <- getCurrentTime

            -- Remove all tokens for this user
            let updatedTokenMap = foldl (\m t -> Map.delete t m)
                                      (adTokenMap db)
                                      (Map.keys $ uiTokens user)

            -- Remove system user mapping if present
            let updatedSysMap = case uiSystemUser user of
                    Just sysUser -> Map.delete sysUser (adSystemUserMap db)
                    Nothing -> adSystemUserMap db

            return db {
                adUsers = Map.delete username (adUsers db),
                adSystemUserMap = updatedSysMap,
                adTokenMap = updatedTokenMap,
                adLastModified = now
            }

-- | Change a user's password
changeUserPassword :: AuthDb -> Text -> Text -> IO AuthDb
changeUserPassword db username newPassword = do
    -- Check if the user exists
    case Map.lookup username (adUsers db) of
        Nothing -> throwIO $ AuthError UserNotFound
        Just user -> do
            -- Hash the new password
            passwordHash <- hashPassword newPassword

            -- Update the user
            now <- getCurrentTime
            let updatedUser = user { uiPasswordHash = Just passwordHash }

            -- Update the database
            return db {
                adUsers = Map.insert username updatedUser (adUsers db),
                adLastModified = now
            }

-- | Change a user's allowed privilege tiers
changeUserPrivilegeTiers :: AuthDb -> Text -> Set PrivilegeTier -> IO AuthDb
changeUserPrivilegeTiers db username newTiers = do
    -- Check if the user exists
    case Map.lookup username (adUsers db) of
        Nothing -> throwIO $ AuthError UserNotFound
        Just user -> do
            -- Update the user
            now <- getCurrentTime
            let updatedUser = user { uiAllowedPrivilegeTiers = newTiers }

            -- Update the database
            return db {
                adUsers = Map.insert username updatedUser (adUsers db),
                adLastModified = now
            }

-- | Check if a user has a specific permission
checkUserPermission :: AuthDb -> UserId -> Permission -> PrivilegeTier -> IO Bool
checkUserPermission db userId permission requestedTier = do
    -- Find the user by ID
    let mUser = findUserById db userId
    case mUser of
        Nothing -> return False
        Just user -> do
            -- Check if the user has the permission in the requested tier
            return $ hasPrivilegeForPermission user permission requestedTier

-- | Find a user by ID
findUserById :: AuthDb -> UserId -> Maybe UserInfo
findUserById db (UserId userId) =
    -- Look through all users to find one with matching ID
    Map.foldl (\result user ->
        if isJust result
            then result
            else if userId == case uiUserId user of UserId uid -> uid
                then Just user
                else Nothing
        ) Nothing (adUsers db)

-- JSON instances for auth types

instance Aeson.ToJSON PasswordHash where
    toJSON PasswordHash{..} = Aeson.object [
            "algorithm" .= phAlgorithm,
            "salt" .= encodeBase64 phSalt,
            "hash" .= encodeBase64 phHash,
            "iterations" .= phIterations,
            "created" .= phCreated
        ]

instance Aeson.FromJSON PasswordHash where
    parseJSON = Aeson.withObject "PasswordHash" $ \v -> do
        phAlgorithm <- v .: "algorithm"
        saltText <- v .: "salt"
        hashText <- v .: "hash"
        phIterations <- v .: "iterations"
        phCreated <- v .: "created"

        let mSalt = decodeBase64 saltText
        let mHash = decodeBase64 hashText

        case (mSalt, mHash) of
            (Just phSalt, Just phHash) -> return PasswordHash{..}
            _ -> fail "Invalid base64 encoding in password hash"

-- Serialize SomeTokenInfo (type-erased)
instance Aeson.ToJSON SomeTokenInfo where
    toJSON (SomeTokenInfo token) = Aeson.object [
            "token" .= tiToken token,
            "created" .= tiCreated token,
            "expires" .= tiExpires token,
            "clientInfo" .= tiClientInfo token,
            "lastUsed" .= tiLastUsed token,
            "permissions" .= map permissionToText (Set.toList (tiPermissions token)),
            "privilegeTier" .= (case fromSing (tiPrivilegeEvidence token) of
                                 Daemon -> "daemon"
                                 Builder -> "builder")
        ]

instance Aeson.FromJSON SomeTokenInfo where
    parseJSON = Aeson.withObject "TokenInfo" $ \v -> do
        token <- v .: "token"
        created <- v .: "created"
        expires <- v .: "expires"
        clientInfo <- v .: "clientInfo"
        lastUsed <- v .: "lastUsed"
        permissionTexts <- v .: "permissions"
        tierText <- v .: "privilegeTier"

        -- Parse permissions
        let mPermissions = mapM textToPermission permissionTexts
        permissions <- case mPermissions of
            Just perms -> return $ Set.fromList perms
            Nothing -> fail "Invalid permission in token"

        -- Create token with appropriate privilege tier
        case tierText of
            "daemon" -> do
                let tokenInfo = TokenInfo {
                        tiToken = token,
                        tiCreated = created,
                        tiExpires = expires,
                        tiClientInfo = clientInfo,
                        tiLastUsed = lastUsed,
                        tiPermissions = permissions,
                        tiPrivilegeEvidence = sDaemon
                    }
                return $ SomeTokenInfo tokenInfo
            "builder" -> do
                let tokenInfo = TokenInfo {
                        tiToken = token,
                        tiCreated = created,
                        tiExpires = expires,
                        tiClientInfo = clientInfo,
                        tiLastUsed = lastUsed,
                        tiPermissions = permissions,
                        tiPrivilegeEvidence = sBuilder
                    }
                return $ SomeTokenInfo tokenInfo
            _ -> fail $ "Invalid privilege tier: " ++ T.unpack tierText

instance Aeson.ToJSON UserInfo where
    toJSON UserInfo{..} = Aeson.object [
            "userId" .= case uiUserId of UserId uid -> uid,
            "username" .= uiUsername,
            "passwordHash" .= uiPasswordHash,
            "permissionLevel" .= permissionLevelToText uiPermissionLevel,
            "specificPermissions" .= map permissionToText (Set.toList uiSpecificPermissions),
            "allowedPrivilegeTiers" .= map showTier (Set.toList uiAllowedPrivilegeTiers),
            "systemUser" .= uiSystemUser,
            "tokens" .= uiTokens,
            "lastLogin" .= uiLastLogin,
            "created" .= uiCreated
        ]
      where
        showTier Daemon = "daemon"
        showTier Builder = "builder"

instance Aeson.FromJSON UserInfo where
    parseJSON = Aeson.withObject "UserInfo" $ \v -> do
        userId <- v .: "userId"
        uiUsername <- v .: "username"
        uiPasswordHash <- v .: "passwordHash"
        permLevelText <- v .: "permissionLevel"
        permissionTexts <- v .: "specificPermissions"
        tierTexts <- v .: "allowedPrivilegeTiers"
        uiSystemUser <- v .: "systemUser"
        uiTokens <- v .: "tokens"
        uiLastLogin <- v .: "lastLogin"
        uiCreated <- v .: "created"

        let uiUserId = UserId userId

        let mPermLevel = textToPermissionLevel permLevelText
        uiPermissionLevel <- case mPermLevel of
            Just level -> return level
            Nothing -> fail $ "Invalid permission level: " ++ T.unpack permLevelText

        let mPermissions = mapM textToPermission permissionTexts
        uiSpecificPermissions <- case mPermissions of
            Just perms -> return $ Set.fromList perms
            Nothing -> fail "Invalid permission in user"

        -- Parse privilege tiers
        let parseTier "daemon" = Just Daemon
            parseTier "builder" = Just Builder
            parseTier _ = Nothing
        let mTiers = mapM parseTier tierTexts
        uiAllowedPrivilegeTiers <- case mTiers of
            Just tiers -> return $ Set.fromList tiers
            Nothing -> fail "Invalid privilege tier in user"

        return UserInfo{..}

instance Aeson.ToJSON AuthDb where
    toJSON AuthDb{..} = Aeson.object [
            "users" .= adUsers,
            "systemUserMap" .= adSystemUserMap,
            "tokenMap" .= adTokenMap,
            "lastModified" .= adLastModified,
            "allowPrivilegeEscalation" .= adAllowPrivilegeEscalation
        ]

instance Aeson.FromJSON AuthDb where
    parseJSON = Aeson.withObject "AuthDb" $ \v -> do
        adUsers <- v .: "users"
        adSystemUserMap <- v .: "systemUserMap"
        adTokenMap <- v .: "tokenMap"
        adLastModified <- v .: "lastModified"
        adAllowPrivilegeEscalation <- v .: "allowPrivilegeEscalation" Control.Applicative.<|> pure False  -- Default to secure setting

        return AuthDb{..}

-- | Repeat an action n times
replicateM :: Int -> IO a -> IO [a]
replicateM n action
    | n <= 0 = return []
    | otherwise = do
        x <- action
        xs <- replicateM (n-1) action
        return (x:xs)

-- We use Control.Applicative.<|> directly for Aeson parsing
