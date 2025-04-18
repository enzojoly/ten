{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Ten.Daemon.Auth (
    -- Core authentication types
    UserCredentials(..),
    AuthToken(..),

    -- Authentication functions
    authenticateUser,
    validateToken,
    generateToken,

    -- User management
    getUserInfo,
    checkUserPermission,
    addUser,
    removeUser,
    changeUserPassword,

    -- Password functions
    hashPassword,
    verifyPassword,

    -- Permission types and functions
    Permission(..),
    UserPermissionLevel(..),
    permissionToText,
    textToPermission,
    permissionLevelToText,
    textToPermissionLevel,
    defaultPermissions,

    -- System user integration
    getSystemUserInfo,
    systemUserExists,
    integrateWithSystemUser,

    -- Token management
    TokenInfo(..),
    createToken,
    revokeToken,
    listUserTokens,
    refreshToken,
    tokenExpired,

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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromMaybe, catMaybes, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
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
import System.Posix.User (
    getUserEntryForName, getGroupEntryForName,
    getAllGroupEntries, getAllUserEntries,
    getRealUserID, getEffectiveUserID,
    getUserEntryForID, userID, groupID,
    userName, homeDirectory)
import System.Random (randomIO, randomRIO)

-- Import Ten modules
import Ten.Core (UserId(..), AuthToken(..), BuildError(..))

-- Type alias for UserEntry since we can't import the constructor directly
type UserEntry = (UserID, String, GroupID)

-- | Authentication errors
data AuthError
    = InvalidCredentials
    | UserNotFound
    | TokenNotFound
    | TokenExpired
    | InsufficientPermissions
    | AuthFileError Text
    | InvalidPassphrase
    | CryptoError Text
    | SystemError Text
    deriving (Show, Eq)

instance Exception AuthError

-- | User credentials for authentication
data UserCredentials = UserCredentials {
    ucUsername :: Text,
    ucToken :: Text
} deriving (Show, Eq)

-- | User permission levels
data UserPermissionLevel
    = UserLevelNone       -- No access
    | UserLevelBasic      -- Basic access (read-only)
    | UserLevelStandard   -- Standard user (can build)
    | UserLevelAdvanced   -- Advanced user (can manage own builds)
    | UserLevelAdmin      -- Administrator (full access)
    deriving (Show, Eq, Ord, Enum, Bounded)

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

-- | Token information
data TokenInfo = TokenInfo {
    tiToken :: Text,           -- The actual token value
    tiCreated :: UTCTime,      -- When the token was created
    tiExpires :: Maybe UTCTime, -- When the token expires (Nothing = no expiry)
    tiClientInfo :: Text,      -- Information about the client
    tiLastUsed :: UTCTime,     -- When the token was last used
    tiPermissions :: Set Permission -- Specific permissions for this token
} deriving (Show, Eq)

-- | Stored password hash
data PasswordHash = PasswordHash {
    phAlgorithm :: Text,      -- Hash algorithm (e.g., "pbkdf2-sha512")
    phSalt :: BS.ByteString,  -- Salt value
    phHash :: BS.ByteString,  -- Actual hash value
    phIterations :: Int,      -- Number of iterations
    phCreated :: UTCTime      -- When this hash was created
} deriving (Show, Eq)

-- | User information
data UserInfo = UserInfo {
    uiUserId :: UserId,
    uiUsername :: Text,
    uiPasswordHash :: Maybe PasswordHash,
    uiPermissionLevel :: UserPermissionLevel,
    uiSpecificPermissions :: Set Permission,
    uiSystemUser :: Maybe Text,  -- Associated system user, if any
    uiTokens :: Map Text TokenInfo,
    uiLastLogin :: Maybe UTCTime,
    uiCreated :: UTCTime
} deriving (Show, Eq)

-- | Authentication database
data AuthDb = AuthDb {
    adUsers :: Map Text UserInfo,  -- Keyed by username
    adSystemUserMap :: Map Text Text,  -- System username to Ten username
    adTokenMap :: Map Text Text,  -- Token to username
    adLastModified :: UTCTime
} deriving (Show, Eq)

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

-- | Create a new token for a user
createToken :: UserInfo -> Text -> Maybe Int -> IO TokenInfo
createToken user clientInfo expirySeconds = do
    token <- generateToken
    now <- getCurrentTime

    -- Calculate expiry time if provided
    let expires = case expirySeconds of
            Just seconds -> Just $ addUTCTime (fromIntegral seconds) now
            Nothing -> Nothing

    -- Create token info with user's permissions
    return TokenInfo {
        tiToken = token,
        tiCreated = now,
        tiExpires = expires,
        tiClientInfo = clientInfo,
        tiLastUsed = now,
        tiPermissions = uiSpecificPermissions user
    }

-- | Check if a token has expired
tokenExpired :: TokenInfo -> IO Bool
tokenExpired TokenInfo{..} = do
    now <- getCurrentTime
    return $ case tiExpires of
        Just expiry -> now > expiry
        Nothing -> False

-- | Get information about a system user
getSystemUserInfo :: Text -> IO (Maybe UserEntry)
getSystemUserInfo username = do
    result <- try $ getUserEntryForName (T.unpack username)
    case result of
        Left (_ :: SomeException) -> return Nothing
        Right entry -> return $ Just (userID entry, userName entry, groupID entry)

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
        (Nothing, _) -> throwIO $ AuthError $ "Ten user does not exist: " <> tenUsername
        (_, False) -> throwIO $ AuthError $ "System user does not exist: " <> sysUsername
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
                adLastModified = now
            }
        else do
            -- Load the database from file
            content <- BS.readFile path
            case Aeson.eitherDecodeStrict content of
                Left err -> throwIO $ AuthError $ "Failed to parse auth file: " <> T.pack err
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

-- | Authenticate a user and generate a token
authenticateUser :: Map Text Text -> AuthDb -> Text -> Text -> Text -> IO (AuthDb, UserId, AuthToken)
authenticateUser config db username password clientInfo = do
    -- Check if the user exists
    user <- case Map.lookup username (adUsers db) of
        Just u -> return u
        Nothing -> do
            -- If not, check if it's a system user
            isSysUser <- systemUserExists username
            let allowedUsers = maybe Set.empty Set.fromList (Map.lookup "allowedUsers" config >>= parseUserList)
            if isSysUser && (Set.null allowedUsers || username `Set.member` allowedUsers)
                then do
                    -- Create a new user based on the system user
                    createUserFromSystem db username clientInfo
                else throwIO $ AuthError InvalidCredentials

    -- Authenticate the user
    now <- getCurrentTime
    case uiPasswordHash user of
        -- If no password is set (e.g., for system users), skip password check
        Nothing -> do
            -- For system users, ensure they're allowed
            sysUser <- case uiSystemUser user of
                Just sysName -> do
                    exists <- systemUserExists sysName
                    if exists then return sysName else throwIO $ AuthError InvalidCredentials
                Nothing -> throwIO $ AuthError InvalidCredentials

            -- Create a token with appropriate permissions
            tokenInfo <- createToken user clientInfo (Just 86400)  -- 24 hour token

            -- Update the database with the new token and login time
            let updatedUser = user {
                    uiTokens = Map.insert (tiToken tokenInfo) tokenInfo (uiTokens user),
                    uiLastLogin = Just now
                }
            let updatedDb = db {
                    adUsers = Map.insert username updatedUser (adUsers db),
                    adTokenMap = Map.insert (tiToken tokenInfo) username (adTokenMap db),
                    adLastModified = now
                }

            return (updatedDb, uiUserId user, AuthToken (tiToken tokenInfo))

        -- If a password is set, validate it
        Just passwordHash -> do
            unless (verifyPassword password passwordHash) $
                throwIO $ AuthError InvalidCredentials

            -- Create a token with appropriate permissions
            tokenInfo <- createToken user clientInfo (Just 86400)  -- 24 hour token

            -- Update the database with the new token and login time
            let updatedUser = user {
                    uiTokens = Map.insert (tiToken tokenInfo) tokenInfo (uiTokens user),
                    uiLastLogin = Just now
                }
            let updatedDb = db {
                    adUsers = Map.insert username updatedUser (adUsers db),
                    adTokenMap = Map.insert (tiToken tokenInfo) username (adTokenMap db),
                    adLastModified = now
                }

            return (updatedDb, uiUserId user, AuthToken (tiToken tokenInfo))
  where
    parseUserList :: Text -> Maybe [Text]
    parseUserList txt = Just $ map T.strip $ T.split (==',') txt

-- | Create a new user from a system user
createUserFromSystem :: AuthDb -> Text -> Text -> IO UserInfo
createUserFromSystem db username clientInfo = do
    -- Check if the system user exists
    sysUserEntry <- getSystemUserInfo username
    case sysUserEntry of
        Nothing -> throwIO $ AuthError $ "System user not found: " <> username
        Just (uid, name, _) -> do
            now <- getCurrentTime
            let userId = UserId $ "sys_" <> username

            -- Create a new user with basic permissions
            return UserInfo {
                uiUserId = userId,
                uiUsername = username,
                uiPasswordHash = Nothing,  -- No password for system users
                uiPermissionLevel = UserLevelStandard,  -- Give standard access by default
                uiSpecificPermissions = defaultPermissions UserLevelStandard,
                uiSystemUser = Just username,
                uiTokens = Map.empty,
                uiLastLogin = Nothing,
                uiCreated = now
            }

-- | Validate a token
validateToken :: AuthDb -> AuthToken -> IO (Maybe (UserId, Set Permission))
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
                        Just tokenInfo -> do
                            -- Check if the token has expired
                            expired <- tokenExpired tokenInfo
                            if expired
                                then return Nothing
                                else return $ Just (uiUserId user, tiPermissions tokenInfo)

-- | Refresh an auth token
refreshToken :: AuthDb -> AuthToken -> IO (AuthDb, AuthToken)
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
                        Just tokenInfo -> do
                            -- Check if the token has expired
                            expired <- tokenExpired tokenInfo
                            if expired
                                then throwIO $ AuthError TokenExpired
                                else do
                                    -- Create a new token with the same permissions and expiry
                                    now <- getCurrentTime
                                    let oldExpiry = tiExpires tokenInfo
                                    let expirySeconds = case oldExpiry of
                                            Nothing -> Nothing
                                            Just expiry -> Just $ floor $ diffUTCTime expiry now

                                    newToken <- generateToken
                                    let newTokenInfo = tokenInfo {
                                            tiToken = newToken,
                                            tiCreated = now,
                                            tiLastUsed = now,
                                            tiExpires = case expirySeconds of
                                                Nothing -> Nothing
                                                Just seconds -> Just $ addUTCTime (fromIntegral seconds) now
                                        }

                                    -- Update the database
                                    let updatedTokens = Map.delete token (uiTokens user)
                                    let updatedTokens' = Map.insert newToken newTokenInfo updatedTokens
                                    let updatedUser = user { uiTokens = updatedTokens' }

                                    let updatedUsers = Map.insert username updatedUser (adUsers db)
                                    let updatedTokenMap = Map.delete token (adTokenMap db)
                                    let updatedTokenMap' = Map.insert newToken username updatedTokenMap

                                    let updatedDb = db {
                                            adUsers = updatedUsers,
                                            adTokenMap = updatedTokenMap',
                                            adLastModified = now
                                        }

                                    return (updatedDb, AuthToken newToken)

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
listUserTokens :: AuthDb -> Text -> IO [TokenInfo]
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
        throwIO $ AuthError $ "User already exists: " <> username

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

-- | Check if a user has a specific permission
checkUserPermission :: AuthDb -> UserId -> Permission -> IO Bool
checkUserPermission db userId permission = do
    -- Find the user by ID
    let mUser = findUserById db userId
    case mUser of
        Nothing -> return False
        Just user -> do
            -- Check if the user has the specific permission
            return $ permission `Set.member` uiSpecificPermissions user

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

-- | JSON instances for auth types

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

instance Aeson.ToJSON TokenInfo where
    toJSON TokenInfo{..} = Aeson.object [
            "token" .= tiToken,
            "created" .= tiCreated,
            "expires" .= tiExpires,
            "clientInfo" .= tiClientInfo,
            "lastUsed" .= tiLastUsed,
            "permissions" .= map permissionToText (Set.toList tiPermissions)
        ]

instance Aeson.FromJSON TokenInfo where
    parseJSON = Aeson.withObject "TokenInfo" $ \v -> do
        tiToken <- v .: "token"
        tiCreated <- v .: "created"
        tiExpires <- v .: "expires"
        tiClientInfo <- v .: "clientInfo"
        tiLastUsed <- v .: "lastUsed"
        permissionTexts <- v .: "permissions"

        let mPermissions = mapM textToPermission permissionTexts
        case mPermissions of
            Just perms -> do
                let tiPermissions = Set.fromList perms
                return TokenInfo{..}
            Nothing -> fail "Invalid permission in token"

instance Aeson.ToJSON UserInfo where
    toJSON UserInfo{..} = Aeson.object [
            "userId" .= case uiUserId of UserId uid -> uid,
            "username" .= uiUsername,
            "passwordHash" .= uiPasswordHash,
            "permissionLevel" .= permissionLevelToText uiPermissionLevel,
            "specificPermissions" .= map permissionToText (Set.toList uiSpecificPermissions),
            "systemUser" .= uiSystemUser,
            "tokens" .= uiTokens,
            "lastLogin" .= uiLastLogin,
            "created" .= uiCreated
        ]

instance Aeson.FromJSON UserInfo where
    parseJSON = Aeson.withObject "UserInfo" $ \v -> do
        userId <- v .: "userId"
        uiUsername <- v .: "username"
        uiPasswordHash <- v .: "passwordHash"
        permLevelText <- v .: "permissionLevel"
        permissionTexts <- v .: "specificPermissions"
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

        return UserInfo{..}

instance Aeson.ToJSON AuthDb where
    toJSON AuthDb{..} = Aeson.object [
            "users" .= adUsers,
            "systemUserMap" .= adSystemUserMap,
            "tokenMap" .= adTokenMap,
            "lastModified" .= adLastModified
        ]

instance Aeson.FromJSON AuthDb where
    parseJSON = Aeson.withObject "AuthDb" $ \v -> do
        adUsers <- v .: "users"
        adSystemUserMap <- v .: "systemUserMap"
        adTokenMap <- v .: "tokenMap"
        adLastModified <- v .: "lastModified"

        return AuthDb{..}

-- | Helper functions

-- | Repeat an action n times
replicateM :: Int -> IO a -> IO [a]
replicateM n action
    | n <= 0 = return []
    | otherwise = do
        x <- action
        xs <- replicateM (n-1) action
        return (x:xs)
