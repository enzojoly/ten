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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Ten.Daemon.Auth (
    -- Core authentication types from Protocol
    UserCredentials(..),
    AuthToken(..),

    -- Authentication functions with privilege awareness
    authenticateUser,
    validateToken,
    validateTokenAtProcessBoundary,
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

    -- Permission types and functions (using Protocol's DaemonCapability)
    permissionToCapability,
    capabilityToPermission,
    defaultCapabilities,
    capabilityRequiresDaemon,
    filterCapabilitiesForTier,

    -- System user integration
    getSystemUserInfo,
    systemUserExists,
    integrateWithSystemUser,

    -- Token management with privilege tier tracking
    TokenInfo(..),
    createTokenForTier,
    createDaemonToken,
    createBuilderToken,
    revokeToken,
    listUserTokens,
    refreshToken,
    refreshDaemonToken,
    refreshBuilderToken,
    tokenExpired,
    tokenHasExpired,
    tokenInfoHasExpired,

    -- Privilege transitions
    dropTokenPrivileges,
    hasPrivilegeForCapability,

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
import Control.Exception (catch, throwIO, try, bracket, finally, SomeException, Exception)
import Control.Monad (void, when, unless, forM_)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State (StateT, runStateT, get, put, modify)
import Control.Applicative (Alternative(..))
import Crypto.Hash (hash, digestFromByteString, Digest, SHA256, SHA512)
import qualified Crypto.Hash as Crypto
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import qualified Crypto.Random as CryptoRandom
import Data.Aeson ((.:), (.=))
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
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime, NominalDiffTime)
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
import System.Random (randomRIO)

-- Import Ten.Core for basic types
import Ten.Core (PrivilegeTier(..), SPrivilegeTier(..), UserId, AuthToken)

-- Import Protocol for authentication and security model
import Ten.Daemon.Protocol (PrivilegeError(..), PrivilegeRequirement, DaemonCapability(..))

-- | Authentication errors
data AuthError
    = InvalidCredentials
    | UserNotFound
    | TokenNotFound
    | TokenExpired
    | AuthFileError Text
    | InvalidPassphrase
    | CryptoError Text
    | SystemError Text
    | PrivilegeEscalationDenied Text
    deriving (Show, Eq)

instance Exception AuthError

-- | Stored password hash
data PasswordHash = PasswordHash {
    phAlgorithm :: Text,      -- Hash algorithm (e.g., "pbkdf2-sha512")
    phSalt :: BS.ByteString,  -- Salt value
    phHash :: BS.ByteString,  -- Actual hash value
    phIterations :: Int,      -- Number of iterations
    phCreated :: UTCTime      -- When this hash was created
} deriving (Show, Eq)

-- | Token information with privilege tier tracking
data TokenInfo (t :: PrivilegeTier) = TokenInfo {
    tiToken :: Text,           -- The actual token value
    tiCreated :: UTCTime,      -- When the token was created
    tiExpires :: Maybe UTCTime, -- When the token expires (Nothing = no expiry)
    tiClientInfo :: Text,      -- Information about the client
    tiLastUsed :: UTCTime,     -- When the token was last used
    tiCapabilities :: Set DaemonCapability, -- Specific capabilities for this token
    tiPrivilegeEvidence :: SPrivilegeTier t  -- Runtime evidence of privilege tier
} deriving (Eq)

-- Implement show for TokenInfo to hide the SPrivilegeTier
instance Show (TokenInfo t) where
    show TokenInfo{..} = "TokenInfo { token = " ++ show tiToken ++
                         ", created = " ++ show tiCreated ++
                         ", expires = " ++ show tiExpires ++
                         ", clientInfo = " ++ show tiClientInfo ++
                         ", lastUsed = " ++ show tiLastUsed ++
                         ", capabilities = " ++ show tiCapabilities ++
                         ", privilegeTier = " ++ show (fromSing tiPrivilegeEvidence) ++ " }"

-- | Token wrapper that erases the phantom type
data SomeTokenInfo = forall t. SomeTokenInfo (TokenInfo t)

instance Show SomeTokenInfo where
    show (SomeTokenInfo token) = show token

instance Eq SomeTokenInfo where
    (SomeTokenInfo t1) == (SomeTokenInfo t2) = tiToken t1 == tiToken t2

-- | User information with allowed privilege tiers
data UserInfo = UserInfo {
    uiUserId :: UserId,
    uiUsername :: Text,
    uiPasswordHash :: Maybe PasswordHash,
    uiCapabilityLevel :: UserCapabilityLevel,
    uiSpecificCapabilities :: Set DaemonCapability,
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

-- | User capability levels - aligned with Protocol's security model
data UserCapabilityLevel
    = UserLevelNone       -- No access
    | UserLevelBasic      -- Basic access (read-only)
    | UserLevelStandard   -- Standard user (can build)
    | UserLevelAdvanced   -- Advanced user (can manage own builds)
    | UserLevelAdmin      -- Administrator (full access)
    deriving (Show, Eq, Ord, Enum, Bounded)

-- | Helper functions to filter capabilities based on privilege tier
filterCapabilitiesForTier :: SPrivilegeTier t -> Set DaemonCapability -> Set DaemonCapability
filterCapabilitiesForTier SDaemon = id  -- Daemon can use all capabilities
filterCapabilitiesForTier SBuilder = Set.filter (not . capabilityRequiresDaemon)  -- Filter daemon-only capabilities

-- | Get the evidence value for a privilege tier
privilegeEvidence :: SPrivilegeTier t -> SPrivilegeTier t
privilegeEvidence = id

-- | Check if a capability requires daemon privileges (using Protocol's model)
capabilityRequiresDaemon :: DaemonCapability -> Bool
capabilityRequiresDaemon StoreAccess = True
capabilityRequiresDaemon SandboxCreation = True
capabilityRequiresDaemon GarbageCollection = True
capabilityRequiresDaemon DerivationRegistration = True
capabilityRequiresDaemon _ = False

-- | Map old-style Permission to new Protocol DaemonCapability
permissionToCapability :: Text -> Maybe DaemonCapability
permissionToCapability "build" = Just DerivationBuild
permissionToCapability "cancel-build" = Just DerivationBuild
permissionToCapability "query-build" = Just BuildQuery
permissionToCapability "query-store" = Just StoreQuery
permissionToCapability "modify-store" = Just StoreAccess
permissionToCapability "run-gc" = Just GarbageCollection
permissionToCapability "manage-users" = Just StoreAccess  -- Admin capability
permissionToCapability "admin" = Just StoreAccess         -- Admin capability
permissionToCapability _ = Nothing

-- | Map a capability back to permission name (for backwards compatibility)
capabilityToPermission :: DaemonCapability -> Text
capabilityToPermission StoreAccess = "modify-store"
capabilityToPermission SandboxCreation = "sandbox-creation"
capabilityToPermission GarbageCollection = "run-gc"
capabilityToPermission DerivationRegistration = "register-derivation"
capabilityToPermission DerivationBuild = "build"
capabilityToPermission StoreQuery = "query-store"
capabilityToPermission BuildQuery = "query-build"

-- | Default capabilities for capability levels - aligned with Protocol
defaultCapabilities :: UserCapabilityLevel -> Set DaemonCapability
defaultCapabilities UserLevelNone = Set.empty
defaultCapabilities UserLevelBasic = Set.fromList [
    StoreQuery,
    BuildQuery
    ]
defaultCapabilities UserLevelStandard = Set.fromList [
    DerivationBuild,
    StoreQuery,
    BuildQuery
    ]
defaultCapabilities UserLevelAdvanced = Set.fromList [
    DerivationBuild,
    DerivationRegistration,
    StoreQuery,
    BuildQuery,
    StoreAccess,
    GarbageCollection
    ]
defaultCapabilities UserLevelAdmin = Set.fromList [
    StoreAccess,
    SandboxCreation,
    GarbageCollection,
    DerivationRegistration,
    DerivationBuild,
    StoreQuery,
    BuildQuery
    ]

-- | Default allowed privilege tiers for capability levels
defaultAllowedTiers :: UserCapabilityLevel -> Set PrivilegeTier
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

-- | Generate a random token with high entropy (using Protocol's security model)
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

-- | Create a token with the specified privilege tier
createTokenForTier :: SPrivilegeTier t -> UserInfo -> Text -> Maybe Int -> Set DaemonCapability -> IO (TokenInfo t)
createTokenForTier evidence user clientInfo expirySeconds capabilities = do
    token <- generateToken
    now <- getCurrentTime

    -- Calculate expiry time if provided
    let expires = case expirySeconds of
            Just seconds -> Just $ addUTCTime (fromIntegral seconds) now
            Nothing -> Nothing

    -- Filter capabilities based on privilege tier
    let filteredCapabilities = filterCapabilitiesForTier evidence capabilities

    -- Create token info with the appropriate privilege evidence
    return TokenInfo {
        tiToken = token,
        tiCreated = now,
        tiExpires = expires,
        tiClientInfo = clientInfo,
        tiLastUsed = now,
        tiCapabilities = filteredCapabilities,
        tiPrivilegeEvidence = evidence
    }

-- | Create a new daemon token for a user
createDaemonToken :: UserInfo -> Text -> Maybe Int -> Set DaemonCapability -> IO (TokenInfo 'Daemon)
createDaemonToken = createTokenForTier SDaemon

-- | Create a new builder token for a user
createBuilderToken :: UserInfo -> Text -> Maybe Int -> Set DaemonCapability -> IO (TokenInfo 'Builder)
createBuilderToken = createTokenForTier SBuilder

-- | Create a builder token from a daemon token (privilege downgrade only)
dropTokenPrivileges :: TokenInfo 'Daemon -> TokenInfo 'Builder
dropTokenPrivileges token =
    -- Create a new token with Builder privilege
    TokenInfo {
        tiToken = tiToken token,
        tiCreated = tiCreated token,
        tiExpires = tiExpires token,
        tiClientInfo = tiClientInfo token,
        tiLastUsed = tiLastUsed token,
        tiCapabilities = Set.filter (not . capabilityRequiresDaemon) (tiCapabilities token),
        tiPrivilegeEvidence = SBuilder
    }

-- | Check if a TokenInfo has expired (for direct TokenInfo values)
tokenExpired :: TokenInfo t -> IO Bool
tokenExpired token = do
    now <- getCurrentTime
    return $ case tiExpires token of
        Just expiry -> now > expiry
        Nothing -> False

-- | Helper to check if a token has expired (for TokenInfo - type-specific version)
tokenInfoHasExpired :: TokenInfo t -> IO Bool
tokenInfoHasExpired = tokenExpired

-- | Helper to check if a token has expired (for SomeTokenInfo - type-erased version)
tokenHasExpired :: SomeTokenInfo -> IO Bool
tokenHasExpired (SomeTokenInfo token) = tokenExpired token

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
        (Nothing, _) -> throwIO $ AuthFileError $ "Ten user does not exist: " <> tenUsername
        (_, False) -> throwIO $ SystemError $ "System user does not exist: " <> sysUsername
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

-- | Check if a user has permission for a specific capability + privilege tier
hasPrivilegeForCapability :: UserInfo -> DaemonCapability -> PrivilegeTier -> Bool
hasPrivilegeForCapability user capability requestedTier =
    -- Check if user has the capability
    capability `Set.member` uiSpecificCapabilities user &&
    -- Check if user is allowed to use the requested tier
    requestedTier `Set.member` uiAllowedPrivilegeTiers user &&
    -- Check if capability is compatible with requested tier
    (requestedTier == Daemon || not (capabilityRequiresDaemon capability))

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
                Left err -> throwIO $ AuthFileError $ "Failed to parse auth file: " <> T.pack err
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

-- | Replicating an action n times (helper function)
replicateM :: Int -> IO a -> IO [a]
replicateM n action
    | n <= 0 = return []
    | otherwise = do
        x <- action
        xs <- replicateM (n-1) action
        return (x:xs)

-- | Authenticate a user and generate a token with privilege validation
authenticateUser ::
    AuthDb ->
    Text ->
    Text ->
    Text ->
    PrivilegeTier ->
    IO (Either AuthError (AuthDb, UserId, AuthToken, PrivilegeTier, Set DaemonCapability))
authenticateUser db username password clientInfo requestedTier = do
    -- Check if the user exists
    case Map.lookup username (adUsers db) of
        Nothing -> return $ Left InvalidCredentials
        Just user -> do
            -- Verify the requested privilege tier is allowed for this user
            if requestedTier `Set.member` uiAllowedPrivilegeTiers user
                then do
                    -- Check for privilege escalation policy
                    if requestedTier == Daemon && not (adAllowPrivilegeEscalation db)
                        then return $ Left $ PrivilegeEscalationDenied "Privilege escalation is disabled globally"
                        else do
                            -- Authenticate the user
                            now <- getCurrentTime
                            case uiPasswordHash user of
                                -- If no password is set (e.g., for system users), check system integration
                                Nothing ->
                                    case uiSystemUser user of
                                        Just sysName -> do
                                            exists <- systemUserExists sysName
                                            if exists
                                                then proceedWithAuthentication db username user clientInfo now requestedTier
                                                else return $ Left InvalidCredentials
                                        Nothing -> return $ Left InvalidCredentials

                                -- If a password is set, validate it
                                Just passwordHash ->
                                    if verifyPassword password passwordHash
                                        then proceedWithAuthentication db username user clientInfo now requestedTier
                                        else return $ Left InvalidCredentials
                else return $ Left $ InsufficientPrivileges requestedTier DaemonRequired

-- | Process authenticated user and create appropriate token
proceedWithAuthentication ::
    AuthDb ->
    Text ->
    UserInfo ->
    Text ->
    UTCTime ->
    PrivilegeTier ->
    IO (Either AuthError (AuthDb, UserId, AuthToken, PrivilegeTier, Set DaemonCapability))
proceedWithAuthentication db username user clientInfo now requestedTier =
    case requestedTier of
        Daemon -> do
            let expirySeconds = Just 86400 -- 24 hours
            tokenInfo <- createDaemonToken user clientInfo expirySeconds (uiSpecificCapabilities user)
            let someToken = SomeTokenInfo tokenInfo

            -- Update user with new token
            let updatedUser = user {
                    uiTokens = Map.insert (tiToken tokenInfo) someToken (uiTokens user),
                    uiLastLogin = Just now
                }

            -- Update database
            let updatedDb = db {
                    adUsers = Map.insert username updatedUser (adUsers db),
                    adTokenMap = Map.insert (tiToken tokenInfo) username (adTokenMap db),
                    adLastModified = now
                }

            return $ Right (updatedDb, uiUserId user, AuthToken (tiToken tokenInfo), Daemon, tiCapabilities tokenInfo)

        Builder -> do
            let expirySeconds = Just 86400 -- 24 hours

            -- For Builder tier, filter out daemon-only capabilities
            let builderCapabilities = Set.filter (not . capabilityRequiresDaemon) (uiSpecificCapabilities user)
            tokenInfo <- createBuilderToken user clientInfo expirySeconds builderCapabilities
            let someToken = SomeTokenInfo tokenInfo

            -- Update user with new token
            let updatedUser = user {
                    uiTokens = Map.insert (tiToken tokenInfo) someToken (uiTokens user),
                    uiLastLogin = Just now
                }

            -- Update database
            let updatedDb = db {
                    adUsers = Map.insert username updatedUser (adUsers db),
                    adTokenMap = Map.insert (tiToken tokenInfo) username (adTokenMap db),
                    adLastModified = now
                }

            return $ Right (updatedDb, uiUserId user, AuthToken (tiToken tokenInfo), Builder, tiCapabilities tokenInfo)

-- | Validate a token for daemon privileges
validateDaemonToken ::
    AuthDb ->
    AuthToken ->
    IO (Maybe (UserId, Set DaemonCapability))
validateDaemonToken db (AuthToken token) = do
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
                        Just someToken -> do
                            -- Check if the token has expired
                            expired <- tokenHasExpired someToken
                            if expired
                                then return Nothing
                                else do
                                    -- Check if token has daemon privileges
                                    let (_, _, _, _, _, caps, tier) = unwrapTokenInfo someToken
                                    case tier of
                                        Daemon -> return $ Just (uiUserId user, caps)
                                        _ -> return Nothing

-- | Validate a token for builder privileges
validateBuilderToken ::
    AuthDb ->
    AuthToken ->
    IO (Maybe (UserId, Set DaemonCapability))
validateBuilderToken db (AuthToken token) = do
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
                        Just someToken -> do
                            -- Check if the token has expired
                            expired <- tokenHasExpired someToken
                            if expired
                                then return Nothing
                                else do
                                    -- Return the capabilities (any token works for builder ops)
                                    let (_, _, _, _, _, caps, _) = unwrapTokenInfo someToken
                                    return $ Just (uiUserId user, caps)

-- | Token validation at process boundaries
validateTokenAtProcessBoundary ::
    AuthDb ->
    AuthToken ->
    PrivilegeTier ->  -- Requested privilege tier
    IO (Maybe (UserId, Set DaemonCapability))
validateTokenAtProcessBoundary db token requestedTier =
    case requestedTier of
        Daemon -> validateDaemonToken db token
        Builder -> validateBuilderToken db token

-- | Type-safe token validation with explicit privilege evidence
validateToken :: forall t.
    AuthDb ->
    AuthToken ->
    SPrivilegeTier t ->
    IO (Maybe (UserId, Set DaemonCapability, SPrivilegeTier t))
validateToken db token evidence =
    -- Simply wrap the appropriate validation function based on privilege tier
    case fromSing evidence of
        Daemon -> do
            -- Validate for daemon privileges
            result <- validateDaemonToken db token
            case result of
                Just (userId, capabilities) -> return $ Just (userId, capabilities, evidence)
                Nothing -> return Nothing
        Builder -> do
            -- Validate for builder privileges (any token is acceptable for builder ops)
            result <- validateBuilderToken db token
            case result of
                Just (userId, capabilities) -> return $ Just (userId, capabilities, evidence)
                Nothing -> return Nothing

-- | Helper to extract privilege evidence from TokenInfo
extractPrivilegeEvidence :: forall t. TokenInfo t -> SPrivilegeTier t
extractPrivilegeEvidence = tiPrivilegeEvidence

-- | Helper to extract capabilities from TokenInfo
extractCapabilities :: forall t. TokenInfo t -> Set DaemonCapability
extractCapabilities = tiCapabilities

-- | Helper to convert SomeTokenInfo to its components for validation
unwrapTokenInfo :: SomeTokenInfo -> (Text, UTCTime, Maybe UTCTime, Text, UTCTime, Set DaemonCapability, PrivilegeTier)
unwrapTokenInfo (SomeTokenInfo token) =
    (tiToken token,
     tiCreated token,
     tiExpires token,
     tiClientInfo token,
     tiLastUsed token,
     tiCapabilities token,
     fromSing (tiPrivilegeEvidence token))

-- | Data type to erase the phantom type from SPrivilegeTier
data SomePrivilegeTier = forall t. SomePrivilegeTier (SPrivilegeTier t)

-- | Refresh a token with validation of the requested privilege level
refreshToken ::
    AuthDb ->
    AuthToken ->
    PrivilegeTier ->  -- Requested privilege tier
    IO (Either AuthError (AuthDb, AuthToken))
refreshToken db (AuthToken token) requestedTier = do
    -- Check if the token exists
    case Map.lookup token (adTokenMap db) of
        Nothing -> return $ Left TokenNotFound
        Just username -> do
            -- Check if the user exists
            case Map.lookup username (adUsers db) of
                Nothing -> return $ Left UserNotFound
                Just user -> do
                    -- Check if the token exists for this user
                    case Map.lookup token (uiTokens user) of
                        Nothing -> return $ Left TokenNotFound
                        Just someToken -> do
                            -- Check if the token has expired
                            expired <- tokenHasExpired someToken
                            if expired
                                then return $ Left TokenExpired
                                else do
                                    -- Check current token privilege vs requested privilege
                                    let (_, _, _, _, _, _, currentTier) = unwrapTokenInfo someToken

                                    -- Validate privilege transition (can't escalate)
                                    case (currentTier, requestedTier) of
                                        -- Same privilege level - straightforward refresh
                                        (Daemon, Daemon) -> refreshWithTier db username user token someToken Daemon
                                        (Builder, Builder) -> refreshWithTier db username user token someToken Builder

                                        -- Privilege downgrade - allowed
                                        (Daemon, Builder) -> refreshWithTier db username user token someToken Builder

                                        -- Privilege escalation - denied
                                        (Builder, Daemon) ->
                                            return $ Left $ InsufficientPrivileges Builder DaemonRequired

-- | Helper function to refresh token with specific privilege tier
refreshWithTier ::
    AuthDb ->
    Text ->
    UserInfo ->
    Text ->
    SomeTokenInfo ->
    PrivilegeTier ->
    IO (Either AuthError (AuthDb, AuthToken))
refreshWithTier db username user oldTokenStr someToken tier = do
    -- Create a new token
    now <- getCurrentTime
    newTokenStr <- generateToken

    -- Extract token info
    let (_, created, oldExpiry, clientInfo, _, caps, _) = unwrapTokenInfo someToken

    -- Calculate new expiry in seconds
    let expirySeconds = case oldExpiry of
            Nothing -> Nothing
            Just expiry ->
                let remaining = diffUTCTime expiry now
                in if remaining > 0
                    then Just (ceiling remaining)
                    else Just (24 * 3600)  -- Default to 24 hours if expired

    -- Create token based on requested tier
    newDb <- case tier of
        Daemon -> do
            -- Create new daemon token
            newTokenInfo <- createDaemonToken user clientInfo expirySeconds caps
            updateDbWithNewToken db username user oldTokenStr newTokenStr newTokenInfo

        Builder -> do
            -- Filter capabilities for builder
            let builderCapabilities = Set.filter (not . capabilityRequiresDaemon) caps

            -- Create new builder token
            newTokenInfo <- createBuilderToken user clientInfo expirySeconds builderCapabilities
            updateDbWithNewToken db username user oldTokenStr newTokenStr newTokenInfo

    return $ Right (newDb, AuthToken newTokenStr)

-- | Update database with new token
updateDbWithNewToken ::
    AuthDb ->
    Text ->
    UserInfo ->
    Text ->
    Text ->
    TokenInfo t ->
    IO AuthDb
updateDbWithNewToken db username user oldTokenStr newTokenStr newToken = do
    now <- getCurrentTime

    -- Wrap token in SomeTokenInfo
    let someToken = SomeTokenInfo newToken

    -- Update user tokens
    let updatedTokens = Map.delete oldTokenStr (uiTokens user)
    let updatedTokens' = Map.insert newTokenStr someToken updatedTokens
    let updatedUser = user { uiTokens = updatedTokens' }

    -- Update token map
    let updatedTokenMap = Map.delete oldTokenStr (adTokenMap db)
    let updatedTokenMap' = Map.insert newTokenStr username updatedTokenMap

    -- Return updated database
    return db {
        adUsers = Map.insert username updatedUser (adUsers db),
        adTokenMap = updatedTokenMap',
        adLastModified = now
    }

-- | Refresh a daemon token (convenience function)
refreshDaemonToken ::
    AuthDb ->
    AuthToken ->
    IO (Either AuthError (AuthDb, AuthToken))
refreshDaemonToken db token = refreshToken db token Daemon

-- | Refresh a builder token (convenience function)
refreshBuilderToken ::
    AuthDb ->
    AuthToken ->
    IO (Either AuthError (AuthDb, AuthToken))
refreshBuilderToken db token = refreshToken db token Builder

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
        Nothing -> throwIO UserNotFound
        Just user -> return $ Map.elems (uiTokens user)

-- | Get user information
getUserInfo :: AuthDb -> Text -> IO UserInfo
getUserInfo db username = do
    -- Check if the user exists
    case Map.lookup username (adUsers db) of
        Nothing -> throwIO UserNotFound
        Just user -> return user

-- | Add a new user
addUser :: AuthDb -> Text -> Text -> UserCapabilityLevel -> IO AuthDb
addUser db username password capabilityLevel = do
    -- Check if the user already exists
    when (Map.member username (adUsers db)) $
        throwIO $ AuthFileError $ "User already exists: " <> username

    -- Create the user
    now <- getCurrentTime
    passwordHash <- hashPassword password
    let userId = UserId $ "user_" <> username <> "_" <> T.pack (formatTime defaultTimeLocale "%Y%m%d%H%M%S" now)

    let user = UserInfo {
            uiUserId = userId,
            uiUsername = username,
            uiPasswordHash = Just passwordHash,
            uiCapabilityLevel = capabilityLevel,
            uiSpecificCapabilities = defaultCapabilities capabilityLevel,
            uiAllowedPrivilegeTiers = defaultAllowedTiers capabilityLevel,
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
        Nothing -> throwIO UserNotFound
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
        Nothing -> throwIO UserNotFound
        Just user -> do
            -- Update the user
            now <- getCurrentTime
            let updatedUser = user { uiAllowedPrivilegeTiers = newTiers }

            -- Update the database
            return db {
                adUsers = Map.insert username updatedUser (adUsers db),
                adLastModified = now
            }

-- | Check if a user has a specific capability
checkUserPermission :: AuthDb -> UserId -> DaemonCapability -> PrivilegeTier -> IO Bool
checkUserPermission db userId capability requestedTier = do
    -- Find the user by ID
    let mUser = findUserById db userId
    case mUser of
        Nothing -> return False
        Just user -> do
            -- Check if the user has the capability in the requested tier
            return $ hasPrivilegeForCapability user capability requestedTier

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
            "capabilities" .= (map capabilityToPermission (Set.toList (tiCapabilities token)) :: [Text]),
            "privilegeTier" .= (case fromSing (tiPrivilegeEvidence token) of
                                 Daemon -> ("daemon" :: Text)
                                 Builder -> ("builder" :: Text))
        ]

instance Aeson.FromJSON SomeTokenInfo where
    parseJSON = Aeson.withObject "TokenInfo" $ \v -> do
        token <- v .: "token"
        created <- v .: "created"
        expires <- v .: "expires"
        clientInfo <- v .: "clientInfo"
        lastUsed <- v .: "lastUsed"
        capabilityTexts <- v .: "capabilities" :: Aeson.Parser [Text]
        tierText <- v .: "privilegeTier" :: Aeson.Parser Text

        -- Parse capabilities
        let capabilityList = catMaybes $ map permissionToCapability capabilityTexts
        let capabilities = Set.fromList capabilityList

        -- Create token with appropriate privilege tier
        case tierText of
            "daemon" -> do
                let tokenInfo = TokenInfo {
                        tiToken = token,
                        tiCreated = created,
                        tiExpires = expires,
                        tiClientInfo = clientInfo,
                        tiLastUsed = lastUsed,
                        tiCapabilities = capabilities,
                        tiPrivilegeEvidence = SDaemon
                    }
                return $ SomeTokenInfo tokenInfo
            "builder" -> do
                let tokenInfo = TokenInfo {
                        tiToken = token,
                        tiCreated = created,
                        tiExpires = expires,
                        tiClientInfo = clientInfo,
                        tiLastUsed = lastUsed,
                        tiCapabilities = capabilities,
                        tiPrivilegeEvidence = SBuilder
                    }
                return $ SomeTokenInfo tokenInfo
            _ -> fail $ "Invalid privilege tier: " ++ T.unpack tierText

instance Aeson.ToJSON UserInfo where
    toJSON UserInfo{..} = Aeson.object [
            "userId" .= case uiUserId of UserId uid -> uid,
            "username" .= uiUsername,
            "passwordHash" .= uiPasswordHash,
            "capabilityLevel" .= show uiCapabilityLevel,
            "specificCapabilities" .= (map capabilityToPermission (Set.toList uiSpecificCapabilities) :: [Text]),
            "allowedPrivilegeTiers" .= (map showTier (Set.toList uiAllowedPrivilegeTiers) :: [Text]),
            "systemUser" .= uiSystemUser,
            "tokens" .= uiTokens,
            "lastLogin" .= uiLastLogin,
            "created" .= uiCreated
        ]
      where
        showTier :: PrivilegeTier -> Text
        showTier Daemon = "daemon"
        showTier Builder = "builder"

instance Aeson.FromJSON UserInfo where
    parseJSON = Aeson.withObject "UserInfo" $ \v -> do
        userId <- v .: "userId"
        uiUsername <- v .: "username"
        uiPasswordHash <- v .: "passwordHash"
        capabilityLevelText <- v .: "capabilityLevel" :: Aeson.Parser String
        capabilityTexts <- v .: "specificCapabilities" :: Aeson.Parser [Text]
        tierTexts <- v .: "allowedPrivilegeTiers" :: Aeson.Parser [Text]
        uiSystemUser <- v .: "systemUser"
        uiTokens <- v .: "tokens"
        uiLastLogin <- v .: "lastLogin"
        uiCreated <- v .: "created"

        let uiUserId = UserId userId

        let uiCapabilityLevel = case capabilityLevelText of
                "UserLevelNone" -> UserLevelNone
                "UserLevelBasic" -> UserLevelBasic
                "UserLevelStandard" -> UserLevelStandard
                "UserLevelAdvanced" -> UserLevelAdvanced
                "UserLevelAdmin" -> UserLevelAdmin
                _ -> UserLevelNone  -- Default to lowest level for safety

        -- Parse capabilities
        let capabilityList = catMaybes $ map permissionToCapability capabilityTexts
        let uiSpecificCapabilities = Set.fromList capabilityList

        -- Parse privilege tiers
        let parseTier :: Text -> Maybe PrivilegeTier
            parseTier "daemon" = Just Daemon
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
