{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Ten.Derivation (
    Derivation(..),
    DerivationInput(..),
    DerivationOutput(..),
    mkDerivation,
    instantiateDerivation,
    serializeDerivation,
    deserializeDerivation,
    hashDerivation
) where

import Control.Monad
import Control.Monad.Reader (ask)
import Control.Monad.State (modify)
import Control.Monad.Except (throwError)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import System.FilePath

import Ten.Core
import qualified Ten.Hash as Hash
import Ten.Store

-- | Input to a derivation
data DerivationInput = DerivationInput
    { inputPath :: StorePath   -- Path in the store
    , inputName :: Text        -- Name to expose this input as
    } deriving (Show, Eq, Ord)

-- | Expected output from a derivation
data DerivationOutput = DerivationOutput
    { outputName :: Text       -- Name of the output
    , outputPath :: StorePath  -- Where it will be stored
    } deriving (Show, Eq, Ord)

-- | A derivation is a pure description of a build
data Derivation = Derivation
    { derivName :: Text                      -- Human-readable name
    , derivBuilder :: StorePath              -- Path to the builder executable
    , derivArgs :: [Text]                    -- Arguments to the builder
    , derivInputs :: Set DerivationInput     -- Input dependencies
    , derivOutputs :: Set DerivationOutput   -- Expected outputs
    , derivEnv :: Map Text Text              -- Environment variables
    } deriving (Show, Eq)

-- | Create a new derivation
mkDerivation :: Text -> StorePath -> [Text] -> Set DerivationInput -> Set Text -> Map Text Text -> TenM 'Eval Derivation
mkDerivation name builder args inputs outputNames env = do
    -- Generate output paths based on the derivation hash
    let derivHash = Hash.hashDerivation name (map T.pack $ show builder : map T.unpack args) (Map.toList env)
    let hashText = Hash.showHash derivHash

    -- Create output specifications with predicted paths
    let outputs = Set.map (\outName -> DerivationOutput
                              { outputName = outName
                              , outputPath = StorePath hashText outName
                              }) outputNames

    -- Record that this is a valid derivation
    addProof TypeProof

    return Derivation
        { derivName = name
        , derivBuilder = builder
        , derivArgs = args
        , derivInputs = inputs
        , derivOutputs = outputs
        , derivEnv = env
        }

-- | Instantiate a derivation for building
instantiateDerivation :: Derivation -> TenM 'Build ()
instantiateDerivation deriv = do
    -- Verify inputs exist
    forM_ (derivInputs deriv) $ \input -> do
        exists <- storePathExists (inputPath input)
        unless exists $ throwError $
            BuildFailed $ "Input does not exist: " <> T.pack (show $ inputPath input)

    -- Record inputs in build state
    let inputPaths = Set.map inputPath (derivInputs deriv)
    modify $ \s -> s { buildInputs = Set.union inputPaths (buildInputs s) }

    -- Verify builder exists
    builderExists <- storePathExists (derivBuilder deriv)
    unless builderExists $ throwError $
        BuildFailed $ "Builder does not exist: " <> T.pack (show $ derivBuilder deriv)

    -- Add input proof
    addProof InputProof

    logMsg 1 $ "Instantiated derivation: " <> derivName deriv

-- | Serialize a derivation to a ByteString
serializeDerivation :: Derivation -> BS.ByteString
serializeDerivation deriv = TE.encodeUtf8 $ T.unlines
    [ "name: " <> derivName deriv
    , "builder: " <> T.pack (show $ derivBuilder deriv)
    , "args: " <> T.intercalate " " (derivArgs deriv)
    , "inputs: " <> T.intercalate ", " (map (T.pack . show) $ Set.toList $ derivInputs deriv)
    , "outputs: " <> T.intercalate ", " (map (T.pack . show) $ Set.toList $ derivOutputs deriv)
    , "env: " <> T.intercalate ", " (map (\(k, v) -> k <> "=" <> v) $ Map.toList $ derivEnv deriv)
    ]

-- | Deserialize a derivation from a ByteString (skeleton implementation)
deserializeDerivation :: BS.ByteString -> Either Text Derivation
deserializeDerivation _ = Left "Deserialization not implemented yet"

-- | Compute a deterministic hash for a derivation
hashDerivation :: Derivation -> Hash.Hash
hashDerivation deriv = Hash.hashByteString $ serializeDerivation deriv
