{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ten.Derivation (
    -- Core types
    Derivation(..),
    DerivationInput(..),
    DerivationOutput(..),

    -- Derivation creation and inspection
    mkDerivation,
    instantiateDerivation,
    derivationEquals,
    derivationOutputPaths,
    derivationInputPaths,

    -- Return-Continuation pattern
    joinDerivation,
    buildToGetInnerDerivation,
    isReturnContinuationDerivation,

    -- Serialization
    serializeDerivation,
    deserializeDerivation,
    derivationToJSON,
    derivationFromJSON,

    -- Hashing and identity
    Ten.Hash.hashDerivation,
    hashDerivationInputs,

    -- Build strategy analysis
    analyzeDerivationStrategy,

    -- Derivation chain handling
    DerivationChain,
    newDerivationChain,
    Ten.Core.addToDerivationChain,
    Ten.Core.isInDerivationChain,
    derivationChainLength
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (modify, gets)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Aeson.Encoding as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as Aeson.KeyMap
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import System.FilePath
import System.Directory (doesFileExist)
import qualified System.Posix.Files as Posix
import System.IO (withFile, IOMode(..))
import System.Process
import System.Exit

import Ten.Core
import Ten.Hash
import Ten.Store
import Ten.Sandbox

-- | Represents a chain of recursive derivations
data DerivationChain = DerivationChain [Text] -- Chain of derivation hashes
    deriving (Show, Eq)

-- | Create a new derivation chain
newDerivationChain :: Derivation -> DerivationChain
newDerivationChain drv = DerivationChain [derivHash drv]

-- | Check if a derivation is in a chain (cycle detection)
detectRecursionCycle :: [Derivation] -> TenM p Bool
detectRecursionCycle derivations =
    -- Check if any derivation appears more than once in the chain
    return $ length (List.nubBy derivationEquals derivations) < length derivations

-- | Get chain length
derivationChainLength :: DerivationChain -> Int
derivationChainLength (DerivationChain hashes) = length hashes

-- | Create a new derivation
mkDerivation :: Text                      -- ^ Name
             -> StorePath                 -- ^ Builder
             -> [Text]                    -- ^ Arguments
             -> Set DerivationInput       -- ^ Inputs
             -> Set Text                  -- ^ Output names
             -> Map Text Text             -- ^ Environment variables
             -> Text                      -- ^ Target system
             -> TenM 'Eval Derivation
mkDerivation name builder args inputs outputNames env system = do
    -- First calculate a deterministic hash for the derivation
    let hashBase = T.concat
            [ name
            , T.pack $ show builder
            , T.intercalate "," args
            , T.intercalate "," $ map (T.pack . show) $ Set.toList inputs
            , T.intercalate "," $ Set.toList outputNames
            , T.intercalate ";" $ map (\(k, v) -> k <> "=" <> v) $ Map.toList env
            , system
            ]

    let derivHash' = T.pack $ show $ hashText hashBase

    -- Create output specifications with predicted paths
    let outputs = Set.map (\outName -> DerivationOutput
                              { outputName = outName
                              , outputPath = StorePath derivHash' outName
                              }) outputNames

    -- Analyze what build strategy this derivation should use
    let strategy = analyzeDerivationStrategy name args env

    -- Add metadata if it's a return-continuation derivation
    let meta = if strategy == MonadicStrategy && isReturnContinuationDerivation name args env
                  then Map.singleton "returnContinuation" "true"
                  else Map.empty

    -- Record proof that this is a valid derivation
    addProof TypeProof

    return Derivation
        { derivName = name
        , derivHash = derivHash'
        , derivBuilder = builder
        , derivArgs = args
        , derivInputs = inputs
        , derivOutputs = outputs
        , derivEnv = env
        , derivSystem = system
        , derivStrategy = strategy
        , derivMeta = meta
        }

-- | Analyze a derivation to determine optimal build strategy
analyzeDerivationStrategy :: Text -> [Text] -> Map Text Text -> BuildStrategy
analyzeDerivationStrategy name args env =
    -- Check various indicators that this derivation uses return-continuation
    if isReturnContinuationDerivation name args env
        then MonadicStrategy
        else
            -- Check for other indicators that require dynamic dependency resolution
            if any isDynamicDependencyArg args || Map.member "TEN_DYNAMIC_DEPS" env
                then MonadicStrategy
                else ApplicativeStrategy
  where
    -- Check if an argument suggests dynamic dependencies
    isDynamicDependencyArg arg =
        "dynamicDeps" `T.isInfixOf` arg ||
        "loadDeps" `T.isInfixOf` arg ||
        "resolveAt" `T.isInfixOf` arg

-- | Check if a derivation uses return-continuation
isReturnContinuationDerivation :: Text -> [Text] -> Map Text Text -> Bool
isReturnContinuationDerivation name args env =
    -- Check various indicators that this derivation uses return-continuation
    Map.member "TEN_RETURN_CONTINUATION" env ||
    Map.member "TEN_INNER_DERIVATION" env ||
    "return-drv" `T.isInfixOf` name ||
    "returnDerivation" `T.isInfixOf` name ||
    any (`T.isInfixOf` name) ["stage1", "phase1", "bootstrap-"] ||
    any returnsDerivation args
  where
    -- Check if an argument suggests returning a derivation
    returnsDerivation arg =
        "return-drv" `T.isInfixOf` arg ||
        "returnDerivation" `T.isInfixOf` arg ||
        "--return" `T.isPrefixOf` arg ||
        "--output-derivation" `T.isPrefixOf` arg

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

    -- Track this derivation in the build chain for recursion detection
    chain <- gets buildChain
    limit <- asks maxRecursionDepth
    when (length chain >= limit) $
        throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show limit)

    -- Check for cycles in build chain
    let isCyclic = any (derivationEquals deriv) chain
    when isCyclic $
        throwError $ CyclicDependency $ "Cyclic derivation detected: " <> derivName deriv

    -- Add to build chain
    addToDerivationChain deriv

    -- Add input proof
    addProof InputProof

    logMsg 1 $ "Instantiated derivation: " <> derivName deriv

-- | The monadic join operation for Return-Continuation
joinDerivation :: Derivation -> TenM 'Build Derivation
joinDerivation outerDrv = do
    -- Build the outer derivation just enough to get inner derivation
    innerDrv <- buildToGetInnerDerivation outerDrv

    -- Record that we performed a join operation
    addProof ReturnProof

    -- Return the inner derivation to be built
    return innerDrv

-- | Build a derivation to the point where it returns an inner derivation
buildToGetInnerDerivation :: Derivation -> TenM 'Build Derivation
buildToGetInnerDerivation drv = do
    env <- ask

    -- Create a temporary sandbox for the build
    let config = defaultSandboxConfig { sandboxReturnSupport = True }

    withSandbox (Set.map inputPath $ derivInputs drv) config $ \sandboxDir -> do
        -- Get the builder from the store
        builderContent <- readFromStore (derivBuilder drv)

        -- Write the builder to the sandbox
        let builderPath = sandboxDir </> "builder"
        liftIO $ BS.writeFile builderPath builderContent
        liftIO $ Posix.setFileMode builderPath 0o755

        -- Prepare environment variables
        let buildEnv = prepareSandboxEnvironment env sandboxDir (derivEnv drv)

        -- Configure the process
        let processConfig = sandboxedProcessConfig
                sandboxDir
                builderPath
                (map T.unpack $ derivArgs drv)
                buildEnv
                defaultSandboxConfig  -- Use default for inner execution

        -- Run the builder
        (exitCode, stdout, stderr) <- liftIO $ readCreateProcessWithExitCode processConfig ""

        -- Check if the builder wrote a derivation file
        let returnPath = returnDerivationPath sandboxDir
        hasReturnDrv <- liftIO $ doesFileExist returnPath

        -- Handle different outcomes
        if exitCode == ExitSuccess && hasReturnDrv
            then do
                -- Read the returned derivation
                content <- liftIO $ BS.readFile returnPath
                case deserializeDerivation content of
                    Left err -> throwError $ SerializationError $
                        "Failed to deserialize returned derivation: " <> err
                    Right innerDrv -> do
                        -- Add proof that we successfully got a returned derivation
                        addProof RecursionProof
                        return innerDrv
            else
                throwError $ BuildFailed $
                    "Builder did not return a derivation at: " <> T.pack returnPath <>
                    "\nExit code: " <> T.pack (show exitCode) <>
                    "\nStdout: " <> T.pack stdout <>
                    "\nStderr: " <> T.pack stderr

-- | Get output paths from a derivation
derivationOutputPaths :: Derivation -> Set StorePath
derivationOutputPaths = Set.map outputPath . derivOutputs

-- | Get input paths from a derivation
derivationInputPaths :: Derivation -> Set StorePath
derivationInputPaths = Set.map inputPath . derivInputs

-- | Serialize a derivation to a ByteString
serializeDerivation :: Derivation -> BS.ByteString
serializeDerivation = LBS.toStrict . Aeson.encode . derivationToJSON

-- | Convert derivation to JSON
derivationToJSON :: Derivation -> Aeson.Value
derivationToJSON Derivation{..} = Aeson.object
    [ "name" .= derivName
    , "hash" .= derivHash
    , "builder" .= storePathToJSON derivBuilder
    , "args" .= derivArgs
    , "inputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList derivInputs)
    , "outputs" .= Aeson.Array (Vector.fromList $ map Aeson.toJSON $ Set.toList derivOutputs)
    , "env" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList derivEnv)
    , "system" .= derivSystem
    , "strategy" .= (case derivStrategy of
                        ApplicativeStrategy -> "applicative" :: Text
                        MonadicStrategy -> "monadic")
    , "meta" .= Aeson.object (map (\(k, v) -> Aeson.fromText k .= v) $ Map.toList derivMeta)
    ]

-- Helper to convert StorePath to JSON
storePathToJSON :: StorePath -> Aeson.Value
storePathToJSON (StorePath hash name) = Aeson.object
    [ "hash" .= hash
    , "name" .= name
    ]

-- Make DerivationInput JSON serializable
instance Aeson.ToJSON DerivationInput where
    toJSON (DerivationInput path name) = Aeson.object
        [ "path" .= storePathToJSON path
        , "name" .= name
        ]

-- Make DerivationOutput JSON serializable
instance Aeson.ToJSON DerivationOutput where
    toJSON (DerivationOutput name path) = Aeson.object
        [ "name" .= name
        , "path" .= storePathToJSON path
        ]

-- | Deserialize a derivation from a ByteString
deserializeDerivation :: BS.ByteString -> Either Text Derivation
deserializeDerivation bs =
    case Aeson.eitherDecode (LBS.fromStrict bs) of
        Left err -> Left $ T.pack $ "JSON parse error: " ++ err
        Right json -> derivationFromJSON json

-- | Convert JSON to Derivation
derivationFromJSON :: Aeson.Value -> Either Text Derivation
derivationFromJSON v =
    case Aeson.parseEither parseDerivation v of
        Left err -> Left $ T.pack err
        Right drv -> Right drv
  where
    parseDerivation = Aeson.withObject "Derivation" $ \o -> do
        name <- o .: "name"
        hash <- o .: "hash"
        builderJSON <- o .: "builder"
        args <- o .: "args"
        inputsJSON <- o .: "inputs"
        outputsJSON <- o .: "outputs"
        envObj <- o .: "env"
        system <- o .: "system"
        strategyText <- o .: "strategy" :: Aeson.Parser Text
        metaObj <- o .: "meta"

        -- Parse nested objects
        builder <- parseStorePath builderJSON

        -- Parse arrays
        inputs <- parseInputs inputsJSON
        outputs <- parseOutputs outputsJSON

        -- Parse maps
        env <- parseEnvMap envObj
        meta <- parseEnvMap metaObj

        -- Parse strategy
        let strategy = case strategyText of
                "monadic" -> MonadicStrategy
                _ -> ApplicativeStrategy

        return Derivation
            { derivName = name
            , derivHash = hash
            , derivBuilder = builder
            , derivArgs = args
            , derivInputs = Set.fromList inputs
            , derivOutputs = Set.fromList outputs
            , derivEnv = env
            , derivSystem = system
            , derivStrategy = strategy
            , derivMeta = meta
            }

    parseStorePath :: Aeson.Value -> Aeson.Parser StorePath
    parseStorePath = Aeson.withObject "StorePath" $ \o -> do
        hash <- o .: "hash"
        name <- o .: "name"
        return $ StorePath hash name

    parseInputs :: Aeson.Value -> Aeson.Parser [DerivationInput]
    parseInputs = Aeson.withArray "Inputs" $ \arr ->
        mapM parseInput (Vector.toList arr)

    parseInput :: Aeson.Value -> Aeson.Parser DerivationInput
    parseInput = Aeson.withObject "DerivationInput" $ \o -> do
        pathObj <- o .: "path"
        name <- o .: "name"
        path <- parseStorePath pathObj
        return $ DerivationInput path name

    parseOutputs :: Aeson.Value -> Aeson.Parser [DerivationOutput]
    parseOutputs = Aeson.withArray "Outputs" $ \arr ->
        mapM parseOutput (Vector.toList arr)

    parseOutput :: Aeson.Value -> Aeson.Parser DerivationOutput
    parseOutput = Aeson.withObject "DerivationOutput" $ \o -> do
        name <- o .: "name"
        pathObj <- o .: "path"
        path <- parseStorePath pathObj
        return $ DerivationOutput name path

    parseEnvMap :: Aeson.Value -> Aeson.Parser (Map Text Text)
    parseEnvMap = Aeson.withObject "Environment" $ \obj ->
        return $ Map.fromList $ map convertKeyValue $ Aeson.KeyMap.toList obj
      where
        convertKeyValue (k, Aeson.String v) = (Aeson.toText k, v)
        convertKeyValue _ = ("", "") -- Fallback for non-string values

-- | Hash a derivation's inputs for dependency tracking
hashDerivationInputs :: Derivation -> Text
hashDerivationInputs drv =
    T.pack $ show $ Ten.Hash.hashText $ T.intercalate ":" $
        map (\input -> storeHash $ inputPath input) $
        Set.toList $ derivInputs drv

-- | Set the file mode (permissions) for a file
setFileMode :: FilePath -> Int -> IO ()
setFileMode path mode = do
    -- Use Posix.setFileMode which accepts a numeric mode
    Posix.setFileMode path (fromIntegral mode)
