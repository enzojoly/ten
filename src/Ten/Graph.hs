{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ten.Graph (
    -- Graph construction
    createBuildGraph,
    addNode,
    addEdge,
    addDerivation,
    BuildGraph(..),

    -- Graph validation
    validateGraph,
    detectCycles,
    detectCyclesFrom,

    -- Dependency tracking
    getDependencies,
    getTransitiveDependencies,

    -- Build order determination
    topologicalSort,

    -- Recursive derivation handling
    detectRecursionCycle,
    addToDerivationChain,

    -- Graph queries
    findAffected,
    findReverseDependent,
    getSubgraph,

    -- Graph operations
    foldGraph,
    mapGraph,
    transitiveClosure,

    -- Path analysis
    findPath,
    findAllPaths,

    -- Graph serialization
    serializeGraph,
    deserializeGraph
) where

import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (get, gets, modify, put)
import Control.Monad.Except (throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as List
import Data.Maybe (isJust, fromJust, catMaybes)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as Aeson
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import System.IO (withFile, IOMode(..))
import Data.Singletons
import Data.Singletons.TH
import Crypto.Hash (hash, SHA256(..), Digest)
import qualified Crypto.Hash as Crypto

import Ten.Core
import Ten.Store

-- | Create a unique ID for a store path
pathNodeId :: StorePath -> Text
pathNodeId path = "path:" <> storeHash path <> ":" <> storeName path

-- | Create a unique ID for a derivation
derivNodeId :: Derivation -> Text
derivNodeId drv = "drv:" <> derivHash drv

-- | Create a unique ID for an output
outputNodeId :: StorePath -> Text
outputNodeId path = "out:" <> storeHash path <> ":" <> storeName path

computeReachablePaths :: (StoreScanOps t) => Set StorePath -> TenM p t (Set StorePath)
computeReachablePaths rootPaths = do
    env <- ask

    -- Traverse the dependency graph using breadth-first search
    let bfs visited [] = return visited
        bfs visited (path:queue)
            | path `Set.member` visited = bfs visited queue
            | otherwise = do
                -- Get direct references from this path
                refs <- getReferencesFromPath path
                -- Continue traversal with new references added to queue
                let newVisited = Set.insert path visited
                let newQueue = queue ++ [r | r <- Set.toList refs, not (r `Set.member` newVisited)]
                bfs newVisited newQueue

    -- Start with the root paths
    bfs Set.empty (Set.toList rootPaths)

-- | Empty build graph
emptyGraph :: BuildGraph
emptyGraph = BuildGraph
    { graphNodes = Map.empty
    , graphEdges = Map.empty
    , graphRoots = Set.empty
    , graphProof = Nothing
    }

-- | Create a build graph from a set of derivations
-- Works in both privilege tiers via different mechanisms
createBuildGraph :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> Set StorePath -> Set Derivation -> TenM 'Eval t BuildGraph
createBuildGraph st requestedOutputs derivations = do
    -- Start with an empty graph
    let graph = emptyGraph

    -- Add all derivations to the graph
    graph' <- foldM (\g d -> addDerivation st g d) graph (Set.toList derivations)

    -- Add requested outputs as roots
    let graph'' = graph' { graphRoots = Set.map outputNodeId requestedOutputs }

    -- Validate the graph
    proof <- validateGraph st graph''

    -- Return the validated graph
    return $ graph'' { graphProof = Just proof }

-- | Add a node to the graph
-- Pure operation, no privilege constraints
addNode :: BuildGraph -> Text -> BuildNode -> BuildGraph
addNode graph nodeId node =
    graph { graphNodes = Map.insert nodeId node (graphNodes graph) }

-- | Add an edge to the graph (dependency relationship)
-- Pure operation, no privilege constraints
addEdge :: BuildGraph -> Text -> Text -> BuildGraph
addEdge graph fromId toId =
    let
        currentDeps = Map.findWithDefault Set.empty fromId (graphEdges graph)
        newDeps = Set.insert toId currentDeps
    in
        graph { graphEdges = Map.insert fromId newDeps (graphEdges graph) }

-- | Add a derivation to the graph
-- Works in both privilege tiers via appropriate mechanisms
addDerivation :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> Derivation -> TenM 'Eval t BuildGraph
addDerivation st graph drv = do
    -- Add the derivation node
    let nodeId = derivNodeId drv
    let graph' = addNode graph nodeId (DerivationNode drv)

    -- Add all input nodes and edges
    graph'' <- foldM (\g input -> do
        -- Get the node IDs
        let inputId = pathNodeId (inputPath input)
        let drvId = derivNodeId drv

        -- Add the input node if it doesn't exist
        let g' = if Map.member inputId (graphNodes g)
                  then g
                  else addNode g inputId (InputNode (inputPath input))

        -- Add the edge: derivation depends on input
        return $ addEdge g' drvId inputId
      ) graph' (Set.toList $ derivInputs drv)

    -- Add all output nodes and edges
    foldM (\g output -> do
        -- Get the node IDs
        let outputId = outputNodeId (outputPath output)
        let drvId = derivNodeId drv

        -- Add the output node
        let g' = addNode g outputId (OutputNode (outputPath output) drv)

        -- Add the edge: output depends on derivation
        return $ addEdge g' outputId drvId
      ) graph'' (Set.toList $ derivOutputs drv)

-- | Validate a build graph
-- Works in both privilege tiers via singleton evidence
validateGraph :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> TenM 'Eval t GraphProof
validateGraph st graph = do
    -- Check for cycles
    (hasCycles, _, _) <- detectCycles st graph
    when hasCycles $
        throwError $ GraphError "Dependency cycle detected in build graph"

    -- Check for completeness (all dependencies are present)
    isComplete <- checkCompleteness st graph
    unless isComplete $
        throwError $ GraphError "Build graph is missing some dependencies"

    -- Return the appropriate proof
    return ValidProof

-- | Detect cycles in the graph using depth-first search
-- Works in both privilege tiers via singleton evidence
detectCycles :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> TenM 'Eval t (Bool, Set Text, Set Text)
detectCycles st graph = do
    -- Check each node that hasn't been visited yet
    foldM (\state@(found, visited, stack) nodeId ->
            if found
                then return state  -- Short-circuit if cycle already found
                else checkNode state nodeId
          )
          (False, Set.empty, Set.empty)
          (Map.keys $ graphNodes graph)
  where
    checkNode :: (Bool, Set Text, Set Text) -> Text -> TenM 'Eval t (Bool, Set Text, Set Text)
    checkNode state@(True, _, _) _ = return state  -- Cycle already found
    checkNode (False, visited, recStack) nodeId =
        if nodeId `Set.member` visited
            then return (False, visited, recStack)
            else detectCyclesFrom st graph nodeId visited recStack

-- | Detect cycles starting from a specific node
-- Works in both privilege tiers via singleton evidence
detectCyclesFrom :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> Text -> Set Text -> Set Text -> TenM 'Eval t (Bool, Set Text, Set Text)
detectCyclesFrom st graph nodeId visited recStack = do
    -- Mark current node as visited and add to recursion stack
    let visited' = Set.insert nodeId visited
    let recStack' = Set.insert nodeId recStack

    -- Get dependencies
    let deps = Map.findWithDefault Set.empty nodeId (graphEdges graph)

    -- Check each dependency
    foldM (\state@(found, visited'', recStack'') depId ->
            if found
                then return state  -- Short-circuit if cycle already found
                else if depId `Set.member` recStack''
                    then return (True, visited'', recStack'')  -- Cycle detected
                    else if depId `Set.member` visited''
                        then return (False, visited'', recStack'')  -- Already visited, no cycle
                        else detectCyclesFrom st graph depId visited'' recStack''
          )
          (False, visited', recStack')
          (Set.toList deps)

-- | Check graph completeness (all dependencies exist as nodes)
-- Works in both privilege tiers via singleton evidence
checkCompleteness :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> TenM 'Eval t Bool
checkCompleteness _ graph = do
    -- Check each edge to make sure its target exists
    let allNodes = Map.keysSet $ graphNodes graph
    let allDeps = Set.unions $ Map.elems $ graphEdges graph
    return $ allDeps `Set.isSubsetOf` allNodes

-- | Topologically sort the graph (return build order)
-- Works in both privilege tiers via singleton evidence
topologicalSort :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> TenM 'Eval t [BuildNode]
topologicalSort st graph = do
    -- Verify the graph is acyclic
    case graphProof graph of
        Just ValidProof -> pure ()
        _ -> do
            (hasCycles, _, _) <- detectCycles st graph
            when hasCycles $
                throwError $ GraphError "Cannot sort graph with cycles"

    -- Run topological sort
    let nodeIds = Map.keys $ graphNodes graph
    sorted <- topologicalSortInternal st graph nodeIds Set.empty []

    -- Convert sorted IDs to nodes
    return $ catMaybes $ map (\nodeId -> Map.lookup nodeId (graphNodes graph)) sorted

-- | Internal topological sort implementation
-- Works in both privilege tiers via singleton evidence
topologicalSortInternal :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> [Text] -> Set Text -> [Text] -> TenM 'Eval t [Text]
topologicalSortInternal _ _ [] _ result = return $ reverse result
topologicalSortInternal st graph (nodeId:rest) visited result =
    if nodeId `Set.member` visited
        then topologicalSortInternal st graph rest visited result
        else do
            -- Visit all dependencies first
            let deps = Map.findWithDefault Set.empty nodeId (graphEdges graph)
            let depsToVisit = filter (\dep -> not $ dep `Set.member` visited) $ Set.toList deps
            result' <- topologicalSortInternal st graph depsToVisit (Set.insert nodeId visited) result
            -- Then add this node
            topologicalSortInternal st graph rest (Set.insert nodeId visited) (nodeId:result')

-- | Get direct dependencies of a node
-- Pure operation, no privilege constraints
getDependencies :: BuildGraph -> Text -> Set Text
getDependencies graph nodeId =
    Map.findWithDefault Set.empty nodeId (graphEdges graph)

-- | Get all transitive dependencies of a node
-- Works in both privilege tiers via singleton evidence
getTransitiveDependencies :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> Text -> TenM 'Eval t (Set Text)
getTransitiveDependencies st graph nodeId = do
    -- Start with direct dependencies
    let directDeps = getDependencies graph nodeId

    -- For each direct dependency, get its transitive dependencies
    transDeps <- foldM (\acc dep -> do
        depDeps <- getTransitiveDependencies st graph dep
        return $ Set.union acc depDeps
      ) Set.empty (Set.toList directDeps)

    -- Return all dependencies (direct + transitive)
    return $ Set.union directDeps transDeps

-- | Find nodes affected by a change
-- Works in both privilege tiers via singleton evidence
findAffected :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> Set StorePath -> TenM 'Eval t (Set Text)
findAffected st graph changedPaths = do
    -- Convert paths to node IDs
    let changedIds = Set.map pathNodeId changedPaths

    -- Find all nodes that depend on the changed nodes (reverse edges)
    foldM (\affected nodeId -> do
        -- Find all nodes that depend on this one
        dependent <- findReverseDependent st graph nodeId
        return $ Set.union affected dependent
      ) changedIds (Set.toList changedIds)

-- | Find all nodes that depend on a given node (reverse edges)
-- Works in both privilege tiers via singleton evidence
findReverseDependent :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> Text -> TenM 'Eval t (Set Text)
findReverseDependent st graph nodeId = do
    -- Find all edges that point to this node
    let reverseEdges = Map.filter (Set.member nodeId) (graphEdges graph)
    let dependentNodes = Map.keysSet reverseEdges

    -- Find nodes that depend on the dependent nodes (recursive)
    foldM (\acc dep -> do
        depDeps <- findReverseDependent st graph dep
        return $ Set.union acc depDeps
      ) dependentNodes (Set.toList dependentNodes)

-- | Compute the transitive closure of a set of nodes
-- Works in both privilege tiers via singleton evidence
transitiveClosure :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> Set Text -> TenM 'Eval t (Set Text)
transitiveClosure st graph startNodes = do
    -- For each starting node, get all transitive dependencies
    foldM (\closure nodeId -> do
        deps <- getTransitiveDependencies st graph nodeId
        return $ Set.union closure (Set.insert nodeId deps)
      ) Set.empty (Set.toList startNodes)

-- | Generic fold over a graph
-- Pure operation, no privilege constraints
foldGraph :: (a -> BuildNode -> a) -> a -> BuildGraph -> a
foldGraph f initial graph =
    Map.foldl' (\acc node -> f acc node) initial (graphNodes graph)

-- | Map a function over all nodes in a graph
-- Pure operation, no privilege constraints
mapGraph :: (BuildNode -> BuildNode) -> BuildGraph -> BuildGraph
mapGraph f graph =
    graph { graphNodes = Map.map f (graphNodes graph) }

-- | Get a subgraph containing only specified nodes and their dependencies
-- Works in both privilege tiers via singleton evidence
getSubgraph :: forall (t :: PrivilegeTier).
    (SingI t) =>
    SPrivilegeTier t -> BuildGraph -> Set Text -> TenM 'Eval t BuildGraph
getSubgraph st graph nodeIds = do
    -- Get transitive closure of specified nodes
    closure <- transitiveClosure st graph nodeIds

    -- Create a new graph with only these nodes and their edges
    let newNodes = Map.filterWithKey (\k _ -> k `Set.member` closure) (graphNodes graph)
    let newEdges = Map.filterWithKey (\k _ -> k `Set.member` closure) (graphEdges graph)
    let newRoots = Set.intersection nodeIds closure

    return BuildGraph
        { graphNodes = newNodes
        , graphEdges = newEdges
        , graphRoots = newRoots
        , graphProof = Nothing  -- Reset proof, needs revalidation
        }

-- | Find a path between two nodes (if exists)
-- Pure operation, no privilege constraints
findPath :: BuildGraph -> Text -> Text -> Maybe [Text]
findPath graph start end =
    findPathDFS graph start end Set.empty []

-- | Depth-first search to find a path
-- Pure operation, no privilege constraints
findPathDFS :: BuildGraph -> Text -> Text -> Set Text -> [Text] -> Maybe [Text]
findPathDFS graph current target visited path
    | current == target = Just (reverse (current:path))
    | current `Set.member` visited = Nothing
    | otherwise =
        let
            visited' = Set.insert current visited
            path' = current:path
            neighbors = Map.findWithDefault Set.empty current (graphEdges graph)
            validNeighbors = Set.filter (\n -> not $ n `Set.member` visited') neighbors
        in
            findFirstPath $ map (\next ->
                findPathDFS graph next target visited' path'
              ) (Set.toList validNeighbors)
  where
    findFirstPath [] = Nothing
    findFirstPath (Just p:_) = Just p
    findFirstPath (Nothing:rest) = findFirstPath rest

-- | Find all paths between two nodes
-- Pure operation, no privilege constraints
findAllPaths :: BuildGraph -> Text -> Text -> [[Text]]
findAllPaths graph start end =
    findAllPathsDFS graph start end Set.empty []

-- | Depth-first search to find all paths
-- Pure operation, no privilege constraints
findAllPathsDFS :: BuildGraph -> Text -> Text -> Set Text -> [Text] -> [[Text]]
findAllPathsDFS graph current target visited path
    | current == target = [reverse (current:path)]
    | current `Set.member` visited = []
    | otherwise =
        let
            visited' = Set.insert current visited
            path' = current:path
            neighbors = Map.findWithDefault Set.empty current (graphEdges graph)
            validNeighbors = Set.filter (\n -> not $ n `Set.member` visited') neighbors
        in
            concat $ map (\next ->
                findAllPathsDFS graph next target visited' path'
              ) (Set.toList validNeighbors)

-- | Check for a recursion cycle in a derivation chain
-- Works in any phase, any privilege tier
detectRecursionCycle :: forall (p :: Phase) (t :: PrivilegeTier).
    (SingI p, SingI t) =>
    Derivation -> TenM p t Bool
detectRecursionCycle drv = do
    -- Get the current derivation chain
    chain <- gets buildChain

    -- Use a more efficient algorithm that handles structural equality properly
    -- and is optimized for potentially long chains
    if null chain || length chain < 2
        then return False
        else do
            -- Map each derivation to its hash for faster comparison
            let hashes = map derivHash chain
            let newHash = derivHash drv
            -- Check if the new derivation's hash is in the chain (O(n) complexity)
            return $ newHash `elem` hashes

-- | Add a derivation to the chain for cycle detection
-- Works in any phase and privilege tier
addToDerivationChain :: forall (p :: Phase) (t :: PrivilegeTier).
    (SingI p, SingI t) =>
    Derivation -> TenM p t ()
addToDerivationChain drv = do
    depth <- gets recursionDepth
    maxDepth <- asks maxRecursionDepth
    when (depth >= maxDepth) $
        throwError $ RecursionLimit $ "Maximum recursion depth exceeded: " <> T.pack (show maxDepth)
    modify $ \s -> s { buildChain = drv : buildChain s, recursionDepth = depth + 1 }

-- | Serialize a build graph to JSON
-- Pure operation, no privilege constraints
serializeGraph :: BuildGraph -> LBS.ByteString
serializeGraph graph = Aeson.encode $ Aeson.object
    [ "nodes" .= serializeNodes (graphNodes graph)
    , "edges" .= serializeEdges (graphEdges graph)
    , "roots" .= Set.toList (graphRoots graph)
    ]
  where
    serializeNodes nodes =
        Map.toList $ Map.mapWithKey (\k v -> serializeNode k v) nodes

    serializeNode key node = Aeson.object $
        case node of
            InputNode path ->
                [ "type" .= ("input" :: Text)
                , "id" .= key
                , "path" .= serializePath path
                ]
            DerivationNode drv ->
                [ "type" .= ("derivation" :: Text)
                , "id" .= key
                , "hash" .= derivHash drv
                , "name" .= derivName drv
                ]
            OutputNode path drv ->
                [ "type" .= ("output" :: Text)
                , "id" .= key
                , "path" .= serializePath path
                , "derivation" .= derivHash drv
                ]

    serializePath path = Aeson.object
        [ "hash" .= storeHash path
        , "name" .= storeName path
        ]

    serializeEdges edges =
        Map.toList $ Map.map Set.toList edges

-- | Deserialize a build graph from JSON
-- Pure operation, no privilege constraints
deserializeGraph :: LBS.ByteString -> Either Text BuildGraph
deserializeGraph json =
    case Aeson.eitherDecode json of
        Left err -> Left $ T.pack $ "JSON parse error: " ++ err
        Right value -> case value of
            Aeson.Object obj -> do
                -- Parse nodes
                nodesVal <- maybe (Left "Missing nodes field") Right $
                            KeyMap.lookup "nodes" obj
                nodes <- parseNodes nodesVal

                -- Parse edges
                edgesVal <- maybe (Left "Missing edges field") Right $
                            KeyMap.lookup "edges" obj
                edges <- parseEdges edgesVal

                -- Parse roots
                rootsVal <- maybe (Left "Missing roots field") Right $
                            KeyMap.lookup "roots" obj
                roots <- parseRoots rootsVal

                Right $ BuildGraph
                    { graphNodes = nodes
                    , graphEdges = edges
                    , graphRoots = roots
                    , graphProof = Nothing -- Will be revalidated
                    }
            _ -> Left "Invalid JSON format for graph"
  where
    parseNodes :: Aeson.Value -> Either Text (Map Text BuildNode)
    parseNodes (Aeson.Array arr) = do
        nodesList <- mapM parseNodeEntry (Vector.toList arr)
        return $ Map.fromList nodesList
    parseNodes _ = Left "Nodes field must be an array"

    parseNodeEntry :: Aeson.Value -> Either Text (Text, BuildNode)
    parseNodeEntry (Aeson.Array pair) =
        if Vector.length pair >= 2 then do
            key <- parseText (pair Vector.! 0)
            node <- parseNode (pair Vector.! 1)
            return (key, node)
        else
            Left "Node entry must be a pair [id, node]"
    parseNodeEntry _ = Left "Node entry must be an array"

    parseNode :: Aeson.Value -> Either Text BuildNode
    parseNode (Aeson.Object obj) = do
        nodeType <- maybe (Left "Missing node type") parseText $
                   KeyMap.lookup "type" obj
        case nodeType of
            "input" -> do
                pathObj <- maybe (Left "Missing path in input node") Right $
                          KeyMap.lookup "path" obj
                path <- parsePath pathObj
                return $ InputNode path
            "derivation" -> do
                hash <- maybe (Left "Missing hash in derivation node") parseText $
                       KeyMap.lookup "hash" obj
                name <- maybe (Left "Missing name in derivation node") parseText $
                       KeyMap.lookup "name" obj

                -- Construct minimal derivation for graph purposes
                let drv = Derivation {
                        derivName = name,
                        derivHash = hash,
                        derivBuilder = StorePath "unknown" "unknown",
                        derivArgs = [],
                        derivInputs = Set.empty,
                        derivOutputs = Set.empty,
                        derivEnv = Map.empty,
                        derivSystem = "unknown",
                        derivStrategy = MonadicStrategy,
                        derivMeta = Map.empty
                    }
                return $ DerivationNode drv

            "output" -> do
                pathObj <- maybe (Left "Missing path in output node") Right $
                          KeyMap.lookup "path" obj
                path <- parsePath pathObj
                hash <- maybe (Left "Missing derivation hash in output node") parseText $
                       KeyMap.lookup "derivation" obj

                -- Handle optional name field consistently with proper Either monad treatment
                name <- case KeyMap.lookup "name" obj of
                    Just nameValue -> parseText nameValue
                    Nothing -> Right "unknown"  -- Default value wrapped in Right

                -- Construct minimal derivation for graph purposes
                let drv = Derivation {
                        derivName = name,
                        derivHash = hash,
                        derivBuilder = StorePath "unknown" "unknown",
                        derivArgs = [],
                        derivInputs = Set.empty,
                        derivOutputs = Set.empty,
                        derivEnv = Map.empty,
                        derivSystem = "unknown",
                        derivStrategy = MonadicStrategy,
                        derivMeta = Map.empty
                    }
                return $ OutputNode path drv

            _ -> Left $ "Unknown node type: " <> nodeType
    parseNode _ = Left "Node must be an object"

    parsePath :: Aeson.Value -> Either Text StorePath
    parsePath (Aeson.Object obj) = do
        hash <- maybe (Left "Missing hash in path") parseText $
               KeyMap.lookup "hash" obj
        name <- maybe (Left "Missing name in path") parseText $
               KeyMap.lookup "name" obj
        return $ StorePath hash name
    parsePath _ = Left "Path must be an object"

    parseEdges :: Aeson.Value -> Either Text (Map Text (Set Text))
    parseEdges (Aeson.Array arr) = do
        edgesList <- mapM parseEdgeEntry (Vector.toList arr)
        return $ Map.fromList edgesList
    parseEdges _ = Left "Edges field must be an array"

    parseEdgeEntry :: Aeson.Value -> Either Text (Text, Set Text)
    parseEdgeEntry (Aeson.Array pair) =
        if Vector.length pair >= 2 then do
            key <- parseText (pair Vector.! 0)
            depList <- parseTextArray (pair Vector.! 1)
            return (key, Set.fromList depList)
        else
            Left "Edge entry must be a pair [id, [dependencies]]"
    parseEdgeEntry _ = Left "Edge entry must be an array"

    parseRoots :: Aeson.Value -> Either Text (Set Text)
    parseRoots val@(Aeson.Array _) = do
        rootsList <- parseTextArray val
        return $ Set.fromList rootsList
    parseRoots _ = Left "Roots field must be an array"

    parseTextArray :: Aeson.Value -> Either Text [Text]
    parseTextArray (Aeson.Array arr) =
        mapM parseText (Vector.toList arr)
    parseTextArray _ = Left "Expected text array"

    parseText :: Aeson.Value -> Either Text Text
    parseText (Aeson.String txt) = Right txt
    parseText _ = Left "Expected string value"
