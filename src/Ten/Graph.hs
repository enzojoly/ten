{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Ten.Graph (
    BuildNode(..),
    BuildGraph(..),
    GraphProof(..),
    createBuildGraph,
    validateGraph,
    topologicalSort,
    findAffected,
    transitiveClosure,
    foldGraph
) where

import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (foldl')

-- Use qualified import for Ten.Core to avoid name collisions
import qualified Ten.Core as Core
-- Still import specific names from Ten.Core that don't cause collisions
import Ten.Core (TenM, Phase(..), BuildError(..), StorePath(..))
import Ten.Derivation
import Ten.Store

-- | Node in a build graph
data BuildNode
    = InputNode StorePath                  -- An input that doesn't need to be built
    | DerivationNode Derivation            -- A derivation that needs to be built
    | OutputNode StorePath Derivation      -- An output produced by a derivation
    deriving (Show, Eq)

-- | Proof about a build graph
data GraphProof
    = AcyclicProof      -- Graph has no cycles
    | CompleteProof     -- Graph contains all dependencies
    | ValidProof        -- Graph is both acyclic and complete
    deriving (Show, Eq)

-- | A build graph representing the dependency relationships
data BuildGraph = BuildGraph
    { graphNodes :: Map Text BuildNode     -- Nodes indexed by ID
    , graphEdges :: Map Text (Set Text)    -- Edges from node -> dependencies
    , graphRoots :: Set Text               -- Root nodes (outputs requested)
    , graphProof :: Maybe GraphProof       -- Proof about this graph
    } deriving (Show, Eq)

-- | Create a node ID for a store path
pathNodeId :: StorePath -> Text
pathNodeId path = "path:" <> storeHash path <> ":" <> storeName path

-- | Create a node ID for a derivation
derivNodeId :: Derivation -> Text
derivNodeId deriv = "drv:" <> derivName deriv

-- | Create a node ID for an output
outputNodeId :: StorePath -> Text
outputNodeId path = "out:" <> storeHash path <> ":" <> storeName path

-- | Create an empty build graph
emptyGraph :: BuildGraph
emptyGraph = BuildGraph
    { graphNodes = Map.empty
    , graphEdges = Map.empty
    , graphRoots = Set.empty
    , graphProof = Nothing
    }

-- | Create a build graph from a set of derivations
createBuildGraph :: Set StorePath -> Set Derivation -> TenM 'Eval BuildGraph
createBuildGraph requestedOutputs derivations = do
    -- Start with an empty graph
    let graph = emptyGraph

    -- Add all derivations to the graph
    graph' <- foldM addDerivation graph (Set.toList derivations)

    -- Add requested outputs as roots
    let graph'' = graph' { graphRoots = Set.map outputNodeId requestedOutputs }

    -- Validate the graph
    proof <- validateGraph graph''

    -- Return the validated graph
    return $ graph'' { graphProof = Just proof }

-- | Add a derivation to the graph
addDerivation :: BuildGraph -> Derivation -> TenM 'Eval BuildGraph
addDerivation graph deriv = do
    -- Add the derivation node
    let nodeId = derivNodeId deriv
    let graph' = graph
            { graphNodes = Map.insert nodeId (DerivationNode deriv) (graphNodes graph)
            }

    -- Add input nodes and edges
    let inputs = derivInputs deriv
    graph'' <- foldM addInputEdge graph' (Set.toList inputs)

    -- Add output nodes and edges
    let outputs = derivOutputs deriv
    graph''' <- foldM (addOutputEdge deriv) graph'' (Set.toList outputs)

    return graph'''

-- | Add an input edge to the graph
addInputEdge :: BuildGraph -> DerivationInput -> TenM 'Eval BuildGraph
addInputEdge graph input = do
    -- Get the node IDs
    let inputId = pathNodeId (inputPath input)
    let derivId = derivNodeId deriv  -- Need to recover the derivation here

    -- Add the input node if it doesn't exist
    let graph' = if Map.member inputId (graphNodes graph)
                   then graph
                   else graph
                        { graphNodes = Map.insert inputId (InputNode (inputPath input)) (graphNodes graph)
                        }

    -- Add the edge: derivation depends on input
    let edges' = Map.insertWith Set.union derivId (Set.singleton inputId) (graphEdges graph')

    return $ graph' { graphEdges = edges' }
  where
    -- This is a placeholder - in a real implementation, we would have the
    -- derivation available in the context or as a parameter
    deriv = error "Derivation not available"

-- | Add an output edge to the graph
addOutputEdge :: Derivation -> BuildGraph -> DerivationOutput -> TenM 'Eval BuildGraph
addOutputEdge deriv graph output = do
    -- Get the node IDs
    let outputId = outputNodeId (outputPath output)
    let derivId = derivNodeId deriv

    -- Add the output node
    let graph' = graph
            { graphNodes = Map.insert outputId (OutputNode (outputPath output) deriv) (graphNodes graph)
            }

    -- Add the edge: output depends on derivation
    let edges' = Map.insertWith Set.union outputId (Set.singleton derivId) (graphEdges graph')

    return $ graph' { graphEdges = edges' }

-- | Validate a build graph
validateGraph :: BuildGraph -> TenM 'Eval GraphProof
validateGraph graph = do
    -- Check for cycles
    hasCycles <- detectCycles graph
    when hasCycles $
        throwError $ GraphError "Dependency cycle detected in build graph"

    -- Check for completeness (all dependencies are present)
    isComplete <- checkCompleteness graph
    unless isComplete $
        throwError $ GraphError "Build graph is missing some dependencies"

    -- Add acyclic proof - Use qualified name from Ten.Core
    Core.addProof Core.AcyclicProof

    -- Return the appropriate proof
    return ValidProof

-- | Detect cycles in the graph (simplified implementation)
detectCycles :: BuildGraph -> TenM 'Eval Bool
detectCycles graph = do
    -- This is a simplified implementation that doesn't actually detect cycles
    -- In a real implementation, we would use a standard graph algorithm
    return False

-- | Check graph completeness (simplified implementation)
checkCompleteness :: BuildGraph -> TenM 'Eval Bool
checkCompleteness _ = do
    -- This is a simplified implementation
    -- In a real implementation, we would verify all dependencies exist
    return True

-- | Topologically sort the graph (return build order)
topologicalSort :: BuildGraph -> TenM 'Eval [BuildNode]
topologicalSort graph = do
    -- Verify the graph is acyclic - Use qualified references
    case graphProof graph of
        Just AcyclicProof -> pure ()
        Just ValidProof -> pure ()
        _ -> validateGraph graph >> pure ()

    -- Simple implementation - this should be replaced with a real topological sort
    let nodes = Map.elems (graphNodes graph)

    -- In a real implementation, we would use a standard topological sort algorithm
    return nodes

-- | Find nodes affected by a change
findAffected :: BuildGraph -> Set StorePath -> TenM p (Set Text)
findAffected graph changedPaths = do
    -- Convert paths to node IDs
    let changedIds = Set.map pathNodeId changedPaths

    -- In a real implementation, we would traverse the graph to find all nodes
    -- that depend on the changed paths
    return changedIds

-- | Compute the transitive closure of a set of nodes
transitiveClosure :: BuildGraph -> Set Text -> TenM p (Set Text)
transitiveClosure graph startNodes = do
    -- In a real implementation, we would traverse the graph to find all nodes
    -- that depend on the start nodes
    return startNodes

-- | Generic fold over a graph
foldGraph :: (a -> BuildNode -> a) -> a -> BuildGraph -> a
foldGraph f initial graph =
    foldl' f initial (Map.elems (graphNodes graph))
