-- "Cocartesian Categorics" (dealing with sum types from a categorical perspective)
-- Relates to how Ten models its build dependency graph
-- Ten's build graph is essentially a cocartesian structure where:

-- "One Plus Zero" - Phases are strictly separated (either evaluation or build, never both)
-- "Something Plus Zero" - A path either exists in the store or doesn't
-- "Commutativity" - Build order independence for unrelated derivations
-- "Associativity" - Nested dependencies resolve consistently

--In Ten.Graph, the BuildNode sum type implements this cocartesian structure:
data BuildNode
    = InputNode StorePath
    | DerivationNode Derivation
    | OutputNode StorePath Derivation
