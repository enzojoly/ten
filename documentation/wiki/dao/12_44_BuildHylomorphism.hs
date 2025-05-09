-- Hylomorphisms for Build Operations:

{-
Hylomorphisms could elegantly express Ten's core build process, which:

First unfolds a dependency graph from derivations (anamorphism)
Then processes the graph to produce build results (catamorphism)
-}

buildHylo :: (a -> GraphF a) -> (GraphF b -> b) -> a -> b
buildHylo ana cata seed = cata . fmap (buildHylo ana cata) . ana $ seed
