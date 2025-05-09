{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, TypeOperators #-}

-- Phase phantom type
newtype TenM (p :: Phase) a = TenM { ... }

-- Singleton for dependency checking with runtime representation
$(singletons [d|
  pathExists :: StorePath -> Bool
  pathExists (StorePath _) = -- implementation

  allExist :: [StorePath] -> Bool
  allExist [] = True
  allExist (p:ps) = pathExists p && allExist ps
  |])

-- Singletons provide the type-level guarantees with runtime access
buildWithDeps :: Sing (deps :: [StorePath])
              -> (AllExist deps ~ 'True)
              -> TenM 'Build BuildResult

-- Liquid Haskell adds rich verification properties
{-@ type ValidDeps = {deps:[StorePath] |
       allExist deps &&
       noConflicts deps &&
       transitiveComplete deps} @-}

{-@ buildWithLiquid :: deps:ValidDeps ->
                       {v:BuilderScript | scriptCompatible deps v} ->
                       TenM 'Build BuildResult @-}
buildWithLiquid :: [StorePath] -> BuilderScript -> TenM 'Build BuildResult

-- Free category for build graph structure
data BuildPath a b where
  Id :: BuildPath a a
  Step :: Dependency a b -> BuildPath a b
  Compose :: BuildPath a b -> BuildPath b c -> BuildPath a c

-- Add Liquid Haskell verification to the graph structure
{-@ data BuildPath a b where
      Id :: x:a -> BuildPath a a
      Step :: d:Dependency a b -> {v:BuildPath a b | depExists d}
      Compose :: p1:BuildPath a b -> p2:BuildPath b c ->
                {v:BuildPath a c | validComposition p1 p2} @-}

-- Adjunction for phase relationship
evalToBuild :: BuildPath ('Eval a) ('Eval b) -> BuildPath ('Build a) ('Build b)
