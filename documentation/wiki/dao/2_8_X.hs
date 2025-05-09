-- Phantom types: Control what operations are allowed in which phase
-- Singletons: Verify dependency existence
-- Free categories: Model the structure of the build graph
-- Adjunctions: Formalize the relationship between phases

-- Phase phantom type
newtype TenM (p :: Phase) a = TenM { ... }

-- Singleton for dependency checking
buildWithDeps :: Sing (deps :: [StorePath])
              -> (AllExist deps ~ 'True)
              -> TenM 'Build BuildResult

-- Free category for build graph structure
data BuildPath a b where
  Id :: BuildPath a a
  Step :: Dependency a b -> BuildPath a b
  Compose :: BuildPath a b -> BuildPath b c -> BuildPath a c

-- Adjunction for phase relationship
evalToBuild :: BuildPath ('Eval a) ('Eval b) -> BuildPath ('Build a) ('Build b)
