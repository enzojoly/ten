-- What IS a Whisker Specifically?

{-
A "whisker" is a way to extend a transformation through function composition while preserving its structure. In concrete programming terms:

A transformation is a function that takes a function and returns a modified version of that function
A whisker is a higher-order function that extends this transformation to work in a new context
-}

{-
Left vs. Right Whiskering: Practical Distinction
-}

-- Left Whiskering
leftWhisker :: (m a -> m b) -> (f m a -> f m b)
{-
In left whiskering, we extend a transformation (m a -> m b) to work in a larger context f (like a different phase). It's "left" because the new context f appears on the left side of the transformation when written in mathematical notation.
Use when: You want the same transformation to work in different phases or contexts while preserving the phase.
-}

-- Right Whiskering
rightWhisker :: (a -> m b) -> (a -> n m b)
{-
In right whiskering, we extend a function (a -> m b) through another transformation n. It's "right" because the new context appears on the right side of the transformation.
Use when: You want to transform a function before applying another transformation to it.
-}

-- Concrete Example: Incremental Build Optimization

-- This is our base transformation - it turns any build function into an incremental one
incrementalBuild ::
  (Derivation -> TenM 'Build BuildResult) ->
  (Derivation -> TenM 'Build BuildResult)
incrementalBuild buildFn = \deriv -> do
  -- Check if anything has changed since last build
  prevBuild <- getPreviousBuild deriv
  case prevBuild of
    Just result | not (inputsChanged deriv) -> do
      logMsg 1 $ "Reusing previous build for " <> derivName deriv
      return result
    _ -> buildFn deriv

{-
This is a transformation that takes a build function and returns a new build function that first checks if we can reuse previous results.
Now here's the right whiskering:
-}

-- This is right whiskering - we're extending our incrementalBuild transformation
withIncrementalBuild ::
  ((Derivation -> TenM 'Build BuildResult) -> (Derivation -> TenM 'Build a)) ->
  ((Derivation -> TenM 'Build BuildResult) -> (Derivation -> TenM 'Build a))
withIncrementalBuild processor = \buildFn -> processor (incrementalBuild buildFn)

{-
We have a processor function that takes a build function and returns some result
We want to apply incrementalBuild before this processor runs
Right whiskering lets us compose these transformations in the correct order
-}

-- Here's what we're actually doing
verifiedIncrementalBuild :: Derivation -> TenM 'Build (BuildResult, Bool)
verifiedIncrementalBuild = withIncrementalBuild $ \buildFn deriv -> do
  result <- buildFn deriv
  verified <- verifyBuildResult deriv result
  return (result, verified)

{-
Without whiskering, we'd have to manually compose these functions:
-}

-- Without whiskering, messy manual composition
verifiedIncrementalBuild' :: Derivation -> TenM 'Build (BuildResult, Bool)
verifiedIncrementalBuild' deriv = do
  -- Duplicate the incremental build logic here
  prevBuild <- getPreviousBuild deriv
  result <- case prevBuild of
    Just r | not (inputsChanged deriv) -> do
      logMsg 1 $ "Reusing previous build for " <> derivName deriv
      return r
    _ -> buildDerivation deriv
  -- Then add verification
  verified <- verifyBuildResult deriv result
  return (result, verified)

{-
Monad Transformers vs. Whiskering
-----------------------------------------------------------
Monad transformers combine effects (like State and Reader) into a single monad.
Whiskering extends transformations to work across different contexts.
-----------------------------------------------------------
Monad transformers: vertical stacking of effects
Whiskering: horizontal composition of transformations
-}

{-
In Ten, whiskering offers concrete benefits:

Separation of Concerns: You define core behaviors (like incremental building) once, separately from how they're used
Composition: You can compose transformations without duplicating logic
Type Safety: Whiskering maintains the phase separation in Ten's type system
Modularity: You can add or remove behaviors (like caching or tracing) without rewriting entire pipelines
-}

{-
Use left whiskering when you have a transformation that should work in multiple phases while preserving phase type safety
Use right whiskering when you need to transform a function before passing it to another higher-order function
-}

-- In Ten, you could implement these whiskers concretely:

-- Left whisker: Make a phase-specific transformation work in any compatible phase
leftWhisker :: (TenM p1 a -> TenM p1 b) -> (TenM p2 a -> TenM p2 b)
leftWhisker transform action = TenM $ do
  env <- ask
  state <- get
  case runTen action env state of
    Left err -> throwError err
    Right (a, state') ->
      case runTen (transform (return a)) env state' of
        Left err -> throwError err
        Right (b, state'') -> do
          put state''
          return b

-- Right whisker: Transform a function before processing it
rightWhisker :: (a -> b) -> ((b -> c) -> (a -> c))
rightWhisker transform processor = \x -> processor (transform x)

-- These are practical implementations, not just theoretical constructs.

{-
Is it useful?
Yes, when:

You need to reuse transformations across different contexts
You want to compose behaviors cleanly
You need to maintain type-level separation

Whiskering is especially valuable in Ten because of the phase separation system. It allows you to create transformations that respect this separation while enabling code reuse.
-}
