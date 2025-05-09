-- Functors vs. Applicatives

{-
Functors have a more limited capability:
Functors let you apply a regular function to a value in a context
-}

fmap :: (a -> b) -> f a -> f b
{- You can only use a "plain" function, not one that's already in a context

-- Functor example -}
fmap (+1) (Just 2)  -- Just 3
fmap (+1) [1,2,3]   -- [2,3,4]

-- Applicatives vs. Monads

{-
Monads indeed subsume applicatives (every monad is an applicative), but there are good reasons to use applicatives when appropriate:

Intent clarity: Using applicatives signals that your computation structure is fixed and doesn't depend on intermediate results
Potential for parallelism: Since applicative operations don't depend on previous results, they could theoretically be executed in parallel
Simpler instances: Some types can be applicatives but not monads
Cleaner syntax for certain patterns, like applying multi-parameter functions to several effects:
-}

-- Applicative style
(+) <$> getNumber1 <*> getNumber2

-- vs Monadic style
do
  x <- getNumber1
  y <- getNumber2
  return (x + y)
-- The key distinction: applicatives combine independent effects, while monads allow effects to depend on previous computations.
