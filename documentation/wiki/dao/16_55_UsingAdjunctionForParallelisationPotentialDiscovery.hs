-- Parallelization Opportunities

-- Adjunctions clearly define dependencies between computations, revealing what can safely be parallelized:
parallelBuild :: [Derivation] -> TenM 'Build [BuildResult]
parallelBuild derivs = do
  -- Group by dependencies revealed by the adjunction structure
  let independentGroups = categoricallyIndependentGroups derivs
  -- Build each group in parallel
  sequenceA $ map buildGroup independentGroups

-- The adjunction shows exactly which build operations depend on each other, making it safe to parallelize operations that belong to different "fibers" of the adjunction.
