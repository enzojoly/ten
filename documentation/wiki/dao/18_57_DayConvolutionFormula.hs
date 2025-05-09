{-
The Day convolution formula:
(F*G)x ≃ ∫^a,b C(a⊗ b,x) × Fa × Gb
-}

-- Implement Day convolution for parallel build composition with formal correctness guarantees:7
data Day f g a where
  Day :: f b -> g c -> ((b, c) -> a) -> Day f g a

parallelBuild :: Day (TenM 'Build) (TenM 'Build) a -> TenM 'Build a
