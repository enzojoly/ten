-- Free/Forgetful Adjunctions

{- Current Implementation: Ten implicitly constructs derivations from basic components but doesn't use free/forgetful adjunctions explicitly: -}
mkDerivation :: Text -> StorePath -> [Text] -> Set DerivationInput -> Set Text -> Map Text Text -> TenM 'Eval Derivation

{- Potential Enhancement: Implement a proper free/forgetful adjunction for derivation construction: -}

-- Free functor: construct derivations from basic components
free :: BuildComponents -> Derivation

-- Forgetful functor: extract basic components from derivations
forget :: Derivation -> BuildComponents

-- Properties of the adjunction (free ⊣ forget)
extractComponents :: Derivation -> BuildComponents
embedComponents :: BuildComponents -> Derivation
