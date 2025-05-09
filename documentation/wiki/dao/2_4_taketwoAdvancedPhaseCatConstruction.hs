-- The most robust approach would combine multiple categorical constructions:

-- Use phantom types for basic phase separation
-- Extend with type families to define phase-specific capabilities
-- Implement bifunctors for dual-phase transformations
-- Use adjunctions to model the relationship between phases
-- Apply natural transformations for phase transitions

-- Advanced phase-aware system
data Phase = Eval | Build
data Capability = Parse | Hash | Execute | Write

-- Type family for phase capabilities
type family PhaseCapabilities (p :: Phase) :: [Capability]
type instance PhaseCapabilities 'Eval = '[ 'Parse, 'Hash ]
type instance PhaseCapabilities 'Build = '[ 'Execute, 'Write ]

-- Phase-aware monad with capability constraints
newtype TenM (p :: Phase) (c :: [Capability]) a = TenM { ... }

-- Bifunctor for dual-phase operations
data PhasePair a b = PhasePair (TenM 'Eval '[ 'Parse ] a) (TenM 'Build '[ 'Execute ] b)

-- Adjunction for phase relationships
data PhaseAdj = PhaseAdj
  { evalToBuild :: forall a. TenM 'Eval '[ 'Parse ] a -> TenM 'Build '[ 'Execute ] a
  , buildToEval :: forall a. TenM 'Build '[ 'Execute ] a -> TenM 'Eval '[ 'Hash ] (Result a)
  }
