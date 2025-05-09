-- Terminal Coalgebras for Derivation Fixed Points:
-- Terminal coalgebra concepts could be applied to formally model the fixpoint semantics of recursive derivations:
fixDerivation :: (Derivation -> BuildM 'Eval Derivation) -> BuildM 'Eval Derivation
