-- Ten leverages Lambek's Lemma (which states that the structure map of an initial algebra is an isomorphism) through its use of fixed points in the dependency graph model.

-- In Ten/Core.hs we see:
fixBuild :: (a -> BuildM 'Eval a) -> a -> BuildM 'Eval a

-- Ten could more explicitly leverage Lambek's Lemma to prove invariants about its build system, such as demonstrating that rebuilds with identical inputs always produce identical outputs.
