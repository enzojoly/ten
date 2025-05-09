-- From Ten.Graph.hs
findAffected :: BuildGraph -> Set StorePath -> TenM p (Set Text)
transitiveClosure :: BuildGraph -> Set Text -> TenM p (Set Text)

{-
These functions deal with the "dependents" relationship, which is contravariant - if A depends on B, then changes to B affect A. The contravariant Yoneda lemma provides a foundation for analyzing these relationships.
Ten could enhance its design by:

Using contravariant Yoneda for dependency analysis
Implementing more efficient impact analysis algorithms
Developing better incremental build techniques
Optimizing the dependency graph structure
-}
