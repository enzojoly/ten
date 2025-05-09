{-
Sum as a Universal Cospan

In category theory, a sum (or coproduct) of objects A and B is an object A+B together with morphisms i₁: A → A+B and i₂: B → A+B such that for any object C and morphisms f: A → C and g: B → C, there exists a unique morphism [f,g]: A+B → C such that [f,g] ∘ i₁ = f and [f,g] ∘ i₂ = g.

This is a universal cospan construction.
-}

-- Ten's error handling embodies sum-like structures:

-- From Ten.Core.hs
data BuildError
  = EvalError Text
  | BuildFailed Text
  | StoreError Text
  | SandboxError Text
  | InputNotFound FilePath
  | HashError Text
  | GraphError Text
  | ResourceError Text

{-
This sum type allows different error cases to be handled uniformly. Ten could further leverage the sum's universal property by:

Implementing a more principled error handling system based on cospans
Providing universal combinators for error transformation
Supporting better error recovery through the universal property
-}
