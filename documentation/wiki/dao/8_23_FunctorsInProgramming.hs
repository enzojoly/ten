-- Functors in Programming

{-
In programming, functors are typically implemented as type constructors F that support a mapping operation (fmap or <$>) that allows functions to operate "inside" the structure: fmap :: (a -> b) -> F a -> F b
Ten's monad TenM is a functor that enables operations within the context of the build system. The type-level phase parameter makes this particularly interesting:
-}
newtype TenM (p :: Phase) a = TenM
  {runTenM :: ReaderT BuildEnv (StateT BuildState (ExceptT BuildError IO)) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader BuildEnv,
      MonadState BuildState,
      MonadError BuildError,
      MonadIO
    )

-- The TenM functor allows mapping functions over values while preserving the build context and phase information.
