-- Original code:
$(singletons [d|
  data StorePath = StorePath String

  pathExists :: StorePath -> Bool
  pathExists (StorePath _) = True
  |])

-- Generated code (simplified):
-- Data type promotion
data StorePath = StorePath String

-- Singleton type
data SStorePath :: StorePath -> Type where
  SStorePath :: String -> SStorePath ('StorePath str)

-- Type-level function
type family PathExists (p :: StorePath) :: Bool where
  PathExists ('StorePath _) = 'True

-- Singleton function
sPathExists :: SStorePath p -> SBool (PathExists p)
sPathExists _ = STrue

-- Type class for conversion
class SingI (a :: StorePath) where
  sing :: SStorePath a

-- And much more...
