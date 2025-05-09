-- Test labeled "prevents access to unauthorized paths" is actually succeeding when it should fail
-- It reads /etc/passwd from a sandbox that should restrict this
-- This suggests the current implementation needs stronger effect isolation

-- Simplified effect tracking
data Effect = NoEffects | FileSystem | Network | Process
  deriving (Eq, Ord, Show)

-- Track phase and effects, but not properties yet
newtype TenM (p :: Phase) (e :: Effect) a = TenM
  {runTenM :: ReaderT BuildEnv (StateT BuildState (ExceptT BuildError IO)) a}

-- Phase-specific operations with appropriate effect constraints
addToStore :: Text -> ByteString -> TenM 'Build 'FileSystem StorePath
evalExpression :: Text -> TenM 'Eval 'NoEffects Derivation
