-- The singletons approach is a sound and complete encoding within Haskell's type system.
-- It provides the same guarantees as true dependent types, but with a different ergonomic profile
--
-- Singletons Library
{-# LANGUAGE DataKinds #-}
-- Singletons Library
{-# LANGUAGE GADTs #-}
-- Singletons Library
{-# LANGUAGE TemplateHaskell #-}
-- Singletons Library
{-# LANGUAGE TypeFamilies #-}
-- Singletons Library
{-# LANGUAGE TypeOperators #-}
-- Singletons Library
{-# LANGUAGE UndecidableInstances #-}

import Data.Singletons.Prelude
import Data.Singletons.TH

$( singletons
     [d|
       data StorePath = StorePath String

       pathExists :: StorePath -> Bool
       pathExists (StorePath _) = True -- Simplified for example

       allExist :: [StorePath] -> Bool
       allExist [] = True
       allExist (p : ps) = pathExists p && allExist ps
       |]
 )

-- Now we have:
-- Type-level StorePath, SStorePath singleton, and
-- type families PathExists and AllExist

-- A build operation that guarantees dependencies exist
buildWithDeps ::
  Sing (deps :: [StorePath]) ->
  (AllExist deps ~ 'True) =>
  BuilderScript ->
  IO BuildResult
buildWithDeps deps script = do
  -- Safe to build - dependencies proven to exist
  executeBuilder script
