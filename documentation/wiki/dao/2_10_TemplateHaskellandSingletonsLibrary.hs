{-
-- What is Template Haskell?
Template Haskell (TH) is a metaprogramming facility for Haskell that allows code generation at compile time.
It's a language extension that enables you to write Haskell code that produces more Haskell code during compilation.
Think of it as a sophisticated macro system that operates on the abstract syntax tree level, rather than simple text substitution.
-}

----------------------------------------------------------------------------------------------------------------
-- Basic Template Haskell Syntax

{-# LANGUAGE TemplateHaskell #-}

-- Splice syntax: runs the expression at compile time
$(functionThatGeneratesCode)

-- Quasiquotation: quote Haskell code as data
[d| data MyType = MyConstructor Int |]  -- quotes declarations
[e| 1 + 2 |]  -- quotes expressions
[t| Int -> Bool |]  -- quotes types

-- Reification: get information about existing declarations
$(reify 'someFunction >>= \info -> ...)
-------------------------------------------------------------------------

{-
 Singletons and Template Haskell
 The singletons library uses Template Haskell to automate the creation of singleton types and their related infrastructure:
 -}

{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies #-}
import Data.Singletons.TH

-- This snippet:
$(singletons [d|
  data StorePath = StorePath String

  pathExists :: StorePath -> Bool
  pathExists (StorePath _) = True
  |])

-- Generates all of this:
-- 1. Kind promotion (StorePath becomes a kind)
-- 2. Singleton type (SStorePath connects values to types)
-- 3. Type families (PathExists becomes a type-level function)
-- 4. Singleton functions (sPathExists operates on singletons)
-- and much more!

-- Without Template Haskell, you'd need to write 20-30 lines of complex code for each definition.
