module Test.Prelude
  ( mkAst
  , mkAnon
  , mkDef
  , mkBind
  , module Lambda.Prelude
  , module X
  ) where

import Lambda.Prelude

-- Re-exports
import Lambda.Language.Name (Name, name, name_) as X
import Lambda.Language.Syntax (Statement, Definition) as X
import Test.QuickCheck (class Arbitrary, (===)) as X
import Test.QuickCheck.Gen (chooseInt) as X
import Test.Spec (Spec, describe, it, itOnly, pending, pending') as X
import Test.Spec.Assertions (shouldEqual) as X
import Test.Spec.QuickCheck (quickCheck) as X

-- Imports for test helpers below
import Lambda.Language.Name (Name)
import Lambda.Language.Nameless as Nameless
import Lambda.Language.Parse (parseDefinition, parseExpression, unsafeParse)
import Lambda.Language.Syntax as Syntax

-- Crashy test helpers for constructing terms

mkAst :: String -> Syntax.Expression
mkAst = unsafeParse parseExpression

mkAnon :: String -> Nameless.Expression
mkAnon = Nameless.from <<< mkAst

mkDef :: String -> Syntax.Definition
mkDef = unsafeParse parseDefinition

mkBind :: String -> Tuple Name Nameless.Expression
mkBind text = Tuple name nameless
 where
  {name, expr} = Syntax.fromDef $ mkDef text
  nameless = Nameless.from expr
