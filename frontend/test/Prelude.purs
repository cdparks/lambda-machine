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
import Lambda.Language.Definition (Definition) as X
import Lambda.Language.Name (Name) as X
import Lambda.Language.Statement (Statement) as X
import Test.QuickCheck (class Arbitrary, (===)) as X
import Test.QuickCheck.Gen (chooseInt) as X
import Test.Spec (Spec, describe, it, itOnly, pending, pending') as X
import Test.Spec.Assertions (shouldEqual) as X
import Test.Spec.QuickCheck (quickCheck) as X

-- Imports for test helpers below

import Lambda.Language.Definition (Definition)
import Lambda.Language.Definition as Definition
import Lambda.Language.Expression (Expression)
import Lambda.Language.Name (Name)
import Lambda.Language.Nameless (Nameless)
import Lambda.Language.Nameless as Nameless
import Lambda.Language.Parser (parse, unsafeRun)
-- Crashy test helpers for constructing terms

mkAst :: String -> Expression
mkAst = unsafeRun parse

mkAnon :: String -> Nameless
mkAnon = Nameless.from <<< mkAst

mkDef :: String -> Definition
mkDef = unsafeRun parse

mkBind :: String -> Tuple Name Nameless
mkBind text = Tuple name nameless
 where
  {name, expr} = Definition.split $ mkDef text
  nameless = Nameless.from expr
