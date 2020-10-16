module Test.Prelude
  ( mkSyn
  , mkExpr
  , mkDef
  , mkBind
  , module Lambda.Prelude
  , module X
  ) where

import Lambda.Prelude

-- Re-exports
import Lambda.Language.Expr (Expr(..), syntaxToExpr) as X
import Lambda.Language.Name (Name, name, name_) as X
import Lambda.Language.Syntax (Definition, Syntax(..), defToSyntax) as X
import Test.QuickCheck (class Arbitrary, (===)) as X
import Test.QuickCheck.Gen (chooseInt) as X
import Test.Spec (Spec, describe, it, itOnly, pending, pending') as X
import Test.Spec.Assertions (shouldEqual) as X
import Test.Spec.QuickCheck (quickCheck) as X

import Lambda.Language.Expr (Expr, syntaxToExpr)
import Lambda.Language.Name (Name)
import Lambda.Language.Parse (parseDefinition, parseSyntax, unsafeParse)
import Lambda.Language.Syntax (Definition, Syntax, defToSyntax)

-- Crashy test helpers for constructing terms

mkSyn :: String -> Syntax
mkSyn = unsafeParse parseSyntax

mkExpr :: String -> Expr
mkExpr = syntaxToExpr <<< mkSyn

mkDef :: String -> Definition
mkDef = unsafeParse parseDefinition

mkBind :: String -> Tuple Name Expr
mkBind text = Tuple def.name expr
 where
  def = mkDef text
  expr = syntaxToExpr $ defToSyntax def
