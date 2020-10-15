module Test.Prelude
  ( mkSyn
  , mkExpr
  , mkDef
  , mkBind
  , module Exports
  , module Prelude
  ) where

import Prelude

import Data.Either (Either(..), either) as Exports
import Data.Maybe (Maybe(..), fromMaybe, maybe) as Exports
import Data.Tuple (Tuple(..))
import Data.Tuple (Tuple(..), uncurry, fst, snd) as Exports
import Lambda.Language.Expr (Expr(..), syntaxToExpr) as Exports
import Lambda.Language.Expr (Expr, syntaxToExpr)
import Lambda.Language.Name (Name)
import Lambda.Language.Name (Name, name, name_) as Exports
import Lambda.Language.Parse (parseDefinition, parseSyntax, unsafeParse)
import Lambda.Language.Syntax (Definition, Syntax(..), defToSyntax) as Exports
import Lambda.Language.Syntax (Definition, Syntax, defToSyntax)
import Test.QuickCheck (class Arbitrary, (===)) as Exports
import Test.QuickCheck.Gen (chooseInt) as Exports
import Test.Spec (Spec, describe, it, itOnly, pending, pending') as Exports
import Test.Spec.Assertions (shouldEqual) as Exports
import Test.Spec.QuickCheck (quickCheck) as Exports

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
