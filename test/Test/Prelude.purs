module Test.Prelude
  ( mkSyn
  , mkExpr
  , mkDef
  , mkBind
  , module Exports
  , module Prelude
  ) where

import Prelude

import Data.Either (Either(..)) as Exports
import Data.Maybe (Maybe(..)) as Exports
import Data.Tuple (Tuple(..))
import Data.Tuple (Tuple(..), uncurry, fst, snd) as Exports
import Language.Expr (Expr(..), syntaxToExpr) as Exports
import Language.Expr (Expr, syntaxToExpr)
import Language.Name (Name)
import Language.Name (Name, name_) as Exports
import Language.Parse (parseDefinition, parseSyntax, unsafeParse)
import Language.Syntax (Definition, Syntax(..), defToSyntax) as Exports
import Language.Syntax (Definition, Syntax, defToSyntax)
import Test.QuickCheck (class Arbitrary) as Exports
import Test.QuickCheck.Gen (chooseInt) as Exports
import Test.Spec (Spec, describe, it, pending, pending') as Exports
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
