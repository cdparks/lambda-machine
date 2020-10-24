module Lambda.Language.ExprSpec
  ( spec
  ) where

import Test.Prelude

import Data.Set as Set
import Lambda.Language.Expr (Expr(..))

spec :: Spec Unit
spec = describe "Lambda.Language.Expr" do
  describe "syntaxToExpr" do
    it "converts an AST to locally-nameless Expr" do
      let
        ast = mkSyn "λx y. f y x"
        expr = syntaxToExpr ast
        expected = Lambda x (Set.singleton f)
          $ Lambda y (Set.singleton f)
          $ Apply (Apply (Free f) (Bound 0))
          $ Bound 1
      expr `shouldEqual` expected

    it "renames names that would otherwise shadow" do
      let
        ast = mkSyn "λx x. x"
        expr = syntaxToExpr ast
        expected = Lambda x Set.empty
          $ Lambda (name "x" $ pure 0) Set.empty
          $ Bound 0
      expr `shouldEqual` expected

x :: Name
x = name_ "x"

y :: Name
y = name_ "y"

f :: Name
f = name_ "f"
