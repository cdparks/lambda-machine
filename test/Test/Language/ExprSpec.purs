module Test.Language.ExprSpec
  ( spec
  ) where

import Test.Prelude

import Data.Set as Set

spec :: Spec Unit
spec = describe "Expr" do
  describe "syntaxToExpr" do
    it "converts an AST to locally-nameless Expr" do
      let
        ast = Lambda x
          $ Lambda y
          $ Apply (Apply (Var f) (Var y))
          $ Var x
        expr = syntaxToExpr ast
        expected = Bind x (Set.singleton f)
          $ Bind y (Set.singleton f)
          $ App (App (Free f) (Bound 0))
          $ Bound 1
      expr `shouldEqual` expected

    it "renames names that would otherwise shadow" do
      let
        ast = Lambda x $ Lambda x $ Var x
        expr = syntaxToExpr ast
        expected = Bind x Set.empty
          $ Bind (name "x" $ pure 0) Set.empty
          $ Bound 0
      expr `shouldEqual` expr


x :: Name
x = name_ "x"

y :: Name
y = name_ "y"

f :: Name
f = name_ "f"
