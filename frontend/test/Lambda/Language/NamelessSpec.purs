module Lambda.Language.NamelessSpec
  ( spec
  ) where

import Test.Prelude

import Data.Set as Set
import Lambda.Language.Name as Name
import Lambda.Language.Nameless (Expression(..))
import Lambda.Language.Nameless as Nameless

spec :: Spec Unit
spec = describe "Lambda.Language.Nameless" do
  describe "Nameless.to" do
    it "converts an AST to locally-nameless Expr" do
      let
        expr = Nameless.from $ mkAst "λx y. f y x"
        expected = Lambda x (Set.singleton f)
          $ Lambda y (Set.singleton f)
          $ Apply (Apply (Free f) (Bound 0))
          $ Bound 1
      expr `shouldEqual` expected

    it "renames names that would otherwise shadow" do
      let
        expr = Nameless.from $ mkAst "λx x. x"
        expected = Lambda x Set.empty
          $ Lambda (Name.withSubscript 0 "x") Set.empty
          $ Bound 0
      expr `shouldEqual` expected

x :: Name
x = Name.from "x"

y :: Name
y = Name.from "y"

f :: Name
f = Name.from "f"
