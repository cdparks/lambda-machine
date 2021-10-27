module Lambda.Language.NamelessSpec
  ( spec
  ) where

import Test.Prelude

import Data.Set as Set
import Lambda.Language.Name as Name
import Lambda.Language.Nameless (Nameless(..))
import Lambda.Language.Nameless as Nameless

spec :: Spec Unit
spec = describe "Lambda.Language.Nameless" do
  describe "Nameless.from" do
    it "creates a locally-nameless Expression from an AST" do
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

    it "eliminates literal natural numbers" do
      let
        s = Name.from "s"
        z = Name.from "z"
        expr = Nameless.from $ mkAst "3"
        expected = Lambda s Set.empty
          $ Lambda z Set.empty
          $ Apply (Bound 1)
          $ Apply (Bound 1)
          $ Apply (Bound 1) (Bound 0)
      expr `shouldEqual` expected

    it "eliminates literal lists" do
      let
        cons = Name.from "cons"
        nil = Name.from "nil"
        expr = Nameless.from $ mkAst "[x, y]"
        expected = Lambda cons Set.empty
          $ Lambda nil Set.empty
          $ Apply (Apply (Bound 1) (Free x))
          $ Apply (Apply (Bound 1) (Free y))
          $ Bound 0
      expr `shouldEqual` expected

x :: Name
x = Name.from "x"

y :: Name
y = Name.from "y"

f :: Name
f = Name.from "f"
