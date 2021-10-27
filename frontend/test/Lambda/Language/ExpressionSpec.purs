module Lambda.Language.ExpressionSpec
  ( spec
  ) where

import Test.Prelude

import Lambda.Language.Name as Name
import Lambda.Language.Expression (Expression(..))
import Lambda.Language.Pretty (Rep(..), pretty, toString)

spec :: Spec Unit
spec = describe "Lambda.Language.Expression" do
  describe "Expression.encodeNat" do
    it "converts Church naturals using s and z to literals" do
      let one = oneWith (Name.from "s") (Name.from "z")
      toString (pretty Sugar one) `shouldEqual` "1"

    it "ignores Church naturals with different base names" do
      let one = oneWith (Name.from "f") (Name.from "z")
      toString (pretty Sugar one) `shouldEqual` "λf. λz. f z"

  describe "Expression.encodeList" do
    it "converts Church lists using cons and nil to literals" do
      let ones = listWith (Name.from "cons") (Name.from "nil") (Nat 1)
      toString (pretty Sugar ones) `shouldEqual` "[1, 1, 1]"

    it "converts Church lists using f and z to literals" do
      let ones = listWith (Name.from "f") (Name.from "z") (Nat 1)
      toString (pretty Sugar ones) `shouldEqual` "[1, 1, 1]"

    it "ignores Church lists with different base names" do
      let ones = listWith (Name.from "g") (Name.from "z") (Nat 1)
      toString (pretty Sugar ones) `shouldEqual` "λg. λz. g 1 (g 1 (g 1 z))"

oneWith :: Name -> Name -> Expression
oneWith s z = Lambda s
  $ Lambda z
  $ Apply (Var s)
  $ Var z

listWith :: Name -> Name -> Expression -> Expression
listWith cons nil x = Lambda cons
  $ Lambda nil
  $ Apply (Apply (Var cons) x)
  $ Apply (Apply (Var cons) x)
  $ Apply (Apply (Var cons) x)
  $ Var nil
