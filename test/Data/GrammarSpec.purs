module Data.GrammarSpec
  ( spec
  ) where

import Test.Prelude hiding (join)

import Data.Grammar (pluralizeWith, joinWith)

spec :: Spec Unit
spec = describe "Data.Grammar" do
  describe "pluralizeWith" do
    it "pluralizes 0 items" do
      pluralizeWith "s" 0 "item" `shouldEqual` "items"

    it "does not pluralize 1 item" do
      pluralizeWith "s" 1 "item" `shouldEqual` "item"

    it "pluralizes > 1 items" $ quickCheck \n ->
      pluralizeWith "s" (un Positive n) "item" === "items"

  describe "joinWith" do
    let join = joinWith {inject: identity, conjunction: "and"}
    it "produces an empty string given an empty foldable" do
      join [] `shouldEqual` ""

    it "returns the only element of a singleton foldable" do
      join ["x"] `shouldEqual` "x"

    it "joins 2 elements with the conjunction" $ do
      join ["x", "y"] `shouldEqual` "x and y"

    it "joins > 2 elements with commas and the conjunction" $ do
      join ["x", "y", "z"] `shouldEqual` "x, y, and z"

-- | Generate positive integers only
newtype Positive = Positive Int

derive instance newtypePositive :: Newtype Positive _
derive instance genericPosition :: Generic Positive _

instance showPosition :: Show Positive where
  show x = genericShow x

instance arbitraryPosition :: Arbitrary Positive where
  arbitrary = Positive <$> chooseInt 1 top
