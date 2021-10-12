module Lambda.Language.WorldSpec
  ( spec
  ) where

import Test.Prelude

import Data.Map as Map
import Data.Set as Set
import Lambda.Language.Name as Name
import Lambda.Language.World (World(..), ConsistencyError(..), Entity(..))
import Lambda.Language.World as World

spec :: Spec Unit
spec = describe "Lambda.Language.World" do
  describe "World.define" do
    it "disallows definitions that depend on non-extant names" do
      let
        world = World.new []
        result = uncurry World.define (mkBind "x = y") world
      result `shouldEqual` Left (Undefined $ Set.singleton y)

    it "allows definitions that depend on extant names" do
      let
        world = World.new [mkBind "y = λa. a"]
        result = uncurry World.define (mkBind "x = y") world
        expected = Right $ mkWorld
          [ Tuple (Global y) [Global x]
          , Tuple (Global x) []
          ]
      result `shouldEqual` expected

    it "disallows redefining names that depend on extant names" do
      let
        world = World.new [mkBind "y x = x", mkBind "x = y y"]
        result = uncurry World.define (mkBind "y = 1") world
      result `shouldEqual` Left (CannotRedefine y $ Set.singleton $ Global x)

  describe "World.undefine" do
    it "disallows deleting definitions that have dependencies" do
      let
        world = World.new [mkBind "y a = a", mkBind "x = y"]
        result = World.undefine y world
      result `shouldEqual` Left (CannotDelete y $ Set.singleton $ Global x)

    it "allows deleting definitions that have no dependencies" do
      let
        world = World.new [mkBind "y = λa. a", mkBind "x = y"]
        result = World.undefine x world
      result `shouldEqual` Right (mkWorld [Tuple (Global y) []])

  describe "World.focus" do
    it "disallows root expressions that depend on non-extant names" do
      let
        world = World.new []
        result = World.focus (mkAnon "y") world
      result `shouldEqual` Left (Undefined $ Set.singleton y)

    it "allows root expressions that depend on extant names" do
      let
        world = World.new [mkBind "y = λa. a"]
        result = World.focus (mkAnon "y") world
        expected = Right $ mkWorld
          [ Tuple (Global y) [Root]
          , Tuple Root []
          ]
      result `shouldEqual` expected

  describe "World.unfocus" do
    it "removes root as a dependency" do
      let
        world = World.focus (mkAnon "y") $ World.new [mkBind "y = λa. a"]
        result = World.unfocus <$> world
      result `shouldEqual` Right (mkWorld [Tuple (Global y) []])

    it "has no effect if world has no focus" do
      let
        world = World.new [mkBind "y = λa. a"]
        result = World.unfocus world
      result `shouldEqual` world

x :: Name
x = Name.from "x"

y :: Name
y = Name.from "y"

mkWorld :: Array (Tuple Entity (Array Entity)) -> World
mkWorld = World <<< Map.fromFoldable <<< map (map Set.fromFoldable)
