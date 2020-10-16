module Lambda.Language.WorldSpec
  ( spec
  ) where

import Test.Prelude

import Data.Map as Map
import Data.Set as Set
import Lambda.Language.World (ConsistencyError(..), Dependency(..))
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
      result `shouldEqual` Right
        { nameToDeps: Map.fromFoldable
          [ Tuple y $ Set.fromFoldable [Global x]
          , Tuple x Set.empty
          ]
        , depToNames: Map.fromFoldable
          [ Tuple (Global x) $ Set.fromFoldable [y]
          , Tuple (Global y) Set.empty
          ]
        }

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
      result `shouldEqual` Right
        { nameToDeps: Map.fromFoldable
          [ Tuple y $ Set.empty
          ]
        , depToNames: Map.fromFoldable
          [ Tuple (Global y) Set.empty
          ]
        }

  describe "World.focus" do
    it "disallows root expressions that depend on non-extant names" do
      let
        world = World.new []
        result = World.focus (mkExpr "y") world
      result `shouldEqual` Left (Undefined $ Set.singleton y)

    it "allows root expressions that depend on extant names" do
      let
        world = World.new [mkBind "y = λa. a"]
        result = World.focus (mkExpr "y") world
      result `shouldEqual` Right
        { nameToDeps: Map.fromFoldable
          [ Tuple y $ Set.fromFoldable [Root]
          ]
        , depToNames: Map.fromFoldable
          [ Tuple Root $ Set.fromFoldable [y]
          , Tuple (Global y) Set.empty
          ]
        }

  describe "World.unfocus" do
    it "removes root as a dependency" do
      let
        world = World.focus (mkExpr "y") $ World.new [mkBind "y = λa. a"]
        result = World.unfocus <$> world
      result `shouldEqual` Right
        { nameToDeps: Map.fromFoldable
          [ Tuple y $ Set.empty
          ]
        , depToNames: Map.fromFoldable
          [ Tuple (Global y) Set.empty
          ]
        }

    it "has no effect if world has no focus" do
      let
        world =  World.new [mkBind "y = λa. a"]
        result = World.unfocus world
      result `shouldEqual` world

x :: Name
x = name_ "x"

y :: Name
y = name_ "y"
