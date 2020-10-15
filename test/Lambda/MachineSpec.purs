module Lambda.MachineSpec
  ( spec
  ) where

import Test.Prelude

import Data.Function (applyN)
import Lambda.Machine as Machine

spec :: Spec Unit
spec = describe "Lambda.Machine" do
  describe "Lambda.Machine.step" do
    it "is stack-safe with programs that loop" do
      let result = stepN 100_000 [mkBind "f x = x x"] $ mkExpr "f f"
      result `shouldEqual` mkSyn "f f"

    it "evaluates addition of church numerals" do
      let result = normalize [mkBind "add m n s z = m s (n s z)"] $ mkExpr "add 2 3"
      result `shouldEqual` mkSyn "5"

    it "lazily consumes an infinite list" do
      let
        globals =
          [ mkBind "fix f = (λx. f (x x)) (λy. f (y y))"
          , mkBind "true t _ = t"
          , mkBind "false _ f = f"
          , mkBind "or x y = x true y"
          , mkBind "any l = l or false"
          , mkBind "cons x xs c n = c x (xs c n)"
          , mkBind "repeat x = fix (λnext. cons x next)"
          ]
        result = normalize globals $ mkExpr "any (repeat true)"
      result `shouldEqual` mkSyn "λt. λ_. t"

stepN :: Int -> Array (Tuple Name Expr) -> Expr -> Syntax
stepN n globals =
  _.root
  <<< Machine.snapshot
  <<< applyN Machine.step n
  <<< Machine.new globals

normalize :: Array (Tuple Name Expr) -> Expr -> Syntax
normalize globals =
  loop <<< Machine.new globals
 where
  loop m
   | Machine.halted m = _.root $ Machine.snapshot m
   | otherwise = loop $ Machine.step m
