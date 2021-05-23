module Lambda.MachineSpec
  ( spec
  ) where

import Test.Prelude

import Data.Function (applyN)
import Lambda.Language.Nameless as Nameless
import Lambda.Language.Syntax as Syntax
import Lambda.Machine as Machine

spec :: Spec Unit
spec = describe "Lambda.Machine" do
  describe "Lambda.Machine.step" do
    it "is stack-safe with programs that loop" do
      let result = stepN 100_000 [mkBind "f x = x x"] $ mkAnon "f f"
      Syntax.unHighlight result `shouldEqual` mkAst "f f"

    it "evaluates addition of church numerals" do
      let result = normalize [mkBind "add m n s z = m s (n s z)"] $ mkAnon "add 2 3"
      result `shouldEqual` mkAst "5"

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
        result = normalize globals $ mkAnon "any (repeat true)"
      result `shouldEqual` mkAst "λt. λ_. t"

-- | Create a new `Machine` and step it N times.
stepN
  :: Int
  -> Array (Tuple Name Nameless.Expression)
  -> Nameless.Expression
  -> Syntax.Expression
stepN n globals =
  Machine.snapshot
  <<< applyN Machine.step n
  <<< Machine.new globals

-- | Create a new `Machine` and step it until it halts.
normalize
  :: Array (Tuple Name Nameless.Expression)
  -> Nameless.Expression
  -> Syntax.Expression
normalize globals =
  loop <<< Machine.new globals
 where
  loop m
   | m.halted = Machine.snapshot m
   | otherwise = loop $ Machine.step m
