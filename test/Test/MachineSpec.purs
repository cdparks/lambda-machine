module Test.MachineSpec
  ( spec
  ) where

import Test.Prelude

import Data.Function (applyN)
import Machine as Machine

spec :: Spec Unit
spec = describe "Machine" do
  describe "Machine.step" do
    it "is stack-safe with programs that loop" do
      let
        machine = Machine.new [mkBind "f x = x x"] $ mkExpr "f f"
        {root, trace} = Machine.snapshot $ applyN Machine.step 100_000 machine
      root `shouldEqual` mkSyn "f f"
      trace `shouldEqual` "Substituted f in for x"
