module Lambda.Language.SnapshotSpec
  ( spec
  ) where

import Test.Prelude

import Lambda.Language.Snapshot (Snapshot(..))
import Lambda.Language.Snapshot as Snapshot
import Lambda.Language.Snapshot.Error (Error(..))
import Lambda.Language.Snapshot.RPN (unsafeTag)
import Lambda.Language.Name as Name
import Lambda.Language.Snapshot.Tag (_VAR, _AP0, _TAK)
import Lambda.Language.Snapshot.Signature as Signature

spec :: Spec Unit
spec = describe "Lambda.Language.Snapshot" do
  describe "Snapshot.from/to" do
    it "roundtrips" do
      let
        defs = [mkDef "id x = x", mkDef "const x y = x"]
        input = pure $ mkAst "const id id"
        result = Snapshot.to =<< Snapshot.from defs input
      result `shouldEqual` Right { defs, input }

  describe "Snapshot.to" do
    it "throws on unprocessed stack values" do
      let
        state = [unsafeTag _VAR 0, unsafeTag _VAR 1]
        result = Snapshot.to $ snapshot state
      result `shouldEqual` Left (ExtraStackValues 2)

    it "throws on stack underflow with empty stack" do
      let
        state = [unsafeTag _AP0 0]
        result = Snapshot.to $ snapshot state
      result `shouldEqual` Left (StackUnderflow { op: "pop", wanted: 1, saw: 0 })

    it "throws on stack underflow with not enough values for Take" do
      let
        state = [unsafeTag _VAR 0, unsafeTag _VAR 1, unsafeTag _TAK 4]
        result = Snapshot.to $ snapshot state
      result `shouldEqual` Left (StackUnderflow { op: "take", wanted: 4, saw: 2 })

    it "throws name index out of range" do
      let
        state = [unsafeTag _VAR 0, unsafeTag _VAR 2]
        result = Snapshot.to $ snapshot state
      result `shouldEqual` Left (IndexOutOfRange 2 names)

snapshot :: Array Int -> Snapshot
snapshot state = Snapshot { sig: Signature.nil, names, state }

names :: Array Name
names = [Name.from "x", Name.from "y"]
