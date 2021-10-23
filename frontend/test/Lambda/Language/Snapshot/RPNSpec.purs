module Lambda.Language.Snapshot.RPNSpec
  ( spec
  ) where

import Test.Prelude

import Lambda.Language.Snapshot.RPN (RPN(..), encode, decode)
import Lambda.Language.Snapshot.Error (Error(..))
import Lambda.Language.Snapshot.Tag (Tag(..), _VAR)

spec :: Spec Unit
spec = describe "Lambda.Language.Snapshot.RPN" do
  describe "RPN.encode/decode" do
    it "roundtrips" do
      let
        ops = [Var 0, Nat 0, Take 0, Lambda 0, Define 0, AppVar 0, Apply]
        result = decode =<< encode ops
      result `shouldEqual` Right ops

  describe "RPN.encode" do
    it "throws on payloads larger than 0x1FFFFFFF" do
      let
        ops = [Var 0x20000000]
        result = runIdentity $ runExceptT $ encode ops
      result `shouldEqual` Left (PayloadOutOfRange _VAR 0x20000000)

    it "throws on payloads smaller than 0" do
      let
        ops = [Var (-1)]
        result = runIdentity $ runExceptT $ encode ops
      result `shouldEqual` Left (PayloadOutOfRange _VAR (-1))

  describe "RPN.decode" do
    -- We're using all 3-bit tags except 0b000
    it "throws on unrecognized tags" $ do
      let result = runIdentity $ runExceptT $ decode [0]
      result `shouldEqual` Left (UnrecognizedTag $ Tag 0)
