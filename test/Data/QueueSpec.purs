module Data.QueueSpec
  ( spec
  ) where

import Test.Prelude

import Data.Queue (Queue)
import Data.Queue as Queue

spec :: Spec Unit
spec = describe "Data.Queue" do
  describe "Queue.fromFoldable" do
    it "round-trips with Queue.toUnfoldable" do
      quickCheck \(xs :: Array Int) ->
        Queue.toUnfoldable (Queue.fromFoldable xs) === xs

    it "maintains the invariant" do
      quickCheck \(xs :: Array Int) ->
        Queue.valid $ Queue.fromFoldable xs

  describe "Queue.toUnfoldable" do
    it "round-trips with Queue.fromFoldable" do
      quickCheck \q ->
        Queue.fromFoldable (Queue.toUnfoldable q :: Array Int) === q

  describe "Queue.push" do
    it "maintains the invariant" do
      quickCheck \(x :: Int) q ->
        Queue.valid q && Queue.valid (Queue.push q x)

  describe "Queue.pop" do
    it "maintains the invariant" do
      quickCheck \(q :: Queue Int) ->
        Queue.valid q && maybe true (Queue.valid <<< snd) (Queue.pop q)

  describe "Queue.extend" do
    it "maintains the invariant" do
      quickCheck \(xs :: Array Int) q ->
        Queue.valid q && Queue.valid (Queue.extend q xs)
