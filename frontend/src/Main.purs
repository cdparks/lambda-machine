module Main
  ( main
  ) where

import Lambda.Prelude

import Components.App as App
import Effect.DOM (getRoot)
import React.Basic.DOM (render)

main :: Effect Unit
main = do
  app <- App.new
  let node = app { code: Nothing }
  render node =<< getRoot
