module Main
  ( main
  ) where

import Lambda.Prelude

import Components.App as App
import React.Basic.DOM (render)
import Effect.DOM (getRoot)

main :: Effect Unit
main = do
  app <- App.new
  let node = app { code: Nothing }
  render node =<< getRoot
