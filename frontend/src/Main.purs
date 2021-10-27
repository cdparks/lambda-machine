module Main
  ( main
  ) where

import Lambda.Prelude

import Components.App as App
import Effect.DOM as DOM
import Effect.QueryParams as QueryParams
import React.Basic.DOM (render)

main :: Effect Unit
main = do
  app <- App.new
  code <- QueryParams.get "code"
  let node = app { code }
  render node =<< DOM.getRoot
