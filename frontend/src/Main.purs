module Main
  ( main
  ) where

import Lambda.Prelude

import Components.App as App
import Effect.DOM as DOM
import Effect.QueryParams as QueryParams
import Lambda.Flags as Flags
import React.Basic.DOM (render)

main :: Effect Unit
main = do
  app <- App.new
  code <- QueryParams.get "code"
  flags <- fromMaybe Flags.none <$> QueryParams.getWith Flags.parse "f"
  let node = app { code, flags }
  render node =<< DOM.getRoot
