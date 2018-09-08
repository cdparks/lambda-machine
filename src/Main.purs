module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import React.Render (renderToId)
import Components.App as App

main :: Effect Unit
main = renderToId "root" App.component {}
