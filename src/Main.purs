module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import React.Render (renderTo)

import Components.App (mkApp)

main :: Effect Unit
main = do
  app <- mkApp
  renderTo "root" $ app {}
