module Main
  ( main
  ) where

import Lambda.Prelude

import Components.App (mkApp)
import React.Render (renderTo)

main :: Effect Unit
main = do
  app <- mkApp
  renderTo "root" $ app { code: Nothing }
