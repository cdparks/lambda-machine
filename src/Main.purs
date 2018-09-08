module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import React.Render (renderTo)

import Components.App as App

main :: Effect Unit
main = renderTo "root" App.component {}
