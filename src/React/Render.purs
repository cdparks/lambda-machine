module React.Render
  ( renderTo
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic (Component, element)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML (window)
import Web.HTML.Window (document)

renderTo
  :: forall props
   . String
  -> Component { | props }
  -> { | props }
  -> Effect Unit
renderTo id component props = do
  mContainer <- getElementById id =<< (map toNonElementParentNode $ document =<< window)
  case mContainer of
    Nothing ->
      throw "Container element not found"
    Just container ->
      render (element component props) container
