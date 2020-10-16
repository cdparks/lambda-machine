module React.Render
  ( renderTo
  ) where

import Lambda.Prelude

import Effect.Exception (throw)
import React.Basic (JSX)
import React.Basic.DOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

-- | Render a `JSX` element into the element with the specified id or
-- | crash.
renderTo :: String -> JSX -> Effect Unit
renderTo id jsx = do
  mContainer <- getElementById id =<< (map toNonElementParentNode $ document =<< window)
  case mContainer of
    Nothing -> throw "Container element not found"
    Just container -> render jsx container
