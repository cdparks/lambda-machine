module Effect.DOM
  ( getBody
  , getRoot
  , getPortal
  ) where

import Lambda.Prelude

import Web.DOM.Element (Element)

-- | Get document.body or throw
foreign import getBody :: Effect Element

-- | Get element with id "app" or throw
foreign import getRoot :: Effect Element

-- | Get element with id "portal" or throw
foreign import getPortal :: Effect Element
