module React.Render
  ( renderToId
  ) where

import Prelude

import Effect (Effect)
import React.Basic as React

foreign import renderToId
  :: forall props
   . String
  -> React.Component props
  -> props
  -> Effect Unit
