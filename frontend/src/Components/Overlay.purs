module Components.Overlay
  ( new
  ) where

import Lambda.Prelude

import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events (stopPropagation)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component)
import React.Portal as Portal

type Props =
  { dismiss :: Maybe (Effect Unit)
  , children :: Array JSX
  }

new :: Component Props
new = do
  portal <- Portal.new
  component "Overlay" \{ dismiss, children } ->
    pure $ portal
      { children:
        [ R.div
          { className: "overlay"
          , children:
            [ R.div
              { className: "overlay-item"
              , children
              }
            ]
          , onClick: handler stopPropagation $ const $ sequence_ dismiss
          }
        ]
      }
