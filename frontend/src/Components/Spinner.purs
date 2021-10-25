module Components.Spinner
  ( new
  ) where

import Lambda.Prelude

import React.Basic.DOM as R
import React.Basic.Hooks (Component, component)
import Components.Overlay as Overlay

new :: Component {}
new = do
  overlay <- Overlay.new
  component "Spinner" \_ ->
    pure $ overlay
      { dismiss: Nothing
      , children:
        [ R.div
          { className: "spinner"
          , children:
            [ R.span
              { className: "glyphicon glyphicon-refresh spin"
              , children: []
              }
            ]
          }
        ]
      }
