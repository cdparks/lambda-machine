module Components.Modal
  ( new
  ) where

import Lambda.Prelude

import Components.Overlay as Overlay
import Components.Level (Level)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events (stopPropagation)
import React.Basic.Events (handler_, handler)
import React.Basic.Hooks (Component, component)

type Props =
  { level :: Level
  , title :: String
  , dismiss :: Effect Unit
  , children :: Array JSX
  }

new :: Component Props
new = do
  overlay <- Overlay.new
  component "Modal" \{ level, title, dismiss, children } ->
    pure $ overlay
      { dismiss: Just dismiss
      , children:
        [ R.div
          { className: fold
            [ "panel panel-"
            , show level
            , " overlay-modal"
            ]
          , onClick: handler stopPropagation $ const $ pure unit
          , children:
            [ R.div
              { className: "panel-heading"
              , children:
                [ R.h3
                  { className: "panel-title"
                  , children:
                    [ R.span
                      { className: "cursor-pointer glyphicon glyphicon-remove pull-right"
                      , onClick: handler_ dismiss
                      }
                    , R.text title
                    ]
                  }
                ]
              }
            , R.div
              { className: "panel-body"
              , children
              }
            ]
          }
        ]
      }
