module Components.Alert
  ( component
  ) where

import Prelude

import Effect (Effect)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events as Events

import Data.Level (Level)

type Props =
  { level :: Level
  , dismiss :: Effect Unit
  , child :: React.JSX
  }

component :: React.Component Props
component =
  React.stateless {displayName: "Alert", render}
 where
  render {level, child, dismiss} =
    R.div
      { className: "alert alert-dismissable alert-" <> show level
      , children:
        [ R.button
          { "type": "button"
          , onClick: Events.handler_ dismiss
          , className: "close"
          , children:
            [ R.span
              { className: "cursor-pointer glyphicon glyphicon-remove pull-right"
              }
            ]
          }
        , child
        ]
      }
