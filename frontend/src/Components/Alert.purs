module Components.Alert
  ( component
  ) where

import Lambda.Prelude

import Components.Level (Level)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)

type Props =
  { level :: Level
  , dismiss :: Effect Unit
  , child :: JSX
  }

component :: Props -> JSX
component { level, dismiss, child } = R.div
  { className: "alert alert-dismissable alert-" <> show level
  , children:
    [ R.button
      { "type": "button"
      , onClick: handler_ dismiss
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
