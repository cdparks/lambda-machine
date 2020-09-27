module Components.Alert
  ( component
  ) where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)

import Data.Level (Level)

type Props =
  { level :: Level
  , dismiss :: Effect Unit
  , child :: JSX
  }

component :: Props -> JSX
component {level, child, dismiss} =
  R.div
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
