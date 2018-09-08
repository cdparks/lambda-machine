module Components.Alert
  ( component
  ) where

import Prelude

import Effect (Effect)

import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events as Events

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Data.Level (Level)

type Props =
  { error :: Maybe (Tuple Level String)
  , dismiss :: Effect Unit
  }

component :: React.Component Props
component =
  React.stateless {displayName: "Header", render}
 where
  render {error: Nothing} = React.empty
  render {error: Just (Tuple level message), dismiss} =
    R.pre
      { className: "alert alert-" <> show level
      , children:
        [ R.span
          { className: "cursor-pointer glyphicon glyphicon-remove pull-right"
          , onClick: Events.handler_ dismiss
          }
        , R.text message
        ]
      }
