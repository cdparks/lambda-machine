module Components.Controls
  ( component
  ) where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events (EventHandler, handler_)

import Data.Expr (Expr)
import Data.PrettyPrint (Rep, ifSugar)

type Props =
  { expr :: Maybe Expr
  , onStep :: Expr -> Effect Unit
  , onClear :: Effect Unit
  , onSave :: Effect Unit
  , onSugar :: Effect Unit
  , rep :: Rep
  }

component :: React.Component Props
component =
  React.stateless {displayName: "Controls", render}
 where
  render {expr, onStep, onClear, onSave, onSugar, rep} =
    R.div
      { className: "add-margin-medium btn-group pull-right"
      , children:
        [ button expr
          { className: "btn btn-default"
          , onClick: onStep
          , label: "Step"
          }
        , button expr
          { className: "btn btn-default"
          , onClick: const onClear
          , label: "Clear"
          }
        , button expr
          { className: "btn btn-default"
          , onClick: const onSave
          , label: "Save"
          }
        , button expr
          { className: "btn " <> ifSugar "btn-danger" "btn-success" rep
          , onClick: const onSugar
          , label: ifSugar "Raw" "Sugar" rep
          }
        ]
      }

button
  :: forall a
   . Maybe a
  ->
    { className :: String
    , onClick :: a -> Effect Unit
    , label :: String
    }
  -> React.JSX
button m {className, onClick, label} =
  R.button
    { className: maybeEnable className m
    , onClick: maybeHandle onClick m
    , children: [R.text label]
    }

maybeHandle :: forall a. (a -> Effect Unit) -> Maybe a -> EventHandler
maybeHandle handle =
  handler_ <<< maybe (pure unit) handle

maybeEnable :: forall a. String -> Maybe a -> String
maybeEnable className =
  maybe
    (className <> " disabled")
    (const className)
