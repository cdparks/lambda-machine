module Components.Controls
  ( component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events as Events

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
  render {expr: Nothing} = React.empty
  render {expr: Just expr, onStep, onClear, onSave, onSugar, rep} =
    R.div
      { className: "add-margin-medium btn-group pull-right"
      , children:
        [ R.button
          { className: "btn btn-default"
          , onClick: Events.handler_ $ onStep expr
          , children: [R.text "Step"]
          }
        , R.button
          { className: "btn btn-default"
          , onClick: Events.handler_ onClear
          , children: [R.text "Clear"]
          }
        , R.button
          { className: "btn btn-default"
          , onClick: Events.handler_ onSave
          , children: [R.text "Save"]
          }
        , R.button
          { className: "btn " <> ifSugar "btn-danger" "btn-success" rep
          , onClick: Events.handler_ onSugar
          , children: [R.text $ ifSugar "Raw" "Sugar" rep]
          }
        ]
      }
