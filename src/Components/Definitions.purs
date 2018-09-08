module Components.Definitions
  ( component
  ) where

import Prelude

import Effect (Effect)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Events as Events

import Data.Name (Name)
import Data.PrettyPrint (Rep, selectRep)
import Data.Syntax (Definition, defToDoc)

type Props =
  { defs :: Array Definition
  , onDelete :: Name -> Effect Unit
  , rep :: Rep
  }

component :: React.Component Props
component =
  React.stateless {displayName: "Definitions", render}
 where
  render {defs, rep, onDelete} =
    R.ul
      { className: "unstyled"
      , children: map (renderDef rep onDelete) defs
      }

  renderDef rep onDelete def =
    R.li
      { className: "definition"
      , children:
        [ R.span
          { className: "cursor-pointer glyphicon glyphicon-remove"
          , onClick: Events.handler_ $ onDelete def.name
          , children: []
          }
        , R.text $ " " <> selectRep (defToDoc def) rep
        ]
      }
