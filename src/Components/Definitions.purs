module Components.Definitions
  ( component
  ) where

import Prelude

import Effect (Effect)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)

import Data.Name (Name)
import Data.PrettyPrint (Rep, selectRep)
import Data.Syntax (Definition, defToDoc)

type Props =
  { defs :: Array Definition
  , onDelete :: Name -> Effect Unit
  , rep :: Rep
  }

component :: Props -> JSX
component {defs, rep, onDelete} =
  R.ul
    { className: "unstyled"
    , children: map renderDef defs
    }
 where
  renderDef def =
    R.li
      { className: "definition"
      , children:
        [ R.span
          { className: "cursor-pointer glyphicon glyphicon-remove"
          , onClick: handler_ $ onDelete def.name
          , children: []
          }
        , R.text $ " " <> selectRep (defToDoc def) rep
        ]
      }
