module Components.Definitions
  ( component
  ) where

import Lambda.Prelude

import Lambda.Language.Name (Name)
import Lambda.Language.Pretty (pretty, Rep, toJSX)
import Lambda.Language.Syntax (Definition(..))
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (handler_)

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
  renderDef def@(Definition { name }) =
    R.li
      { className: "definition"
      , children:
        [ R.span
          { className: "cursor-pointer glyphicon glyphicon-remove"
          , onClick: handler_ $ onDelete name
          , children: []
          }
        , R.text " "
        , toJSX $ pretty rep def
        ]
      }
