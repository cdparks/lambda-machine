module Components.Expressions
  ( component
  ) where

import Lambda.Prelude

import Lambda.Language.PrettyPrint (Rep, Doc, selectRep)
import React.Basic (JSX)
import React.Basic.DOM as R

type Props =
  { history :: Array (Doc String)
  , rep :: Rep
  }

component :: Props -> JSX
component {history, rep} =
  R.ul
    { className: "unstyled scroll-overflow"
    , children: map renderExpr history
    }
 where
  renderExpr expr =
    R.li
      { className: "expression"
      , children: [R.text $ selectRep expr rep]
      }
