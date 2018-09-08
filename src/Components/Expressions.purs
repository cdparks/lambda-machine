module Components.Expressions
  ( component
  ) where

import Prelude

import React.Basic as React
import React.Basic.DOM as R

import Data.PrettyPrint (Rep, Doc, selectRep)

type Props =
  { history :: Array (Doc String)
  , rep :: Rep
  }

component :: React.Component Props
component =
  React.stateless {displayName: "Expressions", render}
 where
  render {history, rep} =
    R.ul
      { className: "unstyled scroll-overflow"
      , children: map (renderExpr rep) history
      }

  renderExpr rep expr =
    R.li
      { className: "expression"
      , children: [R.text $ selectRep expr rep]
      }
