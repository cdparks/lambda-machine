module Components.Expressions
  ( component
  ) where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM as R

import Data.PrettyPrint (Rep, Doc, selectRep)

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
