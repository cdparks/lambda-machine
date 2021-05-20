module Components.Expressions
  ( component
  ) where

import Lambda.Prelude

import Data.Array as Array
import Lambda.Language.History (History)
import Lambda.Language.History as History
import Lambda.Language.Pretty (Rep)
import React.Basic (JSX)
import React.Basic.DOM as R

type Props =
  { history :: History
  , rep :: Rep
  }

component :: Props -> JSX
component {history, rep} = R.ul
  { className: "unstyled scroll-overflow"
  , children: truncate $ History.toJSX rep history
  }

truncate :: List JSX -> Array JSX
truncate = Array.fromFoldable <<< loop 20
 where
  loop :: Int -> List JSX -> List JSX
  loop n = case _ of
    Nil -> Nil
    Cons e es
      | n <= 0 -> item (R.text "…") : Nil
      | otherwise -> item e : loop (n - 1) es

  item :: JSX -> JSX
  item body = R.li
    { className: "expression"
    , children: [body]
    }
