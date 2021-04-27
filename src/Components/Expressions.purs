module Components.Expressions
  ( component
  ) where

import Lambda.Prelude

import Data.Array as Array
import Lambda.Language.PrettyPrint (Rep, Doc, withRep)
import React.Basic (JSX)
import React.Basic.DOM as R

type Props =
  { history :: List (Doc String)
  , rep :: Rep
  }

component :: Props -> JSX
component {history, rep} = R.ul
  { className: "unstyled scroll-overflow"
  , children: truncate rep history
  }

truncate :: Rep -> List (Doc String) -> Array JSX
truncate rep = Array.fromFoldable <<< loop 20
 where
  loop :: Int -> List (Doc String) -> List JSX
  loop n = case _ of
    Nil -> Nil
    Cons t ts
      | n <= 0 -> item "…" : Nil
      | otherwise -> item (withRep rep t) : loop (n - 1) ts

  item :: String -> JSX
  item t = R.li
    { className: "expression"
    , children: [R.text t]
    }
