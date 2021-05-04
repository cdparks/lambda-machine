module Components.Expressions
  ( component
  ) where

import Lambda.Prelude

import Data.Array as Array
import Lambda.Language.Display (Rep, text, pretty)
import Lambda.Language.Syntax (Expression)
import React.Basic (JSX)
import React.Basic.DOM as R

type Props =
  { history :: List Expression
  , rep :: Rep
  }

component :: Props -> JSX
component {history, rep} = R.ul
  { className: "unstyled scroll-overflow"
  , children: truncate rep history
  }

truncate :: Rep -> List Expression -> Array JSX
truncate rep = Array.fromFoldable <<< loop 20
 where
  loop :: Int -> List Expression -> List JSX
  loop n = case _ of
    Nil -> Nil
    Cons e es
      | n <= 0 -> item (text" …") : Nil
      | otherwise -> item (pretty rep e) : loop (n - 1) es

  item :: JSX -> JSX
  item body = R.li
    { className: "expression"
    , children: [body]
    }
