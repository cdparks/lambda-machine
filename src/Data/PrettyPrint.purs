module Data.PrettyPrint
  ( PrettyPrint
  , prettyPrint
  , parensIf
  ) where

import Prelude
import Data.Monoid

class PrettyPrint a where
  prettyPrint :: a -> String

parensIf :: Boolean -> String -> String
parensIf cond s = if cond then "(" <> s <> ")" else s

