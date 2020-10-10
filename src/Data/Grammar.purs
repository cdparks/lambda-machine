module Data.Grammar
  ( pluralizeWith
  , joinWith
  ) where

import Prelude hiding (join)

import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

pluralizeWith :: String -> Int -> String -> String
pluralizeWith suffix n text
  | n == 1 = text
  | otherwise = text <> suffix

joinWith :: forall f. Foldable f => String -> f String -> String
joinWith conj = join conj <<< Array.fromFoldable

join :: String -> Array String -> String
join conj xs = case Array.length xs of
  0 -> ""
  1 -> Array.intercalate "" xs
  2 -> Array.intercalate " " $ withConj conj 1 xs
  n -> Array.intercalate ", " $ withConj conj (n - 1) xs

withConj :: String -> Int -> Array String -> Array String
withConj conj i xs = unsafePartial $ fromJust $ Array.modifyAt i addConj xs
 where
  addConj x = conj <> " " <> x
