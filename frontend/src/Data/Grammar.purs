module Data.Grammar
  ( pluralizeWith
  , joinWith
  ) where

import Lambda.Prelude hiding (join)

import Data.Array as Array
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)

-- | Append suffix if `Int` argument is zero or greater than one.
pluralizeWith :: String -> Int -> String -> String
pluralizeWith suffix n text
  | n == 1 = text
  | otherwise = text <> suffix

-- | Join `Foldable` of `String`s with commas and a conjunction,
-- | if necessary.
joinWith
  :: forall m f
   . Monoid m
  => Foldable f
  => { inject :: String -> m, conjunction :: m }
  -> f m
  -> m
joinWith options = joinInternal options <<< Array.fromFoldable

-- | Implementation of `joinWith` that operates on `Array`.
joinInternal
  :: forall m
   . Monoid m
  => { inject :: String -> m, conjunction :: m }
  -> Array m
  -> m
joinInternal {inject, conjunction} xs = case Array.length xs of
  0 -> mempty
  1 -> Array.intercalate mempty xs
  2 -> Array.intercalate space $ conjOnLast 1
  n -> Array.intercalate comma $ conjOnLast $ n - 1
 where
  space = inject " "
  comma = inject ", "
  conjOnLast i = unsafePartial $ fromJust $ Array.modifyAt i prepend xs
  prepend x = conjunction <> space <> x
