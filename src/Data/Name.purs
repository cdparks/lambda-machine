module Data.Name
  ( isSubscriptChar
  , intToSubscript
  , subscriptToInt
  , Name()
  , name
  , name_
  , next
  ) where

import Prelude
import Data.Array.Unsafe (unsafeIndex)
import Data.Foldable (foldl)
import Data.String (fromCharArray, toCharArray)
import Data.Char (toCharCode)
import Data.Generic
import qualified Data.Set as Set

data Name = Name String Int

name :: String -> Int -> Name
name = Name

name_ :: String -> Name
name_ n = Name n 0

next :: Name -> Name
next (Name n s) = Name n (s + 1)

derive instance genericName :: Generic Name

instance showName :: Show Name where
  show (Name n s)
    | s == 0    = n
    | otherwise = n <> intToSubscript s

instance eqName :: Eq Name where
  eq = gEq

instance ordName :: Ord Name where
  compare = gCompare

intToSubscript :: Int -> String
intToSubscript = digits >>> map (subscriptTable `unsafeIndex`) >>> fromCharArray

subscriptToInt :: String -> Int
subscriptToInt = toCharArray >>> foldl step 0
 where
  step n c = 10 * n + toDigit c
  toDigit c
    | '0' <= c && c <= '9' = toCharCode c - toCharCode '0'
    | '₀' <= c && c <= '₉' = toCharCode c - toCharCode '₀'
    | otherwise            = 0

subscriptChars :: Set.Set Char
subscriptChars =
  Set.fromFoldable ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    `Set.union`
        Set.fromFoldable subscriptTable

isSubscriptChar :: Char -> Boolean
isSubscriptChar = (`Set.member` subscriptChars)

subscriptTable :: Array Char
subscriptTable = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉']

digits :: Int -> Array Int
digits n
  | n < 10    = [n]
  | otherwise = digits (n `div` 10) <> [n `mod` 10]

