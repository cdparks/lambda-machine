module Lambda.Language.Name
  ( isSubscriptChar
  , intToSubscript
  , subscriptToInt
  , Name()
  , name
  , name_
  , next
  ) where

import Lambda.Prelude

import Data.Array (unsafeIndex)
import Data.Char (toCharCode)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Lambda.Language.PrettyPrint (class PrettyPrint)
import Partial.Unsafe (unsafePartial)

data Name = Name String (Maybe Int)

name :: String -> Maybe Int -> Name
name n ms = Name n ms

name_ :: String -> Name
name_ n = Name n Nothing

next :: Name -> Name
next (Name n ms) = Name n $ (_ + 1) <$> ms <|> pure 0

derive instance genericName :: Generic Name _

instance showName :: Show Name where
  show (Name n ms) = n <> maybe "" intToSubscript ms

instance prettyPrintName :: PrettyPrint Name where
  prettyPrint = pure <<< show

instance eqName :: Eq Name where
  eq = genericEq

instance hashableName :: Hashable Name where
  hash (Name n ms) = hash $ Tuple n ms

instance ordName :: Ord Name where
  compare = genericCompare

intToSubscript :: Int -> String
intToSubscript = fromCharArray <<< map (unsafePartial (subscriptTable `unsafeIndex` _)) <<< digits

subscriptToInt :: String -> Int
subscriptToInt =
  foldl step 0 <<< toCharArray
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
isSubscriptChar = (_ `Set.member` subscriptChars)

subscriptTable :: Array Char
subscriptTable = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉']

digits :: Int -> Array Int
digits n
  | n < 10 = [n]
  | otherwise = digits (n `div` 10) <> [n `mod` 10]
