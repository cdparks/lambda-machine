module Lambda.Language.Name
  ( Name
  , from
  , withSubscript
  , next
  ) where

import Lambda.Prelude

import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Lambda.Language.Parser (class Parse, Parser, parse, liftF, satisfy, string, token)
import Partial.Unsafe (unsafePartial)

-- | Source-level name with an optional subscript.
data Name = Name String (Maybe Int)

derive instance eqName :: Eq Name
derive instance ordName :: Ord Name

instance showName :: Show Name where
  show (Name n ms) = n <> maybe "" intToSubscript ms

instance hashableName :: Hashable Name where
  hash (Name n ms) = hash $ Tuple n ms

instance readForeignName :: ReadForeign Name where
  readImpl = liftF parse <=< readImpl

instance writeForeignName :: WriteForeign Name where
  writeImpl = writeImpl <<< show

-- | Construct a `Name` with no subscript.
from :: String -> Name
from s = Name s Nothing

-- | Construct a `Name` with a subscript.
withSubscript :: Int -> String -> Name
withSubscript n s = Name s $ Just n

-- | Append or increment a `Name`'s subscript.
next :: Name -> Name
next (Name n ms) = Name n $ (_ + 1) <$> ms <|> pure 0

-- | Parse a `Name`
-- |
-- | ```ebnf
-- | name
-- |   = (letter | "_")                           (* Initial letter or underscore *)
-- |   , {letter | "-"}                           (* Zero or more letters or hyphens *)
-- |   , ["?"]                                    (* Optional question mark *)
-- |   , {subscript | digit}                      (* Zero or more subscripts or digits *)
-- |   ;
-- |
-- | letter                                       (* Lowercase latin letters *)
-- |   = "a" | "b" | "c" | "d" | "e" | "f" | "g"
-- |   | "h" | "i" | "j" | "k" | "l" | "m" | "n"
-- |   | "o" | "p" | "q" | "r" | "s" | "t" | "u"
-- |   | "v" | "w" | "x" | "y" | "z" ;
-- |
-- | subscript                                    (* Subscripts *)
-- |   = "₀" | "₁" | "₂" | "₃" | "₄" | "₅" | "₆"
-- |   | "₇" | "₈" | "₉" ;
-- |
-- | digit                                        (* Decimal digits *)
-- |   = "0" | "1" | "2" | "3" | "4" | "5" | "6"
-- |   | "7" | "8" | "9" ;
-- | ```
-- |
instance parseName :: Parse Name where
  parse = token do
    first <- satisfy firstChar
    body <- Array.many $ satisfy bodyChar
    question <- string "?" <|> pure ""
    subscript <- Just <$> parseSubscript <|> pure Nothing
    let var = fromCharArray ([first] <> body) <> question
    pure $ Name var subscript

-- | Parse subscripts for a `Name`
parseSubscript :: Parser Int
parseSubscript = subscriptToInt <<< fromCharArray <$> Array.some (satisfy isSubscriptChar)

-- | Parse the first character of a `Name`
firstChar :: Char -> Boolean
firstChar c = isLower c || c == '_'

-- | Parse the remaining (non-question-mark, non-subscript-y)
-- | characters of a `Name`.
bodyChar :: Char -> Boolean
bodyChar c = isLower c || c == '-'

-- | Is a character a lowercase letter?
isLower :: Char -> Boolean
isLower c = 'a' <= c && c <= 'z'

-- | Convert `Int` subscript to textual subscript.
intToSubscript :: Int -> String
intToSubscript = fromCharArray <<< map (unsafePartial (subscriptTable `unsafeIndex` _)) <<< digits

-- | Convert textual subscript to `Int` subscript.
subscriptToInt :: String -> Int
subscriptToInt =
  foldl step 0 <<< toCharArray
 where
  step n c = 10 * n + toDigit c
  toDigit c
    | '0' <= c && c <= '9' = toCharCode c - toCharCode '0'
    | '₀' <= c && c <= '₉' = toCharCode c - toCharCode '₀'
    | otherwise            = 0

-- | Subscripts are digits or subscript digits
subscriptChars :: Set.Set Char
subscriptChars =
  Set.fromFoldable ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']
    `Set.union`
        Set.fromFoldable subscriptTable

-- | Can input be parsed as a subscript?
isSubscriptChar :: Char -> Boolean
isSubscriptChar = (_ `Set.member` subscriptChars)

-- | Fixed array of actual subscript characters.
subscriptTable :: Array Char
subscriptTable = ['₀', '₁', '₂', '₃', '₄', '₅', '₆', '₇', '₈', '₉']

-- | Split decimal integer into its constituent digits.
digits :: Int -> Array Int
digits n
  | n < 10 = [n]
  | otherwise = digits (n `div` 10) <> [n `mod` 10]
