module Lambda.Language.Snapshot.Code
  ( Code(..)
  ) where

import Lambda.Prelude

import Data.String.Pattern (Pattern(..))
import Data.String.CodeUnits (contains, singleton, fromCharArray)
import Lambda.Language.Parser (liftJson, satisfy, class Parse, parse)

-- | Identifies a snapshot
newtype Code = Code String

derive instance newtypeCode :: Newtype Code _
derive newtype instance eqCode :: Eq Code
derive newtype instance showCode :: Show Code
derive newtype instance encodeJsonCode :: EncodeJson Code

instance decodeJsonCode :: DecodeJson Code where
  decodeJson = liftJson parse <=< decodeJson

instance parseCode :: Parse Code where
  parse = (Code <<< fromCharArray) <$> replicateA 8 (satisfy isCodeChar)

isCodeChar :: Char -> Boolean
isCodeChar c = isDigit || (isUpper && not ambiguous)
 where
  isDigit = '0' <= c && c <= '9'
  isUpper = 'A' <= c && c <= 'Z'
  ambiguous = contains pat "ILOU"
  pat = Pattern $ singleton c
