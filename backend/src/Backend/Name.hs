module Backend.Name
  ( Name
  ) where

import Backend.Prelude hiding (takeWhile)

import Data.Attoparsec.Text (Parser, endOfInput, parseOnly, satisfy, takeWhile)
import Data.Text (singleton)
import qualified RIO.Char as Char

newtype Name = Name Text
  deriving newtype (Eq, Show, Ord, ToJSON, PersistField, PersistFieldSql)

instance FromJSON Name where
  parseJSON =
    withText "Name" $ either fail pure . parseOnly (parseName <* endOfInput)

parseName :: Parser Name
parseName =
  toName
    <$> satisfy isStart
    <*> takeWhile isBody
    <*> optional (satisfy (== '?'))
    <*> takeWhile isDigit
 where
  toName start body question digits =
    Name $ mconcat [singleton start, body, maybe "" singleton question, digits]

isStart :: Char -> Bool
isStart c = Char.isAsciiLower c || c == '_'

isBody :: Char -> Bool
isBody c = Char.isAsciiLower c || c == '-'

isDigit :: Char -> Bool
isDigit c = Char.isDigit c || c `elem` subscripts

subscripts :: String
subscripts = "₀₁₂₃₄₅₆₇₈₉"
