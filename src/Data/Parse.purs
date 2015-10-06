module Data.Parse where

import Prelude

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Pos

import Control.Lazy (fix)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))

import Data.Array (many)
import Data.Foldable (foldl)
import Data.String (fromCharArray)
import Data.Either

import Data.Syntax

token :: forall a. Parser String a -> Parser String a
token p = p <* skipSpaces

parseAll :: forall a. Parser String a -> String -> Either ParseError a
parseAll p s = runParser s (skipSpaces *> p <* eof)

formatParseError :: ParseError -> String
formatParseError (ParseError { message: message, position: Position { column: column } }) =
  "Parse error: " <> message <> " at column " <> show column

parseEither :: Parser String (Either Definition Syntax)
parseEither = try (Left <$> parseDefinition) <|> (Right <$> parseSyntax)

parseDefinition :: Parser String Definition
parseDefinition = {name:_, syntax:_}
  <$> parseName
  <*> (token (string "=") *> parseSyntax)

parseSyntax :: Parser String Syntax
parseSyntax = fix parseApply
 where
  parseApply p = do
    first <- parseAtom
    rest <- many parseAtom
    case rest of
      [] -> return first
      _  -> return (foldl Apply first rest)
   where
    parseAtom :: Parser String Syntax
    parseAtom = parseLambda <|> parseVar <|> parens p

    parseLambda :: Parser String Syntax
    parseLambda = Lambda
      <$> (token (string "\\" <|> string "λ") *> parseName)
      <*> (token (string ".") *> p)

parens :: forall a. Parser String a -> Parser String a
parens = between (token (string "(")) (token (string ")"))

parseVar :: Parser String Syntax
parseVar = Var <$> parseName

parseName :: Parser String String
parseName = token do
  first <- satisfy firstChar
  rest <- many (satisfy bodyChar)
  return (fromCharArray ([first] <> rest))

firstChar :: Char -> Boolean
firstChar c = isLower c

bodyChar :: Char -> Boolean
bodyChar c = isLower c || isDigit c || c == '-'

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

isLower :: Char -> Boolean
isLower c = 'a' <= c && c <= 'z'

