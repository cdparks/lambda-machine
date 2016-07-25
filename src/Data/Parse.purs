module Data.Parse
  ( parseAll
  , parseDefinition
  , parseSyntax
  , parseEither
  , unsafeParse
  , formatParseError
  ) where

import Prelude

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Pos

import Control.Lazy (fix)
import Control.Alt ((<|>))
import Control.Apply ((*>), (<*))

import Data.Array (some, many, replicate, length)
import Data.Foldable (foldl, elem)
import Data.String (fromCharArray)
import Data.Either
import Data.Either.Unsafe (fromRight)
import Data.Maybe.Unsafe (fromJust)
import Data.Int (fromString)
import Data.List (List(..))

import Data.Syntax
import Data.Name

token :: forall a. Parser String a -> Parser String a
token p = p <* skipSpaces

parseAll :: forall a. Parser String a -> String -> Either ParseError a
parseAll p s = runParser s (skipSpaces *> p <* eof)

unsafeParse :: forall a. Parser String a -> String -> a
unsafeParse p s = fromRight (parseAll p s)

formatParseError :: String -> ParseError -> String
formatParseError text (ParseError { message: message, position: Position { column: column } }) =
  "Parse error: " <> message <> " at column " <> show column <> "\n" <> text <> "\n" <> caretLine
 where
  caretLine = fromCharArray (replicate (column - 1) ' ') <> "^"

caretForParseError :: ParseError -> String
caretForParseError (ParseError { position: Position { column: column } }) =
  fromCharArray (replicate (column - 1) ' ') <> "^"

parseEither :: Parser String (Either Definition Syntax)
parseEither = try (Left <$> parseDefinition) <|> (Right <$> parseSyntax)

parseDefinition :: Parser String Definition
parseDefinition = {name:_, args:_, syntax:_}
  <$> parseName
  <*> many parseName
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
    parseAtom = parseLambda <|> parseNat <|> parseList p <|> parens p <|> parseVar

    parseLambda :: Parser String Syntax
    parseLambda = Lambda
      <$> (token (string "\\" <|> string "λ") *> parseName)
      <*> (token (string ".") *> p)

parens :: forall a. Parser String a -> Parser String a
parens = between (token (string "(")) (token (string ")"))

toList :: List Syntax -> Syntax
toList xs = Lambda (name_ "cons") (Lambda (name_ "nil") (loop xs))
 where
  loop Nil = Var (name_ "nil")
  loop (Cons x xs) = Apply (Apply (Var (name_ "cons")) x) (loop xs)

toChurch :: Int -> Syntax
toChurch n = Lambda (name_ "s") (Lambda (name_ "z") (loop n))
 where
  loop k
    | k <= 0 = Var (name_ "z")
    | otherwise = Apply (Var (name_ "s")) (loop (k - 1))

parseNat :: Parser String Syntax
parseNat = token do
  digits <- some (satisfy isDigit)
  let n = fromJust (fromString (fromCharArray digits))
  return (toChurch n)

parseList :: Parser String Syntax -> Parser String Syntax
parseList p = token do
  _ <- char '['
  elements <- p `sepBy` token (char ',')
  _ <- char ']'
  return (toList elements)

parseVar :: Parser String Syntax
parseVar = Var <$> parseName

parseName :: Parser String Name
parseName = token do
  first <- satisfy firstChar
  body <- many (satisfy bodyChar)
  question <- string "?" <|> pure ""
  subscript <- parsePrimes <|> parseSubscript
  return (name (fromCharArray ([first] <> body) <> question) subscript)

parsePrimes :: Parser String Int
parsePrimes = length <$> some (satisfy (== '\''))

parseSubscript :: Parser String Int
parseSubscript = subscriptToInt <<< fromCharArray <$> many (satisfy isSubscriptChar)

firstChar :: Char -> Boolean
firstChar c = isLower c || c == '_'

bodyChar :: Char -> Boolean
bodyChar c = isLower c || isDigit c || c == '-'

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

isLower :: Char -> Boolean
isLower c = 'a' <= c && c <= 'z'

