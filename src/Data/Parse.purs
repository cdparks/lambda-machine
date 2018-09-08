module Data.Parse
  ( parseAll
  , parseDefinition
  , parseSyntax
  , parseEither
  , unsafeParse
  , formatParseError
  ) where

import Prelude
  ( bind
  , discard
  , otherwise
  , pure
  , show
  , void
  , (&&)
  , (*>)
  , (-)
  , (<$>)
  , (<*)
  , (<*>)
  , (<<<)
  , (<=)
  , (<>)
  , (==)
  , (||)
  , ($)
  )

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (some, many, replicate, length)
import Data.Either (Either(..), fromRight)
import Data.Foldable (foldl, foldr)
import Data.Int (fromString)
import Data.List (List(..))
import Data.Maybe (fromJust)
import Data.String.CodeUnits (fromCharArray)
import Partial.Unsafe (unsafePartial)
import Text.Parsing.Parser.Combinators (between, sepBy, try)
import Text.Parsing.Parser (ParseError, Parser, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, eof, satisfy, skipSpaces, string)

import Data.Syntax (Definition, Syntax(..))
import Data.Name (Name, isSubscriptChar, name, name_, subscriptToInt)

token :: forall a. Parser String a -> Parser String a
token p = p <* skipSpaces

parseAll :: forall a. Parser String a -> String -> Either ParseError a
parseAll p s = runParser s (skipSpaces *> p <* eof)

unsafeParse :: forall a. Parser String a -> String -> a
unsafeParse p s = unsafePartial (fromRight (parseAll p s))

formatParseError :: String -> ParseError -> String
formatParseError text err =
  "Parse error: " <> message <> " at column " <> show column <> "\n" <> text <> "\n" <> caretLine
 where
  message = parseErrorMessage err
  column = positionColumn (parseErrorPosition err)
  caretLine = fromCharArray (replicate (column - 1) ' ') <> "^"

positionColumn :: Position -> Int
positionColumn (Position {column: column}) = column

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
      [] -> pure first
      _  -> pure (foldl Apply first rest)
   where
    parseAtom :: Parser String Syntax
    parseAtom = parseLambda <|> parseNat <|> parseList p <|> parens p <|> parseVar

    parseLambda :: Parser String Syntax
    parseLambda = do
      void $ token $ string "\\" <|> string "λ"
      names <- some parseName
      void $ token $ string "."
      body <- p
      pure $ foldr Lambda body names

parens :: forall a. Parser String a -> Parser String a
parens = between (token (string "(")) (token (string ")"))

toList :: List Syntax -> Syntax
toList xs = Lambda (name_ "cons") (Lambda (name_ "nil") (loop xs))
 where
  loop Nil = Var (name_ "nil")
  loop (Cons y ys) = Apply (Apply (Var (name_ "cons")) y) (loop ys)

toChurch :: Int -> Syntax
toChurch n = Lambda (name_ "s") (Lambda (name_ "z") (loop n))
 where
  loop k
    | k <= 0 = Var (name_ "z")
    | otherwise = Apply (Var (name_ "s")) (loop (k - 1))

parseNat :: Parser String Syntax
parseNat = token do
  digits <- some (satisfy isDigit)
  let n = unsafePartial (fromJust (fromString (fromCharArray digits)))
  pure (toChurch n)

parseList :: Parser String Syntax -> Parser String Syntax
parseList p = token do
  void $ char '['
  elements <- p `sepBy` token (char ',')
  void $ char ']'
  pure (toList elements)

parseVar :: Parser String Syntax
parseVar = Var <$> parseName

parseName :: Parser String Name
parseName = token do
  first <- satisfy firstChar
  body <- many (satisfy bodyChar)
  question <- string "?" <|> pure ""
  subscript <- parsePrimes <|> parseSubscript
  pure (name (fromCharArray ([first] <> body) <> question) subscript)

parsePrimes :: Parser String Int
parsePrimes = length <$> some (satisfy (_ == '\''))

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
