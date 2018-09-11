module Data.Parse
  ( parseAll
  , parseDefinition
  , parseSyntax
  , parseEither
  , unsafeParse
  , formatParseError
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (some, many, replicate)
import Data.Either (Either(..), fromRight)
import Data.Foldable (fold, foldl, foldr)
import Data.Int (fromString)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust)
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
  fold
    [ "Parse error: "
    , message
    , " at column "
    , show column
    , "\n"
    , text
    , "\n"
    , caretLine
    ]
 where
  message = parseErrorMessage err
  column = positionColumn $ parseErrorPosition err
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
parseSyntax =
  fix parseApply
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

balance :: forall a. Char -> Char -> Parser String a -> Parser String a
balance lhs rhs = between (token (char lhs)) (token (char rhs))

parens :: forall a. Parser String a -> Parser String a
parens = balance '(' ')'

brackets :: forall a. Parser String a -> Parser String a
brackets = balance '[' ']'

toList :: List Syntax -> Syntax
toList xs =
  Lambda (name_ "cons") (Lambda (name_ "nil") (loop xs))
 where
  loop Nil = Var (name_ "nil")
  loop (Cons y ys) = Apply (Apply (Var (name_ "cons")) y) (loop ys)

toChurch :: Int -> Syntax
toChurch n =
  Lambda (name_ "s") (Lambda (name_ "z") (loop n))
 where
  loop k
    | k <= 0 = Var (name_ "z")
    | otherwise = Apply (Var (name_ "s")) (loop (k - 1))

parseNat :: Parser String Syntax
parseNat = token do
  digits <- some $ satisfy isDigit
  let n = unsafePartial $ fromJust $ fromString $ fromCharArray digits
  pure $ toChurch n

parseList :: Parser String Syntax -> Parser String Syntax
parseList p = toList <$> brackets (p `sepBy` token (char ','))

parseVar :: Parser String Syntax
parseVar = Var <$> parseName

parseName :: Parser String Name
parseName = token do
  first <- satisfy firstChar
  body <- many $ satisfy bodyChar
  question <- string "?" <|> pure ""
  subscript <- Just <$> parseSubscript <|> pure Nothing
  let var = fromCharArray ([first] <> body) <> question
  pure $ name var subscript

parseSubscript :: Parser String Int
parseSubscript = subscriptToInt <<< fromCharArray <$> some (satisfy isSubscriptChar)

firstChar :: Char -> Boolean
firstChar c = isLower c || c == '_'

bodyChar :: Char -> Boolean
bodyChar c = isLower c || c == '-'

isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

isLower :: Char -> Boolean
isLower c = 'a' <= c && c <= 'z'
