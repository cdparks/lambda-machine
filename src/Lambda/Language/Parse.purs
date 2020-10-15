module Lambda.Language.Parse
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
import Data.Either (Either(..))
import Data.Foldable (fold, foldl, foldr)
import Data.Int (fromString)
import Data.List (List(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.String.CodeUnits (fromCharArray)
import Lambda.Language.Name (Name, isSubscriptChar, name, name_, subscriptToInt)
import Lambda.Language.Syntax (Definition, Syntax(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Text.Parsing.Parser (ParseError, Parser, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy, try)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, eof, satisfy, skipSpaces, string)

token :: forall a. Parser String a -> Parser String a
token p = p <* skipSpaces

parseAll :: forall a. Parser String a -> String -> Either ParseError a
parseAll p s = runParser s (skipSpaces *> p <* eof)

unsafeParse :: forall a. Parser String a -> String -> a
unsafeParse p s = case parseAll p s of
  Left err -> unsafeCrashWith $ formatParseError s err
  Right a -> a

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
  Lambda cons (Lambda nil (loop xs))
 where
  cons = name_ "cons"
  nil = name_ "nil"
  loop Nil = Var nil
  loop (Cons y ys) = Apply (Apply (Var cons) y) (loop ys)

toChurch :: Int -> Syntax
toChurch n =
  Lambda s (Lambda z (loop n))
 where
  s = name_ "s"
  z = name_ "z"
  loop k
    | k <= 0 = Var z
    | otherwise = Apply (Var s) (loop (k - 1))

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
