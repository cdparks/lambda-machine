module Lambda.Language.Parse
  ( parseAll
  , parseDefinition
  , parseSyntax
  , parseEither
  , unsafeParse
  , formatParseError
  , module X
  ) where

import Lambda.Prelude hiding (between)

import Control.Lazy (fix)
import Data.Array as Array
import Data.Int (fromString)
import Data.Maybe (fromJust)
import Data.String.CodeUnits (fromCharArray)
import Lambda.Language.Name (Name, isSubscriptChar, name, name_, subscriptToInt)
import Lambda.Language.Syntax (Definition, Syntax(..))
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Text.Parsing.Parser (ParseError, Parser, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser (ParseError) as X
import Text.Parsing.Parser.Combinators (between, sepBy, try)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, eof, satisfy, skipSpaces, string)

-- | Run a parser and then consume all trailing space
parseAll :: forall a. Parser String a -> String -> Either ParseError a
parseAll p s = runParser s (skipSpaces *> p <* eof)

-- | Run a parser, crashing if it fails to parse the input. Use this
-- | only for trusted input, e.g. in tests or default definitions.
unsafeParse :: forall a. Parser String a -> String -> a
unsafeParse p s = case parseAll p s of
  Left err ->
    let { message, source, caret } = formatParseError s err
    in unsafeCrashWith $ Array.intercalate "\n"
      [ message
      , source
      , caret
      ]
  Right a -> a

-- | Format a parse error to highlight the position where the malformed
-- | input was encountered.
formatParseError :: String -> ParseError -> { message :: String, source :: String, caret :: String }
formatParseError text err =
  { message: "Parse error: " <> message <> " at column " <> show column
  , source: text
  , caret: caret
  }
 where
  message = parseErrorMessage err
  column = positionColumn $ parseErrorPosition err
  caret = fromCharArray (Array.replicate (column - 1) ' ') <> "^"

-- | Project column from `Position`
positionColumn :: Position -> Int
positionColumn (Position {column}) = column

-- | Parse a `Definition` or an expression
parseEither :: Parser String (Either Definition Syntax)
parseEither = try (Left <$> parseDefinition) <|> (Right <$> parseSyntax)

-- | Parse a `Definition`
-- |
-- | ```ebnf
-- | definition
-- |   = name, {name}, "=", expression ;          (* Definition *)
-- | ```
-- |
parseDefinition :: Parser String Definition
parseDefinition = {name:_, args:_, syntax:_}
  <$> parseName
  <*> Array.many parseName
  <*> (token (string "=") *> parseSyntax)

-- | Parse an expression
-- |
-- | ```ebnf
-- |
-- | expression
-- |   = lambda, name, {name}, ".", expression    (* Lambda abstraction *)
-- |   | name                                     (* Variable *)
-- |   | expression, expression                   (* Application *)
-- |   | "(", expression, ")"                     (* Parentheses *)
-- |   | {digit}                                  (* Natural number *)
-- |   | "[", [expressions], "]"                  (* List *)
-- |   ;
-- | expressions
-- |   = expression, [",", expressions] ;         (* One or more comma-separated expressions *)
-- |
-- | lambda
-- |   = "\"                                      (* Backslash *)
-- |   | "λ"                                      (* Greek letter lambda *)
-- |   ;
-- | ```
-- |
parseSyntax :: Parser String Syntax
parseSyntax =
  fix parseApply
 where
  parseApply p = do
    first <- parseAtom
    rest <- Array.many parseAtom
    case rest of
      [] -> pure first
      _  -> pure (foldl Apply first rest)
   where
    parseAtom :: Parser String Syntax
    parseAtom = parseLambda <|> parseNat <|> parseList p <|> parens p <|> parseVar

    parseLambda :: Parser String Syntax
    parseLambda = do
      void $ token $ string "\\" <|> string "λ"
      names <- Array.some parseName
      void $ token $ string "."
      body <- p
      pure $ foldr Lambda body names

-- | Apply a parser between two grouping characters.
balance :: forall a. Char -> Char -> Parser String a -> Parser String a
balance lhs rhs = between (token (char lhs)) (token (char rhs))

-- | Apply a parser between parens.
parens :: forall a. Parser String a -> Parser String a
parens = balance '(' ')'

-- | Apply a parser between brackets.
brackets :: forall a. Parser String a -> Parser String a
brackets = balance '[' ']'

-- | Convert comma-delimited list to Church-encoded list.
toList :: List Syntax -> Syntax
toList xs =
  Lambda cons (Lambda nil (loop xs))
 where
  cons = name_ "cons"
  nil = name_ "nil"
  loop Nil = Var nil
  loop (Cons y ys) = Apply (Apply (Var cons) y) (loop ys)

-- | Convert a natural number to a Church numeral
toChurch :: Int -> Syntax
toChurch n =
  Lambda s (Lambda z (loop n))
 where
  s = name_ "s"
  z = name_ "z"
  loop k
    | k <= 0 = Var z
    | otherwise = Apply (Var s) (loop (k - 1))

-- | Parse a natural number.
parseNat :: Parser String Syntax
parseNat = token do
  digits <- Array.some $ satisfy isDigit
  let n = unsafePartial $ fromJust $ fromString $ fromCharArray digits
  pure $ toChurch n

-- | Parse a comma-separated list between brackets.
parseList :: Parser String Syntax -> Parser String Syntax
parseList p = toList <$> brackets (p `sepBy` token (char ','))

-- | Parse a variable.
parseVar :: Parser String Syntax
parseVar = Var <$> parseName

-- | Parse a `Name`. This is one of the more complicated parsers,
-- | because I have absolutely no chill, apparently.
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
parseName :: Parser String Name
parseName = token do
  first <- satisfy firstChar
  body <- Array.many $ satisfy bodyChar
  question <- string "?" <|> pure ""
  subscript <- Just <$> parseSubscript <|> pure Nothing
  let var = fromCharArray ([first] <> body) <> question
  pure $ name var subscript

-- | Parse subscripts for a `Name`
parseSubscript :: Parser String Int
parseSubscript = subscriptToInt <<< fromCharArray <$> Array.some (satisfy isSubscriptChar)

-- | Parse the first character of a `Name`
firstChar :: Char -> Boolean
firstChar c = isLower c || c == '_'

-- | Parse the remaining (non-question-mark, non-subscript-y)
-- | characters of a `Name`.
bodyChar :: Char -> Boolean
bodyChar c = isLower c || c == '-'

-- | Is a character a decimal digit?
isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'

-- | Is a character a lowercase letter?
isLower :: Char -> Boolean
isLower c = 'a' <= c && c <= 'z'

-- | Extend a parser to consume trailing whitespace
token :: forall a. Parser String a -> Parser String a
token p = p <* skipSpaces
