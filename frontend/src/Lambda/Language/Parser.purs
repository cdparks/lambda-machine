module Lambda.Language.Parser
  ( class Parse
  , parse
  , Parser
  , run
  , liftJson
  , unsafeRun
  , formatParseError
  , token
  , balance
  , parens
  , brackets
  , module X
  ) where

import Lambda.Prelude hiding (between)

import Data.Array as Array
import Data.String.CodeUnits (fromCharArray)
import Partial.Unsafe (unsafeCrashWith)
import Text.Parsing.Parser (ParseError, parseErrorMessage, parseErrorPosition, runParser)
import Text.Parsing.Parser (fail, ParseError) as X
import Text.Parsing.Parser as Parser
import Text.Parsing.Parser.Combinators (between)
import Text.Parsing.Parser.Combinators (between, option, optionMaybe, optional, sepBy, sepBy1, try) as X
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.String (char, eof, skipSpaces)
import Text.Parsing.Parser.String (char, satisfy, skipSpaces, string) as X

-- | All of our parsers work over strings
type Parser r = Parser.Parser String r

-- | Parse some structure from a string
class Parse a where
  parse :: Parser a

-- | Run a parser and then consume all trailing space
run :: forall a. Parser a -> String -> Either ParseError a
run p s = runParser s (skipSpaces *> p <* eof)

-- | Parse structured data from a JSON string in Either
liftJson :: forall a. Parser a -> String -> Either JsonDecodeError a
liftJson p = lmap TypeMismatch <<< simpleRun p

-- | Run a parser, formatting the parse error as a String
simpleRun :: forall a. Parser a -> String -> Either String a
simpleRun p s = lmap (toString <<< formatParseError s) $ run p s
 where
  toString { message, source, caret } = Array.intercalate "\n"
    [ message
    , source
    , caret
    ]

-- | Run a parser, crashing if it fails to parse the input. Use this
-- | only for trusted input, e.g. in tests or default definitions.
unsafeRun :: forall a. Parser a -> String -> a
unsafeRun p = either unsafeCrashWith identity <<< simpleRun p

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

-- | Extend a parser to consume trailing whitespace
token :: forall a. Parser a -> Parser a
token p = p <* skipSpaces

-- | Apply a parser between two grouping characters.
balance :: forall a. Char -> Char -> Parser a -> Parser a
balance lhs rhs = between (token (char lhs)) (token (char rhs))

-- | Apply a parser between parens.
parens :: forall a. Parser a -> Parser a
parens = balance '(' ')'

-- | Apply a parser between brackets.
brackets :: forall a. Parser a -> Parser a
brackets = balance '[' ']'
