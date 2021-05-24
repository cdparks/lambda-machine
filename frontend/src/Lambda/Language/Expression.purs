module Lambda.Language.Expression
  ( Expression(..)
  , unHighlight
  ) where

import Lambda.Prelude

import Control.Lazy (fix)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Int (fromString)
import Data.List as List
import Data.Maybe (fromJust)
import Data.String.CodeUnits (fromCharArray)
import Lambda.Language.Name (Name)
import Lambda.Language.Name as Name
import Lambda.Language.Parser (class Parse, parse, liftF, Parser, brackets, char, fail, parens, satisfy, sepBy, string, token)
import Lambda.Language.Pretty (Rep(..), class Pretty, pretty, parensIf, Builder, text, style)
import Partial.Unsafe (unsafePartial)

-- | Source-level syntax minus syntactic sugar for lists and natural
-- | numbers; those are eliminated in the parser. The Highlight
-- | constructor is for marking interesting nodes. Cycles are rare
-- | but can technically be created by evaluating recursively defined
-- | fix with no argument.
data Expression
  = Var Name
  | Lambda Name Expression
  | Apply Expression Expression
  | Highlight Expression
  | Cycle

-- | Expression with no Highlight nodes
unHighlight :: Expression -> Expression
unHighlight = case _ of
  e@(Var _) -> e
  Lambda n e -> Lambda n $ unHighlight e
  Apply f a -> Apply (unHighlight f) (unHighlight a)
  Highlight e -> e
  Cycle -> Cycle

-- | Warning - not alpha-equivalence; names matter here
derive instance eqExpression :: Eq Expression
derive instance genericExpression :: Generic Expression _

instance showExpression :: Show Expression where
  show x = genericShow x

instance readForeignExpression :: ReadForeign Expression where
  readImpl = liftF parse <=< readImpl

instance writeForeignExpression :: WriteForeign Expression where
  writeImpl = writeImpl <<< pretty Raw

instance prettyExpression :: Pretty Expression where
  pretty rep = loop false
   where
    loop inApp = case _ of
      Var v ->
        text $ show v
      Lambda name body
        | rep == Sugar -> tryLiteral inApp name body
        | otherwise -> prettyLambda inApp name body
      Apply f a ->
        parensIf inApp
          $ loop (isLambda f) f
          <> text " "
          <> loop (isComposite a) a
      Highlight (Apply f a) ->
        parensIf inApp
          $ highlight Function (loop (isLambda f) f)
          <> text " "
          <> highlight Argument (loop (isComposite a) a)
      Highlight x ->
        highlight Global $ loop inApp x
      Cycle -> text "…"

    tryLiteral inApp name body = fromMaybe' (\_ -> prettyLambda inApp name body) $ do
      head <- literalHead $ Lambda name body
      asNatural head <|> asList head

    prettyLambda inApp name body =
      parensIf inApp $ text ("λ" <> show name <> ". ") <> loop false body

-- | What's being highlighted in an expression
data Highlight
  = Function
  | Argument
  | Global

-- | Highlight interesting parts of the redex
highlight :: Highlight -> Builder -> Builder
highlight = style <<< case _ of
  Function -> "highlight-function"
  Argument -> "highlight-argument"
  Global -> "highlight-global"

-- | Is node an application or a lambda?
isComposite :: Expression -> Boolean
isComposite = case _ of
  Apply _ _  -> true
  Lambda _ _ -> true
  _ -> false

-- | Is node a lambda?
isLambda :: Expression -> Boolean
isLambda = case _ of
  Lambda _ _ -> true
  _ -> false

type Head = { f :: Name, z :: Name, body :: Expression }

-- | Match head of Church-encoded list or natural number
literalHead :: Expression -> Maybe Head
literalHead = case _ of
  Lambda f (Highlight (Lambda z body)) -> Just { f, z, body }
  Lambda f (Lambda z body) -> Just { f, z, body }
  _ -> Nothing

-- | Attempt to interpret syntax as a Church natural.
asNatural :: Head -> Maybe Builder
asNatural {f, z, body} = text <<< show <$> walk 0 body
 where
  walk acc (Apply (Var s) arg)
    | s == f = walk (1 + acc) arg
  walk acc (Var t)
    | t == z = pure acc
  walk _ _ = Nothing

-- | Attempt to interpret syntax as a Church-encoded list.
asList :: Head -> Maybe Builder
asList {f, z, body} = commaSep <$> walk Nil body
 where
  walk acc (Apply (Apply (Var cons) x) xs)
    | cons == f = walk (Cons (pretty Sugar x) acc) xs
  walk acc (Var nil)
    | nil == z = pure $ List.reverse acc
  walk _ _ = Nothing

commaSep :: List Builder -> Builder
commaSep xs = text "[" <> intercalate (text ", ") xs <> text "]"

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
-- |
-- | expressions
-- |   = expression, [",", expressions] ;         (* One or more comma-separated expressions *)
-- |
-- | lambda
-- |   = "\"                                      (* Backslash *)
-- |   | "λ"                                      (* Greek letter lambda *)
-- |   ;
-- | ```
-- |
instance parseExpression :: Parse Expression where
  parse = fix \parseExpr -> foldl Apply
    <$> parseAtom parseExpr
    <*> Array.many (parseAtom parseExpr)

parseAtom :: Parser Expression -> Parser Expression
parseAtom parseExpr =
  failCycle <|> parseLambda <|> parseNat <|> parseList parseExpr <|> parens parseExpr <|> parseVar
 where
  parseLambda :: Parser Expression
  parseLambda = do
    void $ token $ string "\\" <|> string "λ"
    names <- Array.some parse
    void $ token $ string "."
    body <- parseExpr
    pure $ foldr Lambda body names

-- | Convert comma-delimited list to Church-encoded list.
toList :: List Expression -> Expression
toList xs =
  Lambda cons (Lambda nil (loop xs))
 where
  cons = Name.from "cons"
  nil = Name.from "nil"
  loop Nil = Var nil
  loop (Cons y ys) = Apply (Apply (Var cons) y) (loop ys)

-- | Convert a natural number to a Church numeral
toNat :: Int -> Expression
toNat n =
  Lambda s (Lambda z (loop n))
 where
  s = Name.from "s"
  z = Name.from "z"
  loop k
    | k <= 0 = Var z
    | otherwise = Apply (Var s) (loop (k - 1))

-- | Fail if the user pastes a cycle's representation (… or ...) back into the input
failCycle :: Parser Expression
failCycle = token do
  ellipsis <- string "…" <|> string "..."
  fail $ fold
    [ "We don't know how to parse “"
    , ellipsis
    , "”! (Hint: it might indicate cyclic data or truncated output) "
    ]

-- | Parse a natural number.
parseNat :: Parser Expression
parseNat = token do
  digits <- Array.some $ satisfy isDigit
  let n = unsafePartial $ fromJust $ fromString $ fromCharArray digits
  pure $ toNat n

-- | Parse a comma-separated list between brackets.
parseList :: Parser Expression -> Parser Expression
parseList p = toList <$> brackets (p `sepBy` token (char ','))

-- | Parse a variable.
parseVar :: Parser Expression
parseVar = Var <$> parse

-- | Is a character a decimal digit?
isDigit :: Char -> Boolean
isDigit c = '0' <= c && c <= '9'
