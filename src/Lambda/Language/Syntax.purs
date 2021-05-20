module Lambda.Language.Syntax
  ( Statement(..)
  , Definition(..)
  , Expression(..)
  , fromDef
  , unHighlight
  ) where

import Lambda.Prelude

import Data.Foldable (intercalate)
import Data.List as List
import Lambda.Language.Name (Name)
import Lambda.Language.Pretty (Rep(..), class Pretty, pretty, parensIf, Builder, text, style)

-- | A `Statement` is either a top-level `Definition` or an
-- | `Expression` to be evaluated
data Statement
  = Define Definition
  | Eval Expression

-- | Warning - not alpha-equivalence; names matter here
derive instance eqStatement :: Eq Statement
derive instance genericStatement :: Generic Statement _

instance showStatement :: Show Statement where
  show x = genericShow x

instance prettyStatement :: Pretty Statement where
  pretty rep = case _ of
    Define def -> pretty rep def
    Eval expr -> pretty rep expr

-- | A top-level definition
newtype Definition = Definition
  { name :: Name
  , args :: Array Name
  , expr :: Expression
  }

-- | Warning - not alpha-equivalence; names matter here
derive newtype instance eqDefinition :: Eq Definition
derive newtype instance showDefinition :: Show Definition
derive instance newtypeDefinition :: Newtype Definition _

instance prettyDefinition :: Pretty Definition where
  pretty rep (Definition {name, args, expr}) =
    text prefix <> pretty rep expr
   where
    prefix = show name <> " " <> intercalate " " ((show <$> args) <> ["= "])

-- | Convert a `Definition` to an `Expression` returning the `Name`
fromDef :: Definition -> {name :: Name, expr :: Expression}
fromDef (Definition {name, args, expr}) = {name, expr: foldr Lambda expr args}

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
