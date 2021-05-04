module Lambda.Language.Syntax
  ( Statement(..)
  , Definition(..)
  , Expression(..)
  , fromDef
  , unHighlight
  ) where

import Lambda.Prelude

import Data.Foldable (intercalate)
import Lambda.Language.Display(Rep(..), class Display, text, style, class Pretty, pretty, parensIf)
import Lambda.Language.Name (Name)

-- | A `Statement` is either a top-level `Definition` or an
-- | `Expression` to be evaluated
data Statement
  = Define Definition
  | Eval Expression

derive instance genericStatement :: Generic Statement _

instance showStatement :: Show Statement where
  show x = genericShow x

-- | Warning - not alpha-equivalence; names matter here
instance eqStatement :: Eq Statement where
  eq x = genericEq x

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

derive instance newtypeDefinition :: Newtype Definition _
derive newtype instance showDefinition :: Show Definition

-- | Warning - not alpha-equivalence; names matter here
derive newtype instance eqDefinition :: Eq Definition

instance prettyDefinition :: Pretty Definition where
  pretty rep (Definition {name, args, expr}) = fold
    [ text $ formatPrefix name args
    , pretty rep expr
    ]
   where
    formatPrefix name args = fold
      [ show name
      , " "
      , intercalate " " $ (show <$> args) <> ["= "]
      ]

-- | Convert a `Definition` to an `Expression` returning the `Name`
fromDef :: Definition -> {name :: Name, expr :: Expression}
fromDef (Definition {name, args, expr}) = {name, expr: foldr Lambda expr args}

-- | Source-level syntax minus syntactic sugar for lists and natural
-- | numbers; those are eliminated in the parser. The Highlight
-- | constructor is for marking interesting nodes.
data Expression
  = Var Name
  | Lambda Name Expression
  | Apply Expression Expression
  | Highlight Expression

-- | Expression with no Highlight nodes
unHighlight :: Expression -> Expression
unHighlight = case _ of
  e@(Var _) -> e
  Lambda n e -> Lambda n $ unHighlight e
  Apply f a -> Apply (unHighlight f) (unHighlight a)
  Highlight e -> unHighlight e

derive instance genericExpression :: Generic Expression _

instance showExpression :: Show Expression where
  show x = genericShow x

-- | Warning - not alpha-equivalence; names matter here
instance eqExpression :: Eq Expression where
  eq x = genericEq x

instance prettyExpression :: Pretty Expression where
  pretty rep = loop false
   where
    loop inApp = case _ of
      Var v ->
        text $ show v
      Lambda name body ->
        let
          raw = parensIf inApp $ fold
            [ text $ "λ" <> show name <> ". "
            , loop false body
            ]
          lam = unHighlight $ Lambda name body
        in case rep of
          Sugar | Just lit <- asNatural lam <|> asList lam -> lit
          _ -> raw
      Apply f a ->
        parensIf inApp $ fold
          [ loop (isLambda f) f
          , text " "
          , loop (isComposite a) a
          ]
      Highlight (Apply f a) ->
        parensIf inApp $ fold
          [ highlight Function $ loop (isLambda f) f
          , text " "
          , highlight Argument $ loop (isComposite a) a
          ]
      Highlight x ->
        highlight Global $ loop inApp x

-- | What's being highlighted in an expression
data Highlight
  = Function
  | Argument
  | Global

-- | Highlight interesting parts of the redex
highlight :: forall r. Display r => Highlight -> r -> r
highlight = style <<< case _ of
  Function -> "highlight-function"
  Argument -> "highlight-argument"
  Global -> "highlight-global"

-- | Is node an application or lambda?
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

-- | Attempt to interpret syntax as a Church natural.
asNatural :: forall r. Display r => Expression -> Maybe r
asNatural (Lambda s0 (Lambda z0 body)) =
  text <<< show <$> walk body
 where
  walk (Apply (Var s) arg)
    | s == s0 = (_ + 1) <$> walk arg
    | otherwise = Nothing
  walk (Var z)
    | z == z0 = pure 0
    | otherwise = Nothing
  walk _ = Nothing
asNatural _ = Nothing

-- | Attempt to interpret syntax as a Church-encoded list.
asList :: forall r. Display r => Expression -> Maybe r
asList (Lambda cons0 (Lambda nil0 body)) =
  commaSep <$> walk body
 where
  walk (Apply (Apply (Var cons) x) xs)
    | cons == cons0 = Cons (pretty Sugar x) <$> walk xs
    | otherwise = Nothing
  walk (Var nil)
    | nil == nil0 = pure Nil
    | otherwise = Nothing
  walk _ = Nothing
asList _ = Nothing

commaSep :: forall r. Display r => List r -> r
commaSep xs = text "[" <> intercalate (text ", ") xs <> text "]"
