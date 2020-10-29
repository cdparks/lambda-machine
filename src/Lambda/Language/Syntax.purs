module Lambda.Language.Syntax
  ( Statement(..)
  , Definition(..)
  , Expression(..)
  , fromDef
  ) where

import Lambda.Prelude

import Data.List (intercalate)
import Lambda.Language.Name (Name)
import Lambda.Language.PrettyPrint (class PrettyPrint, doc, parensIf, prettyPrint, sugar, raw)

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

instance prettyPrintStatement :: PrettyPrint Statement where
  prettyPrint = case _ of
    Define def -> prettyPrint def
    Eval expr -> prettyPrint expr

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

instance prettyPrintDefinition :: PrettyPrint Definition where
  prettyPrint (Definition {name, args, expr}) = fold
    [ pure $ show name
    , prettyArgs args
    , pure " = "
    , prettyPrint expr
    ]
   where
    prettyArgs [] = pure ""
    prettyArgs as = pure " " <> intercalate (pure " ") (map prettyPrint as)

-- | Convert a `Definition` to an `Expression` returning the `Name`
fromDef :: Definition -> {name :: Name, expr :: Expression}
fromDef (Definition {name, args, expr}) = {name, expr: foldr Lambda expr args}

-- | Source-level syntax minus syntactic sugar for lists and natural
-- | numbers; those are eliminated in the parser.
data Expression
  = Var Name
  | Lambda Name Expression
  | Apply Expression Expression

derive instance genericExpression :: Generic Expression _

instance showExpression :: Show Expression where
  show x = genericShow x

-- | Warning - not alpha-equivalence; names matter here
instance eqExpression :: Eq Expression where
  eq x = genericEq x

instance prettyPrintExpression :: PrettyPrint Expression where
  prettyPrint =
    walk false
   where
    walk inApp = case _ of
      Var v ->
        pure $ show v
      l@(Lambda n b) ->
        let
          simple = parensIf inApp (pure "λ" <> prettyPrint n <> pure ". " <> walk false b)
          literal = tryFromChurch l <|> tryFromList l
        in
          doc {raw: raw simple, sugar: fromMaybe (sugar simple) literal}
      Apply f a ->
        parensIf inApp (walk (isLambda f) f <> pure " " <> walk (isComposite a) a)

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
tryFromChurch :: Expression -> Maybe String
tryFromChurch (Lambda s (Lambda z body)) =
  show <$> walk body
 where
  walk (Apply (Var s') arg)
    | s' == s = (_ + 1) <$> walk arg
    | otherwise = Nothing
  walk (Var z')
    | z' == z = pure 0
    | otherwise = Nothing
  walk _ = Nothing
tryFromChurch _ = Nothing

-- | Attempt to interpret syntax as a Church-encoded list.
tryFromList :: Expression -> Maybe String
tryFromList (Lambda c (Lambda n body)) =
  listToString <$> walk body
 where
  walk (Apply (Apply (Var c') x) xs)
    | c' == c = Cons (sugar $ prettyPrint x) <$> walk xs
    | otherwise = Nothing
  walk (Var n')
    | n' == n = pure Nil
    | otherwise = Nothing
  walk _ = Nothing
tryFromList _ = Nothing

-- | Stringify bracketed, comma-separated list.
listToString :: List String -> String
listToString xs = "[" <> intercalate ", " xs <> "]"
