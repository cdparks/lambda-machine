module Lambda.Language.Syntax
  ( Definition(..)
  , defToSyntax
  , defToDoc
  , Syntax(..)
  ) where

import Lambda.Prelude

import Data.List (intercalate)
import Lambda.Language.Name (Name)
import Lambda.Language.PrettyPrint (class PrettyPrint, Doc, doc, parensIf, prettyPrint, sugar, raw)

-- | Top-level definition
type Definition =
  { name :: Name
  , args :: Array Name
  , syntax :: Syntax
  }

-- | Convert a `Definition` to an anonymous expression
defToSyntax :: Definition -> Syntax
defToSyntax def = foldr Lambda def.syntax def.args

-- | Pretty-print `Definition`
defToDoc :: Definition -> Doc String
defToDoc def =
  fold
    [ pure $ show def.name
    , prettyArgs def.args
    , pure " = "
    , prettyPrint def.syntax
    ]
 where
  prettyArgs [] = pure ""
  prettyArgs as = pure " " <> intercalate (pure " ") (map prettyPrint as)

-- | Source-level syntax minus syntactic sugar for lists and natural
-- | numbers; those are eliminated in the parser.
data Syntax
  = Var Name
  | Lambda Name Syntax
  | Apply Syntax Syntax

derive instance genericSyntax :: Generic Syntax _

instance showSyntax :: Show Syntax where
  show x = genericShow x

-- | Warning - not alpha-equivalence; names matter here
instance eqSyntax :: Eq Syntax where
  eq x = genericEq x

-- | Is node an application or lambda?
isComposite :: Syntax -> Boolean
isComposite (Var _) = false
isComposite _ = true

-- | Is node a lambda?
isLambda :: Syntax -> Boolean
isLambda (Lambda _ _) = true
isLambda _ = false

-- | Attempt to interpret syntax as a Church natural.
tryFromChurch :: Syntax -> Maybe String
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
tryFromList :: Syntax -> Maybe String
tryFromList (Lambda c (Lambda n body)) =
  listToString <$> walk body
 where
  walk (Apply (Apply (Var c') x) xs)
    | c' == c = Cons (sugar (prettySyntax x)) <$> walk xs
    | otherwise = Nothing
  walk (Var n')
    | n' == n = pure Nil
    | otherwise = Nothing
  walk _ = Nothing
tryFromList _ = Nothing

-- | Stringify bracketed, comma-separated list.
listToString :: List String -> String
listToString xs = "[" <> intercalate ", " xs <> "]"

-- | Pretty-print `Syntax`, minimizing parens.
prettySyntax :: Syntax -> Doc String
prettySyntax =
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

instance prettyPrintSyntax :: PrettyPrint Syntax where
  prettyPrint = prettySyntax
