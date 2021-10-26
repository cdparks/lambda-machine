module Lambda.Language.Definition
  ( Definition(..)
  , split
  , join
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Lambda.Language.Expression (Expression(..))
import Lambda.Language.Name (Name)
import Lambda.Language.Parser (class Parse, parse, token, string)
import Lambda.Language.Pretty (class Pretty, pretty, text)

-- | A top-level definition
newtype Definition = Definition
  { id :: Int
  , name :: Name
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
    prefix = fold
      [ show name
      , " "
      , intercalate " " $ fold
        [ show <$> args
        , ["= "]
        ]
      ]

-- | Convert a `Definition` to an `Expression` returning the `Name`
split :: Definition -> {id :: Int, name :: Name, expr :: Expression}
split (Definition {id, name, args, expr}) =
  { id
  , name
  , expr: foldr Lambda expr args
  }

-- | Covert a `Name` and an `Expression` to a `Definition`
join :: Name -> Expression -> Definition
join name = Definition <<< go []
 where
  go args = case _ of
    Lambda arg body-> go (args <> [arg]) body
    expr-> { id: 0, name, args, expr }

-- | Parse a `Definition`
-- |
-- | ```ebnf
-- | definition = name, {name}, "=", expression ;
-- | ```
-- |
instance parseDefinition :: Parse Definition where
  parse = do
    name <- parse
    args <- Array.many parse
    expr <- token (string "=") *> parse
    pure $ Definition {id: 0, name, args, expr}
