module Lambda.Language.Definition
  ( Definition(..)
  , split
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
split :: Definition -> {name :: Name, expr :: Expression}
split (Definition {name, args, expr}) = {name, expr: foldr Lambda expr args}

-- | Parse a `Definition`
-- |
-- | ```ebnf
-- | definition = name, {name}, "=", expression ;
-- | ```
-- |
instance parseDefinition :: Parse Definition where
  parse = map Definition
    $ {name:_, args:_, expr:_}
    <$> parse
    <*> Array.many parse
    <*> (token (string "=") *> parse)
