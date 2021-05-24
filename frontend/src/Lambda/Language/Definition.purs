module Lambda.Language.Definition
  ( Definition(..)
  , Def
  , split
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.Foldable (intercalate)
import Lambda.Language.Expression (Expression(..))
import Lambda.Language.Name (Name)
import Lambda.Language.Parser (class Parse, parse, liftF, token, string)
import Lambda.Language.Pretty (class Pretty, Rep(..), pretty, text)

-- | A top-level definition
newtype Definition = Definition (Def Expression)

-- | Split out for ReadForeign type annotation
type Def expr =
  { name :: Name
  , args :: Array Name
  , expr :: expr
  }

-- | Warning - not alpha-equivalence; names matter here
derive newtype instance eqDefinition :: Eq Definition
derive newtype instance showDefinition :: Show Definition
derive instance newtypeDefinition :: Newtype Definition _

instance readForeignDefinition :: ReadForeign Definition where
  readImpl x = do
    {name, args, expr: e} :: Def String <- readImpl x
    expr <- liftF parse e
    pure $ Definition { name, args, expr }

instance writeForeignDefinition :: WriteForeign Definition where
  writeImpl (Definition {name, args, expr}) = writeImpl
    { name
    , args
    , expr: pretty Raw expr
    }

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
split :: Definition -> {name :: Name, expr :: Expression}
split (Definition {name, args, expr}) =
  { name
  , expr: foldr Lambda expr args
  }

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
    pure $ Definition {name, args, expr}
