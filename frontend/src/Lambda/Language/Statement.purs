module Lambda.Language.Statement
  ( Statement(..)
  ) where

import Lambda.Prelude

import Lambda.Language.Definition (Definition)
import Lambda.Language.Expression (Expression)
import Lambda.Language.Parser (class Parse, parse, try)
import Lambda.Language.Pretty (class Pretty, pretty)

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

-- | Parse a `Definition` or an `Expression`
instance parseStatement :: Parse Statement where
  parse = try (Define <$> parse) <|> (Eval <$> parse)
