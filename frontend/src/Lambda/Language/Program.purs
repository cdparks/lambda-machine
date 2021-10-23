module Lambda.Language.Program
  ( Program
  ) where

import Lambda.Prelude

import Lambda.Language.Definition (Definition)
import Lambda.Language.Expression (Expression)

type Program =
  { defs :: Array Definition
  , expr :: Maybe Expression
  }
