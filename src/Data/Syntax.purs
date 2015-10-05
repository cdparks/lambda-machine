module Data.Syntax where

import Prelude
import Data.Maybe
import Data.Generic

type Name = String

type Definition =
  { name   :: Name
  , syntax :: Syntax
  }

data Syntax
  = Var Name
  | Lambda Name Syntax
  | Apply Syntax Syntax

derive instance genericSyntax :: Generic Syntax
instance showSyntax :: Show Syntax where
  show = gShow

class PrettyPrint a where
  prettyPrint :: a -> String

parensIf :: Boolean -> String -> String
parensIf cond s = if cond then "(" <> s <> ")" else s

instance prettyPrintSyntax :: PrettyPrint Syntax where
  prettyPrint = walk false
   where
    walk inApp e =
      case e of
        Var v      -> v
        Lambda n b -> parensIf inApp ("λ" <> n <> ". " <> walk false b)
        Apply f a  -> walk true f <> " " <> walk true a

