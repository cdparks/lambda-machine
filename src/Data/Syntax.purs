module Data.Syntax
  ( Definition(..)
  , defToSyntax
  , defToString
  , Syntax(..)
  ) where

import Prelude
import Data.Maybe
import Data.Foldable (foldr, intercalate)
import Data.Generic

import Data.PrettyPrint
import Data.Name

type Definition =
  { name   :: Name
  , args   :: Array Name
  , syntax :: Syntax
  }

defToSyntax :: Definition -> Syntax
defToSyntax def = foldr Lambda def.syntax def.args

defToString :: Definition -> String
defToString def = show def.name <> prettyArgs def.args <> " = " <> prettyPrint def.syntax
 where
  prettyArgs [] = ""
  prettyArgs as = " " <> intercalate " " (map show as)

data Syntax
  = Var Name
  | Lambda Name Syntax
  | Apply Syntax Syntax

derive instance genericSyntax :: Generic Syntax
instance showSyntax :: Show Syntax where
  show = gShow

isComposite :: Syntax -> Boolean
isComposite (Var _) = false
isComposite _       = true

isLambda :: Syntax -> Boolean
isLambda (Lambda _ _) = true
isLambda _            = false

instance prettyPrintSyntax :: PrettyPrint Syntax where
  prettyPrint = walk
   where
    walk e =
      case e of
        Var v ->
          show v
        Lambda n b ->
          "λ" <> show n <> ". " <> walk b
        Apply f a ->
          parensIf (isLambda f) (walk f) <> " " <> parensIf (isComposite a) (walk a)

