module Data.Syntax
  ( Name(..)
  , Definition(..)
  , Syntax(..)
  ) where

import Prelude
import Data.Maybe
import Data.Generic

import Data.PrettyPrint

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
          v
        Lambda n b ->
          "λ" <> n <> ". " <> walk b
        Apply f a ->
          parensIf (isLambda f) (walk f) <> " " <> parensIf (isComposite a) (walk a)

