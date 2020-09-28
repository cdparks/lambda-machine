module Language.Param
  ( Param
  , lazy
  , strict
  , unwrap
  , rename
  , isStrict
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Language.Name (Name)
import Language.PrettyPrint (class PrettyPrint)

data Param
  = Lazy Name
  | Strict Name

derive instance genericParam :: Generic Param _

instance showParam :: Show Param where
  show x = genericShow x

instance prettyPrintParam :: PrettyPrint Param where
  prettyPrint (Lazy name) = pure $ show name
  prettyPrint (Strict name) = pure $ "!" <> show name

lazy :: Name -> Param
lazy = Lazy

strict :: Name -> Param
strict = Strict

rename :: Param -> Name -> Param
rename (Lazy _) n = Lazy n
rename (Strict _) n = Strict n

unwrap :: Param -> Name
unwrap (Lazy n) = n
unwrap (Strict n) = n

isStrict :: Param -> Boolean
isStrict (Strict _) = true
isStrict _ = false
