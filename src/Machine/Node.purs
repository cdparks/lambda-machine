module Machine.Node
  ( Node(..)
  , Stuck(..)
  , Env
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Language.Expr (Expr)
import Language.Name (Name)
import Machine.Address (Address)

data Node
  = Node Address Address
  | Closure Env Name Expr
  | Global Name Address
  | Stuck Stuck
  | Pointer Address

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show x = genericShow x

data Stuck
  = StuckVar Name
  | StuckBind Name Address
  | StuckApp Address Address

derive instance genericStuck :: Generic Stuck _

instance showStuck :: Show Stuck where
  show x = genericShow x

type Env = List Address
