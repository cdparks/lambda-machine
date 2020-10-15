module Lambda.Machine.Trace
  ( Trace(..)
  , roots
  ) where

import Lambda.Prelude

import Lambda.Language.Name (Name)
import Lambda.Machine.Address (Address)

data Trace
  = Start
  | Unwound Address
  | Followed Address
  | Fetched Name
  | Instantiated Address
  | Substituted Name Address
  | WentUnder Address
  | Discarded Address Address
  | Halted Address

derive instance genericTrace :: Generic Trace _

instance showTrace :: Show Trace where
  show x = genericShow x

roots :: Trace -> Array Address
roots = case _ of
  Start -> []
  Unwound addr -> [addr]
  Followed addr -> [addr]
  Fetched _ -> []
  Instantiated addr -> [addr]
  Substituted _ addr -> [addr]
  WentUnder addr -> [addr]
  Discarded lhs rhs -> [lhs, rhs]
  Halted addr -> [addr]