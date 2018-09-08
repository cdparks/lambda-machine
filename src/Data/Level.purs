module Data.Level
  ( Level(..)
  , warn
  , danger
  ) where

import Prelude (class Show)

import Data.Tuple (Tuple(..))

data Level = Warning | Danger

instance showLevel :: Show Level where
  show Warning = "warning"
  show Danger  = "danger"

warn :: String -> Tuple Level String
warn = Tuple Warning

danger :: String -> Tuple Level String
danger = Tuple Danger
