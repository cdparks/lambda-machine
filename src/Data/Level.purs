module Data.Level
  ( Level(..)
  ) where

import Prelude

data Level = Info | Success | Warning | Danger

instance showLevel :: Show Level where
  show Info = "info"
  show Success = "success"
  show Warning = "warning"
  show Danger  = "danger"
