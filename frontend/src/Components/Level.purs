module Components.Level
  ( Level(..)
  ) where

import Lambda.Prelude

data Level
  = Default
  | Primary
  | Success
  | Info
  | Warning
  | Danger

instance showLevel :: Show Level where
  show Default = "default"
  show Primary = "primary"
  show Success = "success"
  show Info = "info"
  show Warning = "warning"
  show Danger = "danger"
