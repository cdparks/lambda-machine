module Effect.Save
  ( FileName(..)
  , saveTextAs
  ) where

import Lambda.Prelude

type FileName = String
foreign import saveTextAs :: String -> FileName -> Effect Unit
