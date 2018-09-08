module Effect.Save
  ( FileName(..)
  , saveTextAs
  ) where

import Prelude
import Effect (Effect)

type FileName = String

foreign import saveTextAs :: String -> FileName -> Effect Unit
