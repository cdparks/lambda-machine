module Effect.Save
  ( FileName(..)
  , saveTextAs
  ) where

import Lambda.Prelude

newtype FileName = FileName String

-- | Save text to file
foreign import saveTextAs :: String -> FileName -> Effect Unit
