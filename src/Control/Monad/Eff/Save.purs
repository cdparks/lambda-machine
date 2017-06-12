module Control.Monad.Eff.Save
  ( FileName(..)
  , SAVE(..)
  , saveTextAs
  ) where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)

type FileName = String

foreign import data SAVE :: Effect

foreign import saveTextAs :: forall e. String -> FileName -> Eff (save :: SAVE | e) Unit

