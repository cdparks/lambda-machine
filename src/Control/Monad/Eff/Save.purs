module Control.Monad.Eff.Save where

import Prelude
import Control.Monad.Eff

type FileName = String

foreign import data SAVE :: !

foreign import saveTextAs :: forall e. String -> FileName -> Eff (save :: SAVE | e) Unit

