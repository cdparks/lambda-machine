module Component.Event
  ( getValue
  , getKeyCode
  ) where

import Prelude ((>>>))

import Data.Maybe (fromMaybe)
import Data.Foreign (toForeign)
import Data.Foreign.Lens (int, prop, string)
import Data.Lens.Fold (preview)

getValue :: forall event. event -> String
getValue = toForeign >>> preview (string >>> prop "value" >>> prop "target") >>> fromMaybe ""

getKeyCode :: forall event. event -> Int
getKeyCode = toForeign >>> preview (int >>> prop "keyCode") >>> fromMaybe 0
