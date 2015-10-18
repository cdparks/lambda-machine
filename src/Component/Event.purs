module Component.Event
  ( getValue
  , getKeyCode
  ) where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Nullable (toMaybe)
import Data.Foreign (toForeign)
import Data.Foreign.Lens

getValue :: forall event. event -> String
getValue = toForeign >>> get (string >>> prop "value" >>> prop "target") >>> fromMaybe ""

getKeyCode :: forall event. event -> Int
getKeyCode = toForeign >>> get (int >>> prop "keyCode") >>> fromMaybe 0

