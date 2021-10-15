module Lambda.Language.Snapshot
  ( Snapshot(..)
  , from
  , to
  ) where

import Lambda.Prelude

import Lambda.Language.Definition (Definition)
import Lambda.Language.Expression (Expression)

-- | Will probably want a more compact representation
newtype Snapshot = Snapshot
  { defs :: Array Definition
  , input :: Maybe Expression
  }

derive newtype instance eqSnapshot :: Eq Snapshot
derive newtype instance showSnapshot :: Show Snapshot
derive instance newtypeSnapshot :: Newtype Snapshot _
derive newtype instance readForeignSnapshot :: ReadForeign Snapshot
derive newtype instance writeForeignSnapshot :: WriteForeign Snapshot

from :: Array Definition -> Maybe Expression -> Snapshot
from defs input = coerce { defs, input }

to :: Snapshot -> { defs :: Array Definition, input :: Maybe Expression }
to = coerce
