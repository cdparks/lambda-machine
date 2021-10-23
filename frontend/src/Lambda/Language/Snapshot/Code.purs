module Lambda.Language.Snapshot.Code
  ( Code(..)
  ) where

import Lambda.Prelude

-- | Identifies a snapshot
newtype Code = Code String

derive instance newtypeCode :: Newtype Code _
derive newtype instance eqCode :: Eq Code
derive newtype instance showCode :: Show Code
derive newtype instance readForeignCode :: ReadForeign Code
derive newtype instance writeForeignCode :: WriteForeign Code
