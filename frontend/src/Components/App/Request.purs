module Components.App.Request
  ( Request(..)
  ) where

import Lambda.Prelude

import Lambda.Language.Snapshot.Code (Code)

-- | Actions that must be performed asynchronously
data Request
  = Fetch Code
  | Store
  | Save

derive instance eqRequest :: Eq Request
