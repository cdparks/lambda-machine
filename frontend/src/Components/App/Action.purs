module Components.App.Action
  ( Action(..)
  ) where

import Components.App.Request (Request)
import Components.App.Response (Response)
import Lambda.Language.Name (Name)

-- | Set of user-driven events
data Action
  = Help
  | Dismiss
  | Update String
  | Parse
  | Delete Name
  | Step
  | Clear
  | Toggle
  | Enqueue Request
  | Examine Response
