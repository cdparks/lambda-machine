module Components.App.Alert
  ( Alert(..)
  , Error(..)
  ) where

import Lambda.Api as Api
import Lambda.Language.Parser (ParseError)
import Lambda.Language.Snapshot.Code (Code)
import Lambda.Language.World (ConsistencyError)

-- | Interrupt the main view
data Alert
  = Help
  | Link Code
  | Error Error

-- | Error messages
data Error
  = ApiError Api.Error
  | SaveError String
  | ParseError String ParseError
  | Inconsistent ConsistencyError
