module Components.App.Response
  ( Response(..)
  ) where

import Lambda.Api as Api
import Lambda.Language.Program (Program)
import Lambda.Language.Snapshot.Code (Code)

-- | Result of handling an asynchronous request
data Response
  = Fetched Program
  | Stored Code
  | Saved
  | ApiError Api.Error
  | SaveError String
