module Lambda.Env
  ( api
  , host
  ) where

-- | Get api url from environment or default to the real thing
foreign import api :: String

-- | Get app host from environment or default to the real thing
foreign import host :: String
