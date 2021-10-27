module Effect.QueryParams
  ( get
  ) where

import Lambda.Prelude

import Lambda.Language.Parser (class Parse, parse, run)

-- | Attempt to parse value from query string
get :: forall a. Parse a => String -> Effect (Maybe a)
get = getParamImpl Nothing (hush <<< run parse)

foreign import getParamImpl
  :: forall a
   . Maybe a
  -> (String -> Maybe a)
  -> String
  -> Effect (Maybe a)
