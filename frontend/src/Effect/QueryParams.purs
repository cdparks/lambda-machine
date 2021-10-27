module Effect.QueryParams
  ( get
  , getWith
  ) where

import Lambda.Prelude

import Lambda.Language.Parser (class Parse, parse, run)

-- | Attempt to parse value from query string
get :: forall a. Parse a => String -> Effect (Maybe a)
get = getWith $ hush <<< run parse

-- | Attempt to parse value from query string given a parser
getWith :: forall a. (String -> Maybe a) -> String -> Effect (Maybe a)
getWith = getParamImpl Nothing

foreign import getParamImpl
  :: forall a
   . Maybe a
  -> (String -> Maybe a)
  -> String
  -> Effect (Maybe a)
