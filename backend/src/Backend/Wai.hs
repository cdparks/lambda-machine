module Backend.Wai
  ( Error(..)
  , jsonResponse
  , jsonResponseWith
  , errorResponse
  ) where

import Backend.Prelude
import Network.Wai (mapResponseHeaders)

-- | Return JSON bytestring
jsonResponse :: ToJSON a => a -> Response
jsonResponse = jsonResponseWith ok200

-- | Return JSON bytestring with 'Status'
jsonResponseWith :: ToJSON a => Status -> a -> Response
jsonResponseWith status = responseLBS status headers . encode
  where headers = [(hContentType, "application/json")]

-- | Return 'Error' as JSON
errorResponse :: Error -> Response
errorResponse err@Error {..} =
  mapResponseHeaders (<> errHeaders) $ jsonResponseWith errStatus err

-- | Generic server error
data Error = Error
  { errHeaders :: [Header]
  -- ^ Headers to add to response
  , errStatus :: Status
  -- ^ Status to return
  , errDetail :: Maybe Text
  -- ^ Optional error detail, e.g. a parse failure
  }
  deriving stock Show

instance Exception Error

instance ToJSON Error where
  toEncoding = pairs . mconcat . toPairs
  toJSON = object . toPairs

toPairs :: forall kv . KeyValue kv => Error -> [kv]
toPairs (Error _ Status {..} mDetail) = mconcat
  [ ["status" .= statusCode, "error" .= decodeUtf8 statusMessage]
  , [ "detail" .= detail | detail <- maybeToList mDetail ]
  ]
