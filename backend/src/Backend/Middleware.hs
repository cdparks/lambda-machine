module Backend.Middleware
  ( middleware
  ) where

import Backend.Prelude

import Backend.Settings (Settings(..))
import Backend.Wai (Error(..), errorResponse)
import Network.Wai.Middleware.Autohead (autohead)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleMethods)
import Network.Wai.Middleware.Gzip (def, gzip)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.RequestSizeLimit
  ( defaultRequestSizeLimitSettings
  , requestSizeLimitMiddleware
  , setOnLengthExceeded
  )
import Network.Wai.Middleware.Timeout (timeoutAs)

-- | Enable CORS, logging, compression, and limits
middleware :: Settings -> Middleware
middleware Settings {..} =
  cors (corsPolicy origin)
    . autohead
    . logRequests
    . gzip def
    . limitRequestSize
    . timeoutRequest timeout
 where
  logRequests
    | logLevel == LevelDebug = logStdoutDev
    | otherwise = id

-- | Limit request size to default 2MB and respond with JSON
limitRequestSize :: Middleware
limitRequestSize = requestSizeLimitMiddleware
  $ setOnLengthExceeded handler defaultRequestSizeLimitSettings
 where
  handler bytes _ _ respond = respond $ errorResponse Error
    { errHeaders = []
    , errStatus = status413
    , errDetail =
      Just $ "request entity larger than " <> tshow bytes <> " bytes"
    }

-- | Limit request time and respond with JSON
timeoutRequest :: Int -> Middleware
timeoutRequest = timeoutAs $ errorResponse Error
  { errHeaders = []
  , errStatus = status503
  , errDetail = Nothing
  }

-- | Allow GET and JSON POST from specified origin
corsPolicy :: Text -> Request -> Maybe CorsResourcePolicy
corsPolicy origin _ = pure CorsResourcePolicy
  { corsOrigins = Just ([encodeUtf8 origin], True)
  , corsMethods = simpleMethods
  , corsRequestHeaders = [hContentType]
  , corsExposedHeaders = Nothing
  , corsMaxAge = Nothing
  , corsVaryOrigin = True
  , corsRequireOrigin = True
  , corsIgnoreFailures = False
  }
