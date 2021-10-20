module Backend.Test.Prelude
  ( module X
  , get
  , post
  ) where

import Backend.Prelude as X
import Test.Hspec as X
import Test.Hspec.Wai as X hiding (get, post, pending, pendingWith)
import Network.Wai.Test (SResponse)
import Test.Hspec.Wai.JSON as X

get :: ByteString -> WaiSession st SResponse
get path = X.request methodGet path headers ""

post :: ByteString -> LByteString -> WaiSession st SResponse
post path = X.request methodPost path $ contentType : headers
 where
  contentType = (hContentType, "application/json")

headers :: [Header]
headers = [(hOrigin, "x"), (hAccept, "application/json")]
