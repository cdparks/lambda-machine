module Backend.Test.Prelude
  ( module X
  , shouldBeJust
  , liftEnv
  , get
  , post
  , SResponse(..)
  ) where

import Backend.Prelude as X
import Data.Aeson.Lens as X
import Network.Wai.Test (SResponse(..))
import Test.Hspec as X hiding
  ( expectationFailure
  , shouldBe
  , shouldContain
  , shouldEndWith
  , shouldMatchList
  , shouldNotBe
  , shouldNotContain
  , shouldNotReturn
  , shouldNotSatisfy
  , shouldReturn
  , shouldSatisfy
  , shouldStartWith
  , shouldThrow
  )
import Test.Hspec.Expectations.Lifted as X
import Test.Hspec.Wai as X hiding (get, pending, pendingWith, post)
import Test.Hspec.Wai.JSON as X
import Test.HUnit.Lang (assertFailure)

-- | Unwrap 'Maybe' to 'Just' or fail the test
shouldBeJust :: (HasCallStack, MonadIO m) => Maybe a -> m a
shouldBeJust = maybe (liftIO $ assertFailure "Found Nothing instead of Just") pure

-- | Lift an action on the application environment to 'WaiSession'
liftEnv :: RIO env a -> WaiSession env a
liftEnv act = do
  env <- getState
  liftIO $ runRIO env act

-- | 'get' with correct CORS and Accept headers
get :: ByteString -> WaiSession st SResponse
get path = X.request methodGet path headers ""

-- | 'post' with correct CORS, Accept, and Content-Type headers
post :: ByteString -> LByteString -> WaiSession st SResponse
post path = X.request methodPost path $ contentType : headers
  where contentType = (hContentType, "application/json")

-- | Shared headers
headers :: [Header]
headers = [(hOrigin, "x"), (hAccept, "application/json")]
