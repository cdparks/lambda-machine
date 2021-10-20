{-# LANGUAGE QuasiQuotes #-}

module Backend.ApiSpec
  ( spec
  ) where

import Backend.Test.Prelude

import Backend.Api (api)
import qualified Backend.App as App
import Backend.Middleware (middleware)
import qualified Backend.Settings as Settings
import LoadEnv (loadEnvFrom)

spec :: Spec
spec = with app $ do
  describe "GET /snapshots/:code" $ do
    it "rejects a code that's too short" $ do
      get "/snapshots/ABCDEFG" `shouldRespondWith` status 400 [json|
        { status: 400
        , error: "Bad Request"
        , detail: "wrong length for code: ABCDEFG"
        }
      |]

    it "rejects a code that's too long" $ do
      get "/snapshots/ABCDEFGHJ" `shouldRespondWith` status 400 [json|
        { status: 400
        , error: "Bad Request"
        , detail: "wrong length for code: ABCDEFGHJ"
        }
      |]

    it "rejects a code with illegal characters" $ do
      get "/snapshots/ABCDEFGI" `shouldRespondWith` status 400 [json|
        { status: 400
        , error: "Bad Request"
        , detail: "unrecognized characters in code: ABCDEFGI"
        }
      |]

    it "404s on a nonexistent snapshot" $ do
      get "/snapshots/N0TTHERE" `shouldRespondWith` status 404 [json|
        { status: 404
        , error: "Not Found"
        }
      |]

    it "returns on a snapshot" $ do
      let expected = [json|{sig: 0, names: [], state: []}|]
      get "/snapshots/SNAPSH0T" `shouldRespondWith` expected

  describe "POST /snapshots" $ do
    it "requires a JSON body" $ do
      post "/snapshots" "" `shouldRespondWith` status 400 [json|
        { status: 400
        , error: "Bad Request"
        , detail: "Error in $: not enough input"
        }
      |]

    it "rejects a malformed JSON body" $ do
      let body = [json|{sig: 0, names: []}|]
      post "/snapshots" body `shouldRespondWith` status 400 [json|
        { status: 400
        , error: "Bad Request"
        , detail: "Error in $: parsing Backend.Snapshot.ApiSnapshot(ApiSnapshot) failed, key \"state\" not found"
        }
      |]

    it "accepts a valid JSON body" $ do
      let body = [json|{sig: 0, state: [], names: []}|]
      post "/snapshots" body `shouldRespondWith` 200

app :: IO Application
app = do
  loadEnvFrom ".env.test"
  settings <- Settings.load
  App.run settings $ \env -> runRIO env $ middleware settings <$> api

status :: Int -> ResponseMatcher -> ResponseMatcher
status n m = m { matchStatus = n }
