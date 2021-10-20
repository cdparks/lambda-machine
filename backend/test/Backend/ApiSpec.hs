{-# LANGUAGE QuasiQuotes #-}

module Backend.ApiSpec
  ( spec
  ) where

import Backend.Test.Prelude

import Backend.Api (api)
import qualified Backend.Env as Env
import Backend.Env (Env)
import Backend.Database (deleteWhere, insertKey, runDB)
import qualified Backend.Database as DB
import Backend.Middleware (middleware)
import qualified Backend.Settings as Settings
import Backend.Snapshot (Key(SnapshotKey), Snapshot)
import LoadEnv (loadEnvFrom)

spec :: Spec
spec = withState testApp $ do
  describe "GET /snapshots/:code" $ do
    it "rejects a code that's too short" $ do
      get "/snapshots/ABCDEFG" `shouldRespondWith` status
        400
        [json|
          { status: 400
          , error: "Bad Request"
          , detail: "wrong length for code: ABCDEFG"
          }
        |]

    it "rejects a code that's too long" $ do
      get "/snapshots/ABCDEFGHJ" `shouldRespondWith` status
        400
        [json|
          { status: 400
          , error: "Bad Request"
          , detail: "wrong length for code: ABCDEFGHJ"
          }
        |]

    it "rejects a code with illegal characters" $ do
      get "/snapshots/ABCDEFGI" `shouldRespondWith` status
        400
        [json|
          { status: 400
          , error: "Bad Request"
          , detail: "unrecognized characters in code: ABCDEFGI"
          }
        |]

    it "404s on a nonexistent snapshot" $ do
      get "/snapshots/N0TTHERE" `shouldRespondWith` status
        404
        [json|
          { status: 404
          , error: "Not Found"
          }
        |]

    it "returns an extant snapshot" $ do
      get "/snapshots/SNAPSH0T" `shouldRespondWith` [json|
        { sig: 0
        , names: []
        , state: []
        }
      |]

  describe "POST /snapshots" $ do
    it "requires a JSON body" $ do
      post "/snapshots" "" `shouldRespondWith` status
        400
        [json|
          { status: 400
          , error: "Bad Request"
          , detail: "Error in $: not enough input"
          }
        |]

    it "rejects a malformed JSON body" $ do
      let body = [json|{sig: 0, names: []}|]
      post "/snapshots" body `shouldRespondWith` status
        400
        [json|
          { status: 400
          , error: "Bad Request"
          , detail: "Error in $: parsing Backend.Snapshot.ApiSnapshot(ApiSnapshot) failed, key \"state\" not found"
          }
        |]

    it "accepts and stores a valid snapshot" $ do
      SResponse {..} <- post "/snapshots" [json|
        { sig: 0
        , state: []
        , names: []
        }
      |]
      code <- shouldBeJust $ simpleBody ^? key "code" . _JSON
      snapshot <- liftEnv $ runDB $ DB.get $ SnapshotKey code
      liftIO $ snapshot `shouldBe` Just def

testApp :: IO (Env, Application)
testApp = do
  loadEnvFrom ".env.test"
  settings <- Settings.load
  env <- Env.new settings
  runRIO env $ do
    runDB $ do
      deleteWhere @_ @_ @Snapshot []
      insertKey (SnapshotKey "SNAPSH0T") def
    app <- middleware settings <$> api
    pure (env, app)

status :: Int -> ResponseMatcher -> ResponseMatcher
status n m = m { matchStatus = n }
