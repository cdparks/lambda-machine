module Backend.Api
  ( api
  ) where

import Backend.Prelude

import Backend.Database (HasSqlPool, runDB)
import Backend.Envelope (Envelope(..))
import Backend.Micro ((//), get, param, post, root, run)
import Backend.Random (HasRandom)
import qualified Backend.Snapshot as Snapshot

-- | Generate WAI 'Application'
api :: (HasSqlPool env, HasRandom env) => RIO env Application
api = run $ do
  get root $ pure $ object []
  post "snapshots" $ fmap (Envelope @"code") . Snapshot.store
  get ("snapshots" // param) $ runDB . Snapshot.fetch
