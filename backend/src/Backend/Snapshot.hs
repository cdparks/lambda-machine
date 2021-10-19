{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Snapshot
  ( Snapshot(..)
  , ApiSnapshot(..)
  , fetch
  , store
  ) where

import Backend.Prelude

import Backend.Code (Code)
import qualified Backend.Code as Code
import Backend.Database (HasSqlPool, get404, runDB, tryInsertKey)
import Backend.Micro (internalError)
import Backend.Name (Name)
import Backend.Random (HasRandom)
import Backend.Signature (Signature)
import Database.Persist.TH (mkPersist, persistLowerCase, sqlSettings)

-- | API-level 'Snapshot'
data ApiSnapshot = ApiSnapshot
  { sig :: Signature
  , names :: [Name]
  , state :: [Int32]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkPersist sqlSettings [persistLowerCase|
Snapshot sql=snapshots
  Id Code
  signature Signature
  names (JSONB [Name])
  state (JSONB [Int32])
|]

-- | Fetch 'ApiSnapshot' by 'Code'
fetch :: MonadUnliftIO m => Code -> SqlPersistT m ApiSnapshot
fetch key = do
  Snapshot {..} <- get404 $ SnapshotKey key
  pure ApiSnapshot
    { sig = snapshotSignature
    , names = unJSONB snapshotNames
    , state = unJSONB snapshotState
    }

-- | Store 'ApiSnapshot' returning 'Code'
--
-- Runs in 'RIO' instead of 'SqlPersistT' since 'tryInsertKey' uses
-- exceptions to detect unique violations.
--
store :: (HasSqlPool env, HasRandom env) => ApiSnapshot -> RIO env Code
store ApiSnapshot {..} = unSnapshotKey <$> loop 0
 where
  loop n
    | n >= maxAttempts = internalError message
    | otherwise = do
      key <- SnapshotKey <$> Code.new
      result <- runDB $ tryInsertKey key snapshot
      maybe (loop $ n + 1) pure result

  snapshot = Snapshot sig (JSONB names) (JSONB state)
  maxAttempts = 10 :: Int
  message = mconcat
    ["Failed to generate unique code in ", tshow maxAttempts, " attempts"]
