{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Backend.Snapshot
  ( ApiSnapshot(..)
  , Snapshot(..)
  , SnapshotId
  , EntityField(..)
  , Key(SnapshotKey)
  , fetch
  , store
  ) where

import Backend.Prelude

import Backend.Code (Code)
import qualified Backend.Code as Code
import Backend.Database (HasSqlPool, get404, getBy, runDB, tryInsertKey)
import Backend.Micro (internalError)
import Backend.Name (Name)
import Backend.Random (HasRandom)
import Backend.Signature (Signature)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Database.Persist.TH (mkPersist, persistLowerCase, sqlSettings)

-- | API-level 'Snapshot'
data ApiSnapshot = ApiSnapshot
  { sig :: Signature
  , names :: [Name]
  , state :: [Int32]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Default)

mkPersist sqlSettings [persistLowerCase|
Snapshot sql=snapshots
  Id Code
  signature Signature
  names (JSONB [Name])
  state (JSONB [Int32])
  UniqueSnapshot signature names state
  deriving Eq Show
|]

instance Default Snapshot where
  def = Snapshot def (JSONB []) (JSONB [])

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
-- Attempts to find an identical 'Snapshot' before storing the new one.
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
      result <- runDB $ runMaybeT $ asum
        [ MaybeT $ entityKey <$$> getBy uniqueKey
        , MaybeT $ tryInsertKey key snapshot
        ]
      maybe (loop $ n + 1) pure result

  uniqueKey = UniqueSnapshot sig (JSONB names) (JSONB state)
  snapshot = Snapshot sig (JSONB names) (JSONB state)

  maxAttempts = 10 :: Int
  message = mconcat
    ["Failed to generate unique code in ", tshow maxAttempts, " attempts"]
