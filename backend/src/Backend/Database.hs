module Backend.Database
  ( module X
  , HasSqlPool(..)
  , SqlPool
  , PostgresConf(..)
  , ConnectionString
  , newSqlPool
  , runDB
  , tryInsertKey
  , get404
  ) where

import Backend.Prelude

import Backend.Micro (notFound)
import Database.Persist as X
import Database.Persist.Postgresql
  (ConnectionString, PostgresConf(..), createPostgresqlPool)
import Database.Persist.Sql (runSqlPool)
import Database.PostgreSQL.Simple (SqlError(..))

-- | 'SqlPool' == 'Pool' 'SqlBackend' == 'ConnectionPool'
type SqlPool = ConnectionPool

class HasSqlPool env where
  sqlPoolLens :: Lens' env SqlPool

instance HasSqlPool SqlPool where
  sqlPoolLens = id
  {-# INLINE sqlPoolLens #-}

-- | Execute a query using the sql pool from the ambient environment
runDB
  :: forall env a m
   . (MonadUnliftIO m, MonadReader env m, HasSqlPool env)
  => SqlPersistT m a
  -> m a
runDB act = runSqlPool act =<< view sqlPoolLens

-- | Create a new connection pool
newSqlPool
  :: forall m . (MonadUnliftIO m, MonadLoggerIO m) => PostgresConf -> m SqlPool
newSqlPool PostgresConf {..} = createPostgresqlPool pgConnStr pgPoolSize

-- | Attempt to insert an entity whose key may not be unique
--
-- Used for entities that use randomly generated keys that are likely
-- but not guaranteed to be unique. Returns 'Nothing' if the insertion
-- failed.
--
tryInsertKey
  :: ( MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ SqlBackend
     )
  => Key record
  -> record
  -> SqlPersistT m (Maybe (Key record))
tryInsertKey key record = do
  result <- try $ insertKey key record
  case result of
    Right{} -> pure $ Just key
    Left SqlError { sqlState = "23505" } -> pure Nothing
    Left e -> throwIO e

-- | Fetch an entity or throw a 404
get404
  :: ( MonadUnliftIO m
     , PersistEntity record
     , PersistEntityBackend record ~ SqlBackend
     )
  => Key record
  -> SqlPersistT m record
get404 = maybe notFound pure <=< get
