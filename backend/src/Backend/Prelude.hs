module Backend.Prelude
  ( module X
  ) where

import RIO as X hiding (Handler, timeout)

import Control.Error.Util as X (hush, note)
import Control.Monad as X (replicateM)
import Control.Monad.Logger as X
  ( MonadLogger
  , MonadLoggerIO
  , runNoLoggingT
  , runStderrLoggingT
  , runStdoutLoggingT
  )
import Control.Monad.State as X
  (MonadState, State, evalState, execState, modify, runState)
import Data.Aeson as X
  ( (.:)
  , (.=)
  , FromJSON(..)
  , KeyValue
  , ToJSON(..)
  , decode
  , eitherDecode
  , encode
  , object
  , pairs
  , withObject
  , withText
  )
import Data.Coerce as X (Coercible, coerce)
import Data.Default as X
import Data.Kind as X (Constraint, Type)
import Data.Text as X (pack, unpack)
import Data.Text.Encoding as X (decodeUtf8)
import Database.Esqueleto.PostgreSQL.JSON as X (JSONB(..))
import Database.Persist as X
  (Entity(..), Key, PersistEntity(..), PersistEntityBackend, PersistField)
import Database.Persist.Sql as X
  (ConnectionPool, PersistFieldSql, SqlBackend, SqlPersistT)
import GHC.TypeLits as X (KnownNat, KnownSymbol, Nat, Symbol, natVal, symbolVal)
import Network.HTTP.Types.Header as X
import Network.HTTP.Types.Method as X
import Network.HTTP.Types.Status as X
import Network.Wai as X
  (Application, Middleware, Request, Response, responseLBS)
import RIO.Orphans as X ()
import RIO.Seq as X ((<|), (|>))
import Web.HttpApiData as X (FromHttpApiData(..), ToHttpApiData(..))
import Web.PathPieces as X (PathPiece(..))
