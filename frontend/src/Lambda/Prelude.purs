module Lambda.Prelude
  ( module Prelude
  , module X
  , withReader
  , withState
  ) where

import Prelude

import Control.Alt ((<|>)) as X
import Control.Monad.Reader (class MonadAsk, class MonadReader, Reader, runReader, ask, asks, local) as X
import Control.Monad.Reader (Reader, runReader)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM) as X
import Control.Monad.State (class MonadState, State, evalState, execState, gets, get, modify_, put) as X
import Control.Monad.State (State, evalState)
import Control.MonadZero (guard) as X
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as X
import Data.Either (Either(..), either, hush, note) as X
import Data.Foldable (class Foldable, fold, foldr, foldl, foldM, foldMap, for_, traverse_) as X
import Data.Generic.Rep (class Generic) as X
import Data.HashMap (HashMap) as X
import Data.HashSet (HashSet) as X
import Data.Hashable (class Hashable, hash) as X
import Data.List (List(..), (:)) as X
import Data.Map (Map) as X
import Data.Maybe (Maybe(..), fromMaybe', fromMaybe, maybe, isNothing) as X
import Data.Monoid (class Monoid, mempty) as X
import Data.Newtype (class Newtype, un, wrap) as X
import Data.Semigroup (class Semigroup, append, (<>)) as X
import Data.Set (Set) as X
import Data.Show.Generic (genericShow) as X
import Data.Traversable (class Traversable, traverse, for) as X
import Data.Tuple (Tuple(..), uncurry, fst, snd) as X
import Data.Tuple.Nested ((/\)) as X
import Data.Unfoldable (class Unfoldable) as X
import Debug (trace, traceM, spy, debugger) as X
import Effect (Effect) as X
import Foreign (F, Foreign, ForeignError(..)) as X
import Safe.Coerce (class Coercible, coerce) as X
import Simple.JSON (readJSON, writeJSON, class ReadForeign, readImpl, class WriteForeign, writeImpl) as X

-- | Flipped runReader
withReader :: forall a r. r -> Reader r a -> a
withReader = flip runReader

-- | Flipped evalState
withState :: forall a s. s -> State s a -> a
withState = flip evalState
