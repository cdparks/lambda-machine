module Lambda.Prelude
  ( module Prelude
  , module X
  , withReader
  , withState
  , runIdentity
  , map2
  , (<$$>)
  ) where

import Prelude

import Control.Alt ((<|>)) as X
import Control.Monad.Error.Class (class MonadThrow, class MonadError, throwError, catchError) as X
import Control.Monad.Except.Trans (ExceptT, runExceptT) as X
import Control.Monad.Reader (Reader, runReader) as X
import Control.Monad.Reader.Class (class MonadAsk, class MonadReader, ask, asks, local) as X
import Control.Monad.Reader.Trans (ReaderT, runReaderT) as X
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM) as X
import Control.Monad.State (State, evalState, execState, runState) as X
import Control.Monad.State.Class (class MonadState, gets, get, modify_, put) as X
import Control.Monad.State.Trans (StateT, evalStateT, execStateT, runStateT) as X
import Control.MonadZero (guard) as X
import Data.Argonaut.Core (stringify) as X
import Data.Argonaut.Decode (class DecodeJson, decodeJson, JsonDecodeError(..), printJsonDecodeError) as X
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as X
import Data.Bifunctor (class Bifunctor, bimap, lmap, rmap) as X
import Data.Either (Either(..), either, hush, note) as X
import Data.Foldable (class Foldable, fold, foldr, foldl, foldM, foldMap, for_, traverse_, sequence_) as X
import Data.Generic.Rep (class Generic) as X
import Data.HashMap (HashMap) as X
import Data.HashSet (HashSet) as X
import Data.Hashable (class Hashable, hash) as X
import Data.Identity (Identity(..)) as X
import Data.List (List(..), (:)) as X
import Data.Map (Map) as X
import Data.Maybe (Maybe(..), fromMaybe', fromMaybe, maybe, maybe', isJust, isNothing) as X
import Data.Monoid (class Monoid, mempty) as X
import Data.Newtype (class Newtype, un, wrap, unwrap) as X
import Data.Semigroup (class Semigroup, append, (<>)) as X
import Data.Set (Set) as X
import Data.Show.Generic (genericShow) as X
import Data.Traversable (class Traversable, traverse, for) as X
import Data.Tuple (Tuple(..), uncurry, fst, snd) as X
import Data.Tuple.Nested ((/\)) as X
import Data.Unfoldable (class Unfoldable, replicate, replicateA) as X
import Debug (trace, traceM, spy, debugger) as X
import Effect (Effect) as X
import Effect.Aff (Aff) as X
import Effect.Class (liftEffect) as X
import Safe.Coerce (class Coercible, coerce) as X

--- | Flipped runReader
withReader :: forall a r. r -> X.Reader r a -> a
withReader = flip X.runReader

--- | Flipped evalState
withState :: forall a s. s -> X.State s a -> a
withState = flip X.evalState

-- | Not included in Data.Identity anymore
runIdentity :: forall a. X.Identity a -> a
runIdentity = X.unwrap

map2 :: forall a b f g. Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
map2 f = map (map f)
infixl 4 map2 as <$$>
