module Lambda.Prelude
  ( module Prelude
  , module X
  ) where

import Prelude

import Control.Alt ((<|>)) as X
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM) as X
import Control.Monad.State (class MonadState, evalState, execState, gets, get, modify_, put) as X
import Control.MonadZero (guard) as X
import Data.Either (Either(..), either) as X
import Data.Foldable (class Foldable, fold, foldr, foldl, foldM, foldMap, for_, traverse_) as X
import Data.Generic.Rep (class Generic) as X
import Data.Generic.Rep.Eq (genericEq) as X
import Data.Generic.Rep.Ord (genericCompare) as X
import Data.Generic.Rep.Show (genericShow) as X
import Data.HashMap (HashMap) as X
import Data.HashSet (HashSet) as X
import Data.Hashable (class Hashable, hash) as X
import Data.List (List(..), (:)) as X
import Data.Map (Map) as X
import Data.Maybe (Maybe(..), fromMaybe, maybe) as X
import Data.Newtype (class Newtype, un, wrap) as X
import Data.Set (Set) as X
import Data.Traversable (class Traversable, traverse, for) as X
import Data.Tuple (Tuple(..), uncurry, fst, snd) as X
import Data.Tuple.Nested ((/\)) as X
import Data.Unfoldable (class Unfoldable) as X
import Debug.Trace (trace, traceM, spy) as X
import Effect (Effect) as X
