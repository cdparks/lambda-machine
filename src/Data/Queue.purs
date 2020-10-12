module Data.Queue
  ( Queue
  , fromFoldable
  , toUnfoldable
  , empty
  , singleton
  , pop
  , push
  , extend
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldr, foldl, foldMap)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable, traverse, sequenceDefault)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (class Unfoldable)

newtype Queue a = Queue
  { front :: List a
  , back :: List a
  }

derive instance newtypeQueue :: Newtype (Queue a) _

instance showQueue :: Show a => Show (Queue a) where
  show q = "fromFoldable " <> show (Array.fromFoldable q)

instance eqQueue :: Eq a => Eq (Queue a) where
  eq lhs rhs = eq (toList lhs) (toList rhs)

instance functorQueue :: Functor Queue where
  map f (Queue {front, back}) = Queue
    { front: f <$> front
    , back: f <$> back
    }

instance foldableQueue :: Foldable Queue where
  foldr f z = foldr f z <<< toList
  foldl f z = foldl f z <<< toList
  foldMap f = foldMap f <<< toList

instance traversableQueue :: Traversable Queue where
  traverse f = map mk <<< traverse f <<< toList
   where
    mk front = Queue { front, back: Nil }
  sequence = sequenceDefault

fromFoldable :: forall a f. Foldable f => f a -> Queue a
fromFoldable xs = Queue { front: List.fromFoldable xs, back: Nil }

toUnfoldable :: forall a f. Unfoldable f => Queue a -> f a
toUnfoldable = List.toUnfoldable <<< toList

empty :: forall a. Queue a
empty = Queue {front: Nil, back: Nil}

singleton :: forall a. a -> Queue a
singleton a = Queue { front: Cons a Nil, back: Nil }

toList :: forall a. Queue a -> List a
toList (Queue { front, back }) = front <> List.reverse back

push :: forall a. Queue a -> a -> Queue a
push (Queue {front, back}) x = fixup (Cons x back) front

pop :: forall a. Queue a -> Maybe (Tuple a (Queue a))
pop (Queue {front, back}) = case front of
  Nil -> Nothing
  Cons x xs -> Just $ Tuple x $ fixup back xs

extend :: forall a f. Foldable f => Queue a -> f a -> Queue a
extend = foldl push

fixup :: forall a. List a -> List a -> Queue a
fixup back = case _ of
  Nil -> Queue {front: List.reverse back, back: Nil}
  front -> Queue {front, back}
