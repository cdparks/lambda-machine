module Data.Queue
  ( Queue
  , fromFoldable
  , toUnfoldable
  , empty
  , singleton
  , pop
  , push
  , extend
  , valid
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.List as List
import Data.Traversable (sequenceDefault)
import Test.QuickCheck (class Arbitrary, arbitrary)

-- | Okasaki-style queue with amortized constant push and pop
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
  map f (Queue {front, back}) = queue (f <$> front) (f <$> back)

instance foldableQueue :: Foldable Queue where
  foldr f z = foldr f z <<< toList
  foldl f z = foldl f z <<< toList
  foldMap f = foldMap f <<< toList

instance traversableQueue :: Traversable Queue where
  traverse f =
    map (flip queue Nil)
    <<< traverse f
    <<< toList
  sequence = sequenceDefault

instance arbitraryQueue :: Arbitrary a => Arbitrary (Queue a) where
  arbitrary = queue <$> arbitrary <*> arbitrary

-- | Convert any `Foldable` to a `Queue`, O(n)
fromFoldable :: forall a f. Foldable f => f a -> Queue a
fromFoldable = flip queue Nil <<< List.fromFoldable

-- | Convert a `Queue` to any `Unfoldable`, O(n)
toUnfoldable :: forall a f. Unfoldable f => Queue a -> f a
toUnfoldable = List.toUnfoldable <<< toList

-- | The empty `Queue`, O(1)
empty :: forall a. Queue a
empty = queue Nil Nil

-- | A `Queue` with only one element, O(1)
singleton :: forall a. a -> Queue a
singleton a = queue (a:Nil) Nil

-- | Convert a `Queue` into a `List`, O(n)
toList :: forall a. Queue a -> List a
toList (Queue { front, back }) = front <> List.reverse back

-- | Push an element onto the back of the `Queue`, amortized O(1)
push :: forall a. Queue a -> a -> Queue a
push (Queue {front, back}) x = queue front $ x : back

-- | Pop an element off of the front of the `Queue`, amortized O(1)
pop :: forall a. Queue a -> Maybe (Tuple a (Queue a))
pop (Queue {front, back}) = case front of
  Nil -> Nothing
  Cons x xs -> Just $ Tuple x $ queue xs back

-- | Push each element of a `Foldable` value onto the back of the `Queue`, amortized O(n)
extend :: forall a f. Foldable f => Queue a -> f a -> Queue a
extend = foldl push

-- | Maintain the invariant that a non-empty `Queue` has a non-empty front list
queue :: forall a. List a -> List a -> Queue a
queue front back = case front of
  Nil -> Queue {front: List.reverse back, back: Nil}
  _ -> Queue {front, back}

-- | Validate that the invariant is maintained
valid :: forall a. Queue a -> Boolean
valid (Queue {front, back}) = not (List.null front) || List.null back
