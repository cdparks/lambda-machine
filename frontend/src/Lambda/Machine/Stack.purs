module Lambda.Machine.Stack
  ( Stack
  , singleton
  , push
  , discard
  , replace
  , peek
  , fromList
  , roots
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.List (index)
import Lambda.Machine.Address (Address)

-- | Non-empty stack.
type Stack =
  { top :: Address
  , rest :: List Address
  }

-- | Stack with one element.
singleton :: Address -> Stack
singleton top = { top, rest: Nil }

-- | Push an element on top of the stack
push :: forall s m. MonadState { stack :: Stack | s } m => Address -> m Unit
push address = do
  {top, rest} <- gets _.stack
  modify_ $ _ { stack = {top: address, rest: Cons top rest} }

-- | Discard the top element unless doing so would produce an empty stack.
discard :: forall s m. MonadState { stack :: Stack | s } m => m Unit
discard = do
  {rest} <- gets _.stack
  case rest of
    Cons y ys -> modify_ $ _ { stack = {top: y, rest: ys} }
    Nil -> pure unit

-- | Replace the top element of the stack.
replace :: forall s m. MonadState { stack :: Stack | s } m => Address -> m Unit
replace address = do
  {rest} <- gets _.stack
  modify_ $ _ { stack = {top: address, rest} }

-- | Attempt to return the address on the stack offset from the top.
peek :: forall s m. MonadState { stack :: Stack | s } m => Int -> m (Maybe Address)
peek i = do
  {top, rest} <- gets _.stack
  pure $ index (Cons top rest) i

-- | Attempt to convert a `List` of `Address`'s to a `Stack`
fromList :: List Address -> Maybe Stack
fromList = case _ of
  Nil -> Nothing
  Cons top rest -> Just {top, rest}

-- | Return all addresses from the `Stack` for garbage collection.
roots :: Stack -> Array Address
roots {top, rest} = [top] <> Array.fromFoldable rest
