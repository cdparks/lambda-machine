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

import Prelude

import Control.Monad.State (class MonadState, gets, modify_)
import Data.Array as Array
import Data.List (List(..), index)
import Data.Maybe (Maybe(..))
import Lambda.Machine.Address (Address)

type Stack =
  { top :: Address
  , rest :: List Address
  }

singleton :: Address -> Stack
singleton top = { top, rest: Nil }

push :: forall s m. MonadState { stack :: Stack | s } m => Address -> m Unit
push address = do
  {top, rest} <- gets _.stack
  modify_ $ _ { stack = {top: address, rest: Cons top rest} }

discard :: forall s m. MonadState { stack :: Stack | s } m => m Unit
discard = do
  {rest} <- gets _.stack
  case rest of
    Cons y ys -> modify_ $ _ { stack = {top: y, rest: ys} }
    Nil -> pure unit

replace :: forall s m. MonadState { stack :: Stack | s } m => Address -> m Unit
replace address = do
  {rest} <- gets _.stack
  modify_ $ _ { stack = {top: address, rest} }

peek :: forall s m. MonadState { stack :: Stack | s } m => Int -> m (Maybe Address)
peek i = do
  {top, rest} <- gets _.stack
  pure $ index (Cons top rest) i

fromList :: List Address -> Maybe Stack
fromList = case _ of
  Nil -> Nothing
  Cons top rest -> Just {top, rest}

roots :: Stack -> Array Address
roots {top, rest} = [top] <> Array.fromFoldable rest
