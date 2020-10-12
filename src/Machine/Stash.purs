module Machine.Stash
  ( Stash(..)
  , empty
  , suspend
  , restore
  , roots
  ) where

import Prelude

import Control.Monad.State (class MonadState, gets, modify_)
import Data.Array as Array
import Data.Foldable (fold)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, un)
import Machine.Address (Address)
import Machine.Stack (Stack, fromList)
import Machine.Stack as Stack

newtype Stash = Stash (List Stack)

derive instance newtypeStash :: Newtype Stash _
derive newtype instance showStash :: Show Stash

empty :: Stash
empty = Stash Nil

suspend :: forall s m. MonadState { stack :: Stack, stash :: Stash | s } m => m Unit
suspend = do
  {top, rest} <- gets _.stack
  Stash stacks <- gets _.stash
  modify_ $ _
    { stack = {top, rest: Nil}
    , stash = Stash $ extend stacks rest
    }
 where
  extend stash = maybe stash (_ : stash) <<< fromList

restore :: forall s m. MonadState { stack :: Stack, stash :: Stash | s } m => m (Maybe Address)
restore = do
  Stash stash <- gets _.stash
  case stash of
    Cons stack stacks -> do
      modify_ $ _
        { stack = stack
        , stash = Stash stacks
        }
      pure $ Just stack.top
    _ -> pure Nothing

roots :: Stash -> Array Address
roots = fold <<< Array.fromFoldable <<< map Stack.roots <<< un Stash
