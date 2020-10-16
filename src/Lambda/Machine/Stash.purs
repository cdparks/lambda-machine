module Lambda.Machine.Stash
  ( Stash(..)
  , empty
  , suspend
  , restore
  , roots
  ) where

import Lambda.Prelude

import Data.Array as Array
import Lambda.Machine.Address (Address)
import Lambda.Machine.Stack (Stack, fromList)
import Lambda.Machine.Stack as Stack

-- | A `Stash` is a possibly empty stack of `Stack`s on which we
-- | intend to resume evaluation later.
newtype Stash = Stash (List Stack)

derive instance newtypeStash :: Newtype Stash _
derive newtype instance showStash :: Show Stash

-- | Empty `Stash`
empty :: Stash
empty = Stash Nil

-- | Suspend the current computation by moving the top of the `Stack`
-- | to its own `Stack`, and everything else to the `Stash`.
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

-- | Resume a suspended computation by replacing the `Stack` with
-- | the `Stack` on top of the `Stash` if the `Stash` is non-empty.
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

-- | Return all addresses from the `Stash` for garbage collection.
roots :: Stash -> Array Address
roots = fold <<< Array.fromFoldable <<< map Stack.roots <<< un Stash
