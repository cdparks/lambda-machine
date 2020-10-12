module Machine.Heap
  ( Heap
  , empty
  , alloc
  , reserve
  , fetch
  , update
  , free
  , gc
  ) where

import Prelude

import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State (class MonadState, gets, modify_)
import Data.Foldable (class Foldable, maximum)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (wrap)
import Data.Queue as Queue
import Data.Tuple (Tuple(..))
import Machine.Address (Address)
import Partial.Unsafe (unsafeCrashWith)

type Heap a =
  { memory :: HashMap Address a
  , next :: Address
  }

empty :: forall a. Heap a
empty = { memory: HashMap.empty, next: wrap 1 }

alloc :: forall a s m. MonadState { heap :: Heap a | s } m => a -> m Address
alloc node = modifyHeap \{ memory, next } -> Tuple next
  { memory: HashMap.insert next node memory
  , next: next + wrap 1
  }

reserve :: forall a s m. MonadState { heap :: Heap a | s } m => m Address
reserve = modifyHeap \{ memory, next } -> Tuple next
  { memory
  , next: next + wrap 1
  }

fetch :: forall a s m. MonadState { heap :: Heap a | s } m => Address -> m a
fetch address = withHeap \{ memory } ->
  case HashMap.lookup address memory of
    Just node -> node
    Nothing -> unsafeCrashWith $ "Invalid address " <> show address

update :: forall a s m. MonadState { heap :: Heap a | s } m => Address -> a -> m Unit
update address node = modifyHeap \heap -> Tuple unit $ heap
  { memory = HashMap.insert address node heap.memory
  }

free :: forall a s m. MonadState { heap :: Heap a | s } m => Address -> m Unit
free address = modifyHeap \heap -> Tuple unit $ heap
  { memory = HashMap.delete address heap.memory
  }

gc
  :: forall a s m f
   . MonadState { heap :: Heap a | s } m
  => MonadRec m
  => Foldable f
  => f Address
  -> (a -> Array Address)
  -> m Unit
gc roots children = do
  {memory, next} <- gets _.heap
  toSpace <- tailRecM go
    { queue: Queue.fromFoldable roots
    , toSpace: HashMap.empty
    }
  modify_ _
    { heap =
      { memory: toSpace
      , next: wrap 1 + fromMaybe (wrap 0) (maximum $ HashMap.keys toSpace)
      }
    }
 where
  go {queue, toSpace} =
    case Queue.pop queue of
      Nothing -> pure $ Done toSpace
      Just (Tuple root rest)
        | root `HashMap.member` toSpace ->
            pure $ Loop
              { queue: rest
              , toSpace
              }
        | otherwise -> do
            node <- fetch root
            pure $ Loop
              { queue: Queue.extend rest $ children node
              , toSpace: HashMap.insert root node toSpace
              }

modifyHeap :: forall a s m r. MonadState { heap :: Heap a | s } m => (Heap a -> Tuple r (Heap a)) -> m r
modifyHeap f = do
  heap0 <- gets _.heap
  case f heap0 of
    Tuple r heap1 -> do
      modify_ $ _ { heap = heap1 }
      pure r

withHeap :: forall a s m r. MonadState { heap :: Heap a | s } m => (Heap a -> r) -> m r
withHeap f = modifyHeap $ \heap -> Tuple (f heap) heap
