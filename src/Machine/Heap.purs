module Machine.Heap
  ( Heap
  , empty
  , alloc
  , reserve
  , fetch
  , update
  , free
  ) where

import Prelude

import Control.Monad.State (class MonadState, gets, modify_)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
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

modifyHeap :: forall a s m r. MonadState { heap :: Heap a | s } m => (Heap a -> Tuple r (Heap a)) -> m r
modifyHeap f = do
  heap0 <- gets _.heap
  case f heap0 of
    Tuple r heap1 -> do
      modify_ $ _ { heap = heap1 }
      pure r

withHeap :: forall a s m r. MonadState { heap :: Heap a | s } m => (Heap a -> r) -> m r
withHeap f = modifyHeap $ \heap -> Tuple (f heap) heap
