module Lambda.Machine.Heap
  ( Heap
  , empty
  , alloc
  , reserve
  , fetch
  , update
  , free
  , gc
  ) where

import Lambda.Prelude

import Data.Foldable (maximum)
import Data.HashMap as HashMap
import Data.Queue as Queue
import Lambda.Machine.Address (Address, baseptr, nullptr, offset)
import Partial.Unsafe (unsafeCrashWith)

-- | Heap memory represented by a map from addresses to nodes.
type Heap a =
  { memory :: HashMap Address a
  , next :: Address
  }

-- | Empty heap
empty :: forall a. Heap a
empty = { memory: HashMap.empty, next: baseptr }

-- | Allocate memory for a node and return the `Address`.
alloc :: forall a s m. MonadState { heap :: Heap a | s } m => a -> m Address
alloc node = modifyHeap \{ memory, next } -> Tuple next
  { memory: HashMap.insert next node memory
  , next: offset 1 next
  }

-- | Reserve an `Address` for a node without actually writing it.
reserve :: forall a s m. MonadState { heap :: Heap a | s } m => m Address
reserve = modifyHeap \{ memory, next } -> Tuple next
  { memory
  , next: offset 1 next
  }

-- | Dereference an `Address` or crash.
fetch :: forall a s m. MonadState { heap :: Heap a | s } m => Address -> m a
fetch address = withHeap \{ memory } ->
  case HashMap.lookup address memory of
    Just node -> node
    Nothing -> unsafeCrashWith $ "Invalid address " <> show address

-- | Overwrite the value at an `Address`.
update :: forall a s m. MonadState { heap :: Heap a | s } m => Address -> a -> m Unit
update address node = modifyHeap \heap -> Tuple unit $ heap
  { memory = HashMap.insert address node heap.memory
  }

-- | Free the memory associated with an `Address`.
free :: forall a s m. MonadState { heap :: Heap a | s } m => Address -> m Unit
free address = modifyHeap \heap -> Tuple unit $ heap
  { memory = HashMap.delete address heap.memory
  }

-- | Eliminate unused nodes from the `Heap`. The first argument is a
-- | sequence of root addresses, and the second argument specifies how
-- | to find child addresses of a node. Uses stack-safe breadth-first-
-- | search to move all used addresses to a new heap.
gc
  :: forall a s m f
   . MonadState { heap :: Heap a | s } m
  => MonadRec m
  => Foldable f
  => f Address
  -> (a -> Array Address)
  -> m Unit
gc roots children = do
  toSpace <- tailRecM go
    { queue: Queue.fromFoldable roots
    , toSpace: HashMap.empty
    }
  modify_ _
    { heap =
      { memory: toSpace
      , next: offset 1 $ fromMaybe nullptr $ maximum $ HashMap.keys toSpace
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

-- | Helper to modify the `Heap` and return a value.
modifyHeap :: forall a s m r. MonadState { heap :: Heap a | s } m => (Heap a -> Tuple r (Heap a)) -> m r
modifyHeap f = do
  heap0 <- gets _.heap
  case f heap0 of
    Tuple r heap1 -> do
      modify_ $ _ { heap = heap1 }
      pure r

-- | Helper to use data from the `Heap` without modifying it.
withHeap :: forall a s m r. MonadState { heap :: Heap a | s } m => (Heap a -> r) -> m r
withHeap f = modifyHeap $ \heap -> Tuple (f heap) heap
