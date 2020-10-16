module Lambda.Machine.Globals
  ( Globals
  , empty
  , add
  , get
  , remove
  ) where

import Lambda.Prelude hiding (add, get)

import Data.HashMap as HashMap
import Lambda.Language.Name (Name)
import Lambda.Machine.Address (Address)
import Lambda.Machine.Heap (Heap)
import Lambda.Machine.Heap as Heap
import Partial.Unsafe (unsafeCrashWith)

-- | Top-level names
type Globals = HashMap Name Address

-- | Empty `Globals`
empty :: Globals
empty = HashMap.empty

-- | Add a new top-level definition. Note that the way we reserve
-- | memory for a global before defining it means we can support
-- | direct recursion, but currently this is disallowed by the
-- | consistency-checking in `World`. May relax that once we have
-- | concrete syntax for let-rec/fix beyond what can be defined with
-- | plain lambda calculus.
add
  :: forall a s m. MonadState { heap :: Heap a , globals :: Globals | s} m
  => Name
  -> (Unit -> m a)
  -> m Unit
add name gen = do
  globals <- gets _.globals
  p <- Heap.reserve
  modify_ _ { globals = HashMap.insert name p globals }
  node <- gen unit
  Heap.update p node

-- | Fetch a top-level definition or crash.
get
  :: forall s m
   . MonadState { globals :: Globals | s } m
  => Name
  -> m Address
get name = do
  globals <- gets _.globals
  case HashMap.lookup name globals of
    Just addr -> pure addr
    Nothing -> unsafeCrashWith $ "No global binding for " <> show name

-- | Remove a top-level definition.
remove
  :: forall a s m. MonadState { heap :: Heap a, globals :: Globals | s} m
  => Name
  -> m Unit
remove name = do
  Heap.free =<< get name
  modify_ \s@{globals} -> s { globals = HashMap.delete name globals }
