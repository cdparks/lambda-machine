module Machine.Globals
  ( Globals
  , empty
  , add
  , get
  , remove
  ) where

import Prelude hiding (add)

import Control.Monad.State (class MonadState, gets, modify_)
import Data.Foldable (for_)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe)
import Language.Name (Name)
import Machine.Address (Address)
import Machine.Heap (Heap)
import Machine.Heap as Heap

type Globals = HashMap Name Address

empty :: Globals
empty = HashMap.empty

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

get
  :: forall s m
   . MonadState { globals :: Globals | s } m
  => Name
  -> m (Maybe Address)
get name = gets $ HashMap.lookup name <<< _.globals

remove
  :: forall a s m. MonadState { heap :: Heap a, globals :: Globals | s} m
  => Name
  -> m Unit
remove name = do
  mAddr <- get name
  for_ mAddr \addr -> do
    Heap.free addr
    globals <- gets _.globals
    modify_ _ { globals = HashMap.delete name globals }
