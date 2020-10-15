module Lambda.Machine.Globals
  ( Globals
  , empty
  , add
  , get
  , remove
  ) where

import Prelude hiding (add)

import Control.Monad.State (class MonadState, gets, modify_)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Maybe (Maybe(..))
import Lambda.Language.Name (Name)
import Lambda.Machine.Address (Address)
import Lambda.Machine.Heap (Heap)
import Lambda.Machine.Heap as Heap
import Partial.Unsafe (unsafeCrashWith)

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
  -> m Address
get name = do
  globals <- gets _.globals
  case HashMap.lookup name globals of
    Just addr -> pure addr
    Nothing -> unsafeCrashWith $ "No global binding for " <> show name

remove
  :: forall a s m. MonadState { heap :: Heap a, globals :: Globals | s} m
  => Name
  -> m Unit
remove name = do
  Heap.free =<< get name
  modify_ \s@{globals} -> s { globals = HashMap.delete name globals }
