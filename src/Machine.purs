module Machine
  ( Machine(..)
  , new
  , step
  , add
  , remove
  ) where

import Prelude hiding (add)

import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Monad.State (class MonadState, evalState, execState, gets, modify_)
import Data.Foldable (class Foldable, fold)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Data.Tuple (Tuple, uncurry)
import Language.Expr (Expr)
import Language.Name (Name)
import Machine.Address (Address)
import Machine.Globals (Globals)
import Machine.Globals as Globals
import Machine.Heap (Heap)
import Machine.Heap as Heap
import Machine.Node (Node(..), Stuck(..))
import Machine.Node as Node
import Machine.Stack (Stack)
import Machine.Stack as Stack
import Machine.Stash (Stash)
import Machine.Stash as Stash
import Machine.Trace (Trace(..))
import Partial.Unsafe (unsafeCrashWith)

type Machine =
  { root :: Address
  , stack :: Stack
  , stash :: Stash
  , heap :: Heap Node
  , globals :: Globals
  , trace :: Trace
  }

new :: forall f. Foldable f => f (Tuple Name Expr) -> Expr -> Machine
new rawGlobals expr =
  { root
  , heap
  , globals
  , stack: Stack.singleton root
  , stash: Stash.empty
  , trace: Start
  }
 where
  empty = { heap: Heap.empty, globals: Globals.empty }
  {root, globals, heap} = flip evalState empty $ do
    traverse_ (uncurry Node.define) rawGlobals
    root <- Node.compile expr
    heap <- gets _.heap
    globals <- gets _.globals
    pure {root, globals, heap}

add :: Name -> Expr -> Machine -> Machine
add name expr = execState $ Node.define name expr

remove :: Name -> Machine -> Machine
remove name = execState $ Globals.remove name

emit :: forall s m. MonadState { trace :: Trace | s } m => Trace -> m Unit
emit message = modify_ _ { trace = message }

step :: Machine -> Machine
step = execState $ tailRecM go unit
 where
  go _ = do
    { top } <- gets _.stack
    eval top =<< Heap.fetch top
    trace <- gets _.trace
    pure $ next trace unit
  next = case _ of
    Fetched _ -> Done
    Substituted _ _ -> Done
    Halted _ -> Done
    _ -> Loop

eval :: forall m. MonadState Machine m => Address -> Node -> m Unit
eval top = case _ of
  Node f _ -> do
    Stack.push f
    emit $ Unwound f

  Pointer address -> do
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        Heap.update app $ Node address arg
      Nothing -> pure unit
    Stack.replace address
    emit $ Followed address

  Global name address -> do
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        Heap.update app $ Node address arg
      Nothing -> Heap.update top $ Pointer address
    Stack.replace address
    emit $ Fetched name

  Closure env name e ->
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        x <- Node.instantiate (Cons arg env) e
        Heap.update app $ Pointer x
        Stack.discard
        Stack.replace x
        emit $ Substituted name arg
      Nothing -> do
        arg <- Heap.alloc $ Stuck $ StuckVar name
        x <- Node.instantiate (Cons arg env) e
        p <- Heap.alloc $ Stuck $ StuckBind name x
        Heap.update top $ Pointer p
        Stack.replace x
        emit $ WentUnder top

  Stuck _ ->
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        p <- Heap.alloc $ Stuck $ StuckApp top arg
        Heap.update app $ Pointer p
        Stack.discard
        Stack.replace arg
        Stash.suspend
        emit $ Discarded top arg
      Nothing ->
        Stash.restore >>= case _ of
          Just arg -> emit $ Discarded top arg
          Nothing -> emit $ Halted top

fetchArg :: forall s m. MonadState { heap :: Heap Node | s } m => Address -> m Address
fetchArg address = do
  node <- Heap.fetch address
  case node of
    Node _ arg -> pure arg
    _ -> unsafeCrashWith $ fold
      [ "Non-application below top of stack: "
      , show node
      ]
