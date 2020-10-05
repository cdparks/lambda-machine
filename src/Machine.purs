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
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (traverse_)
import Data.Tuple(Tuple, uncurry)
import Language.Expr (Expr(..))
import Language.Name (Name)
import Machine.Address (Address)
import Machine.Heap (Heap)
import Machine.Heap as Heap
import Machine.Node (Node(..), Stuck(..), Env)
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
  , globals :: HashMap Name Address
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
  empty = { heap: Heap.empty, globals: HashMap.empty }
  {root, globals, heap} = flip evalState empty $ do
    traverse_ (uncurry add) rawGlobals
    globals <- gets _.globals
    root <- compile globals expr
    heap <- gets _.heap
    pure {root, globals, heap}

add
  :: forall s m. MonadState { heap :: Heap Node, globals :: HashMap Name Address | s} m
  => Name
  -> Expr
  -> m Unit
add name expr = do
  addr <- Heap.alloc $ Pointer $ wrap 0
  globals <- gets _.globals
  compiled <- compile globals expr
  Heap.update addr $ Global name compiled
  modify_ _ { globals = HashMap.insert name addr globals }

remove
  :: forall s m. MonadState { heap :: Heap Node, globals :: HashMap Name Address | s} m
  => Name
  -> m Unit
remove name = do
  globals <- gets _.globals
  case HashMap.lookup name globals of
    Nothing -> pure unit
    Just addr -> do
      Heap.free addr
      modify_ _ { globals = HashMap.delete name globals }

compile
  :: forall s m
   . MonadState { heap :: Heap Node | s } m
  => HashMap Name Address
  -> Expr
  -> m Address
compile globals = loop Nil
 where
  loop env0 = case _ of
    Bind name body -> Heap.alloc $ Closure env0 name body
    App f0 a0 -> do
      a <- loop env0 a0
      f <- loop env0 f0
      Heap.alloc $ Node f a
    Bound i ->
      pure $ deref i env0
    Free name ->
      case HashMap.lookup name globals of
        Nothing -> Heap.alloc $ Stuck $ StuckVar name
        Just addr -> pure addr

deref :: Int -> Env -> Address
deref i env = case List.index env i of
  Just a -> a
  Nothing -> unsafeCrashWith $ fold
    [ "Invalid De Bruijn index "
    , show i
    , " in environment "
    , show env
    ]

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
        x <- instantiate (Cons arg env) e
        Heap.update app $ Pointer x
        Stack.discard
        Stack.replace x
        emit $ Substituted name arg
      Nothing -> do
        arg <- Heap.alloc $ Stuck $ StuckVar name
        x <- instantiate (Cons arg env) e
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

instantiate :: forall m. MonadState Machine m => Env -> Expr -> m Address
instantiate env0 = case _ of
  Bind name body ->
    Heap.alloc $ Closure env0 name body
  App f0 a0 -> do
    a <- instantiate env0 a0
    f <- instantiate env0 f0
    Heap.alloc $ Node f a
  Bound i -> do
    pure $ deref i env0
  Free name -> do
    globals <- gets _.globals
    case HashMap.lookup name globals of
      Nothing -> do
        stuck <- Heap.alloc $ Stuck $ StuckVar name
        modify_ _ { globals = HashMap.insert name stuck globals }
        pure stuck
      Just addr -> pure addr

fetchArg :: forall s m. MonadState { heap :: Heap Node | s } m => Address -> m Address
fetchArg address = do
  node <- Heap.fetch address
  case node of
    Node _ arg -> pure arg
    _ -> unsafeCrashWith $ fold
      [ "Non-application below top of stack: "
      , show node
      ]
