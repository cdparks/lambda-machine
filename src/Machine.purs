module Machine
  ( Machine(..)
  , start
  , smallStep
  , bigStep
  ) where

import Prelude

import Control.Monad.State (class MonadState, evalState, gets, modify_)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.List as List
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Traversable (for)
import Data.TraversableWithIndex (forWithIndex)
import Data.Foldable (fold)
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

initialize
  :: HashMap Name Expr
  -> Expr
  -> {root :: Address, globals :: HashMap Name Address, heap :: Heap Node }
initialize rawGlobals expr =
  flip evalState {heap: Heap.empty} do
    globals <- initializeGlobals rawGlobals
    root <- compile globals expr
    heap <- gets _.heap
    pure { root, globals, heap}

initializeGlobals
  :: forall s m
   . MonadState { heap :: Heap Node | s } m
  => HashMap Name Expr
  -> m (HashMap Name Address)
initializeGlobals globals = do
  env <- for globals \expr -> do
    addr <- Heap.alloc $ Pointer $ wrap 0
    pure { addr, expr }
  let addrs = _.addr <$> env
  forWithIndex env \name { addr, expr } -> do
    compiled <- compile addrs expr
    Heap.update addr $ Global name compiled
    pure addr

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

start :: HashMap Name Expr -> Expr -> Machine
start rawGlobals expr =
  { root
  , heap
  , globals
  , stack: Stack.singleton root
  , stash: Stash.empty
  , trace: Start
  }
 where
   {root, globals, heap} = initialize rawGlobals expr

emit :: forall s m. MonadState { trace :: Trace | s } m => Trace -> m Unit
emit message = modify_ _ { trace = message }

smallStep :: forall m. MonadState Machine m => m Unit
smallStep = do
  { top } <- gets _.stack
  eval top =<< Heap.fetch top

bigStep :: forall m. MonadState Machine m => m Unit
bigStep = do
  smallStep
  trace <- gets _.trace
  unless (interesting trace) bigStep
 where
  interesting = case _ of
    Fetched _ -> true
    Substituted _ _ -> true
    Halted _ -> true
    _ -> false

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
