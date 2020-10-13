module Machine
  ( Machine(..)
  , new
  , step
  , add
  , remove
  , halted
  , snapshot
  ) where

import Prelude hiding (add)

import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Monad.State (class MonadState, evalState, execState, gets, get, modify_)
import Data.Foldable (class Foldable, fold)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple, uncurry)
import Language.Expr (Expr(..))
import Language.Name (Name)
import Language.PrettyPrint (prettyPrint, sugar)
import Language.Syntax (Syntax(..))
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
import Machine.Trace as Trace
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

halted :: Machine -> Boolean
halted m = case m.trace of
  Halted _ -> true
  _ -> false

add :: Name -> Expr -> Machine -> Machine
add name expr = execState $ Node.define name expr

remove :: Name -> Machine -> Machine
remove name = execState $ Globals.remove name

emit :: forall s m. MonadState { trace :: Trace | s } m => Trace -> m Unit
emit message = modify_ _ { trace = message }

step :: Machine -> Machine
step = stepWith interesting

stepWith :: (Trace -> Boolean) -> Machine -> Machine
stepWith stop = execState do
  tailRecM go unit
  roots <- getRoots
  Heap.gc roots Node.children
 where
  go _ = do
    { top } <- gets _.stack
    eval top =<< Heap.fetch top
    trace <- gets _.trace
    pure $ next trace unit

  next trace
    | stop trace = Done
    | otherwise = Loop

getRoots :: forall m . MonadState Machine m => m (Array Address)
getRoots = do
  {root, stack, stash, globals, trace} <- get
  pure $ fold
    [ [root]
    , Stack.roots stack
    , Stash.roots stash
    , Trace.roots trace
    ]

interesting :: Trace -> Boolean
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

  Closure _ env name e ->
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        Node.instantiateAt app (Cons arg env) e
        Stack.discard
        emit $ Substituted name arg
      Nothing -> do
        arg <- Heap.alloc $ Stuck $ StuckVar name
        node <- Node.instantiate (Cons arg env) e
        Heap.update top $ Stuck $ StuckBind name node
        Stack.replace node
        emit $ WentUnder top

  Stuck _ ->
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        Heap.update app $ Stuck $ StuckApp top arg
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

formatNode :: forall s m. MonadState { heap :: Heap Node | s } m => Address -> m Syntax
formatNode address = do
  node <- Heap.fetch address
  case node of
    Node f a -> Apply <$> formatNode f <*> formatNode a
    Closure _ env0 name e -> do
      env <- traverse formatNode env0
      pure $ Lambda name $ toSyntax (Var name : env) e
    Stuck term -> formatStuck term
    Pointer a -> formatNode a
    Global name _ -> pure $ Var name

toSyntax :: List Syntax -> Expr -> Syntax
toSyntax env = case _ of
  Bind n _ e -> Lambda n $ toSyntax (Var n : env) e
  App f a -> Apply (toSyntax env f) (toSyntax env a)
  Bound i -> Node.deref i env
  Free n -> Var n

formatStuck :: forall s m. MonadState { heap :: Heap Node | s } m => Stuck -> m Syntax
formatStuck = case _ of
  StuckVar name -> pure $ Var name
  StuckBind name e  -> Lambda name <$> formatNode e
  StuckApp f a -> Apply <$> formatNode f <*> formatNode a

formatSyntax :: Syntax -> String
formatSyntax = sugar <<< prettyPrint

formatTrace :: forall s m. MonadState { heap :: Heap Node | s } m => Trace -> m String
formatTrace = map fold <<< case _ of
  Start -> pure ["Start"]
  Unwound address -> do
    node <- formatNode address
    pure
      [ "Unwound "
      , formatSyntax node
      ]
  Followed address -> do
    node <- formatNode address
    pure
      [ "Followed #"
      , show address
      , " to "
      , formatSyntax node
      ]
  Fetched name -> pure
    [ "Fetched "
    , show name
    ]
  Instantiated address -> do
    node <- formatNode address
    pure
      [ "Instantiated new copy of "
      , formatSyntax node
      ]
  Substituted name address -> do
    stuck <- isStuck <$> Heap.fetch address
    node <- formatNode address
    pure
      [ "Substituted "
      , if stuck then "stuck term " else ""
      , formatSyntax node
      , " in for "
      , show name
      ]
  WentUnder address -> do
    node <- formatNode address
    pure
      [ "Went under stuck lambda "
      , formatSyntax node
      ]
  Discarded discarded next -> do
    stuck <- formatNode discarded
    node <- formatNode next
    pure
      [ "Discarded stuck term "
      , formatSyntax stuck
      , "; evaluating "
      , formatSyntax node
      ]
  Halted address -> do
    node <- formatNode address
    pure
      [ "Halted with "
      , formatSyntax node
      , " on top of stack"
      ]
 where
  isStuck = case _ of
    Stuck _ -> true
    _ -> false

snapshot :: Machine -> { root :: Syntax, trace :: String }
snapshot = evalState do
  root <- formatNode =<< gets _.root
  trace <- formatTrace =<< gets _.trace
  pure { root, trace }
