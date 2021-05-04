module Lambda.Machine
  ( Machine(..)
  , Focus(..)
  , Fetch
  , Redex
  , new
  , step
  , snapshot
  ) where

import Lambda.Prelude

import Data.HashSet as HashSet
import Lambda.Language.Name (Name)
import Lambda.Language.Nameless as Nameless
import Lambda.Language.Syntax as Syntax
import Lambda.Machine.Address (Address)
import Lambda.Machine.Globals (Globals)
import Lambda.Machine.Globals as Globals
import Lambda.Machine.Heap (Heap)
import Lambda.Machine.Heap as Heap
import Lambda.Machine.Node (Env, Node(..), Stuck(..))
import Lambda.Machine.Node as Node
import Lambda.Machine.Stack (Stack)
import Lambda.Machine.Stack as Stack
import Lambda.Machine.Stash (Stash)
import Lambda.Machine.Stash as Stash
import Partial.Unsafe (unsafeCrashWith)

-- | Machine-state based roughly on the Template Instantiation Machine
-- | from Simon Peyton-Jones & David Lester's _Implementing Functional
-- | Languages: A Tutorial_.
-- |
-- | The Template Instantiation Machine is a very simple graph
-- | reduction interpreter. It's too slow and inflexible for a "real"
-- | implementation, but for Lambda Machine, all we care about is that
-- | we can either retain or reconstruct enough syntactic information
-- | to incrementally do lazy-evaluation in human-comprehensible steps.
-- | The Template Instantiation Machine works for our use-case.
-- |
-- | Note that this implementation differs from the Template
-- | Instantiation Machine as specified in the literature:
-- | 1. We do not lambda-lift the program to supercombinators. Instead
-- |    we construct closures for lambdas encountered at runtime.
-- | 2. We evaluate applications one argument at a time, like a person
-- |    might do using pen and paper.
-- | 3. We evaluate inside of under-applied lambdas by secretly
-- |    applying them to special `Stuck` values that cannot themselves
-- |    be further evaluated. This makes certain functions (e.g. the
-- |    predecessor function on Church numerals) work that would
-- |    otherwise get stuck too early in a "real" language.
-- | 4. We retain a pointer to the root expression even as we abandon
-- |    the parts of it that we cannot reduce. This is the `root`
-- |    field. Since the machine updates nodes when they're evaluated,
-- |    we can dereference the `root` to track evaluation.
-- | 5. We call the "dump" the "stash". I don't remember why; maybe it
-- |    looks nice next to stack? This detail isn't important, it just
-- |    helps if you're comparing directly.
-- | 6. We break up evaluation into separate unwind and eval steps.
-- |    Unwinding is the process of finding a redex, and eval reduces
-- |    that redex. When we can't find a redex, we're done. This also
-- |    makes it easy to highlight highlight the redex in the UI. Note
-- |    that we call the redex "the focus" since it might also be a
-- |    lone global identifier (not applied to anything).
-- |
type Machine =
  { root :: Address
  , stack :: Stack
  , focus :: Maybe Focus
  , stash :: Stash
  , heap :: Heap Node
  , globals :: Globals
  , halted :: Boolean
  }

-- | Create a new `Machine` given a list of top-level definitions and
-- | a root expression to start evaluating. Run a garbage collection
-- | up front to drop globals we won't need.
new
  :: forall f. Foldable f
  => f (Tuple Name Nameless.Expression)
  -> Nameless.Expression -> Machine
new rawGlobals expr =
  execState (setFocus =<< unwind)
    { root
    , heap
    , globals
    , stack: Stack.singleton root
    , stash: Stash.empty
    , focus: Nothing
    , halted: false
    }
 where
  empty = { heap: Heap.empty, globals: Globals.empty }
  {root, globals, heap} = flip evalState empty $ do
    traverse_ (uncurry Node.define) rawGlobals
    root <- Node.compile expr
    Heap.gc [root] Node.children
    heap <- gets _.heap
    globals <- gets _.globals
    pure {root, globals, heap}

-- | Set the focus, marking the machine as halted if it's Nothing
setFocus
  :: forall s m
   . MonadState { focus :: Maybe Focus, halted :: Boolean | s } m
  => Maybe Focus
  -> m Unit
setFocus focus = modify_ _ { focus = focus, halted = isNothing focus }

-- | Perform one big reduction step
step :: Machine -> Machine
step = execState do
  mFocus <- gets _.focus
  for_ mFocus $ \focus -> do
    eval focus
    setFocus =<< unwind
    roots <- getRoots
    Heap.gc roots Node.children

-- | Find all root addresses for garbage collecting. Note that
-- | top-level definitions are not considered roots.
getRoots :: forall m . MonadState Machine m => m (Array Address)
getRoots = do
  {root, stack, stash, globals} <- get
  pure $ fold
    [ [root]
    , Stack.roots stack
    , Stash.roots stash
    ]

-- | Suspended evaluation of a global variable or an application
data Focus
  = Fetch Fetch
  | Redex Redex

-- | Suspended fetch of a top-level variable
type Fetch =
  -- | Address of the Global itself
  { global :: Address
  -- | Address to update with node
  , target :: Address
  -- | Address of what the Global refers to
  , address :: Address
  -- | Node to write to target
  , node :: Node
  }

-- | Suspended application
type Redex =
  -- | Address in which to instantiate body
  { target :: Address
  -- | New environment including the argument
  , env :: Env Address
  -- | Expression to instantiate in the environment
  , body :: Nameless.Expression
  }

derive instance genericFocus :: Generic Focus _

instance showFocus :: Show Focus where
  show x = genericShow x

instance eqFocus :: Eq Focus where
  eq x = genericEq x

-- | Evaluate a global or redex
eval :: forall m. MonadState Machine m => Focus -> m Unit
eval = case _ of
  Fetch {target, node, address} -> do
    Heap.update target node
    Stack.replace address
  Redex {target, env, body} -> do
    Node.instantiateAt target env body
    Stack.discard

-- | Find the next global or redex to evaluate in constant stack space
unwind :: forall m. MonadState Machine m => MonadRec m => m (Maybe Focus)
unwind = tailRecM go unit
 where
  go _ = do
    { top } <- gets _.stack
    node <- Heap.fetch top
    case node of
      -- Application node - unwind by pushing the left-hand-side onto
      -- the stack
      Apply f _ -> do
        Stack.push f
        pure $ Loop unit

      -- Pointer node - dereference and push onto the stack. If the
      -- pointee is being applied, update the application node to point
      -- at it. If the pointer is to itself, halt and let formatNode
      -- format this as a cycle.
      Pointer address
        | address == top -> pure $ Done Nothing
        | otherwise -> do
            parent <- Stack.peek 1
            for_ parent \app -> do
              arg <- fetchArg app
              Heap.update app $ Apply address arg
            Stack.replace address
            pure $ Loop unit

      -- Top-level definition - pause unwinding and store enough
      -- information that eval can push the address on the stack and
      -- update the right node.
      Global _ address -> do
        Stack.peek 1 >>= case _ of
          Just app -> do
            arg <- fetchArg app
            pure $ Done $ Just $ Fetch
              { global: top
              , target: app
              , address
              , node: Apply address arg
              }
          Nothing ->
            pure $ Done $ Just $ Fetch
              { global: top
              , target: top
              , address
              , node: Pointer address
              }

      -- Closure - if there's an argument, we can pause unwinding and
      -- prepare for eval to instantiate the closure's body in a new
      -- environment that includes the argument.
      --
      -- Otherwise, allocate a stuck value and instantiate the body with
      -- that instead. We can then continue unwinding to find a redex in
      -- the non-stuck part of the body.
      Closure {name, env, body} ->
        Stack.peek 1 >>= case _ of
          Just app -> do
            arg <- fetchArg app
            pure $ Done $ Just $ Redex
              { target: app
              , env: Cons arg env
              , body
              }
          Nothing -> do
            arg <- Heap.alloc $ Stuck $ StuckVar name
            inst <- Node.instantiate (Cons arg env) body
            Heap.update top $ Stuck $ StuckLambda name inst
            Stack.replace inst
            pure $ Loop unit

      -- Stuck node - if there is an argument, push everything beneath it
      -- onto the stash, and continue unwinding on the argument.
      --
      -- Otherwise, attempt to restore the stash and keep going. If there's
      -- nothing left, we're done.
      Stuck _ ->
        Stack.peek 1 >>= case _ of
          Just app -> do
            arg <- fetchArg app
            Heap.update app $ Stuck $ StuckApply top arg
            Stack.discard
            Stack.replace arg
            Stash.suspend
            pure $ Loop unit
          Nothing ->
            Stash.restore >>= case _ of
              Just _ -> pure $ Loop unit
              Nothing -> pure $ Done Nothing

-- | Fetch the right-hand-side of an application node or crash
fetchArg :: forall s m. MonadState { heap :: Heap Node | s } m => Address -> m Address
fetchArg address = do
  node <- Heap.fetch address
  case node of
    Apply _ arg -> pure arg
    _ -> unsafeCrashWith $ fold
      [ "Non-application below top of stack: "
      , show node
      ]

-- | Hide the focus until the argument finishes
hideFocus :: forall s r m. MonadState { focus :: Maybe Focus | s } m => m r -> m r
hideFocus = refocusWith $ const Nothing

-- | Restore the focus if the argument drops it
restoreFocus :: forall s r m. MonadState { focus :: Maybe Focus | s } m => m r -> m r
restoreFocus = refocusWith identity

-- | Modify the focus before running the body, then restore the original focus
refocusWith
  :: forall s r m
   . MonadState { focus :: Maybe Focus | s } m
  => (Maybe Focus -> Maybe Focus)
  -> m r
  -> m r
refocusWith f body = do
  focus <- gets _.focus
  modify_ _ { focus = f focus }
  r <- body
  modify_ _ { focus = focus }
  pure r

formatNode
  :: forall s m
   . MonadState { focus :: Maybe Focus, heap :: Heap Node | s } m
  => Address
  -> m Syntax.Expression
formatNode = hideFocus <<< formatNodeWith HashSet.empty

formatRoot
  :: forall s m
   . MonadState { focus :: Maybe Focus, heap :: Heap Node | s } m
  => Address
  -> m Syntax.Expression
formatRoot = restoreFocus <<< formatNodeWith HashSet.empty

-- | Fetch the node and convert it to `Syntax.Expression`.
formatNodeWith
  :: forall s m
   . MonadState { focus :: Maybe Focus, heap :: Heap Node | s } m
  => HashSet Address
  -> Address
  -> m Syntax.Expression
formatNodeWith seen0 address
  | address `HashSet.member` seen0 = pure $ Syntax.Cycle
  | otherwise = do
    let seen = HashSet.insert address seen0
    node <- Heap.fetch address
    focus <- gets _.focus
    if map highlight focus == Just address
      then do
        modify_ _ { focus = Nothing }
        Syntax.Highlight <$> format seen node
      else format seen node
 where
  format seen = case _ of
    Apply f a -> Syntax.Apply <$> formatNodeWith seen f <*> formatNodeWith seen a
    Closure {env, name, body} -> do
      formatted <- traverse (formatNodeWith seen) env
      pure $ Syntax.Lambda name $ toSyntax (Syntax.Var name : formatted) body
    Stuck term -> formatStuckWith seen term
    Pointer a -> formatNodeWith seen a
    Global name _ -> pure $ Syntax.Var name

-- | Convert a `Nameless.Expression` in a `Closure` to `Syntax.Expression`
toSyntax
  :: List Syntax.Expression
  -> Nameless.Expression
  -> Syntax.Expression
toSyntax env = case _ of
  Nameless.Lambda n _ e -> Syntax.Lambda n $ toSyntax (Syntax.Var n : env) e
  Nameless.Apply f a -> Syntax.Apply (toSyntax env f) (toSyntax env a)
  Nameless.Bound i -> Node.deref i env
  Nameless.Free n -> Syntax.Var n

-- | Format a `Stuck` node.
formatStuckWith
  :: forall s m
   . MonadState { focus :: Maybe Focus, heap :: Heap Node | s } m
  => HashSet Address
  -> Stuck
  -> m Syntax.Expression
formatStuckWith seen = case _ of
  StuckVar name -> pure $ Syntax.Var name
  StuckLambda name e  -> Syntax.Lambda name <$> formatNodeWith seen e
  StuckApply f a -> Syntax.Apply <$> formatNodeWith seen f <*> formatNodeWith seen a

highlight :: Focus -> Address
highlight = case _ of
  Fetch { global } -> global
  Redex { target } -> target

-- | Take a snapshot of a `Machine` by formatting the `root` node
snapshot :: Machine -> Syntax.Expression
snapshot = evalState $ formatRoot =<< gets _.root
