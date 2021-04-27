module Lambda.Machine
  ( Machine(..)
  , new
  , step
  , halted
  , snapshot
  ) where

import Lambda.Prelude

import Lambda.Language.Name (Name)
import Lambda.Language.Nameless as Nameless
import Lambda.Language.PrettyPrint (prettyPrint, sugar)
import Lambda.Language.Syntax as Syntax
import Lambda.Machine.Address (Address)
import Lambda.Machine.Globals (Globals)
import Lambda.Machine.Globals as Globals
import Lambda.Machine.Heap (Heap)
import Lambda.Machine.Heap as Heap
import Lambda.Machine.Node (Node(..), Stuck(..))
import Lambda.Machine.Node as Node
import Lambda.Machine.Stack (Stack)
import Lambda.Machine.Stack as Stack
import Lambda.Machine.Stash (Stash)
import Lambda.Machine.Stash as Stash
import Lambda.Machine.Trace (Trace(..))
import Lambda.Machine.Trace as Trace
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
-- |
type Machine =
  { root :: Address
  , stack :: Stack
  , stash :: Stash
  , heap :: Heap Node
  , globals :: Globals
  , trace :: Trace
  }

-- | Create a new `Machine` given a list of top-level definitions and
-- | a root expression to start evaluating. Run a garbage collection
-- | up front to drop globals we won't need.
new
  :: forall f. Foldable f
  => f (Tuple Name Nameless.Expression)
  -> Nameless.Expression -> Machine
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
    Heap.gc [root] Node.children
    heap <- gets _.heap
    globals <- gets _.globals
    pure {root, globals, heap}

-- | Has the `Machine` halted?
halted :: Machine -> Boolean
halted m = case m.trace of
  Halted _ -> true
  _ -> false

-- | Update the `trace` field with a summary of the most recent step.
emit :: forall s m. MonadState { trace :: Trace | s } m => Trace -> m Unit
emit message = modify_ _ { trace = message }

-- | Perform one (big) step of evaluation. We consider the following to
-- | be big steps:
-- | 1. Replacing a global name with its definition
-- | 2. Beta-reduction
-- | 3. Halting
-- |
-- | See the `interesting` function for where this is exactly spelled
-- | out.
step :: Machine -> Machine
step = stepWith interesting

-- | Perform steps in constant stack space until the `Trace`
-- | satisfies the first argument. Passing `const true` would
-- | correspond to truly small-step evaluation. Specifically, you
-- | would observe stack unwinding, pointer-chasing, etc.
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

-- | Find all root addresses for garbage collecting. Note that
-- | top-level definitions are not considered roots.
getRoots :: forall m . MonadState Machine m => m (Array Address)
getRoots = do
  {root, stack, stash, globals, trace} <- get
  pure $ fold
    [ [root]
    , Stack.roots stack
    , Stash.roots stash
    , Trace.roots trace
    ]

-- | Was the last step interesting?
interesting :: Trace -> Boolean
interesting = case _ of
  Fetched _ -> true
  Substituted _ _ -> true
  Halted _ -> true
  _ -> false

-- | Perform one state transition based on the `Node` at the top
-- | of the stack.
eval :: forall m. MonadState Machine m => Address -> Node -> m Unit
eval top = case _ of
  -- Application node - unwind by pushing the left-hand-side onto the
  -- stack
  Apply f _ -> do
    Stack.push f
    emit $ Unwound f

  -- Pointer node - dereference and push onto the stack. If the pointee
  -- is being applied, update the application node to point at it.
  Pointer address -> do
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        Heap.update app $ Apply address arg
      Nothing -> pure unit
    Stack.replace address
    emit $ Followed address

  -- Top-level definition - push the address onto the stack. If the
  -- address is being applied, update the application node to point at
  -- it.
  Global name address -> do
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        Heap.update app $ Apply address arg
      Nothing -> Heap.update top $ Pointer address
    Stack.replace address
    emit $ Fetched name

  -- Closure - if there is an argument, push it on the front of the
  -- closure's environment and instantiate the body. Then update the
  -- application node to point at the newly constructed node.
  --
  -- Otherwise, allocate a stuck value and instantiate the body with
  -- that in the environment instead. Finally allocate a stuck lambda
  -- and update the top of the stack to point at it.
  Closure {env, name, body} ->
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        Node.instantiateAt app (Cons arg env) body
        Stack.discard
        emit $ Substituted name arg
      Nothing -> do
        arg <- Heap.alloc $ Stuck $ StuckVar name
        node <- Node.instantiate (Cons arg env) body
        Heap.update top $ Stuck $ StuckLambda name node
        Stack.replace node
        emit $ WentUnder top

  -- Stuck node - if there is an argument, push everything beneath it
  -- onto the stash, and continue evaluation on the argument.
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
        emit $ Discarded top arg
      Nothing ->
        Stash.restore >>= case _ of
          Just arg -> emit $ Discarded top arg
          Nothing -> emit $ Halted top

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

-- | Fetch the node and convert it to `Syntax.Expression`.
formatNode :: forall s m. MonadState { heap :: Heap Node | s } m => Address -> m Syntax.Expression
formatNode address = do
  node <- Heap.fetch address
  case node of
    Apply f a -> Syntax.Apply <$> formatNode f <*> formatNode a
    Closure {env, name, body} -> do
      formatted <- traverse formatNode env
      pure $ Syntax.Lambda name $ toSyntax (Syntax.Var name : formatted) body
    Stuck term -> formatStuck term
    Pointer a -> formatNode a
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
formatStuck :: forall s m. MonadState { heap :: Heap Node | s } m => Stuck -> m Syntax.Expression
formatStuck = case _ of
  StuckVar name -> pure $ Syntax.Var name
  StuckLambda name e  -> Syntax.Lambda name <$> formatNode e
  StuckApply f a -> Syntax.Apply <$> formatNode f <*> formatNode a

-- | Pretty-print `Syntax.Expression`
formatSyntax :: Syntax.Expression -> String
formatSyntax = sugar <<< prettyPrint

-- | Convert a `Trace` to a human-readable explanation.
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

-- | Take a snapshot of a `Machine` by formatting the `root` node and
-- | producing a human-readable trace.
snapshot :: Machine -> { root :: Syntax.Expression, trace :: String }
snapshot = evalState do
  root <- formatNode =<< gets _.root
  trace <- formatTrace =<< gets _.trace
  pure { root, trace }
