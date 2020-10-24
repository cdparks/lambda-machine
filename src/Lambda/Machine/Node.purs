module Lambda.Machine.Node
  ( Node(..)
  , Stuck(..)
  , Env
  , deref
  , define
  , compile
  , instantiate
  , instantiateAt
  , children
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.List as List
import Lambda.Language.Expr (Expr)
import Lambda.Language.Expr as Expr
import Lambda.Language.Name (Name)
import Lambda.Machine.Address (Address)
import Lambda.Machine.Globals (Globals)
import Lambda.Machine.Globals as Globals
import Lambda.Machine.Heap (Heap)
import Lambda.Machine.Heap as Heap
import Partial.Unsafe (unsafeCrashWith)

-- | Nodes in the graph.  Closures contain references to the global
-- | addresses they use as well as their local environment. `Stuck`
-- | nodes cannot be evaluated any further. Pointers are used in
-- | certain kinds of updates.
data Node
  = Apply Address Address
  | Closure (Array Address) (Env Address) Name Expr
  | Global Name Address
  | Stuck Stuck
  | Pointer Address

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show x = genericShow x

-- | Stuck nodes cannot be evaluated any further.
data Stuck
  = StuckVar Name
  | StuckLambda Name Address
  | StuckApply Address Address

derive instance genericStuck :: Generic Stuck _

instance showStuck :: Show Stuck where
  show x = genericShow x

-- | Closure environments are lists. Construction is fast; indexing is
-- | slow. Environments should be small enough that it doesn't matter.
type Env = List

-- | Index a closure's environment with the De-Bruijn index in a bound
-- | variable.
deref :: forall a. Show a => Int -> Env a -> a
deref i env = case List.index env i of
  Just a -> a
  Nothing -> unsafeCrashWith $ fold
    [ "Invalid De Bruijn index "
    , show i
    , " in environment "
    , show env
    ]

-- | Add a top-level definition to the `Globals`.
define
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Name
  -> Expr
  -> m Unit
define name expr = Globals.add name \_ ->
  Global name <$> compile expr

-- | Instantiate a closed expression.
compile
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Expr
  -> m Address
compile = instantiate Nil

-- | Instantiate an expression in an environment. This builds a shallow
-- | graph of an expression - shallow because we don't go under lambdas
-- | until they're applied.
instantiate
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Env Address
  -> Expr
  -> m Address
instantiate env = case _ of
  Expr.Lambda name fvs body -> do
    addrs <- traverse Globals.get $ Array.fromFoldable fvs
    Heap.alloc $ Closure addrs env name body
  Expr.Apply f0 a0 -> do
    a <- instantiate env a0
    f <- instantiate env f0
    Heap.alloc $ Apply f a
  Expr.Bound i -> do
    pure $ deref i env
  Expr.Free name -> Globals.get name

-- | Instantiate an expression in an environment and overwrite the
-- | given address. Generates fewer pointer chains since we don't
-- | need an indirection.
instantiateAt
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Address
  -> Env Address
  -> Expr
  -> m Unit
instantiateAt target env = case _ of
  Expr.Lambda name fvs body -> do
    addrs <- traverse Globals.get $ Array.fromFoldable fvs
    Heap.update target $ Closure addrs env name body
  Expr.Apply f0 a0 -> do
    a <- instantiate env a0
    f <- instantiate env f0
    Heap.update target $ Apply f a
  Expr.Bound i ->
    Heap.update target $ Pointer $ deref i env
  Expr.Free name -> do
    addr <- Globals.get name
    Heap.update target $ Global name addr

-- | Find all addresses embedded in a `Node`. Used for pointer-chasing
-- | in garbage collection.
children :: Node -> Array Address
children = case _ of
  Apply lhs rhs -> [lhs, rhs]
  Closure fvs env _ _ -> fvs <> Array.fromFoldable env
  Global _ addr -> [addr]
  Stuck (StuckVar _) -> []
  Stuck (StuckLambda _ addr) -> [addr]
  Stuck (StuckApply lhs rhs) -> [lhs, rhs]
  Pointer addr -> [addr]
