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
import Lambda.Language.Expr (Expr(..))
import Lambda.Language.Name (Name)
import Lambda.Machine.Address (Address)
import Lambda.Machine.Globals (Globals)
import Lambda.Machine.Globals as Globals
import Lambda.Machine.Heap (Heap)
import Lambda.Machine.Heap as Heap
import Partial.Unsafe (unsafeCrashWith)

data Node
  = Node Address Address
  | Closure (Array Address) (Env Address) Name Expr
  | Global Name Address
  | Stuck Stuck
  | Pointer Address

derive instance genericNode :: Generic Node _

instance showNode :: Show Node where
  show x = genericShow x

data Stuck
  = StuckVar Name
  | StuckBind Name Address
  | StuckApp Address Address

derive instance genericStuck :: Generic Stuck _

instance showStuck :: Show Stuck where
  show x = genericShow x

type Env = List

deref :: forall a. Show a => Int -> Env a -> a
deref i env = case List.index env i of
  Just a -> a
  Nothing -> unsafeCrashWith $ fold
    [ "Invalid De Bruijn index "
    , show i
    , " in environment "
    , show env
    ]

define
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Name
  -> Expr
  -> m Unit
define name expr = Globals.add name \_ ->
  Global name <$> compile expr

compile
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Expr
  -> m Address
compile = instantiate Nil

instantiate
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Env Address
  -> Expr
  -> m Address
instantiate env = case _ of
  Bind name fvs body -> do
    addrs <- traverse Globals.get $ Array.fromFoldable fvs
    Heap.alloc $ Closure addrs env name body
  App f0 a0 -> do
    a <- instantiate env a0
    f <- instantiate env f0
    Heap.alloc $ Node f a
  Bound i -> do
    pure $ deref i env
  Free name -> Globals.get name

instantiateAt
  :: forall s m
   . MonadState { heap :: Heap Node, globals :: Globals | s } m
  => Address
  -> Env Address
  -> Expr
  -> m Unit
instantiateAt target env = case _ of
  Bind name fvs body -> do
    addrs <- traverse Globals.get $ Array.fromFoldable fvs
    Heap.update target $ Closure addrs env name body
  App f0 a0 -> do
    a <- instantiate env a0
    f <- instantiate env f0
    Heap.update target $ Node f a
  Bound i ->
    Heap.update target $ Pointer $ deref i env
  Free name -> do
    addr <- Globals.get name
    Heap.update target $ Global name addr

children :: Node -> Array Address
children = case _ of
  Node lhs rhs -> [lhs, rhs]
  Closure fvs env _ _ -> fvs <> Array.fromFoldable env
  Global _ addr -> [addr]
  Stuck (StuckVar _) -> []
  Stuck (StuckBind _ addr) -> [addr]
  Stuck (StuckApp lhs rhs) -> [lhs, rhs]
  Pointer addr -> [addr]
