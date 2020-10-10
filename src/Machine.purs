module Machine
  ( Machine(..)
  , new
  , step
  , add
  , remove
  , halted
  , snapshot
  , testBig
  , testSmall
  ) where

import Prelude hiding (add)

import Effect (Effect)
import Control.Monad.Rec.Class (tailRecM, Step(..))
import Control.Monad.State (class MonadState, evalState, execState, gets, modify_)
import Data.Foldable (class Foldable, fold)
import Data.List (List(..), (:))
import Language.Parse (unsafeParse, parseSyntax)
import Effect.Console (log)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..), uncurry)
import Language.Expr (Expr(..), syntaxToExpr)
import Language.Syntax (Syntax(..))
import Language.PrettyPrint (prettyPrint, sugar)
import Language.Name (Name, name_)
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
stepWith stop m = execState (tailRecM go unit) m
 where
  go _ = do
    { top } <- gets _.stack
    eval top =<< Heap.fetch top
    trace <- gets _.trace
    pure if stop trace
      then Done unit
      else Loop unit

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

  Closure env name e ->
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        node <- Node.instantiate (Cons arg env) e
        Heap.update app $ Pointer node
        Stack.discard
        Stack.replace node
        emit $ Substituted name arg
      Nothing -> do
        arg <- Heap.alloc $ Stuck $ StuckVar name
        node <- Node.instantiate (Cons arg env) e
        stuck <- Heap.alloc $ Stuck $ StuckBind name node
        Heap.update top $ Pointer stuck
        Stack.replace node
        emit $ WentUnder top

  Stuck _ ->
    Stack.peek 1 >>= case _ of
      Just app -> do
        arg <- fetchArg app
        stuck <- Heap.alloc $ Stuck $ StuckApp top arg
        Heap.update app $ Pointer stuck
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
    Closure env0 name e -> do
      env <- traverse formatNode env0
      pure $ Lambda name $ toSyntax (Var name : env) e
    Stuck term -> formatStuck term
    Pointer a -> formatNode a
    Global name _ -> pure $ Var name

toSyntax :: List Syntax -> Expr -> Syntax
toSyntax env = case _ of
  Bind n e -> Lambda n $ toSyntax (Var n : env) e
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

testBig :: Int -> String -> Effect Unit
testBig = testWith (stepWith interesting)

testSmall :: Int -> String -> Effect Unit
testSmall = testWith (stepWith $ const true)

testWith :: (Machine -> Machine) -> Int -> String -> Effect Unit
testWith next n = go 0 <<< new prelude <<< parse
 where
  go i m
    | i >= n = log $ "Did not halt in " <> show n <> " steps"
    | otherwise = do
        let { root, trace } = snapshot m
        log $ "Step " <> show i
        log $ "  trace: " <> trace
        log $ "  root:  " <> formatSyntax root
        case m.trace of
          Halted _ -> pure unit
          _ -> go (i + 1) $ next m

parse :: String -> Expr
parse = syntaxToExpr <<< unsafeParse parseSyntax

def :: String -> String -> Tuple Name Expr
def n = Tuple (name_ n) <<< parse

infix 1 def as =.

prelude :: Array (Tuple Name Expr)
prelude =
  [ "identity" =. "λx. x"
  , "const" =. "λx y. x"
  , "true" =. "λt f. t"
  , "false" =. "λt f. f"
  , "fix" =. "λf. (λx. f (x x)) (λy. f (y y))"
  , "and" =. "λx y. x y false"
  , "or" =. "λx y. x true y"
  , "not" =. "λx. x false true"
  , "foldr" =. "λf z l. l f z"
  , "all" =. "foldr and true"
  , "any" =. "foldr or false"
  , "cons" =. "λx xs cons nil. cons x (xs cons nil)"
  , "nil" =. "λx xs cons nil. nil"
  , "iterate" =. "fix (λnext. λf x. cons x (next f (f x)))"
  , "repeat" =. "fix (λnext. λx. cons x (next x))"
  , "pair" =. "λa b f. f a b"
  , "fst" =. "λp. p (λa b. a)"
  , "snd" =. "λp. p (λa b. b)"
  , "succ" =. "λn s z. s (n s z)"
  , "add" =. "λm n s z. m s (n s z)"
  , "mul" =. "λm n s z. m (n s) z"
  , "pred" =. "λn f x. n (λg. λh. h (g f)) (λu. x) (λu. u)"
  , "is-zero?" =. "λn. n (λx. false) true"
  ]
