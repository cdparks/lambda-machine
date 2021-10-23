module Lambda.Language.Snapshot
  ( Snapshot(..)
  , module Error
  , from
  , to
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.Map as Map
import Lambda.Language.Definition (Definition)
import Lambda.Language.Definition as Definition
import Lambda.Language.Expression (Expression)
import Lambda.Language.Expression as Expression
import Lambda.Language.Name (Name)
import Lambda.Language.Snapshot.Error (Error(..))
import Lambda.Language.Snapshot.Error (Error(..)) as Error
import Lambda.Language.Snapshot.RPN (RPN(..))
import Lambda.Language.Snapshot.RPN as RPN
import Lambda.Language.Snapshot.Signature (Signature)
import Lambda.Language.Snapshot.Signature as Signature

-- | Compact representation of a machine state
newtype Snapshot = Snapshot
  { sig :: Signature
  , names :: Array Name
  , state :: Array Int
  }

derive instance newtypeSnapshot :: Newtype Snapshot _
derive newtype instance eqSnapshot :: Eq Snapshot
derive newtype instance showSnapshot :: Show Snapshot
derive newtype instance readForeignSnapshot :: ReadForeign Snapshot
derive newtype instance writeForeignSnapshot :: WriteForeign Snapshot

-- | Convert definitions and input to `Snapshot`
from :: Array Definition -> Maybe Expression -> Either Error Snapshot
from defs input = runIdentity $ runExceptT $ do
    state <- RPN.encode rpn
    pure $ Snapshot { sig, names, state }
 where
  sig = Signature.deflate defs
  Tuple names rpn = flatten defs input

-- | Attempt to load definitions and input from `Snapshot`
to :: Snapshot -> Either Error { defs :: Array Definition, input :: Maybe Expression }
to (Snapshot { sig, names, state }) = do
  case runIdentity $ runExceptT $ evalStateT act start of
    Left err -> Left err
    Right (Tuple userDefs input) -> do
      let defs = Signature.inflate sig <> userDefs
      pure { defs, input }
 where
  start = { stack: Nil, defs: [], names }
  act = replay =<< RPN.decode state

-- | Collect names and program to a sequence of RPN instructions
flatten :: Array Definition -> Maybe Expression -> Tuple (Array Name) (Array RPN)
flatten defs input = collect do
  flatDefs <- traverse flattenDef defs
  flatInput <- traverse flattenExpr input
  pure $ Array.concat flatDefs <> fromMaybe [] flatInput

-- | Flatten definition to a sequence of RPN instructions
flattenDef :: forall m. MonadState Dictionary m => Definition -> m (Array RPN)
flattenDef def = case Definition.split def of
  {id, name, expr}
    | id /= 0 -> pure []
    | otherwise -> do
      i <- save name
      ops <- flattenExpr expr
      pure $ ops <> [Define i]

-- | Flatten expression to a sequence of RPN instructions
flattenExpr :: forall m. MonadState Dictionary m => Expression -> m (Array RPN)
flattenExpr = case _ of
  Expression.Var n -> do
    i <- save n
    pure [Var i]
  Expression.Nat i ->
    pure [Nat i]
  Expression.List xs -> do
    let arr = Array.fromFoldable xs
    ys <- traverse flattenExpr arr
    pure $ Array.concat ys <> [Take $ Array.length arr]
  Expression.Apply (Expression.Var n) arg -> do
    i <- save n
    flatArg <- flattenExpr arg
    pure $ flatArg <> [AppVar i]
  Expression.Apply fun arg -> do
    flatArg <- flattenExpr arg
    flatFun <- flattenExpr fun
    pure $ flatArg <> flatFun <> [Apply]
  Expression.Lambda n body -> do
    i <- save n
    flatBody <- flattenExpr body
    pure $ flatBody <> [Lambda i]
  Expression.Highlight e -> flattenExpr e
  Expression.Cycle -> pure []

-- | Store names and cache their position
type Dictionary =
  { names :: Array Name
  , cache :: Map Name Int
  }

-- | Collect names
collect :: forall a. State Dictionary a -> Tuple (Array Name) a
collect act = Tuple names result
 where
  dict = { names: [], cache: Map.empty}
  Tuple result { names } = runState act dict

-- | Save or fetch a `Name` in the dictionary returning its index
save :: forall m. MonadState Dictionary m => Name -> m Int
save name = do
  { names, cache } <- get
  case Map.lookup name cache of
    Just i -> pure i
    Nothing -> do
      let i = Array.length names
      modify_ \s -> s
        { names = names <> [name]
        , cache = Map.insert name i cache
        }
      pure i

-- | Create definitions and input from sequence of encoded RPN instructions
replay :: forall m. MonadThrow Error m => MonadState Store m => Array RPN -> m (Tuple (Array Definition) (Maybe Expression))
replay ops = do
  traverse_ step ops
  { defs, stack } <- get
  case stack of
    Nil ->  pure $ Tuple defs Nothing
    Cons e Nil -> pure $ Tuple defs $ Just e
    _ -> throwError $ ExtraStackValues $ Array.length $ Array.fromFoldable stack
 where
  step = case _ of
    Var i -> do
      name <- fetch i
      push $ Expression.Var name
    Nat i -> do
      push $ Expression.Nat i
    Take i -> do
      xs <- take i
      push $ Expression.List xs
    Lambda i -> do
      body <- pop
      name <- fetch i
      push $ Expression.Lambda name body
    Define i -> do
      body <- pop
      name <- fetch i
      define $ Definition.join name body
    AppVar i -> do
      arg <- pop
      name <- fetch i
      push $ Expression.Apply (Expression.Var name) arg
    Apply -> do
      f <- pop
      arg <- pop
      push $ Expression.Apply f arg

-- | Stack of expressions, new definitions, name store
type Store =
  { stack :: List Expression
  , defs :: Array Definition
  , names :: Array Name
  }

-- | Pop top of stack or throw
pop :: forall m. MonadThrow Error m => MonadState Store m => m Expression
pop = do
  { stack } <- get
  case stack of
    Nil -> throwError $ StackUnderflow { op: "pop", wanted: 1, saw: 0 }
    Cons x xs -> do
      modify_ \s -> s { stack = xs }
      pure x

-- | Push expression on top of stack
push :: forall m. MonadState Store m => Expression -> m Unit
push e = modify_ $ \s -> s { stack = e : s.stack }

-- | Add definition to defs
define :: forall m. MonadState Store m => Definition -> m Unit
define def = do
  { defs } <- get
  modify_ \s -> s { defs = defs <> [def] }

-- | Take exactly n items from the stack or throw
take :: forall m. MonadThrow Error m => MonadState Store m => Int -> m (List Expression)
take n = do
  { stack } <- get
  { acc, stack: newStack } <- go { acc: Nil, stack } n
  modify_ \s -> s { stack = newStack }
  pure acc
 where
  go { acc, stack } i
    | i <= 0 = pure { acc, stack }
    | otherwise = case stack of
        Nil -> throwError $ StackUnderflow { op: "take", wanted: n, saw: n - i }
        Cons x xs -> go { acc: Cons x acc, stack: xs } $ i - 1

-- | Fetch name by index or throw
fetch :: forall m. MonadThrow Error m => MonadState Store m => Int -> m Name
fetch i = do
  { names } <- get
  case Array.index names i of
    Just x -> pure x
    Nothing -> throwError $ IndexOutOfRange i names
