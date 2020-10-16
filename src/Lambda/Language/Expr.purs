module Lambda.Language.Expr
  ( Expr(..)
  , syntaxToExpr
  , exprToSyntax
  , freeVars
  ) where

import Lambda.Prelude

import Data.Array (cons, unsafeIndex)
import Data.Map as Map
import Data.Set as Set
import Lambda.Language.Name (Name, next)
import Lambda.Language.PrettyPrint (class PrettyPrint, parensIf)
import Lambda.Language.Syntax (Syntax(..))
import Partial.Unsafe (unsafePartial)

-- | Locally nameless expression tree using zero-based De-Bruijn
-- | indexes. That is, `λx. x` is represented by `λx. 0`, and
-- | `λx. λy. x` is represented by `λx. λy. 1`. Binders are annotated
-- | with their source-level name for quoting back to `Syntax` as well
-- | as the set of free-variables they reference. The latter is used
-- | to avoid garbage-collecting global definitions.
data Expr
  = Bound Int
  | Free Name
  | Bind Name (Set Name) Expr
  | App Expr Expr

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

-- | Alpha-equivalence
instance eqExpr :: Eq Expr where
  eq lhs rhs = case lhs, rhs of
    Bound i, Bound j -> i == j
    Free n, Free m -> n == m
    Bind _ _ x, Bind _ _ y -> x == y
    App f a, App g b -> f == g && a == b
    _, _ -> false

-- | Convert plain `Syntax` to a locally-nameless `Expr`
syntaxToExpr :: Syntax -> Expr
syntaxToExpr = alphaInternal <<< go Map.empty
 where
  go env = case _ of
    Var n ->
      case Map.lookup n env of
        Nothing -> {expr: Free n, fvs: Set.singleton n}
        Just i -> {expr: Bound i, fvs: Set.empty}
    Lambda n body ->
      let
        shifted = Map.insert n 0 $ (_ + 1) <$> env
        {expr, fvs} = go shifted body
      in {expr: Bind n fvs expr, fvs}
    Apply f0 a0 ->
      let
        f = go env f0
        a = go env a0
      in {expr: App f.expr a.expr, fvs: f.fvs <> a.fvs}

-- | Alpha-convert an `Expr` such that no names are shadowed.
alpha :: Expr -> Expr
alpha expr = alphaInternal { expr, fvs: freeVars expr }

-- | Alpha-conversion when we already have an `Expr`'s free variables.
alphaInternal :: {expr :: Expr, fvs :: Set Name} -> Expr
alphaInternal x =
  loop x.fvs x.expr
 where
  loop env = case _ of
    Bound i ->
      Bound i
    Free n ->
      Free n
    Bind n fvs b ->
      let {used, new} = fresh env n
      in Bind new fvs $ loop used b
    App f a ->
      App (loop env f) (loop env a)

-- | Conjure a fresh name by appending a subscript
fresh :: Set Name -> Name -> {used :: Set Name, new :: Name}
fresh env n
  | n `Set.member` env = fresh env (next n)
  | otherwise = {used: Set.insert n env, new: n}

-- | Access an `Expr`'s precomputed free variables.
freeVars :: Expr -> Set Name
freeVars = case _ of
  Bound _ -> Set.empty
  Free n -> Set.singleton n
  Bind _ fvs b -> fvs
  App f a -> freeVars f <> freeVars a

-- | Quote an `Expr` back to `Syntax`. `Expr`s are alpha-converted,
-- | so the resulting AST will be equivalent, but may have slightly
-- | different names.
exprToSyntax :: Expr -> Syntax
exprToSyntax =
  loop []
 where
  loop env = case _ of
    Bound i ->
      Var $ unsafePartial $ env `unsafeIndex` i
    Free n ->
      Var n
    Bind n _ b ->
      Lambda n $ loop (n `cons` env) b
    App f a ->
      Apply (loop env f) (loop env a)

instance prettyPrintExpr :: PrettyPrint Expr where
  prettyPrint =
    walk false
   where
    walk inApp = case _ of
      Bound i ->
        pure $ show i
      Free n ->
        pure $ show n
      Bind _ _ b ->
        parensIf inApp $ pure "λ. " <> walk false b
      App f a ->
        walk true f <> pure " " <> walk true a

{-

-- PSC can TCO this, but it's a mess to read

syntaxToExpr :: Syntax -> Expr
syntaxToExpr syn = go Nil $ Down {env: Map.empty, syn}
 where
  go stack = case _ of
    Down {env, syn} -> case syn of
      Var n ->
        case Map.lookup n env of
          Nothing -> go stack $ Up {fvs: Set.singleton n, expr:  Free n}
          Just i -> go stack $ Up {fvs: Set.empty, expr: Bound i}
      Lambda n b ->
        let shifted = Map.insert n 0 $ (_ + 1) <$> env
        in go (MkBind n : stack) $ Down {env: shifted, syn: b}
      Apply f a ->
        go (GoRight a env : stack) $ Down {env, syn: f}

    Up {fvs, expr} -> case stack of
      Nil -> expr
      Cons x xs -> case x of
        MkBind n -> go xs $ Up {fvs, expr: Bind n fvs expr}
        GoRight syn env -> go (MkApp expr fvs : xs) $ Down {env, syn}
        MkApp f ffvs -> go xs $ Up {fvs: fvs <> ffvs, expr: App f expr}

data Direction
  = Down { env :: Map Name Int, syn :: Syntax }
  | Up { fvs :: Set Name, expr :: Expr }

data Step
  = MkBind Name
  | GoRight Syntax (Map Name Int)
  | MkApp Expr (Set Name)
-}
