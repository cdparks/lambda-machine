module Language.Expr
  ( Expr(..)
  , Environment(..)
  , syntaxToExpr
  , exprToSyntax
  , freeVars
  ) where

import Prelude

import Data.Array (cons, unsafeIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Language.Name (Name, next)
import Language.PrettyPrint (class PrettyPrint, parensIf)
import Language.Syntax (Syntax(..))
import Partial.Unsafe (unsafePartial)

data Expr
  = Bound Int
  | Free Name
  | Bind Name (Set Name) Expr
  | App Expr Expr

type Environment a = Map.Map Name a

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

syntaxToExpr :: Syntax -> Expr
syntaxToExpr = alpha <<< go Map.empty
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

alpha :: {expr :: Expr, fvs :: Set Name} -> Expr
alpha x =
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

fresh :: Set Name -> Name -> {used :: Set Name, new :: Name}
fresh env n
  | n `Set.member` env = fresh env (next n)
  | otherwise = {used: Set.insert n env, new: n}

freeVars :: Expr -> Set Name
freeVars = case _ of
  Bound _ -> Set.empty
  Free n -> Set.singleton n
  Bind _ fvs b -> fvs
  App f a -> freeVars f <> freeVars a

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
