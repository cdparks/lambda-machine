module Language.Expr
  ( Expr(..)
  , Environment(..)
  , syntaxToExpr
  , exprToSyntax
  , freeVars
  , alpha
  ) where

import Prelude

import Data.Array (cons, unsafeIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Set (Set)
import Data.Set as Set
import Language.Name (Name, next)
import Language.PrettyPrint (class PrettyPrint, parensIf)
import Language.Syntax (Syntax(..))
import Partial.Unsafe (unsafePartial)

data Expr
  = Bound Int
  | Free Name
  | Bind Name Expr
  | App Expr Expr

type Environment a = Map.Map Name a

derive instance genericExpr :: Generic Expr _

instance showExpr :: Show Expr where
  show x = genericShow x

syntaxToExpr :: Syntax -> Expr
syntaxToExpr =
  alpha <<< loop Map.empty
 where
  loop env = case _ of
    Var n ->
      maybe (Free n) Bound (Map.lookup n env)
    Lambda n b ->
      let env' = Map.insert n 0 (map (_ + 1) env)
      in Bind n (loop env' b)
    Apply f a ->
      App (loop env f) (loop env a)

alpha :: Expr -> Expr
alpha e =
  loop (freeVars e) e
 where
  loop env = case _ of
    Bound i ->
      Bound i
    Free n ->
      Free n
    Bind n b ->
      let {used, new} = fresh env n
      in Bind new (loop used b)
    App f a ->
      App (loop env f) (loop env a)

fresh :: Set Name -> Name -> {used :: Set Name, new :: Name}
fresh env n
  | n `Set.member` env = fresh env (next n)
  | otherwise = {used:  Set.insert n env, new: n}

exprToSyntax :: Expr -> Syntax
exprToSyntax =
  loop []
 where
  loop env = case _ of
    Bound i ->
      Var $ unsafePartial $ env `unsafeIndex` i
    Free n ->
      Var n
    Bind n b ->
      Lambda n $ loop (n `cons` env) b
    App f a ->
      Apply (loop env f) (loop env a)

freeVars :: Expr -> Set Name
freeVars =
  walk Set.empty
 where
  walk vars = case _ of
    Bound _ ->
      vars
    Free n ->
      Set.insert n vars
    Bind _ b ->
      walk vars b
    App f a ->
      walk (walk vars f) a

instance prettyPrintExpr :: PrettyPrint Expr where
  prettyPrint =
    walk false
   where
    walk inApp = case _ of
      Bound i ->
        pure $ show i
      Free n ->
        pure $ show n
      Bind _ b ->
        parensIf inApp $ pure "λ. " <> walk false b
      App f a ->
        walk true f <> pure " " <> walk true a
