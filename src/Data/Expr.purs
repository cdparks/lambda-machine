module Data.Expr
  ( Expr(..)
  , Environment(..)
  , syntaxToExpr
  , exprToSyntax
  , freeVars
  , globalNames
  , undefinedNames
  , namesReferencing
  , formatUndefinedError
  , formatUndefinedWarning
  , substitute
  , step
  , alpha
  ) where

import Prelude
import Data.Maybe
import Data.Tuple
import Control.Alt ((<|>))
import Data.Generic

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Array (cons)
import Data.Array.Unsafe (unsafeIndex)
import Data.Foldable (intercalate, foldl)

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Data.Syntax
import Data.PrettyPrint
import Data.Name

data Expr
  = Bound Int
  | Free Name
  | Bind Name Expr
  | App Expr Expr

type Environment a = Map.Map Name a

derive instance genericExpr :: Generic Expr
instance showExpr :: Show Expr where
  show = gShow

syntaxToExpr :: Syntax -> Expr
syntaxToExpr = loop Map.empty >>> alpha
 where
  loop env a =
    case a of
      Var n ->
        maybe (Free n) Bound (Map.lookup n env)
      Lambda n b ->
        let env' = Map.insert n 0 (map (+1) env)
        in Bind n (loop env' b)
      Apply f a ->
        App (loop env f) (loop env a)

alpha :: Expr -> Expr
alpha e = loop (freeVars e) e
 where
  loop env e =
    case e of
      Bound i ->
        Bound i
      Free n ->
        Free n
      Bind n e ->
        let pair = fresh env n
        in Bind (snd pair) (loop (fst pair) e)
      App f a ->
        App (loop env f) (loop env a)

fresh :: Set.Set Name -> Name -> Tuple (Set.Set Name) Name
fresh env n
  | n `Set.member` env = fresh env (next n)
  | otherwise          = Tuple (Set.insert n env) n

exprToSyntax :: Expr -> Syntax
exprToSyntax = loop []
 where
  loop env e =
    case e of
      Bound i ->
        Var (env `unsafeIndex` i)
      Free n ->
        Var n
      Bind n e ->
        let env' = n `cons` env
        in Lambda n (loop env' e)
      App f a ->
        Apply (loop env f) (loop env a)

freeVars :: Expr -> Set.Set Name
freeVars = walk Set.empty
 where
  walk vars e =
    case e of
      Bound _ ->
        vars
      Free n ->
        Set.insert n vars
      Bind _ b ->
        walk vars b
      App f a ->
        walk (walk vars f) a

globalNames :: forall a. Environment a -> Set.Set Name
globalNames = Map.keys >>> Set.fromFoldable

undefinedNames :: Expr -> Environment Expr -> Set.Set Name
undefinedNames expr env = freeVars expr `Set.difference` globalNames env

namesReferencing :: Name -> Environment Expr -> Set.Set Name
namesReferencing name = Map.toList >>> foldl step Set.empty
 where
  step keys (Tuple key expr)
    | name `Set.member` freeVars expr = Set.insert key keys
    | otherwise                       = keys

formatUndefinedError :: String -> Set.Set Name -> String
formatUndefinedError text missing =
  "No top-level definition for " <> formatNames missing <> " referenced in\n" <> text

formatUndefinedWarning :: Name -> Set.Set Name -> String
formatUndefinedWarning name names =
  "Deleted definition " <> show name <> " is still referenced by " <> formatNames names

formatNames :: Set.Set Name -> String
formatNames = Set.toList >>> map show >>> intercalate ", "

instance prettyPrintExpr :: PrettyPrint Expr where
  prettyPrint = walk false
   where
    walk inApp e =
      case e of
        Bound i ->
          show i
        Free n ->
          show n
        Bind _ b ->
          parensIf inApp ("λ. " <> walk false b)
        App f a ->
          walk true f <> " " <> walk true a

step :: Environment Expr -> Expr -> Maybe Expr
step env e =
  case e of
    App (Bind n e) m ->
      return (substitute m e)
    App f a ->
      (App <$> step env f <*> pure a) <|> (App <$> pure f <*> step env a)
    Bind n e ->
      Bind n <$> step env e
    Free n ->
      Map.lookup n env
    Bound i ->
      Nothing

substitute :: Expr -> Expr -> Expr
substitute v = walk 0
 where
  walk index e =
    case e of
      Bound i
        | i == index -> v
        | otherwise  -> Bound i
      Free n -> Free n
      Bind n b -> Bind n (walk (index + 1) b)
      App f a -> App (walk index f) (walk index a)

