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

import Control.Alt ((<|>))
import Data.Array (cons, unsafeIndex)
import Data.Foldable (intercalate, foldl, fold)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

import Data.Name (Name, next)
import Data.PrettyPrint (class PrettyPrint, parensIf)
import Data.Syntax (Syntax(..))

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
      let pair = fresh env n
      in Bind (snd pair) (loop (fst pair) b)
    App f a ->
      App (loop env f) (loop env a)

fresh :: Set.Set Name -> Name -> Tuple (Set.Set Name) Name
fresh env n
  | n `Set.member` env = fresh env (next n)
  | otherwise = Tuple (Set.insert n env) n

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

freeVars :: Expr -> Set.Set Name
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

globalNames :: forall a. Environment a -> Set.Set Name
globalNames = Set.fromFoldable <<< Map.keys

undefinedNames :: Expr -> Environment Expr -> Set.Set Name
undefinedNames expr env = freeVars expr `Set.difference` globalNames env

namesReferencing :: Name -> Environment Expr -> Set.Set Name
namesReferencing name =
  foldl go Set.empty <<< toList
 where
  toList :: forall a. Environment a -> List (Tuple Name a)
  toList = Map.toUnfoldable

  go keys (Tuple key expr)
    | name `Set.member` freeVars expr = Set.insert key keys
    | otherwise = keys

formatUndefinedError :: String -> Set.Set Name -> String
formatUndefinedError text missing =
  fold
    [ "No top-level definition for "
    , formatNames missing
    , " referenced in\n"
    , text
    ]

formatUndefinedWarning :: Name -> Set.Set Name -> String
formatUndefinedWarning name names =
  fold
    [ "Deleted definition "
    , show name
    , " is still referenced by "
    , formatNames names
    ]

formatNames :: Set.Set Name -> String
formatNames =
  intercalate ", " <<< map show <<< toList
 where
  toList :: forall a. Set.Set a -> List a
  toList = Set.toUnfoldable

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

step :: Environment Expr -> Expr -> Maybe Expr
step env = case _ of
  App (Bind n b) m ->
    pure $ substitute m b
  App f a ->
    (App <$> step env f <*> pure a) <|> (App <$> pure f <*> step env a)
  Bind n b ->
    Bind n <$> step env b
  Free n ->
    Map.lookup n env
  Bound i ->
    Nothing

substitute :: Expr -> Expr -> Expr
substitute v = walk 0
 where
  walk index = case _ of
    Bound i
      | i == index -> v
      | otherwise -> Bound i
    Free n ->
      Free n
    Bind n b ->
      Bind n $ walk (index + 1) b
    App f a ->
      App (walk index f) (walk index a)
