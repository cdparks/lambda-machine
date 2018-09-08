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

import Prelude (class Show, map, otherwise, pure, show, (+), (<$>), (<*>), (<>), (==), (>>>))
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..), fst, snd)
import Control.Alt ((<|>))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import Data.Map as Map
import Data.Set as Set
import Data.List (List)
import Data.Array (cons, unsafeIndex)
import Data.Foldable (intercalate, foldl)
import Partial.Unsafe (unsafePartial)

import Data.Syntax (Syntax(..))
import Data.PrettyPrint (class PrettyPrint, parensIf)
import Data.Name (Name, next)

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
syntaxToExpr = loop Map.empty >>> alpha
 where
  loop env s =
    case s of
      Var n ->
        maybe (Free n) Bound (Map.lookup n env)
      Lambda n b ->
        let env' = Map.insert n 0 (map (_ + 1) env)
        in Bind n (loop env' b)
      Apply f a ->
        App (loop env f) (loop env a)

alpha :: Expr -> Expr
alpha e0 = loop (freeVars e0) e0
 where
  loop env e1 =
    case e1 of
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
  | otherwise          = Tuple (Set.insert n env) n

exprToSyntax :: Expr -> Syntax
exprToSyntax = loop []
 where
  loop env e =
    case e of
      Bound i ->
        Var (unsafePartial (env `unsafeIndex` i))
      Free n ->
        Var n
      Bind n b ->
        let env' = n `cons` env
        in Lambda n (loop env' b)
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
namesReferencing name =
  toList >>> foldl step' Set.empty
 where
  toList :: forall a. Environment a -> List (Tuple Name a)
  toList = Map.toUnfoldable
  step' keys (Tuple key expr)
    | name `Set.member` freeVars expr = Set.insert key keys
    | otherwise                       = keys

formatUndefinedError :: String -> Set.Set Name -> String
formatUndefinedError text missing =
  "No top-level definition for " <> formatNames missing <> " referenced in\n" <> text

formatUndefinedWarning :: Name -> Set.Set Name -> String
formatUndefinedWarning name names =
  "Deleted definition " <> show name <> " is still referenced by " <> formatNames names

formatNames :: Set.Set Name -> String
formatNames =
  toList >>> map show >>> intercalate ", "
 where
  toList :: forall a. Set.Set a -> List a
  toList = Set.toUnfoldable

instance prettyPrintExpr :: PrettyPrint Expr where
  prettyPrint = walk false
   where
    walk inApp e =
      case e of
        Bound i ->
          pure (show i)
        Free n ->
          pure (show n)
        Bind _ b ->
          parensIf inApp (pure "λ. " <> walk false b)
        App f a ->
          walk true f <> pure " " <> walk true a

step :: Environment Expr -> Expr -> Maybe Expr
step env e =
  case e of
    App (Bind n b) m ->
      pure (substitute m b)
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
  walk index e =
    case e of
      Bound i
        | i == index -> v
        | otherwise  -> Bound i
      Free n -> Free n
      Bind n b -> Bind n (walk (index + 1) b)
      App f a -> App (walk index f) (walk index a)

