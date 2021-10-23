module Lambda.Language.Nameless
  ( Nameless(..)
  , from
  , freeVars
  , alpha
  ) where

import Lambda.Prelude

import Data.Map as Map
import Data.Set as Set
import Lambda.Language.Expression (Expression, encodeNat, encodeList)
import Lambda.Language.Expression as Expression
import Lambda.Language.Name (Name, next)
import Lambda.Language.Name as Name
import Lambda.Language.Pretty (class Pretty, parensIf, text)
import Partial.Unsafe (unsafeCrashWith)

-- | Locally nameless expression tree using zero-based De-Bruijn
-- | indexes. That is, `λx. x` is represented by `λx. 0`, and
-- | `λx. λy. x` is represented by `λx. λy. 1`. Binders are annotated
-- | with their source-level name for quoting back to an `Expression`
-- | as well as the set of free-variables they reference. The latter
-- | is used to avoid garbage-collecting global definitions.
data Nameless
  = Bound Int
  | Free Name
  | Lambda Name (Set Name) Nameless
  | Apply Nameless Nameless

derive instance genericNameless :: Generic Nameless _

instance showNameless :: Show Nameless where
  show x = genericShow x

-- | Alpha-equivalence
instance eqNameless :: Eq Nameless where
  eq lhs rhs = case lhs, rhs of
    Bound i, Bound j -> i == j
    Free n, Free m -> n == m
    Lambda _ _ x, Lambda _ _ y -> x == y
    Apply f a, Apply g b -> f == g && a == b
    _, _ -> false

-- | Alph-equivalent expressions should have the same hash
instance hashableNameless :: Hashable Nameless where
  hash = case _ of
    Bound i -> hash i
    Free n -> hash n
    Lambda _ _ b -> hash b
    Apply f a -> hash $ Tuple f a

-- | Create a locally-nameless expression from an AST
-- | Also eliminates literals
from :: Expression -> Nameless
from = alphaInternal <<< go Map.empty
 where
  go env = case _ of
    Expression.Var n ->
      case Map.lookup n env of
        Nothing -> {expr: Free n, fvs: Set.singleton n}
        Just i -> {expr: Bound i, fvs: Set.empty}
    Expression.Nat i -> go env $ encodeNat i
    Expression.List xs -> go env $  do
      let names = Map.keys env
      let {new: cons} = fresh names $ Name.from "cons"
      let {new: nil} = fresh names $ Name.from "nil"
      encodeList cons nil xs
    Expression.Lambda n body ->
      let
        shifted = Map.insert n 0 $ (_ + 1) <$> env
        {expr, fvs} = go shifted body
      in {expr: Lambda n fvs expr, fvs}
    Expression.Apply f0 a0 ->
      let
        f = go env f0
        a = go env a0
      in {expr: Apply f.expr a.expr, fvs: f.fvs <> a.fvs}
    Expression.Highlight x -> go env x
    Expression.Cycle -> unsafeCrashWith "Parser should never produce a cycle"

-- | Alpha-convert an nameless expression such that no names are shadowed.
alpha :: Nameless -> Nameless
alpha expr = alphaInternal { expr, fvs: freeVars expr }

-- | Alpha-conversion when we already have an nameless expression's free variables
alphaInternal :: {expr :: Nameless, fvs :: Set Name} -> Nameless
alphaInternal x =
  loop x.fvs x.expr
 where
  loop env = case _ of
    Bound i ->
      Bound i
    Free n ->
      Free n
    Lambda n fvs b ->
      let {used, new} = fresh env n
      in Lambda new fvs $ loop used b
    Apply f a ->
      Apply (loop env f) (loop env a)

-- | Conjure a fresh name by appending a subscript
fresh :: Set Name -> Name -> {used :: Set Name, new :: Name}
fresh env n
  | n `Set.member` env = fresh env (next n)
  | otherwise = {used: Set.insert n env, new: n}

-- | Access a nameless expression's precomputed free variables.
freeVars :: Nameless -> Set Name
freeVars = case _ of
  Bound _ -> Set.empty
  Free n -> Set.singleton n
  Lambda _ fvs _ -> fvs
  Apply f a -> freeVars f <> freeVars a

instance prettyNameless :: Pretty Nameless where
  pretty _ =
    walk false
   where
    walk inApp = case _ of
      Bound i ->
        text $ show i
      Free n ->
        text $ show n
      Lambda _ _ b ->
        parensIf inApp $ text "λ. " <> walk false b
      Apply f a ->
        walk true f <> text " " <> walk true a
