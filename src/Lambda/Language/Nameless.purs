module Lambda.Language.Nameless
  ( Expression(..)
  , from
  , syntax
  , freeVars
  , alpha
  ) where

import Lambda.Prelude

import Data.Array (cons, unsafeIndex)
import Data.Map as Map
import Data.Set as Set
import Lambda.Language.Name (Name, next)
import Lambda.Language.Pretty (class Pretty, parensIf, text)
import Lambda.Language.Syntax as Syntax
import Partial.Unsafe (unsafePartial, unsafeCrashWith)

-- | Locally nameless expression tree using zero-based De-Bruijn
-- | indexes. That is, `λx. x` is represented by `λx. 0`, and
-- | `λx. λy. x` is represented by `λx. λy. 1`. Binders are annotated
-- | with their source-level name for quoting back to `Syntax` as well
-- | as the set of free-variables they reference. The latter is used
-- | to avoid garbage-collecting global definitions.
data Expression
  = Bound Int
  | Free Name
  | Lambda Name (Set Name) Expression
  | Apply Expression Expression

derive instance genericExpression :: Generic Expression _

instance showExpression :: Show Expression where
  show x = genericShow x

-- | Alpha-equivalence
instance eqExpression :: Eq Expression where
  eq lhs rhs = case lhs, rhs of
    Bound i, Bound j -> i == j
    Free n, Free m -> n == m
    Lambda _ _ x, Lambda _ _ y -> x == y
    Apply f a, Apply g b -> f == g && a == b
    _, _ -> false

-- | Create a locally-nameless `Expression` from an AST
from :: Syntax.Expression -> Expression
from = alphaInternal <<< go Map.empty
 where
  go env = case _ of
    Syntax.Var n ->
      case Map.lookup n env of
        Nothing -> {expr: Free n, fvs: Set.singleton n}
        Just i -> {expr: Bound i, fvs: Set.empty}
    Syntax.Lambda n body ->
      let
        shifted = Map.insert n 0 $ (_ + 1) <$> env
        {expr, fvs} = go shifted body
      in {expr: Lambda n fvs expr, fvs}
    Syntax.Apply f0 a0 ->
      let
        f = go env f0
        a = go env a0
      in {expr: Apply f.expr a.expr, fvs: f.fvs <> a.fvs}
    Syntax.Highlight x -> go env x
    Syntax.Cycle -> unsafeCrashWith "Parser should never produce a cycle"

-- | Alpha-convert an `Expression` such that no names are shadowed.
alpha :: Expression -> Expression
alpha expr = alphaInternal { expr, fvs: freeVars expr }

-- | Alpha-conversion when we already have an `Expression`'s free variables.
alphaInternal :: {expr :: Expression, fvs :: Set Name} -> Expression
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

-- | Access an `Expression`'s precomputed free variables.
freeVars :: Expression -> Set Name
freeVars = case _ of
  Bound _ -> Set.empty
  Free n -> Set.singleton n
  Lambda _ fvs _ -> fvs
  Apply f a -> freeVars f <> freeVars a

-- | Quote an `Expression` back to `Syntax.Expression`. `Expression`s
-- | are alpha-converted, so the resulting AST will be equivalent, but
-- | may have slightly different names.
syntax :: Expression -> Syntax.Expression
syntax =
  loop []
 where
  loop env = case _ of
    Bound i ->
      Syntax.Var $ unsafePartial $ env `unsafeIndex` i
    Free n ->
      Syntax.Var n
    Lambda n _ b ->
      Syntax.Lambda n $ loop (n `cons` env) b
    Apply f a ->
      Syntax.Apply (loop env f) (loop env a)

instance prettyExpression :: Pretty Expression where
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
