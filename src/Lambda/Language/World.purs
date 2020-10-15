module Lambda.Language.World
  ( World
  , ConsistencyError(..)
  , formatError
  , new
  , define
  , undefine
  , focus
  , unfocus
  -- Exposed only for testing
  , Dependency(..)
  ) where

import Lambda.Prelude hiding (add)

import Data.Array as Array
import Data.Grammar as Grammar
import Data.Map as Map
import Data.Set as Set
import Lambda.Language.Expr (Expr, freeVars)
import Lambda.Language.Name (Name)
import Partial.Unsafe (unsafeCrashWith)

type World =
  { nameToDeps :: Map Name (Set Dependency)
  , depToNames :: Map Dependency (Set Name)
  }

data Dependency
  = Global Name
  | Root

derive instance genericDependency :: Generic Dependency _

instance eqDependency :: Eq Dependency where
  eq x = genericEq x

instance ordDependency :: Ord Dependency where
  compare x = genericCompare x

instance showDependency :: Show Dependency where
  show = case _ of
    Global name -> show name
    Root -> "the input"

data ConsistencyError
  = Undefined (Set Name)
  | CannotDelete Name (Set Dependency)

derive instance genericConsistencyError :: Generic ConsistencyError _

instance eqConsistencyError :: Eq ConsistencyError where
  eq x = genericEq x

instance ordConsistencyError :: Ord ConsistencyError where
  compare x = genericCompare x

instance showConsistencyError :: Show ConsistencyError where
  show = formatError

formatError :: ConsistencyError -> String
formatError = case _ of
  Undefined missing -> fold
    [ "No top-level "
    , Grammar.pluralizeWith "s" (Set.size missing) "definition"
    , " for "
    , Grammar.joinWith "and" $ show <$> Array.fromFoldable missing
    ]
  CannotDelete name deps -> fold
    [ "Cannot delete "
    , show name
    , " because it's still referenced by "
    , Grammar.joinWith "and" $ show <$> Array.fromFoldable deps
    ]

empty :: World
empty =
  { nameToDeps: Map.empty
  , depToNames: Map.empty
  }

new :: Array (Tuple Name Expr) -> World
new prelude = case foldM (flip addGlobal) empty prelude of
  Left err -> unsafeCrashWith $ "Malformed prelude: " <> formatError err
  Right world -> world
 where
  addGlobal (Tuple name expr) = add (Global name) expr

define :: Name -> Expr -> World -> Either ConsistencyError World
define name = add $ Global name

undefine :: Name -> World -> Either ConsistencyError World
undefine name world = do
  let deps = fromMaybe Set.empty $ Map.lookup name world.nameToDeps
  if Set.size deps == 0
    then pure $ remove (Global name) world
    else Left $ CannotDelete name deps

focus :: Expr -> World -> Either ConsistencyError World
focus = add Root

unfocus :: World -> World
unfocus = remove Root

add :: Dependency -> Expr -> World -> Either ConsistencyError World
add dep expr world = do
  let
    newWorld = remove dep world
    fvs = freeVars expr
    missing = fvs `Set.difference` globals newWorld
  if Set.size missing == 0
    then pure $ combine newWorld $ fromFreeVars dep fvs
    else Left $ Undefined missing

remove :: Dependency -> World -> World
remove dep world@{ nameToDeps, depToNames } =
  case Map.lookup dep depToNames of
    Nothing -> world
    Just names ->
      { depToNames: Map.delete dep depToNames
      , nameToDeps: foldl eliminate nameToDepsWithoutDep names
      }
 where
  eliminate m name = Map.update (pure <<< Set.delete dep) name m
  nameToDepsWithoutDep = case dep of
    Global name -> Map.delete name nameToDeps
    Root -> nameToDeps

globals :: World -> Set Name
globals = Map.keys <<< _.nameToDeps

fromFreeVars :: Dependency -> Set Name -> World
fromFreeVars dep fvs =
  { nameToDeps
  , depToNames
  }
 where
  nameToDeps = Map.fromFoldable $ case dep of
    Root -> others
    Global name -> Array.snoc others $ Tuple name Set.empty
  others = do
    name <- Array.fromFoldable fvs
    pure $ Tuple name $ Set.singleton dep
  depToNames = Map.singleton dep fvs

combine :: World -> World -> World
combine lhs rhs =
  { nameToDeps: Map.unionWith Set.union lhs.nameToDeps rhs.nameToDeps
  , depToNames: Map.unionWith Set.union lhs.depToNames rhs.depToNames
  }
