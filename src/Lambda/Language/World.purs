module Lambda.Language.World
  ( World
  , ConsistencyError(..)
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
import Lambda.Language.Name (Name)
import Lambda.Language.Nameless (Expression, freeVars)
import Partial.Unsafe (unsafeCrashWith)

-- | Representation of dependencies between global definitions and the
-- | main expression.
type World =
  { nameToDeps :: Map Name (Set Dependency)
  , depToNames :: Map Dependency (Set Name)
  }

-- | A `Dependency` is either a global `Name`, or the root expression
-- | under evaluation.
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

-- | The `World` is inconsistent if anything depends on an undefined `Name`, or
-- | if we attempt to delete a `Name` that is depended on by anything else.
data ConsistencyError
  = Undefined (Set Name)
  | CannotDelete Name (Set Dependency)

derive instance genericConsistencyError :: Generic ConsistencyError _

instance eqConsistencyError :: Eq ConsistencyError where
  eq x = genericEq x

instance ordConsistencyError :: Ord ConsistencyError where
  compare x = genericCompare x

instance showConsistencyError :: Show ConsistencyError where
  show = case _ of
    Undefined missing -> fold
      [ "No top-level "
      , Grammar.pluralizeWith "s" (Set.size missing) "definition"
      , " for "
      , join missing
      ]
    CannotDelete name deps -> fold
      [ "Cannot delete "
      , show name
      , " because it's still referenced by "
      , join deps
      ]
   where
    join :: forall a f. Show a => Foldable f => f a -> String
    join = Grammar.joinWith {inject: identity, conjunction: "and" }
      <<< map (show <<< show)
      <<< Array.fromFoldable

-- | An empty `World` has no definitions
empty :: World
empty =
  { nameToDeps: Map.empty
  , depToNames: Map.empty
  }

-- | Create a new `World` given a list of top-level definitions. Crashes
-- | if any definition depends on `Name`s that did not appear before it.
new :: Array (Tuple Name Expression) -> World
new prelude = case foldM (flip addGlobal) empty prelude of
  Left err -> unsafeCrashWith $ "Malformed prelude: " <> show err
  Right world -> world
 where
  addGlobal (Tuple name expr) = add (Global name) expr

-- | Attempt to define a new top-level definition. Fails if the
-- | definition mentions other undefined `Name`s.
define :: Name -> Expression -> World -> Either ConsistencyError World
define name = add $ Global name

-- | Attempt to delete an existing top-level definition. Fails if any
-- | other definition still depends on it.
undefine :: Name -> World -> Either ConsistencyError World
undefine name world = do
  let deps = fromMaybe Set.empty $ Map.lookup name world.nameToDeps
  if Set.size deps == 0
    then pure $ remove (Global name) world
    else Left $ CannotDelete name deps

-- | Attempt to focus the `World` on a new root expression. Fails if
-- | the expression mentions any undefined `Name`s.
focus :: Expression -> World -> Either ConsistencyError World
focus = add Root

-- | Remove root expression.
unfocus :: World -> World
unfocus = remove Root

-- | Internal operation for adding a new element to the `World`.
add :: Dependency -> Expression -> World -> Either ConsistencyError World
add dep expr world = do
  let
    newWorld = remove dep world
    fvs = freeVars expr
    missing = fvs `Set.difference` globals newWorld
  if Set.size missing == 0
    then pure $ combine newWorld $ fromFreeVars dep fvs
    else Left $ Undefined missing

-- | Internal operation for removing an element from the `World`.
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

-- | Set of top-level `Name`s.
globals :: World -> Set Name
globals = Map.keys <<< _.nameToDeps

-- | Create a minimal `World` based on an item's free variables
-- | assuming nothing else can depend on this item yet.
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

-- | Monoidally combine `World`s by unioning `Map`s point-wise.
combine :: World -> World -> World
combine lhs rhs =
  { nameToDeps: Map.unionWith Set.union lhs.nameToDeps rhs.nameToDeps
  , depToNames: Map.unionWith Set.union lhs.depToNames rhs.depToNames
  }
