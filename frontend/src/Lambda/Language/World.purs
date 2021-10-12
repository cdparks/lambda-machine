module Lambda.Language.World
  ( World(..)
  , Graph
  , ConsistencyError(..)
  , new
  , empty
  , define
  , undefine
  , focus
  , unfocus
  -- Exposed only for testing
  , Entity(..)
  ) where

import Lambda.Prelude hiding (add)

import Data.Array as Array
import Data.Grammar as Grammar
import Data.Map as Map
import Data.Set as Set
import Lambda.Language.Name (Name)
import Lambda.Language.Nameless (Nameless, freeVars)
import Partial.Unsafe (unsafeCrashWith)

-- | Representation of dependencies between global definitions and the
-- | main expression.
newtype World = World Graph

derive instance newtypeWorld :: Newtype World _
derive newtype instance eqWorld :: Eq World
derive newtype instance showWorld :: Show World

-- | Map an entity to the set of other entities that depend on it
type Graph = Map Entity (Set Entity)

-- | A `Entity` is either a global `Name`, or the root expression
-- | under evaluation.
data Entity
  = Global Name
  | Root

derive instance eqEntity :: Eq Entity
derive instance ordEntity :: Ord Entity

instance showEntity :: Show Entity where
  show = case _ of
    Global name -> show name
    Root -> "the input"

-- | The `World` is inconsistent if anything depends on an undefined `Name`, or
-- | if we attempt to delete a `Name` that is depended on by anything else.
data ConsistencyError
  = Undefined (Set Name)
  | CannotDelete Name (Set Entity)
  | CannotRedefine Name (Set Entity)

derive instance eqConsistencyError :: Eq ConsistencyError
derive instance ordConsistencyError :: Ord ConsistencyError

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
    CannotRedefine name deps -> fold
      [ "Cannot redefine "
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
empty = World Map.empty

-- | Create a new `World` given a list of top-level definitions. Crashes
-- | if any definition depends on `Name`s that did not appear before it.
new :: Array (Tuple Name Nameless) -> World
new prelude = case foldM (flip addGlobal) empty prelude of
  Left err -> unsafeCrashWith $ "Malformed prelude: " <> show err
  Right world -> world
 where
  addGlobal (Tuple name expr) = add (Global name) expr

-- | Attempt to define a new top-level definition. Fails if the
-- | definition mentions other undefined `Name`s.
define :: Name -> Nameless -> World -> Either ConsistencyError World
define name x world = do
  let refs = referencing name world
  if Set.size refs == 0
    then add (Global name) x world
    else Left $ CannotRedefine name refs

-- | Attempt to delete an existing top-level definition. Fails if any
-- | other definition still depends on it.
undefine :: Name -> World -> Either ConsistencyError World
undefine name world = do
  let refs = referencing name world
  if Set.size refs == 0
    then pure $ remove (Global name) world
    else Left $ CannotDelete name refs

-- | Set of `Entity`s that refer to this name
referencing :: Name -> World -> Set Entity
referencing name = fromMaybe Set.empty <<< Map.lookup (Global name) <<< un World

-- | Attempt to focus the `World` on a new root expression. Fails if
-- | the expression mentions any undefined `Name`s.
focus :: Nameless -> World -> Either ConsistencyError World
focus = add Root

-- | Remove root expression.
unfocus :: World -> World
unfocus = remove Root

-- | Internal operation for adding a new element to the `World`.
add :: Entity -> Nameless -> World -> Either ConsistencyError World
add entity expr world = do
  let
    fvs = freeVars expr
    missing = fvs `Set.difference` globals world
    isClosed = case entity of
      Global name -> Set.size missing == 0 || missing == Set.singleton name
      Root -> Set.size missing == 0
  if isClosed
    then pure $ combine world $ fromFreeVars entity fvs
    else Left $ Undefined missing

-- | Internal operation for removing an element from the `World`.
remove :: Entity -> World -> World
remove entity = World <<< map (Set.delete entity) <<< Map.delete entity <<< un World

-- | Set of top-level `Name`s.
globals :: World -> Set Name
globals = Set.mapMaybe name <<< Map.keys <<< un World
 where
  name = case _ of
    Root -> Nothing
    Global n -> pure n

-- | Create a minimal `World` based on an item's free variables
-- | assuming nothing else can depend on this item yet.
fromFreeVars :: Entity -> Set Name -> World
fromFreeVars entity fvs = World graph
 where
  graph = Map.fromFoldable $ Array.snoc others $ Tuple entity Set.empty
  others = do
    name <- Array.fromFoldable fvs
    pure $ Tuple (Global name) $ Set.singleton entity

-- | Monoidally combine `World`s by unioning `Map`s point-wise.
combine :: World -> World -> World
combine (World lhs) (World rhs) = World $ Map.unionWith Set.union lhs rhs
