module Components.App.State
  ( State
  , Status
  , new
  , empty
  , reduce
  , status
  , handle
  ) where

import Lambda.Prelude hiding (State)

import Components.App.Action (Action)
import Components.App.Action as Action
import Components.App.Alert (Alert)
import Components.App.Alert as Alert
import Components.App.Request (Request)
import Components.App.Request as Request
import Components.App.Response (Response)
import Components.App.Response as Response
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List as List
import Effect.Save (Filename(..))
import Effect.Save as Save
import Lambda.Api as Api
import Lambda.Language.Definition (Definition)
import Lambda.Language.Definition as Definition
import Lambda.Language.Expression (Expression)
import Lambda.Language.History (History)
import Lambda.Language.History as History
import Lambda.Language.Name (Name)
import Lambda.Language.Nameless (Nameless)
import Lambda.Language.Nameless as Nameless
import Lambda.Language.Parser as Parser
import Lambda.Language.Prelude as Prelude
import Lambda.Language.Pretty (Rep(..), pretty, toString)
import Lambda.Language.Pretty as Pretty
import Lambda.Language.Program (Program)
import Lambda.Language.Snapshot.Code (Code)
import Lambda.Language.Statement (Statement(..))
import Lambda.Language.World (World)
import Lambda.Language.World as World
import Lambda.Machine (Machine)
import Lambda.Machine as Machine

-- | Manages text-input, global definitions, semantic consistency
-- | (world field), and machine state, if any.
type State =
  { text :: String
  , defs :: Array Definition
  , expr :: Maybe Expression
  , world :: World
  , machine :: Maybe Machine
  , history :: History
  , steps :: Maybe Int
  , alert :: Maybe Alert
  , request :: Maybe Request
  , rep :: Rep
  }

-- | Used to enabled/disable controls
type Status =
  { isHalted :: Boolean
  , hasProgram :: Boolean
  , hasMachine :: Boolean
  }

-- | Extract application status
status :: State -> Status
status { defs, expr, machine } =
  { isHalted: maybe true _.halted machine
  , hasProgram: not $ Array.null defs && isNothing expr
  , hasMachine: isJust machine
  }

-- | Service asynchronous requests given `dispatch` and the current `State`
handle :: (Action -> Effect Unit) -> State -> Request -> Aff Unit
handle dispatch s = case _ of
  Request.Fetch code -> do
    result <- Api.fetch code
    done $ either Response.ApiError Response.Fetched result
  Request.Store -> do
    result <- Api.store $ extractProgram s
    done $ either Response.ApiError Response.Stored result
  Request.Save -> do
    result <- liftEffect $ Save.save
      { text: formatSession s
      , to: Filename "evaluation.txt"
      }
    done $ either Response.SaveError (const Response.Saved) result
 where
  done = liftEffect <<< dispatch <<< Action.Examine

-- | Empty state with default global definitions
-- | Optionally ready to load existing snapshot
new :: Maybe Code -> State
new code = empty
  { defs = Prelude.defs
  , world = World.new $ defsToGlobals Prelude.defs
  , request = Request.Fetch <$> code
  }

-- | Empty state with no global definitions
empty :: State
empty =
  { text: ""
  , expr: Nothing
  , defs: []
  , world: World.empty
  , machine: Nothing
  , history: History.empty
  , rep: Sugar
  , alert: Nothing
  , steps: Nothing
  , request: Nothing
  }

-- | Update `State` in response to an `Action`
reduce :: State -> Action -> State
reduce s = case _ of
  Action.Help -> help s
  Action.Dismiss -> dismiss s
  Action.Update text -> update text s
  Action.Parse -> parse s.text s
  Action.Delete name -> delete name s
  Action.Step -> step s
  Action.Clear -> clear s
  Action.Toggle -> toggle s
  Action.Enqueue request -> enqueue request s
  Action.Examine response -> examine response $ dismiss s

-- | Present the tutorial alert
help :: State -> State
help = _ { alert = pure Alert.Help  }

-- | Dismiss any alert
dismiss :: State -> State
dismiss = _ { alert = Nothing }

-- | Update input text
update :: String -> State -> State
update text = _ { alert = Nothing, text = text }

-- | Attempt to parse the input. Input may be a definition, an
-- | expression, or malformed, in which case, we'll surface a
-- | syntax error.
parse :: String -> State -> State
parse text s
  | text == "" = s
  | otherwise = case Parser.run Parser.parse text of
    Left error -> s
      { alert = pure $ Alert.Error $ Alert.ParseError text error
      }
    Right (Define def) -> addDef def s
    Right (Eval expr) -> setExpr expr s

-- | Attempt to delete a global definition. If the input or any other
-- | global definition depends on it, we'll refuse to delete the
-- | definition and present an error message.
delete :: Name -> State -> State
delete name s = case World.undefine name s.world of
  Left error -> s
    { alert = pure $ Alert.Error $ Alert.Inconsistent error
    }
  Right world -> s
    { defs = deleteByName name s.defs
    , world = world
    , alert = Nothing
    }

-- | Do one step of evaluation and update the history.
step :: State -> State
step s = fromMaybe s do
  machine <- Machine.step <$> s.machine
  pure $ s
    { machine = pure machine
    , steps = (1 + _) <$> s.steps
    , history = History.add (Machine.snapshot machine) s.history
    }

-- | Unload the `Machine` and unset the main expression
clear :: State -> State
clear s = s
  { world = World.unfocus s.world
  , expr = Nothing
  , machine = Nothing
  , history = History.empty
  , steps = Nothing
  }

-- | Toggle syntactic sugar for Church numerals and lists.
toggle :: State -> State
toggle s = s { rep = Pretty.toggle s.rep }

-- | Enqueue request for async processing
enqueue :: Request -> State -> State
enqueue request = _ { request = pure request }

-- | Enqueue request for async processing
examine :: Response -> State -> State
examine response s = case response of
  Response.Saved -> s
    { request = Nothing
    }
  Response.Fetched program -> load program s
  Response.Stored code -> s
    { alert = pure $ Alert.Link code
    , request = Nothing
    }
  Response.ApiError err -> s
    { alert = pure $ Alert.Error $ Alert.ApiError err
    , request = Nothing
    }
  Response.SaveError err -> s
    { alert = pure $ Alert.Error $ Alert.SaveError err
    , request = Nothing
    }

-- | Load `Program` into `State` or set an error alert
load :: Program -> State -> State
load { defs, expr } old = case result of
  Left alert -> old { alert = pure alert, request = Nothing }
  Right state -> state
 where
  define state = alertFail <<< flip addDef state
  focus state = alertFail <<< flip setExpr state

  result = do
    state <- foldM define empty defs
    maybe (pure state) (focus state) expr

  alertFail state = case state.alert of
    Just alert@(Alert.Error _) -> Left alert
    _ -> Right state

-- | Attempt to add a new global definition. If the definition depends
-- | on undefined names, present an error message.
addDef :: Definition -> State -> State
addDef def s = case World.define name nameless s.world of
  Left error -> s
    { alert = pure $ Alert.Error $ Alert.Inconsistent error
    }
  Right world -> s
    { text = ""
    , defs = deleteByName name s.defs `Array.snoc` def
    , world = world
    , alert = Nothing
    }
 where
  { expr, name } = Definition.split def
  nameless = Nameless.from expr

-- | Attempt to set the main expression. If the expression depends on
-- | undefined names, present an error message.
setExpr :: Expression -> State -> State
setExpr syntax s = case World.focus expr s.world of
  Left error -> s
    { alert = pure $ Alert.Error $ Alert.Inconsistent error
    }
  Right world ->
    let
      machine = Machine.new globals expr
      snapshot = Machine.snapshot machine
    in s
      { text = ""
      , expr = Just syntax
      , world = world
      , machine = pure machine
      , history = History.new snapshot
      , alert = Nothing
      , steps = Just 0
      }
 where
  expr = Nameless.from syntax
  globals = defsToGlobals s.defs

-- | Remove a definition by name. Used when deleting a definition or
-- | redefining an extant definition.
deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = Array.filter $ (_ /= name) <<< _.name <<< Definition.split

-- | Convert `Definition`s to pairs of names and expressions.
defsToGlobals :: Array Definition -> Array (Tuple Name Nameless)
defsToGlobals = map \def ->
  let {expr, name} = Definition.split def
  in Tuple name $ Nameless.from expr

-- | Pull `Program` out of `State` to store
extractProgram :: State -> Program
extractProgram { defs, expr } = { defs, expr }

-- | Format session as pretty-printed text
formatSession :: State -> String
formatSession { rep, defs, history } = layout
  (toString <<< pretty rep <$> defs)
  (Array.fromFoldable $ List.reverse $ History.toStrings rep history)
 where
  layout [] rhs = intercalate "\n" rhs
  layout lhs [] = intercalate "\n" lhs
  layout lhs rhs = intercalate "\n" $ fold [lhs, pure "", rhs]
