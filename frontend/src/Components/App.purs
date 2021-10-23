module Components.App
  ( mkApp
  ) where

import Lambda.Prelude hiding (State)

import Components.Alert (Level(..))
import Components.Alert as Alert
import Components.ConsistencyError as ConsistencyError
import Components.Controls as Controls
import Components.Definitions as Definitions
import Components.Expressions as Expressions
import Components.Footer as Footer
import Components.Help as Help
import Components.Input as Input
import Components.ParseError as ParseError
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.Grammar (pluralizeWith)
import Data.List as List
import Effect.Class (liftEffect)
import Effect.Save (FileName(..), saveTextAs)
import Lambda.Api as Api
import Lambda.Env as Env
import Lambda.Language.Definition (Definition(..))
import Lambda.Language.Definition as Definition
import Lambda.Language.Expression (Expression)
import Lambda.Language.History (History)
import Lambda.Language.History as History
import Lambda.Language.Name (Name)
import Lambda.Language.Nameless (Nameless)
import Lambda.Language.Nameless as Nameless
import Lambda.Language.Parser (parse)
import Lambda.Language.Parser as Parser
import Lambda.Language.Prelude as Prelude
import Lambda.Language.Pretty (Rep(..), toggle, pretty, toString)
import Lambda.Language.Program (Program)
import Lambda.Language.Snapshot.Code (Code)
import Lambda.Language.Statement (Statement(..))
import Lambda.Language.World (World)
import Lambda.Language.World as World
import Lambda.Machine (Machine)
import Lambda.Machine as Machine
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, mkReducer, useReducer)
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff (useAff)

type Props =
  { code :: Maybe Code
  }

-- | Manages text-input, global definitions, semantic consistency
-- | (world field), and machine state, if any.
type State =
  { text :: String
  , defs :: Array Definition
  , expr :: Maybe Expression
  , world :: World
  , machine :: Maybe Machine
  , history :: History
  , rep :: Rep
  , alert :: Maybe (Tuple Level JSX)
  , steps :: Maybe Int
  , request :: Maybe Request
  }

data Request
  = Fetch Code
  | Store Program

derive instance eqRequest :: Eq Request

mkApp :: Component Props
mkApp = do
  reducer <- mkReducer update
  component "App" \{ code: mCode }  -> Hooks.do
    state /\ dispatch <- useReducer (mkState mCode) reducer
    _ <- useAff state.request $ case state.request of
      Nothing -> pure unit
      Just (Store program) -> do
        result <- Api.store program
        liftEffect $ do
          dispatch $ case result of
            Left err -> ShowError err
            Right code -> ShowCode code
      Just (Fetch code) -> do
        result <- Api.fetch code
        liftEffect $ dispatch $ case result of
          Left err -> ShowError err
          Right program -> Load program

    pure $ R.div
      { className: "container"
      , children:
        [ row $ R.h2
          { className: "page-header"
          , children: [R.text "Lambda Machine"]
          }
        , row $ case state.alert of
            Nothing ->
              React.empty
            Just (Tuple level body) ->
              Alert.component
                { level
                , child: body
                , dismiss: dispatch DismissAlert
                }
        , row $ Input.component
          { text: state.text
          , onChange: dispatch <<< UpdateText
          , onSubmit: dispatch ParseText
          , onHelp: dispatch ShowHelp
          }
        , row $ R.h3_ [R.text "Definitions"]
        , row $ Definitions.component
          { defs: state.defs
          , rep: state.rep
          , onDelete: dispatch <<< DeleteDef
          }
        , split
          (stepsHeader state.steps)
          (Controls.component
            { machine: state.machine
            , onStep: dispatch <<< Step
            , onClear: dispatch Clear
            , onShare: dispatch Share
            , onSave: save state
            , onSugar: dispatch ToggleSugar
            , rep: state.rep
            }
          )
        , row $ Expressions.component
          { history: state.history
          , rep: state.rep
          }
        , row $ Footer.component {}
        ]
      }

-- | Row containing a single full-width element
row :: JSX -> JSX
row child =
  R.div
    { className: "row"
    , children: [R.div {className: "col-sm-12", children: [child]}]
    }

-- | Row containing two equal-width elements
split :: JSX -> JSX -> JSX
split lhs rhs =
  R.div
    { className: "row"
    , children:
      [ R.div {className: "col-sm-6", children: [lhs]}
      , R.div {className: "col-sm-6", children: [rhs]}
      ]
    }

-- | Set of user-driven events
data Action
  = ShowHelp
  | DismissAlert
  | UpdateText String
  | ParseText
  | DeleteDef Name
  | AddDef Definition
  | SetExpr Expression
  | Step Machine
  | Clear
  | ToggleSugar
  | ShowError Api.Error
  | ShowCode Code
  | Load Program
  | Share

-- | Empty state with default global definitions
mkState :: Maybe Code -> State
mkState code = empty
  { defs = Prelude.defs
  , world = World.new $ defsToGlobals Prelude.defs
  , request = Fetch <$> code
  }

empty :: State
empty =
  { text: ""
  , expr: Nothing
  , defs: []
  , world: World.empty
  , machine: Nothing
  , history: History.empty
  , rep: Raw
  , alert: Nothing
  , steps: Nothing
  , request: Nothing
  }

load :: Program -> State -> State
load { defs, expr } old =
  case result of
    Left alert -> old { alert = Just alert, request = Nothing }
    Right state -> state { request = Nothing }
 where
  define state = alertFail <<< flip addDef state
  focus state = alertFail <<< flip setExpr state
  result = do
    state <- foldM define empty defs
    maybe (pure state) (focus state) expr

alertFail :: State -> Either (Tuple Level JSX) State
alertFail state = case state.alert of
  Just alert -> Left alert
  Nothing -> Right state

-- | Update `State` in response to an `Action`
update :: State -> Action -> State
update s = case _ of
  ShowHelp -> showHelp s
  DismissAlert -> dismissAlert s
  UpdateText text -> updateText text s
  ParseText -> parseText s
  DeleteDef name -> deleteDef name s
  AddDef def -> addDef def s
  SetExpr expr -> setExpr expr s
  Step machine -> step machine s
  Clear -> clear s
  Share -> share s
  ToggleSugar -> toggleSugar s
  ShowError err -> showError err s
  ShowCode code -> showCode code s
  Load program -> load program s

-- | Present the tutorial alert
showHelp :: State -> State
showHelp = _
  { alert = pure $ Tuple Info $ Help.component {}
  }

-- | Show Api error
showError :: Api.Error -> State -> State
showError err = _
  { alert = pure $ Tuple Info $ R.p
    { children:
      [ R.text $ toString $ pretty Sugar err
      ]
    }
  , request = Nothing
  }

-- | Show link to machine that loads snapshot
showCode :: Code -> State -> State
showCode code = _
  { alert = pure $ Tuple Info $ R.p
    { children:
      [ R.text $ Env.host <> "/" <> unwrap code
      ]
    }
  , request = Nothing
  }

-- | Dismiss any alert
dismissAlert :: State -> State
dismissAlert = _
  { alert = Nothing
  }

-- | Update input text
updateText :: String -> State -> State
updateText text = _
  { alert = Nothing
  , text = text
  }

-- | Attempt to parse the input. Input may be a definition, an
-- | expression, or malformed, in which case, we'll surface a
-- | syntax error.
parseText :: State -> State
parseText s
  | s.text == "" = s
parseText s =
  case Parser.run parse s.text of
    Left error -> s
      { alert = pure
        $ Tuple Danger
        $ ParseError.component
          { input: s.text
          , error
          }
      }
    Right (Define def) ->
      addDef def s
    Right (Eval syntax) ->
      setExpr syntax s

-- | Attempt to delete a global definition. If the input or any other
-- | global definition depends on it, we'll refuse to delete the
-- | definition and present an error message.
deleteDef :: Name -> State -> State
deleteDef name s = case World.undefine name s.world of
  Left error -> s
    { alert = pure
      $ Tuple Danger
      $ ConsistencyError.component
        { error
        }
    }
  Right world -> s
    { defs = deleteByName name s.defs
    , world = world
    , alert = Nothing
    }

-- | Remove a definition by name. Used when deleting a definition or
-- | redefining an extant definition.
deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = Array.filter $ (_ /= name) <<< _.name <<< un Definition

-- | Attempt to add a new global definition. If the definition depends
-- | on undefined names, present an error message.
addDef :: Definition -> State -> State
addDef def s = case World.define name nameless s.world of
  Left error -> s
    { alert = pure
      $ Tuple Danger
      $ ConsistencyError.component
        { error
        }
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
setExpr syntax s =
  case World.focus expr s.world of
    Left error -> s
      { alert = pure
        $ Tuple Danger
        $ ConsistencyError.component
          { error
          }
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

-- | Convert `Definition`s to pairs of names and expressions.
defsToGlobals :: Array Definition -> Array (Tuple Name Nameless)
defsToGlobals = map \def ->
  let {expr, name} = Definition.split def
  in Tuple name $ Nameless.from expr

-- | Do one step of evaluation and update the history.
step :: Machine -> State -> State
step m0 s =  s
  { machine = pure m
  , history = history
  , steps = (1 + _) <$> s.steps
  }
 where
  m = Machine.step m0
  snapshot = Machine.snapshot m
  history = History.add snapshot s.history

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
toggleSugar :: State -> State
toggleSugar s = s {rep = toggle s.rep}

-- | Store program
share :: State -> State
share s = s { request = Just $ Store { defs, expr } }
 where
  { defs, expr } = s

-- | Download the state as a text file.
save :: State -> Effect Unit
save {rep, defs, history} =
  saveTextAs text $ FileName "evaluation.txt"
 where
  text = intercalate "\n" $ fold
    [ toString <<< pretty rep <$> defs
    , pure ""
    , Array.fromFoldable $ List.reverse $ History.toStrings rep history
    ]

stepsHeader :: Maybe Int -> JSX
stepsHeader = case _ of
  Nothing -> R.h3
    { className: "text-muted"
    , children: [R.text "Steps"]
    }
  Just n -> R.h3_
    [ R.text $ fold
      [ show n
      , " "
      , pluralizeWith "s" n "Step"
      ]
    ]
