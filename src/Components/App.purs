module Components.App
  ( mkApp
  ) where

import Lambda.Prelude

import Components.Alert (Level(..))
import Components.Alert as Alert
import Components.Controls as Controls
import Components.Definitions as Definitions
import Components.Expressions as Expressions
import Components.Footer as Footer
import Components.Help as Help
import Components.Input as Input
import Data.Array (concat, cons, filter, reverse, snoc)
import Data.Foldable (intercalate)
import Effect.Save (FileName(..), saveTextAs)
import Lambda.Language.Expr
  ( Expr
  , syntaxToExpr
  )
import Lambda.Language.Name (Name)
import Lambda.Language.Parse (formatParseError, parseAll, parseDefinition, parseEither, unsafeParse)
import Lambda.Language.PrettyPrint (Rep(..), Doc, prettyPrint, selectRep, toggleRep)
import Lambda.Language.Syntax (Definition, Syntax, defToDoc, defToSyntax)
import Lambda.Language.World (World)
import Lambda.Language.World as World
import Lambda.Machine (Machine)
import Lambda.Machine as Machine
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, mkReducer, useReducer)
import React.Basic.Hooks as Hooks

-- | Manages text-input, global definitions, semantic consistency
-- | (world field), and machine state, if any.
type State =
  { text :: String
  , defs :: Array Definition
  , world :: World
  , machine :: Maybe Machine
  , history :: Array (Doc String)
  , rep :: Rep
  , alert :: Maybe (Tuple Level JSX)
  }

mkApp :: Component {}
mkApp = do
  reducer <- mkReducer update
  component "App" \_ -> Hooks.do
    state /\ dispatch <- useReducer initialState reducer
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
          (R.h3_ [R.text "Evaluation"])
          (Controls.component
            { machine: state.machine
            , onStep: dispatch <<< Step
            , onClear: dispatch Clear
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

-- | Default set of global definitions
prelude :: Array Definition
prelude =
  unsafeParse parseDefinition <$>
    [ "identity x = x"
    , "const x y = x"
    , "fix f = (λx. f (x x)) (λy. f (y y))"
    , "true t f = t"
    , "false t f = f"
    , "and x y = x y false"
    , "or x y = x true y"
    , "succ n s z = s (n s z)"
    , "pred n f x = n (λg. λh. h (g f)) (λu. x) (λu. u)"
    , "add m n s z = m s (n s z)"
    , "mul m n s z = m (n s) z"
    , "is-zero? n = n (λx. false) true"
    , "foldr f z l = l f z"
    , "any = foldr or false"
    , "all = foldr and true"
    ]

-- | Set of user-driven events
data Action
  = ShowHelp
  | DismissAlert
  | UpdateText String
  | ParseText
  | DeleteDef Name
  | AddDef Definition
  | SetExpr Syntax
  | Step Machine
  | Clear
  | ToggleSugar

-- | Empty state with default global definitions
initialState :: State
initialState =
  { text: ""
  , defs: prelude
  , world: World.new $ defsToGlobals prelude
  , machine: Nothing
  , history: []
  , rep: Raw
  , alert: Nothing
  }

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
  ToggleSugar -> toggleSugar s

-- | Present the tutorial alert
showHelp :: State -> State
showHelp =
  _ {alert = pure $ Tuple Info $ Help.component {} }

-- | Dismiss any alert
dismissAlert :: State -> State
dismissAlert =
  _ {alert = Nothing}

-- | Update input text
updateText :: String -> State -> State
updateText text =
  _ {alert = Nothing, text = text}

-- | Attempt to parse the input. Input may be a definition, an
-- | expression, or malformed, in which case, we'll surface a
-- | syntax error.
parseText :: State -> State
parseText s
  | s.text == "" = s
parseText s =
  case parseAll parseEither s.text of
    Left error ->
      s {alert = pure $ alert Danger $ formatParseError s.text error}
    Right (Left def) ->
      addDef def s
    Right (Right syntax) ->
      setExpr syntax s

-- | Attempt to delete a global definition. If the input or any other
-- | global definition depends on it, we'll refuse to delete the
-- | definition and present an error message.
deleteDef :: Name -> State -> State
deleteDef name s = case World.undefine name s.world of
  Left err ->
    s {alert = pure $ alert Danger $ show err}
  Right world -> s
    { defs = deleteByName name s.defs
    , world = world
    , alert = Nothing
    }

-- | Remove a definition by name. Used when deleting a definition or
-- | redefining an extant definition.
deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = filter $ (_ /= name) <<< _.name

-- | Attempt to add a new global definition. If the definition depends
-- | on undefined names, present an error message.
addDef :: Definition -> State -> State
addDef def s = case World.define def.name expr s.world of
  Left err ->
    s {alert = pure $ alert Danger $ show err}
  Right world -> s
    { text = ""
    , defs = deleteByName def.name s.defs `snoc` def
    , world = world
    , alert = Nothing
    }
 where
  expr = syntaxToExpr $ defToSyntax def

-- | Attempt to set the main expression. If the expression depends on
-- | undefined names, present an error message.
setExpr :: Syntax -> State -> State
setExpr syntax s =
  case World.focus expr s.world of
    Left err ->
      s {alert = pure $ alert Danger $ show err}
    Right world -> s
      { text = ""
      , world = world
      , machine = machine
      , history = history
      , alert = Nothing
      }
 where
  expr = syntaxToExpr syntax
  globals = defsToGlobals s.defs
  history = pure $ prettyPrint syntax
  machine = pure $ Machine.new globals expr

-- | Convert `Definition`s to pairs of names and expressions.
defsToGlobals :: Array Definition -> Array (Tuple Name Expr)
defsToGlobals = map \def -> Tuple def.name $ syntaxToExpr $ defToSyntax def

-- | Do one step of evaluation and update the history.
step :: Machine -> State -> State
step m0 s =  s
  { machine = pure m
  , history = history
  }
 where
  m = Machine.step m0
  { root } = Machine.snapshot m
  history
    | Machine.halted m = s.history
    | otherwise = prettyPrint root `cons` s.history

-- | Unload the `Machine` and unset the main expression
clear :: State -> State
clear s = s
  { world = World.unfocus s.world
  , machine = Nothing
  , history = []
  }

-- | Toggle syntactic sugar for Church numerals and lists.
toggleSugar :: State -> State
toggleSugar s = s {rep = toggleRep s.rep}

-- | Download the state as a text file.
save :: State -> Effect Unit
save {rep, defs, history} =
  saveTextAs text $ FileName "evaluation.txt"
 where
  allDefs = concat
    [ map defToDoc defs
    , [pure ""]
    , reverse history
    ]
  text = intercalate "\n" $ flip selectRep rep <$> allDefs

-- | Create an alert element with the specified message.
alert :: Level -> String -> Tuple Level JSX
alert level message =
  Tuple level body
 where
  body = R.p
    { className: "preformatted"
    , children: [R.text message]
    }
