module Components.App
  ( mkApp
  ) where

import Prelude

import Components.Alert as Alert
import Components.Alert (Level(..))
import Components.Controls as Controls
import Components.Definitions as Definitions
import Components.Expressions as Expressions
import Components.Footer as Footer
import Components.Help as Help
import Components.Input as Input
import Data.Array (concat, cons, filter, reverse, snoc)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Save (saveTextAs)
import Lambda.Language.Expr
  ( Expr
  , syntaxToExpr
  )
import Lambda.Language.Name (Name)
import Lambda.Language.World (World)
import Lambda.Language.World as World
import Lambda.Language.Parse (formatParseError, parseAll, parseDefinition, parseEither, unsafeParse)
import Lambda.Language.PrettyPrint (Rep(..), Doc, prettyPrint, selectRep, toggleRep)
import Lambda.Language.Syntax (Definition, Syntax, defToDoc, defToSyntax)
import Lambda.Machine (Machine)
import Lambda.Machine as Machine
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, mkReducer, useReducer)
import React.Basic.Hooks as Hooks

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

row :: JSX -> JSX
row child =
  R.div
    { className: "row"
    , children: [R.div {className: "col-sm-12", children: [child]}]
    }

split :: JSX -> JSX -> JSX
split lhs rhs =
  R.div
    { className: "row"
    , children:
      [ R.div {className: "col-sm-6", children: [lhs]}
      , R.div {className: "col-sm-6", children: [rhs]}
      ]
    }

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

showHelp :: State -> State
showHelp =
  _ {alert = pure $ Tuple Info $ Help.component {} }

dismissAlert :: State -> State
dismissAlert =
  _ {alert = Nothing}

updateText :: String -> State -> State
updateText text =
  _ {alert = Nothing, text = text}

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

deleteDef :: Name -> State -> State
deleteDef name s = case World.undefine name s.world of
  Left err ->
    s {alert = pure $ alert Danger $ show err}
  Right world -> s
    { defs = deleteByName name s.defs
    , world = world
    , machine = Machine.remove name <$> s.machine
    , alert = Nothing
    }

deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = filter $ (_ /= name) <<< _.name

addDef :: Definition -> State -> State
addDef def s = case World.define def.name expr s.world of
  Left err ->
    s {alert = pure $ alert Danger $ show err}
  Right world -> s
    { text = ""
    , defs = deleteByName def.name s.defs `snoc` def
    , world = world
    , machine = Machine.add def.name expr <$> s.machine
    , alert = Nothing
    }
 where
  expr = syntaxToExpr $ defToSyntax def

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

defsToGlobals :: Array Definition -> Array (Tuple Name Expr)
defsToGlobals = map \def -> Tuple def.name $ syntaxToExpr $ defToSyntax def

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

clear :: State -> State
clear s = s
  { world = World.unfocus s.world
  , machine = Nothing
  , history = []
  }

toggleSugar :: State -> State
toggleSugar s = s {rep = toggleRep s.rep}

save :: State -> Effect Unit
save {rep, defs, history} =
  saveTextAs text "evaluation.txt"
 where
  allDefs = concat
    [ map defToDoc defs
    , [pure ""]
    , reverse history
    ]
  text = intercalate "\n" $ flip selectRep rep <$> allDefs

alert :: Level -> String -> Tuple Level JSX
alert level message =
  Tuple level body
 where
  body = R.p
    { className: "preformatted"
    , children: [R.text message]
    }
