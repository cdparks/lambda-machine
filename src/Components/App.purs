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
import Control.MonadZero (guard)
import Data.Array (concat, cons, filter, reverse, snoc)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Save (saveTextAs)
import Language.Expr
  ( Environment
  , Expr
  , alpha
  , exprToSyntax
  , formatUndefinedError
  , formatUndefinedWarning
  , namesReferencing
  , step
  , syntaxToExpr
  , undefinedNames
  )
import Language.Name (Name)
import Language.Parse (formatParseError, parseAll, parseDefinition, parseEither, unsafeParse)
import Language.PrettyPrint (Rep(..), Doc, prettyPrint, selectRep, toggleRep)
import Language.Syntax (Definition, Syntax, defToDoc, defToSyntax)
import React.Basic (JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, mkReducer, useReducer)
import React.Basic.Hooks as Hooks

type State =
  { text :: String
  , expr :: Maybe Expr
  , history :: Array (Doc String)
  , defs :: Array Definition
  , env :: Environment Expr
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
            { expr: state.expr
            , onStep: dispatch <<< Reduce
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

initialDefs :: Array Definition
initialDefs =
  unsafeParse parseDefinition <$>
    [ "identity x = x"
    , "const x _ = x"
    , "fix f = (λx. f (x x)) (λy. f (y y))"
    , "true t _ = t"
    , "false _ f = f"
    , "and x y = x y false"
    , "or x y = x true y"
    , "foldr f z l = l f z"
    , "any = foldr or false"
    , "all = foldr and true"
    , "add m n s z = m s (n s z)"
    , "mul m n s z = m (n s) z"
    ]

initialEnv :: Environment Expr
initialEnv =
  Map.fromFoldable $ fromDef <$> initialDefs
 where
  fromDef def = Tuple def.name $ syntaxToExpr $ defToSyntax def

data Action
  = ShowHelp
  | DismissAlert
  | UpdateText String
  | ParseText
  | DeleteDef Name
  | AddDef Definition
  | SetExpr Syntax
  | Reduce Expr
  | Clear
  | ToggleSugar

initialState :: State
initialState =
  { text: ""
  , expr: Nothing
  , history: []
  , defs: initialDefs
  , env: initialEnv
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
  Reduce expr -> reduce expr s
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
deleteDef name s = s
  { defs = deleteByName name s.defs
  , env = env
  , alert = do
      guard $ Set.size names /= 0
      pure $ alert Warning $ formatUndefinedWarning name names
  }
 where
  env = Map.delete name s.env
  names = namesReferencing name env

deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = filter $ (_ /= name) <<< _.name

addDef :: Definition -> State -> State
addDef def s =
  if Set.isEmpty missing
    then s {text = "", defs = defs, env = env}
    else s {alert = pure $ alert Danger $ formatUndefinedError s.text missing}
 where
  defs = deleteByName def.name s.defs `snoc` def
  expr = syntaxToExpr (defToSyntax def)
  env = Map.insert def.name expr s.env
  missing = undefinedNames expr (Map.delete def.name s.env)

setExpr :: Syntax -> State -> State
setExpr syntax =
  _ {text = "", history = history, expr = expr}
 where
  history = pure $ prettyPrint syntax
  expr = pure $ syntaxToExpr syntax

reduce :: Expr -> State -> State
reduce expr s =
  case alpha <$> step s.env expr of
    Nothing -> s
    Just reduced -> s
      { history = prettyPrint (exprToSyntax reduced) `cons` s.history
      , expr = pure reduced
      }

clear :: State -> State
clear = _ {expr = Nothing, history = []}

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
  text = intercalate "\n" $ map (\def -> selectRep def rep) allDefs

alert :: Level -> String -> Tuple Level JSX
alert level message =
  Tuple level body
 where
  body = R.p
    { className: "preformatted"
    , children: [R.text message]
    }
