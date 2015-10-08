module Component.App where

import Prelude

import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Array (snoc, filter)
import Data.List (toList)
import qualified Data.Map as Map

import Data.Syntax
import Data.Expr
import Data.Parse
import Text.Parsing.Parser (ParseError(..))

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as RD
import qualified React.DOM.Props as RP

import Component.Event

type AppState =
  { text    :: String
  , expr    :: Maybe Expr
  , history :: Array String
  , defs    :: Array Definition
  , env     :: Environment
  , error   :: Maybe String
  }

data AppAction
  = DoNothing
  | NewText String
  | ParseText
  | Reduce Expr
  | DismissAlert
  | Remove Name

type AppProps = Unit

initialDefs :: Array Definition
initialDefs =
  [ { name: "i"
    , syntax: unsafeParse parseSyntax "λx. x"
    }
  , { name: "k"
    , syntax: unsafeParse parseSyntax "λx. λy. x"
    }
  , { name: "fix"
    , syntax: unsafeParse parseSyntax "λf. (λx. f (x x)) (λy. f (y y))"
    }
  ]

initialEnv :: Environment
initialEnv = Map.fromList (toList (map fromDef initialDefs))
 where
  fromDef def = Tuple def.name (syntaxToExpr def.syntax)

appClass :: R.ReactClass AppProps
appClass = T.createClass (T.simpleSpec initialState update render)
 where
  initialState :: AppState
  initialState =
    { text: ""
    , expr: Nothing
    , history: []
    , defs: initialDefs
    , env: initialEnv
    , error: Nothing
    }

  update :: T.PerformAction _ AppState _ AppAction
  update props action = T.modifyState (step action)
   where
    step action =
      case action of
        DoNothing ->
          \s -> s
        NewText text ->
          \s -> s { text = text, error = Nothing }
        DismissAlert ->
          \s -> s { error = Nothing }
        Remove name ->
          \s -> s { defs = removeByName name s.defs, env = Map.delete name s.env }
        ParseText ->
          parse
        Reduce expr ->
          reduce expr

  parse :: AppState -> AppState
  parse s
    | s.text == "" = s
  parse s =
    case parseAll parseEither s.text of
      Left error   ->
        s { text = "", error = Just (formatParseError s.text error) }
      Right (Left def) ->
        addDef def s
      Right (Right syntax) ->
        addExpr syntax s

  removeByName :: Name -> Array Definition -> Array Definition
  removeByName name = filter (_.name >>> (/= name))

  addDef :: Definition -> AppState -> AppState
  addDef def s =
    s { text = "", defs = defs, env = env }
   where
    defs = removeByName def.name s.defs `snoc` def
    env = Map.insert def.name (syntaxToExpr def.syntax) s.env

  addExpr :: Syntax -> AppState -> AppState
  addExpr syntax s =
    s { text = "", history = history, expr = expr }
   where
    history = [prettyPrint syntax]
    expr = Just (syntaxToExpr syntax)

  reduce :: Expr -> AppState -> AppState
  reduce expr s =
    case step s.env expr of
      Nothing -> s
      Just expr' ->
        s { history = s.history `snoc` prettyPrint (exprToSyntax expr'), expr = Just expr' }

  render :: T.Render _ AppState _ AppAction
  render send state props _ =
    RD.div
      [ RP.className "container" ]
      [ RD.div
        [ RP.className "row" ]
        [ header]
      , RD.div
        [ RP.className "row" ]
        (alert send state.error)
      , RD.div
        [ RP.className "row" ]
        [ input send state.text ]
      , RD.div
        [ RP.className "row" ]
        (define send state.defs)
      , RD.div
        [ RP.className "row" ]
        (evaluate send state.history state.expr)
      ]

  header :: R.ReactElement
  header = RD.div
    [ RP.className "header" ]
    [ RD.h3
      [ RP.className "text-muted" ]
      [ RD.text "Lambda Machine" ]
    , RD.hr' []
    ]

  alert :: _ -> Maybe String -> Array R.ReactElement
  alert send Nothing = []
  alert send (Just error) =
    [ RD.div
      [ RP.className "col-sm-12" ]
      [ RD.pre
        [ RP.className "alert alert-danger" ]
        [ RD.span
          [ RP.className "glyphicon glyphicon-remove pull-right"
          , RP.onClick \_ -> send DismissAlert
          ]
          []
        , RD.text error
        ]
      ]
    ]

  input :: _ -> String -> R.ReactElement
  input send value = RD.div
    [ RP.className "col-sm-12" ]
    [ RD.div
      [ RP.className "input-group" ]
      [ RD.input
        [ RP.className "form-control monospace-font"
        , RP.placeholder "<definition> or <expression>"
        , RP.value value
        , RP.onKeyUp (handleKeyPress >>> send)
        , RP.onChange (handleChangeEvent >>> send)
        ]
        []
      , RD.span
        [ RP.className "input-group-btn" ]
        [ RD.button
          [ RP.className "btn btn-default"
          , RP.onClick \_ -> send ParseText
          ]
          [ RD.text "Parse" ]
        ]
      ]
    ]

  define :: _ -> Array Definition -> Array R.ReactElement
  define send definitions =
    [ RD.h3' [ RD.text "Definitions" ]
    , RD.div
      [ RP.className "monospace-font col-sm-12" ]
      (map (renderDef send) definitions)
    ]

  renderDef :: _ -> Definition -> R.ReactElement
  renderDef send def = RD.h4'
    [ RD.div
      [ RP.className "glyphicon glyphicon-remove"
      , RP.onClick \_ -> send (Remove def.name)
      ]
      []
    , RD.text (" " <> def.name <> " = " <> prettyPrint def.syntax)
    ]

  evaluate :: _ -> Array String -> Maybe Expr -> Array R.ReactElement
  evaluate send history Nothing = []
  evaluate send history (Just expr) =
    [ RD.h3' [ RD.text "Evaluation" ]
    , RD.div
      [ RP.className "col-sm-12" ]
      [ RD.div'
        [ RD.button
          [ RP.className "btn btn-default pull-right"
          , RP.onClick \_ -> send (Reduce expr)
          ]
          [ RD.text "Step" ]
        ]
      , RD.div
        [ RP.className "monospace-font" ]
        (map renderSyntax history)
      ]
    ]

  renderSyntax :: String -> R.ReactElement
  renderSyntax syntax = RD.h4' [ RD.text syntax]

handleKeyPress :: forall event. event -> AppAction
handleKeyPress e =
  case getKeyCode e of
    13 ->
      ParseText
    _  ->
      DoNothing

handleChangeEvent :: forall event. event -> AppAction
handleChangeEvent = getValue >>> NewText

