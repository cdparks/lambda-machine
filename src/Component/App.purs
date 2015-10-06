module Component.App where

import Prelude

import Data.Maybe
import Data.Either
import Data.Array (snoc)

import Data.Syntax
import Data.Parse
import Data.Expr

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
  , error   :: Maybe String
  }

data AppAction
  = DoNothing
  | NewText String
  | ParseText
  | Reduce Expr
  | DismissAlert

type AppProps = Unit

appClass :: R.ReactClass AppProps
appClass = T.createClass (T.simpleSpec initialState update render)
 where
  initialState :: AppState
  initialState =
    { text: ""
    , expr: Nothing
    , history: []
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
        ParseText ->
          parse
        Reduce expr ->
          reduce expr

  parse :: AppState -> AppState
  parse s
    | s.text == "" = s
  parse s =
    case parseAll parseSyntax s.text of
      Left error   ->
        s { text = "", error = Just (formatParseError error) }
      Right syntax ->
        s { text = "", history = [prettyPrint syntax], expr = Just (syntaxToExpr syntax) }

  reduce :: Expr -> AppState -> AppState
  reduce expr =
    case step expr of
      Nothing ->
        \s -> s
      Just expr' ->
        \s -> s { history = s.history `snoc` prettyPrint (exprToSyntax expr'), expr = Just expr' }

  render :: T.Render _ AppState _ AppAction
  render send state props _ =
    RD.div
      [ RP.className "container" ]
      [ RD.div
        [ RP.className "row" ]
        [ header]
      , RD.div
        [ RP.className "row" ]
        [ input send state.text ]
      , RD.div
        [ RP.className "row" ]
        (alert send state.error)
      , RD.div
        [ RP.className "row add-margin-large" ]
        (evaluate send state.history state.expr)
      ]

  alert :: _ -> Maybe String -> Array R.ReactElement
  alert send Nothing = []
  alert send (Just error) =
    [ RD.div
      [ RP.className "alert alert-danger"
      , RP.onClick \_ -> send DismissAlert
      ]
      [ RD.text error ]
    ]

  evaluate :: _ -> Array String -> Maybe Expr -> Array R.ReactElement
  evaluate send history Nothing = []
  evaluate send history (Just expr) =
    [ RD.div
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

  header :: R.ReactElement
  header = RD.div
    [ RP.className "header" ]
    [ RD.h3
      [ RP.className "text-muted" ]
      [ RD.text "LambdaMachine" ]
    , RD.hr' []
    ]

  input :: _ -> String -> R.ReactElement
  input send value = RD.div
    [ RP.className "col-sm-12" ]
    [ RD.div
      [ RP.className "input-group" ]
      [ RD.input
        [ RP.className "form-control monospace-font"
        , RP.placeholder "<expression>"
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

  evaluator :: _ -> Array String -> Maybe String -> R.ReactElement
  evaluator send exprs scrutinee = RD.div
    [ RP.className "col-sm-12" ]
    [ RD.h3
      [ RP.className "panel-title" ]
      [ RD.text "Nothing to see here yet..." ]
    , RD.div'
      [ RD.ul' (map expression exprs) ]
    ]

  expression :: String -> R.ReactElement
  expression expr = RD.li' [ RD.text expr ]

handleKeyPress :: forall event. event -> AppAction
handleKeyPress e =
  case getKeyCode e of
    13 ->
      ParseText
    _  ->
      DoNothing

handleChangeEvent :: forall event. event -> AppAction
handleChangeEvent = getValue >>> NewText

