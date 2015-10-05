module Component.App where

import Prelude

import Data.Maybe

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as RD
import qualified React.DOM.Props as RP

import Component.Event

type AppState =
  { text         :: String
  , errorMessage :: Maybe String
  , scrutinee    :: Maybe String
  , expressions  :: Array String
  , definitions  :: Array String
  }

data AppAction
  = DoNothing
  | NewText String
  | CompleteText
  | Evaluate
  | Step

type AppProps = Unit

appClass :: R.ReactClass AppProps
appClass = T.createClass (T.simpleSpec initialState update render)
 where
  initialState :: AppState
  initialState =
    { text: ""
    , errorMessage: Nothing
    , scrutinee: Nothing
    , expressions: []
    , definitions: []
    }

  update :: T.PerformAction _ AppState _ AppAction
  update props action = T.modifyState (step action)
   where
    step DoNothing      = \s -> s
    step (NewText text) = \s -> s { text = text }
    step CompleteText   = \s -> s { text = "", expressions = [], scrutinee = Just s.text }

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
        [ RP.className "row add-margin-large" ]
        [ evaluator send state.expressions state.scrutinee ]
      ]

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
        [ RP.className "form-control"
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
          , RP.onClick \_ -> send CompleteText
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
      CompleteText
    _  ->
      DoNothing

handleChangeEvent :: forall event. event -> AppAction
handleChangeEvent = getValue >>> NewText

