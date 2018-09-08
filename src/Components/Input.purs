module Components.Input
  ( component
  ) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))

import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue, key)
import React.Basic.Events as Events
import React.Basic.Events (merge)

type Props =
  { text :: String
  , onChange :: String -> Effect Unit
  , onSubmit :: Effect Unit
  }

component :: React.Component Props
component =
  React.stateless {displayName: "Input", render}
 where
  render {text, onChange, onSubmit} =
    R.div
      { className: "input-group"
      , children:
        [ R.input
          { className: "form-control monospace-font"
          , placeholder: "<definition> or <expression>"
          , onChange: Events.handler events $ dispatch onChange onSubmit
          , value: text
          }
        , R.span
          { className: "input-group-btn"
          , children:
            [ R.button
              { className: "btn btn-default"
              , onClick: Events.handler_ onSubmit
              , children: [R.text "Parse"]
              }
            ]
          }
        ]
      }

events :: Events.EventFn Events.SyntheticEvent {key :: Maybe String, targetValue :: Maybe String}
events = merge {key, targetValue}

dispatch
  :: (String -> Effect Unit)
  -> Effect Unit
  -> {key :: Maybe String, targetValue :: Maybe String}
  -> Effect Unit
dispatch onChange onSubmit {key, targetValue} =
  case key, targetValue of
    Just "Enter", _ ->
      onSubmit
    _, Just value ->
      onChange value
    _, _ ->
      pure unit
