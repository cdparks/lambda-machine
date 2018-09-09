module Components.Input
  ( component
  ) where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue, preventDefault)
import React.Basic.Events as Events

type Props =
  { text :: String
  , onChange :: String -> Effect Unit
  , onSubmit :: Effect Unit
  , onHelp :: Effect Unit
  }

component :: React.Component Props
component =
  React.stateless {displayName: "Input", render}
 where
  render {text, onChange, onSubmit, onHelp} =
    R.form
      { onSubmit: Events.handler preventDefault $ const onSubmit
      , children:
        [ inputGroup
          [ inputGroupBtn $ R.button
            { className: "btn btn-info"
            , "type": "button"
            , onClick: Events.handler_ onHelp
            , children: [R.text "Help"]
            }
          , R.input
            { className: "form-control monospace-font"
            , placeholder: "expression or definition"
            , onChange: Events.handler targetValue $ traverse_ onChange
            , value: text
            }
          , inputGroupBtn $ R.button
            { className: "btn btn-default"
            , "type": "submit"
            , onClick: Events.handler_ onSubmit
            , children: [R.text "Parse"]
            }
          ]
        ]
      }


inputGroup :: Array React.JSX -> React.JSX
inputGroup children =
  R.div
    { className: "input-group"
    , children
    }

inputGroupBtn :: React.JSX -> React.JSX
inputGroupBtn child =
  R.div
    { className: "input-group-btn"
    , children: [child]
    }
