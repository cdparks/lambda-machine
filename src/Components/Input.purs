module Components.Input
  ( component
  ) where

import Lambda.Prelude

import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue, preventDefault)
import React.Basic.Events (handler, handler_)

type Props =
  { text :: String
  , onChange :: String -> Effect Unit
  , onSubmit :: Effect Unit
  , onHelp :: Effect Unit
  }

component :: Props -> JSX
component {text, onChange, onSubmit, onHelp} =
  R.form
    { onSubmit: handler preventDefault $ const onSubmit
    , children:
      [ inputGroup
        [ inputGroupBtn $ R.button
          { className: "btn btn-info"
          , "type": "button"
          , onClick: handler_ onHelp
          , children: [R.text "Help"]
          }
        , R.input
          { className: "form-control monospace-font"
          , placeholder: "expression or definition"
          , onChange: handler targetValue $ traverse_ onChange
          , value: text
          }
        , inputGroupBtn $ R.button
          { className: "btn btn-default"
          , "type": "submit"
          , onClick: handler_ onSubmit
          , children: [R.text "Parse"]
          }
        ]
      ]
    }


inputGroup :: Array JSX -> JSX
inputGroup children =
  R.div
    { className: "input-group"
    , children
    }

inputGroupBtn :: JSX -> JSX
inputGroupBtn child =
  R.div
    { className: "input-group-btn"
    , children: [child]
    }
