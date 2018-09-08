module Components.Input
  ( component
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe)
import Effect (Effect)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue, key)
import React.Basic.Events as Events
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

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
        [ R.input $ hideOnKeyUp
          { className: "form-control monospace-font"
          , placeholder: "<definition> or <expression>"
          , onChange: Events.handler targetValue $ traverse_ onChange
          , onKeyUp: Events.handler key $ submitOnEnter onSubmit
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

submitOnEnter :: Effect Unit -> Maybe String -> Effect Unit
submitOnEnter onSubmit = traverse_ \key ->
  when (key == "Enter") onSubmit

-- input props don't allow an onKeyUp handler
hideOnKeyUp :: forall props. { onKeyUp :: EventHandler | props } -> { | props }
hideOnKeyUp = unsafeCoerce
