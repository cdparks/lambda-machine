module Components.Controls
  ( component
  ) where

import Lambda.Prelude

import Lambda.Language.PrettyPrint (Rep, ifSugar)
import Lambda.Machine (Machine)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler, handler_)

type Props =
  { machine :: Maybe Machine
  , onStep :: Machine -> Effect Unit
  , onClear :: Effect Unit
  , onSave :: Effect Unit
  , onSugar :: Effect Unit
  , rep :: Rep
  }

component :: Props -> JSX
component {machine, onStep, onClear, onSave, onSugar, rep} =
  R.div
    { className: "add-margin-medium btn-group pull-right"
    , children:
      [ button machine
        { className: "btn btn-default"
        , onClick: onStep
        , label: "Step"
        }
      , button machine
        { className: "btn btn-default"
        , onClick: const onClear
        , label: "Clear"
        }
      , button machine
        { className: "btn btn-default"
        , onClick: const onSave
        , label: "Save"
        }
      , button (pure unit)
        { className: "btn " <> ifSugar "btn-danger" "btn-success" rep
        , onClick: const onSugar
        , label: ifSugar "Raw" "Sugar" rep
        }
      ]
    }

-- | Create a button element enabled based on the presence of the first
-- | argument.
button
  :: forall a
   . Maybe a
  ->
    { className :: String
    , onClick :: a -> Effect Unit
    , label :: String
    }
  -> JSX
button m {className, onClick, label} =
  R.button
    { className: maybeEnable className m
    , onClick: maybeHandle onClick m
    , children: [R.text label]
    }

-- | Build handler if second argument is present.
maybeHandle :: forall a. (a -> Effect Unit) -> Maybe a -> EventHandler
maybeHandle handle =
  handler_ <<< maybe (pure unit) handle

-- | Enable element if second argument is present.
maybeEnable :: forall a. String -> Maybe a -> String
maybeEnable className =
  maybe
    (className <> " disabled")
    (const className)
