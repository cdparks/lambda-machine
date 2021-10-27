module Components.Controls
  ( component
  ) where

import Lambda.Prelude

import Lambda.Flags (Flags)
import Lambda.Language.Pretty (Rep, select)
import React.Basic (JSX, empty)
import React.Basic.DOM as R
import React.Basic.Events (handler_)

type Props =
  { flags :: Flags
  , onStep :: Maybe (Effect Unit)
  , onClear :: Maybe (Effect Unit)
  , onShare :: Maybe (Effect Unit)
  , onSave :: Maybe (Effect Unit)
  , onSugar :: Maybe (Effect Unit)
  , rep :: Rep
  }

component :: Props -> JSX
component { flags, onStep, onClear, onShare, onSave, onSugar, rep } =
  R.div
    { className: "add-margin-medium btn-group pull-right"
    , children:
      [ button
        { label: "Step"
        , className: "btn btn-default"
        , onClick: onStep
        }
      , button
        { label: "Clear"
        , className: "btn btn-default"
        , onClick: onClear
        }
      , if flags.sharing
          then button
            { label: "Share"
            , className: "btn btn-default"
            , onClick: onShare
            }
          else empty
      , button
        { label: "Save"
        , className: "btn btn-default"
        , onClick: onSave
        }
      , button
        { label: select rep
          { sugar: "Raw"
          , raw: "Sugar"
          }
        , className: select rep
          { sugar: "btn btn-danger"
          , raw: "btn btn-success"
          }
        , onClick: onSugar
        }
      ]
    }

-- | Disable button if `onClick` is `Nothing`
button
  ::
    { label :: String
    , className :: String
    , onClick :: Maybe (Effect Unit)
    }
  -> JSX
button { label, className, onClick } =
  R.button
    { className
    , onClick: handler_ $ fromMaybe (pure unit) $ onClick
    , children: [R.text label]
    , disabled: isNothing onClick
    }
