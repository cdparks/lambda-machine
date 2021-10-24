module Components.Copy
  ( new
  ) where

import Lambda.Prelude

import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useState)
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks as Hooks
import Effect.Aff (delay, Milliseconds(..))
import Effect.Copy as Copy

type Props =
  { text :: String
  }

data Copying
  = Ready
  | Copying
  | DidCopy
  | NoCopy

derive instance eqCopying :: Eq Copying

new :: Component Props
new = component "Copy" \{ text } -> Hooks.do
  copying /\ setCopying <- useState $ if Copy.hasClipboard then Ready else NoCopy
  _ <- useAff (Tuple text copying) do
    case copying of
      Ready -> pure unit
      NoCopy -> pure unit
      Copying -> do
        result <- Copy.copy text
        -- Artificial delay to avoid flash
        delay $ Milliseconds 300.0
        liftEffect $ setCopying $ const $ case result of
          Left _ -> NoCopy
          Right _ -> DidCopy
      DidCopy -> do
        delay $ Milliseconds 1000.0
        liftEffect $ setCopying $ const Ready

  let
    startCopying = setCopying $ const Copying
    copyButton = case copying of
      Ready -> R.button
        { className
        , style
        , "type": "submit"
        , onClick: handler_ startCopying
        , children: copyIcon "Copy to Clipboard"
        }
      Copying -> R.button
        { className
        , style
        , disabled: true
        , "type": "submit"
        , children: copyIcon "Copying…"
        }
      DidCopy -> R.button
        { className
        , style
        , "type": "submit"
        , onClick: handler_ startCopying
        , children: copyIcon "Copied!"
        }
      NoCopy -> R.button
        { className
        , style
        , disabled: true
        , "type": "submit"
        , children: copyIcon "Copy Manually"
        }

  pure $ R.form
    { onSubmit: handler preventDefault $ const $ pure unit
    , children:
      [ R.div
        { className: "input-group"
        , children:
          [ R.input
            { className: "form-control"
            , readOnly: true
            , autoComplete: "off"
            , autoCorrect: "off"
            , autoCapitalize: "off"
            , spellCheck: false
            , value: text
            }
          , R.div
            { className: "input-group-btn"
            , children: [copyButton]
            }
          ]
        }
      ]
    }
 where
  className = "btn btn-default"
  style = R.css { width: "160px" }
  copyIcon label =
    [ R.span
      { className: "glyphicon glyphicon-copy"
      , children: []
      }
    , R.text $ " " <> label
    ]
