module Components.App
  ( new
  ) where

import Lambda.Prelude hiding (State)

import Components.Alert (component) as Alert
import Components.App.Action as Action
import Components.App.Alert (Alert(..), Error(..)) as Alert
import Components.App.Request as Request
import Components.App.State as State
import Components.ConsistencyError as ConsistencyError
import Components.Controls as Controls
import Components.Copy as Copy
import Components.Definitions as Definitions
import Components.Expressions as Expressions
import Components.ApiError as ApiError
import Components.Footer as Footer
import Components.Help as Help
import Components.Input as Input
import Components.Level as Level
import Components.Modal as Modal
import Components.ParseError as ParseError
import Components.Spinner as Spinner
import Data.Grammar (pluralizeWith)
import Lambda.Env as Env
import Lambda.Language.Snapshot.Code (Code)
import React.Basic (fragment, JSX)
import React.Basic as React
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component, mkReducer, useReducer)
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Aff (useAff)

type Props =
  { code :: Maybe Code
  }

new :: Component Props
new = do
  reducer <- mkReducer State.reduce
  spinner <- Spinner.new
  modal <- linkModal
  component "App" \{ code: mCode }  -> Hooks.do
    state /\ dispatch <- useReducer (State.new mCode) reducer
    _ <- useAff state.request $ traverse_ (State.handle dispatch state) state.request

    let
      dismiss = dispatch Action.Dismiss
      alert = case state.alert of
        Just Alert.Help ->
          row $ helpAlert dismiss
        Just (Alert.Error error) ->
          row $ errorAlert dismiss error
        Just (Alert.Link code) ->
          row $ modal { dismiss, code }
        Nothing ->
          React.empty

      { isHalted, hasProgram, hasMachine } = State.status state
      dispatchIf cond = (_ <$ guard cond) <<< dispatch

    pure $ fragment
      [ R.div
        { className: "container"
        , children:
          [ row $ R.h2
            { className: "page-header"
            , children: [R.text "Lambda Machine"]
            }
          , alert
          , row $ Input.component
            { text: state.text
            , onChange: dispatch <<< Action.Update
            , onSubmit: dispatch Action.Parse
            , onHelp: dispatch Action.Help
            }
          , row $ R.h3_ [R.text "Definitions"]
          , row $ Definitions.component
            { defs: state.defs
            , rep: state.rep
            , onDelete: dispatch <<< Action.Delete
            }
          , split
            (stepsHeader state.steps)
            (Controls.component
              { onStep: dispatchIf (not isHalted) Action.Step
              , onClear: dispatchIf hasMachine Action.Clear
              , onShare: dispatchIf hasProgram $ Action.Enqueue Request.Store
              , onSave: dispatchIf hasProgram $ Action.Enqueue Request.Save
              , onSugar: pure $ dispatch $ Action.Toggle
              , rep: state.rep
              }
            )
          , row $ Expressions.component
            { history: state.history
            , rep: state.rep
            }
          , row $ Footer.component {}
          ]
        }
        , maybe React.empty (\_ -> spinner {}) state.request
      ]

errorAlert :: Effect Unit -> Alert.Error -> JSX
errorAlert dismiss = case _ of
  Alert.ApiError error -> Alert.component
    { dismiss
    , level: Level.Danger
    , child: ApiError.component { error }
    }
  Alert.SaveError error -> Alert.component
    { dismiss
    , level: Level.Danger
    , child: R.text error
    }
  Alert.ParseError input error -> Alert.component
    { dismiss
    , level: Level.Danger
    , child: ParseError.component
      { input
      , error
      }
    }
  Alert.Inconsistent error -> Alert.component
    { dismiss
    , level: Level.Danger
    , child: ConsistencyError.component { error }
    }

helpAlert :: Effect Unit -> JSX
helpAlert dismiss = Alert.component
  { dismiss
  , level: Level.Info
  , child: Help.component {}
  }

linkModal :: Component { dismiss :: Effect Unit, code :: Code }
linkModal = do
  copy <- Copy.new
  modal <- Modal.new
  pure \{ dismiss, code } -> modal
    { dismiss
    , level: Level.Info
    , title: "Copy Link To This Machine"
    , children:
      [ copy
        { text: Env.host <> "/?code=" <> unwrap code
        }
      ]
    }

-- | Row containing a single full-width element
row :: JSX -> JSX
row child =
  R.div
    { className: "row"
    , children: [R.div {className: "col-sm-12", children: [child]}]
    }

-- | Row containing two equal-width elements
split :: JSX -> JSX -> JSX
split lhs rhs =
  R.div
    { className: "row"
    , children:
      [ R.div {className: "col-sm-6", children: [lhs]}
      , R.div {className: "col-sm-6", children: [rhs]}
      ]
    }

stepsHeader :: Maybe Int -> JSX
stepsHeader = case _ of
  Nothing -> R.h3
    { className: "text-muted"
    , children: [R.text "Steps"]
    }
  Just n -> R.h3_
    [ R.text $ fold
      [ show n
      , " "
      , pluralizeWith "s" n "Step"
      ]
    ]
