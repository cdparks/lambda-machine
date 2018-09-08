module Components.App
  ( component
  ) where

import Prelude (discard, map, pure, unit, ($), (/=), (<$>), (<<<), (==))
import Control.MonadZero (guard)
import Data.Array (cons, snoc, filter)
import Data.Either (Either(..))
-- import Data.Foldable (intercalate)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import React.Basic as React
import React.Basic.DOM as R

import Components.Alert as Alert
import Components.Input as Input
import Components.Definitions as Definitions
import Components.Controls as Controls
import Components.Expressions as Expressions
import Components.Footer as Footer
import Data.Expr
  ( Environment
  , Expr
  , alpha
  , exprToSyntax
  , formatUndefinedError
  , formatUndefinedWarning
  , namesReferencing
  , step
  , syntaxToExpr
  , undefinedNames
  )
import Data.Level (Level, warn, danger)
import Data.Name (Name)
import Data.Parse (formatParseError, parseAll, parseDefinition, parseEither, unsafeParse)
import Data.PrettyPrint (Rep(..), Doc, prettyPrint, toggleRep)
import Data.Syntax (Definition, Syntax, defToSyntax)
-- import Data.Syntax (Definition, Syntax, defToDoc, defToSyntax)

-- import Control.Monad.Eff.Save (SAVE, saveTextAs)
-- import Control.Monad.Trans.Class (lift)
-- import Control.Monad.Eff.Class (liftEff)
-- import Control.Monad.Aff (Aff, Canceler, forkAff)

type State =
  { text :: String
  , expr :: Maybe Expr
  , history :: Array (Doc String)
  , defs :: Array Definition
  , env :: Environment Expr
  , rep :: Rep
  , error :: Maybe (Tuple Level String)
  }

component :: React.Component {}
component =
  React.component {displayName: "App", initialState, receiveProps, render}
 where
  receiveProps _ =
    pure unit

  render {state, setState} =
    R.div
      { className: "container"
      , children:
        [ row $ R.h2
          { className: "page-header"
          , children: [R.text "Lambda Machine"]
          }
        , row $ React.element Alert.component
          { error: state.error
          , dismiss: setState deleteError
          }
        , row $ React.element Input.component
          { text: state.text
          , onChange: setState <<< updateText
          , onSubmit: setState parseText
          }
        , row $ R.h3_ [R.text "Definitions"]
        , row $ React.element Definitions.component
          { defs: state.defs
          , rep: state.rep
          , onDelete: setState <<< deleteDef
          }
        , split
          (R.h3_ [R.text "Evaluation"])
          (React.element Controls.component
            { expr: state.expr
            , onStep: setState <<< reduce
            , onClear: setState clear
            , onSave: setState \s -> s
            , onSugar: setState toggleSugar
            , rep: state.rep
            }
          )
        , row $ React.element Expressions.component
          { history: state.history
          , rep: state.rep
          }
        , row $ React.element Footer.component {}
        ]
      }

row :: React.JSX -> React.JSX
row child =
  R.div
    { className: "row"
    , children: [R.div {className: "col-sm-12", children: [child]}]
    }

split :: React.JSX -> React.JSX -> React.JSX
split lhs rhs =
  R.div
    { className: "row"
    , children:
      [ R.div {className: "col-sm-6", children: [lhs]}
      , R.div {className: "col-sm-6", children: [rhs]}
      ]
    }

initialState :: State
initialState =
  { text: ""
  , expr: Nothing
  , history: []
  , defs: initialDefs
  , env: initialEnv
  , rep: Raw
  , error: Nothing
  }

initialDefs :: Array Definition
initialDefs =
  map (unsafeParse parseDefinition)
    [ "fix f = (λx. f (x x)) (λy. f (y y))"
    , "true t _ = t"
    , "false _ f = f"
    , "and x y = x y false"
    , "or x y = x true y"
    , "foldr f z l = l f z"
    , "any = foldr or false"
    , "all = foldr and true"
    , "add m n s z = m s (n s z)"
    , "mul m n s z = m (n s) z"
    ]

initialEnv :: Environment Expr
initialEnv =
  Map.fromFoldable (map fromDef initialDefs)
 where
  fromDef def = Tuple def.name $ syntaxToExpr $ defToSyntax def

{-
update :: T.PerformAction _ State _ AppAction
update action _ _ =
  case action of
    DoNothing ->
      pure unit
    NewText text ->
      void (T.modifyState (\s -> s { text = text, error = Nothing }))
    DismissAlert ->
      void (T.modifyState (\s -> s { error = Nothing }))
    Remove name ->
      void (T.modifyState (remove name))
    ParseText ->
      void (T.modifyState parse)
    Reduce expr ->
      void (T.modifyState (reduce expr))
    Clear ->
      void (T.modifyState clear)
    Save -> void do
      mState <- T.modifyState id
      for mState $ \state ->
        state <$ lift (save state)
    ToggleSugar ->
      void (T.modifyState toggleSugar)


save :: forall eff. State -> Aff (save :: SAVE | eff) (Canceler (save :: SAVE | eff))
save s = do
  let
    allDefs = map defToDoc s.defs <> reverse s.history
    text = intercalate "\n" (map s.rep allDefs)
  forkAff (liftEff (saveTextAs text "evaluation.txt"))
-}

setError :: String -> State -> State
setError message = _ {error = pure $ danger message}

deleteError :: State -> State
deleteError = _ {error = Nothing}

updateText :: String -> State -> State
updateText text = _ { error = Nothing, text = text }

parseText :: State -> State
parseText s
  | s.text == "" = s
parseText s =
  case parseAll parseEither s.text of
    Left error ->
      setError (formatParseError s.text error) s
    Right (Left def) ->
      addDef def s
    Right (Right syntax) ->
      setExpr syntax s

deleteDef :: Name -> State -> State
deleteDef name s = s
  { defs = deleteByName name s.defs
  , env = env
  , error = error
  }
 where
  env = Map.delete name s.env
  names = namesReferencing name env
  error = do
    guard $ Set.size names /= 0
    pure $ warn $ formatUndefinedWarning name names

deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = filter $ (_ /= name) <<< _.name

addDef :: Definition -> State -> State
addDef def s =
  if Set.isEmpty missing
    then s {text = "", defs = defs, env = env}
    else setError (formatUndefinedError s.text missing) s
 where
  defs = deleteByName def.name s.defs `snoc` def
  expr = syntaxToExpr (defToSyntax def)
  env = Map.insert def.name expr s.env
  missing = undefinedNames expr (Map.delete def.name s.env)

setExpr :: Syntax -> State -> State
setExpr syntax =
  _ {text = "", history = history, expr = expr}
 where
  history = pure $ prettyPrint syntax
  expr = pure $ syntaxToExpr syntax

reduce :: Expr -> State -> State
reduce expr s =
  case alpha <$> step s.env expr of
    Nothing -> s
    Just reduced -> s
      { history = prettyPrint (exprToSyntax reduced) `cons` s.history
      , expr = pure reduced
      }

clear :: State -> State
clear s =
  s {history = maybe [] toHistory s.expr}
 where
  toHistory = pure <<< prettyPrint <<< exprToSyntax

toggleSugar :: State -> State
toggleSugar s = s {rep = toggleRep s.rep}
