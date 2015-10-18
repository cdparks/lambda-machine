module Component.App
  ( appClass
  , AppProps(..)
  ) where

import Prelude

import Data.Maybe
import Data.Either
import Data.Tuple
import Data.Array (uncons, cons, snoc, filter, singleton, reverse, null)
import Data.List (toList)
import Data.Foldable (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Syntax
import Data.Expr
import Data.Parse
import Data.PrettyPrint

import Text.Parsing.Parser (ParseError(..))

import Control.Monad.Eff
import Control.Monad.Eff.Save

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as RD
import qualified React.DOM.Props as RP

import Component.Event

data Level = Warning | Danger

instance showLevel :: Show Level where
  show Warning = "warning"
  show Danger  = "danger"

type AppState =
  { text    :: String
  , expr    :: Maybe Expr
  , history :: Array String
  , defs    :: Array Definition
  , env     :: Environment
  , error   :: Maybe (Tuple Level String)
  }

data AppAction
  = DoNothing
  | NewText String
  | ParseText
  | Reduce Expr
  | DismissAlert
  | Remove Name
  | Clear
  | Save

type AppProps = Unit

appClass :: R.ReactClass AppProps
appClass = T.createClass (T.simpleSpec initialState update render)

initialDefs :: Array Definition
initialDefs =
  [ { name: "i"
    , syntax: unsafeParse parseSyntax "λx. x"
    }
  , { name: "k"
    , syntax: unsafeParse parseSyntax "λx. λy. x"
    }
  , { name: "fix"
    , syntax: unsafeParse parseSyntax "λf. (λx. f (x x)) (λy. f (y y))"
    }
  ]

initialEnv :: Environment
initialEnv = Map.fromList (toList (map fromDef initialDefs))
 where
  fromDef def = Tuple def.name (syntaxToExpr def.syntax)

initialState :: AppState
initialState =
  { text: ""
  , expr: Nothing
  , history: []
  , defs: initialDefs
  , env: initialEnv
  , error: Nothing
  }

update :: T.PerformAction _ AppState _ AppAction
update props action =
  case action of
    DoNothing ->
      return unit
    NewText text ->
      T.modifyState (\s -> s { text = text, error = Nothing })
    DismissAlert ->
      T.modifyState (\s -> s { error = Nothing })
    Remove name ->
      T.modifyState (remove name)
    ParseText ->
      T.modifyState parse
    Reduce expr ->
      T.modifyState (reduce expr)
    Clear ->
      T.modifyState clear
    Save -> do
      s <- T.getState
      T.async \k -> do
        let text = intercalate "\n" (map defToString s.defs <> reverse s.history)
        saveTextAs text "evaluation.txt"
        k unit
     where
      defToString def = def.name <> " = " <> prettyPrint def.syntax

fail :: String -> AppState -> AppState
fail message s =
  s { text = "", error = Just (Tuple Danger message) }

parse :: AppState -> AppState
parse s
  | s.text == "" = s
parse s =
  case parseAll parseEither s.text of
    Left error   ->
      fail (formatParseError s.text error) s
    Right (Left def) ->
      addDef def s
    Right (Right syntax) ->
      addExpr syntax s

remove :: String -> AppState -> AppState
remove name s =
  s { defs = deleteByName name s.defs, env = env', error = error }
 where
  env' = Map.delete name s.env
  names = namesReferencing name env'
  error = if Set.size names == 0
             then Nothing
             else Just (Tuple Warning (formatUndefinedWarning name names))

deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = filter (_.name >>> (/= name))

addDef :: Definition -> AppState -> AppState
addDef def s =
  if Set.size missing == 0
    then
      s { text = "", defs = defs, env = env }
    else
      fail (formatUndefinedError s.text missing) s
 where
  defs = deleteByName def.name s.defs `snoc` def
  expr = syntaxToExpr def.syntax
  env = Map.insert def.name expr s.env
  missing = undefinedNames expr (Map.delete def.name s.env)

addExpr :: Syntax -> AppState -> AppState
addExpr syntax s =
  s { text = "", history = history, expr = expr }
 where
  history = [prettyPrint syntax]
  expr = Just (syntaxToExpr syntax)

reduce :: Expr -> AppState -> AppState
reduce expr s =
  case step s.env expr of
    Nothing -> s
    Just expr' ->
      s { history = prettyPrint (exprToSyntax expr') `cons` s.history, expr = Just expr' }

clear :: AppState -> AppState
clear s = s { history = maybe [] (exprToSyntax >>> prettyPrint >>> singleton) s.expr }

render :: T.Render _ AppState _ AppAction
render send state props _ =
  RD.div
    [ RP.className "container" ]
    [ RD.div
      [ RP.className "row" ]
      [ header ]
    , RD.div
      [ RP.className "row" ]
      (alert send state.error)
    , RD.div
      [ RP.className "row" ]
      [ input send state.text ]
    , RD.div
      [ RP.className "row" ]
      (renderDefs send state.defs)
    , RD.div
      [ RP.className "row" ]
      (renderExprs send state.history state.expr)
    , RD.div
      [ RP.className "row" ]
      [ footer ]
    ]

header :: R.ReactElement
header = RD.div
  [ RP.className "page-header" ]
  [ RD.h2'
    [ RD.text "Lambda Machine" ]
  ]

footer :: R.ReactElement
footer = RD.div'
  [ RD.hr' []
  , RD.a
    [ RP.href "https://github.com/cdparks/lambda-machine"
    , RP.className "pull-right"
    ]
    [ RD.text "Source on GitHub" ]
  ]

alert :: _ -> Maybe (Tuple Level String) -> Array R.ReactElement
alert send Nothing = []
alert send (Just (Tuple level error)) =
  [ RD.div
    [ RP.className "col-sm-12" ]
    [ RD.pre
      [ RP.className ("alert alert-" <> show level) ]
      [ RD.span
        [ RP.className "glyphicon glyphicon-remove pull-right"
        , RP.onClick \_ -> send DismissAlert
        ]
        []
      , RD.text error
      ]
    ]
  ]

input :: _ -> String -> R.ReactElement
input send value = RD.div
  [ RP.className "col-sm-12" ]
  [ RD.div
    [ RP.className "input-group" ]
    [ RD.input
      [ RP.className "form-control monospace-font"
      , RP.placeholder "<definition> or <expression>"
      , RP.value value
      , RP.onKeyUp (handleKeyPress >>> send)
      , RP.onChange (handleChangeEvent >>> send)
      ]
      []
    , RD.span
      [ RP.className "input-group-btn" ]
      [ RD.button
        [ RP.className "btn btn-default"
        , RP.onClick \_ -> send ParseText
        ]
        [ RD.text "Parse" ]
      ]
    ]
  ]

renderDefs :: _ -> Array Definition -> Array R.ReactElement
renderDefs send definitions =
  [ RD.h3' [ RD.text "Definitions" ]
  , RD.div
    [ RP.className "monospace-font col-sm-12" ]
    (map (renderDef send) definitions)
  ]

renderDef :: _ -> Definition -> R.ReactElement
renderDef send def = RD.h4'
  [ RD.div
    [ RP.className "glyphicon glyphicon-remove"
    , RP.onClick \_ -> send (Remove def.name)
    ]
    []
  , RD.text (" " <> def.name <> " = " <> prettyPrint def.syntax)
  ]

renderExprs :: _ -> Array String -> Maybe Expr -> Array R.ReactElement
renderExprs send history Nothing = []
renderExprs send history (Just expr) =
  [ RD.h3' [ RD.text "Evaluation" ]
  , RD.div
    [ RP.className "col-sm-12" ]
    [ RD.div
      [ RP.className "btn-group pull-right" ]
      [ RD.button
        [ RP.className "btn btn-default"
        , RP.onClick \_ -> send (Reduce expr)
        ]
        [ RD.text "Step" ]
      , RD.button
        [ RP.className "btn btn-default"
        , RP.onClick \_ -> send Clear
        ]
        [ RD.text "Clear" ]
      , RD.button
        [ RP.className "btn btn-default"
        , RP.onClick \_ -> send Save
        ]
        [ RD.text "Save" ]
      ]
    , RD.div
      [ RP.className "hide-overflow" ]
      [ RD.div
        [ RP.className "scroll-overflow monospace-font" ]
        (renderHistory history)
      ]
    ]
  ]

renderHistory :: Array String -> Array R.ReactElement
renderHistory hs =
  case uncons hs of
    Nothing -> []
    Just { head = head, tail = tail } ->
      RD.h4' [ RD.text head ] `cons` map renderSyntax tail

renderSyntax :: String -> R.ReactElement
renderSyntax syntax = RD.h4
  [ RP.className "text-muted" ]
  [ RD.text syntax]

handleKeyPress :: forall event. event -> AppAction
handleKeyPress e =
  case getKeyCode e of
    13 ->
      ParseText
    _  ->
      DoNothing

handleChangeEvent :: forall event. event -> AppAction
handleChangeEvent = getValue >>> NewText

