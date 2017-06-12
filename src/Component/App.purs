module Component.App
  ( spec
  , initialState
  , Level
  , AppAction
  ) where

import Prelude
  ( class Show
  , Unit
  , bind
  , id
  , map
  , pure
  , show
  , unit
  , void
  , ($)
  , (/=)
  , (<$)
  , (<>)
  , (==)
  , (>>>)
  )

import Data.Maybe (Maybe(..), maybe)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array (uncons, cons, snoc, filter, singleton, reverse)
import Data.Foldable (intercalate)
import Data.Map as Map
import Data.Set as Set

import Data.Name (Name)
import Data.Syntax (Definition, Syntax, defToDoc, defToSyntax)
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
import Data.Parse (formatParseError, parseAll, parseDefinition, parseEither, unsafeParse)
import Data.PrettyPrint (Doc, doc, prettyPrint, raw, sugar)
import Data.Traversable (for)

import Control.Monad.Eff.Save (SAVE, saveTextAs)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (Aff, Canceler, forkAff)

import Thermite as T

import React as R
import React.DOM as RD
import React.DOM.Props as RP

import Component.Event (getKeyCode, getValue)

data Level = Warning | Danger

instance showLevel :: Show Level where
  show Warning = "warning"
  show Danger  = "danger"

type AppState =
  { text     :: String
  , expr     :: Maybe Expr
  , history  :: Array (Doc String)
  , defs     :: Array Definition
  , env      :: Environment Expr
  , rep      :: forall a. Doc a -> a
  , error    :: Maybe (Tuple Level String)
  }

ifSugar :: forall a. (Doc a -> a) -> a -> a -> a
ifSugar rep s r = rep (doc {sugar: s, raw: r})

data AppAction
  = DoNothing
  | NewText String
  | ParseText
  | Reduce Expr
  | DismissAlert
  | Remove Name
  | Clear
  | Save
  | ToggleSugar

spec :: T.Spec _ AppState _ AppAction
spec = T.simpleSpec update render

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
initialEnv = Map.fromFoldable (map fromDef initialDefs)
 where
  fromDef def = Tuple def.name (syntaxToExpr (defToSyntax def))

initialState :: AppState
initialState =
  { text: ""
  , expr: Nothing
  , history: []
  , defs: initialDefs
  , env: initialEnv
  , rep: raw
  , error: Nothing
  }

update :: T.PerformAction _ AppState _ AppAction
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

save :: forall eff. AppState -> Aff (save :: SAVE | eff) (Canceler (save :: SAVE | eff))
save s = do
  let
    allDefs = map defToDoc s.defs <> reverse s.history
    text = intercalate "\n" (map s.rep allDefs)
  forkAff (liftEff (saveTextAs text "evaluation.txt"))

fail :: String -> AppState -> AppState
fail message s =
  s { error = Just (Tuple Danger message) }

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

remove :: Name -> AppState -> AppState
remove name s =
  s { defs = deleteByName name s.defs, env = env', error = error }
 where
  env' = Map.delete name s.env
  names = namesReferencing name env'
  error = if Set.size names == 0
             then Nothing
             else Just (Tuple Warning (formatUndefinedWarning name names))

deleteByName :: Name -> Array Definition -> Array Definition
deleteByName name = filter (_.name >>> (_ /= name))

addDef :: Definition -> AppState -> AppState
addDef def s =
  if Set.size missing == 0
    then
      s { text = "", defs = defs, env = env }
    else
      fail (formatUndefinedError s.text missing) s
 where
  defs = deleteByName def.name s.defs `snoc` def
  expr = syntaxToExpr (defToSyntax def)
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
      let expr'' = alpha expr'
      in s { history = prettyPrint (exprToSyntax expr'') `cons` s.history, expr = Just expr'' }

clear :: AppState -> AppState
clear s = s { history = maybe [] (exprToSyntax >>> prettyPrint >>> singleton) s.expr }

toggleSugar :: AppState -> AppState
toggleSugar s = ifSugar s.rep (s { rep = raw }) (s { rep = sugar })

render :: T.Render AppState _ AppAction
render send _ state _ =
  [ RD.div
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
      (renderDefs send state.rep state.defs)
    , RD.div
      [ RP.className "row" ]
      (renderExprs send state.rep state.history state.expr)
    , RD.div
      [ RP.className "row" ]
      [ footer ]
    ]
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

renderDefs :: _ -> (Doc String -> String) -> Array Definition -> Array R.ReactElement
renderDefs send rep definitions =
  [ RD.div
    [ RP.className "col-sm-12" ]
    [ RD.h3' [ RD.text "Definitions" ] ]
  , RD.div
    [ RP.className "col-sm-12 monospace-font col-sm-12" ]
    (map (renderDef send rep) definitions)
  ]

renderDef :: _ -> (Doc String -> String) -> Definition -> R.ReactElement
renderDef send rep def = RD.h4'
  [ RD.div
    [ RP.className "glyphicon glyphicon-remove"
    , RP.onClick \_ -> send (Remove def.name)
    ]
    []
  , RD.text (" " <> rep (defToDoc def))
  ]

renderExprs :: _ -> (Doc String -> String) -> Array (Doc String) -> Maybe Expr -> Array R.ReactElement
renderExprs send rep history Nothing = []
renderExprs send rep history (Just expr) =
  [ RD.div
    [ RP.className "col-sm-6" ]
    [ RD.h3' [ RD.text "Evaluation" ] ]
  , RD.div
    [ RP.className "col-sm-6" ]
    [ RD.div
      [ RP.className "add-margin-medium btn-group pull-right" ]
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
      , RD.button
        [ RP.className ("btn " <> ifSugar rep "btn-danger" "btn-success")
        , RP.onClick \_ -> send ToggleSugar
        ]
        [ RD.text (ifSugar rep "Raw" "Sugar") ]
      ]
    ]
  , RD.div
    [ RP.className "col-sm-12 hide-overflow" ]
    [ RD.div
      [ RP.className "scroll-overflow monospace-font" ]
      (renderHistory rep history)
    ]
  ]

renderHistory :: (Doc String -> String) -> Array (Doc String) -> Array R.ReactElement
renderHistory rep hs =
  case uncons hs of
    Nothing -> []
    Just { head: head, tail: tail } ->
      RD.h4' [ RD.text (rep head) ] `cons` map (rep >>> renderSyntax) tail

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

