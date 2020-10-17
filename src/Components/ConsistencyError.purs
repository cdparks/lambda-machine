module Components.ConsistencyError
  ( component
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.Grammar as Grammar
import Data.Set as Set
import Lambda.Language.World (ConsistencyError(..))
import React.Basic (JSX)
import React.Basic.DOM as R

type Props =
  { error :: ConsistencyError
  }

component :: Props -> JSX
component { error } = R.p { children }
 where
  children = case error of
    Undefined missing ->
      [ R.text "No top-level "
      , R.text $ Grammar.pluralizeWith "s" (Set.size missing) "definition"
      , R.text " for "
      , R.span_ $ join missing
      ]
    CannotDelete name deps ->
      [ R.text "Cannot delete "
      , code $ show name
      , R.text " because it's still referenced by "
      , R.span_ $ join deps
      ]

  join :: forall a f. Show a => Foldable f => f a -> Array JSX
  join = Grammar.joinWith {inject, conjunction: [R.text "and"]}
    <<< map (pure <<< code <<< show)
    <<< Array.fromFoldable

  inject :: String -> Array JSX
  inject = pure <<< R.text

  code :: String -> JSX
  code = R.code_ <<< pure <<< R.text
