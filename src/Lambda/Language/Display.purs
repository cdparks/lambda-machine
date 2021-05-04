module Lambda.Language.Display
  ( class Display
  , text
  , style
  , class Pretty
  , pretty
  , Rep(..)
  , parensIf
  , select
  , toggle
  )
  where

import Lambda.Prelude

import React.Basic (JSX)
import React.Basic.DOM as R

class Monoid r <= Display r where
  text :: String -> r
  style :: String -> r -> r

data Rep = Raw | Sugar

class Pretty a where
  pretty :: forall r. Display r => Rep -> a -> r

parensIf :: forall r. Display r => Boolean -> r -> r
parensIf cond body
  | cond = text "(" <> body <> text ")"
  | otherwise = body

select :: forall a. Rep -> { sugar :: a, raw :: a } -> a
select rep {sugar, raw} = case rep of
  Sugar -> sugar
  Raw -> raw

toggle :: Rep -> Rep
toggle = case _ of
  Sugar -> Raw
  Raw -> Sugar

instance displayString :: Display String where
  text s = s
  style _ s = s

instance displayJSX :: Display JSX where
  text = R.text
  style className body = R.span { className, children: [body] }
