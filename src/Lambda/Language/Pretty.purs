module Lambda.Language.Pretty
  ( class Pretty
  , pretty
  , Rep(..)
  , parensIf
  , select
  , toggle
  , Builder
  , text
  , style
  , toJSX
  , toString
  )
  where

import Lambda.Prelude

import React.Basic (JSX)
import React.Basic.DOM as R

data Rep = Raw | Sugar

derive instance genericRep :: Generic Rep _
derive instance eqRep :: Eq Rep

-- | Pretty-print some structure to a Builder
class Pretty a where
  pretty :: Rep -> a -> Builder

-- | Wrap with parentheses if condition is true
parensIf :: Boolean -> Builder -> Builder
parensIf cond body
  | cond = text "(" <> body <> text ")"
  | otherwise = body

-- | Select from object based on 'Rep'
select :: forall a. Rep -> { sugar :: a, raw :: a } -> a
select rep {sugar, raw} = case rep of
  Sugar -> sugar
  Raw -> raw

toggle :: Rep -> Rep
toggle = case _ of
  Sugar -> Raw
  Raw -> Sugar

-- | (<>) for JSX is really slow if you have lots of text fragments.
-- | By combining adjacent text, and _then_ going to JSX, we create
-- | way fewer DOM nodes.
data Builder
  = Empty
  | Text String
  | Style String Builder
  | Append Builder Builder

instance semigroupBuilder :: Semigroup Builder where
  append = Append

instance monoidBuilder :: Monoid Builder where
  mempty = Empty

-- | Text node
text :: String -> Builder
text = Text

-- | Style node using CSS classname
style :: String -> Builder -> Builder
style = Style

-- | Render builder to JSX
toJSX :: Builder -> JSX
toJSX = buildWith R.text \className node -> R.span
  { className
  , children: [node]
  }

-- | Render builder to text
toString :: Builder -> String
toString = buildWith identity \_ -> identity

-- | Reassociate appends to the right and pre-concatentate strings
buildWith
  :: forall r
   . Monoid r
  => (String -> r)
  -- ^ Generate text representation
  -> (String -> r -> r)
  -- ^ Generate styled representation
  -> Builder
  -> r
buildWith fromText fromStyle = search
 where
  -- Look for text and reassociate appends
  search = case _ of
    Text s -> fromText s
    Style cls node -> fromStyle cls $ search node
    Append (Text s) rhs -> collect s rhs
    Append (Append s t) u -> search $ Append s $ Append t u
    Append s t -> search s <> search t
    Empty -> mempty

  -- Accumulate text and reassociate appends
  collect acc = case _ of
    Text s -> fromText $ acc <> s
    Append (Text s) rhs -> collect (acc <> s) rhs
    Append (Append s t) u -> collect acc $ Append s $ Append t u
    node -> fromText acc <> search node
