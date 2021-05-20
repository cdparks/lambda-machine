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

-- | Toggle 'Rep'
toggle :: Rep -> Rep
toggle = case _ of
  Sugar -> Raw
  Raw -> Sugar

-- | (<>) for JSX is really slow if you have lots of text fragments.
-- | By combining adjacent text, and _then_ going to JSX, we create
-- | way fewer DOM nodes.
newtype Builder = Builder (Node -> Node)

instance semigroupBuilder :: Semigroup Builder where
  append (Builder lhs) (Builder rhs) = Builder $ lhs <<< rhs

instance monoidBuilder :: Monoid Builder where
  mempty  = Builder identity

-- | Appends are right-associated by construction since
-- | Nodes cannot appear as their left operand
data Node
  = Leaf Leaf
  | Append Leaf Node

-- | Technically Style is a "node", but it acts like a leaf for Append
data Leaf
  = Empty
  | Text String
  | Style String Node

-- | Text node automatically coalesces adjacent text
text :: String -> Builder
text s = Builder $ case _ of
  Leaf Empty -> Leaf $ Text s
  Leaf (Text t) -> Leaf $ Text $ s <> t
  Append (Text t) rhs -> Append (Text $ s <> t) rhs
  node -> Append (Text s) node

-- | Style node using CSS classname
style :: String -> Builder -> Builder
style cls (Builder f) = Builder $ Append $ Style cls $ f $ Leaf Empty

-- | Render builder to JSX
toJSX :: Builder -> JSX
toJSX = buildWith R.text \className node -> R.span
  { className
  , children: [node]
  }

-- | Render builder to text
toString :: Builder -> String
toString = buildWith identity \_ -> identity

-- | Convert Builder to actual representation
buildWith
  :: forall r
   . Monoid r
  => (String -> r)
  -- ^ Generate text representation
  -> (String -> r -> r)
  -- ^ Generate styled representation
  -> Builder
  -> r
buildWith onText onStyle (Builder f) = onNode $ f $ Leaf Empty
 where
  onNode = case _ of
    Leaf leaf -> onLeaf leaf
    Append leaf node -> onLeaf leaf <> onNode node

  onLeaf = case _ of
    Empty -> mempty
    Text t -> onText t
    Style className node -> onStyle className $ onNode node
