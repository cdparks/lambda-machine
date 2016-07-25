module Data.PrettyPrint
  ( Doc
  , runDoc
  , doc
  , sugar
  , raw
  , class PrettyPrint
  , prettyPrint
  , parensIf
  ) where

import Prelude
import Data.Monoid

data Doc a = Doc a a

runDoc :: forall a. Doc a -> {sugar :: a, raw :: a}
runDoc (Doc s r) = {sugar: s, raw: r}

doc :: forall a. {sugar :: a, raw :: a} -> Doc a
doc o = Doc o.sugar o.raw

sugar :: forall a. Doc a -> a
sugar (Doc s _) = s

raw :: forall a. Doc a -> a
raw (Doc _ r) = r

instance docFunctor :: Functor Doc where
  map f (Doc s d) = Doc (f s) (f d)

instance docApply :: Apply Doc where
  apply (Doc f g) (Doc a b) = Doc (f a) (g b)

instance docApplicative :: Applicative Doc where
  pure a = Doc a a

instance docSemigroup :: Semigroup a => Semigroup (Doc a) where
  append d1 d2 = append <$> d1 <*> d2

instance docMonoid :: Monoid a => Monoid (Doc a) where
  mempty = pure mempty

class PrettyPrint a where
  prettyPrint :: a -> Doc String

parensIf :: Boolean -> Doc String -> Doc String
parensIf cond d
  | cond      = pure "(" <> d <> pure ")"
  | otherwise = d
