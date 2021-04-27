module Lambda.Language.PrettyPrint
  ( Rep(..)
  , ifSugar
  , selectRep
  , withRep
  , toggleRep
  , Doc
  , doc
  , sugar
  , raw
  , class PrettyPrint
  , prettyPrint
  , parensIf
  ) where

import Lambda.Prelude

-- | Either syntactic sugar or raw text.
data Rep
  = Sugar
  | Raw

-- | Project the appropriate `Doc` representation.
selectRep :: forall a. Doc a -> Rep -> a
selectRep (Doc s r) = ifSugar s r

-- | Flipped version of 'selectRep'
withRep :: forall a. Rep -> Doc a -> a
withRep = flip selectRep

-- | Flip `Raw` to `Sugar` and vice-versa.
toggleRep :: Rep -> Rep
toggleRep = ifSugar Raw Sugar

-- | Select the first argument given `Sugar`.
ifSugar :: forall a. a -> a -> Rep -> a
ifSugar s _ Sugar = s
ifSugar _ r Raw = r

-- | Document containing two representations of the same value.
data Doc a = Doc a a

-- | Project both representations out of `Doc`.
runDoc :: forall a. Doc a -> {sugar :: a, raw :: a}
runDoc (Doc s r) = {sugar: s, raw: r}

-- | Smart constructor for `Doc`.
doc :: forall a. {sugar :: a, raw :: a} -> Doc a
doc o = Doc o.sugar o.raw

-- | Project sugared representation out of `Doc`.
sugar :: forall a. Doc a -> a
sugar (Doc s _) = s

-- | Project raw representation out of `Doc`.
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

-- | Wrap `Doc` in parens if condition is satisfied.
parensIf :: Boolean -> Doc String -> Doc String
parensIf cond d
  | cond      = pure "(" <> d <> pure ")"
  | otherwise = d
