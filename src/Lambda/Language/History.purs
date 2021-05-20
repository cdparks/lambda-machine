module Lambda.Language.History
  ( History
  , empty
  , new
  , add
  , toStrings
  , toJSX
  ) where

import Lambda.Prelude hiding (add)

import Lambda.Language.Display (class Display, class Pretty, pretty, text, style, select, Rep(..))
import React.Basic (JSX)

-- | Cache pretty-printed JSX and text with and without syntactic sugar
-- |
-- | Then we can incrementally build each list and switch between them without
-- | redoing a bunch of work.
-- |
newtype History = History
  { sugar :: List Bundle
  , raw :: List Bundle
  }

derive instance newtypeHistory :: Newtype History _

-- | No history
empty :: History
empty = History {sugar: Nil, raw: Nil}

-- | Single expression
new :: forall a. Pretty a => a -> History
new a = add a empty

-- | Add expression
add :: forall a. Pretty a => a -> History -> History
add a (History {sugar, raw}) = History
  { sugar: Cons (pretty Sugar a) sugar
  , raw: Cons (pretty Raw a) raw
  }

-- | Pretty-printed text format, most-recently evaluated first
toStrings :: Rep -> History -> List String
toStrings = toListWith $ _.text <<< un Bundle

-- | Pretty-printed JSX format, most-recently evaluated first
toJSX :: Rep -> History -> List JSX
toJSX = toListWith $ _.jsx <<< un Bundle

-- | Extract one list of pretty-printed elments from the history
toListWith :: forall r. (Bundle -> r) -> Rep -> History -> List r
toListWith f rep = map f <<< select rep <<< un History

-- | Cache JSX and text representation
newtype Bundle = Bundle
  { jsx :: JSX
  , text :: String
  }

derive instance newtypeBundle :: Newtype Bundle _
derive instance genericBundle :: Generic Bundle _

instance semigroupBundle :: Semigroup Bundle where
  append (Bundle x) (Bundle y) = Bundle $ x <> y

instance monoidBundle :: Monoid Bundle where
  mempty = Bundle
    { jsx: mempty
    , text: mempty
    }

instance displayBundle :: Display Bundle where
  text s = Bundle
    { jsx: text s
    , text: text s
    }
  style className (Bundle {jsx, text}) = Bundle
    { jsx: style className jsx
    , text: style className text
    }
