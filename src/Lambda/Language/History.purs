module Lambda.Language.History
  ( History
  , empty
  , new
  , add
  , toStrings
  , toJSX
  ) where

import Lambda.Prelude hiding (add)

import Lambda.Language.Pretty (class Pretty, pretty, select, Rep(..))
import Lambda.Language.Pretty as P
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

-- | No history
empty :: History
empty = History {sugar: Nil, raw: Nil}

-- | Single expression
new :: forall a. Pretty a => a -> History
new a = add a empty

-- | Add expression
add :: forall a. Pretty a => a -> History -> History
add a (History {sugar, raw}) = History
  { sugar: Cons (bundle Sugar a) sugar
  , raw: Cons (bundle Raw a) raw
  }

-- | Pretty-printed text format, most-recently evaluated first
toStrings :: Rep -> History -> List String
toStrings = toListWith $ _.text

-- | Pretty-printed JSX format, most-recently evaluated first
toJSX :: Rep -> History -> List JSX
toJSX = toListWith $ _.jsx

-- | Extract one list of pretty-printed elments from the history
toListWith :: forall r. (Bundle -> r) -> Rep -> History -> List r
toListWith f rep = map f <<< select rep <<< coerce

-- | Pretty print and bundle JSX and text representations
bundle :: forall a. Pretty a => Rep -> a -> Bundle
bundle rep a =
  { jsx: P.toJSX node
  , text: P.toString node
  }
 where
  node = pretty rep a

type Bundle =
  { jsx :: JSX
  , text :: String
  }
