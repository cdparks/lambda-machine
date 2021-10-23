module Lambda.Language.Snapshot.Tag
  ( Tag(..)
  , _VAR
  , _NAT
  , _TAK
  , _LAM
  , _DEF
  , _APV
  , _AP0
  ) where

import Lambda.Prelude

import Lambda.Language.Pretty (class Pretty, text)

-- | 3-bit tag used to encode RPN instructions as Ints
newtype Tag = Tag Int

derive instance newtypeTag :: Newtype Tag _
derive newtype instance eqTag :: Eq Tag

instance showTag :: Show Tag where
  show tag
    | tag == _VAR = "_VAR"
    | tag == _NAT = "_NAT"
    | tag == _TAK = "_TAK"
    | tag == _LAM = "_LAM"
    | tag == _DEF = "_DEF"
    | tag == _APV = "_APV"
    | tag == _APV = "_AP0"
    | otherwise = "?" <> show (un Tag tag)

instance prettyTag :: Pretty Tag where
  pretty _ = text <<< show

-- | Fetch name at index
_VAR :: Tag
_VAR = Tag 1

-- | Push literal natural number
_NAT :: Tag
_NAT = Tag 2

-- | Take N items from stack as literal list
_TAK :: Tag
_TAK = Tag 3

-- | Create lambda from name and expression on stack
_LAM :: Tag
_LAM = Tag 4

-- | Create definition from name and expression on stack
_DEF :: Tag
_DEF = Tag 5

-- | Apply name using variable index to top of stack
_APV :: Tag
_APV = Tag 6

-- | Apply top of stack to element beneath it
_AP0 :: Tag
_AP0 = Tag 7
