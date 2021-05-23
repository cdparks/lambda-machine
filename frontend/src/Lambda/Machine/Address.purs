module Lambda.Machine.Address
  ( Address
  , offset
  , baseptr
  , nullptr
  ) where

import Lambda.Prelude

-- | Heap address for nodes in a graph
newtype Address = Address Int

derive newtype instance eqAddress :: Eq Address
derive newtype instance hashableAddress :: Hashable Address
derive newtype instance ordAddress :: Ord Address

instance showAddress :: Show Address where
  show (Address i) = "#" <> show i

offset :: Int -> Address -> Address
offset i address = Address $ coerce address + i

baseptr :: Address
baseptr = coerce 1

nullptr :: Address
nullptr = coerce 0
