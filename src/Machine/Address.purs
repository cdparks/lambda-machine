module Machine.Address
  ( Address
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable)
import Data.Newtype (class Newtype)

newtype Address = Address Int

derive instance newtypeAddress :: Newtype Address _
derive instance genericAddress :: Generic Address _
derive newtype instance eqAddress :: Eq Address
derive newtype instance hashableAddress :: Hashable Address
derive newtype instance ordAddress :: Ord Address
derive newtype instance semiringAddress :: Semiring Address
derive newtype instance ringAddress :: Ring Address

instance showAddress :: Show Address where
  show (Address i) = "#" <> show i
