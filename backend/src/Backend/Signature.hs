module Backend.Signature
  ( Signature(..)
  ) where

import Backend.Prelude

-- | Bitset indiciating which Prelude definitions are used in a session
--
-- For example, 3 => 0x3 => 0b11 means that the first two Prelude
-- definitions are used. Prelude currently only has 22 definitions,
-- so we can use an 'Int32' here and integer in Postgres.
--
newtype Signature = Signature Int32
  deriving newtype (Eq, Show, Ord, Hashable, FromJSON, ToJSON, PersistField, PersistFieldSql, Default)
