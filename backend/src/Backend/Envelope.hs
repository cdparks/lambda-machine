module Backend.Envelope
  ( Envelope(..)
  ) where

import Backend.Prelude

-- | Wrap a value in an object with one field
--
-- e.g.
--
-- >>> encode (Envelope @"count" @Int 3)
-- "{\"count\":3}"
--
newtype Envelope (field :: Symbol) a = Envelope
  { unEnvelope :: a
  }
  deriving stock (Eq, Show)

instance (KnownSymbol field, ToJSON a) => ToJSON (Envelope field a) where
  toEncoding = pairs . mconcat . toPairs
  toJSON = object . toPairs

instance (KnownSymbol field, FromJSON a) => FromJSON (Envelope field a) where
  parseJSON = withObject "Envelope" $ fmap Envelope . (.: key)
    where key = pack $ symbolVal $ Proxy @field

toPairs
  :: forall field a kv
   . (KeyValue kv, KnownSymbol field, ToJSON a)
  => Envelope field a
  -> [kv]
toPairs (Envelope value) = [key .= value]
  where key = pack $ symbolVal $ Proxy @field
