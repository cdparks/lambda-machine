module Lambda.Language.Snapshot.Signature
  ( Signature
  , deflate
  , inflate
  , nil
  ) where

import Lambda.Prelude

import Data.Array as Array
import Data.Int.Bits ((.&.), (.|.))
import Lambda.Language.Definition (Definition(..))
import Lambda.Language.Prelude as Prelude

-- | Bit field indicating which prelude definitions are used
newtype Signature = Signature Int

derive newtype instance eqSignature :: Eq Signature
derive newtype instance showSignature :: Show Signature
derive newtype instance encodeJsonSignature :: EncodeJson Signature
derive newtype instance decodeJsonSignature :: DecodeJson Signature

-- | Empty Signature uses no prelude definitions
nil :: Signature
nil = Signature 0

-- | Generate prelude signature by OR'ing together ids
deflate :: Array Definition -> Signature
deflate = Signature <<< flip foldl 0 \acc (Definition {id}) -> acc .|. id

-- | Convert signature back into prelude definitions
inflate :: Signature -> Array Definition
inflate (Signature sig) = Array.filter used Prelude.defs
 where
  used (Definition {id}) = (id .&. sig) /= 0
