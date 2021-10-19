module Backend.Code
  ( Code
  , new
  , parse
  , ParseError(..)
  ) where

import Backend.Prelude

import Backend.Random (HasRandom)
import qualified Backend.Random as Random
import qualified Data.Text as Text
import Data.Vector ((!))
import qualified Data.Vector as V
import Text.Read (Read(..))

-- | Random 8-character code using 32 unambiguous characters
--
-- See http://www.crockford.com/base32.html. We're not encoding
-- anything, just trying to make semi-readable non-numeric ids.
--
newtype Code = Code Text
  deriving newtype (Eq, Show, Ord, Hashable, ToJSON, ToHttpApiData, PersistField, PersistFieldSql)

-- | Errors encountered during parsing
data ParseError
  = -- | Code must be 8 characters long
    WrongLength Text
  | -- | Code must only consist of characters from 'alphabet'
    UnrecognizedCharacters Text
  deriving stock (Eq, Show)

instance Display ParseError where
  display = \case
    WrongLength text -> "wrong length for code: " <> display text
    UnrecognizedCharacters text ->
      "unrecognized characters in code: " <> display text

instance PathPiece Code where
  toPathPiece = coerce
  fromPathPiece = hush . parse

instance FromHttpApiData Code where
  parseUrlPiece = first textDisplay . parse

instance FromJSON Code where
  parseJSON =
    withText "Code" $ either (fail . unpack . textDisplay) pure . parse

instance Read Code where
  readsPrec _ = either mempty ok . parse . pack where ok code = [(code, "")]

-- | Generate a new random 'Code'
new :: forall env . HasRandom env => RIO env Code
new = Code . pack <$> replicateM size char
  where char = (alphabet !) <$> Random.range (0, lastIndex)

-- | Parse 'Code' from 'Text'
parse :: Text -> Either ParseError Code
parse raw
  | Text.length upper /= size = Left $ WrongLength raw
  | Text.any (`notElem` alphabet) upper = Left $ UnrecognizedCharacters raw
  | otherwise = pure $ Code upper
  where upper = Text.toUpper raw

-- | Set of allowed characters
--
-- NOINLINE - compute once and store as a CAF.
alphabet :: Vector Char
alphabet = V.fromList $ ['0' .. '9'] <> filter unambiguous ['A' .. 'Z']
  where unambiguous = (`notElem` ("ILOU" :: String))
{-# NOINLINE alphabet #-}

-- | Last valid index into 'alphabet'
--
-- NOINLINE - compute once and store as a CAF.
lastIndex :: Int
lastIndex = V.length alphabet - 1
{-# NOINLINE lastIndex #-}

-- | 'Code' is always 8 characters long
size :: Int
size = 8
