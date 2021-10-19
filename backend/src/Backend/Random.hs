module Backend.Random
  ( HasRandom(..)
  , Random
  , new
  , uniform
  , range
  ) where

import Backend.Prelude

import System.Random.MWC
  (GenIO, Uniform(..), UniformRange(..), createSystemRandom)
import System.Random.Stateful (StatefulGen(..))

-- | Environment has access to a random generator
class HasRandom env where
  randomLens :: Lens' env Random

instance HasRandom Random where
  randomLens = id
  {-# INLINE randomLens #-}

-- | Fix source of randomness to use 'IO'
newtype Random = Random GenIO

instance StatefulGen Random IO where
  uniformWord32 = coerce @(GenIO -> IO Word32) uniformWord32
  {-# INLINE uniformWord32 #-}
  uniformWord64 = coerce @(GenIO -> IO Word64) uniformWord64
  {-# INLINE uniformWord64 #-}

-- | Create new generator
new :: MonadIO m => m Random
new = coerce <$> liftIO createSystemRandom

-- | Generate a random value
uniform :: forall a env . (HasRandom env, Uniform a) => RIO env a
uniform = do
  gen <- view randomLens
  liftIO $ uniformM gen

-- | Generate a random value from within an inclusive range
range :: forall a env . (HasRandom env, UniformRange a) => (a, a) -> RIO env a
range x = do
  gen <- view randomLens
  liftIO $ uniformRM x gen
