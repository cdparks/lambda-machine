module Backend.Env
  ( new
  , Env(..)
  ) where

import Backend.Prelude

import Backend.Database (HasSqlPool(..), SqlPool, newSqlPool)
import Backend.Random (HasRandom(..), Random)
import qualified Backend.Random as Random
import Backend.Settings (HasSettings(..), Settings(..))

new :: Settings -> IO Env
new settings@Settings {..} = do
  sqlPool <- runNoLoggingT $ newSqlPool postgresConf
  random <- Random.new
  options <- setLogMinLevel logLevel <$> logOptionsHandle stdout False
  (logger, shutdown) <- newLogFunc options
  pure Env{..}

data Env = Env
  { random :: Random
  , sqlPool :: SqlPool
  , settings :: Settings
  , logger :: LogFunc
  , shutdown :: IO ()
  }

instance HasRandom Env where
  randomLens = lens random $ \env r -> env { random = r }
  {-# INLINE randomLens #-}

instance HasSqlPool Env where
  sqlPoolLens = lens sqlPool $ \env x -> env { sqlPool = x }
  {-# INLINE sqlPoolLens #-}

instance HasSettings Env where
  settingsLens = lens settings $ \env x -> env { settings = x }
  {-# INLINE settingsLens #-}

instance HasLogFunc Env where
  logFuncL = lens logger $ \env x -> env { logger = x }
  {-# INLINE logFuncL #-}
