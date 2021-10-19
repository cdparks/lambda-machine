module Backend.App
  ( run
  , App(..)
  ) where

import Backend.Prelude

import Backend.Database (HasSqlPool(..), SqlPool, newSqlPool)
import Backend.Random (HasRandom(..), Random)
import qualified Backend.Random as Random
import Backend.Settings (HasSettings(..), Settings(..))

run :: Settings -> (App -> IO a) -> IO a
run settings@Settings {..} act = do
  sqlPool <- runNoLoggingT $ newSqlPool postgresConf
  random <- Random.new
  options <- setLogMinLevel logLevel <$> logOptionsHandle stdout False
  withLogFunc options $ \logger -> act App { .. }

data App = App
  { random :: Random
  , sqlPool :: SqlPool
  , settings :: Settings
  , logger :: LogFunc
  }

instance HasRandom App where
  randomLens = lens random $ \env r -> env { random = r }
  {-# INLINE randomLens #-}

instance HasSqlPool App where
  sqlPoolLens = lens sqlPool $ \env x -> env { sqlPool = x }
  {-# INLINE sqlPoolLens #-}

instance HasSettings App where
  settingsLens = lens settings $ \env x -> env { settings = x }
  {-# INLINE settingsLens #-}

instance HasLogFunc App where
  logFuncL = lens logger $ \env x -> env { logger = x }
