module Backend.Main
  ( main
  ) where

import Backend.Prelude

import Backend.Api (api)
import Backend.Env (Env(..))
import qualified Backend.Env as Env
import Backend.Middleware (middleware)
import Backend.Settings (Settings(..))
import qualified Backend.Settings as Settings
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  settings <- Settings.load
  env <- Env.new settings
  app <- runRIO env $ middleware settings <$> api
  Warp.run (port settings) app `finally` shutdown env
