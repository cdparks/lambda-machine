module Backend.Main
  ( main
  ) where

import Backend.Prelude

import Backend.Api (api)
import qualified Backend.App as App
import Backend.Middleware (middleware)
import Backend.Settings (Settings(..))
import qualified Backend.Settings as Settings
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  settings@Settings {..} <- Settings.load
  App.run settings $ \app -> do
    application <- runRIO app $ do
      logDebug $ displayShow settings
      middleware settings <$> api
    Warp.run port application
