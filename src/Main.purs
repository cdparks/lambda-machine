module Main where

import Prelude

import Data.Maybe
import Data.Nullable (toMaybe)

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Component.App

import qualified React as R

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Types as DOM

body :: forall eff. Eff (dom :: DOM.DOM | eff) (Maybe DOM.Element)
body = do
  window <- DOM.window
  document <- DOM.document window
  maybeBody <- toMaybe <$> DOM.body document
  return (DOM.htmlElementToElement <$> maybeBody)

main :: forall eff. Eff (dom :: DOM.DOM, console :: CONSOLE | eff) Unit
main = do
  m <- body
  case m of
    Nothing ->
      log "No body element"
    Just element -> do
      R.render (R.createFactory appClass unit) element
      return unit

