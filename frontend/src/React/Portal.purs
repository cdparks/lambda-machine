module React.Portal
  ( new
  ) where

import Lambda.Prelude hiding (State)

import Effect.DOM (getPortal, getBody)
import React.Basic (JSX, fragment, empty)
import React.Basic.Hooks (Component, component, useState, useEffect)
import React.Basic.Hooks as Hooks
import Web.DOM.Element (Element, setAttribute, getAttribute)

type Props =
  { children :: Array JSX
  }

-- | Maybe, but all Justs (DisableScrolling) are equal
-- | Makes useEffect work even though we can't compare elements
data State
  = GetPortal
  | DisableScrolling Element

instance eqState :: Eq State where
  eq GetPortal GetPortal = true
  eq (DisableScrolling _) (DisableScrolling _) = true
  eq _ _ = false

new :: Component Props
new = component "Portal" \{ children } -> Hooks.do
  state /\ setState <- useState GetPortal
  useEffect state $ case state of
    -- Find portal element, stash it for rendering, and move onto next state
    GetPortal -> do
      portal <- getPortal
      setState $ const $ DisableScrolling portal
      pure mempty
    -- Disable scrolling on body and set up clean-up handler
    DisableScrolling _ -> do
      body <- getBody
      style <- fromMaybe "" <$> getAttribute "style" body
      setAttribute "style" "overflow: hidden;" body
      pure $ setAttribute "style" style body
  pure $ case state of
    GetPortal -> empty
    DisableScrolling portal -> createPortal (fragment children) portal

foreign import createPortal :: JSX -> Element -> JSX
