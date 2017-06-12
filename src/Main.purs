module Main where

import Prelude (Unit, unit)

import DOM (DOM)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Save (SAVE)

import Component.App (spec, initialState)
import Thermite as T

main :: forall eff. Eff (dom :: DOM, save :: SAVE | eff) Unit
main = T.defaultMain spec initialState unit
