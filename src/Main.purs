module Main where

import Prelude

import Effect (Effect)
import Component (ui)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

-- | Run the app.
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI ui unit body
