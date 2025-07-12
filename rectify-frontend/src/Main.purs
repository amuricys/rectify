module Main where

import Prelude

import Component.Parent as Parent
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Parent.component unit body