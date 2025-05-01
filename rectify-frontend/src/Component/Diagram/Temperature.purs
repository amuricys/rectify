module Component.Diagram.Temperature where

import Prelude

import Backend.TimeSeries (TemperatureData)
import Component.Diagram.TimeSeries as Component.TimeSeries
import Diagram.TimeSeries (mkDataPointNode, mkLink)
import Effect.Class (class MonadEffect)
import Halogen as H

canvasDivId :: String
canvasDivId = "temperatureDiv"

yScale :: Number
yScale = 2000.0

component :: forall i m. MonadEffect m => H.Component (Component.TimeSeries.Query TemperatureData) i Void m
component = Component.TimeSeries.component @100
  canvasDivId
  (\msg -> { beta: msg.beta, betaCounter: msg.betaCounter })
  (\i x -> mkDataPointNode (-i) (x.beta / yScale) x.betaCounter (x.beta / yScale))
  (\x -> mkLink x (x + 1))