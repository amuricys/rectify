module Optimization.Temperature.Component where

import Prelude

import Optimization.TimeSeries.Component as TimeSeries
import Diagram.TimeSeries (mkDataPointNode, mkLink)
import Effect.Class (class MonadEffect)
import Halogen as H

canvasDivId :: String
canvasDivId = "temperatureDiv"

yScale :: Number
yScale = 2000.0

newtype TemperatureData = TemperatureData { beta :: Number, betaCounter :: Int }

-- component :: forall i m. MonadEffect m => H.Component (TimeSeries.Query TemperatureData) i Void m
-- component = TimeSeries.component @100
--   canvasDivId
--   (\msg -> { beta: msg.beta, betaCounter: msg.betaCounter })
--   (\i x -> mkDataPointNode (-i) (x.beta / yScale) x.betaCounter (x.beta / yScale))
--   (\x -> mkLink x (x + 1))