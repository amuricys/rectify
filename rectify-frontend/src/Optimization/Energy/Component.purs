module Optimization.Energy.Component where

import Prelude

import Optimization.TimeSeries.Component as TimeSeries
import Diagram.TimeSeries (mkDataPointNode, mkLink)
import Effect.Class (class MonadEffect)
import Halogen as H
import Type.Prelude (Proxy(..))

canvasDivId :: String
canvasDivId = "energyDiv"

yScale :: Number
yScale = 30000.0

newtype EnergyData = EnergyData { fitness :: Number, betaCounter :: Int }

-- component :: forall i m. MonadEffect m => H.Component (TimeSeries.Query EnergyData) i Void m
-- component = TimeSeries.component @100
--   canvasDivId
--   (\msg -> { fitness: msg.fitness, betaCounter: msg.betaCounter })
--   (\i x -> mkDataPointNode (-i) (x.fitness / yScale - 50.0) x.betaCounter (x.fitness / yScale - 50.0))
--   (\x -> mkLink x (x + 1))

-- hmm :: Proxy (10 :: Int)
-- hmm = Proxy
