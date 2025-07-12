module Component.Optimization.Diagram.Energy where

import Prelude

import Backend.TimeSeries (EnergyData)
import Component.Optimization.Diagram.TimeSeries as Component.TimeSeries
import Diagram.TimeSeries (mkDataPointNode, mkLink)
import Effect.Class (class MonadEffect)
import Halogen as H
import Type.Data.Peano as Peano
import Type.Prelude (Proxy(..))

canvasDivId :: String
canvasDivId = "energyDiv"

yScale :: Number
yScale = 30000.0

component :: forall i m. MonadEffect m => H.Component (Component.TimeSeries.Query EnergyData) i Void m
component = Component.TimeSeries.component @100
  canvasDivId
  (\msg -> { fitness: msg.fitness, betaCounter: msg.betaCounter })
  (\i x -> mkDataPointNode (-i) (x.fitness / yScale - 50.0) x.betaCounter (x.fitness / yScale - 50.0))
  (\x -> mkLink x (x + 1))

hmm :: Proxy (10 :: Int)
hmm = Proxy
