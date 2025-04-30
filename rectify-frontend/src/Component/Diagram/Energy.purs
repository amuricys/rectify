module Component.Diagram.Energy where

import Prelude

import Backend (DiagramData(..))
import Component.Diagram as Diagram
import Data.Maybe (Maybe(..))
import Diagram.Surface as Diagram.Surface
import Effect.Class (class MonadEffect)
import Halogen as H

canvasDivId :: String
canvasDivId = "energyDiv"

data Query a = ReceiveEnergy { fitness :: Number } a

handleQuery :: forall m a. MonadEffect m => Query a -> H.HalogenM Diagram.State Diagram.Action () Void m (Maybe a)
handleQuery = case _ of
  -- Here we'll talk to GoJS
  ReceiveEnergy msg a -> do
    H.get >>= case _ of
      Diagram.Diagram d -> do
        
        Diagram.updateDiagram d (DiagramData {nodes: [msg], links: []}) a
      Diagram.NoDiagramYet -> pure $ Just a

component :: forall i m. MonadEffect m => H.Component Query i Void m
component = Diagram.diagramComponent canvasDivId Diagram.Surface.diag handleQuery