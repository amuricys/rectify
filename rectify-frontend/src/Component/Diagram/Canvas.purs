module Component.Diagram.Canvas where

import Prelude

import Algorithm (Algorithm(..))
import Backend (DiagramData)
import Component.Diagram as Diagram
import Data.Maybe (Maybe(..))
import Diagram.Surface as Diagram.Surface
import Effect.Class (class MonadEffect)
import Halogen as H

-- Maybe use https://hackage.haskell.org/package/purescript-bridge for this

canvasDivId :: String
canvasDivId = "canvasDiv"

data Query a
  = ReceiveDiagramData (DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData) a
  | AlgorithmChange (DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData) Algorithm a

handleQuery :: forall m a. MonadEffect m => Query a -> H.HalogenM Diagram.State Diagram.Action () Void m (Maybe a)
handleQuery = case _ of
  -- Here we'll talk to GoJS
  AlgorithmChange state alg a -> do
    let diag = case alg of
                Surface -> Diagram.Surface.diag
                TSP -> Diagram.Surface.diag
                Reservoir -> Diagram.Surface.diag
    Diagram.initDiagram canvasDivId state diag
    pure $ Just a
  ReceiveDiagramData msg a -> do
    H.get >>= case _ of
      Diagram.DiagramAndState { diagram: d } -> Diagram.updateDiagram d msg a
      -- If for some reason we don't have a diagram, we'll initialize a surface one
      Diagram.NoDiagramYet { state: s } -> Diagram.initDiagram canvasDivId msg Diagram.Surface.diag

component :: forall i m. MonadEffect m => H.Component Query i Void m
component = Diagram.diagramComponent unit canvasDivId Diagram.Surface.diag handleQuery