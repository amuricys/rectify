module Component.Optimization.Diagram.Canvas where

import Prelude

import Backend.Optimization.Problem as Problem
import Backend.Optimization.Problem.Reservoir as Reservoir
import Backend.Optimization.Problem.Surface as Surface
import Backend.Optimization.Problem.TSP as TSP
import Component.Optimization.Diagram as Diagram
import Data.Maybe (Maybe(..))
import Diagram.Surface as Diagram.Surface
import Diagram.TSP as Diagram.TSP
import Effect.Class (class MonadEffect)
import Halogen as H

-- Maybe use https://hackage.haskell.org/package/purescript-bridge for this

canvasDivId :: String
canvasDivId = "canvasDiv"

data Query a
  = ReceiveDiagramData Problem.Problem a
  | ProblemChange Problem.Problem a
initDiagram' prob = case prob of
  Problem.Surface state -> Diagram.initDiagram canvasDivId (Surface.fromSolution state) Diagram.Surface.diag
  Problem.TSP state -> Diagram.initDiagram canvasDivId (TSP.fromSolution state) Diagram.TSP.diag
  Problem.Reservoir state -> Diagram.initDiagram canvasDivId (Reservoir.fromSolution state) Diagram.Surface.diag

updateDiagram' d prob = case prob of
  Problem.Surface state -> Diagram.updateDiagram d (Surface.fromSolution state)
  Problem.TSP state -> Diagram.updateDiagram d (TSP.fromSolution state)
  Problem.Reservoir state -> Diagram.updateDiagram d (Reservoir.fromSolution state)

handleQuery :: forall m a. 
  MonadEffect m 
  => Query a 
  -> H.HalogenM (Diagram.State Unit) Diagram.Action () Void m (Maybe a)
handleQuery = case _ of
  -- Here we'll talk to GoJS
  ProblemChange prob a -> do
    initDiagram' prob
    pure $ Just a
  ReceiveDiagramData msg a -> do
    x <- H.get
    case x.diagram of
      Just d -> updateDiagram' d msg *> pure (Just a)
      -- If for some reason we don't have a diagram, we'll initialize a surface one
      Nothing -> pure $ Just a -- Diagram.initDiagram canvasDivId msg Diagram.Surface.diag *> pure (Just a)


component :: forall i m. MonadEffect m => H.Component Query i Void m
component = Diagram.diagramComponent unit canvasDivId Diagram.Surface.diag handleQuery