module Optimization.Renderer.Diagram where

import Prelude

import Diagram.Surface as Surface
import Diagram.TSP as TSP
import Effect (Effect)
import Effect.Exception (throw)
import GoJS.Diagram (Diagram_, _model)
import GoJS.Model (mergeLinkDataArray_, mergeNodeDataArray_)
import Optimization.Problem (toDiagramData)
import Optimization.Problem as Problem
import Went.Diagram.Make (MakeDiagram)
import Went.Diagram.Make as Went

type MkDiagram nodeData linkData = Array (Record nodeData) -> Array (Record linkData) -> MakeDiagram nodeData linkData Diagram_ Unit
updateDiagram :: forall p nodeData linkData. Problem.DiagramData p nodeData linkData => Diagram_ -> p -> Effect Unit
updateDiagram diagram prob = do
  let { nodes, links } = toDiagramData prob
  let m = diagram # _model
  m # mergeNodeDataArray_ nodes
  m # mergeLinkDataArray_ links

initDiagram :: forall p nodeData linkData. Problem.DiagramData p nodeData linkData => String -> p -> MkDiagram nodeData linkData -> Effect Diagram_
initDiagram divId prob mk = do
  let { nodes, links } = toDiagramData prob
  Went.make divId (mk nodes links)
