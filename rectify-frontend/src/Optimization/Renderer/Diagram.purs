module Optimization.Renderer.Diagram where

import Prelude

import Diagram.Surface as Surface
import Diagram.TSP as TSP
import Effect (Effect)
import Effect.Exception (throw)
import GoJS.Diagram (Diagram_, _model)
import GoJS.Model (mergeLinkDataArray_, mergeNodeDataArray_)
import Optimization.Problem as Problem
import Went.Diagram.Make (MakeDiagram)
import Went.Diagram.Make as Went




type MkDiagram nodeData linkData = Array (Record nodeData) -> Array (Record linkData) -> MakeDiagram nodeData linkData Diagram_ Unit
updateDiagram :: Diagram_ -> Problem.GoJSProblem -> Effect Unit
updateDiagram diagram prob = case prob of
  Problem.GoJSSurface state -> updateD (Problem.solutionToSurfaceDiagramData state)
  Problem.GoJSTSP state -> updateD (Problem.solutionToTSPDiagramData state)
  Problem.GoJSReservoir _ -> throw "Not implemented"
  where 
    updateD :: forall nodeData linkData. Problem.DiagramData nodeData linkData -> Effect Unit
    updateD { nodes, links } = do
      let m = diagram # _model
      m # mergeNodeDataArray_ nodes
      m # mergeLinkDataArray_ links


initDiagram :: String -> Problem.GoJSProblem -> Effect Diagram_
initDiagram divId prob = case prob of
  Problem.GoJSSurface state -> initD (Problem.solutionToSurfaceDiagramData state) Surface.diag
  Problem.GoJSTSP state -> initD (Problem.solutionToTSPDiagramData state) TSP.diag
  Problem.GoJSReservoir _ -> throw "Not implemented"
  where
    initD :: forall nodeData linkData. Problem.DiagramData nodeData linkData -> MkDiagram nodeData linkData -> Effect Diagram_
    initD {nodes, links} mkDiagram = do
      Went.make divId (mkDiagram nodes links)

