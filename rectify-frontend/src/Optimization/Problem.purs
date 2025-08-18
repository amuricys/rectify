module Optimization.Problem where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Array (length, zip, (..))
import Data.Generic.Rep (class Generic)
import Data.Tuple (uncurry)
import Diagram.Surface as Diagram.Surface
import Diagram.TSP as Diagram.TSP




type Point2D = { x :: Number, y :: Number }

type SurfaceSolution = { inner :: Array Point2D, outer :: Array Point2D }

type TSPSolution = { cities :: Array Point2D }

type ReservoirSolution = { network :: Array Point2D }

data RendererType
foreign import data GoJSTag :: RendererType
foreign import data ThreeJSTag :: RendererType

data Problem
  = Surface SurfaceSolution
  | TSP TSPSolution
  | Reservoir ReservoirSolution
  | Surface3D
  | TSP3D

derive instance genericProblem :: Generic Problem _

instance decodeJsonProblem :: DecodeJson Problem where
  decodeJson = genericDecodeJson

type DiagramData nodeData linkData = { nodes :: Array (Record nodeData), links :: Array (Record linkData) }

cyclicalArrayToDiagramData :: String -> Int -> Array Point2D -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
cyclicalArrayToDiagramData cat start points = {
  nodes: map (uncurry toPoint) (zip (start .. (start + length points)) points)
  , links: map (toLink start (start + length points - 1)) (start .. (start + length points - 1))
  }
  where 
    toPoint :: Int -> Point2D -> Record Diagram.Surface.NodeData
    toPoint id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }
    toLink :: Int -> Int -> Int -> Record Diagram.Surface.LinkData
    toLink first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

solutionToSurfaceDiagramData :: SurfaceSolution -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
solutionToSurfaceDiagramData { inner, outer } = 
  cyclicalArrayToDiagramData "Outer" 0 outer <> cyclicalArrayToDiagramData "Inner" (length outer) inner

solutionToTSPDiagramData :: TSPSolution -> DiagramData Diagram.TSP.NodeData Diagram.TSP.LinkData
solutionToTSPDiagramData { cities } = 
  cyclicalArrayToDiagramData "Cities" 0 cities
