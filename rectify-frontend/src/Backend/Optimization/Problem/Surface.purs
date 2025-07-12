module Backend.Optimization.Problem.Surface where

import Prelude

import Component.Optimization.Diagram (DiagramData(..))
import Data.Array (length, zip, (..))
import Data.Tuple (uncurry)
import Diagram.Surface as Diagram.Surface


type SurfacePoint = { x :: Number, y :: Number }

type SolutionData = { inner :: Array SurfacePoint, outer :: Array SurfacePoint }

cyclicalArrayToDiagramData
  :: String -> Int -> Array SurfacePoint -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
cyclicalArrayToDiagramData cat start points = DiagramData {
  nodes: map (uncurry $ toPoint cat) (zip (start .. (start + length points)) points)
  , links: map (toLink cat start (start + length points - 1)) (start .. (start + length points - 1))
  }

toPoint :: String -> Int -> SurfacePoint -> Record Diagram.Surface.NodeData
toPoint cat id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }

toLink :: String -> Int -> Int -> Int -> Record Diagram.Surface.LinkData
toLink cat first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

fromSolution :: SolutionData -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
fromSolution { inner, outer } = 
  cyclicalArrayToDiagramData "Outer" 0 outer <> cyclicalArrayToDiagramData "Inner" (length outer) inner
