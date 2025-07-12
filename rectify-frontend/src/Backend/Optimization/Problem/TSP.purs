module Backend.Optimization.Problem.TSP where

import Prelude

import Component.Optimization.Diagram (DiagramData(..))
import Data.Array (length, zip, (..))
import Data.Tuple (uncurry)
import Diagram.TSP as Diagram.TSP

type TSPPoint = { x :: Number, y :: Number }

type SolutionData = { cities :: Array TSPPoint }


cyclicalArrayToDiagramData
  :: String -> Int -> Array TSPPoint -> DiagramData Diagram.TSP.NodeData Diagram.TSP.LinkData
cyclicalArrayToDiagramData cat start points = DiagramData {
  nodes: map (uncurry $ toPoint cat) (zip (start .. (start + length points)) points)
  , links: map (toLink cat start (start + length points - 1)) (start .. (start + length points - 1))
  }

toPoint :: String -> Int -> TSPPoint -> Record Diagram.TSP.NodeData
toPoint cat id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }

toLink :: String -> Int -> Int -> Int -> Record Diagram.TSP.LinkData
toLink cat first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

fromSolution :: SolutionData -> DiagramData Diagram.TSP.NodeData Diagram.TSP.LinkData
fromSolution { cities } = cyclicalArrayToDiagramData "Outer" 0 cities

