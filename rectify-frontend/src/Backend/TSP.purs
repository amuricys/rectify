module Backend.TSP where

import Prelude

import Backend (BackendPoint, DiagramData(..), TSPSolutionData)
import Data.Array (length, zip, (..))
import Data.Tuple (uncurry)
import Diagram.Surface as Diagram.Surface

cyclicalArrayToDiagramData
  :: String -> Int -> Array BackendPoint -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
cyclicalArrayToDiagramData cat start points = DiagramData
  { nodes: map (uncurry $ toPoint cat) (zip (start .. (start + length points)) points)
  , links: map (toLink cat start (start + length points - 1)) (start .. (start + length points - 1))
  }

toPoint :: String -> Int -> BackendPoint -> Record Diagram.Surface.NodeData
toPoint cat id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }

toLink :: String -> Int -> Int -> Int -> Record Diagram.Surface.LinkData
toLink cat first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

fromTSPSolutionData :: TSPSolutionData -> DiagramData Diagram.Surface.NodeData Diagram.Surface.LinkData
fromTSPSolutionData { cities } = cyclicalArrayToDiagramData "Outer" 0 cities

