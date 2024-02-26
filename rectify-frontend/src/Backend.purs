module Backend where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (length, zip, (..))
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Tuple (uncurry)
import Diagram as Diagram

data BackendSolution
  = SurfaceSolution
      { inner :: Array BackendPoint
      , outer :: Array BackendPoint
      }
  | TSPSolution
      { cities :: Array BackendPoint
      }

derive instance Generic BackendSolution _
instance decodeJsonBackendSolution :: DecodeJson BackendSolution where
  decodeJson a = genericDecodeJson a

instance encodeJsonBackendSolution :: EncodeJson BackendSolution where
  encodeJson a = genericEncodeJson a

type BackendSimState sol =
  { currentBeta :: Int
  , currentFitness :: Number
  , currentSolution :: sol
  }

type BackendPoint = { x :: Number, y :: Number }

cyclicalArrayToDiagramData :: String -> Int -> Array BackendPoint -> { nodes :: Array (Record Diagram.NodeData), links :: Array (Record Diagram.LinkData) }
cyclicalArrayToDiagramData cat start points =
  { nodes: map (uncurry $ toPoint cat) (zip (start .. (start + length points)) points)
  , links: map (toLink cat start (length points - 1)) (start .. (start + length points - 1))
  }

toPoint :: String -> Int -> BackendPoint -> Record Diagram.NodeData
toPoint cat id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }

toLink :: String -> Int -> Int -> Int -> Record Diagram.LinkData
toLink cat first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

fromSolution :: BackendSolution -> { nodes :: Array (Record Diagram.NodeData), links :: Array (Record Diagram.LinkData) }
fromSolution = case _ of
  SurfaceSolution { inner, outer } ->
    cyclicalArrayToDiagramData "Outer" 0 outer <> cyclicalArrayToDiagramData "Inner" (length outer) inner
  TSPSolution { cities } -> cyclicalArrayToDiagramData "Outer" 0 cities

parse
  :: String
  -> Either String { nodes :: Array (Record Diagram.NodeData), links :: Array (Record Diagram.LinkData) }
parse = pure <<< fromSolution <<< _.currentSolution <=< lmap show <<< decodeJson @(BackendSimState BackendSolution) <=< jsonParser
