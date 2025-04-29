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
import Diagram.Surface as Diagram.Surface

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
  { betaCounter :: Int
  , beta :: Number
  , fitness :: Number
  , solution :: sol
  }

type BackendPoint = { x :: Number, y :: Number }

newtype DiagramData = DiagramData { nodes :: Array (Record Diagram.Surface.NodeData), links :: Array (Record Diagram.Surface.LinkData) }

derive newtype instance semigroupDiagramData :: Semigroup DiagramData
derive newtype instance decodeJsonDiagramData :: DecodeJson DiagramData
derive newtype instance encodeJsonDiagramData :: EncodeJson DiagramData

cyclicalArrayToDiagramData
  :: String -> Int -> Array BackendPoint -> DiagramData
cyclicalArrayToDiagramData cat start points = DiagramData
  { nodes: map (uncurry $ toPoint cat) (zip (start .. (start + length points)) points)
  , links: map (toLink cat start (start + length points - 1)) (start .. (start + length points - 1))
  }

toPoint :: String -> Int -> BackendPoint -> Record Diagram.Surface.NodeData
toPoint cat id p = { key: id, loc: show p.x <> " " <> show p.y, category: cat }

toLink :: String -> Int -> Int -> Int -> Record Diagram.Surface.LinkData
toLink cat first last i = { key: i, from: i, to: if i /= last then i + 1 else first, category: cat }

fromSolution :: BackendSolution -> DiagramData
fromSolution = case _ of
  SurfaceSolution { inner, outer } ->
    cyclicalArrayToDiagramData "Outer" 0 outer <> cyclicalArrayToDiagramData "Inner" (length outer) inner
  TSPSolution { cities } -> cyclicalArrayToDiagramData "Outer" 0 cities

parse
  :: String
  -> Either String (BackendSimState DiagramData)
parse =
  pure <<< (\x -> x { solution = fromSolution x.solution })
  <=<
  lmap show <<< decodeJson @(BackendSimState BackendSolution)
  <=< jsonParser
