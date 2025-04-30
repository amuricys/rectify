module Backend where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)


class FromSolution a where
  fromSolution :: Solution -> a

type SurfaceSolutionData = { inner :: Array BackendPoint, outer :: Array BackendPoint }
type TSPSolutionData = { cities :: Array BackendPoint }

data Solution
  = SurfaceSolution SurfaceSolutionData
  | TSPSolution TSPSolutionData

derive instance Generic Solution _
instance decodeJsonSolution :: DecodeJson Solution where
  decodeJson a = genericDecodeJson a

instance encodeJsonSolution :: EncodeJson Solution where
  encodeJson a = genericEncodeJson a

type SimState sol =
  { betaCounter :: Int
  , beta :: Number
  , fitness :: Number
  , solution :: sol
  }

type BackendPoint = { x :: Number, y :: Number }

newtype DiagramData nodeData linkData = DiagramData { nodes :: Array (Record nodeData), links :: Array (Record linkData) }

derive newtype instance semigroupDiagramData :: Semigroup (DiagramData nodeData linkData)
derive newtype instance decodeJsonDiagramData :: (DecodeJson (Record nodeData), DecodeJson (Record linkData)) => DecodeJson (DiagramData nodeData linkData)
derive newtype instance encodeJsonDiagramData :: (EncodeJson (Record nodeData), EncodeJson (Record linkData)) => EncodeJson (DiagramData nodeData linkData)

parse :: String -> Either String (SimState Solution)
parse = lmap show <<< decodeJson @(SimState Solution) <=< jsonParser