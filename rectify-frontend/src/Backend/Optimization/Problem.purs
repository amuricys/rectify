module Backend.Optimization.Problem where

import Backend.Optimization.Problem.Reservoir as Reservoir
import Backend.Optimization.Problem.Surface as Surface
import Backend.Optimization.Problem.TSP as TSP
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Generic.Rep (class Generic)

data Problem
  = Surface Surface.SolutionData
  | TSP TSP.SolutionData
  | Reservoir Reservoir.SolutionData

derive instance genericProblem :: Generic Problem _

instance decodeJsonProblem :: DecodeJson Problem where
  decodeJson = genericDecodeJson