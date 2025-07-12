module Backend.Optimization.Algorithm where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Generic.Rep (class Generic)

data AlgorithmData 
  = SimulatedAnnealing {
      beta :: Number,
      betaCounter :: Int,
      fitness :: Number
    }
  | GeneticAlgorithm {
      populationSize :: Int,
      mutationRate :: Number,
      crossoverRate :: Number,
      fitness :: Number
    }

derive instance genericAlgorithmData :: Generic AlgorithmData _

instance decodeJsonAlgorithmData :: DecodeJson AlgorithmData where
  decodeJson = genericDecodeJson