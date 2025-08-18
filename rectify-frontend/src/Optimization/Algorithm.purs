module Optimization.Algorithm where

import Prelude

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Sum as CAS
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

type SimulatedAnnealingData = {
  beta :: Number,
  betaCounter :: Int,
  fitness :: Number
}

simulatedAnnealingDataCodec :: CA.JsonCodec SimulatedAnnealingData
simulatedAnnealingDataCodec = CA.object "SimulatedAnnealingData" (CAR.record {
  "beta": CA.number,
  "betaCounter": CA.int,
  "fitness": CA.number
})

type GeneticAlgorithmData = {
  populationSize :: Int,
  mutationRate :: Number,
  crossoverRate :: Number,
  fitness :: Number
}

geneticAlgorithmDataCodec :: CA.JsonCodec GeneticAlgorithmData
geneticAlgorithmDataCodec = CA.object "GeneticAlgorithmData" (CAR.record {
  "populationSize": CA.int,
  "mutationRate": CA.number,
  "crossoverRate": CA.number,
  "fitness": CA.number
})

data AlgorithmData 
  = SimulatedAnnealing SimulatedAnnealingData
  | GeneticAlgorithm GeneticAlgorithmData

data AlgorithmTags = SimulatedAnnealingTag | GeneticAlgorithmTag

algorithmDataCodec :: CA.JsonCodec AlgorithmData
algorithmDataCodec = CAS.taggedSum "AlgorithmData"
  (case _ of
    SimulatedAnnealingTag -> "simulatedAnnealing"
    GeneticAlgorithmTag -> "geneticAlgorithm"
  )
  (case _ of
    "simulatedAnnealing" -> Just SimulatedAnnealingTag
    "geneticAlgorithm" -> Just GeneticAlgorithmTag
    _ -> Nothing
  )
  (case _ of
    SimulatedAnnealingTag -> Right (map SimulatedAnnealing <<< CA.decode simulatedAnnealingDataCodec)
    GeneticAlgorithmTag -> Right (map GeneticAlgorithm <<< CA.decode geneticAlgorithmDataCodec)
  )
  (case _ of
    SimulatedAnnealing d -> Tuple SimulatedAnnealingTag (Just (CA.encode simulatedAnnealingDataCodec d))
    GeneticAlgorithm d -> Tuple GeneticAlgorithmTag (Just (CA.encode geneticAlgorithmDataCodec d))
  )
  
