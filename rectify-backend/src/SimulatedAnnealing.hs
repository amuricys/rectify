{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module SimulatedAnnealing where

import Data.Aeson (ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Data.Bifunctor (first)
import Debug.Pretty.Simple (pTrace, pTraceShow)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (evalState)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Helpers (tap)
import Random
import Prelude

-- Starting from here: https://oleg.fi/gists/posts/2020-06-02-simulated-annealing.html
newtype Probability = Probability {unProbability :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional)

data Algorithm = Surface | TSP | Reservoir
  deriving (Eq, Show, Generic)

data Problem es metric beta solution = Problem
  { initial :: solution,
    neighbor :: solution -> Eff es solution,
    fitness :: solution -> metric,
    schedule :: Integer -> beta, -- temperature schedule; list of betas represented as fn
    acceptance ::
      solution ->
      solution ->
      metric ->
      metric ->
      beta ->
      Eff es Probability
  }

data SimState metric solution = SimState
  { currentSolution :: solution,
    currentFitness :: metric,
    currentBeta :: Integer
  }
  deriving (Show, Generic)

data SAEff :: Effect where
  Step :: SimState metric solution -> SAEff m (SimState metric solution)

makeEffect ''SAEff

instance (Eq metric, Eq solution) => Eq (SimState metric solution) where
  (==) a b = currentSolution a == currentSolution b && currentFitness a == currentFitness b && currentBeta a == currentBeta b

instance (ToJSON metric, ToJSON solution) => ToJSON (SimState metric solution) where
  -- Exclude gen from serialization
  toJSON SimState {currentSolution, currentFitness, currentBeta} =
    Object $
      fromList
        [ ("currentSolution", toJSON currentSolution),
          ("currentFitness", toJSON currentFitness),
          ("currentBeta", toJSON currentBeta)
        ]

problemToInitialSimState :: Problem es metric beta solution -> SimState metric solution
problemToInitialSimState problem =
  let is = problem.initial
      f = problem.fitness is
   in SimState {currentSolution = is, currentFitness = f, currentBeta = 0}

runSAPure ::
  (Show metric) =>
  (Show beta) =>
  (RandomEff :> es) =>
  Problem es metric beta solution ->
  Eff (SAEff : es) (SimState metric solution) ->
  Eff es (SimState metric solution)
runSAPure p@(Problem {neighbor, fitness, schedule, acceptance}) =
  let s0 = problemToInitialSimState p
   in reinterpret (evalState s0) $ \_ -> \case
        Step s -> do
          coinFlip <- Probability <$> nextDouble
          nghb <- neighbor s.currentSolution
          let nghbFitness = fitness nghb
          accept <- acceptance s.currentSolution nghb s.currentFitness nghbFitness (schedule s.currentBeta)
          let newBeta = currentBeta s + 1
          pure $
            if coinFlip <= accept
              then SimState {currentSolution = nghb, currentFitness = nghbFitness, currentBeta = newBeta}
              else s {currentBeta = newBeta}
