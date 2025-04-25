{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}

module SimulatedAnnealing where

import Prelude

import Data.Aeson (ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import System.Random (Random)
import System.Random.SplitMix (SMGen, nextDouble)
import Helpers (tap)
import Debug.Pretty.Simple (pTrace, pTraceShow)

-- Starting from here: https://oleg.fi/gists/posts/2020-06-02-simulated-annealing.html
newtype Probability = Probability {unProbability :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, Random)

data Algorithm = Surface | TSP | Reservoir
  deriving (Eq, Show, Generic)

data Problem metric beta solution = Problem
  { initial :: SMGen -> solution,
    neighbor :: SMGen -> solution -> (SMGen, solution),
    fitness :: solution -> metric,
    schedule :: Integer -> beta, -- temperature schedule; list of betas represented as fn
    acceptance ::
      solution ->
      solution ->
      metric ->
      metric ->
      beta ->
      Probability
  }

data SimState metric solution = SimState
  { currentSolution :: solution,
    currentFitness :: metric,
    currentBeta :: Integer,
    gen :: SMGen
  }
  deriving (Show, Generic)
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

problemToInitialSimState :: Problem metric beta solution -> SMGen -> SimState metric solution
problemToInitialSimState problem seed = let
  is = problem.initial seed
  f = problem.fitness is
  in SimState {currentSolution = is, currentFitness = f, currentBeta = 0, gen = seed}

step :: Show metric => Show beta =>
  Problem metric beta solution ->
  SimState metric solution ->
  SimState metric solution
step (Problem {initial, neighbor, fitness, schedule, acceptance}) s =
  let (coinFlip, nextGen) = first Probability $ nextDouble (gen s)
      -- The same generator is used for both the neighbor and the acceptance probability
      (nextGen', nghb) = neighbor nextGen (currentSolution s)
      nghbFitness = fitness nghb
      accept = acceptance s.currentSolution nghb s.currentFitness nghbFitness (schedule s.currentBeta)
      newBeta = currentBeta s + 1
   in if coinFlip <= accept
        then SimState {currentSolution = nghb, currentFitness = nghbFitness, currentBeta = newBeta, gen = nextGen'}
        else s {currentBeta = newBeta, gen = nextGen'}
