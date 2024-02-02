{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

module SimulatedAnnealing where

import Control.Monad.Trans.Accum
import RIO
import System.Random.SplitMix
import System.Random (Random)
import Data.Aeson (ToJSON (toJSON), Value(Object))
import Data.Aeson.KeyMap (fromList)

-- Starting from here: https://oleg.fi/gists/posts/2020-06-02-simulated-annealing.html
newtype Probability = Probability {unProbability :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional, Random)

data Problem metric beta solution = Problem
  { initial :: SMGen -> solution,
    neighbor :: SMGen -> solution -> solution,
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
instance (ToJSON metric, ToJSON solution) => ToJSON (SimState metric solution) where
  -- Exclude gen from serialization
  toJSON SimState {currentSolution, currentFitness, currentBeta} = Object $ fromList
    [ ("currentSolution", toJSON currentSolution)
    , ("currentFitness", toJSON currentFitness)
    , ("currentBeta", toJSON currentBeta)
    ]

step ::
  Problem metric beta solution ->
  SimState metric solution ->
  SimState metric solution
step (Problem {initial, neighbor, fitness, schedule, acceptance}) s =
  let (coinFlip, nextGen) = first Probability $ nextDouble (gen s)
      -- The same generator is used for both the neighbor and the acceptance probability
      nghb = neighbor nextGen (currentSolution s)
      nghbFitness = fitness nghb
      accept = acceptance (currentSolution s) nghb (currentFitness s) nghbFitness (schedule $ currentBeta s)
      newBeta = currentBeta s + 1
   in if coinFlip <= accept
        then SimState {currentSolution = nghb, currentFitness = nghbFitness, currentBeta = newBeta, gen = nextGen}
        else s {currentBeta = newBeta, gen = nextGen}
