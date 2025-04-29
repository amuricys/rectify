{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}

module SimulatedAnnealing where

import Data.Aeson (ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Effectful (Eff, type (:>))
import Effectful.State.Dynamic (State, get)
import Effectful.State.Static.Local (evalState)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Helpers (tap)
import Random (RandomEff, nextDouble)
import Prelude

-- Starting from here: https://oleg.fi/gists/posts/2020-06-02-simulated-annealing.html
newtype Probability = Probability {unProbability :: Double}
  deriving newtype (Show, Eq, Num, Ord, Fractional)

data Algorithm = Surface | TSP | Reservoir
  deriving (Eq, Show, Generic)

data Problem m metric beta solution = Problem
  { initial :: m solution,
    neighbor :: solution -> m solution,
    fitness :: solution -> metric,
    schedule :: Integer -> beta, -- temperature schedule; list of betas represented as fn
    acceptance ::
      solution ->
      solution ->
      metric ->
      metric ->
      beta ->
      m Probability
  }

data SimState metric solution = SimState
  { currentSolution :: solution,
    currentFitness :: metric,
    currentBeta :: Integer
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

problemToInitialSimState :: (Monad m) => Problem m metric beta solution -> m (SimState metric solution)
problemToInitialSimState problem = do
  is <- problem.initial
  let f = problem.fitness is
  pure SimState {currentSolution = is, currentFitness = f, currentBeta = 0}

step ::
  (RandomEff :> es) =>
  Problem (Eff es) metric beta solution ->
  SimState metric solution ->
  Eff es (SimState metric solution)
step Problem {neighbor, fitness, schedule, acceptance} s = do
  -- 1) propose a neighbor
  nghb <- neighbor (currentSolution s)
  let fNghb = fitness nghb

  -- 2) compute acceptance probability
  prob <-
    acceptance
      (currentSolution s)
      nghb
      (currentFitness s)
      fNghb
      (schedule (currentBeta s))

  -- 3) flip the coin
  coin <- Probability <$> nextDouble

  -- 4) bump beta and decide
  let newBeta = currentBeta s + 1
  pure $
    if coin <= prob
      then SimState nghb fNghb newBeta
      else s {currentBeta = newBeta}
