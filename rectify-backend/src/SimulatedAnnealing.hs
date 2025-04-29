{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SimulatedAnnealing where

import Data.Aeson (ToJSON (toJSON), Value (Object))
import Data.Aeson.KeyMap (fromList)
import Effectful (Eff, type (:>))
import Effectful.State.Dynamic (State, get)
import Effectful.State.Static.Local (evalState)
import Effectful.TH (makeEffect)
import GHC.Generics (Generic)
import Random (RandomEff, nextDouble)
import Prelude
import Debug.Pretty.Simple (pTraceShow)

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

data SimState metric solution beta = SimState
  { solution :: solution,
    fitness :: metric,
    betaCounter :: Integer, -- Need both beta and counter to send to frontend
    beta :: beta
  }
  deriving (Eq, Show, Generic)

instance (ToJSON metric, ToJSON solution, ToJSON beta) => ToJSON (SimState metric solution beta) where
  -- Exclude gen from serialization
  toJSON SimState {solution, fitness, beta, betaCounter} =
    Object $
      fromList
        [ ("solution", toJSON solution),
          ("fitness", toJSON fitness),
          ("beta", toJSON beta),
          ("betaCounter", toJSON betaCounter)
        ]

problemToInitialSimState :: (Monad m) => Problem m metric beta solution -> m (SimState metric solution beta)
problemToInitialSimState problem = do
  is <- problem.initial
  let f = problem.fitness is
  pure
    SimState
      { solution = is,
        fitness = f,
        betaCounter = 0,
        beta = problem.schedule 0
      }

step ::
  RandomEff :> es =>
  Problem (Eff es) metric beta solution ->
  SimState metric solution beta ->
  Eff es (SimState metric solution beta)
step Problem {neighbor, fitness, schedule, acceptance} s = do
  -- 1) propose a neighbor
  nghb <- neighbor s.solution
  let fNghb = fitness nghb

  -- 2) compute acceptance probability
  prob <-
    acceptance
      s.solution
      nghb
      s.fitness
      fNghb
      (schedule s.betaCounter)

  -- 3) flip the coin
  coin <- Probability <$> nextDouble
  pure $
    if coin <= prob
      then SimState nghb fNghb newBetaCounter (schedule newBetaCounter)
      else s {betaCounter = newBetaCounter, beta = schedule newBetaCounter}
      where newBetaCounter = betaCounter s + 1
