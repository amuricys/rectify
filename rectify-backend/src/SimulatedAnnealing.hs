{-# LANGUAGE BlockArguments #-}

module SimulatedAnnealing where

import Control.Monad.Trans.Accum
import RIO
import System.Random.SplitMix

-- Starting from here: https://oleg.fi/gists/posts/2020-06-02-simulated-annealing.html
type Probability = Double

data Problem metric beta solution = Problem
  { initial :: SMGen -> solution,
    neighbor :: SMGen -> solution -> solution,
    fitness :: solution -> metric,
    schedule :: [beta], -- temperature schedule
    acceptance ::
      solution ->
      solution ->
      metric ->
      metric ->
      beta ->
      Probability
  }

data SimState metric beta solution = SimState
  { currentSolution :: solution,
    currentFitness :: metric,
    currentBeta :: beta,
    gen :: SMGen
  }

solve ::
  forall metric beta solution.
  Problem metric beta solution ->
  SMGen ->
  [solution]
solve (Problem {initial, neighbor, fitness, schedule, acceptance}) gen = case schedule of
  [] -> []
  (h : _) -> evalAccum (go schedule) [init]
    where
      init =
        SimState
          { currentSolution = initial gen,
            currentFitness = fitness (initial gen),
            currentBeta = h,
            gen = gen
          }
      go :: [beta] -> Accum [SimState metric beta solution] [solution]
      go [] = fmap (fmap currentSolution) look -- fmap into Accum, then into list of states
      go (beta : betas) = do
        latestState <-
          look >>= \case
            [] -> pure init
            (x : _) -> pure x
        let (coinFlip, nextGen) = nextDouble gen
            next = neighbor nextGen (currentSolution latestState)
            nextFitness = fitness next
            accept = acceptance (currentSolution latestState) next (currentFitness latestState) nextFitness beta
        add $ if accept >= 1.0 || accept >= coinFlip then [SimState {currentSolution = next, currentFitness = nextFitness, currentBeta = beta, gen = nextGen}] else [latestState {gen = nextGen}]
        go betas

