-- Rectify.Optimization.SA
-- Simulated Annealing algorithm

import Rectify.Optimization
import Lean.Data.Json

namespace Rectify.Optimization.SA

open Rectify.Optimization

-- ============================================================================
-- SA Adapter: bridges a problem to SA
-- ============================================================================

/-- What SA needs to know about a problem -/
structure Adapter (Solution : Type) where
  /-- Generate a random initial solution -/
  initial : RandomM Solution
  /-- Fitness function (lower is better) -/
  fitness : Solution → Float
  /-- Generate a neighbor by perturbation -/
  neighbor : Solution → RandomM Solution

-- ============================================================================
-- SA Parameters
-- ============================================================================

structure Params where
  /-- Starting temperature -/
  initialTemp : Float := 1000.0
  /-- Minimum temperature -/
  finalTemp : Float := 0.1
  /-- Multiplicative cooling: temp *= coolingRate each step -/
  coolingRate : Float := 0.9995
  deriving Repr

-- ============================================================================
-- SA State
-- ============================================================================

structure State (Solution : Type) where
  /-- Current solution -/
  current : Solution
  /-- Current fitness -/
  currentFitness : Float
  /-- Best solution found so far -/
  best : Solution
  /-- Best fitness found so far -/
  bestFitness : Float
  /-- Current temperature -/
  temperature : Float
  /-- Step counter -/
  stepCount : Nat
  deriving Repr

-- ============================================================================
-- SA Algorithm
-- ============================================================================

def temperature (params : Params) (step : Nat) : Float :=
  let temp := params.initialTemp * (params.coolingRate ^ step.toFloat)
  if temp < params.finalTemp then params.finalTemp else temp

def acceptanceProbability (fitCurrent fitCandidate temp : Float) : Float :=
  if fitCandidate < fitCurrent then 1.0
  else if temp ≤ 0.0 then 0.0
  else Float.exp ((fitCurrent - fitCandidate) / temp)

/-- Initialize SA state -/
def init (adapter : Adapter Solution) (params : Params) : RandomM (State Solution) := do
  let sol ← adapter.initial
  let fit := adapter.fitness sol
  pure {
    current := sol
    currentFitness := fit
    best := sol
    bestFitness := fit
    temperature := params.initialTemp
    stepCount := 0
  }

/-- Perform one SA step -/
def step (adapter : Adapter Solution) (params : Params) (state : State Solution)
    : RandomM (State Solution) := do
  -- Generate neighbor
  let candidate ← adapter.neighbor state.current
  let fitCandidate := adapter.fitness candidate

  -- Compute acceptance probability
  let temp := temperature params state.stepCount
  let prob := acceptanceProbability state.currentFitness fitCandidate temp

  -- Accept or reject
  let coin ← randFloat
  let (newCurrent, newFit) :=
    if coin < prob then (candidate, fitCandidate)
    else (state.current, state.currentFitness)

  -- Update best
  let (newBest, newBestFit) :=
    if newFit < state.bestFitness then (newCurrent, newFit)
    else (state.best, state.bestFitness)

  pure {
    current := newCurrent
    currentFitness := newFit
    best := newBest
    bestFitness := newBestFit
    temperature := temp
    stepCount := state.stepCount + 1
  }

/-- Run n steps -/
def runSteps (adapter : Adapter Solution) (params : Params)
    (state : State Solution) (n : Nat) : RandomM (State Solution) := do
  let mut s := state
  for _ in [:n] do
    s ← step adapter params s
  pure s

-- ============================================================================
-- JSON serialization
-- ============================================================================

instance [Lean.ToJson Solution] : Lean.ToJson (State Solution) where
  toJson s := Lean.Json.mkObj [
    ("current", Lean.toJson s.current),
    ("currentFitness", Lean.toJson s.currentFitness),
    ("best", Lean.toJson s.best),
    ("bestFitness", Lean.toJson s.bestFitness),
    ("temperature", Lean.toJson s.temperature),
    ("stepCount", s.stepCount),
    ("algorithm", "SimulatedAnnealing")
  ]

end Rectify.Optimization.SA
