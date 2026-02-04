-- src/Rectify.lean
-- Optimization server

import Lean
import Rectify.Optimization
import Rectify.Optimization.SA
import Rectify.Optimization.TSP
import Rectify.WebSockets
import Lean.Data.Json

namespace Rectify

open Lean
open Rectify.Optimization
open Rectify.Optimization.SA
open Rectify.Optimization.TSP

-- ============================================================================
-- Server state
-- ============================================================================

inductive RunState where
  | paused
  | running
  | stepping
  deriving Repr, DecidableEq

/-- Which problem × algorithm combination is active -/
inductive ActiveRunner where
  | tspSA
  -- | surfaceSA  -- TODO
  -- | tspGA      -- TODO
  deriving Repr, DecidableEq

structure ServerState where
  runState : RunState
  activeRunner : ActiveRunner
  /-- RNG state threaded through steps -/
  rng : StdGen
  /-- TSP + SA state -/
  tspSA : SA.State Tour
  deriving Repr

-- ============================================================================
-- Initialization
-- ============================================================================

def initServerState (seed : Nat := 42) : ServerState :=
  let rng := mkStdGen seed
  -- Initialize TSP+SA
  let (tspState, rng') := (SA.init TSP.saAdapter TSP.saParams).run rng
  {
    runState := .paused
    activeRunner := .tspSA
    rng := rng'
    tspSA := tspState
  }

-- ============================================================================
-- Stepping
-- ============================================================================

def stepServer (state : ServerState) : ServerState :=
  match state.activeRunner with
  | .tspSA =>
    let (newTspState, newRng) := (SA.step TSP.saAdapter TSP.saParams state.tspSA).run state.rng
    { state with tspSA := newTspState, rng := newRng }

-- ============================================================================
-- JSON output
-- ============================================================================

def serverStateToJson (state : ServerState) : String :=
  match state.activeRunner with
  | .tspSA => toString (toJson state.tspSA)

-- ============================================================================
-- Message handling
-- ============================================================================

def handleMessage (stateRef : IO.Ref ServerState) (msg : String) : IO Unit := do
  let state ← stateRef.get
  match msg.trim with
  | "Play" | "Unpause" =>
    if state.runState == .paused then
      stateRef.modify fun s => { s with runState := .running }
      IO.println "Running"
  | "Pause" =>
    if state.runState == .running then
      stateRef.modify fun s => { s with runState := .paused }
      IO.println "Paused"
  | "Step" =>
    if state.runState == .paused then
      stateRef.modify fun s => { s with runState := .stepping }
      IO.println "Stepping"
  | "TSP_SA" =>
    stateRef.modify fun s => { s with activeRunner := .tspSA }
    IO.println "Switched to TSP + Simulated Annealing"
  | cmd =>
    if cmd.startsWith "Reset" then
      let seedStr := cmd.drop 6 |>.trim
      let seed := seedStr.toNat?.getD 42
      let newState := initServerState seed
      stateRef.set newState
      WebSocket.broadcast (serverStateToJson newState)
      WebSocket.service 1
      IO.println s!"Reset with seed {seed}"
    else
      IO.println s!"Unknown message: {msg}"

-- ============================================================================
-- Runner loop
-- ============================================================================

def runnerLoop (stateRef : IO.Ref ServerState) : IO Unit := do
  while true do
    let state ← stateRef.get
    match state.runState with
    | .running =>
      let newState := stepServer state
      stateRef.set newState
      WebSocket.broadcast (serverStateToJson newState)
      WebSocket.service 1  -- Flush the broadcast
    | .stepping =>
      let newState := stepServer state
      stateRef.set { newState with runState := .paused }
      WebSocket.broadcast (serverStateToJson newState)
      WebSocket.service 1  -- Flush the broadcast
    | .paused =>
      IO.sleep 10  -- Don't spin when paused

-- ============================================================================
-- Main server
-- ============================================================================

def serverLoop (stateRef : IO.Ref ServerState) : IO Unit := do
  -- Start runner in background task
  let _ ← IO.asTask (runnerLoop stateRef)

  -- Main loop: handle messages
  while true do
    match ← WebSocket.receiveNonBlocking with
    | some msg => handleMessage stateRef msg
    | none => pure ()
    WebSocket.service 10

def main : IO Unit := do
  IO.println "Starting Rectify optimization server on port 8081..."

  let seed ← IO.rand 0 1000000
  let stateRef ← IO.mkRef (initServerState seed)

  try
    WebSocket.init 8081

    -- Send initial state
    let state ← stateRef.get
    WebSocket.broadcast (serverStateToJson state)

    serverLoop stateRef
  catch e =>
    IO.eprintln s!"Error: {e}"

  WebSocket.destroy
  IO.println "Server shutdown complete"

end Rectify
