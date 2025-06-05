-- src/Rectify.lean
import Lean
import Rectify.Dynamics
import Rectify.WebSockets
import Lean.Data.Json

namespace Rectify

open Lean

def π : Float := 3.14159265358979323846

inductive DynamicalSystem where
  | harmonicOscillator
  | doublePendulum
  | lorenzSystem
  | duffingOscillator
  | vanDerPolOscillator
  deriving instance Repr, DecidableEq for DynamicalSystem

-- State management
inductive ServerState where
  | paused
  | running
  | stepping
  | changingSystem (system : DynamicalSystem)
  deriving instance Repr, DecidableEq for ServerState

structure RunnerState where
  currentSystem : DynamicalSystem
  harmonicOscillator : HarmonicOscillatorState
  doublePendulum : DoublePendulumState
  lorenzSystem : LorenzState
  duffingOscillator : DuffingState
  vanDerPolOscillator : VanDerPolState
  deriving Repr

structure RunnerParams where
  harmonicOscillator : HarmonicOscillatorParams
  doublePendulum : DoublePendulumParams
  lorenzSystem : LorenzParams
  duffingOscillator : DuffingParams
  vanDerPolOscillator : VanDerPolParams
  deriving Repr

def defaultParams : RunnerParams :=
  { harmonicOscillator := { m := 1.0, k := 1.0 },
    doublePendulum := { m := 1.0, l := 1.0 },
    lorenzSystem := { σ := 10.0, ρ := 28.0, β := 8.0 / 3.0 },
    duffingOscillator := { δ := 1.0, α := 1.0, β := 1.0, γ := 1.0, ω := 1.0 },
    vanDerPolOscillator := { μ := 1.0 } }

def initialRunnerState : RunnerState :=
  { currentSystem := .harmonicOscillator,
    harmonicOscillator := { x := 1.0, p := 0.0 },
    doublePendulum := { θ₁ := π / 4, θ₂ := π / 6, ω₁ := 0.0, ω₂ := 0.0 },
    lorenzSystem := { x := 1.0, y := 1.0, z := 1.0 },
    duffingOscillator := { x := 1.0, v := 0.0 },
    vanDerPolOscillator := { x := 1.0, y := 0.0 } }

-- JSON encoding for trajectories
structure TrajectoryPoint where
  x : Float
  y : Float
  deriving Repr, ToJson

-- Message handling
def handleMessage (state : IO.Ref ServerState) (msg : String) : IO Unit := do
  let s ← state.get
  match msg.trim with
  | "Unpause" =>
    if s == .paused then
      state.set .running
      IO.println "Unpausing"
  | "Pause" =>
    if s == .running then
      state.set .paused
      IO.println "Pausing"
  | "Step" =>
    if s == .paused then
      state.set .stepping
      IO.println "Stepping"
  | "HarmonicOscillator" =>
    state.set (.changingSystem .harmonicOscillator)
    IO.println "Switching to Harmonic Oscillator"
  | "DoublePendulum" =>
    state.set (.changingSystem .doublePendulum)
    IO.println "Switching to Double Pendulum"
  | "LorenzSystem" =>
    state.set (.changingSystem .lorenzSystem)
    IO.println "Switching to Lorenz System"
  | "DuffingOscillator" =>
    state.set (.changingSystem .duffingOscillator)
    IO.println "Switching to Duffing Oscillator"
  | "VanDerPolOscillator" =>
    state.set (.changingSystem .vanDerPolOscillator)
    IO.println "Switching to Van Der Pol Oscillator"
  | _ => IO.println s!"Unknown message: {msg}"

def stepRunner (params : RunnerParams) (runnerState : RunnerState) : RunnerState :=
  match runnerState.currentSystem with
  | .harmonicOscillator => { runnerState with harmonicOscillator := harmonicOscillatorSteps params.harmonicOscillator runnerState.harmonicOscillator 0.0 0.0 100 }
  | .doublePendulum => sorry -- { runnerState with doublePendulum := doublePendulumSteps params.doublePendulum runnerState.doublePendulum 0.0 0.0 100 }
  | .lorenzSystem => { runnerState with lorenzSystem := lorenzSteps params.lorenzSystem runnerState.lorenzSystem 0.0 0.0 100 }
  | .duffingOscillator => { runnerState with duffingOscillator := duffingSteps params.duffingOscillator runnerState.duffingOscillator 0.0 0.0 100 }
  | .vanDerPolOscillator => { runnerState with vanDerPolOscillator := vanDerPolSteps params.vanDerPolOscillator runnerState.vanDerPolOscillator 0.0 0.0 100 }

def runnerStateToJson (runnerState : RunnerState) : String :=
  match runnerState.currentSystem with
  | .harmonicOscillator => toString (toJson runnerState.harmonicOscillator)
  | .doublePendulum => toString (toJson runnerState.doublePendulum)
  | .lorenzSystem => toString (toJson runnerState.lorenzSystem)
  | .duffingOscillator => toString (toJson runnerState.duffingOscillator)
  | .vanDerPolOscillator => toString (toJson runnerState.vanDerPolOscillator)

def systemRunner (state : IO.Ref ServerState) : IO Unit := do
  let mut runnerState := initialRunnerState
  let mut params := defaultParams
  while true do
    let s ← state.get
    match s with
    | .running => do
        runnerState := stepRunner params runnerState
        WebSocket.broadcast (runnerStateToJson runnerState)
    | .stepping => do
        runnerState := stepRunner params runnerState
        WebSocket.broadcast (runnerStateToJson runnerState)
        state.set .paused
    | .changingSystem system => do
        runnerState := { runnerState with currentSystem := system }
        WebSocket.broadcast (runnerStateToJson runnerState)
        state.set .running
    | _ => pure ()

-- Main server
def serverLoop (state : IO.Ref ServerState) : IO Unit := do
  -- Start dynamical system tasks
  let _ ← IO.asTask (systemRunner state)

  -- Main loop handles WebSocket messages
  while true do
    -- Check for incoming messages
    match ← WebSocket.receiveNonBlocking with
    | some msg => handleMessage state msg
    | none => pure ()

    -- Service WebSocket
    WebSocket.service 10

def main : IO Unit := do
  IO.println "Starting Lean dynamical systems server on port 8081..."

  -- Initialize state
  let state ← IO.mkRef .paused
  try
    WebSocket.init 8081

    -- Send initial state
    WebSocket.broadcast (runnerStateToJson initialRunnerState)

    serverLoop state
  catch e =>
    IO.eprintln s!"Error: {e}"

  WebSocket.destroy
  IO.println "Server shutdown complete"
