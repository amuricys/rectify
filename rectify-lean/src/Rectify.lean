import Rectify.Dynamics.HarmonicOscillator
import Rectify.WebSockets
import SciLean

namespace Rectify

open Rectify

def serverLoop : IO Unit := do
  let mut running := true

  -- Initial parameters
  let m := 1.0
  let k := 10.0
  let x₀ := 1.0
  let p₀ := 0.0

  IO.println "Computing initial trajectory..."
  let trajectory := Dynamics.harmonicTrajectory m k x₀ p₀ 0.1 100
  -- For now, let's just create a simple JSON string
  let json := s!"\"system\": \"harmonic\", \"message\": \"Hello from Lean!\", \"points\": {trajectory.size}}"

  IO.println "Broadcasting initial message..."
  WebSocket.FFI.broadcast json

  -- Service loop - run for a limited time for testing
  let mut iterations := 0
  while running && iterations < 100 do  -- Run for ~5 seconds then stop
    -- Service WebSocket with 50ms timeout
    WebSocket.FFI.service 50
    iterations := iterations + 1

    -- Log progress
    if iterations % 20 = 0 then
      IO.println s!"Serviced {iterations} times..."
      -- Broadcast again
      WebSocket.FFI.broadcast s!"\"iteration\": {iterations}"

def main : IO Unit := do
  IO.println "Starting Lean WebSocket server on port 8081..."

  try
    WebSocket.FFI.init 8081
    serverLoop
  catch e =>
    IO.eprintln s!"Error: {e}"

  WebSocket.FFI.destroy
  IO.println "Server shutdown complete"
