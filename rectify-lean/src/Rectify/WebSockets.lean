import Lean

namespace Rectify.WebSocket.FFI

-- FFI declarations matching the C implementation
@[extern "lean_ws_init"]
opaque init (port : UInt16) : IO Unit

@[extern "lean_ws_broadcast"]
opaque broadcast (msg : @& String) : IO Unit

@[extern "lean_ws_service"]
opaque service (timeoutMs : UInt32) : IO Unit

@[extern "lean_ws_destroy"]
opaque destroy : IO Unit

end Rectify.WebSocket.FFI

namespace Rectify.WebSocket

-- JSON encoding for trajectory data
structure TrajectoryPoint where
  time : Float
  x : Float
  y : Float
  z : Option Float := none
  deriving Repr

def trajectoryToJson (points : Array TrajectoryPoint) : String :=
  let jsonPoints := points.map fun p =>
    s!"\{\"time\": {p.time}, \"x\": {p.x}, \"y\": {p.y}" ++
    (match p.z with
     | some z => s!", \"z\": {z}}"
     | none => "}")
  "{\"trajectory\": [" ++ String.intercalate ", " jsonPoints.toList ++ "]}"

end Rectify.WebSocket
