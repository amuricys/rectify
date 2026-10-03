import Lean

namespace Rectify.WebSocket

-- FFI declarations matching the C implementation
@[extern "lean_ws_init"]
opaque init (port : UInt16) : IO Unit

@[extern "lean_ws_broadcast"]
opaque broadcast (msg : @& String) : IO Unit

@[extern "lean_ws_service"]
opaque service (timeoutMs : UInt32) : IO Unit

@[extern "lean_ws_receive_nonblocking"]
opaque receiveNonBlocking : IO (Option String)

@[extern "lean_ws_destroy"]
opaque destroy : IO Unit

end Rectify.WebSocket
