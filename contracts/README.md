# Workbench interfaces

The existing protocols remain unchanged by the repository restructuring:

| Integration | Commands | Observations |
| --- | --- | --- |
| Julia on port 8082 | JSON: AddSystem, RemoveSystem, Wire, Unwire, SetParams, SetState, Control, DefineCustomSystem | JSON: Templates, WorldState, StateUpdate, Ack, Error |
| Lean on port 8081 | Text: Play, Pause, Step, Reset with seed | JSON: annealing state including current/best tours and fitness |

See [Julia dispatch](../runtimes/julia/src/Server.jl), [its client](../apps/web/src/lib/stores/algebraic.svelte.ts), and [Lean dispatch](../runtimes/lean/src/Rectify.lean). These are the implemented contracts. This directory does not introduce a replacement protocol that the servers already understand.

For a future Unison service, define explicit run identity, sequence/round, node identity, work count, objective values, migration events, and checkpoint references. Keep algorithm-specific and visualization-specific payloads separate from the common run envelope. Start with the island experiment and implement both sides before generalizing.

Portable experiment descriptions should reference implementations and versions. They are not executable Haskell Core, Unison terms, or proof certificates. Proof and numerical-validity claims need their own precise boundaries.
