# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Rectify is an interactive visualizer for dynamical systems and optimization algorithms. It connects a Svelte frontend to mathematical backends in Julia and Lean 4.

**Architecture**: Frontend (Svelte/Three.js) communicates via WebSocket JSON protocol with:
- Julia backend (port 8082): Runtime-composable open dynamical systems using AlgebraicDynamics.jl/Catlab.jl
- Lean backend (port 8081): Type-verified systems and optimization (TSP, simulated annealing)

## Build & Run Commands

### Development Environment (Nix recommended)
```bash
nix develop              # Full environment
nix develop .#frontend   # Just Node/npm
nix develop .#lean       # Just Lean toolchain
nix develop .#julia      # Just Julia
```

### Frontend (Svelte 5)
```bash
cd rectify-frontend
npm install
npm run dev              # Dev server at http://localhost:5173
npm run build            # Production build
npm run check            # Svelte type checking
```

### Julia Backend
```bash
cd rectify-julia
julia --project=. -e 'using Pkg; Pkg.instantiate()'  # First time setup
julia --project=. run.jl                              # Start server at ws://8082
```

### Lean Backend
```bash
cd rectify-lean
lake build               # Build all
lake exe rectify         # Run server at ws://8081
```

## Key Architecture Concepts

### Open vs Closed Systems
- **Closed system**: Self-contained ODE (e.g., standard Lorenz attractor)
- **Open system**: Has typed input/output ports, can be composed with other systems via wiring diagrams

### Wiring and Composition
When wiring changes in Julia backend:
1. The `UndirectedWiringDiagram` (Catlab) is rebuilt
2. Systems are recomposed via `AlgebraicDynamics.oapply`
3. Solver is reconstructed with new composed system
4. This takes 100-500ms; simulation pauses during rewiring

### WebSocket Protocol
Frontend → Backend: `AddSystem`, `RemoveSystem`, `Wire`, `Unwire`, `SetParams`, `Control`
Backend → Frontend: `WorldState`, `StateUpdate` (60 Hz), `Templates`, `Error`, `Ack`

## Key Files

### Frontend
- `rectify-frontend/src/routes/+page.svelte` - Main app with tab switching
- `rectify-frontend/src/lib/stores/algebraic.svelte.ts` - Julia WebSocket state (Svelte 5 runes)
- `rectify-frontend/src/lib/stores/optimization.svelte.ts` - Optimization state
- `rectify-frontend/src/lib/components/AlgebraicRenderer.svelte` - Three.js 3D visualization

### Julia Backend
- `rectify-julia/run.jl` - Entry point
- `rectify-julia/src/Systems.jl` - System template registry (Lorenz, Rossler, VanDerPol, etc.)
- `rectify-julia/src/Server.jl` - WebSocket server and message dispatch
- `rectify-julia/src/Composition.jl` - Wiring diagram composition via Catlab
- `rectify-julia/src/Simulation.jl` - Integration (Euler, RK4)

### Lean Backend
- `rectify-lean/src/Rectify.lean` - Server state machine
- `rectify-lean/src/Rectify/Dynamics/` - System implementations
- `rectify-lean/src/Rectify/Optimization/` - SA, TSP algorithms

## Adding New System Templates (Julia)

Add to `SYSTEM_REGISTRY` in `rectify-julia/src/Systems.jl`:
```julia
function my_system_machine(; param=1.0)
    ContinuousMachine{Float64}(
        ninputs, nstates, noutputs,
        (u, x, p, t) -> [...],  # dynamics: dx/dt
        (x, p) -> x             # readout: outputs
    )
end
```

## Tech Stack Notes

- **Frontend state**: Svelte 5 runes (`$state`, `$derived`, `$effect`)
- **3D rendering**: Three.js with rolling 2000-point trajectory buffers
- **Julia deps**: AlgebraicDynamics.jl, Catlab.jl, HTTP.jl (WebSockets)
- **Lean deps**: SciLean for verified ODE integration, FFI to libwebsockets via C

## Legacy Components

`rectify-backend/` (Haskell) and `rectify-clash/` (FPGA) are under maintenance and not actively used.
