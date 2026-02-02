# rectify

A cozy visualizer for dynamical systems and optimization algorithms.

## Vision

Rectify is an interactive tool for exploring mathematical systems visually. The goal is to make abstract concepts tangible through real-time visualization with a relaxing, aesthetic interface.

**Dynamical Systems**: Visualize chaotic attractors, oscillators, and other continuous systems. Adjust parameters in real-time, scrub through time, and measure system properties.

**Optimization Algorithms**: Watch simulated annealing, genetic algorithms, TSP solvers, and eventually neural networks iterate toward solutions. See the process, not just the result.

**Open Systems & Composition**: Using ideas from applied category theory (via AlgebraicJulia), systems can be wired together—the output of one becoming the input of another. Build modular, composable dynamical systems.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                     rectify-frontend (Svelte)                   │
│                        http://localhost:5173                    │
│                                                                 │
│   ┌─────────────────┐              ┌─────────────────────────┐  │
│   │  ThreeJS        │              │  Controls               │  │
│   │  Renderer       │              │  - Play/Pause/Step      │  │
│   │                 │              │  - System selection     │  │
│   │  - Trajectories │              │  - Wiring (Julia)       │  │
│   │  - Multi-system │              │  - Parameters           │  │
│   └─────────────────┘              └─────────────────────────┘  │
└──────────────┬────────────────────────────────┬─────────────────┘
               │                                │
               │ WebSocket                      │ WebSocket
               │ ws://localhost:8081            │ ws://localhost:8082
               │                                │
┌──────────────▼──────────────┐  ┌──────────────▼──────────────────┐
│     rectify-lean (Lean 4)   │  │     rectify-julia (Julia)       │
│                             │  │                                 │
│  Single closed systems:     │  │  Open composable systems:       │
│  - Lorenz attractor         │  │  - Multiple systems in parallel │
│  - Harmonic oscillator      │  │  - Runtime wiring of I/O ports  │
│  - Duffing oscillator       │  │  - AlgebraicDynamics.jl         │
│  - Van der Pol oscillator   │  │  - Catlab.jl (category theory)  │
│                             │  │                                 │
│  Uses SciLean for verified  │  │  Dynamic system definition      │
│  ODE integration (RK4)      │  │  and composition at runtime     │
└─────────────────────────────┘  └─────────────────────────────────┘
```

## Tech Stack

| Layer | Technology | Purpose |
|-------|------------|---------|
| Frontend | Svelte 5 + SvelteKit | Reactive UI with modern runes API |
| 3D Rendering | Three.js | WebGL visualization of trajectories |
| Backend (verified) | Lean 4 + SciLean | Type-safe dynamical systems with compile-time guarantees |
| Backend (dynamic) | Julia + AlgebraicDynamics.jl | Runtime-composable open systems via category theory |
| Dev Environment | Nix | Reproducible builds across all components |

## Getting Started

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled

### Development

```bash
# Enter the development environment (includes Node, Lean, Julia)
nix develop

# Or enter specific sub-environments
nix develop .#frontend   # Just Node/npm
nix develop .#lean       # Just Lean toolchain
nix develop .#julia      # Just Julia
```

### Running

**Frontend** (Svelte):
```bash
cd rectify-frontend
npm install
npm run dev
# → http://localhost:5173
```

**Backend** (Lean — single closed systems):
```bash
cd rectify-lean
lake build
lake exe rectify
# → ws://localhost:8081
```

**Backend** (Julia — composable open systems):
```bash
cd rectify-julia
julia --project=. -e 'using Pkg; Pkg.instantiate()'
julia --project=. run.jl
# → ws://localhost:8082
```

## Project Structure

```
rectify/
├── flake.nix              # Nix flake for reproducible dev environment
├── rectify-frontend/      # Svelte 5 + SvelteKit + Three.js
│   ├── src/
│   │   ├── lib/
│   │   │   ├── components/
│   │   │   │   ├── Renderer.svelte        # Agnostic canvas container
│   │   │   │   ├── LorenzRenderer.svelte  # ThreeJS for Lean backend
│   │   │   │   ├── AlgebraicRenderer.svelte # ThreeJS for Julia backend
│   │   │   │   ├── Controls.svelte        # Lean backend controls
│   │   │   │   └── AlgebraicControls.svelte # Julia backend controls
│   │   │   └── stores/
│   │   │       ├── dynamics.svelte.ts     # Lean WebSocket state
│   │   │       └── algebraic.svelte.ts    # Julia WebSocket state
│   │   └── routes/
│   │       └── +page.svelte               # Main application
│   └── package.json
├── rectify-lean/          # Lean 4 dynamical systems server
│   ├── lakefile.lean
│   └── src/
│       ├── Rectify.lean                   # Main server + state machine
│       ├── Rectify/
│       │   ├── Dynamics/
│       │   │   ├── Lorenz.lean
│       │   │   ├── HarmonicOscillator.lean
│       │   │   ├── DuffingOscillator.lean
│       │   │   └── VanDerPolOscillator.lean
│       │   └── WebSockets.lean            # FFI bindings
│       └── Rectify/WebSockets.c           # libwebsockets integration
├── rectify-julia/         # Julia AlgebraicDynamics server
│   ├── Project.toml
│   ├── run.jl
│   └── src/
│       ├── Systems.jl                     # Open system definitions
│       └── Server.jl                      # WebSocket server + world state
├── rectify-backend/       # (Legacy) Haskell optimization algorithms
└── rectify-clash/         # (Legacy) FPGA reservoir computing
```

## Concepts

### Open vs Closed Systems

A **closed system** evolves according to its own equations with no external input:
```
dx/dt = σ(y - x)
dy/dt = x(ρ - z) - y     ← Lorenz system (closed)
dz/dt = xy - βz
```

An **open system** has typed input and output ports:
```
         ┌─────────────┐
  ρ_ext ─┤             ├─ x
         │   Lorenz    ├─ y
         │             ├─ z
         └─────────────┘
```

Open systems can be **composed** by wiring outputs to inputs:
```
┌──────────┐         ┌──────────┐
│ Harmonic │─ pos ──▶│  Lorenz  │
│ Oscillator│        │  (ρ_ext) │
└──────────┘         └──────────┘
```

This composition is formalized using **operads** from category theory, ensuring that compositions are mathematically well-typed.

### Why Two Backends?

**Lean** provides compile-time verification. The differential equations are part of the type system; SciLean generates correct-by-construction integrators. This is ideal for "blessed" systems where correctness matters.

**Julia** provides runtime flexibility. Users can define arbitrary equations, compose systems dynamically, and experiment freely. AlgebraicDynamics.jl handles the categorical machinery for safe composition.

The long-term vision is to bridge these: verify composition rules in Lean, execute in Julia, with a certified protocol between them.

## License

MIT
