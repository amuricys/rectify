# rectify

A **visual math workbench** for exploring optimization, dynamical systems, and functional programming through interactive experiments.

Choose mathematical objects and operations, watch them evolve, and investigate how they behave. The aim is an expressive workbench with a cozy visual interface, where users can eventually describe their own problems and experiments.

## Project direction

Rectify began with visual optimization: select a **problem** independently from an **algorithm**. Problems of interest include traveling-salesman tours, surface free-energy minimization, and reservoir-computer topology search. Algorithm directions include simulated annealing, genetic algorithms, and particle swarm optimization.

Compositional dynamical systems form another workbench and are the current focus. Applied category theory motivates building larger systems by wiring smaller systems together. The project also serves as a home base for exploring Haskell, Julia, Lean, Clash, and potentially other languages and runtimes.

Read [VISION.md](VISION.md) for the full direction: user-defined experiments, the unresolved surface model and 3D extension, proofs, runtime parallelism, THC, and Unison distributed experiments. These ambitions extend beyond the implemented features below.

## Documentation

- [VISION.md](VISION.md): project intent, research directions, and open decisions.
- [docs/REPOSITORY.md](docs/REPOSITORY.md): layout, implementation axes, and migration map.
- [infra/README.md](infra/README.md): development shell, local services, and cloud commands.
- [AGENTS.md](AGENTS.md): shared guidance for agents working in the repository.
- [CLAUDE.md](CLAUDE.md): entry point directing Claude to the same guidance.
- [ARCHITECTURE.md](ARCHITECTURE.md): earlier open-systems design proposal; some details differ from current code.

## Current application

The Svelte 5 frontend has two active tabs:

| Workbench | Backend | Implemented foundations |
| --- | --- | --- |
| Open Systems | Julia, WebSocket on port 8082 | Built-in attractors and oscillators, custom equations, runtime wiring, parameter editing, integration, trajectory and time-series views. |
| Optimization | Lean 4, WebSocket on port 8081 | Traveling-salesman problem over 30 Finnish cities, simulated annealing, play/pause/step/reset, and tour visualization. |

The open-systems renderer is being migrated from a large imperative Three.js component to Threlte components in `apps/web/src/lib/threlte/`. Treat this as work in progress, including its interactions.

Julia represents systems as AlgebraicDynamics `ContinuousMachine`s. The current composition implementation manually routes signals between machines inside a combined machine; it does not construct a Catlab wiring diagram and call `oapply`. Integration uses explicit Euler or RK4 routines. The frontend stores a rolling history for visualization.

The active Lean server runs optimization. Separate oscillator modules use SciLean, but they are not the active server path. Their presence should not be read as a claim that all numerical or geometric behavior has been formally verified.

## Repository map

| Directory | Purpose |
| --- | --- |
| `apps/web/` | SvelteKit frontend, Three.js/Threlte rendering, controls, and WebSocket stores. |
| `runtimes/julia/` | Open-system templates, custom equations, world state, composition, integration, and WebSocket server. |
| `runtimes/lean/` | Active TSP/annealing server, optimization abstractions, separate dynamical-system modules, and C WebSocket bindings. |
| `runtimes/haskell/native/` | Earlier Haskell optimization server and surface experiments, including sized boundaries and circular indexing. |
| `runtimes/haskell/kernels/` | Dependency-light geometry baseline for compiler comparisons. |
| `runtimes/haskell/thc/` | Separate THC compatibility probe; native GHC reference runner. |
| `runtimes/unison/` | Distributed experiments, including proposed optimizer islands and neural-network training. |
| `runtimes/bend/` | Proposed parallel geometry experiment. |
| `workbenches/` | Mathematical entry points across implementations. |
| `contracts/`, `experiments/` | Interface documentation and experiment descriptions. |
| `hardware/clash/` | FPGA reservoir-computing experiments in Clash. |
| `infra/` | Terranix/Nix infrastructure experiments, including AWS FPGA provisioning sketches. |
| `flake.nix` | Full development shell, optional focused shells, and Terranix configuration outputs. |

The Haskell and Clash work is outside the current frontend path, but remains part of the project's research direction. There are no Bend or Unison implementations yet, and no general user-defined optimization language or completed topology-search-to-FPGA pipeline.

## Workspace commands

Run these from the repository root:

```bash
python3 scripts/workspace.py list
python3 scripts/workspace.py doctor
python3 scripts/workspace.py check
python3 scripts/workspace.py run web dev
python3 scripts/workspace.py run web build
python3 scripts/haskell_probe.py ghc
```

The workspace helper dispatches explicit local commands. It does not install toolchains or deploy services. Planned runtimes have no runnable actions. See [THC setup](runtimes/haskell/thc/README.md) and [Unison direction](runtimes/unison/README.md).

## Development

Nix development shells are defined for the full environment and individual parts:

```bash
nix develop
# Or choose one:
nix develop .#frontend
nix develop .#lean
nix develop .#julia
```

The default shell includes Node, Julia, Lean via Elan, native Haskell, Clash, Unison UCM, Bend/HVM, THC build tools, Terranix, Terraform, and the AWS CLI. THC wrappers select a separate compiler and require a source checkout. Language dependencies and the pinned Lean toolchain are fetched on first use. Equivalent local toolchains can also be used.

After installing the project dependencies described below, `rectify up` starts web, Julia, and Lean together and stops them together. Use `rectify up web julia` to select services. Focused shells remain available, including `.#deploy`, `.#thc`, `.#unison`, and `.#bend`. See [infrastructure commands](infra/README.md) for cloud setup.

Start the frontend and the backend for the desired tab in separate terminals.

### Frontend

```bash
cd apps/web
npm install
npm run dev
# http://localhost:5173
```

### Julia open systems

```bash
cd runtimes/julia
julia --project=. -e 'using Pkg; Pkg.instantiate()'
julia --project=. run.jl
# ws://localhost:8082
```

### Lean optimization

```bash
cd runtimes/lean
lake build
lake exe rectify
# ws://localhost:8081
```

The Lean build requires its configured toolchain and native libwebsockets dependencies. Backend addresses default to localhost. Set `VITE_JULIA_WS_URL` and `VITE_LEAN_WS_URL` before building to change them; see `apps/web/.env.example`. HTTPS deployments require secure WebSocket endpoints.

## Validation

`python3 scripts/test_tooling.py` checks infrastructure command dispatch and local service cleanup. `nix flake check` checks rendered infrastructure invariants. These checks do not provision cloud resources or establish runtime compatibility.

`npm run build` in `apps/web/` produces the static frontend. Its package manifest currently has no `check` or `test` script. A successful build does not validate browser interactions or backend mathematics.

The native Haskell tree contains old tests whose Cabal stanza is currently disabled and whose imports need updating. Clash has its own test target; the new geometry probe has an independent runner. The active frontend and Julia paths do not yet have a general automated suite. Verify the affected behavior explicitly when making changes, and distinguish successful builds from runtime checks and mathematical guarantees.

## License

MIT
