# Repository structure and implementation axes

Rectify separates the experiment a user wants to perform from the language, compiler, and execution model used to implement it. This layout prepares those choices without requiring one common runtime or a universal mathematical API.

## Layout

```text
apps/web/                   Svelte workbench and visualizations
runtimes/
  haskell/
    native/                 Existing server, surface, and TSP implementation
    kernels/                Small dependency-light comparison kernels
    thc/                    Independent Cabal project and THC compatibility probe
  julia/                    Existing compositional dynamics implementation
  lean/                     Existing optimization and oscillator work
  unison/                   Distributed-computation experiment design
  bend/                     Parallel geometry experiment design
hardware/clash/             Existing reservoir hardware project
workbenches/                Mathematical concepts and implementation entry points
contracts/                  Existing protocols and proposed future boundaries
experiments/                Versioned experiment descriptions, not a job scheduler
docs/                       Architecture decisions and development context
scripts/                    Local discovery, commands, and probes
infra/                      Infrastructure experiments
workspace.json              Project paths, status, tools, and explicit commands
```

Language-native packages remain intact. Files moved from the old project roots retain their package names and mathematical behavior. The existing Threlte work travels with the frontend. The small new geometry package is a compatibility baseline; the old surface implementation does not depend on it yet.

## Independent dimensions

| Dimension | Examples |
| --- | --- |
| Mathematical problem | Tour length, surface free energy, reservoir quality |
| Search or evolution rule | Annealing, future evolutionary methods, ODE integration |
| Representation and adapter | Tour permutation and swap proposal; polygon and deformation proposal |
| Implementation | Haskell, Julia, Lean, Unison, Bend |
| Execution model | Native evaluation, JIT specialization, distributed processes, parallel evaluation, hardware circuits |
| Observation | Tour, surface, phase portrait, convergence plot, migration network |

These dimensions have compatibility constraints. A language is not an algorithm, and hardware synthesis is not an interchangeable execution flag for arbitrary Haskell. Start with explicit supported combinations. Cross-language sharing initially means example inputs, semantics, and comparison results, not a claim that one compiler can consume all implementations.

## Haskell execution paths

Keep the existing GHC application, the THC experiment, and Clash in independent build plans. The root Cabal project selects the native application. THC's project selects only its probe and the small kernel library. Clash retains its own Cabal and Stack files. Compiler and dependency requirements can therefore evolve separately.

THC uses GHC's frontend and executes Core through Truffle/Graal; it does not simply run the GHC-produced native executable. See the [upstream architecture](https://github.com/ekmett/thc/blob/main/docs/architecture.md). Our proposed use is to compare repeated geometric computation with native GHC, then investigate specialization of repeated scoring or deformation operations. It is not yet a replacement runtime for the server.

Haskell source, a THC execution, and a Clash circuit can share mathematical intent without sharing every representation. Reservoir simulation versus synthesized fixed-point hardware needs explicit numerical and timing comparisons.

## Unison as distributed mathematics

Unison is an independent implementation axis, not merely an orchestration layer around other backends. Remote neural-network training is one candidate; a network of interacting optimizers is another. The first proposed visual experiment is described in [the Unison entry point](../runtimes/unison/README.md).

The browser observes experiment events; Unison owns its computations and communication semantics. The boundary can eventually be HTTP or WebSocket messages without exposing Unison's internal representation to the Svelte application. Existing backends need not be rewritten before this can be explored.

## Mathematical modules inside each runtime

As code is extracted, aim for problem representations/objectives, algorithm transitions, pairing-specific adapters, geometry, and execution/transport as separate modules. Keep proofs near the mathematical definitions they cover. This repository move does not rename the existing Haskell modules or alter their algorithms; those extractions need behavior checks of their own.

Do not create a generic adapter until at least one concrete pairing explains its operations. In particular, SA neighbor proposals, genetic crossover, and swarm motion have different requirements.

## Migration map

| Previous root | Current root |
| --- | --- |
| `rectify-frontend/` | `apps/web/` |
| `rectify-backend/` | `runtimes/haskell/native/` |
| `rectify-julia/` | `runtimes/julia/` |
| `rectify-lean/` | `runtimes/lean/` |
| `rectify-clash/` | `hardware/clash/` |

Nix paths, root Cabal selection, editor paths, and current documentation follow the new layout. Cached build products may retain old absolute paths and need rebuilding. The older ARCHITECTURE.md remains a historical proposal. Resource names in infrastructure and language package names are intentionally unchanged.

## First useful milestones

1. Compare the geometry probe under GHC and a pinned THC checkout, including unsupported dependencies and startup/warmup behavior.
2. Specify and implement a tiny Unison optimizer archipelago locally, first with a deterministic synchronous migration schedule.
3. Connect its events to an island-network visualization and then experiment with asynchronous migration.
4. Revisit the surface model and extract its geometry independently of a full server/runtime port.

These are proposed experiments, not a claim that THC, distributed search, or Bend execution already works in this checkout.
