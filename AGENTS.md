# Development guidance

## Read first

Read [docs/REPOSITORY.md](docs/REPOSITORY.md) for the workspace layout, then [VISION.md](VISION.md) for project intent and [README.md](README.md) for the current implementation and commands. Inspect the relevant source before treating a design document or code comment as an implemented guarantee. [ARCHITECTURE.md](ARCHITECTURE.md) is an earlier proposal focused on open systems and includes unimplemented details.

Rectify is a visual math workbench and a home base for functional-programming exploration. Optimization and compositional dynamical systems are relatively independent areas. Julia is the current focus, but the Haskell surface and Clash reservoir work retain research value.

## Preserve the design intent

- Keep optimization problems distinct from algorithms. State the operations an algorithm needs from a problem; do not assume every pairing is automatically meaningful.
- User-authored problem definitions and algorithm operations are a direction to develop. The custom-equation editor is not already a general optimization language.
- Treat the surface energy and geometric model as an experiment. Do not silently replace its objective or validity rules to make a solver convenient.
- Distinguish bounded indices, nonempty structures, geometric validity, physical validity, and formal proofs. Each is a different claim.
- Multiple languages are intentional. Do not consolidate the project or delete an inactive implementation merely because it is outside the current UI.
- Unison is an independent distributed-computation axis; neural-network training is one candidate, not its sole role. Optimizer islands are another proposed experiment.
- THC has a compatibility scaffold; Bend and Unison remain candidate experiments. Do not document them as installed backends, promise automatic speedups, or imply a verified cross-language pipeline exists.
- Preserve the visual and interactive quality of the workbench alongside mathematical correctness.

These principles guide implementation within the user's requested scope; they do not turn every research direction into an assigned task.

## Find the relevant code

| Area | Starting points |
| --- | --- |
| App and active tabs | `apps/web/src/routes/+page.svelte` |
| Julia connection and client history | `apps/web/src/lib/stores/algebraic.svelte.ts` |
| Open-system rendering | `apps/web/src/lib/threlte/` in the current renderer migration |
| Optimization client | `apps/web/src/lib/stores/optimization.svelte.ts`, `apps/web/src/lib/components/TSPRenderer.svelte` |
| Julia entry and protocol | `runtimes/julia/run.jl`, `runtimes/julia/src/Server.jl` |
| System definitions and custom equations | `runtimes/julia/src/Systems.jl`, `runtimes/julia/src/CustomSystems.jl` |
| World, composition, integration | `runtimes/julia/src/World.jl`, `runtimes/julia/src/Composition.jl`, `runtimes/julia/src/Simulation.jl` |
| Lean optimization server | `runtimes/lean/src/Rectify.lean`, `runtimes/lean/src/Rectify/Optimization/` |
| Haskell optimization abstraction | `runtimes/haskell/native/src/SimulatedAnnealing.hs` |
| Surface model and geometry | `runtimes/haskell/native/src/SimulatedAnnealing/Surface/`, `runtimes/haskell/native/src/Util/Index.hs`, `runtimes/haskell/native/src/Util/LinAlg.hs` |
| THC and native comparison | `runtimes/haskell/thc/`, `runtimes/haskell/kernels/`, `scripts/haskell_probe.py` |
| Distributed experiments | `runtimes/unison/README.md` |
| Workspace discovery | `workspace.json`, `scripts/workspace.py` |
| Reservoir hardware | `hardware/clash/src/Reservoir/` |
| Development and infrastructure | `flake.nix`, `infra/` |

## Work from the actual state

Start with `git status` and preserve existing work. In particular, inspect the current renderer migration before assuming the old `AlgebraicRenderer.svelte` still exists. Follow the imports used by the current route.

The active frontend connects to Julia on port 8082 and Lean on port 8081. Julia uses JSON commands and updates. Lean currently receives text commands such as `Play`, `Pause`, `Step`, and `Reset <seed>` and returns JSON optimization state. These are separate protocols.

Julia currently composes machines through manual signal routing; it does not call `oapply` in the composition implementation. Trace input defaults, state-replacement semantics, port numbering, and integration behavior across both ends when changing wiring. Code comments can describe intended behavior that is not fully implemented.

The active Lean server runs TSP with simulated annealing. Oscillator modules exist separately. Using Lean or SciLean alone does not establish a claimed proof; inspect the theorem, assumptions, and any admitted obligations.

## Validation and handoff

Use the commands in README.md and package/build manifests. The frontend currently has `dev`, `build`, and `preview` scripts, but no `check` or `test` script. A successful production build does not establish correct types throughout the app, browser interactions, or backend behavior.

For mathematical changes, verify the property being changed with an appropriate example, numerical comparison, property test, or proof. For protocol changes, check both the client and server. For renderer changes, exercise the affected interactions where tooling permits. Report environmental failures separately from code failures and untested behavior.

The native Haskell test stanza is disabled and its test sources have stale imports; do not report it as a working suite. Clash has an independent test target. The new geometry probe runs separately under native GHC or THC. There is no general automated frontend/Julia suite at present. Add focused regression coverage when implementing substantive fixes rather than inventing a passing test command.

Keep changes scoped to the requested experiment. Update relevant docs when behavior or an architectural decision changes. In handoffs, state what changed, what was validated, what remains uncertain, and the next concrete step. Clearly distinguish owner intent, proposed designs, implemented behavior, and measured results.

Infrastructure files are experiment scaffolding, not evidence of an active deployment. Reading or editing them does not by itself authorize provisioning cloud resources.

## Runtime independence

The root Cabal project includes only the native Haskell server. Clash and the THC probe use separate project files; do not force them onto one compiler version. The default Nix shell intentionally includes all runtime and deployment toolchains. Keep compiler-specific requirements scoped: native/Clash use GHC 9.8, while the THC wrappers select their own GHC. Tool availability does not establish backend compatibility. Preserve native package names when moving directories.

Use `python3 scripts/workspace.py list` for registered commands and statuses. A scaffold is not a functioning backend. Keep Unison codebase databases local; version reviewable transcripts/exports and explicit project/dependency references instead. Preserve the distinction between a definition in Git and a definition actually loaded into UCM.
