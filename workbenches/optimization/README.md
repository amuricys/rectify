# Optimization workbench

Problem definitions, algorithm transitions, pairing adapters, and execution are separate concerns. This index links the mathematical experiments to their implementations.

| Problem | Existing foundation | Next questions |
| --- | --- | --- |
| TSP | [Haskell](../../runtimes/haskell/native/src/SimulatedAnnealing/TSP/Problem.hs), [Lean](../../runtimes/lean/src/Rectify/Optimization/TSP.lean) | Agree distance conventions, neighbor semantics, and reproducible examples. |
| Surface free energy | [Haskell surface](../../runtimes/haskell/native/src/SimulatedAnnealing/Surface/Problem.hs) | Physical model, valid deformations, intersections, containment, 3D representation. |
| Reservoir topology | [Clash reservoir](../../hardware/clash/src/Reservoir/Project.hs) | Candidate encoding and fitness; connect search to simulation and hardware. |

Implemented search foundations: [Haskell generic step](../../runtimes/haskell/native/src/SimulatedAnnealing.hs) and [Lean annealing](../../runtimes/lean/src/Rectify/Optimization/SA.lean). The surface currently uses greedy acceptance; both TSP neighbors swap cities. Do not infer an implementation of genetic algorithms or PSO from the vision document.

Execution experiments include [THC kernels](../../runtimes/haskell/thc/README.md) and [Unison optimizer islands](../../runtimes/unison/README.md). A distributed search method introduces migration semantics in addition to the local optimizer.
