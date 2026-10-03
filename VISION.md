# Rectify project vision

Rectify is a **visual math workbench**: a place to define mathematical objects, run experiments on them, and understand what happens by watching and interacting with the process. It is also a home base for exploring functional programming, types, proofs, and different execution models through substantial mathematical problems.

The visual experience matters in its own right. The workbench should feel inviting, tactile, and aesthetically coherent. Watching an optimization unfold or wiring two oscillators together is part of the intellectual exploration, not just a way to display a final answer.

This document records project intent. It distinguishes existing foundations from research directions; it does not commit every idea to an implementation or a release. See [README.md](README.md) for the current application and [AGENTS.md](AGENTS.md) for development guidance.

## Several mathematical workbenches

The project has relatively independent areas of exploration that can share an interface and useful infrastructure without being forced into one mathematical abstraction.

**Optimization** was the original motivation: choose a problem, choose an algorithm, and watch the search evolve. Traveling-salesman tours, deforming surfaces, and reservoir topologies give the search concrete objects to act on.

**Compositional dynamical systems** are the current focus. Applied category theory motivates expressing systems with inputs and outputs and composing them through wiring. The Julia implementation uses AlgebraicDynamics machines, with custom equations and interactive composition. Exploring these ideas is valuable independently of the optimization workbench.

There are possible connections: an optimizer might search parameters or wiring for a dynamical system. Those are future experiments, not a reason to make either workbench depend on the other today.

## Problems and algorithms are separate choices

The intended optimization interface has two axes:

| Axis | Directions |
| --- | --- |
| Problems | Traveling-salesman tours; surface free-energy minimization, initially in 2D; reservoir-computer topology search; further experiments including neural-network training. |
| Algorithms | Simulated annealing; genetic algorithms; particle swarm optimization; other methods as useful. |

Simulated annealing and TSP have implementations today. The Haskell tree contains the surface experiment. The other entries describe directions, not a complete set of implemented combinations.

Conceptually, a problem describes candidate solutions, their meaning, validity conditions, and an objective. An algorithm describes how to explore those candidates. The bridge supplies the operations a particular algorithm needs: initialization and neighbor proposals for annealing, for example, or mutation and crossover for a genetic algorithm.

Independence does not mean every algorithm works on every representation without additional design. A surface does not automatically have a meaningful crossover operation, nor does a tour automatically have the position and velocity operations a swarm method might expect. Make those requirements explicit rather than quietly changing the problem to fit an algorithm.

The code already contains two useful starting points:

- [The Haskell `Problem` abstraction](runtimes/haskell/native/src/SimulatedAnnealing.hs) bundles initialization, neighbor generation, fitness, schedule, and acceptance. It is generic over effects, metrics, temperature-like values, and solutions, but mixes problem and annealing concerns.
- [The Lean optimization framework](runtimes/lean/src/Rectify/Optimization.lean) separates a solution's fitness from [the annealing adapter and parameters](runtimes/lean/src/Rectify/Optimization/SA.lean).

Use these as evidence from previous experiments. Neither is a final universal interface.

## Users should be able to describe the experiment

A major direction is moving beyond selecting hardcoded examples and adjusting sliders. A user should eventually be able to describe things such as:

- What constitutes a solution, including its structure and constraints.
- How to construct an initial candidate and evaluate its objective.
- How to propose a neighbor or supply another algorithm-specific operation.
- How an algorithm is configured, including its temperature schedule where applicable.
- How to visualize the candidate and inspect the run.

The motivating example is a surface: describe its representation, define a deformation, express an energy, and watch a search. A neighbor may be a pure transformation, or a proposal with explicit randomness; the description should preserve that distinction.

The ambition is a Deimos-like authoring experience, in the owner's terms: descriptions supplied through the frontend become executable experiments. The exact reference, language, syntax, and compilation strategy remain to be specified. This is not yet a commitment to natural-language compilation or to arbitrary host-language execution.

Julia's existing custom-equation editor is an early instance of user-authored mathematics. It does not yet provide user-defined optimization problems, representations, or algorithm adapters.

A possible design to investigate is a typed description or intermediate representation with validation, execution, and visualization adapters. Before choosing it, work through a concrete surface example: what can the user express, which invariants can be checked, how are errors explained, and what must a backend implement? Keep reproducible definitions, parameters, and random seeds in view so an experiment can be revisited.

## The surface problem is a central research question

The surface experiment is a major motivation for the project, even though it is absent from the current frontend. Its physical formulation is still open.

The existing [Haskell surface representation](runtimes/haskell/native/src/SimulatedAnnealing/Surface/Surface2D.hs) holds inner and outer closed polygonal boundaries in sized vectors. [Neighbor proposals](runtimes/haskell/native/src/SimulatedAnnealing/Surface/Problem.hs) move an outer point, spread the displacement, influence the inner boundary, and adjust sampling by removing or adding points. Intersection checks reject some proposed changes. The objective is an experimental free-energy expression involving enclosed areas and deformation.

That implementation is a starting point, not an established physical model. Even its annealing behavior needs care when revisited: the surface acceptance function currently accepts only energy improvements, with the temperature-dependent expression commented out.

The questions to resolve include:

- Which forces and energy terms should the model represent, and which quantities should be conserved?
- How should the boundaries interact, and what does valid thickness or containment mean?
- Which edits preserve closure, orientation, and freedom from self-intersection?
- What representation makes a 3D version tractable? Adding a coordinate to a polygon does not define a closed surface mesh.
- Would a different physical or geometric formulation make the intended experiment clearer?

Types can express useful structural facts. The current Haskell work explores bounded circular indices and changes to vector lengths while preserving nonemptiness. Those properties do not themselves establish that a polygon is simple, that boundaries remain nested, or that the modeled physics is appropriate.

Lean is a candidate for stating and proving geometric invariants and preservation properties. A proof effort should name the representation, assumptions, operations, and arithmetic model it covers. A theorem about exact geometry does not automatically certify a floating-point implementation or a generated executable.

## Functional programming is part of the purpose

Rectify is deliberately a place to explore different programming technologies through real problems. A useful experiment may justify a separate implementation even when consolidating languages would simplify deployment.

| Technology | Role or direction |
| --- | --- |
| Haskell | Existing optimization and surface experiments; exploration of effects, sized data, and type-level invariants. |
| Julia and AlgebraicDynamics | Existing runtime system definitions and composition; current applied-category-theory exploration. |
| Lean | Existing optimization and oscillator code; further work on dependent types, specifications, and proofs. |
| THC | Experimental Haskell execution path through Truffle/Graal; a small geometry compatibility probe is scaffolded separately from the native server. |
| Clash | Existing reservoir hardware experiments; a route to investigate realizing selected topologies in FPGA hardware. |
| Unison | A distributed-computation axis: proposed optimizer islands, ensembles, and neural-network training. No backend is implemented yet. |
| Bend | Proposed surface-processing experiment motivated by runtime-managed parallel evaluation. Suitability and performance remain to be evaluated. |

These are research roles, not permanent ownership boundaries or promises that every language will support every workbench.

### Parallel execution and proofs

The Bend idea comes from wanting parallelism to be a property of execution, without having to express thread creation throughout the mathematical code. A possible experiment is to partition space when looking for intersections among surface segments and evaluate independent candidate checks in parallel.

The desired separation is between expressing transformations and their invariants, and choosing how independent work executes. It remains necessary to establish what is independent, handle partition boundaries correctly, and measure the cost of partitioning and execution. Runtime parallelism, useful speedup, and preservation of semantics are hypotheses to test for the chosen implementation.

This motivation is not a claim that Lean cannot run concurrent computations or that Bend replaces a proof system. If specifications or proofs live in one language and execution in another, the connection between them is an additional design and validation problem. No verified translation pipeline exists here today.

### Distributed experiments in Unison

Unison should contribute an independent way of expressing computation. Distributed neural-network training was the original candidate, not a fixed assignment or the whole purpose of this axis.

A proposed first experiment is a visual archipelago of optimizers: local searches exchange candidates over a configurable graph, while the workbench shows convergence, diversity, and migration. Distribution becomes part of the object under study. Other candidates include distributed ensembles of chaotic systems and discrete-time simulations with explicit communication delays. See [the Unison design entry point](runtimes/unison/README.md).

Start with explicit timing and reproducibility semantics. A synchronous experiment and an asynchronous one can have different mathematical behavior even with identical random seeds. These proposals do not turn networked computations into the same composition abstraction used by the Julia machines.

### Reservoir search and hardware

The intended reservoir-computing experiment searches for a useful topology, then realizes a selected reservoir as hardware whose topology is fixed during a run. This connects optimization to the Clash experiments and the Terranix infrastructure for AWS FPGA exploration.

The repository contains reservoir computations and infrastructure sketches, but not a complete topology-search-to-FPGA workflow. Candidate encoding, fitness evaluation, simulation, hardware generation, and validation of the generated design remain distinct pieces to connect. A fixed deployed topology does not mean the reservoir's state stops evolving.

## What finishing means

The research directions can remain open while individual workbenches become complete enough to use. Finishing a selected scope means a user can define or select an experiment, run and inspect it, understand failures, and reproduce it without editing incidental backend plumbing.

A proposed standard for each usable slice is: a concrete example, explicit mathematical semantics, appropriate correctness checks, a coherent visual interaction, and documented ways to run and revisit it. Saving experiment definitions and run configuration is part of that direction; it is not implemented as a general workspace feature today.

There is no fixed ordering here between improving compositional systems, restoring the surface workbench, generalizing problem authoring, or trying a new runtime. Choose the next concrete experiment with the owner. Complete its usable path while keeping the other directions legible and available.

## Decisions still open

- The next workbench to bring to a usable completion point.
- The surface model, its 3D representation, and the invariants worth proving first.
- The user description language and its relationship to backend types and code generation.
- The compatibility contract between problems, algorithm adapters, and renderers.
- The boundary between verified specifications and execution in another runtime.
- Which Bend or Unison experiment would establish something useful with limited scope.

Record decisions as they are made, including the question answered and the evidence behind the choice. Preserve unresolved questions as questions.
