# Unison distributed experiments

Status: design entry point. No Unison backend, project publication, or Cloud deployment has been created. Neural-network training remains an option; Unison's role is broader exploration of distributed computation as mathematics the user can see.

## Proposed first experiment

Build an archipelago of optimizers. Each island owns a candidate or population, a local search rule, and a random stream. Islands exchange candidates along visible directed connections. The user can change migration intervals, communication topology, or local search settings and observe convergence and diversity.

Start with the same small TSP instance on every island and one local search method. Show each island's best tour, score history, and work count; animate migrations and record whether incoming candidates are adopted. Later, allow heterogeneous algorithms where the candidate representation and objective are compatible.

This makes location and communication part of the experiment. It relates optimization to compositional thinking without claiming that message-passing optimizers are the same mathematical object as the Julia ODE machines.

A first local version should use numbered rounds and deterministic migration/merge order. Assign explicit seeds per island. Then introduce asynchronous delivery as a separate experimental mode: a common seed alone does not reproduce a run whose result depends on message order. Record migration decisions and topology changes as events.

Pausing and stepping also need distributed semantics. Initially, a step can mean one global round with migration at a barrier. Avoid presenting independently advancing remote islands as if a browser click were an instantaneous global pause.

## Other experiments worth exploring

- An ensemble of chaotic systems, displayed as a constellation of trajectories, with distributed parameter sweeps and uncertainty summaries.
- Coupled discrete-time simulations with explicit communication delays, making latency and topology visible parts of their dynamics. Do not silently interpret network latency as faithful continuous-time coupling.
- Distributed neural-network training: partition data, calculate gradients, aggregate updates, checkpoint, and visualize communication and learning. Establish a small numerical reference before considering accelerators or large models.

The optimizer archipelago is the recommended first candidate because it connects directly to the existing workbench and produces an interpretable visual experiment with small computations. It is a proposal, not an owner-approved replacement for training research.

## Development and repository boundary

Unison stores definitions in a UCM-managed codebase rather than treating source files as the complete codebase. Git should hold reviewable source exports/transcripts, test inputs, experiment metadata, and pinned project/dependency references. Keep local UCM databases out of Git. See the [official tour](https://www.unison-lang.org/docs/tour/).

Start a private local workspace from this directory:

```bash
ucm --codebase-create .local/codebase
```

Inside UCM, create the initial project with `project.create rectify`. This is local setup, not publication. Once code exists, record the tested UCM version, project/branch, definition hashes, and dependency versions; no fictional remote reference is recorded here.

Use [UCM transcripts](https://www.unison-lang.org/docs/tooling/transcripts/) for repeatable examples. `ucm transcript path/to/example.md` runs against a fresh temporary codebase by default. Transcripts must explicitly establish the built-ins and dependencies they need. Add the first executable transcript with the actual island implementation.

Unison's `Remote` ability expresses distributed work; Cloud provides execution resources and service deployment. The official [local development guide](https://www.unison.cloud/docs/local-development/) documents local handlers, including `Cloud.main.local.serve` for interactive services. Validate locally before choosing deployment infrastructure.

The proposed external boundary is a service accepting an experiment and emitting run/island/migration events. Implement the first UI adapter when this service exists. Neither native Haskell values nor Unison code hashes should be treated as a cross-language serialization format.
