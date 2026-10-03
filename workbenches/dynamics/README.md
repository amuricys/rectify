# Compositional dynamics workbench

The active path is [the Julia server](../../runtimes/julia/src/Server.jl), its [machine templates](../../runtimes/julia/src/Systems.jl), [composition](../../runtimes/julia/src/Composition.jl), and [integration](../../runtimes/julia/src/Simulation.jl), observed by [the Svelte frontend](../../apps/web/src/routes/+page.svelte).

The current implementation routes machine outputs to inputs manually. Port conventions, unwired defaults, state replacement, integration steps, and feedback semantics need explicit verification. Applied category theory motivates the exploration; formal guarantees must be tied to actual definitions and proofs.

Future distributed ensembles or delayed discrete-time networks can live in Unison, but their timing and communication semantics must be specified independently of synchronous ODE composition. Share observations and example definitions where meaningful, rather than assuming one universal composition operation.
