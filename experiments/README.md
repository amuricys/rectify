# Experiment descriptions

The JSON files here are design records and comparison inputs, not commands accepted by the existing backend servers. There is no generic experiment launcher yet. Each description names a problem, algorithm where applicable, implementation choices, parameters, and status.

Keep problem choice, algorithm choice, implementation, and execution mode distinct. A future user-authoring interface can create the same kind of description once its language and validation rules are defined.

For a runnable experiment, additionally record immutable code/dependency references, datasets, seeds, numeric conventions, and result/checkpoint locations. Distributed experiments also need scheduling or event records for reproducibility. Do not check large outputs or local runtime databases into this directory.

The geometry probe is runnable through the commands recorded in its description. The island description remains a proposal. Runtime support is recorded explicitly; a JSON entry is not proof that a backend exists.
