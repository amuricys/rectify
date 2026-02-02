<!--
  AlgebraicControls.svelte

  Control panel for the AlgebraicDynamics (Julia) backend.
  Supports adding/removing systems and wiring them together.
-->
<script lang="ts">
	import { algebraic } from '$lib/stores/algebraic.svelte';

	type SystemKind = 'lorenz' | 'harmonic' | 'vanderpol' | 'duffing';

	const systemKinds: { id: SystemKind; label: string }[] = [
		{ id: 'lorenz', label: 'Lorenz' },
		{ id: 'harmonic', label: 'Harmonic' },
		{ id: 'vanderpol', label: 'Van der Pol' },
		{ id: 'duffing', label: 'Duffing' }
	];

	let newSystemKind = $state<SystemKind>('lorenz');
	let systemCounter = $state(1);

	function addSystem() {
		const id = `${newSystemKind}_${systemCounter++}`;
		algebraic.addSystem(id, newSystemKind);
	}

	// Wiring state
	let wireFrom = $state<string>('');
	let wireFromPort = $state(1);
	let wireTo = $state<string>('');
	let wireToPort = $state(1);

	function createWire() {
		if (wireFrom && wireTo) {
			algebraic.wire(wireFrom, wireFromPort, wireTo, wireToPort);
		}
	}

	// Derive system list from world state
	const systemIds = $derived(
		algebraic.world ? Object.keys(algebraic.world.systems) : []
	);
</script>

<div class="controls">
	<div class="control-group">
		<span class="label">Connection</span>
		<div class="status" class:connected={algebraic.status === 'connected'}>
			{algebraic.status}
		</div>
		{#if algebraic.status === 'disconnected' || algebraic.status === 'error'}
			<button onclick={() => algebraic.connect()}>Connect</button>
		{:else if algebraic.status === 'connected'}
			<button onclick={() => algebraic.disconnect()}>Disconnect</button>
		{/if}
	</div>

	<div class="control-group">
		<span class="label">Playback</span>
		<div class="button-row">
			<button onclick={() => algebraic.play()}>Play</button>
			<button onclick={() => algebraic.pause()}>Pause</button>
			<button onclick={() => algebraic.step()}>Step</button>
			<button onclick={() => algebraic.reset()}>Reset</button>
		</div>
	</div>

	<div class="control-group">
		<span class="label">Add System</span>
		<select bind:value={newSystemKind}>
			{#each systemKinds as kind}
				<option value={kind.id}>{kind.label}</option>
			{/each}
		</select>
		<button onclick={addSystem}>Add</button>
	</div>

	{#if systemIds.length > 0}
		<div class="control-group">
			<span class="label">Systems</span>
			<div class="system-list">
				{#each systemIds as id}
					{@const sys = algebraic.world?.systems[id]}
					<div class="system-item">
						<span class="system-id">{id}</span>
						<span class="system-kind">{sys?.kind}</span>
						<button class="small" onclick={() => algebraic.removeSystem(id)}>×</button>
					</div>
				{/each}
			</div>
		</div>

		<div class="control-group">
			<span class="label">Wire Systems</span>
			<div class="wire-row">
				<select bind:value={wireFrom}>
					<option value="">From...</option>
					{#each systemIds as id}
						<option value={id}>{id}</option>
					{/each}
				</select>
				<input type="number" bind:value={wireFromPort} min="1" max="3" class="port-input" />
			</div>
			<div class="wire-row">
				<select bind:value={wireTo}>
					<option value="">To...</option>
					{#each systemIds as id}
						<option value={id}>{id}</option>
					{/each}
				</select>
				<input type="number" bind:value={wireToPort} min="1" max="3" class="port-input" />
			</div>
			<button onclick={createWire}>Wire</button>
		</div>

		{#if algebraic.world && algebraic.world.wires.length > 0}
			<div class="control-group">
				<span class="label">Wires</span>
				<div class="wire-list">
					{#each algebraic.world.wires as wire}
						<div class="wire-item">
							{wire.from_system}:{wire.from_port} → {wire.to_system}:{wire.to_port}
						</div>
					{/each}
				</div>
			</div>
		{/if}
	{/if}

	{#if algebraic.world}
		<div class="control-group">
			<span class="label">Time</span>
			<code>{algebraic.world.t.toFixed(3)}</code>
		</div>
	{/if}
</div>

<style>
	.controls {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		padding: 1rem;
		background: var(--bg-panel);
		border: 1px solid var(--border);
		min-width: 220px;
		max-height: 100%;
		overflow-y: auto;
	}

	.control-group {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.label {
		font-size: 0.75rem;
		text-transform: uppercase;
		letter-spacing: 0.1em;
		color: var(--text-dim);
	}

	.status {
		font-size: 0.875rem;
		color: var(--text-dim);
	}

	.status.connected {
		color: #6aaf8c;
	}

	.button-row {
		display: flex;
		gap: 0.25rem;
		flex-wrap: wrap;
	}

	select, input {
		background: var(--bg-dark);
		border: 1px solid var(--border);
		color: var(--text);
		padding: 0.4rem;
		font-family: inherit;
		font-size: 0.875rem;
	}

	select:focus, input:focus {
		outline: none;
		border-color: var(--accent);
	}

	.system-list {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
	}

	.system-item {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		padding: 0.25rem 0.5rem;
		background: var(--bg-dark);
		border: 1px solid var(--border);
		font-size: 0.75rem;
	}

	.system-id {
		flex: 1;
		color: var(--accent);
	}

	.system-kind {
		color: var(--text-dim);
	}

	button.small {
		padding: 0.1rem 0.4rem;
		font-size: 0.75rem;
	}

	.wire-row {
		display: flex;
		gap: 0.25rem;
	}

	.wire-row select {
		flex: 1;
	}

	.port-input {
		width: 3rem;
	}

	.wire-list {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
	}

	.wire-item {
		font-size: 0.75rem;
		color: var(--text-dim);
		padding: 0.25rem;
		background: var(--bg-dark);
	}

	code {
		font-size: 0.875rem;
		color: var(--accent);
	}
</style>
