<!--
  OptimizationControls.svelte

  Control panel for the Lean optimization backend.
-->
<script lang="ts">
	import { optimization } from '$lib/stores/optimization.svelte';

	// Derived stats
	const stats = $derived(optimization.state ? {
		currentFitness: optimization.state.currentFitness.toFixed(1),
		bestFitness: optimization.state.bestFitness.toFixed(1),
		temperature: optimization.state.temperature.toFixed(1),
		stepCount: optimization.state.stepCount,
		improvement: ((1 - optimization.state.bestFitness / optimization.state.currentFitness) * 100).toFixed(1)
	} : null);
</script>

<div class="controls">
	<div class="control-group">
		<span class="label">Connection</span>
		<div class="status" class:connected={optimization.status === 'connected'}>
			{optimization.status}
		</div>
		{#if optimization.status === 'disconnected' || optimization.status === 'error'}
			<button onclick={() => optimization.connect()}>Connect</button>
		{:else if optimization.status === 'connected'}
			<button onclick={() => optimization.disconnect()}>Disconnect</button>
		{/if}
	</div>

	<div class="control-group">
		<span class="label">Playback</span>
		<div class="button-row">
			<button class="toggle" class:running={optimization.running} onclick={() => optimization.toggle()}>
				{optimization.running ? 'Pause' : 'Play'}
			</button>
			<button onclick={() => optimization.step()}>Step</button>
		</div>
	</div>

	<div class="control-group">
		<span class="label">Seed</span>
		<div class="seed-row">
			<input
				type="number"
				value={optimization.seed}
				onchange={(e) => optimization.setSeed(parseInt(e.currentTarget.value) || 42)}
			/>
			<button onclick={() => optimization.reset()}>Reset</button>
		</div>
	</div>

	{#if stats}
		<div class="control-group">
			<span class="label">TSP + Simulated Annealing</span>
			<div class="stats">
				<div class="stat-row">
					<span class="stat-label">Step</span>
					<span class="stat-value">{stats.stepCount}</span>
				</div>
				<div class="stat-row">
					<span class="stat-label">Temperature</span>
					<span class="stat-value temp">{stats.temperature}</span>
				</div>
				<div class="stat-row">
					<span class="stat-label">Current</span>
					<span class="stat-value">{stats.currentFitness} km</span>
				</div>
				<div class="stat-row highlight">
					<span class="stat-label">Best</span>
					<span class="stat-value best">{stats.bestFitness} km</span>
				</div>
			</div>
		</div>

		<div class="control-group">
			<span class="label">Algorithm</span>
			<div class="algo-name">{optimization.state?.algorithm || 'Unknown'}</div>
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
		min-width: 200px;
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
		color: #7a9b68;
	}

	.button-row {
		display: flex;
		gap: 0.25rem;
		flex-wrap: wrap;
	}

	.stats {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
		font-size: 0.875rem;
	}

	.stat-row {
		display: flex;
		justify-content: space-between;
		padding: 0.25rem 0;
		border-bottom: 1px solid var(--border);
	}

	.stat-row.highlight {
		background: rgba(122, 155, 104, 0.1);
		padding: 0.25rem;
		margin: 0 -0.25rem;
		border-radius: 2px;
	}

	.stat-label {
		color: var(--text-dim);
	}

	.stat-value {
		color: var(--text);
		font-family: monospace;
	}

	.stat-value.temp {
		color: #b85c4a;
	}

	.stat-value.best {
		color: #7a9b68;
		font-weight: bold;
	}

	.algo-name {
		font-size: 0.875rem;
		color: var(--accent);
	}

	.toggle {
		min-width: 4rem;
	}

	.toggle.running {
		background: rgba(122, 155, 104, 0.2);
		border-color: #7a9b68;
	}

	.seed-row {
		display: flex;
		gap: 0.25rem;
	}

	.seed-row input {
		flex: 1;
		background: var(--bg-dark);
		border: 1px solid var(--border);
		color: var(--text);
		padding: 0.4rem;
		font-family: inherit;
		font-size: 0.875rem;
		width: 5rem;
	}

	.seed-row input:focus {
		outline: none;
		border-color: var(--accent);
	}
</style>
