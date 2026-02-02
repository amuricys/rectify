<!--
  Controls.svelte

  Control panel for the dynamics simulation.
  Connect/disconnect, play/pause, step, system selection.
-->
<script lang="ts">
	import { dynamics } from '$lib/stores/dynamics.svelte';

	type SystemName = 'HarmonicOscillator' | 'LorenzSystem' | 'DuffingOscillator' | 'VanDerPolOscillator';

	const systems: { id: SystemName; label: string }[] = [
		{ id: 'LorenzSystem', label: 'Lorenz' },
		{ id: 'HarmonicOscillator', label: 'Harmonic' },
		{ id: 'DuffingOscillator', label: 'Duffing' },
		{ id: 'VanDerPolOscillator', label: 'Van der Pol' }
	];

	let selectedSystem = $state<SystemName>('LorenzSystem');

	function handleSystemChange(system: SystemName) {
		selectedSystem = system;
		dynamics.selectSystem(system);
	}
</script>

<div class="controls">
	<div class="control-group">
		<span class="label">Connection</span>
		<div class="status" class:connected={dynamics.status === 'connected'}>
			{dynamics.status}
		</div>
		{#if dynamics.status === 'disconnected' || dynamics.status === 'error'}
			<button onclick={() => dynamics.connect()}>Connect</button>
		{:else if dynamics.status === 'connected'}
			<button onclick={() => dynamics.disconnect()}>Disconnect</button>
		{/if}
	</div>

	<div class="control-group">
		<span class="label">Playback</span>
		<button onclick={() => dynamics.unpause()}>Play</button>
		<button onclick={() => dynamics.pause()}>Pause</button>
		<button onclick={() => dynamics.step()}>Step</button>
	</div>

	<div class="control-group">
		<span class="label">System</span>
		{#each systems as sys}
			<button
				class:active={selectedSystem === sys.id}
				onclick={() => handleSystemChange(sys.id)}
			>
				{sys.label}
			</button>
		{/each}
	</div>

	{#if dynamics.currentState}
		<div class="control-group state-display">
			<span class="label">State</span>
			<code>
				x: {dynamics.currentState.x.toFixed(4)}<br>
				y: {dynamics.currentState.y.toFixed(4)}<br>
				z: {dynamics.currentState.z.toFixed(4)}
			</code>
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
		color: #6aaf8c;
	}

	.state-display code {
		font-size: 0.75rem;
		color: var(--accent);
		line-height: 1.5;
	}
</style>
