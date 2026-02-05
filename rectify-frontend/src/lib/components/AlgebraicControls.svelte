<!--
  AlgebraicControls.svelte

  Control panel for the AlgebraicDynamics (Julia) backend.
  Supports adding/removing systems and wiring them together.
  Uses templates from server for available system types.
-->
<script lang="ts">
	import { algebraic } from '$lib/stores/algebraic.svelte';
	import CustomSystemEditor from './CustomSystemEditor.svelte';

	// Wiring state
	let wireFrom = $state<string>('');
	let wireFromPort = $state(1);
	let wireTo = $state<string>('');
	let wireToPort = $state(1);

	function createWire() {
		if (wireFrom && wireTo && wireFrom !== wireTo) {
			algebraic.wire(wireFrom, wireFromPort, wireTo, wireToPort);
			wireFrom = '';
			wireTo = '';
		}
	}

	// Get max ports for a system
	function getMaxOutputs(systemId: string): number {
		const sys = algebraic.systemList.find(s => s.id === systemId);
		return sys?.noutputs || 3;
	}

	function getMaxInputs(systemId: string): number {
		const sys = algebraic.systemList.find(s => s.id === systemId);
		return sys?.ninputs || 1;
	}
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
		{#if algebraic.error}
			<div class="error">{algebraic.error}</div>
		{/if}
	</div>

	<div class="control-group">
		<span class="label">Playback</span>
		<div class="button-row">
			<button onclick={() => algebraic.toggle()} class:active={algebraic.running}>
				{algebraic.running ? 'Pause' : 'Play'}
			</button>
			<button onclick={() => algebraic.step()}>Step</button>
			<button onclick={() => algebraic.reset()}>Reset</button>
		</div>
	</div>

	<div class="control-group">
		<span class="label">Speed</span>
		<div class="speed-control">
			<input
				type="range"
				min="0.1"
				max="5"
				step="0.1"
				bind:value={algebraic.speed}
				onchange={() => algebraic.setSpeed(algebraic.speed)}
			/>
			<span class="speed-value">{algebraic.speed.toFixed(1)}x</span>
		</div>
	</div>

	{#if algebraic.templateList.length > 0}
		<div class="control-group">
			<span class="label">Add System</span>
			<div class="template-grid">
				{#each algebraic.templateList as template}
					<button
						class="template-btn"
						onclick={() => algebraic.addSystem(template.id, { x: Math.random() * 100, y: Math.random() * 100 })}
						title={`${template.nstates} states, ${template.ninputs} in, ${template.noutputs} out`}
					>
						{template.name}
					</button>
				{/each}
			</div>
		</div>
	{/if}

	{#if algebraic.systemList.length > 0}
		<div class="control-group">
			<span class="label">Systems ({algebraic.systemList.length})</span>
			<div class="system-list">
				{#each algebraic.systemList as sys}
					<div class="system-item">
						<div class="system-header">
							<span class="system-id">{sys.id.split('_')[0]}</span>
							<button class="small danger" onclick={() => algebraic.removeSystem(sys.id)}>×</button>
						</div>
						<div class="system-state">
							{#each sys.state as val, i}
								{@const tmpl = algebraic.templateList.find(t => t.id === sys.templateId)}
								{@const name = tmpl?.state_names?.[i] ?? `v${i}`}
								<span class="state-val" title={name}><span class="state-name">{name}:</span> {val.toFixed(2)}</span>
							{/each}
						</div>
					</div>
				{/each}
			</div>
		</div>

		<div class="control-group">
			<span class="label">Wire Systems</span>
			<div class="wire-form">
				<div class="wire-row">
					<select bind:value={wireFrom}>
						<option value="">Output from...</option>
						{#each algebraic.systemList as sys}
							<option value={sys.id}>{sys.id.split('_')[0]}</option>
						{/each}
					</select>
					<input
						type="number"
						bind:value={wireFromPort}
						min="1"
						max={wireFrom ? getMaxOutputs(wireFrom) : 3}
						class="port-input"
						title="Output port"
					/>
				</div>
				<div class="wire-arrow">↓</div>
				<div class="wire-row">
					<select bind:value={wireTo}>
						<option value="">Input to...</option>
						{#each algebraic.systemList as sys}
							<option value={sys.id}>{sys.id.split('_')[0]}</option>
						{/each}
					</select>
					<input
						type="number"
						bind:value={wireToPort}
						min="1"
						max={wireTo ? getMaxInputs(wireTo) : 1}
						class="port-input"
						title="Input port"
					/>
				</div>
				<button onclick={createWire} disabled={!wireFrom || !wireTo || wireFrom === wireTo}>
					Connect
				</button>
			</div>
		</div>

		{#if algebraic.wireList.length > 0}
			<div class="control-group">
				<span class="label">Wires ({algebraic.wireList.length})</span>
				<div class="wire-list">
					{#each algebraic.wireList as wire}
						<div class="wire-item">
							<span class="wire-desc">
								{wire.fromSystem.split('_')[0]}:{wire.fromPort} → {wire.toSystem.split('_')[0]}:{wire.toPort}
							</span>
							<span class="wire-value">{wire.value.toFixed(2)}</span>
							<button class="small danger" onclick={() => algebraic.unwire(wire.id)}>×</button>
						</div>
					{/each}
				</div>
			</div>
		{/if}
	{/if}

	<div class="control-group">
		<span class="label">Time</span>
		<code class="time-display">{algebraic.time.toFixed(3)}s</code>
	</div>

	<CustomSystemEditor />
</div>

<style>
	.controls {
		display: flex;
		flex-direction: column;
		gap: 1rem;
		padding: 1rem;
		background: var(--bg-panel);
		border-right: 1px solid var(--border);
		min-width: 240px;
		max-width: 280px;
		max-height: 100%;
		overflow-y: auto;
	}

	.control-group {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.label {
		font-size: 0.7rem;
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

	.error {
		font-size: 0.75rem;
		color: #b85c4a;
	}

	.button-row {
		display: flex;
		gap: 0.25rem;
		flex-wrap: wrap;
	}

	button {
		background: var(--bg-dark);
		border: 1px solid var(--border);
		color: var(--text);
		padding: 0.4rem 0.6rem;
		font-family: inherit;
		font-size: 0.8rem;
		cursor: pointer;
		transition: all 0.15s;
	}

	button:hover {
		border-color: var(--accent);
		color: var(--accent);
	}

	button:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	button.active {
		background: var(--accent);
		color: var(--bg-dark);
		border-color: var(--accent);
	}

	button.small {
		padding: 0.15rem 0.4rem;
		font-size: 0.7rem;
	}

	button.danger:hover {
		border-color: var(--danger, #b85c4a);
		color: var(--danger, #b85c4a);
	}

	.speed-control {
		display: flex;
		align-items: center;
		gap: 0.5rem;
	}

	.speed-control input[type='range'] {
		flex: 1;
		accent-color: var(--accent);
	}

	.speed-value {
		font-size: 0.75rem;
		color: var(--accent);
		min-width: 2.5rem;
		text-align: right;
	}

	.template-grid {
		display: grid;
		grid-template-columns: repeat(2, 1fr);
		gap: 0.25rem;
	}

	.template-btn {
		font-size: 0.7rem;
		padding: 0.3rem;
	}

	select,
	input {
		background: var(--bg-dark);
		border: 1px solid var(--border);
		color: var(--text);
		padding: 0.4rem;
		font-family: inherit;
		font-size: 0.8rem;
	}

	select:focus,
	input:focus {
		outline: none;
		border-color: var(--accent);
	}

	.system-list {
		display: flex;
		flex-direction: column;
		gap: 0.35rem;
	}

	.system-item {
		background: var(--bg-dark);
		border: 1px solid var(--border);
		padding: 0.4rem;
		font-size: 0.75rem;
	}

	.system-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.25rem;
	}

	.system-id {
		color: var(--accent);
		font-weight: 500;
	}

	.system-state {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
	}

	.state-val {
		color: var(--text-dim);
		font-family: monospace;
		font-size: 0.7rem;
	}

	.state-name {
		color: var(--text-muted, #6b5d4f);
		font-weight: bold;
	}

	.wire-form {
		display: flex;
		flex-direction: column;
		gap: 0.35rem;
	}

	.wire-row {
		display: flex;
		gap: 0.25rem;
	}

	.wire-row select {
		flex: 1;
	}

	.port-input {
		width: 2.5rem;
		text-align: center;
	}

	.wire-arrow {
		text-align: center;
		color: var(--text-dim);
		font-size: 0.8rem;
	}

	.wire-list {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
	}

	.wire-item {
		display: flex;
		align-items: center;
		gap: 0.5rem;
		font-size: 0.7rem;
		padding: 0.3rem 0.4rem;
		background: var(--bg-dark);
		border: 1px solid var(--border);
	}

	.wire-desc {
		flex: 1;
		color: var(--text-dim);
	}

	.wire-value {
		color: var(--accent);
		font-family: monospace;
	}

	.time-display {
		font-size: 1rem;
		color: var(--accent);
	}
</style>
