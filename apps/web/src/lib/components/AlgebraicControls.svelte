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

	// Template hover popup
	let hoveredTemplate = $state<string | null>(null);
	let pendingSpeed = $state(1);
	let pendingDt = $state('0.001');
	let dtApplied = $state(false);
	let dtAppliedTimer: ReturnType<typeof setTimeout> | null = null;

	$effect(() => {
		pendingSpeed = algebraic.speed;
	});

	$effect(() => {
		pendingDt = algebraic.dt.toString();
	});

	function applySpeed(value: number) {
		const clamped = Math.max(0.1, Math.min(5, value));
		pendingSpeed = clamped;
		algebraic.setSpeed(clamped);
	}

	function applyDt() {
		const parsed = Number(pendingDt);
		if (!Number.isFinite(parsed) || parsed <= 0) return;
		algebraic.setDt(parsed);
		dtApplied = true;
		if (dtAppliedTimer) clearTimeout(dtAppliedTimer);
		dtAppliedTimer = setTimeout(() => {
			dtApplied = false;
			dtAppliedTimer = null;
		}, 900);
	}

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
			{#if algebraic.errorCode === 'NON_FINITE_STATE' && algebraic.recoverAction === 'resumeFinite'}
				<button class="small" onclick={() => algebraic.resumeFinite()}>Resume Last Finite</button>
			{/if}
		{/if}
	</div>

	<div class="control-group">
		<span class="label">Playback</span>
		<div class="button-row playback-row">
			<button onclick={() => algebraic.toggle()} class:active={algebraic.running}>
				{algebraic.running ? 'Pause' : 'Play'}
			</button>
			<div class="step-inline">
				<button class="step-btn" onclick={() => algebraic.step()}>Step</button>
				<span class:applied={dtApplied}>{dtApplied ? '✓' : 'dt ='}</span>
				<input
					type="text"
					bind:value={pendingDt}
					class="dt-input"
					onkeydown={(e) => {
						if (e.key === 'Enter') {
							e.preventDefault();
							applyDt();
						}
					}}
				/>
			</div>
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
				value={pendingSpeed}
				oninput={(e) => applySpeed(Number((e.currentTarget as HTMLInputElement).value))}
			/>
			<span class="speed-value">{pendingSpeed.toFixed(1)}x</span>
		</div>
	</div>

	{#if algebraic.templateList.length > 0}
		<div class="control-group">
			<span class="label">Add System</span>
			<div class="template-grid">
				{#each algebraic.templateList as template}
					<div class="template-wrapper">
						<button
							class="template-btn"
							onclick={() => algebraic.addSystem(template.id, { x: Math.random() * 100, y: Math.random() * 100 })}
							onmouseenter={() => hoveredTemplate = template.id}
							onmouseleave={() => hoveredTemplate = null}
						>
							{template.name}
						</button>
						{#if hoveredTemplate === template.id}
							<div class="template-popup">
								<div class="popup-heading">states</div>
								{#each template.state_names as name}
									<div class="popup-item">{name} : &#x211D;</div>
								{/each}
								{#if template.parameters.length > 0}
									<div class="popup-heading">params</div>
									{#each template.parameters as p}
										<div class="popup-item">{p.name} = {p.default}</div>
									{/each}
								{/if}
							</div>
						{/if}
					</div>
				{/each}
			</div>
		</div>
	{/if}

	{#if algebraic.systemList.length > 0}
		{@const standaloneSystems = algebraic.systemList.filter(s => !algebraic.getCompositeForSystem(s.id))}
		{#if algebraic.compositeGroups.length > 0}
			<div class="control-group">
				<span class="label">Composites ({algebraic.compositeGroups.length})</span>
				<div class="system-list">
					{#each algebraic.compositeGroups as group}
						<div class="system-item composite-item">
							<div class="system-header">
								<span class="system-id composite-name">{group.name}</span>
							</div>
							<div class="composite-members">
								{#each group.memberSystemIds as memberId}
									{@const sys = algebraic.systemList.find(s => s.id === memberId)}
									{#if sys}
										<span class="member-tag">{sys.templateId.split('_')[0]}</span>
									{/if}
								{/each}
							</div>
						</div>
					{/each}
				</div>
			</div>
		{/if}

		{#if standaloneSystems.length > 0}
			<div class="control-group">
				<span class="label">Systems ({standaloneSystems.length})</span>
				<div class="system-list">
					{#each standaloneSystems as sys}
						<div class="system-item">
							<div class="system-header">
								<span class="system-id">{sys.id.split('_')[0]}</span>
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
		{/if}

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
		width: 100%;
		height: 100%;
		overflow-y: auto;
		box-sizing: border-box;
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
		align-items: center;
	}

	.playback-row {
		row-gap: 0.4rem;
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

	.step-inline {
		display: inline-flex;
		align-items: center;
		gap: 0.3rem;
		padding: 0.25rem 0.45rem;
		background: var(--bg-dark);
		border: 1px solid var(--border);
		font-size: 0.75rem;
	}

	.step-btn {
		margin-right: 0.2rem;
	}

	.step-inline span {
		color: var(--text-dim);
		font-family: 'CMU Serif', serif;
		min-width: 2.4rem;
		text-align: right;
		transition: color 0.12s ease;
	}

	.step-inline span.applied {
		color: #7a9b68;
		font-weight: 700;
	}

	.dt-input {
		width: 4.8rem;
		padding: 0.2rem 0.35rem;
		font-size: 0.75rem;
		border: 1px solid var(--border);
		background: transparent;
		color: var(--accent);
		font-family: monospace;
	}

	.template-grid {
		display: grid;
		grid-template-columns: repeat(2, 1fr);
		gap: 0.25rem;
	}

	.template-wrapper {
		position: relative;
	}

	.template-btn {
		font-size: 0.7rem;
		padding: 0.3rem;
		width: 100%;
	}

	.template-popup {
		position: absolute;
		left: 100%;
		top: 0;
		margin-left: 0.25rem;
		background: var(--bg-dark);
		border: 1px solid var(--accent);
		padding: 0.35rem 0.5rem;
		font-size: 0.65rem;
		z-index: 100;
		min-width: 80px;
	}

	.popup-heading {
		color: var(--text-dim);
		font-weight: bold;
		font-size: 0.6rem;
		text-transform: uppercase;
		letter-spacing: 0.05em;
		margin-top: 0.25rem;
		margin-bottom: 0.1rem;
	}

	.popup-heading:first-child {
		margin-top: 0;
	}

	.popup-item {
		color: var(--accent);
		font-family: 'CMU Serif', serif;
		font-style: italic;
		font-size: 0.65rem;
		padding-left: 0.25rem;
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
		font-family: 'CMU Serif', serif;
		font-style: italic;
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
		font-family: 'CMU Serif', serif;
	}

	.time-display {
		font-size: 1rem;
		color: var(--accent);
	}

	.composite-item {
		border-color: #4d3d2e;
		border-style: dashed;
	}

	.composite-name {
		font-size: 0.7rem;
	}

	.composite-members {
		display: flex;
		gap: 0.3rem;
		flex-wrap: wrap;
	}

	.member-tag {
		font-size: 0.65rem;
		color: var(--text-dim);
		background: var(--bg-panel, #1a1410);
		padding: 0.1rem 0.3rem;
		border: 1px solid #3a2e24;
	}
</style>
