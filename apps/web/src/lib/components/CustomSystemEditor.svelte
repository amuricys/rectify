<!--
  CustomSystemEditor.svelte

  Collapsible editor for defining custom dynamical systems.
  Parses equation syntax client-side, sends to Julia backend for compilation.
-->
<script lang="ts">
	import { algebraic } from '$lib/stores/algebraic.svelte';
	import { parseEquations, type ParsedSystem, type ParseError } from '$lib/utils/equationParser';

	let expanded = $state(false);
	let name = $state('');
	let equationText = $state(`dx/dt = sigma * (y - x)
dy/dt = x * (rho - z) - y
dz/dt = x * y - beta * z

parameters: sigma = 10.0, rho = 28.0, beta = 2.667
initial: x = 1.0, y = 1.0, z = 1.0`);
	let error = $state<string | null>(null);
	let validationResult = $state<ParsedSystem | null>(null);

	function validate() {
		error = null;
		validationResult = null;

		if (!name.trim()) {
			error = 'Name is required';
			return;
		}

		const result = parseEquations(equationText, name.trim());

		if ('error' in result) {
			const pe = result as ParseError;
			error = pe.line ? `Line ${pe.line}: ${pe.error}` : pe.error;
			return;
		}

		validationResult = result as ParsedSystem;
	}

	function create() {
		if (!validationResult) {
			validate();
			if (!validationResult) return;
		}

		algebraic.defineCustomSystem(validationResult);
		error = null;
		validationResult = null;
	}
</script>

<div class="editor-section">
	<button class="toggle-btn" onclick={() => (expanded = !expanded)}>
		{expanded ? '▾' : '▸'} Custom System
	</button>

	{#if expanded}
		<div class="editor-content">
			<div class="field">
				<label for="sys-name">Name</label>
				<input id="sys-name" type="text" bind:value={name} placeholder="my_system" />
			</div>

			<div class="field">
				<label for="sys-equations">Equations</label>
				<textarea id="sys-equations" bind:value={equationText} rows="10" spellcheck="false"
				></textarea>
			</div>

			<div class="button-row">
				<button onclick={validate}>Validate</button>
				<button onclick={create} class="create-btn" disabled={!validationResult}>Create</button>
			</div>

			{#if error}
				<div class="error-display">{error}</div>
			{/if}

			{#if validationResult}
				<div class="validation-ok">
					<div class="ok-header">Valid</div>
					<div class="ok-detail">
						{validationResult.stateVars.length} states: {validationResult.stateVars.join(', ')}
					</div>
					{#if validationResult.parameters.length > 0}
						<div class="ok-detail">
							{validationResult.parameters.length} params: {validationResult.parameters
								.map((p) => `${p.name}=${p.default}`)
								.join(', ')}
						</div>
					{/if}
				</div>
			{/if}
		</div>
	{/if}
</div>

<style>
	.editor-section {
		border-top: 1px solid var(--border);
		padding-top: 0.5rem;
	}

	.toggle-btn {
		background: none;
		border: none;
		color: var(--text-dim);
		font-size: 0.75rem;
		cursor: pointer;
		padding: 0.25rem 0;
		text-transform: uppercase;
		letter-spacing: 0.1em;
		width: 100%;
		text-align: left;
	}

	.toggle-btn:hover {
		color: var(--accent);
	}

	.editor-content {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
		margin-top: 0.5rem;
	}

	.field {
		display: flex;
		flex-direction: column;
		gap: 0.25rem;
	}

	.field label {
		font-size: 0.65rem;
		text-transform: uppercase;
		letter-spacing: 0.08em;
		color: var(--text-muted);
	}

	.field input {
		background: var(--bg-dark);
		border: 1px solid var(--border);
		color: var(--text);
		padding: 0.35rem 0.5rem;
		font-family: inherit;
		font-size: 0.8rem;
	}

	.field input:focus {
		outline: none;
		border-color: var(--accent);
	}

	textarea {
		background: var(--bg-dark);
		border: 1px solid var(--border);
		color: var(--text);
		padding: 0.5rem;
		font-family: 'IBM Plex Mono', 'Fira Code', monospace;
		font-size: 0.7rem;
		line-height: 1.5;
		resize: vertical;
		tab-size: 2;
	}

	textarea:focus {
		outline: none;
		border-color: var(--accent);
	}

	.button-row {
		display: flex;
		gap: 0.25rem;
	}

	.button-row button {
		flex: 1;
		background: var(--bg-dark);
		border: 1px solid var(--border);
		color: var(--text);
		padding: 0.35rem 0.5rem;
		font-family: inherit;
		font-size: 0.75rem;
		cursor: pointer;
	}

	.button-row button:hover {
		border-color: var(--accent);
		color: var(--accent);
	}

	.button-row button:disabled {
		opacity: 0.4;
		cursor: not-allowed;
	}

	.create-btn {
		background: var(--accent-dim) !important;
		color: var(--text) !important;
		border-color: var(--accent) !important;
	}

	.create-btn:disabled {
		background: var(--bg-dark) !important;
		border-color: var(--border) !important;
	}

	.error-display {
		font-size: 0.7rem;
		color: var(--danger, #b85c4a);
		padding: 0.35rem 0.5rem;
		background: rgba(184, 92, 74, 0.1);
		border: 1px solid rgba(184, 92, 74, 0.3);
	}

	.validation-ok {
		font-size: 0.7rem;
		padding: 0.35rem 0.5rem;
		background: rgba(122, 155, 104, 0.1);
		border: 1px solid rgba(122, 155, 104, 0.3);
	}

	.ok-header {
		color: var(--success, #7a9b68);
		font-weight: bold;
		margin-bottom: 0.15rem;
	}

	.ok-detail {
		color: var(--text-dim);
	}
</style>
