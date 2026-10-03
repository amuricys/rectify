<!--
	EditOverlay.svelte
	HTML parameter editor rendered as a sibling of <Canvas> with screen-space positioning.
-->
<script lang="ts">
	import { algebraic } from '$lib/stores/algebraic.svelte';

	interface Props {
		editingSystemId: string | null;
		onclose: () => void;
	}

	let { editingSystemId, onclose }: Props = $props();

	let sys = $derived(editingSystemId ? algebraic.systemList.find(s => s.id === editingSystemId) : null);
	let tmpl = $derived(sys ? algebraic.templateList.find(t => t.id === sys.templateId) : null);
</script>

{#if editingSystemId && sys && tmpl}
	<div class="edit-overlay" style="right: 16px; top: 16px;">
		<div class="edit-header">
			<span>{tmpl.name}</span>
			<button class="edit-close" onclick={onclose}>x</button>
		</div>
		{#each tmpl.parameters as param, i}
			<div class="edit-row">
				<label for="param-{i}">{param.name}</label>
				<input
					id="param-{i}"
					type="number"
					step="any"
					value={sys.parameters[param.name] ?? param.default}
					onchange={(e) => {
						const val = parseFloat((e.target as HTMLInputElement).value);
						if (!isNaN(val) && editingSystemId) {
							algebraic.setParams(editingSystemId, { ...sys!.parameters, [param.name]: val });
						}
					}}
				/>
			</div>
		{/each}
	</div>
{/if}

<style>
	.edit-overlay {
		position: absolute;
		background: #1a1410;
		border: 1px solid #c9a84c;
		border-radius: 4px;
		padding: 0.5rem;
		min-width: 160px;
		z-index: 10;
		font-family: 'CMU Serif', serif;
		font-size: 0.75rem;
		color: #d4c5a0;
	}

	.edit-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.4rem;
		color: #c9a84c;
		font-weight: bold;
	}

	.edit-close {
		background: none;
		border: none;
		color: #9a8b78;
		cursor: pointer;
		font-size: 0.8rem;
		padding: 0 0.2rem;
	}

	.edit-close:hover {
		color: #c9a84c;
	}

	.edit-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 0.5rem;
		margin-bottom: 0.25rem;
	}

	.edit-row label {
		color: #9a8b78;
	}

	.edit-row input {
		width: 70px;
		background: #0f0b08;
		border: 1px solid #3a2e24;
		color: #d4c5a0;
		padding: 0.2rem 0.3rem;
		font-family: monospace;
		font-size: 0.7rem;
		text-align: right;
	}

	.edit-row input:focus {
		outline: none;
		border-color: #c9a84c;
	}
</style>
