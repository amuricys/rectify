<script lang="ts">
	import Renderer from '$lib/components/Renderer.svelte';
	import LorenzRenderer from '$lib/components/LorenzRenderer.svelte';
	import AlgebraicRenderer from '$lib/components/AlgebraicRenderer.svelte';
	import Controls from '$lib/components/Controls.svelte';
	import AlgebraicControls from '$lib/components/AlgebraicControls.svelte';

	type Backend = 'lean' | 'julia';
	let backend = $state<Backend>('julia');
</script>

<div class="app">
	<header>
		<h1>rectify</h1>
		<span class="subtitle">dynamical systems visualizer</span>
		<div class="backend-switch">
			<button class:active={backend === 'lean'} onclick={() => backend = 'lean'}>
				Lean
			</button>
			<button class:active={backend === 'julia'} onclick={() => backend = 'julia'}>
				Julia
			</button>
		</div>
	</header>

	<main>
		<aside>
			{#if backend === 'lean'}
				<Controls />
			{:else}
				<AlgebraicControls />
			{/if}
		</aside>

		<section class="viewport">
			<Renderer>
				{#snippet children({ width, height })}
					{#if backend === 'lean'}
						<LorenzRenderer {width} {height} />
					{:else}
						<AlgebraicRenderer {width} {height} />
					{/if}
				{/snippet}
			</Renderer>
		</section>
	</main>
</div>

<style>
	.app {
		display: flex;
		flex-direction: column;
		height: 100vh;
	}

	header {
		display: flex;
		align-items: baseline;
		gap: 1rem;
		padding: 1rem;
		border-bottom: 1px solid var(--border);
	}

	.backend-switch {
		margin-left: auto;
		display: flex;
		gap: 0.25rem;
	}

	.backend-switch button {
		padding: 0.25rem 0.75rem;
		font-size: 0.75rem;
	}

	h1 {
		font-size: 1.25rem;
		font-weight: 400;
		letter-spacing: 0.15em;
		color: var(--accent-glow);
	}

	.subtitle {
		font-size: 0.75rem;
		color: var(--text-dim);
		letter-spacing: 0.05em;
	}

	main {
		display: flex;
		flex: 1;
		overflow: hidden;
	}

	aside {
		flex-shrink: 0;
	}

	.viewport {
		flex: 1;
		padding: 1rem;
	}
</style>
