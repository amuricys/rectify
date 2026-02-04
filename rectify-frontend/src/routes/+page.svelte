<script lang="ts">
	import { onMount } from 'svelte';
	import Renderer from '$lib/components/Renderer.svelte';
	import TSPRenderer from '$lib/components/TSPRenderer.svelte';
	import OptimizationControls from '$lib/components/OptimizationControls.svelte';
	import AlgebraicRenderer from '$lib/components/AlgebraicRenderer.svelte';
	import AlgebraicControls from '$lib/components/AlgebraicControls.svelte';
	import { optimization } from '$lib/stores/optimization.svelte';
	import { algebraic } from '$lib/stores/algebraic.svelte';

	type Tab = 'optimization' | 'dynamics';
	let activeTab = $state<Tab>('dynamics');

	// Auto-connect to the appropriate backend when switching tabs
	$effect(() => {
		if (activeTab === 'optimization') {
			if (optimization.status === 'disconnected') {
				optimization.connect();
			}
		} else if (activeTab === 'dynamics') {
			if (algebraic.status === 'disconnected') {
				algebraic.connect();
			}
		}
	});

	onMount(() => {
		// Connect to dynamics by default
		algebraic.connect();

		return () => {
			optimization.disconnect();
			algebraic.disconnect();
		};
	});
</script>

<div class="app">
	<header>
		<h1>rectify</h1>
		<nav class="tabs">
			<button class:active={activeTab === 'dynamics'} onclick={() => (activeTab = 'dynamics')}>
				Open Systems
			</button>
			<button class:active={activeTab === 'optimization'} onclick={() => (activeTab = 'optimization')}>
				Optimization
			</button>
		</nav>
	</header>

	<main>
		{#if activeTab === 'optimization'}
			<aside>
				<OptimizationControls />
			</aside>
			<section class="viewport">
				<Renderer>
					{#snippet children({ width, height })}
						<TSPRenderer {width} {height} />
					{/snippet}
				</Renderer>
			</section>
		{:else}
			<aside>
				<AlgebraicControls />
			</aside>
			<section class="viewport">
				<Renderer>
					{#snippet children({ width, height })}
						<AlgebraicRenderer {width} {height} />
					{/snippet}
				</Renderer>
			</section>
		{/if}
	</main>
</div>

<style>
	.app {
		display: flex;
		flex-direction: column;
		height: 100vh;
		background: var(--bg);
	}

	header {
		display: flex;
		align-items: center;
		gap: 2rem;
		padding: 0.75rem 1rem;
		border-bottom: 1px solid var(--border);
		background: var(--bg-panel);
	}

	h1 {
		font-size: 1.1rem;
		font-weight: 400;
		letter-spacing: 0.15em;
		color: var(--accent-glow);
		margin: 0;
	}

	.tabs {
		display: flex;
		gap: 0.25rem;
	}

	.tabs button {
		background: transparent;
		border: 1px solid transparent;
		color: var(--text-dim);
		padding: 0.4rem 0.8rem;
		font-family: inherit;
		font-size: 0.8rem;
		cursor: pointer;
		transition: all 0.15s;
		letter-spacing: 0.05em;
	}

	.tabs button:hover {
		color: var(--text);
	}

	.tabs button.active {
		color: var(--accent);
		border-color: var(--border);
		background: var(--bg-dark);
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
		padding: 0;
		position: relative;
	}
</style>
