<script lang="ts">
	import { onMount } from 'svelte';
	import Renderer from '$lib/components/Renderer.svelte';
	import TSPRenderer from '$lib/components/TSPRenderer.svelte';
	import OptimizationControls from '$lib/components/OptimizationControls.svelte';
	import AlgebraicScene from '$lib/threlte/AlgebraicScene.svelte';
	import AlgebraicControls from '$lib/components/AlgebraicControls.svelte';
	import { optimization } from '$lib/stores/optimization.svelte';
	import { algebraic } from '$lib/stores/algebraic.svelte';

	type Tab = 'optimization' | 'dynamics';
	let activeTab = $state<Tab>('dynamics');

	// Sidebar state
	let sidebarWidth = $state(260);
	let sidebarCollapsed = $state(false);
	let isResizingSidebar = false;
	let resizeStartX = 0;
	let resizeStartWidth = 0;
	const SIDEBAR_MIN = 180;
	const SIDEBAR_MAX = 450;

	function onSidebarResizeStart(event: MouseEvent) {
		isResizingSidebar = true;
		resizeStartX = event.clientX;
		resizeStartWidth = sidebarWidth;
		document.body.style.cursor = 'col-resize';
		document.body.style.userSelect = 'none';
		window.addEventListener('mousemove', onSidebarResizeMove);
		window.addEventListener('mouseup', onSidebarResizeEnd);
	}

	function onSidebarResizeMove(event: MouseEvent) {
		if (!isResizingSidebar) return;
		const delta = event.clientX - resizeStartX;
		sidebarWidth = Math.min(SIDEBAR_MAX, Math.max(SIDEBAR_MIN, resizeStartWidth + delta));
	}

	function onSidebarResizeEnd() {
		isResizingSidebar = false;
		document.body.style.cursor = '';
		document.body.style.userSelect = '';
		window.removeEventListener('mousemove', onSidebarResizeMove);
		window.removeEventListener('mouseup', onSidebarResizeEnd);
	}

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
		{#if !sidebarCollapsed}
			<aside style="width: {sidebarWidth}px;">
				{#if activeTab === 'optimization'}
					<OptimizationControls />
				{:else}
					<AlgebraicControls />
				{/if}
			</aside>
			<!-- svelte-ignore a11y_no_static_element_interactions -->
			<div class="sidebar-handle" onmousedown={onSidebarResizeStart}>
				<div class="handle-line"></div>
			</div>
		{/if}
		<div class="collapse-rail">
			<button class="collapse-btn" onclick={() => sidebarCollapsed = !sidebarCollapsed} title={sidebarCollapsed ? 'Show sidebar' : 'Hide sidebar'}>
				{sidebarCollapsed ? '▶' : '◀'}
			</button>
		</div>
		<section class="viewport">
			{#if activeTab === 'optimization'}
				<Renderer>
					{#snippet children({ width, height })}
						<TSPRenderer {width} {height} />
					{/snippet}
				</Renderer>
			{:else}
				<AlgebraicScene />
			{/if}
		</section>
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
		overflow: hidden;
		border-right: 1px solid var(--border);
	}

	.sidebar-handle {
		flex-shrink: 0;
		width: 5px;
		cursor: col-resize;
		background: transparent;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: background 0.15s;
	}

	.sidebar-handle:hover,
	.sidebar-handle:active {
		background: var(--border);
	}

	.handle-line {
		width: 1px;
		height: 40px;
		background: var(--border);
		border-radius: 1px;
	}

	.sidebar-handle:hover .handle-line,
	.sidebar-handle:active .handle-line {
		background: var(--accent);
		width: 2px;
	}

	.collapse-rail {
		flex-shrink: 0;
		display: flex;
		align-items: flex-start;
		padding-top: 0.5rem;
	}

	.collapse-btn {
		background: var(--bg-panel);
		border: 1px solid var(--border);
		border-left: none;
		color: var(--text-dim);
		font-size: 0.55rem;
		padding: 0.4rem 0.2rem;
		cursor: pointer;
		line-height: 1;
		border-radius: 0 3px 3px 0;
	}

	.collapse-btn:hover {
		color: var(--accent);
		border-color: var(--accent);
	}

	.viewport {
		flex: 1;
		padding: 0;
		position: relative;
	}
</style>
