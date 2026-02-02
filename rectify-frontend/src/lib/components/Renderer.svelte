<!--
  Renderer.svelte

  A container component that provides a canvas-based rendering surface.
  Agnostic to what is rendered inside - could be ThreeJS, GoJS, raw canvas, etc.
  The actual rendering is handled by the child component via the canvas slot.
-->
<script lang="ts">
	import type { Snippet } from 'svelte';

	interface Props {
		children: Snippet<[{ width: number; height: number }]>;
	}

	let { children }: Props = $props();

	let containerEl: HTMLDivElement;
	let width = $state(800);
	let height = $state(600);

	$effect(() => {
		if (!containerEl) return;

		const observer = new ResizeObserver((entries) => {
			const entry = entries[0];
			if (entry) {
				width = entry.contentRect.width;
				height = entry.contentRect.height;
			}
		});

		observer.observe(containerEl);
		return () => observer.disconnect();
	});
</script>

<div class="renderer-container" bind:this={containerEl}>
	{@render children({ width, height })}
</div>

<style>
	.renderer-container {
		width: 100%;
		height: 100%;
		overflow: hidden;
		background: #000;
		border: 1px solid var(--border);
	}
</style>
