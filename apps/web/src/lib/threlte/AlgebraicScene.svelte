<script lang="ts">
	import { Canvas } from '@threlte/core';
	import SceneContents from './SceneContents.svelte';
	import EditOverlay from './EditOverlay.svelte';
	import { CANVAS_FONT } from './constants';
	import { onMount } from 'svelte';

	let editingSystemId: string | null = $state(null);

	onMount(() => {
		// Pre-load CMU Serif for canvas textures
		Promise.all([
			document.fonts.load(`400 16px ${CANVAS_FONT}`),
			document.fonts.load(`700 16px ${CANVAS_FONT}`),
			document.fonts.load(`italic 16px ${CANVAS_FONT}`)
		]).catch(() => { /* font fallback is acceptable */ });
	});
</script>

<div class="scene-container">
	<Canvas renderMode="always" colorManagementEnabled={false}>
		<SceneContents bind:editingSystemId />
	</Canvas>

	<EditOverlay {editingSystemId} onclose={() => editingSystemId = null} />
</div>

<style>
	.scene-container {
		position: relative;
		width: 100%;
		height: 100%;
		background: #0f0b08;
		border: 1px solid var(--border);
	}
</style>
