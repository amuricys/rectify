<script lang="ts">
	import { T, useThrelte, useTask } from '@threlte/core';
	import { interactivity } from '@threlte/extras';
	import * as THREE from 'three';
	import { algebraic, type SystemState, type WireState, type CompositeGroup } from '$lib/stores/algebraic.svelte';
	import SystemWindow from './SystemWindow.svelte';
	import WireVisual from './WireVisual.svelte';
	import WireDragOverlay from './WireDragOverlay.svelte';
	import CompositeWindow from './CompositeWindow.svelte';
	import { HEADER_HEIGHT } from './constants';
	import type { PortDotInfo, WireDragState } from './interactionTypes';
	import { getPosition, getWindowDims } from './positionStore';

	interface Props {
		editingSystemId: string | null;
	}

	let { editingSystemId = $bindable() }: Props = $props();

	// Enable Threlte interactivity plugin (raycasting/events)
	interactivity();

	const ctx = useThrelte();
	const { size, camera } = ctx;

	// Camera zoom
	let cameraZoom = $state(1);

	// Camera dimensions derived from size and zoom
	let viewSize = $derived(100 * cameraZoom);
	let aspect = $derived($size.height > 0 ? $size.width / $size.height : 1);

	// Wire drag state (shared context for PortDot → WireDragOverlay communication)
	let wireDrag: WireDragState = $state({
		active: false,
		sourcePort: null,
		pendingRewire: null,
		cursorWorld: new THREE.Vector2(0, 0)
	});

	// Background plane for pan and wire drop
	let isDraggingComposite = $state(false);
	let compositeDragId: string | null = $state(null);
	let compositeDragStarts = new Map<string, { x: number; y: number }>();
	let compositeDragOrigin = new THREE.Vector2();

	// Wheel zoom on the renderer's DOM element
	$effect(() => {
		const el = ctx.renderer?.domElement;
		if (!el) return;
		const onWheel = (e: WheelEvent) => {
			e.preventDefault();
			const factor = e.deltaY > 0 ? 1.1 : 0.9;
			cameraZoom = Math.max(0.3, Math.min(3, cameraZoom * factor));
		};
		el.addEventListener('wheel', onWheel, { passive: false });
		return () => el.removeEventListener('wheel', onWheel);
	});

	// Enable local clipping on the renderer
	$effect(() => {
		if (ctx.renderer) {
			ctx.renderer.localClippingEnabled = true;
			ctx.renderer.setClearColor(0x0f0b08, 1);
		}
	});

	// Helper: screen to world
	function screenToWorld(screenX: number, screenY: number): THREE.Vector2 {
		const el = ctx.renderer?.domElement;
		if (!el || !$camera) return new THREE.Vector2(0, 0);
		const rect = el.getBoundingClientRect();
		const ndcX = ((screenX - rect.left) / rect.width) * 2 - 1;
		const ndcY = -((screenY - rect.top) / rect.height) * 2 + 1;
		const cam = $camera as THREE.OrthographicCamera;
		const worldX = ndcX * (cam.right - cam.left) / 2;
		const worldY = ndcY * (cam.top - cam.bottom) / 2;
		return new THREE.Vector2(worldX, worldY);
	}

	// Expose screenToWorld for child components
	export function getScreenToWorld() { return screenToWorld; }

	function getEditorScreenPos(systemId: string): { x: number; y: number } | null {
		const sys = algebraic.systemList.find(s => s.id === systemId);
		if (!sys || !$camera || !ctx.renderer) return null;
		const el = ctx.renderer.domElement;
		const rect = el.getBoundingClientRect();
		const cam = $camera as THREE.OrthographicCamera;
		const worldX = (sys.position?.x ?? 0) + 25; // right edge + offset
		const worldY = (sys.position?.y ?? 0) + 23;
		const ndcX = worldX / ((cam.right - cam.left) / 2);
		const ndcY = worldY / ((cam.top - cam.bottom) / 2);
		const screenX = ((ndcX + 1) / 2) * rect.width;
		const screenY = ((1 - ndcY) / 2) * rect.height;
		return { x: screenX, y: screenY };
	}

	// Build obstacles array from all system positions (for wire routing)
	function getObstacles(): Array<{ x: number; y: number; w: number; h: number }> {
		return algebraic.systemList.map(sys => {
			const pos = getPosition(sys.id);
			const dims = getWindowDims(sys.id);
			return {
				x: pos.x,
				y: pos.y,
				w: dims.width,
				h: dims.height + HEADER_HEIGHT
			};
		});
	}

	// Composite header drag handlers
	function onCompositeHeaderDown(compositeId: string, event: PointerEvent) {
		const group = algebraic.compositeGroups.find(g => g.id === compositeId);
		if (!group) return;
		isDraggingComposite = true;
		compositeDragId = compositeId;
		compositeDragOrigin.set(event.clientX, event.clientY);
		compositeDragStarts.clear();
		for (const memberId of group.memberSystemIds) {
			const sys = algebraic.systemList.find(s => s.id === memberId);
			if (sys) {
				compositeDragStarts.set(memberId, { x: sys.position?.x ?? 0, y: sys.position?.y ?? 0 });
			}
		}
	}
</script>

<!-- Orthographic camera -->
<T.OrthographicCamera
	makeDefault
	position.z={200}
	left={-viewSize * aspect}
	right={viewSize * aspect}
	top={viewSize}
	bottom={-viewSize}
	near={0.1}
	far={1000}
/>

<!-- Lights -->
<T.AmbientLight color={0x605040} />
<T.DirectionalLight color={0xffffff} intensity={0.6} position.z={100} />

<!-- System windows -->
{#each algebraic.systemList as sys (sys.id)}
	<SystemWindow
		{sys}
		{wireDrag}
		onEditSystem={(id) => { editingSystemId = editingSystemId === id ? null : id; }}
		{screenToWorld}
		{getObstacles}
	/>
{/each}

<!-- Wires -->
{#each algebraic.wireList as wire (wire.id)}
	<WireVisual {wire} {getObstacles} />
{/each}

<!-- Wire drag preview -->
<WireDragOverlay {wireDrag} {getObstacles} {screenToWorld} />

<!-- Composite windows -->
{#each algebraic.compositeGroups as group (group.id)}
	<CompositeWindow
		{group}
		{getObstacles}
		onHeaderDown={(e) => onCompositeHeaderDown(group.id, e)}
	/>
{/each}
