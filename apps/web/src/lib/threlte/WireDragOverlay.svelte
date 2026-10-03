<!--
	WireDragOverlay.svelte
	Preview wire during drag creation.
	Uses same A* + fillet smoothing for the preview path.
-->
<script lang="ts">
	import { T, useTask, useThrelte } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic } from '$lib/stores/algebraic.svelte';
	import { HEADER_HEIGHT, WIRE_DRAG_MAX_VERTS } from './constants';
	import type { WireDragState } from './interactionTypes';
	import { computeWireRoute } from './utils/wireRouter';
	import { smoothCorners } from './utils/wireSmoothing';
	import { getPosition, getWindowDims } from './positionStore';

	interface Props {
		wireDrag: WireDragState;
		getObstacles: () => Array<{ x: number; y: number; w: number; h: number }>;
		screenToWorld: (x: number, y: number) => THREE.Vector2;
	}

	let { wireDrag, getObstacles, screenToWorld }: Props = $props();

	const ctx = useThrelte();

	const dragGeometry = new THREE.BufferGeometry();
	dragGeometry.setAttribute('position', new THREE.BufferAttribute(new Float32Array(WIRE_DRAG_MAX_VERTS * 3), 3));
	dragGeometry.setDrawRange(0, 0);

	let lastUpdateTime = 0;

	function getPortWorldPos(sysId: string, portIdx: number, isOutput: boolean): THREE.Vector2 {
		const sys = algebraic.systemList.find(s => s.id === sysId);
		if (!sys) return new THREE.Vector2(0, 0);
		const pos = getPosition(sysId);
		const dims = getWindowDims(sysId);
		const gx = pos.x;
		const gy = pos.y;
		const winWidth = dims.width;
		const winHeight = dims.height;
		const contentTop = winHeight / 2 - HEADER_HEIGHT;
		const contentBottom = -winHeight / 2;
		const contentHeight = contentTop - contentBottom;

		if (isOutput) {
			const nOutputs = sys.noutputs;
			const t = portIdx / (nOutputs + 1);
			const yPos = contentBottom + t * contentHeight;
			return new THREE.Vector2(gx + winWidth / 2, gy + yPos);
		} else {
			const nInputs = sys.ninputs;
			const t = portIdx / (nInputs + 1);
			const yPos = contentBottom + (1 - t) * contentHeight;
			return new THREE.Vector2(gx - winWidth / 2, gy + yPos);
		}
	}

	// Listen for pointermove on the renderer element while dragging
	$effect(() => {
		if (!wireDrag.active) {
			dragGeometry.setDrawRange(0, 0);
			return;
		}
		const el = ctx.renderer?.domElement;
		if (!el) return;

		const onMove = (e: PointerEvent) => {
			wireDrag.cursorWorld = screenToWorld(e.clientX, e.clientY);
		};

		const onUp = (e: PointerEvent) => {
			// If not dropped on a valid port (PortDot handles that), clean up
			if (wireDrag.active) {
				if (wireDrag.pendingRewire) {
					const rewire = wireDrag.pendingRewire;
					const targetSys = algebraic.systemList.find(s => s.id === rewire.toSystem);
					if (targetSys) algebraic.setState(rewire.toSystem, [...targetSys.state]);
					algebraic.unwire(rewire.id);
				}
				wireDrag.active = false;
				wireDrag.sourcePort = null;
				wireDrag.pendingRewire = null;
			}
		};

		el.addEventListener('pointermove', onMove);
		window.addEventListener('pointerup', onUp);
		return () => {
			el.removeEventListener('pointermove', onMove);
			window.removeEventListener('pointerup', onUp);
		};
	});

	// Update drag line on each frame while active
	useTask(() => {
		if (!wireDrag.active || !wireDrag.sourcePort) return;

		const now = performance.now();
		const from = getPortWorldPos(wireDrag.sourcePort.systemId, wireDrag.sourcePort.portIndex, true);
		const to = wireDrag.cursorWorld;
		const positions = dragGeometry.attributes.position.array as Float32Array;

		if (now - lastUpdateTime > 50) {
			lastUpdateTime = now;
			const obstacles = getObstacles();
			const rawRoute = computeWireRoute(from, to, obstacles);
			const route = smoothCorners(rawRoute);
			const nVerts = Math.min(route.length, WIRE_DRAG_MAX_VERTS);
			for (let i = 0; i < nVerts; i++) {
				positions[i * 3] = route[i].x;
				positions[i * 3 + 1] = route[i].y;
				positions[i * 3 + 2] = route[i].z;
			}
			dragGeometry.setDrawRange(0, nVerts);
		} else {
			const drawCount = dragGeometry.drawRange.count;
			if (drawCount >= 2) {
				const lastIdx = drawCount - 1;
				positions[lastIdx * 3] = to.x;
				positions[lastIdx * 3 + 1] = to.y;
				positions[lastIdx * 3 + 2] = 3;
			}
		}
		dragGeometry.attributes.position.needsUpdate = true;
	});
</script>

{#if wireDrag.active}
	<T.Line geometry={dragGeometry}>
		<T.LineBasicMaterial color={0xc9a84c} transparent opacity={0.3} depthTest={false} />
	</T.Line>
{/if}
