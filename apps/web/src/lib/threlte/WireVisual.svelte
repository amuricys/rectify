<!--
	WireVisual.svelte
	A*-routed smooth wire between ports with value whisker.
	Bug #4 fix: uses fillet-based smoothing instead of CatmullRom.
-->
<script lang="ts">
	import { T, useTask } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic, type WireState } from '$lib/stores/algebraic.svelte';
	import { HEADER_HEIGHT } from './constants';
	import { computeWireRoute } from './utils/wireRouter';
	import { smoothCorners } from './utils/wireSmoothing';
	import { getPosition, getWindowDims } from './positionStore';

	interface Props {
		wire: WireState;
		getObstacles: () => Array<{ x: number; y: number; w: number; h: number }>;
	}

	let { wire, getObstacles }: Props = $props();

	// Main wire line
	const wireGeometry = new THREE.BufferGeometry();
	const wirePositions = new Float32Array(600); // up to 200 verts
	wireGeometry.setAttribute('position', new THREE.BufferAttribute(wirePositions, 3));
	wireGeometry.setDrawRange(0, 0);

	// Value whisker
	const valueGeometry = new THREE.BufferGeometry();
	valueGeometry.setAttribute('position', new THREE.BufferAttribute(new Float32Array(12), 3));
	valueGeometry.setDrawRange(0, 0);

	// Hidden if inside a composite that's not in look-inside mode
	let composite = $derived(algebraic.compositeGroups.find(g => g.internalWireIds.includes(wire.id)));
	let hidden = $derived(composite != null && !composite.lookInside);

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

	let lastFromX = 0, lastFromY = 0, lastToX = 0, lastToY = 0;

	useTask(() => {
		if (hidden) {
			wireGeometry.setDrawRange(0, 0);
			valueGeometry.setDrawRange(0, 0);
			return;
		}

		const from = getPortWorldPos(wire.fromSystem, wire.fromPort, true);
		const to = getPortWorldPos(wire.toSystem, wire.toPort, false);

		// Only recompute route if ports moved
		if (from.x !== lastFromX || from.y !== lastFromY || to.x !== lastToX || to.y !== lastToY) {
			lastFromX = from.x; lastFromY = from.y;
			lastToX = to.x; lastToY = to.y;

			const obstacles = getObstacles();
			const rawRoute = computeWireRoute(from, to, obstacles);
			const route = smoothCorners(rawRoute);

			const positions = wireGeometry.attributes.position.array as Float32Array;
			const nVerts = Math.min(route.length, 200);
			for (let i = 0; i < nVerts; i++) {
				positions[i * 3] = route[i].x;
				positions[i * 3 + 1] = route[i].y;
				positions[i * 3 + 2] = route[i].z;
			}
			wireGeometry.attributes.position.needsUpdate = true;
			wireGeometry.setDrawRange(0, nVerts);
		}

		// Update value whisker
		const routePos = wireGeometry.attributes.position.array as Float32Array;
		const drawCount = wireGeometry.drawRange.count;
		if (drawCount >= 2) {
			const dx = routePos[3] - routePos[0];
			const dy = routePos[4] - routePos[1];
			const len = Math.sqrt(dx * dx + dy * dy);
			if (len > 0.1) {
				const perpX = -dy / len;
				const perpY = dx / len;
				const deflection = Math.tanh(wire.value / 5) * 3;
				const from = getPortWorldPos(wire.fromSystem, wire.fromPort, true);
				const valPos = valueGeometry.attributes.position.array as Float32Array;
				valPos[0] = from.x; valPos[1] = from.y; valPos[2] = 4;
				valPos[3] = from.x + dx / len * 2; valPos[4] = from.y + dy / len * 2; valPos[5] = 4;
				valPos[6] = from.x + dx / len * 2 + perpX * deflection; valPos[7] = from.y + dy / len * 2 + perpY * deflection; valPos[8] = 4;
				valPos[9] = from.x + dx / len * 4 + perpX * deflection * 0.5; valPos[10] = from.y + dy / len * 4 + perpY * deflection * 0.5; valPos[11] = 4;
				valueGeometry.attributes.position.needsUpdate = true;
				valueGeometry.setDrawRange(0, 4);
			}
		}
	});
</script>

{#if !hidden}
	<!-- Main wire line -->
	<T.Line geometry={wireGeometry}>
		<T.LineBasicMaterial color={0xc9a84c} transparent opacity={0.7} depthTest={false} />
	</T.Line>

	<!-- Value whisker -->
	<T.Line geometry={valueGeometry}>
		<T.LineBasicMaterial color={0xc9a84c} transparent opacity={0.4} depthTest={false} />
	</T.Line>
{/if}
