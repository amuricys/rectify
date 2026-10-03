<!--
	GhostWire.svelte
	Bug #3 fix: Ghost wires use A*-routed paths (same as regular wires)
	instead of straight 2-point lines.
-->
<script lang="ts">
	import { T } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic } from '$lib/stores/algebraic.svelte';
	import { HEADER_HEIGHT } from './constants';
	import { computeWireRoute } from './utils/wireRouter';
	import { smoothCorners } from './utils/wireSmoothing';
	import { getPosition, getWindowDims } from './positionStore';

	interface Props {
		memberId: string;
		portIndex: number;
		compositeX: number;
		compositeY: number;
		compositeWidth: number;
		compositeHeight: number;
		getObstacles: () => Array<{ x: number; y: number; w: number; h: number }>;
	}

	let { memberId, portIndex, compositeX, compositeY, compositeWidth, compositeHeight, getObstacles }: Props = $props();

	// Compute the ghost wire route from border to port
	let ghostLineGeom = $derived.by(() => {
		const sys = algebraic.systemList.find(s => s.id === memberId);
		if (!sys) return null;

		const pos = getPosition(memberId);
		const dims = getWindowDims(memberId);
		const gx = pos.x;
		const gy = pos.y;
		const winWidth = dims.width;
		const winHeight = dims.height;
		const contentTop = winHeight / 2 - HEADER_HEIGHT;
		const contentBottom = -winHeight / 2;
		const contentHeight = contentTop - contentBottom;
		const nInputs = sys.ninputs;
		const t = portIndex / (nInputs + 1);
		const yPos = contentBottom + (1 - t) * contentHeight;

		const portWorldX = gx - winWidth / 2;
		const portWorldY = gy + yPos;

		// Border position (left edge of composite)
		const borderX = compositeX - compositeWidth / 2;

		// Route from border to port using A*
		const from = new THREE.Vector2(borderX, portWorldY);
		const to = new THREE.Vector2(portWorldX, portWorldY);
		const obstacles = getObstacles();
		const rawRoute = computeWireRoute(from, to, obstacles);
		const route = smoothCorners(rawRoute);

		// Convert to local composite coordinates
		const localPoints = route.map(p => new THREE.Vector3(
			p.x - compositeX,
			p.y - compositeY,
			3
		));

		const geom = new THREE.BufferGeometry().setFromPoints(localPoints);
		// Compute line distances for dashed material
		const line = new THREE.Line(geom, new THREE.LineDashedMaterial({
			color: 0x9a8b78, dashSize: 1, gapSize: 1, transparent: true, opacity: 0.4
		}));
		line.computeLineDistances();
		return line;
	});
</script>

{#if ghostLineGeom}
	<T is={ghostLineGeom} />
{/if}
