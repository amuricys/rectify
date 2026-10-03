<!--
	TimeSeriesView.svelte
	2D line plots with tick labels inside a system window.
-->
<script lang="ts">
	import { T, useTask } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic, type SystemState } from '$lib/stores/algebraic.svelte';
	import { HEADER_HEIGHT, TIME_WINDOW_SAMPLES, STATE_COLORS, TS_MARGIN } from './constants';
	import { createLegendSprite, createTickSprite, updateYTickSprite, updateXTickSprite } from './utils/sprites';

	interface Props {
		sys: SystemState;
		color: THREE.Color;
		nstates: number;
		stateNames: string[];
		windowWidth: number;
		windowHeight: number;
	}

	let { sys, color, nstates, stateNames, windowWidth, windowHeight }: Props = $props();

	// Pre-allocate geometries per state variable
	const lineGeometries: THREE.BufferGeometry[] = [];
	const lineColors: THREE.Color[] = [];
	for (let i = 0; i < nstates; i++) {
		const geom = new THREE.BufferGeometry();
		const pos = new Float32Array(TIME_WINDOW_SAMPLES * 3);
		geom.setAttribute('position', new THREE.BufferAttribute(pos, 3));
		geom.setDrawRange(0, 0);
		lineGeometries.push(geom);
		lineColors.push(STATE_COLORS[i % STATE_COLORS.length]);
	}

	// Legend sprites
	const legends: THREE.Sprite[] = [];
	for (let i = 0; i < nstates; i++) {
		legends.push(createLegendSprite(stateNames[i] || `v${i}`, lineColors[i]));
	}

	// Tick sprites
	const yTickSprite = createTickSprite(128, 512);
	const xTickSprite = createTickSprite(512, 64);

	// Initialize tick sprite scales
	const initYAxisH = windowHeight - TS_MARGIN.bottom - TS_MARGIN.top;
	const initYScale = Math.min(40, initYAxisH);
	yTickSprite.scale.set(initYScale * (128 / 512), initYScale, 1);
	yTickSprite.position.set(-windowWidth / 2 + TS_MARGIN.left / 2, (TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);

	const initXAxisW = windowWidth - TS_MARGIN.left - TS_MARGIN.right;
	const initXScale = Math.min(40, initXAxisW);
	xTickSprite.scale.set(initXScale, initXScale * (64 / 512), 1);
	xTickSprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2, -windowHeight / 2 + TS_MARGIN.bottom / 2 - 1, 2);

	// Axis lines geometry
	const tsAxisGeometry = new THREE.BufferGeometry();
	const tsAxisPoints = [
		-windowWidth / 2 + TS_MARGIN.left, -windowHeight / 2 + TS_MARGIN.bottom, 0,
		windowWidth / 2 - TS_MARGIN.right, -windowHeight / 2 + TS_MARGIN.bottom, 0,
		-windowWidth / 2 + TS_MARGIN.left, -windowHeight / 2 + TS_MARGIN.bottom, 0,
		-windowWidth / 2 + TS_MARGIN.left, windowHeight / 2 - TS_MARGIN.top, 0,
	];
	tsAxisGeometry.setAttribute('position', new THREE.Float32BufferAttribute(tsAxisPoints, 3));

	// Persistent Y range (monotonically expanding)
	let tsYMin = Infinity;
	let tsYMax = -Infinity;

	useTask(() => {
		const history = algebraic.getHistory(sys.id);
		const timeHistory = algebraic.getTimeHistory(sys.id);
		if (history.length === 0) return;

		const startIdx = Math.max(0, history.length - TIME_WINDOW_SAMPLES);
		const visibleHistory = history.slice(startIdx);
		const visibleTimes = timeHistory.slice(startIdx);
		const len = visibleHistory.length;
		if (len === 0) return;

		const startTime = visibleTimes.length > 0 ? visibleTimes[0] : 0;
		const endTime = visibleTimes.length > 0 ? visibleTimes[visibleTimes.length - 1] : 0;

		let frameMin = Infinity, frameMax = -Infinity;
		for (const state of visibleHistory) {
			for (let v = 0; v < nstates && v < state.length; v++) {
				frameMin = Math.min(frameMin, state[v]);
				frameMax = Math.max(frameMax, state[v]);
			}
		}
		if (frameMin < tsYMin) tsYMin = frameMin;
		if (frameMax > tsYMax) tsYMax = frameMax;

		const winW = windowWidth;
		const winH = windowHeight;
		const xMin = -winW / 2 + TS_MARGIN.left;
		const xMax = winW / 2 - TS_MARGIN.right;
		const yMin = -winH / 2 + TS_MARGIN.bottom;
		const yMax = winH / 2 - TS_MARGIN.top;
		const xRange = xMax - xMin;
		const yRange = yMax - yMin;
		const dataRange = tsYMax - tsYMin;

		for (let v = 0; v < nstates; v++) {
			const geom = lineGeometries[v];
			if (!geom) continue;
			const posArr = geom.attributes.position.array as Float32Array;
			for (let i = 0; i < len && i < TIME_WINDOW_SAMPLES; i++) {
				const state = visibleHistory[i];
				const value = v < state.length ? state[v] : 0;
				const x = xMin + (i / (TIME_WINDOW_SAMPLES - 1)) * xRange;
				const y = yMin + ((value - tsYMin) / (dataRange > 0.001 ? dataRange : 1)) * yRange * 0.9 + yRange * 0.05;
				posArr[i * 3] = x;
				posArr[i * 3 + 1] = Number.isFinite(y) ? y : yMin;
				posArr[i * 3 + 2] = 0;
			}
			geom.attributes.position.needsUpdate = true;
			geom.setDrawRange(0, len);
		}

		updateYTickSprite(yTickSprite, tsYMin, tsYMax, winW, winH, TS_MARGIN);
		updateXTickSprite(xTickSprite, startTime, endTime, winW, winH, TS_MARGIN);
	});
</script>

<T.Group position.y={-HEADER_HEIGHT / 2}>
	<!-- State variable lines -->
	{#each lineGeometries as geom, i}
		<T.Line geometry={geom}>
			<T.LineBasicMaterial color={lineColors[i]} transparent opacity={0.9} />
		</T.Line>
	{/each}

	<!-- Legends -->
	{#each legends as legend, i}
		<T
			is={legend}
			position={[windowWidth / 2 - 5, windowHeight / 2 - 4 - i * 3, 1]}
		/>
	{/each}

	<!-- Axis lines -->
	<T.LineSegments geometry={tsAxisGeometry}>
		<T.LineBasicMaterial color={0x3a2e24} transparent opacity={0.5} />
	</T.LineSegments>

	<!-- Tick sprites -->
	<T is={yTickSprite} />
	<T is={xTickSprite} />
</T.Group>
