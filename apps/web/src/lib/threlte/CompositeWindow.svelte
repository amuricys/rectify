<!--
	CompositeWindow.svelte
	Composite group overlay: dashed border, header, buttons, skeleton/look-inside.
	Bug #1 fix: header z=2, skeleton z=1
	Bug #3 fix: ghost wires use A*-routed paths
	Bug #7 fix: skeleton mirrors actual member positions proportionally
-->
<script lang="ts">
	import { T, useTask } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic, type CompositeGroup, type FreeStateInfo } from '$lib/stores/algebraic.svelte';
	import {
		HEADER_HEIGHT, CORNER_RADIUS, TS_MARGIN, TIME_WINDOW_SAMPLES, STATE_COLORS
	} from './constants';
	import { createRoundedRectBorder, createHeaderShape } from './utils/geometry';
	import {
		createTextSprite, createButtonSprite, createLegendSprite,
		createTickSprite, createTooltipSprite, updateYTickSprite, updateXTickSprite
	} from './utils/sprites';
	import { getSystemColor } from './utils/colorMap';
	import { computeWireRoute } from './utils/wireRouter';
	import { smoothCorners } from './utils/wireSmoothing';
	import { getPosition, getWindowDims } from './positionStore';
	import SkeletonView from './SkeletonView.svelte';
	import GhostWire from './GhostWire.svelte';

	interface Props {
		group: CompositeGroup;
		getObstacles: () => Array<{ x: number; y: number; w: number; h: number }>;
		onHeaderDown: (e: PointerEvent) => void;
	}

	let { group, getObstacles, onHeaderDown }: Props = $props();

	const compositeColor = new THREE.Color(0x4d3d2e);

	// Compute bounds from member positions (using local position store)
	let memberBounds = $derived.by(() => {
		let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
		for (const memberId of group.memberSystemIds) {
			const pos = getPosition(memberId);
			const dims = getWindowDims(memberId);
			if (pos) {
				const gx = pos.x;
				const gy = pos.y;
				const halfW = dims.width / 2;
				const halfH = (dims.height + HEADER_HEIGHT) / 2;
				minX = Math.min(minX, gx - halfW);
				maxX = Math.max(maxX, gx + halfW);
				minY = Math.min(minY, gy - halfH);
				maxY = Math.max(maxY, gy + halfH);
			}
		}
		const padding = 8;
		return {
			minX: minX - padding, maxX: maxX + padding,
			minY: minY - padding, maxY: maxY + padding
		};
	});

	let winWidth = $derived(memberBounds.maxX - memberBounds.minX);
	let winHeight = $derived(memberBounds.maxY - memberBounds.minY - HEADER_HEIGHT);
	let cx = $derived((memberBounds.minX + memberBounds.maxX) / 2);
	let cy = $derived((memberBounds.minY + memberBounds.maxY) / 2);

	// Border (dashed, rounded)
	let borderLine = $derived(createRoundedRectBorder(
		winWidth, winHeight + HEADER_HEIGHT, CORNER_RADIUS,
		new THREE.LineDashedMaterial({ color: 0x4d3d2e, dashSize: 2, gapSize: 1, transparent: true, opacity: 0.6 })
	));

	// Header
	let headerGeom = $derived(new THREE.ShapeGeometry(createHeaderShape(winWidth, HEADER_HEIGHT, CORNER_RADIUS)));

	// Button hovers
	let closeHovered = $state(false);
	let minimizeHovered = $state(false);
	let lookInsideHovered = $state(false);

	// Close confirm
	let closeConfirmActive = $state(false);
	let closeConfirmTimer: ReturnType<typeof setTimeout> | null = null;

	function onCloseClick() {
		if (closeConfirmActive) {
			for (const memberId of group.memberSystemIds) {
				algebraic.removeSystem(memberId);
			}
			closeConfirmActive = false;
			if (closeConfirmTimer) clearTimeout(closeConfirmTimer);
		} else {
			closeConfirmActive = true;
			closeConfirmTimer = setTimeout(() => { closeConfirmActive = false; }, 2000);
		}
	}

	function onLookInsideClick() {
		group.lookInside = !group.lookInside;
	}

	// Free states for time series
	let freeStates = $derived(algebraic.getCompositeFreeStateInfo(group.id));

	// Time series geometries
	const tsGeometries: THREE.BufferGeometry[] = [];
	const tsColors: THREE.Color[] = [];

	$effect(() => {
		// Rebuild time series geometries when free states change
		const nFree = freeStates.length;
		while (tsGeometries.length < nFree) {
			const geom = new THREE.BufferGeometry();
			const pos = new Float32Array(TIME_WINDOW_SAMPLES * 3);
			geom.setAttribute('position', new THREE.BufferAttribute(pos, 3));
			geom.setDrawRange(0, 0);
			tsGeometries.push(geom);
			tsColors.push(STATE_COLORS[tsGeometries.length - 1 % STATE_COLORS.length]);
		}
	});

	// Tick sprites for composite time series
	const yTickSprite = createTickSprite(128, 512);
	const xTickSprite = createTickSprite(512, 64);

	// Axis lines
	let tsAxisGeom = $derived.by(() => {
		const geom = new THREE.BufferGeometry();
		const pts = [
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			winWidth / 2 - TS_MARGIN.right, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, winHeight / 2 - TS_MARGIN.top, 0,
		];
		geom.setAttribute('position', new THREE.Float32BufferAttribute(pts, 3));
		return geom;
	});

	let tsYMin = Infinity;
	let tsYMax = -Infinity;

	// Ghost wires: unconnected member input ports
	let ghostWireData = $derived.by(() => {
		if (!group.lookInside) return [];
		const memberSet = new Set(group.memberSystemIds);
		const connectedInputs = new Set<string>();
		for (const wire of algebraic.wireList) {
			if (memberSet.has(wire.fromSystem) && memberSet.has(wire.toSystem)) {
				connectedInputs.add(`${wire.toSystem}:${wire.toPort}`);
			}
		}
		const wires: Array<{ memberId: string; portIndex: number }> = [];
		for (const memberId of group.memberSystemIds) {
			const sys = algebraic.systemList.find(s => s.id === memberId);
			if (!sys) continue;
			for (let p = 1; p <= sys.nstates; p++) {
				if (!connectedInputs.has(`${memberId}:${p}`)) {
					wires.push({ memberId, portIndex: p });
				}
			}
		}
		return wires;
	});

	useTask(() => {
		if (group.lookInside) return;

		const history = algebraic.getCompositeHistory(group.id);
		const timeHistory = algebraic.getCompositeTimeHistory(group.id);
		if (history.length === 0) return;

		const startIdx = Math.max(0, history.length - TIME_WINDOW_SAMPLES);
		const visibleHistory = history.slice(startIdx);
		const visibleTimes = timeHistory.slice(startIdx);
		const len = visibleHistory.length;
		if (len === 0) return;

		const startTime = visibleTimes.length > 0 ? visibleTimes[0] : 0;
		const endTime = visibleTimes.length > 0 ? visibleTimes[visibleTimes.length - 1] : 0;

		let frameMin = Infinity, frameMax = -Infinity;
		const nFree = freeStates.length;
		for (const state of visibleHistory) {
			for (let v = 0; v < nFree && v < state.length; v++) {
				if (Number.isFinite(state[v])) {
					frameMin = Math.min(frameMin, state[v]);
					frameMax = Math.max(frameMax, state[v]);
				}
			}
		}
		if (frameMin < tsYMin) tsYMin = frameMin;
		if (frameMax > tsYMax) tsYMax = frameMax;

		const wW = winWidth, wH = winHeight;
		const xMin = -wW / 2 + TS_MARGIN.left;
		const xMax = wW / 2 - TS_MARGIN.right;
		const yMin = -wH / 2 + TS_MARGIN.bottom;
		const yMax = wH / 2 - TS_MARGIN.top;
		const xRange = xMax - xMin;
		const yRange = yMax - yMin;
		const dataRange = tsYMax - tsYMin;

		for (let v = 0; v < nFree && v < tsGeometries.length; v++) {
			const geom = tsGeometries[v];
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

		updateYTickSprite(yTickSprite, tsYMin, tsYMax, wW, wH, TS_MARGIN);
		updateXTickSprite(xTickSprite, startTime, endTime, wW, wH, TS_MARGIN);
	});
</script>

<T.Group position.x={cx} position.y={cy} position.z={-1}>
	<!-- Dashed border -->
	<T is={borderLine} />

	<!-- Header bar (Bug #1 fix: z=2 so always on top of skeleton at z=1) -->
	<T.Mesh
		position.y={winHeight / 2}
		position.z={2}
		geometry={headerGeom}
		onpointerdown={(e: any) => onHeaderDown(e.nativeEvent)}
	>
		<T.MeshBasicMaterial color={0x3a2e24} transparent opacity={0.3} side={THREE.DoubleSide} />
	</T.Mesh>

	<!-- Header label -->
	{@const headerLabel = createTextSprite(group.name, compositeColor)}
	<T is={headerLabel} position={[-winWidth / 2 + 16, winHeight / 2, 3]} />

	<!-- Close button -->
	<T.Mesh
		position={[-winWidth / 2 + 3, winHeight / 2, 3]}
		onpointerenter={() => closeHovered = true}
		onpointerleave={() => closeHovered = false}
		onclick={onCloseClick}
	>
		<T.PlaneGeometry args={[3, 3]} />
		<T.MeshBasicMaterial
			color={closeConfirmActive ? 0xb85c4a : (closeHovered ? 0x5a4030 : 0x2a2118)}
			transparent
			opacity={closeHovered || closeConfirmActive ? 0.8 : 0.4}
			side={THREE.DoubleSide}
		/>
	</T.Mesh>
	{@const closeLbl = createButtonSprite(closeConfirmActive ? '!' : 'x', closeConfirmActive)}
	<T is={closeLbl} position={[-winWidth / 2 + 3, winHeight / 2, 4]} scale={[4, 3.5, 1]} />

	<!-- Minimize button -->
	<T.Mesh
		position={[-winWidth / 2 + 7, winHeight / 2, 3]}
		onpointerenter={() => minimizeHovered = true}
		onpointerleave={() => minimizeHovered = false}
	>
		<T.PlaneGeometry args={[3, 3]} />
		<T.MeshBasicMaterial
			color={minimizeHovered ? 0x5a4030 : 0x2a2118}
			transparent
			opacity={minimizeHovered ? 0.8 : 0.4}
			side={THREE.DoubleSide}
		/>
	</T.Mesh>
	{@const minLbl = createButtonSprite('\u2014', false)}
	<T is={minLbl} position={[-winWidth / 2 + 7, winHeight / 2, 4]} scale={[4, 3.5, 1]} />

	<!-- Look-inside button (Bug #6 fix: has hover) -->
	<T.Mesh
		position={[winWidth / 2 - 6, winHeight / 2, 3]}
		onpointerenter={() => lookInsideHovered = true}
		onpointerleave={() => lookInsideHovered = false}
		onclick={onLookInsideClick}
	>
		<T.PlaneGeometry args={[6, 3]} />
		<T.MeshBasicMaterial
			color={group.lookInside ? 0x8a7435 : (lookInsideHovered ? 0x5a4030 : 0x2a2118)}
			transparent
			opacity={lookInsideHovered || group.lookInside ? 0.8 : 0.4}
			side={THREE.DoubleSide}
		/>
	</T.Mesh>
	{@const lookLbl = createButtonSprite('\u25C9', group.lookInside)}
	<T is={lookLbl} position={[winWidth / 2 - 6, winHeight / 2, 4]} scale={[4, 3.5, 1]} />

	<!-- Skeleton view (shown when NOT look-inside, Bug #7 fix) -->
	{#if !group.lookInside}
		<SkeletonView
			{group}
			compositeWidth={winWidth}
			compositeHeight={winHeight}
			compositeX={cx}
			compositeY={cy}
		/>

		<!-- Time series for composite free states -->
		<T.Group position.y={-HEADER_HEIGHT / 2}>
			{#each tsGeometries.slice(0, freeStates.length) as geom, i}
				<T.Line geometry={geom}>
					<T.LineBasicMaterial color={STATE_COLORS[i % STATE_COLORS.length]} transparent opacity={0.9} />
				</T.Line>
			{/each}

			{#each freeStates as fs, i}
				{@const legend = createLegendSprite(fs.name, STATE_COLORS[i % STATE_COLORS.length])}
				<T is={legend} position={[winWidth / 2 - 5, winHeight / 2 - 4 - i * 3, 1]} />
			{/each}

			<T.LineSegments geometry={tsAxisGeom}>
				<T.LineBasicMaterial color={0x3a2e24} transparent opacity={0.5} />
			</T.LineSegments>
			<T is={yTickSprite} />
			<T is={xTickSprite} />
		</T.Group>
	{/if}

	<!-- Ghost wires (shown when look-inside, Bug #3 fix: A*-routed) -->
	{#if group.lookInside}
		{#each ghostWireData as gw}
			<GhostWire
				memberId={gw.memberId}
				portIndex={gw.portIndex}
				compositeX={cx}
				compositeY={cy}
				compositeWidth={winWidth}
				compositeHeight={winHeight}
				{getObstacles}
			/>
		{/each}
	{/if}
</T.Group>
