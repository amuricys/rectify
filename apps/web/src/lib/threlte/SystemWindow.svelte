<!--
	SystemWindow.svelte
	Per-system window: border, header, buttons, content area (phase/time series), ports.
	Uses Threlte interactivity for drag, click, hover.
	Bug #2 fix: visibility controlled by `visible` prop (binary, no opacity manipulation)
	Bug #5 fix: buttons are plain meshes with reactive material color
	Bug #6 fix: all buttons get onpointerenter/onpointerleave for hover
	Bug #8 fix: same `visible` prop hides axes completely
-->
<script lang="ts">
	import { T } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic, type SystemState } from '$lib/stores/algebraic.svelte';
	import {
		HEADER_HEIGHT, CORNER_RADIUS, DEFAULT_WINDOW_WIDTH, DEFAULT_WINDOW_HEIGHT,
		STATE_NAMES, VIEW_PRESETS, VIEW_CYCLE, MIN_WINDOW_SIZE
	} from './constants';
	import type { ViewMode, ViewAngle } from './types';
	import type { WireDragState } from './interactionTypes';
	import { createRoundedRectBorder, createHeaderShape } from './utils/geometry';
	import { createTextSprite, createButtonSprite } from './utils/sprites';
	import { getSystemColor } from './utils/colorMap';
	import { getPosition, setPosition, setWindowDims } from './positionStore';
	import PortDot from './PortDot.svelte';
	import PhaseView from './PhaseView.svelte';
	import TimeSeriesView from './TimeSeriesView.svelte';

	interface Props {
		sys: SystemState;
		wireDrag: WireDragState;
		onEditSystem: (id: string) => void;
		screenToWorld: (x: number, y: number) => THREE.Vector2;
		getObstacles: () => Array<{ x: number; y: number; w: number; h: number }>;
	}

	let { sys, wireDrag, onEditSystem, screenToWorld, getObstacles }: Props = $props();

	const color = getSystemColor(sys.id);
	const tmpl = $derived(algebraic.templateList.find(t => t.id === sys.templateId));
	const stateNames = $derived(tmpl?.state_names ?? STATE_NAMES);
	const nstates = sys.nstates;

	let viewMode: ViewMode = $state('phase');
	let viewAngle: ViewAngle = $state('ISO');
	let windowWidth = $state(DEFAULT_WINDOW_WIDTH);
	let windowHeight = $state(DEFAULT_WINDOW_HEIGHT);
	let minimized = $state(false);

	// Position: local state, initialized from backend, updated by drag
	let posX = $state(sys.position?.x ?? 0);
	let posY = $state(sys.position?.y ?? 0);

	// Keep position store in sync for wire routing and composite bounds
	$effect(() => {
		setPosition(sys.id, posX, posY);
	});

	// Keep window dims in sync
	$effect(() => {
		setWindowDims(sys.id, windowWidth, windowHeight);
	});

	// Bug #2 fix: binary visibility based on composite membership
	let composite = $derived(algebraic.compositeGroups.find(g => g.memberSystemIds.includes(sys.id)));
	let hiddenByComposite = $derived(composite != null && !composite.lookInside);

	// Clip planes for phase content (plain array, mutated in-place, read by Three.js directly)
	const clipPlanes = [
		new THREE.Plane(new THREE.Vector3(1, 0, 0), 0),
		new THREE.Plane(new THREE.Vector3(-1, 0, 0), 0),
		new THREE.Plane(new THREE.Vector3(0, 1, 0), 0),
		new THREE.Plane(new THREE.Vector3(0, -1, 0), 0),
	];

	// Update clip planes when position/size changes
	$effect(() => {
		const gx = posX;
		const gy = posY;
		const halfW = windowWidth / 2;
		const left = gx - halfW;
		const right = gx + halfW;
		const bottom = gy - windowHeight / 2 - HEADER_HEIGHT / 2;
		const top = gy + windowHeight / 2 - HEADER_HEIGHT / 2;
		clipPlanes[0].constant = -left;
		clipPlanes[1].constant = right;
		clipPlanes[2].constant = -bottom;
		clipPlanes[3].constant = top;
	});

	// Dynamic bounds for phase mapping (plain object, mutated in useTask, not reactive)
	const bounds = {
		min: new THREE.Vector3(Infinity, Infinity, Infinity),
		max: new THREE.Vector3(-Infinity, -Infinity, -Infinity),
		initialized: false
	};

	// Content rotation for 3D
	let contentRotX = $state(nstates >= 3 ? -0.4 : 0);
	let contentRotY = $state(nstates >= 3 ? 0.3 : 0);

	// Border geometry (recreated on resize via $derived)
	let borderLine = $derived(createRoundedRectBorder(
		windowWidth, windowHeight + HEADER_HEIGHT, CORNER_RADIUS,
		new THREE.LineBasicMaterial({ color, transparent: true, opacity: 0.5 })
	));

	// Header geometry
	let headerGeom = $derived(new THREE.ShapeGeometry(createHeaderShape(windowWidth, HEADER_HEIGHT, CORNER_RADIUS)));

	// Button hover states
	let closeHovered = $state(false);
	let minimizeHovered = $state(false);
	let phaseHovered = $state(false);
	let timeHovered = $state(false);
	let viewCycleHovered = $state(false);
	let editHovered = $state(false);

	// Close confirm state
	let closeConfirmActive = $state(false);
	let closeConfirmTimer: ReturnType<typeof setTimeout> | null = null;

	// Drag state
	let isDragging = $state(false);
	let dragStart = new THREE.Vector2();
	let dragStartPos = new THREE.Vector2();
	let isResizing = $state(false);
	let resizeStart = new THREE.Vector2();
	let resizeStartSize = new THREE.Vector2();
	let resizeStartPos = new THREE.Vector2();

	// Content rotation/pan drag
	let isRotating = $state(false);
	let isPanning = $state(false);
	let rotateStart = new THREE.Vector2();

	// Port layout
	let nOutputs = $derived(sys.noutputs);
	let nInputs = $derived(sys.ninputs);
	let contentTop = $derived(windowHeight / 2 - HEADER_HEIGHT);
	let contentBottom = $derived(-windowHeight / 2);
	let contentHeight = $derived(contentTop - contentBottom);

	// --- Handlers ---

	function onHeaderDown(e: any) {
		isDragging = true;
		dragStart.set(e.nativeEvent.clientX, e.nativeEvent.clientY);
		dragStartPos.set(posX, posY);
		e.stopPropagation?.();
	}

	function onCloseClick() {
		if (closeConfirmActive) {
			algebraic.removeSystem(sys.id);
			closeConfirmActive = false;
			if (closeConfirmTimer) clearTimeout(closeConfirmTimer);
		} else {
			closeConfirmActive = true;
			closeConfirmTimer = setTimeout(() => { closeConfirmActive = false; }, 2000);
		}
	}

	function onMinimizeClick() {
		minimized = !minimized;
	}

	function onPhaseClick() {
		viewMode = 'phase';
	}

	function onTimeClick() {
		viewMode = 'timeseries';
	}

	function onViewCycleClick() {
		if (nstates < 3) return;
		const idx = VIEW_CYCLE.indexOf(viewAngle);
		viewAngle = VIEW_CYCLE[(idx + 1) % VIEW_CYCLE.length];
		const preset = VIEW_PRESETS[viewAngle];
		contentRotX = preset.rx;
		contentRotY = preset.ry;
	}

	function onEditClick() {
		onEditSystem(sys.id);
	}

	function onResizeDown(e: any) {
		isResizing = true;
		resizeStart.set(e.nativeEvent.clientX, e.nativeEvent.clientY);
		resizeStartSize.set(windowWidth, windowHeight);
		resizeStartPos.set(posX, posY);
		e.stopPropagation?.();
	}

	function onContentDown(e: any) {
		if (viewMode !== 'phase') return;
		if (nstates >= 3) {
			isRotating = true;
		} else {
			isPanning = true;
		}
		rotateStart.set(e.nativeEvent.clientX, e.nativeEvent.clientY);
		e.stopPropagation?.();
	}

	// Global pointer move/up (attached to window)
	$effect(() => {
		if (!isDragging && !isResizing && !isRotating && !isPanning) return;

		const onMove = (e: PointerEvent) => {
			if (isDragging) {
				const w = screenToWorld(e.clientX, e.clientY);
				const w0 = screenToWorld(dragStart.x, dragStart.y);
				const dx = w.x - w0.x;
				const dy = w.y - w0.y;
				posX = dragStartPos.x + dx;
				posY = dragStartPos.y + dy;
			} else if (isResizing) {
				const w = screenToWorld(e.clientX, e.clientY);
				const w0 = screenToWorld(resizeStart.x, resizeStart.y);
				const dx = w.x - w0.x;
				const dy = -(w.y - w0.y);
				const newW = Math.max(MIN_WINDOW_SIZE, resizeStartSize.x + dx);
				const newH = Math.max(MIN_WINDOW_SIZE, resizeStartSize.y + dy);
				windowWidth = newW;
				windowHeight = newH;
			} else if (isRotating) {
				const dx = e.clientX - rotateStart.x;
				const dy = e.clientY - rotateStart.y;
				contentRotY += dx * 0.008;
				contentRotX += dy * 0.008;
				rotateStart.set(e.clientX, e.clientY);
			} else if (isPanning) {
				const dx = e.clientX - rotateStart.x;
				const dy = e.clientY - rotateStart.y;
				rotateStart.set(e.clientX, e.clientY);
			}
		};

		const onUp = () => {
			isDragging = false;
			isResizing = false;
			isRotating = false;
			isPanning = false;
		};

		window.addEventListener('pointermove', onMove);
		window.addEventListener('pointerup', onUp);
		return () => {
			window.removeEventListener('pointermove', onMove);
			window.removeEventListener('pointerup', onUp);
		};
	});

	// Note: borderLine and headerGeom are $derived, auto-recreated on windowWidth/windowHeight change
	// Threlte handles disposal of old objects when <T is={...}> receives a new value
</script>

<!-- Bug #2/#8 fix: binary visibility instead of opacity manipulation -->
<T.Group
	position.x={posX}
	position.y={posY}
	position.z={isDragging ? 10 : 0}
	visible={!hiddenByComposite}
>
	<!-- Border -->
	<T is={borderLine} />

	<!-- Header bar -->
	<T.Mesh
		position.y={windowHeight / 2}
		position.z={2}
		geometry={headerGeom}
		onpointerdown={onHeaderDown}
	>
		<T.MeshBasicMaterial
			color={color}
			transparent
			opacity={0.3}
			side={THREE.DoubleSide}
		/>
	</T.Mesh>

	<!-- Close button (Bug #5 fix: plain mesh, not canvas texture) -->
	<T.Mesh
		position={[-windowWidth / 2 + 3, windowHeight / 2, 3]}
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
	<!-- Close button label -->
	{@const closeLabel = createButtonSprite(closeConfirmActive ? '!' : 'x', closeConfirmActive)}
	<T
		is={closeLabel}
		position={[-windowWidth / 2 + 3, windowHeight / 2, 4]}
		scale={[4, 3.5, 1]}
	/>

	<!-- Minimize button -->
	<T.Mesh
		position={[-windowWidth / 2 + 7, windowHeight / 2, 3]}
		onpointerenter={() => minimizeHovered = true}
		onpointerleave={() => minimizeHovered = false}
		onclick={onMinimizeClick}
	>
		<T.PlaneGeometry args={[3, 3]} />
		<T.MeshBasicMaterial
			color={minimizeHovered ? 0x5a4030 : 0x2a2118}
			transparent
			opacity={minimizeHovered ? 0.8 : 0.4}
			side={THREE.DoubleSide}
		/>
	</T.Mesh>
	{@const minLabel = createButtonSprite('\u2014', false)}
	<T
		is={minLabel}
		position={[-windowWidth / 2 + 7, windowHeight / 2, 4]}
		scale={[4, 3.5, 1]}
	/>

	<!-- Header label -->
	{@const systemName = sys.templateId.split('_')[0]}
	{@const headerLabel = createTextSprite(systemName, color)}
	<T is={headerLabel} position={[-windowWidth / 2 + 16, windowHeight / 2, 3]} />

	{#if !minimized}
		<!-- Phase button -->
		<T.Mesh
			position={[windowWidth / 2 - 16, windowHeight / 2, 3]}
			onpointerenter={() => phaseHovered = true}
			onpointerleave={() => phaseHovered = false}
			onclick={onPhaseClick}
		>
			<T.PlaneGeometry args={[8, 3]} />
			<T.MeshBasicMaterial
				color={viewMode === 'phase' ? 0x8a7435 : (phaseHovered ? 0x5a4030 : 0x2a2118)}
				transparent
				opacity={0.01}
				side={THREE.DoubleSide}
			/>
		</T.Mesh>
		{@const phaseBtnLabel = createButtonSprite('Phase', viewMode === 'phase')}
		<T is={phaseBtnLabel} position={[windowWidth / 2 - 16, windowHeight / 2, 4]} />

		<!-- Time button -->
		<T.Mesh
			position={[windowWidth / 2 - 6, windowHeight / 2, 3]}
			onpointerenter={() => timeHovered = true}
			onpointerleave={() => timeHovered = false}
			onclick={onTimeClick}
		>
			<T.PlaneGeometry args={[8, 3]} />
			<T.MeshBasicMaterial
				color={viewMode === 'timeseries' ? 0x8a7435 : (timeHovered ? 0x5a4030 : 0x2a2118)}
				transparent
				opacity={0.01}
				side={THREE.DoubleSide}
			/>
		</T.Mesh>
		{@const timeBtnLabel = createButtonSprite('Time', viewMode === 'timeseries')}
		<T is={timeBtnLabel} position={[windowWidth / 2 - 6, windowHeight / 2, 4]} />

		<!-- View cycle button (3D only) -->
		{#if nstates >= 3}
			<T.Mesh
				position={[windowWidth / 2 - 26, windowHeight / 2, 3]}
				onpointerenter={() => viewCycleHovered = true}
				onpointerleave={() => viewCycleHovered = false}
				onclick={onViewCycleClick}
			>
				<T.PlaneGeometry args={[6, 3]} />
				<T.MeshBasicMaterial
					color={viewCycleHovered ? 0x5a4030 : 0x2a2118}
					transparent
					opacity={0.01}
					side={THREE.DoubleSide}
				/>
			</T.Mesh>
			{@const vcLabel = createButtonSprite(viewAngle, false)}
			<T is={vcLabel} position={[windowWidth / 2 - 26, windowHeight / 2, 4]} scale={[6, 3.5, 1]} />
		{/if}

		<!-- Edit button -->
		<T.Mesh
			position={[windowWidth / 2 - 36, windowHeight / 2, 3]}
			onpointerenter={() => editHovered = true}
			onpointerleave={() => editHovered = false}
			onclick={onEditClick}
		>
			<T.PlaneGeometry args={[6, 3]} />
			<T.MeshBasicMaterial
				color={editHovered ? 0x5a4030 : 0x2a2118}
				transparent
				opacity={0.01}
				side={THREE.DoubleSide}
			/>
		</T.Mesh>
		{@const editLabel = createButtonSprite('Edit', false)}
		<T is={editLabel} position={[windowWidth / 2 - 36, windowHeight / 2, 4]} scale={[6, 3.5, 1]} />

		<!-- Resize handle -->
		<T.Mesh
			position={[windowWidth / 2 - 2, -windowHeight / 2 - HEADER_HEIGHT + 2, 3]}
			onpointerdown={onResizeDown}
		>
			<T.PlaneGeometry args={[4, 4]} />
			<T.MeshBasicMaterial color={color} transparent opacity={0.4} side={THREE.DoubleSide} />
		</T.Mesh>

		<!-- Content area (click for rotate/pan) -->
		<T.Mesh
			position.y={-HEADER_HEIGHT / 2}
			position.z={-0.5}
			onpointerdown={onContentDown}
		>
			<T.PlaneGeometry args={[windowWidth, windowHeight]} />
			<T.MeshBasicMaterial transparent opacity={0} side={THREE.DoubleSide} />
		</T.Mesh>

		<!-- Phase View -->
		{#if viewMode === 'phase'}
			<PhaseView
				{sys}
				{color}
				{nstates}
				{stateNames}
				{windowWidth}
				{windowHeight}
				{clipPlanes}
				{bounds}
				contentRotX={contentRotX}
				contentRotY={contentRotY}
			/>
		{/if}

		<!-- Time Series View -->
		{#if viewMode === 'timeseries'}
			<TimeSeriesView
				{sys}
				{color}
				{nstates}
				{stateNames}
				{windowWidth}
				{windowHeight}
			/>
		{/if}
	{/if}

	<!-- Output ports (right edge) -->
	{#each { length: nOutputs } as _, i}
		{@const t = (i + 1) / (nOutputs + 1)}
		{@const yPos = contentBottom + t * contentHeight}
		{@const outNames = tmpl?.output_names ?? []}
		<PortDot
			systemId={sys.id}
			portIndex={i + 1}
			isOutput={true}
			portName={outNames[i] || `out${i + 1}`}
			{color}
			xOffset={windowWidth / 2}
			{yPos}
			{wireDrag}
		/>
	{/each}

	<!-- Input ports (left edge) -->
	{#each { length: nInputs } as _, i}
		{@const t = (i + 1) / (nInputs + 1)}
		{@const yPos = contentBottom + (1 - t) * contentHeight}
		{@const inNames = tmpl?.input_names ?? []}
		<PortDot
			systemId={sys.id}
			portIndex={i + 1}
			isOutput={false}
			portName={inNames[i] || `in${i + 1}`}
			{color}
			xOffset={-windowWidth / 2}
			{yPos}
			{wireDrag}
		/>
	{/each}
</T.Group>
