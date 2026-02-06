<!--
  AlgebraicRenderer.svelte

  Three.js renderer for multiple open dynamical systems.
  Each system is rendered in its own draggable "window" container.
  Windows are arranged in 2D (like OS windows) with 3D content inside.
-->
<script lang="ts">
	import { onMount } from 'svelte';
	import * as THREE from 'three';
	import { algebraic, type SystemState, type WireState, type CompositeGroup, type FreeStateInfo } from '$lib/stores/algebraic.svelte';

	interface Props {
		width: number;
		height: number;
	}

	let { width, height }: Props = $props();

	let canvasEl: HTMLCanvasElement;
	let renderer: THREE.WebGLRenderer;
	let scene: THREE.Scene;
	let camera: THREE.OrthographicCamera;

	// Window dimensions (in world units)
	const WINDOW_WIDTH = 40;
	const WINDOW_HEIGHT = 40;
	const HEADER_HEIGHT = 4;

	type ViewMode = 'phase' | 'timeseries';
	type ViewAngle = 'ISO' | 'XY' | 'XZ' | 'YZ';

	// Per-system window visualization
	interface SystemWindow {
		group: THREE.Group;
		header: THREE.Mesh;
		headerLabel: THREE.Sprite;
		// Mode buttons
		phaseButton: THREE.Mesh;
		timeButton: THREE.Mesh;
		phaseButtonLabel: THREE.Sprite;
		timeButtonLabel: THREE.Sprite;
		// View cycle button (3D only)
		viewCycleButton: THREE.Mesh;
		viewCycleLabel: THREE.Sprite;
		viewAngle: ViewAngle;
		// Resize handle
		resizeHandle: THREE.Mesh;
		// Content
		contentGroup: THREE.Group;
		border: THREE.Line;
		// Phase space view
		trailGeometry: THREE.BufferGeometry;
		trail: THREE.Line;
		point: THREE.Mesh;
		axesGroup: THREE.Group; // 3D axes for phase view
		// Phase tick marks
		phaseTickGroup: THREE.Group;
		lastTickBounds: { min: THREE.Vector3; max: THREE.Vector3 } | null;
		// Time series view
		timeSeriesGroup: THREE.Group;
		timeSeriesLines: THREE.Line[];
		timeSeriesGeometries: THREE.BufferGeometry[];
		legendSprites: THREE.Sprite[];
		// Axes / ticks
		tsAxisLines: THREE.LineSegments;
		yTickSprite: THREE.Sprite;
		xTickSprite: THREE.Sprite;
		// Clipping
		clipPlanes: THREE.Plane[];
		// Shared
		color: THREE.Color;
		viewMode: ViewMode;
		nstates: number;
		windowWidth: number;
		windowHeight: number;
		bounds: {
			min: THREE.Vector3;
			max: THREE.Vector3;
			initialized: boolean;
		};
		// Time series persistent Y range (monotonically expanding)
		tsYMin: number;
		tsYMax: number;
		// Port dots (B3)
		outputPorts: PortDot[];
		inputPorts: PortDot[];
		portDividerLine: THREE.Line | null;
		// Edit button
		editButton: THREE.Mesh;
		editButtonLabel: THREE.Sprite;
		// Close & minimize buttons
		closeButton: THREE.Mesh;
		closeButtonLabel: THREE.Sprite;
		minimizeButton: THREE.Mesh;
		minimizeButtonLabel: THREE.Sprite;
		// Minimized state
		minimized: boolean;
		paramsExpanded: boolean;
		stateValueSprites: THREE.Sprite[];
		paramValueSprites: THREE.Sprite[];
		caretButton: THREE.Mesh | null;
		caretLabel: THREE.Sprite | null;
		savedWindowHeight: number;
		lastMinimizedUpdate: number;
	}

	// B3: Port dot visual
	interface PortDot {
		mesh: THREE.Mesh;
		ring: THREE.LineLoop;
		hitArea: THREE.Mesh;
		label: THREE.Sprite;
		systemId: string;
		portIndex: number; // 1-indexed
		isOutput: boolean;
	}

	// B4: Wire visual
	interface WireVisual {
		wireId: string;
		line: THREE.Line;
		geometry: THREE.BufferGeometry;
		fromSystemId: string;
		fromPort: number;
		toSystemId: string;
		toPort: number;
		valueLine: THREE.Line;
		valueGeometry: THREE.BufferGeometry;
	}

	// Composite window visualization
	interface CompositeWindow {
		id: string;
		group: THREE.Group;
		border: THREE.Line;
		header: THREE.Mesh;
		headerLabel: THREE.Sprite;
		closeButton: THREE.Mesh;
		closeButtonLabel: THREE.Sprite;
		minimizeButton: THREE.Mesh;
		minimizeButtonLabel: THREE.Sprite;
		lookInsideButton: THREE.Mesh;
		lookInsideLabel: THREE.Sprite;
		// Outer visualization
		contentGroup: THREE.Group;
		timeSeriesGroup: THREE.Group;
		timeSeriesLines: THREE.Line[];
		timeSeriesGeometries: THREE.BufferGeometry[];
		tsAxisLines: THREE.LineSegments;
		yTickSprite: THREE.Sprite;
		xTickSprite: THREE.Sprite;
		legendSprites: THREE.Sprite[];
		// Layout
		windowWidth: number;
		windowHeight: number;
		lookInside: boolean;
		minimized: boolean;
		combinedNstates: number;
		memberWindowIds: string[];
		color: THREE.Color;
		// Time series range
		tsYMin: number;
		tsYMax: number;
		// Free state tracking
		freeStates: FreeStateInfo[];
		// Ghost wires (dashed lines from border to inner unconnected ports)
		ghostWires: THREE.Line[];
		// Cached name for detecting changes
		cachedName: string;
		// Skeleton view (shown when NOT look-inside)
		skeletonGroup: THREE.Group | null;
		// Composite ports on outer border
		compositePorts: CompositePort[];
	}

	// Composite port on the outer box border
	interface CompositePort {
		memberSystemId: string;
		portIndex: number;
		isOutput: boolean;
		mesh: THREE.Mesh;
		ring: THREE.LineLoop;
		hitArea: THREE.Mesh;
		label: THREE.Sprite;
	}

	// Colors for individual state variables in time series view
	const STATE_COLORS = [
		new THREE.Color(0xd4785a), // warm-red
		new THREE.Color(0xc9a84c), // gold
		new THREE.Color(0x8a9b68), // sage
		new THREE.Color(0xd4956b), // copper
		new THREE.Color(0xb85c4a), // brick
		new THREE.Color(0xc4b078), // pale-gold
	];

	const STATE_NAMES = ['x', 'y', 'z', 'w', 'v', 'u'];
	const TIME_WINDOW_SAMPLES = 500;
	const MIN_WINDOW_SIZE = 25;
	const DEFAULT_WINDOW_WIDTH = 46;
	const DEFAULT_WINDOW_HEIGHT = 46;
	const TS_MARGIN = { left: 10, right: 12, bottom: 8, top: 4 };
	const NUM_TICKS = 5;
	const PHASE_HALF_EXTENT = 14; // local units for phase mapping
	const PHASE_BOUNDS_HISTORY = 500;
	const CORNER_RADIUS = 2.5; // world units for rounded corners
	const CANVAS_FONT = "'CMU Serif', serif";

	const systemWindows = new Map<string, SystemWindow>();
	const compositeWindows = new Map<string, CompositeWindow>();
	const MAX_POINTS = 2000;

	// Drag state
	let isDragging = false;
	let isResizing = false;
	let draggedWindow: SystemWindow | null = null;
	let dragStart = new THREE.Vector2();
	let windowStartPos = new THREE.Vector2();
	let windowStartSize = new THREE.Vector2();
	let isRotating = false;
	let isPanning = false;
	let rotateStart = new THREE.Vector2();

	// Edit system state
	let editingSystemId: string | null = $state(null);

	// Close confirm state
	let closeConfirmId: string | null = null;
	let closeConfirmTimer: ReturnType<typeof setTimeout> | null = null;
	let closeConfirmTooltip: THREE.Sprite | null = null;
	let closeConfirmWindow: SystemWindow | null = null;
	let closeConfirmGroup: THREE.Group | null = null; // For composite close confirm

	// B6: Wire drag state
	let isWiring = false;
	let wireSourcePort: PortDot | null = null;
	let pendingRewire: WireState | null = null;
	let wireDragLine: THREE.Line | null = null;
	let lastWireDragTime = 0;
	const WIRE_DRAG_MAX_VERTS = 200;

	// B4: Wire visuals
	const wireVisuals = new Map<string, WireVisual>();

	// Raycaster for mouse interaction
	const raycaster = new THREE.Raycaster();
	const mouse = new THREE.Vector2();

	// Color palette for systems
	const COLORS = [
		new THREE.Color(0xc9a84c), // gold
		new THREE.Color(0xb85c4a), // brick
		new THREE.Color(0x8a9b68), // sage
		new THREE.Color(0xd4956b), // copper
		new THREE.Color(0x9b7a5c), // leather
		new THREE.Color(0xc4b078), // pale-gold
		new THREE.Color(0xa86e5a), // amber
		new THREE.Color(0x7a9b8a), // warm-teal
	];
	let colorIndex = 0;

	let animationId: number;
	let cameraZoom = 1;

	function initScene() {
		renderer = new THREE.WebGLRenderer({
			canvas: canvasEl,
			antialias: true,
			alpha: true
		});
		renderer.setPixelRatio(window.devicePixelRatio);
		renderer.setSize(width, height);
		renderer.setClearColor(0x0f0b08, 1);
		renderer.localClippingEnabled = true;

		scene = new THREE.Scene();

		// Orthographic camera for 2D window layout
		const aspect = width / height;
		const viewSize = 100;
		camera = new THREE.OrthographicCamera(
			-viewSize * aspect,
			viewSize * aspect,
			viewSize,
			-viewSize,
			0.1,
			1000
		);
		camera.position.set(0, 0, 200);
		camera.lookAt(0, 0, 0);

		// Ambient light
		const ambientLight = new THREE.AmbientLight(0x605040);
		scene.add(ambientLight);

		// Directional light
		const dirLight = new THREE.DirectionalLight(0xffffff, 0.6);
		dirLight.position.set(0, 0, 100);
		scene.add(dirLight);

		canvasEl.addEventListener('mousedown', onMouseDown);
		canvasEl.addEventListener('mousemove', onMouseMove);
		canvasEl.addEventListener('mouseup', onMouseUp);
		canvasEl.addEventListener('mouseleave', onMouseUp);
		canvasEl.addEventListener('wheel', onWheel);
	}

	// Phase 2: Rounded rect shape helper
	function createRoundedRectShape(w: number, h: number, r: number): THREE.Shape {
		const shape = new THREE.Shape();
		const x = -w / 2, y = -h / 2;
		shape.moveTo(x + r, y);
		shape.lineTo(x + w - r, y);
		shape.absarc(x + w - r, y + r, r, -Math.PI / 2, 0, false);
		shape.lineTo(x + w, y + h - r);
		shape.absarc(x + w - r, y + h - r, r, 0, Math.PI / 2, false);
		shape.lineTo(x + r, y + h);
		shape.absarc(x + r, y + h - r, r, Math.PI / 2, Math.PI, false);
		shape.lineTo(x, y + r);
		shape.absarc(x + r, y + r, r, Math.PI, (3 * Math.PI) / 2, false);
		return shape;
	}

	function createRoundedRectBorder(
		w: number,
		h: number,
		r: number,
		material: THREE.LineBasicMaterial | THREE.LineDashedMaterial
	): THREE.Line {
		const shape = createRoundedRectShape(w, h, r);
		const points = shape.getPoints(32);
		const geom = new THREE.BufferGeometry().setFromPoints(
			points.map((p) => new THREE.Vector3(p.x, p.y, 0))
		);
		const line = new THREE.Line(geom, material);
		if (material instanceof THREE.LineDashedMaterial) {
			line.computeLineDistances();
		}
		return line;
	}

	// Phase 2: Rounded-top header shape (flat bottom, rounded top corners)
	function createHeaderShape(width: number, headerHeight: number, radius: number): THREE.Shape {
		const shape = new THREE.Shape();
		const x = -width / 2, y = -headerHeight / 2;
		// Start at bottom-left, go right (flat bottom)
		shape.moveTo(x, y);
		shape.lineTo(x + width, y);
		// Right side up, rounded top-right
		shape.lineTo(x + width, y + headerHeight - radius);
		shape.absarc(x + width - radius, y + headerHeight - radius, radius, 0, Math.PI / 2, false);
		// Top edge, right to left
		shape.lineTo(x + radius, y + headerHeight);
		// Rounded top-left
		shape.absarc(x + radius, y + headerHeight - radius, radius, Math.PI / 2, Math.PI, false);
		// Left side down
		shape.lineTo(x, y);
		return shape;
	}

	// Phase 4: Smooth wire path with CatmullRom curve
	function smoothWirePath(points: THREE.Vector3[]): THREE.Vector3[] {
		if (points.length <= 2) return points;
		const curve = new THREE.CatmullRomCurve3(points, false, 'catmullrom', 0.3);
		const numSamples = Math.max(points.length * 8, 32);
		return curve.getPoints(numSamples);
	}

	function createTextSprite(text: string, color: THREE.Color): THREE.Sprite {
		const canvas = document.createElement('canvas');
		const context = canvas.getContext('2d')!;
		canvas.width = 256;
		canvas.height = 64;

		context.fillStyle = 'transparent';
		context.fillRect(0, 0, canvas.width, canvas.height);

		context.font = `700 28px ${CANVAS_FONT}`;
		context.fillStyle = `rgb(${Math.floor(color.r * 255)}, ${Math.floor(color.g * 255)}, ${Math.floor(color.b * 255)})`;
		context.textAlign = 'left';
		context.textBaseline = 'middle';
		context.fillText(text, 10, canvas.height / 2);

		const texture = new THREE.CanvasTexture(canvas);
		texture.needsUpdate = true;

		const material = new THREE.SpriteMaterial({
			map: texture,
			transparent: true
		});

		const sprite = new THREE.Sprite(material);
		sprite.scale.set(20, 5, 1);

		return sprite;
	}

	function createButtonSprite(text: string, active: boolean): THREE.Sprite {
		const canvas = document.createElement('canvas');
		const context = canvas.getContext('2d')!;
		canvas.width = 96;
		canvas.height = 40;

		context.fillStyle = active ? '#8a7435' : '#2a2118';
		context.roundRect(0, 0, canvas.width, canvas.height, 6);
		context.fill();

		if (active) {
			context.strokeStyle = '#c9a84c';
			context.lineWidth = 2;
			context.stroke();
		}

		context.font = `700 18px ${CANVAS_FONT}`;
		context.fillStyle = active ? '#ffffff' : '#9a8b78';
		context.textAlign = 'center';
		context.textBaseline = 'middle';
		context.fillText(text, canvas.width / 2, canvas.height / 2);

		const texture = new THREE.CanvasTexture(canvas);
		const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
		const sprite = new THREE.Sprite(material);
		sprite.scale.set(8, 3.5, 1);
		return sprite;
	}

	function createLegendSprite(name: string, color: THREE.Color): THREE.Sprite {
		const canvas = document.createElement('canvas');
		const context = canvas.getContext('2d')!;
		canvas.width = 64;
		canvas.height = 32;

		context.fillStyle = `rgb(${Math.floor(color.r * 255)}, ${Math.floor(color.g * 255)}, ${Math.floor(color.b * 255)})`;
		context.fillRect(4, 10, 12, 12);

		context.font = `italic 16px ${CANVAS_FONT}`;
		context.fillStyle = '#9a8b78';
		context.textAlign = 'left';
		context.textBaseline = 'middle';
		context.fillText(name, 20, 16);

		const texture = new THREE.CanvasTexture(canvas);
		const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
		const sprite = new THREE.Sprite(material);
		sprite.scale.set(6, 3, 1);
		return sprite;
	}

	function createTooltipSprite(text: string, color: string): THREE.Sprite {
		const canvas = document.createElement('canvas');
		const ctx = canvas.getContext('2d')!;
		canvas.width = 256;
		canvas.height = 32;
		ctx.font = `400 14px ${CANVAS_FONT}`;
		ctx.fillStyle = color;
		ctx.textAlign = 'left';
		ctx.textBaseline = 'middle';
		ctx.fillText(text, 4, canvas.height / 2);
		const texture = new THREE.CanvasTexture(canvas);
		const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
		const sprite = new THREE.Sprite(material);
		sprite.scale.set(22, 3, 1);
		return sprite;
	}

	function resetCloseConfirm() {
		if (closeConfirmTimer) {
			clearTimeout(closeConfirmTimer);
			closeConfirmTimer = null;
		}
		if (closeConfirmTooltip) {
			const parentGroup = closeConfirmWindow?.group ?? closeConfirmGroup;
			if (parentGroup) {
				parentGroup.remove(closeConfirmTooltip);
			}
			(closeConfirmTooltip.material as THREE.SpriteMaterial).map?.dispose();
			(closeConfirmTooltip.material as THREE.Material).dispose();
			closeConfirmTooltip = null;
		}
		closeConfirmId = null;
		closeConfirmWindow = null;
		closeConfirmGroup = null;
	}

	function formatTickValue(value: number): string {
		if (value === 0) return '0';
		const abs = Math.abs(value);
		if (abs >= 1000) return value.toExponential(1);
		if (abs >= 100) return value.toFixed(0);
		if (abs >= 10) return value.toFixed(1);
		if (abs >= 1) return value.toFixed(1);
		if (abs >= 0.01) return value.toFixed(2);
		return value.toExponential(1);
	}

	function createTickSprite(canvasW: number, canvasH: number): THREE.Sprite {
		const canvas = document.createElement('canvas');
		canvas.width = canvasW;
		canvas.height = canvasH;
		const texture = new THREE.CanvasTexture(canvas);
		const material = new THREE.SpriteMaterial({ map: texture, transparent: true, depthTest: false });
		return new THREE.Sprite(material);
	}

	function updateYTickSprite(sprite: THREE.Sprite, minVal: number, maxVal: number, winW: number, winH: number) {
		const material = sprite.material as THREE.SpriteMaterial;
		const texture = material.map!;
		const canvas = texture.image as HTMLCanvasElement;
		const ctx = canvas.getContext('2d')!;
		ctx.clearRect(0, 0, canvas.width, canvas.height);

		ctx.font = `400 20px ${CANVAS_FONT}`;
		ctx.fillStyle = '#9a8b78';
		ctx.textAlign = 'right';

		const pad = 16;
		for (let i = 0; i <= NUM_TICKS; i++) {
			const t = i / NUM_TICKS;
			const y = canvas.height - pad - t * (canvas.height - 2 * pad);
			const value = minVal + t * (maxVal - minVal);

			ctx.fillRect(canvas.width - 6, y - 1, 6, 2);
			ctx.textBaseline = 'middle';
			ctx.fillText(formatTickValue(value), canvas.width - 10, y);
		}

		texture.needsUpdate = true;
		// Phase 3: Fixed-size tick labels — clamp scale
		const yAxisHeight = winH - TS_MARGIN.bottom - TS_MARGIN.top;
		const yFixedScale = 40;
		const yScale = Math.min(yFixedScale, yAxisHeight);
		sprite.scale.set(yScale * (128 / 512), yScale, 1);
		sprite.position.set(-winW / 2 + TS_MARGIN.left / 2,
			(TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);
	}

	function updateXTickSprite(sprite: THREE.Sprite, startTime: number, endTime: number, winW: number, winH: number) {
		const material = sprite.material as THREE.SpriteMaterial;
		const texture = material.map!;
		const canvas = texture.image as HTMLCanvasElement;
		const ctx = canvas.getContext('2d')!;
		ctx.clearRect(0, 0, canvas.width, canvas.height);

		ctx.font = `400 16px ${CANVAS_FONT}`;
		ctx.fillStyle = '#9a8b78';
		ctx.textAlign = 'center';

		const pad = 12;
		for (let i = 0; i <= NUM_TICKS; i++) {
			const t = i / NUM_TICKS;
			const x = pad + t * (canvas.width - 2 * pad);
			const timeSec = startTime + t * (endTime - startTime);

			ctx.fillRect(x - 0.5, 0, 1, 4);
			ctx.textBaseline = 'top';
			ctx.fillText(timeSec.toFixed(1) + 's', x, 6);
		}

		texture.needsUpdate = true;
		// Phase 3: Fixed-size tick labels — clamp scale
		const xAxisWidth = winW - TS_MARGIN.left - TS_MARGIN.right;
		const xFixedScale = 40;
		const xScale = Math.min(xFixedScale, xAxisWidth);
		sprite.scale.set(xScale, xScale * (64 / 512), 1);
		sprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2,
			-winH / 2 + TS_MARGIN.bottom / 2 - 1, 2);
	}

	function createClipPlanes(): THREE.Plane[] {
		return [
			new THREE.Plane(new THREE.Vector3(1, 0, 0), 0),
			new THREE.Plane(new THREE.Vector3(-1, 0, 0), 0),
			new THREE.Plane(new THREE.Vector3(0, 1, 0), 0),
			new THREE.Plane(new THREE.Vector3(0, -1, 0), 0),
		];
	}

	function updateClipPlanes(win: SystemWindow) {
		const gx = win.group.position.x;
		const gy = win.group.position.y;
		const halfW = win.windowWidth / 2;
		const left = gx - halfW;
		const right = gx + halfW;
		const bottom = gy - win.windowHeight / 2 - HEADER_HEIGHT / 2;
		const top = gy + win.windowHeight / 2 - HEADER_HEIGHT / 2;

		win.clipPlanes[0].constant = -left;
		win.clipPlanes[1].constant = right;
		win.clipPlanes[2].constant = -bottom;
		win.clipPlanes[3].constant = top;
	}

	// A3: Dimension-adaptive axes with arrowheads
	function create3DAxes(clipPlanes: THREE.Plane[], stateNames: string[], nstates: number): THREE.Group {
		const axesGroup = new THREE.Group();
		const axisLength = 500;

		const axisConfigs = [
			{ color: 0xd4785a, dir: new THREE.Vector3(1, 0, 0), name: stateNames[0] || 'x' },
			{ color: 0xc9a84c, dir: new THREE.Vector3(0, 1, 0), name: stateNames[1] || 'y' },
			{ color: 0x8a9b68, dir: new THREE.Vector3(0, 0, 1), name: stateNames[2] || 'z' },
		];

		// Only create axes matching dimensions
		const numAxes = Math.min(nstates, 3);

		for (let i = 0; i < numAxes; i++) {
			const { color, dir, name } = axisConfigs[i];
			const geom = new THREE.BufferGeometry().setFromPoints([
				dir.clone().multiplyScalar(-axisLength),
				dir.clone().multiplyScalar(axisLength)
			]);
			const lineMat = new THREE.LineBasicMaterial({ color: 0x3a2e24, clippingPlanes: clipPlanes });
			axesGroup.add(new THREE.Line(geom, lineMat));

			// Arrowhead at positive end (distance=19)
			const coneGeom = new THREE.ConeGeometry(0.6, 2, 8);
			const coneMat = new THREE.MeshBasicMaterial({ color, clippingPlanes: clipPlanes });
			const cone = new THREE.Mesh(coneGeom, coneMat);
			const arrowPos = dir.clone().multiplyScalar(19);
			cone.position.copy(arrowPos);
			// Rotate cone to point along axis direction
			if (i === 0) cone.rotation.z = -Math.PI / 2; // X axis
			else if (i === 2) cone.rotation.x = Math.PI / 2; // Z axis
			// Y axis already points up by default
			axesGroup.add(cone);

			const label = createTextSprite(name, new THREE.Color(color));
			label.scale.set(3, 1.5, 1);
			label.position.copy(dir.clone().multiplyScalar(18));
			(label.material as THREE.SpriteMaterial).clippingPlanes = clipPlanes;
			axesGroup.add(label);
		}

		return axesGroup;
	}

	// A4: Compute nice tick intervals
	function computeNiceTicks(min: number, max: number, n: number): number[] {
		const range = max - min;
		if (range < 1e-10) return [min];
		const roughStep = range / n;
		const mag = Math.pow(10, Math.floor(Math.log10(roughStep)));
		const normStep = roughStep / mag;
		let niceStep: number;
		if (normStep <= 1.5) niceStep = 1 * mag;
		else if (normStep <= 3) niceStep = 2 * mag;
		else if (normStep <= 7) niceStep = 5 * mag;
		else niceStep = 10 * mag;

		const start = Math.ceil(min / niceStep) * niceStep;
		const ticks: number[] = [];
		for (let v = start; v <= max + niceStep * 0.01; v += niceStep) {
			ticks.push(v);
			if (ticks.length > n + 2) break;
		}
		return ticks;
	}

	// A4: Create phase tick marks group
	function createPhaseTickGroup(clipPlanes: THREE.Plane[]): THREE.Group {
		const group = new THREE.Group();
		// Pre-allocate tick sprites (up to 5 per axis, 3 axes = 15 max)
		for (let i = 0; i < 15; i++) {
			const canvas = document.createElement('canvas');
			canvas.width = 64;
			canvas.height = 24;
			const texture = new THREE.CanvasTexture(canvas);
			const material = new THREE.SpriteMaterial({
				map: texture,
				transparent: true,
				depthTest: false,
				clippingPlanes: clipPlanes
			});
			const sprite = new THREE.Sprite(material);
			sprite.scale.set(4, 1.5, 1);
			sprite.visible = false;
			group.add(sprite);
		}
		return group;
	}

	// A4: Update phase tick marks
	function updatePhaseTickMarks(win: SystemWindow) {
		if (!win.bounds.initialized) return;
		const b = win.bounds;

		// Check if bounds changed > 5% since last update
		if (win.lastTickBounds) {
			const lb = win.lastTickBounds;
			const rangeX = b.max.x - b.min.x;
			const rangeY = b.max.y - b.min.y;
			const rangeZ = b.max.z - b.min.z;
			const dX = Math.abs(lb.max.x - b.max.x) + Math.abs(lb.min.x - b.min.x);
			const dY = Math.abs(lb.max.y - b.max.y) + Math.abs(lb.min.y - b.min.y);
			const dZ = Math.abs(lb.max.z - b.max.z) + Math.abs(lb.min.z - b.min.z);
			if (dX < rangeX * 0.05 && dY < rangeY * 0.05 && dZ < rangeZ * 0.05) return;
		}

		win.lastTickBounds = {
			min: b.min.clone(),
			max: b.max.clone()
		};

		const mapping = getPhaseMapping(win);
		const children = win.phaseTickGroup.children as THREE.Sprite[];
		let tickIdx = 0;

		const axes = [
			{ dim: 0, min: b.min.x, max: b.max.x, dir: new THREE.Vector3(1, 0, 0), offset: new THREE.Vector3(0, -1.5, 0) },
			{ dim: 1, min: b.min.y, max: b.max.y, dir: new THREE.Vector3(0, 1, 0), offset: new THREE.Vector3(-1.5, 0, 0) },
			{ dim: 2, min: b.min.z, max: b.max.z, dir: new THREE.Vector3(0, 0, 1), offset: new THREE.Vector3(0, -1.5, 0) },
		];

		for (let a = 0; a < Math.min(win.nstates, 3); a++) {
			const ax = axes[a];
			const ticks = computeNiceTicks(ax.min, ax.max, 5);
			for (const val of ticks) {
				if (tickIdx >= 15) break;
				const sprite = children[tickIdx];
				// Map data value to local coords
				const localVal = (val - mapping.center[a]) * mapping.scale;
				const pos = ax.dir.clone().multiplyScalar(localVal).add(ax.offset);
				sprite.position.copy(pos);
				sprite.visible = true;

				// Update text
				const canvas = (sprite.material as THREE.SpriteMaterial).map!.image as HTMLCanvasElement;
				const ctx = canvas.getContext('2d')!;
				ctx.clearRect(0, 0, canvas.width, canvas.height);
				ctx.font = `400 14px ${CANVAS_FONT}`;
				ctx.fillStyle = '#9a8b78';
				ctx.textAlign = 'center';
				ctx.textBaseline = 'middle';
				ctx.fillText(formatTickValue(val), canvas.width / 2, canvas.height / 2);
				((sprite.material as THREE.SpriteMaterial).map as THREE.CanvasTexture).needsUpdate = true;
				tickIdx++;
			}
		}
		// Hide unused ticks
		for (let i = tickIdx; i < 15; i++) {
			children[i].visible = false;
		}
	}

	// A1: Dynamic bounds centering — compute center and scale from observed bounds
	function getPhaseMapping(win: SystemWindow): { center: number[]; scale: number } {
		const b = win.bounds;
		if (!b.initialized) {
			return { center: [0, 0, 0], scale: 0.5 };
		}
		const ranges = [
			b.max.x - b.min.x,
			b.max.y - b.min.y,
			b.max.z - b.min.z
		];
		const center = [
			(b.min.x + b.max.x) / 2,
			(b.min.y + b.max.y) / 2,
			(b.min.z + b.max.z) / 2
		];
		const maxRange = Math.max(...ranges.slice(0, Math.min(win.nstates, 3)), 0.001);
		const scale = (PHASE_HALF_EXTENT * 2) / maxRange;
		return { center, scale };
	}

	// A1: Map state to local coords using dynamic bounds
	function mapStateToLocal(state: number[], win: SystemWindow, nstates: number): THREE.Vector3 {
		const mapping = getPhaseMapping(win);
		const result = new THREE.Vector3(0, 0, 0);
		const sx = Number.isFinite(state[0]) ? state[0] : 0;
		const sy = Number.isFinite(state[1]) ? state[1] : 0;
		const sz = Number.isFinite(state[2]) ? state[2] : 0;

		if (nstates >= 3) {
			result.x = (sx - mapping.center[0]) * mapping.scale;
			result.y = (sy - mapping.center[1]) * mapping.scale;
			result.z = (sz - mapping.center[2]) * mapping.scale;
		} else if (nstates >= 2) {
			result.x = (sx - mapping.center[0]) * mapping.scale;
			result.y = (sy - mapping.center[1]) * mapping.scale;
			result.z = 0;
		} else if (nstates >= 1) {
			result.x = (sx - mapping.center[0]) * mapping.scale;
			result.y = 0;
			result.z = 0;
		}

		return result;
	}

	// A6: View angle presets
	const VIEW_PRESETS: Record<ViewAngle, { rx: number; ry: number }> = {
		'ISO': { rx: -0.4, ry: 0.3 },
		'XY': { rx: 0, ry: 0 },
		'XZ': { rx: -Math.PI / 2, ry: 0 },
		'YZ': { rx: 0, ry: -Math.PI / 2 },
	};
	const VIEW_CYCLE: ViewAngle[] = ['ISO', 'XY', 'XZ', 'YZ'];

	// B3: Create a port dot
	function createPortDot(
		systemId: string,
		portIndex: number,
		isOutput: boolean,
		portName: string,
		color: THREE.Color,
		xOffset: number,
		yPos: number
	): PortDot {
		// Filled circle
		const circleGeom = new THREE.CircleGeometry(1.0, 16);
		const circleMat = new THREE.MeshBasicMaterial({
			color: isOutput ? color : 0x3a2e24,
			transparent: true,
			opacity: isOutput ? 0.8 : 0.3,
			side: THREE.DoubleSide,
			depthTest: false
		});
		const mesh = new THREE.Mesh(circleGeom, circleMat);
		mesh.position.set(xOffset, yPos, 5);
		mesh.userData = { isPort: true, systemId, portIndex, isOutput };

		// Ring outline
		const ringGeom = new THREE.BufferGeometry().setFromPoints(
			Array.from({ length: 17 }, (_, i) => {
				const angle = (i / 16) * Math.PI * 2;
				return new THREE.Vector3(Math.cos(angle) * 1.2, Math.sin(angle) * 1.2, 0);
			})
		);
		const ringMat = new THREE.LineBasicMaterial({ color, transparent: true, opacity: 0.6, depthTest: false });
		const ring = new THREE.LineLoop(ringGeom, ringMat);
		ring.position.set(xOffset, yPos, 5);

		// Hit area (invisible, larger)
		const hitGeom = new THREE.PlaneGeometry(4, 4);
		const hitMat = new THREE.MeshBasicMaterial({ transparent: true, opacity: 0.0, side: THREE.DoubleSide, depthTest: false });
		const hitArea = new THREE.Mesh(hitGeom, hitMat);
		hitArea.position.set(xOffset, yPos, 4);
		hitArea.userData = { isPort: true, systemId, portIndex, isOutput };

		// Label (outside box)
		const labelOffset = isOutput ? 5 : -5;
		const label = createTextSprite(portName, new THREE.Color(0x9a8b78));
		label.scale.set(8, 2.5, 1);
		label.position.set(xOffset + labelOffset, yPos, 5);

		return { mesh, ring, hitArea, label, systemId, portIndex, isOutput };
	}

	// B3: Create all ports for a system window
	function createPorts(
		group: THREE.Group,
		id: string,
		sys: SystemState,
		winWidth: number,
		winHeight: number,
		color: THREE.Color
	): { outputPorts: PortDot[]; inputPorts: PortDot[]; dividerLine: THREE.Line | null } {
		const tmpl = algebraic.templateList.find(t => t.id === sys.templateId);
		const outputPorts: PortDot[] = [];
		const inputPorts: PortDot[] = [];

		const contentTop = winHeight / 2 - HEADER_HEIGHT;
		const contentBottom = -winHeight / 2;
		const contentHeight = contentTop - contentBottom;

		// Output ports (right edge)
		const nOutputs = sys.noutputs;
		const outNames = tmpl?.output_names ?? [];
		for (let i = 0; i < nOutputs; i++) {
			const t = (i + 1) / (nOutputs + 1);
			const yPos = contentBottom + t * contentHeight;
			const port = createPortDot(id, i + 1, true, outNames[i] || `out${i + 1}`, color, winWidth / 2, yPos);
			group.add(port.mesh);
			group.add(port.ring);
			group.add(port.hitArea);
			group.add(port.label);
			outputPorts.push(port);
		}

		// Input ports (left edge)
		const nInputs = sys.ninputs;
		const inNames = tmpl?.input_names ?? [];
		const nstates = sys.nstates;
		// State inputs on top, param inputs below
		for (let i = 0; i < nInputs; i++) {
			const t = (i + 1) / (nInputs + 1);
			const yPos = contentBottom + (1 - t) * contentHeight;
			const port = createPortDot(id, i + 1, false, inNames[i] || `in${i + 1}`, color, -winWidth / 2, yPos);
			group.add(port.mesh);
			group.add(port.ring);
			group.add(port.hitArea);
			group.add(port.label);
			inputPorts.push(port);
		}

		// Divider line between state and param inputs (if applicable)
		let dividerLine: THREE.Line | null = null;
		if (nstates > 0 && nInputs > nstates) {
			const dividerY = contentBottom + (1 - (nstates + 0.5) / (nInputs + 1)) * contentHeight;
			const divGeom = new THREE.BufferGeometry().setFromPoints([
				new THREE.Vector3(-winWidth / 2 - 1, dividerY, 5),
				new THREE.Vector3(-winWidth / 2 + 6, dividerY, 5)
			]);
			const divMat = new THREE.LineBasicMaterial({ color: 0x3a2e24, transparent: true, opacity: 0.4, depthTest: false });
			dividerLine = new THREE.Line(divGeom, divMat);
			group.add(dividerLine);
		}

		return { outputPorts, inputPorts, dividerLine };
	}

	// B5: A* wire pathfinding
	function computeWireRoute(
		from: THREE.Vector2,
		to: THREE.Vector2,
		obstacles: Array<{ x: number; y: number; w: number; h: number }>
	): THREE.Vector3[] {
		const cellSize = 2;
		const padding = 2;

		// Grid bounds
		const minX = Math.min(from.x, to.x) - 60;
		const maxX = Math.max(from.x, to.x) + 60;
		const minY = Math.min(from.y, to.y) - 60;
		const maxY = Math.max(from.y, to.y) + 60;

		const cols = Math.ceil((maxX - minX) / cellSize);
		const rows = Math.ceil((maxY - minY) / cellSize);

		// Create obstacle grid
		const blocked = new Set<number>();
		for (const obs of obstacles) {
			const ox1 = Math.floor((obs.x - obs.w / 2 - padding - minX) / cellSize);
			const ox2 = Math.ceil((obs.x + obs.w / 2 + padding - minX) / cellSize);
			const oy1 = Math.floor((obs.y - obs.h / 2 - padding - minY) / cellSize);
			const oy2 = Math.ceil((obs.y + obs.h / 2 + padding - minY) / cellSize);
			for (let cx = ox1; cx <= ox2; cx++) {
				for (let cy = oy1; cy <= oy2; cy++) {
					if (cx >= 0 && cx < cols && cy >= 0 && cy < rows) {
						blocked.add(cy * cols + cx);
					}
				}
			}
		}

		// A* search
		const startCol = Math.round((from.x - minX) / cellSize);
		const startRow = Math.round((from.y - minY) / cellSize);
		const endCol = Math.round((to.x - minX) / cellSize);
		const endRow = Math.round((to.y - minY) / cellSize);

		const key = (c: number, r: number) => r * cols + c;
		const heuristic = (c: number, r: number) => Math.abs(c - endCol) + Math.abs(r - endRow);

		const startKey = key(startCol, startRow);
		const endKey = key(endCol, endRow);

		const gScore = new Map<number, number>();
		const fScore = new Map<number, number>();
		const cameFrom = new Map<number, number>();
		const openSet = new Set<number>();
		const closed = new Set<number>();
		const nodePos = new Map<number, { c: number; r: number }>();

		gScore.set(startKey, 0);
		fScore.set(startKey, heuristic(startCol, startRow));
		openSet.add(startKey);
		nodePos.set(startKey, { c: startCol, r: startRow });

		const dirs = [[1, 0], [-1, 0], [0, 1], [0, -1]];
		let found = false;
		let iterations = 0;
		const maxIterations = 3000;

		while (openSet.size > 0 && iterations < maxIterations) {
			iterations++;
			// Find lowest f in open set
			let bestKey = -1;
			let bestF = Infinity;
			for (const k of openSet) {
				const f = fScore.get(k) ?? Infinity;
				if (f < bestF) {
					bestF = f;
					bestKey = k;
				}
			}
			if (bestKey === -1) break;

			if (bestKey === endKey) {
				found = true;
				break;
			}

			openSet.delete(bestKey);
			closed.add(bestKey);

			const pos = nodePos.get(bestKey)!;
			const currentG = gScore.get(bestKey)!;

			for (const [dc, dr] of dirs) {
				const nc = pos.c + dc;
				const nr = pos.r + dr;
				if (nc < 0 || nc >= cols || nr < 0 || nr >= rows) continue;
				const nk = key(nc, nr);
				if (closed.has(nk) || blocked.has(nk)) continue;

				const ng = currentG + 1;
				const prevG = gScore.get(nk);
				if (prevG === undefined || ng < prevG) {
					gScore.set(nk, ng);
					fScore.set(nk, ng + heuristic(nc, nr));
					cameFrom.set(nk, bestKey);
					nodePos.set(nk, { c: nc, r: nr });
					openSet.add(nk);
				}
			}
		}

		if (found) {
			// Reconstruct path from endKey to startKey
			const rawPath: THREE.Vector3[] = [];
			let cur = endKey;
			while (cur !== startKey) {
				const p = nodePos.get(cur)!;
				rawPath.push(new THREE.Vector3(minX + p.c * cellSize, minY + p.r * cellSize, 3));
				const prev = cameFrom.get(cur);
				if (prev === undefined) break;
				cur = prev;
			}
			rawPath.push(new THREE.Vector3(from.x, from.y, 3));
			rawPath.reverse();
			rawPath.push(new THREE.Vector3(to.x, to.y, 3));

			// Remove collinear points
			if (rawPath.length > 2) {
				const simplified: THREE.Vector3[] = [rawPath[0]];
				for (let i = 1; i < rawPath.length - 1; i++) {
					const prev = rawPath[i - 1];
					const curr = rawPath[i];
					const next = rawPath[i + 1];
					const dx1 = curr.x - prev.x;
					const dy1 = curr.y - prev.y;
					const dx2 = next.x - curr.x;
					const dy2 = next.y - curr.y;
					if (Math.abs(dx1 * dy2 - dy1 * dx2) > 0.01) {
						simplified.push(curr);
					}
				}
				simplified.push(rawPath[rawPath.length - 1]);
				return simplified;
			}
			return rawPath;
		}

		// Fallback: simple Z-route (right from source, then vertical, then right to target)
		const midX = (from.x + to.x) / 2;
		return [
			new THREE.Vector3(from.x, from.y, 3),
			new THREE.Vector3(from.x + 4, from.y, 3),
			new THREE.Vector3(midX, from.y, 3),
			new THREE.Vector3(midX, to.y, 3),
			new THREE.Vector3(to.x - 4, to.y, 3),
			new THREE.Vector3(to.x, to.y, 3)
		];
	}

	// B4: Get port world position
	function getPortWorldPos(port: PortDot): THREE.Vector2 {
		const win = systemWindows.get(port.systemId);
		if (!win) return new THREE.Vector2(0, 0);
		return new THREE.Vector2(
			win.group.position.x + port.mesh.position.x,
			win.group.position.y + port.mesh.position.y
		);
	}

	// B4: Create wire visual
	function createWireVisual(wireId: string, fromSysId: string, fromPort: number, toSysId: string, toPort: number): WireVisual {
		const geometry = new THREE.BufferGeometry();
		const material = new THREE.LineBasicMaterial({ color: 0xc9a84c, transparent: true, opacity: 0.7, depthTest: false });
		const line = new THREE.Line(geometry, material);
		scene.add(line);

		// Value whisker line (4 points)
		const valueGeometry = new THREE.BufferGeometry();
		valueGeometry.setAttribute('position', new THREE.BufferAttribute(new Float32Array(12), 3));
		valueGeometry.setDrawRange(0, 0);
		const valueMat = new THREE.LineBasicMaterial({ color: 0xc9a84c, transparent: true, opacity: 0.4, depthTest: false });
		const valueLine = new THREE.Line(valueGeometry, valueMat);
		scene.add(valueLine);

		return { wireId, line, geometry, fromSystemId: fromSysId, fromPort, toSystemId: toSysId, toPort, valueLine, valueGeometry };
	}

	// B7: Update wire route
	function updateWireRoute(wv: WireVisual) {
		const fromWin = systemWindows.get(wv.fromSystemId);
		const toWin = systemWindows.get(wv.toSystemId);
		if (!fromWin || !toWin) return;

		const fromPortDot = fromWin.outputPorts.find(p => p.portIndex === wv.fromPort);
		const toPortDot = toWin.inputPorts.find(p => p.portIndex === wv.toPort);
		if (!fromPortDot || !toPortDot) return;

		const from = getPortWorldPos(fromPortDot);
		const to = getPortWorldPos(toPortDot);

		// Build obstacles from all windows
		const obstacles: Array<{ x: number; y: number; w: number; h: number }> = [];
		for (const win of systemWindows.values()) {
			obstacles.push({
				x: win.group.position.x,
				y: win.group.position.y,
				w: win.windowWidth,
				h: win.windowHeight + HEADER_HEIGHT
			});
		}

		const rawRoute = computeWireRoute(from, to, obstacles);
		const route = smoothWirePath(rawRoute);
		const positions = new Float32Array(route.length * 3);
		for (let i = 0; i < route.length; i++) {
			positions[i * 3] = route[i].x;
			positions[i * 3 + 1] = route[i].y;
			positions[i * 3 + 2] = route[i].z;
		}
		wv.geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
		wv.geometry.attributes.position.needsUpdate = true;
	}

	// B7: Sync wire visuals with store
	function syncWireVisuals() {
		const wires = algebraic.wireList;
		const currentWireIds = new Set(wires.map(w => w.id));

		// Remove visuals for deleted wires
		for (const [id, wv] of wireVisuals) {
			if (!currentWireIds.has(id)) {
				scene.remove(wv.line);
				wv.geometry.dispose();
				(wv.line.material as THREE.Material).dispose();
				scene.remove(wv.valueLine);
				wv.valueGeometry.dispose();
				(wv.valueLine.material as THREE.Material).dispose();
				wireVisuals.delete(id);
			}
		}

		// Create/update visuals
		for (const wire of wires) {
			let wv = wireVisuals.get(wire.id);
			if (!wv) {
				wv = createWireVisual(wire.id, wire.fromSystem, wire.fromPort, wire.toSystem, wire.toPort);
				wireVisuals.set(wire.id, wv);
			}
			updateWireRoute(wv);

			// Update value whisker on the output port
			const fromWin = systemWindows.get(wire.fromSystem);
			if (fromWin) {
				const fromPortDot = fromWin.outputPorts.find(p => p.portIndex === wire.fromPort);
				if (fromPortDot) {
					const portWorld = getPortWorldPos(fromPortDot);
					// Get wire direction from first segment
					const routePos = wv.geometry.attributes.position?.array as Float32Array | undefined;
					if (routePos && routePos.length >= 6) {
						const dx = routePos[3] - routePos[0];
						const dy = routePos[4] - routePos[1];
						const len = Math.sqrt(dx * dx + dy * dy);
						if (len > 0.1) {
							// Perpendicular direction
							const perpX = -dy / len;
							const perpY = dx / len;
							const deflection = Math.tanh(wire.value / 5) * 3;
							const valPos = wv.valueGeometry.attributes.position.array as Float32Array;
							// 4-point whisker: port → out along wire → deflected → back
							valPos[0] = portWorld.x; valPos[1] = portWorld.y; valPos[2] = 4;
							valPos[3] = portWorld.x + dx / len * 2; valPos[4] = portWorld.y + dy / len * 2; valPos[5] = 4;
							valPos[6] = portWorld.x + dx / len * 2 + perpX * deflection; valPos[7] = portWorld.y + dy / len * 2 + perpY * deflection; valPos[8] = 4;
							valPos[9] = portWorld.x + dx / len * 4 + perpX * deflection * 0.5; valPos[10] = portWorld.y + dy / len * 4 + perpY * deflection * 0.5; valPos[11] = 4;
							wv.valueGeometry.attributes.position.needsUpdate = true;
							wv.valueGeometry.setDrawRange(0, 4);
						}
					}
				}
			}
		}

		// Update input port opacity and connected label shift
		const connectedInputs = new Set<string>();
		const connectedOutputs = new Set<string>();
		for (const wire of wires) {
			connectedInputs.add(`${wire.toSystem}:${wire.toPort}`);
			connectedOutputs.add(`${wire.fromSystem}:${wire.fromPort}`);
		}
		for (const win of systemWindows.values()) {
			for (const port of win.inputPorts) {
				const connected = connectedInputs.has(`${port.systemId}:${port.portIndex}`);
				(port.mesh.material as THREE.MeshBasicMaterial).opacity = connected ? 0.8 : 0.3;
				// Shift label up when wire is connected to avoid overlap
				const baseY = port.mesh.position.y;
				port.label.position.y = connected ? baseY + 2 : baseY;
			}
			for (const port of win.outputPorts) {
				const connected = connectedOutputs.has(`${port.systemId}:${port.portIndex}`);
				const baseY = port.mesh.position.y;
				port.label.position.y = connected ? baseY + 2 : baseY;
			}
		}
	}

	function createSystemWindow(id: string, sys: SystemState): SystemWindow {
		const color = COLORS[colorIndex % COLORS.length];
		colorIndex++;
		const nstates = sys.nstates;
		const winWidth = DEFAULT_WINDOW_WIDTH;
		const winHeight = DEFAULT_WINDOW_HEIGHT;
		const tmpl = algebraic.templateList.find(t => t.id === sys.templateId);
		const stateNames = tmpl?.state_names ?? STATE_NAMES;

		const group = new THREE.Group();
		group.userData = { systemId: id };

		const windowIndex = systemWindows.size;
		const gridCols = 3;
		const spacing = winWidth + 8;
		const startX = -spacing;
		const startY = 40;
		const defaultX = startX + (windowIndex % gridCols) * spacing;
		const defaultY = startY - Math.floor(windowIndex / gridCols) * (winHeight + HEADER_HEIGHT + 8);

		group.position.set(sys.position?.x ?? defaultX, sys.position?.y ?? defaultY, 0);

		// Window border (rounded corners)
		const borderMaterial = new THREE.LineBasicMaterial({ color, transparent: true, opacity: 0.5 });
		const border = createRoundedRectBorder(winWidth, winHeight + HEADER_HEIGHT, CORNER_RADIUS, borderMaterial);
		group.add(border);

		// Header bar (rounded top corners)
		const headerShape = createHeaderShape(winWidth, HEADER_HEIGHT, CORNER_RADIUS);
		const headerGeometry = new THREE.ShapeGeometry(headerShape);
		const headerMaterial = new THREE.MeshBasicMaterial({ color, transparent: true, opacity: 0.3, side: THREE.DoubleSide });
		const header = new THREE.Mesh(headerGeometry, headerMaterial);
		header.position.set(0, winHeight / 2, 0);
		header.userData = { isHeader: true, systemId: id };
		group.add(header);

		// Close button (X)
		const closeButtonGeom = new THREE.PlaneGeometry(3, 3);
		const closeButtonMat = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const closeButton = new THREE.Mesh(closeButtonGeom, closeButtonMat);
		closeButton.position.set(-winWidth / 2 + 3, winHeight / 2, 1);
		closeButton.userData = { isCloseButton: true, systemId: id };
		group.add(closeButton);

		const closeButtonLabel = createButtonSprite('x', false);
		closeButtonLabel.position.set(-winWidth / 2 + 3, winHeight / 2, 2);
		closeButtonLabel.scale.set(4, 3.5, 1);
		group.add(closeButtonLabel);

		// Minimize button (—)
		const minimizeButtonGeom = new THREE.PlaneGeometry(3, 3);
		const minimizeButtonMat = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const minimizeButton = new THREE.Mesh(minimizeButtonGeom, minimizeButtonMat);
		minimizeButton.position.set(-winWidth / 2 + 7, winHeight / 2, 1);
		minimizeButton.userData = { isMinimizeButton: true, systemId: id };
		group.add(minimizeButton);

		const minimizeButtonLabel = createButtonSprite('\u2014', false);
		minimizeButtonLabel.position.set(-winWidth / 2 + 7, winHeight / 2, 2);
		minimizeButtonLabel.scale.set(4, 3.5, 1);
		group.add(minimizeButtonLabel);

		// Label (shifted right to make room for close/minimize)
		const systemName = sys.templateId.split('_')[0];
		const label = createTextSprite(systemName, color);
		label.position.set(-winWidth / 2 + 16, winHeight / 2, 1);
		group.add(label);

		// Mode buttons
		const phaseButtonGeometry = new THREE.PlaneGeometry(8, 3);
		const phaseButtonMaterial = new THREE.MeshBasicMaterial({ color: 0x8a7435, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const phaseButton = new THREE.Mesh(phaseButtonGeometry, phaseButtonMaterial);
		phaseButton.position.set(winWidth / 2 - 16, winHeight / 2, 1);
		phaseButton.userData = { isPhaseButton: true, systemId: id };
		group.add(phaseButton);

		const phaseButtonLabel = createButtonSprite('Phase', true);
		phaseButtonLabel.position.set(winWidth / 2 - 16, winHeight / 2, 2);
		group.add(phaseButtonLabel);

		const timeButtonGeometry = new THREE.PlaneGeometry(8, 3);
		const timeButtonMaterial = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const timeButton = new THREE.Mesh(timeButtonGeometry, timeButtonMaterial);
		timeButton.position.set(winWidth / 2 - 6, winHeight / 2, 1);
		timeButton.userData = { isTimeButton: true, systemId: id };
		group.add(timeButton);

		const timeButtonLabel = createButtonSprite('Time', false);
		timeButtonLabel.position.set(winWidth / 2 - 6, winHeight / 2, 2);
		group.add(timeButtonLabel);

		// A6: View cycle button (3D only)
		const viewCycleGeometry = new THREE.PlaneGeometry(6, 3);
		const viewCycleMaterial = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const viewCycleButton = new THREE.Mesh(viewCycleGeometry, viewCycleMaterial);
		viewCycleButton.position.set(winWidth / 2 - 26, winHeight / 2, 1);
		viewCycleButton.userData = { isViewCycleButton: true, systemId: id };
		viewCycleButton.visible = nstates >= 3;
		group.add(viewCycleButton);

		const viewCycleLabel = createButtonSprite('ISO', false);
		viewCycleLabel.position.set(winWidth / 2 - 26, winHeight / 2, 2);
		viewCycleLabel.scale.set(6, 3.5, 1);
		viewCycleLabel.visible = nstates >= 3;
		group.add(viewCycleLabel);

		// Resize handle
		const resizeHandleGeom = new THREE.PlaneGeometry(4, 4);
		const resizeHandleMat = new THREE.MeshBasicMaterial({ color, transparent: true, opacity: 0.4, side: THREE.DoubleSide });
		const resizeHandle = new THREE.Mesh(resizeHandleGeom, resizeHandleMat);
		resizeHandle.position.set(winWidth / 2 - 2, -winHeight / 2 - HEADER_HEIGHT + 2, 1);
		resizeHandle.userData = { isResizeHandle: true, systemId: id };
		group.add(resizeHandle);

		// Edit button (bottom-left of header)
		const editButtonGeom = new THREE.PlaneGeometry(6, 3);
		const editButtonMat = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const editButton = new THREE.Mesh(editButtonGeom, editButtonMat);
		editButton.position.set(winWidth / 2 - 36, winHeight / 2, 1);
		editButton.userData = { isEditButton: true, systemId: id };
		group.add(editButton);

		const editButtonLabel = createButtonSprite('Edit', false);
		editButtonLabel.position.set(winWidth / 2 - 36, winHeight / 2, 2);
		editButtonLabel.scale.set(6, 3.5, 1);
		group.add(editButtonLabel);

		// === Phase Space View ===
		const contentGroup = new THREE.Group();
		contentGroup.position.set(0, -HEADER_HEIGHT / 2, 0);

		// A5: Adaptive dimensionality
		if (nstates >= 3) {
			contentGroup.rotation.x = -0.4;
			contentGroup.rotation.y = 0.3;
		}
		// 2D / 1D: flat (rx=0, ry=0) — default

		group.add(contentGroup);

		// Clipping planes
		const clipPlanes = createClipPlanes();

		// Trail geometry
		const trailGeometry = new THREE.BufferGeometry();
		const positions = new Float32Array(MAX_POINTS * 3);
		const colors = new Float32Array(MAX_POINTS * 3);
		trailGeometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
		trailGeometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
		trailGeometry.setDrawRange(0, 0);
		const trailMaterial = new THREE.LineBasicMaterial({ vertexColors: true, transparent: true, opacity: 0.9, clippingPlanes: clipPlanes });
		const trail = new THREE.Line(trailGeometry, trailMaterial);
		contentGroup.add(trail);

		// Current point sphere
		const pointGeometry = new THREE.SphereGeometry(0.8, 16, 16);
		const pointMaterial = new THREE.MeshBasicMaterial({ color, transparent: true, opacity: 0.95, clippingPlanes: clipPlanes });
		const point = new THREE.Mesh(pointGeometry, pointMaterial);
		contentGroup.add(point);

		// A3: Dimension-adaptive axes with arrowheads
		const axesGroup = create3DAxes(clipPlanes, stateNames, nstates);
		contentGroup.add(axesGroup);

		// A4: Phase tick marks
		const phaseTickGroup = createPhaseTickGroup(clipPlanes);
		contentGroup.add(phaseTickGroup);

		// === Time Series View ===
		const timeSeriesGroup = new THREE.Group();
		timeSeriesGroup.position.set(0, -HEADER_HEIGHT / 2, 0);
		timeSeriesGroup.visible = false;
		group.add(timeSeriesGroup);

		const timeSeriesLines: THREE.Line[] = [];
		const timeSeriesGeometries: THREE.BufferGeometry[] = [];
		const legendSprites: THREE.Sprite[] = [];

		for (let i = 0; i < nstates; i++) {
			const tsGeometry = new THREE.BufferGeometry();
			const tsPositions = new Float32Array(TIME_WINDOW_SAMPLES * 3);
			tsGeometry.setAttribute('position', new THREE.BufferAttribute(tsPositions, 3));
			tsGeometry.setDrawRange(0, 0);

			const stateColor = STATE_COLORS[i % STATE_COLORS.length];
			const tsMaterial = new THREE.LineBasicMaterial({ color: stateColor, transparent: true, opacity: 0.9 });
			const tsLine = new THREE.Line(tsGeometry, tsMaterial);
			timeSeriesGroup.add(tsLine);
			timeSeriesLines.push(tsLine);
			timeSeriesGeometries.push(tsGeometry);

			const legendSprite = createLegendSprite(stateNames[i] || `v${i}`, stateColor);
			legendSprite.position.set(winWidth / 2 - 5, winHeight / 2 - 4 - i * 3, 1);
			timeSeriesGroup.add(legendSprite);
			legendSprites.push(legendSprite);
		}

		// Axis lines for time series
		const tsAxisGeometry = new THREE.BufferGeometry();
		const tsAxisPoints = [
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			winWidth / 2 - TS_MARGIN.right, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, winHeight / 2 - TS_MARGIN.top, 0,
		];
		tsAxisGeometry.setAttribute('position', new THREE.Float32BufferAttribute(tsAxisPoints, 3));
		const tsAxisMaterial = new THREE.LineBasicMaterial({ color: 0x3a2e24, transparent: true, opacity: 0.5 });
		const tsAxisLines = new THREE.LineSegments(tsAxisGeometry, tsAxisMaterial);
		timeSeriesGroup.add(tsAxisLines);

		// Tick sprites (Phase 3: fixed-size)
		const yTickSprite = createTickSprite(128, 512);
		const initYAxisH = winHeight - TS_MARGIN.bottom - TS_MARGIN.top;
		const initYScale = Math.min(40, initYAxisH);
		yTickSprite.scale.set(initYScale * (128 / 512), initYScale, 1);
		yTickSprite.position.set(-winWidth / 2 + TS_MARGIN.left / 2, (TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);
		timeSeriesGroup.add(yTickSprite);

		const xTickSprite = createTickSprite(512, 64);
		const initXAxisW = winWidth - TS_MARGIN.left - TS_MARGIN.right;
		const initXScale = Math.min(40, initXAxisW);
		xTickSprite.scale.set(initXScale, initXScale * (64 / 512), 1);
		xTickSprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2, -winHeight / 2 + TS_MARGIN.bottom / 2 - 1, 2);
		timeSeriesGroup.add(xTickSprite);

		// B3: Port dots
		const { outputPorts, inputPorts, dividerLine } = createPorts(group, id, sys, winWidth, winHeight, color);

		scene.add(group);

		return {
			group, header, headerLabel: label,
			phaseButton, timeButton, phaseButtonLabel, timeButtonLabel,
			viewCycleButton, viewCycleLabel, viewAngle: 'ISO' as ViewAngle,
			resizeHandle, contentGroup, border,
			trailGeometry, trail, point, axesGroup,
			phaseTickGroup, lastTickBounds: null,
			timeSeriesGroup, timeSeriesLines, timeSeriesGeometries, legendSprites,
			tsAxisLines, yTickSprite, xTickSprite, clipPlanes,
			color, viewMode: 'phase' as ViewMode, nstates,
			windowWidth: winWidth, windowHeight: winHeight,
			bounds: { min: new THREE.Vector3(Infinity, Infinity, Infinity), max: new THREE.Vector3(-Infinity, -Infinity, -Infinity), initialized: false },
			tsYMin: Infinity, tsYMax: -Infinity,
			outputPorts, inputPorts, portDividerLine: dividerLine,
			editButton, editButtonLabel,
			closeButton, closeButtonLabel,
			minimizeButton, minimizeButtonLabel,
			minimized: false, paramsExpanded: false,
			stateValueSprites: [], paramValueSprites: [],
			caretButton: null, caretLabel: null,
			savedWindowHeight: winHeight, lastMinimizedUpdate: 0
		};
	}

	function getOrCreateWindow(id: string, sys: SystemState): SystemWindow {
		let win = systemWindows.get(id);
		if (!win) {
			win = createSystemWindow(id, sys);
			systemWindows.set(id, win);
			updateClipPlanes(win);
		}
		return win;
	}

	function isFiniteState(state: number[], nstates: number): boolean {
		const dims = Math.min(nstates, 3);
		for (let i = 0; i < dims; i++) {
			if (!Number.isFinite(state[i])) return false;
		}
		return true;
	}

	function extendBounds(min: THREE.Vector3, max: THREE.Vector3, state: number[], nstates: number) {
		if (nstates >= 1) {
			min.x = Math.min(min.x, state[0]);
			max.x = Math.max(max.x, state[0]);
		}
		if (nstates >= 2) {
			min.y = Math.min(min.y, state[1]);
			max.y = Math.max(max.y, state[1]);
		}
		if (nstates >= 3) {
			min.z = Math.min(min.z, state[2]);
			max.z = Math.max(max.z, state[2]);
		}
	}

	function recomputeBounds(win: SystemWindow, history: number[][], currentState: number[]) {
		const min = new THREE.Vector3(Infinity, Infinity, Infinity);
		const max = new THREE.Vector3(-Infinity, -Infinity, -Infinity);
		let hasFiniteSamples = false;
		const dims = Math.min(win.nstates, 3);
		const start = Math.max(0, history.length - PHASE_BOUNDS_HISTORY);

		for (let i = start; i < history.length; i++) {
			const state = history[i];
			if (!isFiniteState(state, win.nstates)) continue;
			extendBounds(min, max, state, win.nstates);
			hasFiniteSamples = true;
		}

		if (isFiniteState(currentState, win.nstates)) {
			extendBounds(min, max, currentState, win.nstates);
			hasFiniteSamples = true;
		}

		if (!hasFiniteSamples) {
			win.bounds.min.set(-1, -1, -1);
			win.bounds.max.set(1, 1, 1);
			win.bounds.initialized = true;
			return;
		}

		// Keep origin visible while still allowing follow behavior.
		if (dims >= 1) {
			min.x = Math.min(min.x, 0);
			max.x = Math.max(max.x, 0);
		}
		if (dims >= 2) {
			min.y = Math.min(min.y, 0);
			max.y = Math.max(max.y, 0);
		}
		if (dims >= 3) {
			min.z = Math.min(min.z, 0);
			max.z = Math.max(max.z, 0);
		}

		win.bounds.min.copy(min);
		win.bounds.max.copy(max);
		win.bounds.initialized = true;
	}

	function setViewMode(win: SystemWindow, mode: ViewMode) {
		if (win.viewMode === mode) return;
		win.viewMode = mode;

		win.contentGroup.visible = mode === 'phase';
		win.timeSeriesGroup.visible = mode === 'timeseries';

		const updateButton = (oldSprite: THREE.Sprite, text: string, active: boolean): THREE.Sprite => {
			const newSprite = createButtonSprite(text, active);
			newSprite.position.copy(oldSprite.position);
			win.group.remove(oldSprite);
			win.group.add(newSprite);
			(oldSprite.material as THREE.SpriteMaterial).map?.dispose();
			(oldSprite.material as THREE.Material).dispose();
			return newSprite;
		};

		win.phaseButtonLabel = updateButton(win.phaseButtonLabel, 'Phase', mode === 'phase');
		win.timeButtonLabel = updateButton(win.timeButtonLabel, 'Time', mode === 'timeseries');
	}

	// A6: Cycle view angle
	function cycleViewAngle(win: SystemWindow) {
		if (win.nstates < 3) return;
		const currentIdx = VIEW_CYCLE.indexOf(win.viewAngle);
		const nextIdx = (currentIdx + 1) % VIEW_CYCLE.length;
		win.viewAngle = VIEW_CYCLE[nextIdx];
		const preset = VIEW_PRESETS[win.viewAngle];
		win.contentGroup.rotation.x = preset.rx;
		win.contentGroup.rotation.y = preset.ry;

		// Update label
		const oldLabel = win.viewCycleLabel;
		const newLabel = createButtonSprite(win.viewAngle, false);
		newLabel.position.copy(oldLabel.position);
		newLabel.scale.set(6, 3.5, 1);
		win.group.remove(oldLabel);
		win.group.add(newLabel);
		(oldLabel.material as THREE.SpriteMaterial).map?.dispose();
		(oldLabel.material as THREE.Material).dispose();
		win.viewCycleLabel = newLabel;
	}

	function resizeWindow(win: SystemWindow, newWidth: number, newHeight: number) {
		newWidth = Math.max(MIN_WINDOW_SIZE, newWidth);
		newHeight = Math.max(MIN_WINDOW_SIZE, newHeight);
		win.windowWidth = newWidth;
		win.windowHeight = newHeight;

		win.group.remove(win.border);
		win.border.geometry.dispose();
		(win.border.material as THREE.Material).dispose();
		const newBorderMat = new THREE.LineBasicMaterial({ color: win.color, transparent: true, opacity: 0.5 });
		win.border = createRoundedRectBorder(newWidth, newHeight + HEADER_HEIGHT, CORNER_RADIUS, newBorderMat);
		win.group.add(win.border);

		win.header.geometry.dispose();
		const newHeaderShape = createHeaderShape(newWidth, HEADER_HEIGHT, CORNER_RADIUS);
		win.header.geometry = new THREE.ShapeGeometry(newHeaderShape);
		win.header.position.set(0, newHeight / 2, 0);

		win.closeButton.position.set(-newWidth / 2 + 3, newHeight / 2, 1);
		win.closeButtonLabel.position.set(-newWidth / 2 + 3, newHeight / 2, 2);
		win.minimizeButton.position.set(-newWidth / 2 + 7, newHeight / 2, 1);
		win.minimizeButtonLabel.position.set(-newWidth / 2 + 7, newHeight / 2, 2);
		win.headerLabel.position.set(-newWidth / 2 + 16, newHeight / 2, 1);

		win.phaseButton.position.set(newWidth / 2 - 16, newHeight / 2, 1);
		win.phaseButtonLabel.position.set(newWidth / 2 - 16, newHeight / 2, 2);
		win.timeButton.position.set(newWidth / 2 - 6, newHeight / 2, 1);
		win.timeButtonLabel.position.set(newWidth / 2 - 6, newHeight / 2, 2);
		win.viewCycleButton.position.set(newWidth / 2 - 26, newHeight / 2, 1);
		win.viewCycleLabel.position.set(newWidth / 2 - 26, newHeight / 2, 2);
		win.editButton.position.set(newWidth / 2 - 36, newHeight / 2, 1);
		win.editButtonLabel.position.set(newWidth / 2 - 36, newHeight / 2, 2);

		win.resizeHandle.position.set(newWidth / 2 - 2, -newHeight / 2 - HEADER_HEIGHT + 2, 1);

		const scaleRatio = Math.min(newWidth / DEFAULT_WINDOW_WIDTH, newHeight / DEFAULT_WINDOW_HEIGHT);
		win.contentGroup.scale.set(scaleRatio, scaleRatio, scaleRatio);

		// Update time series axis lines
		const tsAxisPositions = win.tsAxisLines.geometry.attributes.position.array as Float32Array;
		tsAxisPositions[0] = -newWidth / 2 + TS_MARGIN.left;
		tsAxisPositions[1] = -newHeight / 2 + TS_MARGIN.bottom;
		tsAxisPositions[3] = newWidth / 2 - TS_MARGIN.right;
		tsAxisPositions[4] = -newHeight / 2 + TS_MARGIN.bottom;
		tsAxisPositions[6] = -newWidth / 2 + TS_MARGIN.left;
		tsAxisPositions[7] = -newHeight / 2 + TS_MARGIN.bottom;
		tsAxisPositions[9] = -newWidth / 2 + TS_MARGIN.left;
		tsAxisPositions[10] = newHeight / 2 - TS_MARGIN.top;
		win.tsAxisLines.geometry.attributes.position.needsUpdate = true;

		// Phase 3: Fixed-size tick labels in resize
		const yAxisHeight = newHeight - TS_MARGIN.bottom - TS_MARGIN.top;
		const ryScale = Math.min(40, yAxisHeight);
		win.yTickSprite.scale.set(ryScale * (128 / 512), ryScale, 1);
		win.yTickSprite.position.set(-newWidth / 2 + TS_MARGIN.left / 2 - 1, (TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);
		const xAxisWidth = newWidth - TS_MARGIN.left - TS_MARGIN.right;
		const rxScale = Math.min(40, xAxisWidth);
		win.xTickSprite.scale.set(rxScale, rxScale * (64 / 512), 1);
		win.xTickSprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2, -newHeight / 2 + TS_MARGIN.bottom / 2 - 2, 2);

		updateClipPlanes(win);

		for (let i = 0; i < win.legendSprites.length; i++) {
			win.legendSprites[i].position.set(newWidth / 2 - 5, newHeight / 2 - 4 - i * 3, 1);
		}

		// Reposition port dots
		const contentTop = newHeight / 2 - HEADER_HEIGHT;
		const contentBottom = -newHeight / 2;
		const contentHeight = contentTop - contentBottom;

		for (let i = 0; i < win.outputPorts.length; i++) {
			const port = win.outputPorts[i];
			const t = (i + 1) / (win.outputPorts.length + 1);
			const yPos = contentBottom + t * contentHeight;
			const xPos = newWidth / 2;
			port.mesh.position.set(xPos, yPos, 5);
			port.ring.position.set(xPos, yPos, 5);
			port.hitArea.position.set(xPos, yPos, 4);
			port.label.position.set(xPos + 5, yPos, 5);
		}

		for (let i = 0; i < win.inputPorts.length; i++) {
			const port = win.inputPorts[i];
			const t = (i + 1) / (win.inputPorts.length + 1);
			const yPos = contentBottom + (1 - t) * contentHeight;
			const xPos = -newWidth / 2;
			port.mesh.position.set(xPos, yPos, 5);
			port.ring.position.set(xPos, yPos, 5);
			port.hitArea.position.set(xPos, yPos, 4);
			port.label.position.set(xPos - 5, yPos, 5);
		}

		// Reposition divider line
		if (win.portDividerLine && win.inputPorts.length > win.nstates) {
			const dividerY = contentBottom + (1 - (win.nstates + 0.5) / (win.inputPorts.length + 1)) * contentHeight;
			const divPos = win.portDividerLine.geometry.attributes.position.array as Float32Array;
			divPos[0] = -newWidth / 2 - 1;
			divPos[1] = dividerY;
			divPos[3] = -newWidth / 2 + 6;
			divPos[4] = dividerY;
			win.portDividerLine.geometry.attributes.position.needsUpdate = true;
		}
	}

	function createMinimizedValueSprite(text: string): THREE.Sprite {
		const canvas = document.createElement('canvas');
		const ctx = canvas.getContext('2d')!;
		canvas.width = 256;
		canvas.height = 32;
		ctx.font = `italic 16px ${CANVAS_FONT}`;
		ctx.fillStyle = '#9a8b78';
		ctx.textAlign = 'left';
		ctx.textBaseline = 'middle';
		ctx.fillText(text, 4, canvas.height / 2);
		const texture = new THREE.CanvasTexture(canvas);
		const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
		const sprite = new THREE.Sprite(material);
		sprite.scale.set(16, 3, 1);
		return sprite;
	}

	function updateMinimizedValueSprite(sprite: THREE.Sprite, text: string) {
		const mat = sprite.material as THREE.SpriteMaterial;
		const texture = mat.map!;
		const canvas = texture.image as HTMLCanvasElement;
		const ctx = canvas.getContext('2d')!;
		ctx.clearRect(0, 0, canvas.width, canvas.height);
		ctx.font = `italic 16px ${CANVAS_FONT}`;
		ctx.fillStyle = '#9a8b78';
		ctx.textAlign = 'left';
		ctx.textBaseline = 'middle';
		ctx.fillText(text, 4, canvas.height / 2);
		texture.needsUpdate = true;
	}

	function minimizeWindow(win: SystemWindow, sys: SystemState) {
		win.savedWindowHeight = win.windowHeight;
		win.minimized = true;
		win.paramsExpanded = false;

		// Hide visualization content
		win.contentGroup.visible = false;
		win.timeSeriesGroup.visible = false;
		win.phaseButton.visible = false;
		win.phaseButtonLabel.visible = false;
		win.timeButton.visible = false;
		win.timeButtonLabel.visible = false;
		win.viewCycleButton.visible = false;
		win.viewCycleLabel.visible = false;
		win.editButton.visible = false;
		win.editButtonLabel.visible = false;
		win.resizeHandle.visible = false;

		// Create state value sprites
		const tmpl = algebraic.templateList.find(t => t.id === sys.templateId);
		const stateNames = tmpl?.state_names ?? STATE_NAMES;
		const nstates = sys.nstates;
		const compactHeight = HEADER_HEIGHT + nstates * 3.5 + 4;

		for (let i = 0; i < nstates; i++) {
			const name = stateNames[i] || `v${i}`;
			const val = sys.state[i]?.toFixed(3) ?? '0';
			const sprite = createMinimizedValueSprite(`${name} = ${val}`);
			sprite.position.set(-win.windowWidth / 2 + 12, win.windowHeight / 2 - HEADER_HEIGHT - 2 - i * 3.5, 1);
			win.group.add(sprite);
			win.stateValueSprites.push(sprite);
		}

		// Create caret button at bottom for expanding params
		const params = tmpl?.parameters ?? [];
		if (params.length > 0) {
			const caretGeom = new THREE.PlaneGeometry(4, 3);
			const caretMat = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
			const caretButton = new THREE.Mesh(caretGeom, caretMat);
			const caretY = win.windowHeight / 2 - HEADER_HEIGHT - 2 - nstates * 3.5 - 1;
			caretButton.position.set(-win.windowWidth / 2 + 6, caretY, 1);
			caretButton.userData = { isCaretButton: true, systemId: sys.id };
			win.group.add(caretButton);
			win.caretButton = caretButton;

			const caretLabel = createButtonSprite('\u25B6', false);
			caretLabel.position.set(-win.windowWidth / 2 + 6, caretY, 2);
			caretLabel.scale.set(4, 3, 1);
			win.group.add(caretLabel);
			win.caretLabel = caretLabel;
		}

		resizeWindow(win, win.windowWidth, compactHeight);
		win.lastMinimizedUpdate = 0;
	}

	function restoreWindow(win: SystemWindow) {
		win.minimized = false;
		win.paramsExpanded = false;

		// Show visualization content
		if (win.viewMode === 'phase') {
			win.contentGroup.visible = true;
		} else {
			win.timeSeriesGroup.visible = true;
		}
		win.phaseButton.visible = true;
		win.phaseButtonLabel.visible = true;
		win.timeButton.visible = true;
		win.timeButtonLabel.visible = true;
		win.viewCycleButton.visible = win.nstates >= 3;
		win.viewCycleLabel.visible = win.nstates >= 3;
		win.editButton.visible = true;
		win.editButtonLabel.visible = true;
		win.resizeHandle.visible = true;

		// Remove state value sprites
		for (const s of win.stateValueSprites) {
			win.group.remove(s);
			(s.material as THREE.SpriteMaterial).map?.dispose();
			(s.material as THREE.Material).dispose();
		}
		win.stateValueSprites = [];

		// Remove param value sprites
		for (const s of win.paramValueSprites) {
			win.group.remove(s);
			(s.material as THREE.SpriteMaterial).map?.dispose();
			(s.material as THREE.Material).dispose();
		}
		win.paramValueSprites = [];

		// Remove caret
		if (win.caretButton) {
			win.group.remove(win.caretButton);
			(win.caretButton.material as THREE.Material).dispose();
			win.caretButton.geometry.dispose();
			win.caretButton = null;
		}
		if (win.caretLabel) {
			win.group.remove(win.caretLabel);
			(win.caretLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.caretLabel.material as THREE.Material).dispose();
			win.caretLabel = null;
		}

		resizeWindow(win, win.windowWidth, win.savedWindowHeight);
	}

	function toggleParamsExpanded(win: SystemWindow, sys: SystemState) {
		const tmpl = algebraic.templateList.find(t => t.id === sys.templateId);
		const params = tmpl?.parameters ?? [];
		if (params.length === 0) return;

		win.paramsExpanded = !win.paramsExpanded;

		if (win.paramsExpanded) {
			// Show params
			for (let i = 0; i < params.length; i++) {
				const p = params[i];
				const val = sys.parameters[p.name] ?? p.default;
				const sprite = createMinimizedValueSprite(`${p.name} = ${val.toFixed(3)}`);
				const yBase = win.windowHeight / 2 - HEADER_HEIGHT - 2 - sys.nstates * 3.5 - 2;
				sprite.position.set(-win.windowWidth / 2 + 12, yBase - i * 3.5, 1);
				win.group.add(sprite);
				win.paramValueSprites.push(sprite);
			}
			// Update caret to point down
			if (win.caretLabel) {
				const oldLabel = win.caretLabel;
				const newLabel = createButtonSprite('\u25BC', false);
				newLabel.position.copy(oldLabel.position);
				newLabel.scale.set(4, 3, 1);
				win.group.remove(oldLabel);
				win.group.add(newLabel);
				(oldLabel.material as THREE.SpriteMaterial).map?.dispose();
				(oldLabel.material as THREE.Material).dispose();
				win.caretLabel = newLabel;
			}
			// Expand compact height
			const newHeight = HEADER_HEIGHT + sys.nstates * 3.5 + 4 + params.length * 3.5 + 2;
			resizeWindow(win, win.windowWidth, newHeight);
		} else {
			// Remove params
			for (const s of win.paramValueSprites) {
				win.group.remove(s);
				(s.material as THREE.SpriteMaterial).map?.dispose();
				(s.material as THREE.Material).dispose();
			}
			win.paramValueSprites = [];
			// Update caret to point right
			if (win.caretLabel) {
				const oldLabel = win.caretLabel;
				const newLabel = createButtonSprite('\u25B6', false);
				newLabel.position.copy(oldLabel.position);
				newLabel.scale.set(4, 3, 1);
				win.group.remove(oldLabel);
				win.group.add(newLabel);
				(oldLabel.material as THREE.SpriteMaterial).map?.dispose();
				(oldLabel.material as THREE.Material).dispose();
				win.caretLabel = newLabel;
			}
			const compactHeight = HEADER_HEIGHT + sys.nstates * 3.5 + 4;
			resizeWindow(win, win.windowWidth, compactHeight);
		}
	}

	// Helper to set opacity on all materials in a THREE.Group
	function setGroupOpacity(group: THREE.Group, opacity: number) {
		group.traverse((child) => {
			if (child instanceof THREE.Mesh || child instanceof THREE.Line || child instanceof THREE.LineLoop || child instanceof THREE.LineSegments) {
				const mat = child.material as THREE.Material;
				mat.transparent = true;
				mat.opacity = Math.min(opacity, (mat as any)._baseOpacity ?? mat.opacity);
				if ((mat as any)._baseOpacity === undefined) {
					(mat as any)._baseOpacity = mat.opacity;
				}
				mat.opacity = opacity * ((mat as any)._baseOpacity ?? 1);
			} else if (child instanceof THREE.Sprite) {
				const mat = child.material as THREE.SpriteMaterial;
				if ((mat as any)._baseOpacity === undefined) {
					(mat as any)._baseOpacity = mat.opacity;
				}
				mat.opacity = opacity * ((mat as any)._baseOpacity ?? 1);
			}
		});
	}

	function restoreGroupOpacity(group: THREE.Group) {
		group.traverse((child) => {
			if (child instanceof THREE.Mesh || child instanceof THREE.Line || child instanceof THREE.LineLoop || child instanceof THREE.LineSegments) {
				const mat = child.material as THREE.Material;
				if ((mat as any)._baseOpacity !== undefined) {
					mat.opacity = (mat as any)._baseOpacity;
				}
			} else if (child instanceof THREE.Sprite) {
				const mat = child.material as THREE.SpriteMaterial;
				if ((mat as any)._baseOpacity !== undefined) {
					mat.opacity = (mat as any)._baseOpacity;
				}
			}
		});
	}

	function createCompositeWindow(group: CompositeGroup): CompositeWindow {
		const color = new THREE.Color(0x4d3d2e);

		// Compute bounding box from member system positions
		let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
		for (const memberId of group.memberSystemIds) {
			const win = systemWindows.get(memberId);
			if (win) {
				const gx = win.group.position.x;
				const gy = win.group.position.y;
				const halfW = win.windowWidth / 2;
				const halfH = (win.windowHeight + HEADER_HEIGHT) / 2;
				minX = Math.min(minX, gx - halfW);
				maxX = Math.max(maxX, gx + halfW);
				minY = Math.min(minY, gy - halfH);
				maxY = Math.max(maxY, gy + halfH);
			}
		}
		const padding = 8;
		minX -= padding; maxX += padding;
		minY -= padding; maxY += padding;
		const winWidth = maxX - minX;
		const winHeight = maxY - minY - HEADER_HEIGHT;
		const cx = (minX + maxX) / 2;
		const cy = (minY + maxY) / 2;

		const threeGroup = new THREE.Group();
		threeGroup.position.set(cx, cy, -1);
		threeGroup.userData = { compositeId: group.id };

		// Dashed border (rounded corners)
		const borderMat = new THREE.LineDashedMaterial({
			color: 0x4d3d2e, dashSize: 2, gapSize: 1,
			transparent: true, opacity: 0.6
		});
		const border = createRoundedRectBorder(winWidth, winHeight + HEADER_HEIGHT, CORNER_RADIUS, borderMat);
		threeGroup.add(border);

		// Header bar (rounded top)
		const headerShape = createHeaderShape(winWidth, HEADER_HEIGHT, CORNER_RADIUS);
		const headerGeom = new THREE.ShapeGeometry(headerShape);
		const headerMat = new THREE.MeshBasicMaterial({
			color: 0x3a2e24, transparent: true, opacity: 0.3, side: THREE.DoubleSide
		});
		const header = new THREE.Mesh(headerGeom, headerMat);
		header.position.set(0, winHeight / 2, 0);
		header.userData = { isCompositeHeader: true, compositeId: group.id };
		threeGroup.add(header);

		// Header label
		const headerLabel = createTextSprite(group.name, color);
		headerLabel.position.set(-winWidth / 2 + 16, winHeight / 2, 1);
		threeGroup.add(headerLabel);

		// Close button
		const closeBtnGeom = new THREE.PlaneGeometry(3, 3);
		const closeBtnMat = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const closeButton = new THREE.Mesh(closeBtnGeom, closeBtnMat);
		closeButton.position.set(-winWidth / 2 + 3, winHeight / 2, 1);
		closeButton.userData = { isCompositeCloseButton: true, compositeId: group.id };
		threeGroup.add(closeButton);

		const closeButtonLabel = createButtonSprite('x', false);
		closeButtonLabel.position.set(-winWidth / 2 + 3, winHeight / 2, 2);
		closeButtonLabel.scale.set(4, 3.5, 1);
		threeGroup.add(closeButtonLabel);

		// Minimize button
		const minBtnGeom = new THREE.PlaneGeometry(3, 3);
		const minBtnMat = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const minimizeButton = new THREE.Mesh(minBtnGeom, minBtnMat);
		minimizeButton.position.set(-winWidth / 2 + 7, winHeight / 2, 1);
		minimizeButton.userData = { isCompositeMinimizeButton: true, compositeId: group.id };
		threeGroup.add(minimizeButton);

		const minimizeButtonLabel = createButtonSprite('\u2014', false);
		minimizeButtonLabel.position.set(-winWidth / 2 + 7, winHeight / 2, 2);
		minimizeButtonLabel.scale.set(4, 3.5, 1);
		threeGroup.add(minimizeButtonLabel);

		// Look-inside button (eye icon)
		const lookBtnGeom = new THREE.PlaneGeometry(6, 3);
		const lookBtnMat = new THREE.MeshBasicMaterial({ color: 0x2a2118, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const lookInsideButton = new THREE.Mesh(lookBtnGeom, lookBtnMat);
		lookInsideButton.position.set(winWidth / 2 - 6, winHeight / 2, 1);
		lookInsideButton.userData = { isLookInsideButton: true, compositeId: group.id };
		threeGroup.add(lookInsideButton);

		const lookInsideLabel = createButtonSprite('\u25C9', group.lookInside);
		lookInsideLabel.position.set(winWidth / 2 - 6, winHeight / 2, 2);
		lookInsideLabel.scale.set(4, 3.5, 1);
		threeGroup.add(lookInsideLabel);

		// Get free states (unconstrained by internal wires)
		const freeStates = algebraic.getCompositeFreeStateInfo(group.id);
		const combinedNstates = freeStates.length;

		// Time series visualization for FREE states only
		const timeSeriesGroup = new THREE.Group();
		timeSeriesGroup.position.set(0, -HEADER_HEIGHT / 2, 0);
		timeSeriesGroup.visible = !group.lookInside;
		threeGroup.add(timeSeriesGroup);

		const timeSeriesLines: THREE.Line[] = [];
		const timeSeriesGeometries: THREE.BufferGeometry[] = [];
		const legendSprites: THREE.Sprite[] = [];

		for (let fi = 0; fi < freeStates.length; fi++) {
			const fs = freeStates[fi];
			const tsGeom = new THREE.BufferGeometry();
			const tsPos = new Float32Array(TIME_WINDOW_SAMPLES * 3);
			tsGeom.setAttribute('position', new THREE.BufferAttribute(tsPos, 3));
			tsGeom.setDrawRange(0, 0);
			const stateColor = STATE_COLORS[fi % STATE_COLORS.length];
			const tsMat = new THREE.LineBasicMaterial({ color: stateColor, transparent: true, opacity: 0.9 });
			const tsLine = new THREE.Line(tsGeom, tsMat);
			timeSeriesGroup.add(tsLine);
			timeSeriesLines.push(tsLine);
			timeSeriesGeometries.push(tsGeom);

			const legend = createLegendSprite(fs.name, stateColor);
			legend.position.set(winWidth / 2 - 5, winHeight / 2 - 4 - fi * 3, 1);
			timeSeriesGroup.add(legend);
			legendSprites.push(legend);
		}

		// Axis lines
		const tsAxisGeom = new THREE.BufferGeometry();
		const tsAxisPts = [
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			winWidth / 2 - TS_MARGIN.right, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, winHeight / 2 - TS_MARGIN.top, 0,
		];
		tsAxisGeom.setAttribute('position', new THREE.Float32BufferAttribute(tsAxisPts, 3));
		const tsAxisMat = new THREE.LineBasicMaterial({ color: 0x3a2e24, transparent: true, opacity: 0.5 });
		const tsAxisLines = new THREE.LineSegments(tsAxisGeom, tsAxisMat);
		timeSeriesGroup.add(tsAxisLines);

		const yTickSprite = createTickSprite(128, 512);
		const cwInitYAxisH = winHeight - TS_MARGIN.bottom - TS_MARGIN.top;
		const cwInitYScale = Math.min(40, cwInitYAxisH);
		yTickSprite.scale.set(cwInitYScale * (128 / 512), cwInitYScale, 1);
		yTickSprite.position.set(-winWidth / 2 + TS_MARGIN.left / 2, (TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);
		timeSeriesGroup.add(yTickSprite);

		const xTickSprite = createTickSprite(512, 64);
		const cwInitXAxisW = winWidth - TS_MARGIN.left - TS_MARGIN.right;
		const cwInitXScale = Math.min(40, cwInitXAxisW);
		xTickSprite.scale.set(cwInitXScale, cwInitXScale * (64 / 512), 1);
		xTickSprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2, -winHeight / 2 + TS_MARGIN.bottom / 2 - 1, 2);
		timeSeriesGroup.add(xTickSprite);

		// Content group (for future phase view)
		const contentGroup = new THREE.Group();
		contentGroup.position.set(0, -HEADER_HEIGHT / 2, 0);
		contentGroup.visible = false;
		// TODO: subspace projection for >3D composite phase view
		threeGroup.add(contentGroup);

		scene.add(threeGroup);

		return {
			id: group.id,
			group: threeGroup, border, header, headerLabel,
			closeButton, closeButtonLabel,
			minimizeButton, minimizeButtonLabel,
			lookInsideButton, lookInsideLabel,
			contentGroup, timeSeriesGroup,
			timeSeriesLines, timeSeriesGeometries,
			tsAxisLines, yTickSprite, xTickSprite,
			legendSprites,
			windowWidth: winWidth, windowHeight: winHeight,
			lookInside: group.lookInside,
			minimized: group.minimized,
			combinedNstates,
			memberWindowIds: [...group.memberSystemIds],
			color,
			tsYMin: Infinity, tsYMax: -Infinity,
			freeStates,
			ghostWires: [],
			cachedName: group.name,
			skeletonGroup: null,
			compositePorts: []
		};
	}

	function updateCompositeWindowBounds(cw: CompositeWindow) {
		// Recompute bounding box from member positions
		let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
		for (const memberId of cw.memberWindowIds) {
			const win = systemWindows.get(memberId);
			if (win) {
				const gx = win.group.position.x;
				const gy = win.group.position.y;
				const halfW = win.windowWidth / 2;
				const halfH = (win.windowHeight + HEADER_HEIGHT) / 2;
				minX = Math.min(minX, gx - halfW);
				maxX = Math.max(maxX, gx + halfW);
				minY = Math.min(minY, gy - halfH);
				maxY = Math.max(maxY, gy + halfH);
			}
		}
		const padding = 8;
		minX -= padding; maxX += padding;
		minY -= padding; maxY += padding;
		const newW = maxX - minX;
		const newH = maxY - minY - HEADER_HEIGHT;
		const cx = (minX + maxX) / 2;
		const cy = (minY + maxY) / 2;

		cw.group.position.set(cx, cy, -1);
		cw.windowWidth = newW;
		cw.windowHeight = newH;

		// Update border (rounded corners)
		cw.group.remove(cw.border);
		cw.border.geometry.dispose();
		(cw.border.material as THREE.Material).dispose();
		const newCwBorderMat = new THREE.LineDashedMaterial({
			color: 0x4d3d2e, dashSize: 2, gapSize: 1,
			transparent: true, opacity: 0.6
		});
		cw.border = createRoundedRectBorder(newW, newH + HEADER_HEIGHT, CORNER_RADIUS, newCwBorderMat);
		cw.group.add(cw.border);

		// Update header (rounded top)
		cw.header.geometry.dispose();
		const newCwHeaderShape = createHeaderShape(newW, HEADER_HEIGHT, CORNER_RADIUS);
		cw.header.geometry = new THREE.ShapeGeometry(newCwHeaderShape);
		cw.header.position.set(0, newH / 2, 0);

		cw.headerLabel.position.set(-newW / 2 + 16, newH / 2, 1);
		cw.closeButton.position.set(-newW / 2 + 3, newH / 2, 1);
		cw.closeButtonLabel.position.set(-newW / 2 + 3, newH / 2, 2);
		cw.minimizeButton.position.set(-newW / 2 + 7, newH / 2, 1);
		cw.minimizeButtonLabel.position.set(-newW / 2 + 7, newH / 2, 2);
		cw.lookInsideButton.position.set(newW / 2 - 6, newH / 2, 1);
		cw.lookInsideLabel.position.set(newW / 2 - 6, newH / 2, 2);
	}

	function updateCompositeTimeSeries(cw: CompositeWindow) {
		const group = algebraic.compositeGroups.find(g => g.id === cw.id);
		if (!group) return;

		const history = algebraic.getCompositeHistory(cw.id);
		const timeHistory = algebraic.getCompositeTimeHistory(cw.id);
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
			for (let v = 0; v < cw.combinedNstates && v < state.length; v++) {
				if (Number.isFinite(state[v])) {
					frameMin = Math.min(frameMin, state[v]);
					frameMax = Math.max(frameMax, state[v]);
				}
			}
		}

		if (frameMin < cw.tsYMin) cw.tsYMin = frameMin;
		if (frameMax > cw.tsYMax) cw.tsYMax = frameMax;

		const winW = cw.windowWidth;
		const winH = cw.windowHeight;
		const xMin = -winW / 2 + TS_MARGIN.left;
		const xMax = winW / 2 - TS_MARGIN.right;
		const yMin = -winH / 2 + TS_MARGIN.bottom;
		const yMax = winH / 2 - TS_MARGIN.top;
		const xRange = xMax - xMin;
		const yRange = yMax - yMin;
		const dataRange = cw.tsYMax - cw.tsYMin;

		for (let v = 0; v < cw.combinedNstates && v < cw.timeSeriesGeometries.length; v++) {
			const geometry = cw.timeSeriesGeometries[v];
			const posArr = geometry.attributes.position.array as Float32Array;

			for (let i = 0; i < len && i < TIME_WINDOW_SAMPLES; i++) {
				const state = visibleHistory[i];
				const value = v < state.length ? state[v] : 0;
				const x = xMin + (i / (TIME_WINDOW_SAMPLES - 1)) * xRange;
				const y = yMin + ((value - cw.tsYMin) / (dataRange > 0.001 ? dataRange : 1)) * yRange * 0.9 + yRange * 0.05;
				posArr[i * 3] = x;
				posArr[i * 3 + 1] = Number.isFinite(y) ? y : yMin;
				posArr[i * 3 + 2] = 0;
			}

			geometry.attributes.position.needsUpdate = true;
			geometry.setDrawRange(0, len);
		}

		updateYTickSprite(cw.yTickSprite, cw.tsYMin, cw.tsYMax, winW, winH);
		updateXTickSprite(cw.xTickSprite, startTime, endTime, winW, winH);
	}

	function clearGhostWires(cw: CompositeWindow) {
		for (const gw of cw.ghostWires) {
			cw.group.remove(gw);
			gw.geometry.dispose();
			(gw.material as THREE.Material).dispose();
		}
		cw.ghostWires = [];
	}

	function rebuildGhostWires(cw: CompositeWindow) {
		clearGhostWires(cw);
		if (!cw.lookInside) return;

		// For each member's unconnected input ports, draw a dashed line from the
		// composite border edge to the inner port position.
		const group = algebraic.compositeGroups.find(g => g.id === cw.id);
		if (!group) return;
		const memberSet = new Set(group.memberSystemIds);

		// Collect internally-connected input ports
		const connectedInputs = new Set<string>();
		for (const wire of algebraic.wireList) {
			if (memberSet.has(wire.fromSystem) && memberSet.has(wire.toSystem)) {
				connectedInputs.add(`${wire.toSystem}:${wire.toPort}`);
			}
		}

		for (const memberId of group.memberSystemIds) {
			const win = systemWindows.get(memberId);
			if (!win) continue;
			const sys = algebraic.systemList.find(s => s.id === memberId);
			if (!sys) continue;

			for (const port of win.inputPorts) {
				// Skip internally connected ports and param inputs
				if (connectedInputs.has(`${memberId}:${port.portIndex}`)) continue;
				if (port.portIndex > sys.nstates) continue; // Only state inputs get ghost wires

				// Port world position relative to composite group
				const portWorldX = win.group.position.x + port.mesh.position.x;
				const portWorldY = win.group.position.y + port.mesh.position.y;
				// Composite local position
				const localX = portWorldX - cw.group.position.x;
				const localY = portWorldY - cw.group.position.y;
				// Border left edge
				const borderX = -cw.windowWidth / 2;

				const geom = new THREE.BufferGeometry().setFromPoints([
					new THREE.Vector3(borderX, localY, 3),
					new THREE.Vector3(localX, localY, 3)
				]);
				const mat = new THREE.LineDashedMaterial({
					color: 0x9a8b78, dashSize: 1, gapSize: 1,
					transparent: true, opacity: 0.4
				});
				const line = new THREE.Line(geom, mat);
				line.computeLineDistances();
				cw.group.add(line);
				cw.ghostWires.push(line);
			}
		}
	}

	// Rebuild the time series objects when free states change (membership or wiring change)
	function rebuildCompositeTimeSeries(cw: CompositeWindow) {
		const group = algebraic.compositeGroups.find(g => g.id === cw.id);
		if (!group) return;

		// Clean up old time series
		for (const geom of cw.timeSeriesGeometries) geom.dispose();
		for (const line of cw.timeSeriesLines) {
			cw.timeSeriesGroup.remove(line);
			(line.material as THREE.Material).dispose();
		}
		for (const sprite of cw.legendSprites) {
			cw.timeSeriesGroup.remove(sprite);
			(sprite.material as THREE.SpriteMaterial).map?.dispose();
			(sprite.material as THREE.Material).dispose();
		}
		cw.timeSeriesLines = [];
		cw.timeSeriesGeometries = [];
		cw.legendSprites = [];

		// Rebuild with new free states
		const freeStates = algebraic.getCompositeFreeStateInfo(group.id);
		cw.freeStates = freeStates;
		cw.combinedNstates = freeStates.length;
		cw.tsYMin = Infinity;
		cw.tsYMax = -Infinity;

		const winWidth = cw.windowWidth;
		const winHeight = cw.windowHeight;

		for (let fi = 0; fi < freeStates.length; fi++) {
			const fs = freeStates[fi];
			const tsGeom = new THREE.BufferGeometry();
			const tsPos = new Float32Array(TIME_WINDOW_SAMPLES * 3);
			tsGeom.setAttribute('position', new THREE.BufferAttribute(tsPos, 3));
			tsGeom.setDrawRange(0, 0);
			const stateColor = STATE_COLORS[fi % STATE_COLORS.length];
			const tsMat = new THREE.LineBasicMaterial({ color: stateColor, transparent: true, opacity: 0.9 });
			const tsLine = new THREE.Line(tsGeom, tsMat);
			cw.timeSeriesGroup.add(tsLine);
			cw.timeSeriesLines.push(tsLine);
			cw.timeSeriesGeometries.push(tsGeom);

			const legend = createLegendSprite(fs.name, stateColor);
			legend.position.set(winWidth / 2 - 5, winHeight / 2 - 4 - fi * 3, 1);
			cw.timeSeriesGroup.add(legend);
			cw.legendSprites.push(legend);
		}
	}

	// Phase 5: Build skeleton overlay showing member boxes and internal wires
	function buildSkeletonOverlay(cw: CompositeWindow) {
		clearSkeletonOverlay(cw);
		const group = algebraic.compositeGroups.find(g => g.id === cw.id);
		if (!group) return;

		const skGroup = new THREE.Group();
		skGroup.position.set(0, -HEADER_HEIGHT / 2, 1);

		const members = group.memberSystemIds;
		const boxW = 10, boxH = 7;
		const cols = Math.max(1, Math.floor(cw.windowWidth / (boxW + 4)));
		const startX = -((Math.min(members.length, cols) - 1) * (boxW + 4)) / 2;
		const startY = (cw.windowHeight / 2 - HEADER_HEIGHT - 6);

		const memberPositions = new Map<string, { x: number; y: number }>();

		for (let i = 0; i < members.length; i++) {
			const memberId = members[i];
			const sys = algebraic.systemList.find(s => s.id === memberId);
			if (!sys) continue;

			const col = i % cols;
			const row = Math.floor(i / cols);
			const bx = startX + col * (boxW + 4);
			const by = startY - row * (boxH + 3);

			memberPositions.set(memberId, { x: bx, y: by });

			// Small rounded box
			const boxBorder = createRoundedRectBorder(boxW, boxH, 1.5,
				new THREE.LineBasicMaterial({ color: 0x4d3d2e, transparent: true, opacity: 0.5 })
			);
			boxBorder.position.set(bx, by, 0);
			skGroup.add(boxBorder);

			// Label
			const sysName = sys.templateId.split('_')[0];
			const win = systemWindows.get(memberId);
			const labelColor = win ? win.color : new THREE.Color(0x9a8b78);
			const label = createTextSprite(sysName, labelColor);
			label.scale.set(8, 2.5, 1);
			label.position.set(bx, by, 1);
			skGroup.add(label);
		}

		// Draw internal wire connections as thin lines between boxes
		for (const wire of algebraic.wireList) {
			if (group.internalWireIds.includes(wire.id)) {
				const fromPos = memberPositions.get(wire.fromSystem);
				const toPos = memberPositions.get(wire.toSystem);
				if (fromPos && toPos) {
					const geom = new THREE.BufferGeometry().setFromPoints([
						new THREE.Vector3(fromPos.x + boxW / 2, fromPos.y, 0.5),
						new THREE.Vector3(toPos.x - boxW / 2, toPos.y, 0.5)
					]);
					const mat = new THREE.LineBasicMaterial({
						color: 0x9a8b78, transparent: true, opacity: 0.3
					});
					skGroup.add(new THREE.Line(geom, mat));
				}
			}
		}

		cw.group.add(skGroup);
		cw.skeletonGroup = skGroup;
	}

	function clearSkeletonOverlay(cw: CompositeWindow) {
		if (!cw.skeletonGroup) return;
		cw.group.remove(cw.skeletonGroup);
		cw.skeletonGroup.traverse((child) => {
			if (child instanceof THREE.Mesh || child instanceof THREE.Line || child instanceof THREE.LineLoop) {
				child.geometry.dispose();
				(child.material as THREE.Material).dispose();
			} else if (child instanceof THREE.Sprite) {
				(child.material as THREE.SpriteMaterial).map?.dispose();
				(child.material as THREE.Material).dispose();
			}
		});
		cw.skeletonGroup = null;
	}

	// Phase 6: Build composite ports on the outer box border
	function buildCompositePorts(cw: CompositeWindow) {
		clearCompositePorts(cw);
		const group = algebraic.compositeGroups.find(g => g.id === cw.id);
		if (!group) return;

		const memberSet = new Set(group.memberSystemIds);
		// Collect internally connected ports
		const internalInputs = new Set<string>();
		const internalOutputs = new Set<string>();
		for (const wire of algebraic.wireList) {
			if (memberSet.has(wire.fromSystem) && memberSet.has(wire.toSystem)) {
				internalInputs.add(`${wire.toSystem}:${wire.toPort}`);
				internalOutputs.add(`${wire.fromSystem}:${wire.fromPort}`);
			}
		}

		const externalInputs: { memberId: string; portIndex: number; color: THREE.Color }[] = [];
		const externalOutputs: { memberId: string; portIndex: number; color: THREE.Color }[] = [];

		for (const memberId of group.memberSystemIds) {
			const sys = algebraic.systemList.find(s => s.id === memberId);
			const win = systemWindows.get(memberId);
			if (!sys || !win) continue;

			// External inputs: not connected internally
			for (const port of win.inputPorts) {
				if (port.portIndex > sys.nstates) continue; // Only state inputs
				if (!internalInputs.has(`${memberId}:${port.portIndex}`)) {
					externalInputs.push({ memberId, portIndex: port.portIndex, color: win.color });
				}
			}

			// External outputs: not connected internally
			for (const port of win.outputPorts) {
				if (!internalOutputs.has(`${memberId}:${port.portIndex}`)) {
					externalOutputs.push({ memberId, portIndex: port.portIndex, color: win.color });
				}
			}
		}

		const contentTop = cw.windowHeight / 2 - HEADER_HEIGHT;
		const contentBottom = -cw.windowHeight / 2;
		const contentHeight = contentTop - contentBottom;

		// Place external inputs on left border
		for (let i = 0; i < externalInputs.length; i++) {
			const ei = externalInputs[i];
			const t = (i + 1) / (externalInputs.length + 1);
			const yPos = contentBottom + t * contentHeight;
			const port = createPortDot(
				ei.memberId, ei.portIndex, false,
				`in${ei.portIndex}`, ei.color,
				-cw.windowWidth / 2, yPos
			);
			cw.group.add(port.mesh);
			cw.group.add(port.ring);
			cw.group.add(port.hitArea);
			cw.group.add(port.label);
			cw.compositePorts.push({
				memberSystemId: ei.memberId,
				portIndex: ei.portIndex,
				isOutput: false,
				mesh: port.mesh,
				ring: port.ring,
				hitArea: port.hitArea,
				label: port.label
			});
		}

		// Place external outputs on right border
		for (let i = 0; i < externalOutputs.length; i++) {
			const eo = externalOutputs[i];
			const t = (i + 1) / (externalOutputs.length + 1);
			const yPos = contentBottom + t * contentHeight;
			const port = createPortDot(
				eo.memberId, eo.portIndex, true,
				`out${eo.portIndex}`, eo.color,
				cw.windowWidth / 2, yPos
			);
			cw.group.add(port.mesh);
			cw.group.add(port.ring);
			cw.group.add(port.hitArea);
			cw.group.add(port.label);
			cw.compositePorts.push({
				memberSystemId: eo.memberId,
				portIndex: eo.portIndex,
				isOutput: true,
				mesh: port.mesh,
				ring: port.ring,
				hitArea: port.hitArea,
				label: port.label
			});
		}
	}

	function clearCompositePorts(cw: CompositeWindow) {
		for (const cp of cw.compositePorts) {
			cw.group.remove(cp.mesh);
			cw.group.remove(cp.ring);
			cw.group.remove(cp.hitArea);
			cw.group.remove(cp.label);
			cp.mesh.geometry.dispose();
			(cp.mesh.material as THREE.Material).dispose();
			cp.ring.geometry.dispose();
			(cp.ring.material as THREE.Material).dispose();
			cp.hitArea.geometry.dispose();
			(cp.hitArea.material as THREE.Material).dispose();
			(cp.label.material as THREE.SpriteMaterial).map?.dispose();
			(cp.label.material as THREE.Material).dispose();
		}
		cw.compositePorts = [];
	}

	function removeCompositeWindow(id: string) {
		const cw = compositeWindows.get(id);
		if (!cw) return;

		clearGhostWires(cw);
		clearSkeletonOverlay(cw);
		clearCompositePorts(cw);
		scene.remove(cw.group);
		// Dispose geometries and materials
		cw.border.geometry.dispose();
		(cw.border.material as THREE.Material).dispose();
		cw.header.geometry.dispose();
		(cw.header.material as THREE.Material).dispose();
		(cw.headerLabel.material as THREE.SpriteMaterial).map?.dispose();
		(cw.headerLabel.material as THREE.Material).dispose();
		cw.closeButton.geometry.dispose();
		(cw.closeButton.material as THREE.Material).dispose();
		(cw.closeButtonLabel.material as THREE.SpriteMaterial).map?.dispose();
		(cw.closeButtonLabel.material as THREE.Material).dispose();
		cw.minimizeButton.geometry.dispose();
		(cw.minimizeButton.material as THREE.Material).dispose();
		(cw.minimizeButtonLabel.material as THREE.SpriteMaterial).map?.dispose();
		(cw.minimizeButtonLabel.material as THREE.Material).dispose();
		cw.lookInsideButton.geometry.dispose();
		(cw.lookInsideButton.material as THREE.Material).dispose();
		(cw.lookInsideLabel.material as THREE.SpriteMaterial).map?.dispose();
		(cw.lookInsideLabel.material as THREE.Material).dispose();
		for (const geom of cw.timeSeriesGeometries) geom.dispose();
		for (const line of cw.timeSeriesLines) (line.material as THREE.Material).dispose();
		for (const sprite of cw.legendSprites) {
			(sprite.material as THREE.SpriteMaterial).map?.dispose();
			(sprite.material as THREE.Material).dispose();
		}
		cw.tsAxisLines.geometry.dispose();
		(cw.tsAxisLines.material as THREE.Material).dispose();
		(cw.yTickSprite.material as THREE.SpriteMaterial).map?.dispose();
		(cw.yTickSprite.material as THREE.Material).dispose();
		(cw.xTickSprite.material as THREE.SpriteMaterial).map?.dispose();
		(cw.xTickSprite.material as THREE.Material).dispose();

		// Restore member opacity
		for (const memberId of cw.memberWindowIds) {
			const win = systemWindows.get(memberId);
			if (win) restoreGroupOpacity(win.group);
		}

		compositeWindows.delete(id);
	}

	function toggleLookInside(cw: CompositeWindow) {
		cw.lookInside = !cw.lookInside;

		// Also update the store's group state so it persists across $effect cycles
		const group = algebraic.compositeGroups.find(g => g.id === cw.id);
		if (group) {
			group.lookInside = cw.lookInside;
		}

		if (cw.lookInside) {
			// Enter look-inside mode: show inner systems, hide skeleton + outer time series
			cw.timeSeriesGroup.visible = false;
			cw.contentGroup.visible = false;
			clearSkeletonOverlay(cw);
			// Restore member systems to full opacity
			for (const memberId of cw.memberWindowIds) {
				const win = systemWindows.get(memberId);
				if (win) restoreGroupOpacity(win.group);
			}
			// Internal wires full opacity
			for (const wire of algebraic.wireList) {
				if (algebraic.isInternalWire(wire.id)) {
					const wv = wireVisuals.get(wire.id);
					if (wv) {
						(wv.line.material as THREE.LineBasicMaterial).opacity = 0.7;
						(wv.valueLine.material as THREE.LineBasicMaterial).opacity = 0.4;
					}
				}
			}
			// Build ghost wires
			rebuildGhostWires(cw);
			// Hide composite ports
			for (const cp of cw.compositePorts) {
				cp.mesh.visible = false;
				cp.ring.visible = false;
				cp.hitArea.visible = false;
				cp.label.visible = false;
			}
		} else {
			// Exit look-inside mode: skeleton view + time series, hide members completely
			cw.timeSeriesGroup.visible = true;
			// Hide members completely (not dim)
			for (const memberId of cw.memberWindowIds) {
				const win = systemWindows.get(memberId);
				if (win) setGroupOpacity(win.group, 0);
			}
			// Internal wires hidden
			for (const wire of algebraic.wireList) {
				if (algebraic.isInternalWire(wire.id)) {
					const wv = wireVisuals.get(wire.id);
					if (wv) {
						(wv.line.material as THREE.LineBasicMaterial).opacity = 0;
						(wv.valueLine.material as THREE.LineBasicMaterial).opacity = 0;
					}
				}
			}
			// Remove ghost wires, build skeleton
			clearGhostWires(cw);
			buildSkeletonOverlay(cw);
			// Show composite ports
			for (const cp of cw.compositePorts) {
				cp.mesh.visible = true;
				cp.ring.visible = true;
				cp.hitArea.visible = true;
				cp.label.visible = true;
			}
		}

		// Update button label
		const oldLabel = cw.lookInsideLabel;
		const newLabel = createButtonSprite('\u25C9', cw.lookInside);
		newLabel.position.copy(oldLabel.position);
		newLabel.scale.set(4, 3.5, 1);
		cw.group.remove(oldLabel);
		cw.group.add(newLabel);
		(oldLabel.material as THREE.SpriteMaterial).map?.dispose();
		(oldLabel.material as THREE.Material).dispose();
		cw.lookInsideLabel = newLabel;
	}

	function updateSystemWindow(id: string, sys: SystemState) {
		const win = getOrCreateWindow(id, sys);
		const history = algebraic.getHistory(id);

		if (history.length === 0) return;

		// Minimized: update state value sprites at reduced rate
		if (win.minimized) {
			const now = performance.now();
			if (now - win.lastMinimizedUpdate > 100) {
				win.lastMinimizedUpdate = now;
				const tmpl = algebraic.templateList.find(t => t.id === sys.templateId);
				const stateNames = tmpl?.state_names ?? STATE_NAMES;
				for (let i = 0; i < win.stateValueSprites.length && i < sys.state.length; i++) {
					const name = stateNames[i] || `v${i}`;
					updateMinimizedValueSprite(win.stateValueSprites[i], `${name} = ${sys.state[i]?.toFixed(3) ?? '0'}`);
				}
			}
			return;
		}

		recomputeBounds(win, history, sys.state);

		const winW = win.windowWidth;
		const winH = win.windowHeight;

		if (win.viewMode === 'phase') {
			// === Phase Space View ===
			const phaseHistory = history.filter((state) => isFiniteState(state, sys.nstates));
			const len = Math.min(phaseHistory.length, MAX_POINTS);
			if (len === 0) return;
			const localPos = mapStateToLocal(sys.state, win, sys.nstates);
			win.point.position.copy(localPos);

			const positions = win.trailGeometry.attributes.position.array as Float32Array;
			const trailColors = win.trailGeometry.attributes.color.array as Float32Array;

			for (let i = 0; i < len; i++) {
				const state = phaseHistory[phaseHistory.length - len + i];
				const pos = mapStateToLocal(state, win, sys.nstates);

				positions[i * 3] = pos.x;
				positions[i * 3 + 1] = pos.y;
				positions[i * 3 + 2] = pos.z;

				const t = i / len;
				const intensity = 0.4 + t * 0.6;
				trailColors[i * 3] = win.color.r * intensity;
				trailColors[i * 3 + 1] = win.color.g * intensity;
				trailColors[i * 3 + 2] = win.color.b * intensity;
			}

			win.trailGeometry.attributes.position.needsUpdate = true;
			win.trailGeometry.attributes.color.needsUpdate = true;
			win.trailGeometry.setDrawRange(0, len);

			// A4: Update phase tick marks
			updatePhaseTickMarks(win);
		} else {
			// === Time Series View ===
			const timeHistory = algebraic.getTimeHistory(id);
			const startIdx = Math.max(0, history.length - TIME_WINDOW_SAMPLES);
			const visibleHistory = history.slice(startIdx);
			const visibleTimes = timeHistory.slice(startIdx);
			const len = visibleHistory.length;

			if (len === 0) return;

			// A2: Real timestamps for X axis
			const startTime = visibleTimes.length > 0 ? visibleTimes[0] : 0;
			const endTime = visibleTimes.length > 0 ? visibleTimes[visibleTimes.length - 1] : 0;

			let frameMin = Infinity;
			let frameMax = -Infinity;
			for (const state of visibleHistory) {
				for (let v = 0; v < win.nstates && v < state.length; v++) {
					frameMin = Math.min(frameMin, state[v]);
					frameMax = Math.max(frameMax, state[v]);
				}
			}

			if (frameMin < win.tsYMin) win.tsYMin = frameMin;
			if (frameMax > win.tsYMax) win.tsYMax = frameMax;
			const yDataMin = win.tsYMin;
			const yDataMax = win.tsYMax;

			const xMin = -winW / 2 + TS_MARGIN.left;
			const xMax = winW / 2 - TS_MARGIN.right;
			const yMin = -winH / 2 + TS_MARGIN.bottom;
			const yMax = winH / 2 - TS_MARGIN.top;
			const xRange = xMax - xMin;
			const yRange = yMax - yMin;

			const dataRange = yDataMax - yDataMin;

			for (let v = 0; v < win.nstates; v++) {
				const geometry = win.timeSeriesGeometries[v];
				if (!geometry) continue;

				const posArr = geometry.attributes.position.array as Float32Array;

				for (let i = 0; i < len && i < TIME_WINDOW_SAMPLES; i++) {
					const state = visibleHistory[i];
					const value = v < state.length ? state[v] : 0;

					const x = xMin + (i / (TIME_WINDOW_SAMPLES - 1)) * xRange;
					const y = yMin + ((value - yDataMin) / (dataRange > 0.001 ? dataRange : 1)) * yRange * 0.9 + yRange * 0.05;

					posArr[i * 3] = x;
					posArr[i * 3 + 1] = y;
					posArr[i * 3 + 2] = 0;
				}

				geometry.attributes.position.needsUpdate = true;
				geometry.setDrawRange(0, len);
			}

			updateYTickSprite(win.yTickSprite, yDataMin, yDataMax, winW, winH);
			updateXTickSprite(win.xTickSprite, startTime, endTime, winW, winH);
		}
	}

	function removeSystemWindow(id: string) {
		const win = systemWindows.get(id);
		if (win) {
			scene.remove(win.group);
			win.trailGeometry.dispose();
			(win.trail.material as THREE.Material).dispose();
			(win.point.material as THREE.Material).dispose();
			(win.point.geometry as THREE.BufferGeometry).dispose();
			for (const geom of win.timeSeriesGeometries) {
				geom.dispose();
			}
			for (const line of win.timeSeriesLines) {
				(line.material as THREE.Material).dispose();
			}
			for (const sprite of win.legendSprites) {
				(sprite.material as THREE.SpriteMaterial).map?.dispose();
				(sprite.material as THREE.Material).dispose();
			}
			(win.header.material as THREE.Material).dispose();
			win.header.geometry.dispose();
			(win.headerLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.headerLabel.material as THREE.Material).dispose();
			(win.phaseButton.material as THREE.Material).dispose();
			win.phaseButton.geometry.dispose();
			(win.phaseButtonLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.phaseButtonLabel.material as THREE.Material).dispose();
			(win.timeButton.material as THREE.Material).dispose();
			win.timeButton.geometry.dispose();
			(win.timeButtonLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.timeButtonLabel.material as THREE.Material).dispose();
			(win.viewCycleButton.material as THREE.Material).dispose();
			win.viewCycleButton.geometry.dispose();
			(win.viewCycleLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.viewCycleLabel.material as THREE.Material).dispose();
			(win.resizeHandle.material as THREE.Material).dispose();
			win.resizeHandle.geometry.dispose();
			(win.editButton.material as THREE.Material).dispose();
			win.editButton.geometry.dispose();
			(win.editButtonLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.editButtonLabel.material as THREE.Material).dispose();
			(win.closeButton.material as THREE.Material).dispose();
			win.closeButton.geometry.dispose();
			(win.closeButtonLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.closeButtonLabel.material as THREE.Material).dispose();
			(win.minimizeButton.material as THREE.Material).dispose();
			win.minimizeButton.geometry.dispose();
			(win.minimizeButtonLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.minimizeButtonLabel.material as THREE.Material).dispose();
			// Dispose minimized sprites
			for (const s of win.stateValueSprites) {
				(s.material as THREE.SpriteMaterial).map?.dispose();
				(s.material as THREE.Material).dispose();
			}
			for (const s of win.paramValueSprites) {
				(s.material as THREE.SpriteMaterial).map?.dispose();
				(s.material as THREE.Material).dispose();
			}
			if (win.caretButton) {
				(win.caretButton.material as THREE.Material).dispose();
				win.caretButton.geometry.dispose();
			}
			if (win.caretLabel) {
				(win.caretLabel.material as THREE.SpriteMaterial).map?.dispose();
				(win.caretLabel.material as THREE.Material).dispose();
			}
			// Reset close confirm if this system is being confirmed
			if (closeConfirmId === id) resetCloseConfirm();
			win.border.geometry.dispose();
			(win.border.material as THREE.Material).dispose();
			win.tsAxisLines.geometry.dispose();
			(win.tsAxisLines.material as THREE.Material).dispose();
			(win.yTickSprite.material as THREE.SpriteMaterial).map?.dispose();
			(win.yTickSprite.material as THREE.Material).dispose();
			(win.xTickSprite.material as THREE.SpriteMaterial).map?.dispose();
			(win.xTickSprite.material as THREE.Material).dispose();
			// Dispose phase tick group sprites
			for (const child of win.phaseTickGroup.children) {
				if (child instanceof THREE.Sprite) {
					(child.material as THREE.SpriteMaterial).map?.dispose();
					(child.material as THREE.Material).dispose();
				}
			}
			// Dispose port dots
			const disposePorts = (ports: PortDot[]) => {
				for (const p of ports) {
					p.mesh.geometry.dispose();
					(p.mesh.material as THREE.Material).dispose();
					p.ring.geometry.dispose();
					(p.ring.material as THREE.Material).dispose();
					p.hitArea.geometry.dispose();
					(p.hitArea.material as THREE.Material).dispose();
					(p.label.material as THREE.SpriteMaterial).map?.dispose();
					(p.label.material as THREE.Material).dispose();
				}
			};
			disposePorts(win.outputPorts);
			disposePorts(win.inputPorts);
			if (win.portDividerLine) {
				win.portDividerLine.geometry.dispose();
				(win.portDividerLine.material as THREE.Material).dispose();
			}
			systemWindows.delete(id);
		}
	}

	function getEditorScreenPos(systemId: string): { x: number; y: number } | null {
		const win = systemWindows.get(systemId);
		if (!win || !camera || !canvasEl) return null;
		const rect = canvasEl.getBoundingClientRect();
		// World position of the right edge of the window, header height
		const worldX = win.group.position.x + win.windowWidth / 2 + 2;
		const worldY = win.group.position.y + win.windowHeight / 2;
		// Convert world to screen
		const ndcX = worldX / ((camera.right - camera.left) / 2);
		const ndcY = worldY / ((camera.top - camera.bottom) / 2);
		const screenX = ((ndcX + 1) / 2) * rect.width;
		const screenY = ((1 - ndcY) / 2) * rect.height;
		return { x: screenX, y: screenY };
	}

	function screenToWorld(screenX: number, screenY: number): THREE.Vector2 {
		const rect = canvasEl.getBoundingClientRect();
		const ndcX = ((screenX - rect.left) / rect.width) * 2 - 1;
		const ndcY = -((screenY - rect.top) / rect.height) * 2 + 1;

		const worldX = ndcX * (camera.right - camera.left) / 2;
		const worldY = ndcY * (camera.top - camera.bottom) / 2;

		return new THREE.Vector2(worldX, worldY);
	}

	function onMouseDown(event: MouseEvent) {
		updateMousePosition(event);
		raycaster.setFromCamera(mouse, camera);

		const closeButtons: THREE.Mesh[] = [];
		const minimizeButtons: THREE.Mesh[] = [];
		const phaseButtons: THREE.Mesh[] = [];
		const timeButtons: THREE.Mesh[] = [];
		const viewCycleButtons: THREE.Mesh[] = [];
		const editButtons: THREE.Mesh[] = [];
		const resizeHandles: THREE.Mesh[] = [];
		const headers: THREE.Mesh[] = [];
		for (const win of systemWindows.values()) {
			closeButtons.push(win.closeButton);
			minimizeButtons.push(win.minimizeButton);
			phaseButtons.push(win.phaseButton);
			timeButtons.push(win.timeButton);
			if (win.viewCycleButton.visible) viewCycleButtons.push(win.viewCycleButton);
			editButtons.push(win.editButton);
			resizeHandles.push(win.resizeHandle);
			headers.push(win.header);
		}

		// Close button (confirm-to-delete)
		const closeIntersects = raycaster.intersectObjects(closeButtons);
		if (closeIntersects.length > 0) {
			const button = closeIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			if (closeConfirmId === systemId) {
				// Second click — delete
				algebraic.removeSystem(systemId);
				resetCloseConfirm();
			} else {
				// First click — show confirm tooltip
				resetCloseConfirm();
				closeConfirmId = systemId;
				const win = systemWindows.get(systemId);
				if (win) {
					closeConfirmWindow = win;
					closeConfirmTooltip = createTooltipSprite('click again to remove', '#b85c4a');
					closeConfirmTooltip.position.set(-win.windowWidth / 2 + 16, win.windowHeight / 2 + 3, 3);
					win.group.add(closeConfirmTooltip);
					closeConfirmTimer = setTimeout(resetCloseConfirm, 2000);
				}
			}
			return;
		}

		// Minimize button
		const minimizeIntersects = raycaster.intersectObjects(minimizeButtons);
		if (minimizeIntersects.length > 0) {
			const button = minimizeIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			const win = systemWindows.get(systemId);
			if (win) {
				const sys = algebraic.systemList.find(s => s.id === systemId);
				if (win.minimized) {
					restoreWindow(win);
				} else if (sys) {
					minimizeWindow(win, sys);
				}
			}
			return;
		}

		// Composite buttons (close, minimize, look-inside, drag header)
		const compositeCloseButtons: THREE.Mesh[] = [];
		const compositeMinButtons: THREE.Mesh[] = [];
		const compositeLookButtons: THREE.Mesh[] = [];
		const compositeHeaders: THREE.Mesh[] = [];
		for (const cw of compositeWindows.values()) {
			compositeCloseButtons.push(cw.closeButton);
			compositeMinButtons.push(cw.minimizeButton);
			compositeLookButtons.push(cw.lookInsideButton);
			compositeHeaders.push(cw.header);
		}

		const compCloseIntersects = raycaster.intersectObjects(compositeCloseButtons);
		if (compCloseIntersects.length > 0) {
			const button = compCloseIntersects[0].object as THREE.Mesh;
			const compositeId = button.userData.compositeId;
			const group = algebraic.compositeGroups.find(g => g.id === compositeId);
			if (group) {
				const cw = compositeWindows.get(compositeId);
				if (closeConfirmId === compositeId) {
					// Second click — delete all members
					for (const memberId of group.memberSystemIds) {
						algebraic.removeSystem(memberId);
					}
					resetCloseConfirm();
				} else {
					// First click — show confirm tooltip
					resetCloseConfirm();
					closeConfirmId = compositeId;
					if (cw) {
						closeConfirmGroup = cw.group;
						const n = group.memberSystemIds.length;
						closeConfirmTooltip = createTooltipSprite(`click again to remove ${n} systems`, '#b85c4a');
						closeConfirmTooltip.position.set(-cw.windowWidth / 2 + 20, cw.windowHeight / 2 + 3, 3);
						cw.group.add(closeConfirmTooltip);
						closeConfirmTimer = setTimeout(resetCloseConfirm, 2000);
					}
				}
			}
			return;
		}

		const compLookIntersects = raycaster.intersectObjects(compositeLookButtons);
		if (compLookIntersects.length > 0) {
			const button = compLookIntersects[0].object as THREE.Mesh;
			const compositeId = button.userData.compositeId;
			const cw = compositeWindows.get(compositeId);
			if (cw) toggleLookInside(cw);
			return;
		}

		const compHeaderIntersects = raycaster.intersectObjects(compositeHeaders);
		if (compHeaderIntersects.length > 0) {
			const header = compHeaderIntersects[0].object as THREE.Mesh;
			const compositeId = header.userData.compositeId;
			const cw = compositeWindows.get(compositeId);
			if (cw) {
				// Drag all members together
				isDragging = true;
				// Use first member window as drag proxy — we'll handle multi-drag in onMouseMove
				draggedWindow = null;
				dragStart.set(event.clientX, event.clientY);
				// Store composite drag state
				(cw as any)._dragging = true;
				(cw as any)._dragStartPositions = new Map(
					cw.memberWindowIds.map(id => {
						const win = systemWindows.get(id);
						return [id, { x: win?.group.position.x ?? 0, y: win?.group.position.y ?? 0 }];
					})
				);
				windowStartPos.set(cw.group.position.x, cw.group.position.y);
				canvasEl.style.cursor = 'grabbing';
			}
			return;
		}

		// Caret button (params expand/collapse in minimized view)
		const caretButtons: THREE.Mesh[] = [];
		for (const win of systemWindows.values()) {
			if (win.caretButton) caretButtons.push(win.caretButton);
		}
		const caretIntersects = raycaster.intersectObjects(caretButtons);
		if (caretIntersects.length > 0) {
			const button = caretIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			const win = systemWindows.get(systemId);
			const sys = algebraic.systemList.find(s => s.id === systemId);
			if (win && sys) {
				toggleParamsExpanded(win, sys);
			}
			return;
		}

		// Edit button
		const editIntersects = raycaster.intersectObjects(editButtons);
		if (editIntersects.length > 0) {
			const button = editIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			editingSystemId = editingSystemId === systemId ? null : systemId;
			return;
		}

		// A6: Check view cycle buttons
		const viewCycleIntersects = raycaster.intersectObjects(viewCycleButtons);
		if (viewCycleIntersects.length > 0) {
			const button = viewCycleIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			const win = systemWindows.get(systemId);
			if (win) cycleViewAngle(win);
			return;
		}

		const phaseIntersects = raycaster.intersectObjects(phaseButtons);
		if (phaseIntersects.length > 0) {
			const button = phaseIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			const win = systemWindows.get(systemId);
			if (win) setViewMode(win, 'phase');
			return;
		}

		const timeIntersects = raycaster.intersectObjects(timeButtons);
		if (timeIntersects.length > 0) {
			const button = timeIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			const win = systemWindows.get(systemId);
			if (win) setViewMode(win, 'timeseries');
			return;
		}

		// B6: Check port hits for wire dragging
		const outputPortHits: THREE.Mesh[] = [];
		const inputPortHits: THREE.Mesh[] = [];
		for (const win of systemWindows.values()) {
			for (const p of win.outputPorts) outputPortHits.push(p.hitArea);
			for (const p of win.inputPorts) inputPortHits.push(p.hitArea);
		}

		const outPortIntersects = raycaster.intersectObjects(outputPortHits);
		if (outPortIntersects.length > 0) {
			const hit = outPortIntersects[0].object as THREE.Mesh;
			const { systemId, portIndex } = hit.userData;
			const win = systemWindows.get(systemId);
			if (win) {
				const port = win.outputPorts.find(p => p.portIndex === portIndex);
				if (port) {
					isWiring = true;
					wireSourcePort = port;
					pendingRewire = null;
					lastWireDragTime = 0;
					// Create drag line with pre-allocated vertices for pathfinding
					const dragGeom = new THREE.BufferGeometry();
					dragGeom.setAttribute('position', new THREE.BufferAttribute(new Float32Array(WIRE_DRAG_MAX_VERTS * 3), 3));
					dragGeom.setDrawRange(0, 2);
					const dragMat = new THREE.LineBasicMaterial({ color: 0xc9a84c, transparent: true, opacity: 0.3, depthTest: false });
					wireDragLine = new THREE.Line(dragGeom, dragMat);
					scene.add(wireDragLine);
					canvasEl.style.cursor = 'crosshair';
				}
			}
			return;
		}

		// C1: Check connected input ports for re-wiring
		const inPortIntersects = raycaster.intersectObjects(inputPortHits);
		if (inPortIntersects.length > 0) {
			const hit = inPortIntersects[0].object as THREE.Mesh;
			const { systemId, portIndex } = hit.userData;
			// Check if this input is connected
			const connectedWire = algebraic.wireList.find(w => w.toSystem === systemId && w.toPort === portIndex);
			if (connectedWire) {
				// Start re-wire from the original output port
				const fromWin = systemWindows.get(connectedWire.fromSystem);
				if (fromWin) {
					const fromPort = fromWin.outputPorts.find(p => p.portIndex === connectedWire.fromPort);
					if (fromPort) {
						// Start new wire drag
						isWiring = true;
						wireSourcePort = fromPort;
						pendingRewire = connectedWire;
						lastWireDragTime = 0;
						const dragGeom = new THREE.BufferGeometry();
						dragGeom.setAttribute('position', new THREE.BufferAttribute(new Float32Array(WIRE_DRAG_MAX_VERTS * 3), 3));
						dragGeom.setDrawRange(0, 2);
						const dragMat = new THREE.LineBasicMaterial({ color: 0xc9a84c, transparent: true, opacity: 0.3, depthTest: false });
						wireDragLine = new THREE.Line(dragGeom, dragMat);
						scene.add(wireDragLine);
						canvasEl.style.cursor = 'crosshair';
					}
				}
			}
			return;
		}

		const resizeIntersects = raycaster.intersectObjects(resizeHandles);
		if (resizeIntersects.length > 0) {
			const handle = resizeIntersects[0].object as THREE.Mesh;
			const systemId = handle.userData.systemId;
			const win = systemWindows.get(systemId);
			if (win) {
				isResizing = true;
				draggedWindow = win;
				dragStart.set(event.clientX, event.clientY);
				windowStartSize.set(win.windowWidth, win.windowHeight);
				windowStartPos.set(win.group.position.x, win.group.position.y);
				canvasEl.style.cursor = 'nwse-resize';
			}
			return;
		}

		const intersects = raycaster.intersectObjects(headers);
		if (intersects.length > 0) {
			const header = intersects[0].object as THREE.Mesh;
			const systemId = header.userData.systemId;
			const win = systemWindows.get(systemId);

			if (win) {
				isDragging = true;
				draggedWindow = win;
				dragStart.set(event.clientX, event.clientY);
				windowStartPos.set(win.group.position.x, win.group.position.y);
				win.group.position.z = 10;
				canvasEl.style.cursor = 'grabbing';
			}
			return;
		}

		// A5: Check content area for rotation (3D) or panning (2D/1D)
		const worldPos = screenToWorld(event.clientX, event.clientY);
		for (const win of systemWindows.values()) {
			const gx = win.group.position.x;
			const gy = win.group.position.y;
			const halfW = win.windowWidth / 2;
			const contentTop = gy + (win.windowHeight - HEADER_HEIGHT) / 2;
			const contentBottom = gy - (win.windowHeight + HEADER_HEIGHT) / 2;
			if (worldPos.x >= gx - halfW && worldPos.x <= gx + halfW &&
				worldPos.y >= contentBottom && worldPos.y <= contentTop) {
				if (win.viewMode === 'phase') {
					if (win.nstates >= 3) {
						isRotating = true;
						draggedWindow = win;
						rotateStart.set(event.clientX, event.clientY);
						canvasEl.style.cursor = 'move';
					} else {
						// 2D/1D: pan
						isPanning = true;
						draggedWindow = win;
						rotateStart.set(event.clientX, event.clientY);
						canvasEl.style.cursor = 'grab';
					}
				}
				break;
			}
		}
	}

	function onMouseMove(event: MouseEvent) {
		updateMousePosition(event);

		// B6: Wire drag line update with pathfinding
		if (isWiring && wireSourcePort && wireDragLine) {
			const worldPos = screenToWorld(event.clientX, event.clientY);
			const fromPos = getPortWorldPos(wireSourcePort);
			const positions = wireDragLine.geometry.attributes.position.array as Float32Array;

			const now = performance.now();
			if (now - lastWireDragTime > 50) {
				lastWireDragTime = now;
				// Build obstacles from all windows
				const obstacles: Array<{ x: number; y: number; w: number; h: number }> = [];
				for (const win of systemWindows.values()) {
					obstacles.push({
						x: win.group.position.x,
						y: win.group.position.y,
						w: win.windowWidth,
						h: win.windowHeight + HEADER_HEIGHT
					});
				}
				const rawRoute = computeWireRoute(fromPos, worldPos, obstacles);
				const route = smoothWirePath(rawRoute);
				const nVerts = Math.min(route.length, WIRE_DRAG_MAX_VERTS);
				for (let i = 0; i < nVerts; i++) {
					positions[i * 3] = route[i].x;
					positions[i * 3 + 1] = route[i].y;
					positions[i * 3 + 2] = route[i].z;
				}
				wireDragLine.geometry.setDrawRange(0, nVerts);
			} else {
				// Between throttled updates, just update the last vertex position
				const drawCount = wireDragLine.geometry.drawRange.count;
				if (drawCount >= 2) {
					const lastIdx = drawCount - 1;
					positions[lastIdx * 3] = worldPos.x;
					positions[lastIdx * 3 + 1] = worldPos.y;
					positions[lastIdx * 3 + 2] = 3;
				}
			}
			wireDragLine.geometry.attributes.position.needsUpdate = true;

			// Highlight nearby input ports
			raycaster.setFromCamera(mouse, camera);
			const inputHitAreas: THREE.Mesh[] = [];
			for (const win of systemWindows.values()) {
				for (const p of win.inputPorts) {
					if (p.systemId !== wireSourcePort.systemId) {
						inputHitAreas.push(p.hitArea);
					}
				}
			}
			// Reset all non-source input port highlights
			for (const win of systemWindows.values()) {
				for (const p of win.inputPorts) {
					const isConnected = algebraic.wireList.some(w => w.toSystem === p.systemId && w.toPort === p.portIndex);
					(p.mesh.material as THREE.MeshBasicMaterial).opacity = isConnected ? 0.8 : 0.3;
					(p.mesh.material as THREE.MeshBasicMaterial).color.set(0x3a2e24);
				}
			}
			const inIntersects = raycaster.intersectObjects(inputHitAreas);
			if (inIntersects.length > 0) {
				const hit = inIntersects[0].object as THREE.Mesh;
				const { systemId, portIndex } = hit.userData;
				const win = systemWindows.get(systemId);
				if (win) {
					const port = win.inputPorts.find(p => p.portIndex === portIndex);
					if (port) {
						(port.mesh.material as THREE.MeshBasicMaterial).opacity = 1.0;
						(port.mesh.material as THREE.MeshBasicMaterial).color.set(0xc9a84c);
						canvasEl.style.cursor = 'crosshair';
					}
				}
			}
			return;
		}

		if (isRotating && draggedWindow) {
			const deltaX = event.clientX - rotateStart.x;
			const deltaY = event.clientY - rotateStart.y;
			const sensitivity = 0.008;
			draggedWindow.contentGroup.rotation.y += deltaX * sensitivity;
			draggedWindow.contentGroup.rotation.x += deltaY * sensitivity;
			rotateStart.set(event.clientX, event.clientY);
			return;
		}

		// A5: 2D/1D panning
		if (isPanning && draggedWindow) {
			const deltaX = event.clientX - rotateStart.x;
			const deltaY = event.clientY - rotateStart.y;
			const viewWidth = camera.right - camera.left;
			const viewHeight = camera.top - camera.bottom;
			const rect = canvasEl.getBoundingClientRect();
			const sensitivity = 0.15;

			// Shift the bounds center by adjusting content position
			const worldDx = (deltaX / rect.width) * viewWidth * sensitivity;
			const worldDy = -(deltaY / rect.height) * viewHeight * sensitivity;

			if (draggedWindow.nstates >= 2) {
				draggedWindow.contentGroup.position.x += worldDx;
				draggedWindow.contentGroup.position.y += worldDy;
			} else {
				// 1D: pan in x only
				draggedWindow.contentGroup.position.x += worldDx;
			}
			rotateStart.set(event.clientX, event.clientY);
			return;
		}

		if (isResizing && draggedWindow) {
			const currentScreen = new THREE.Vector2(event.clientX, event.clientY);
			const deltaScreen = currentScreen.clone().sub(dragStart);

			const viewWidth = camera.right - camera.left;
			const viewHeight = camera.top - camera.bottom;
			const rect = canvasEl.getBoundingClientRect();

			const deltaWorldX = (deltaScreen.x / rect.width) * viewWidth;
			const deltaWorldY = (deltaScreen.y / rect.height) * viewHeight;

			const newWidth = windowStartSize.x + deltaWorldX;
			const newHeight = windowStartSize.y + deltaWorldY;
			const finalWidth = Math.max(MIN_WINDOW_SIZE, newWidth);
			const finalHeight = Math.max(MIN_WINDOW_SIZE, newHeight);
			// Anchor top-left corner: shift group position so top-left stays fixed
			draggedWindow.group.position.x = windowStartPos.x + (finalWidth - windowStartSize.x) / 2;
			draggedWindow.group.position.y = windowStartPos.y - (finalHeight - windowStartSize.y) / 2;
			resizeWindow(draggedWindow, newWidth, newHeight);
		} else if (isDragging && draggedWindow) {
			const currentScreen = new THREE.Vector2(event.clientX, event.clientY);
			const deltaScreen = currentScreen.clone().sub(dragStart);

			const viewWidth = camera.right - camera.left;
			const viewHeight = camera.top - camera.bottom;
			const rect = canvasEl.getBoundingClientRect();

			const deltaWorldX = (deltaScreen.x / rect.width) * viewWidth;
			const deltaWorldY = -(deltaScreen.y / rect.height) * viewHeight;

			draggedWindow.group.position.x = windowStartPos.x + deltaWorldX;
			draggedWindow.group.position.y = windowStartPos.y + deltaWorldY;
			updateClipPlanes(draggedWindow);
		} else if (isDragging && !draggedWindow) {
			// Composite drag: move all members
			const currentScreen = new THREE.Vector2(event.clientX, event.clientY);
			const deltaScreen = currentScreen.clone().sub(dragStart);
			const viewWidth = camera.right - camera.left;
			const viewHeight = camera.top - camera.bottom;
			const rect = canvasEl.getBoundingClientRect();
			const deltaWorldX = (deltaScreen.x / rect.width) * viewWidth;
			const deltaWorldY = -(deltaScreen.y / rect.height) * viewHeight;

			for (const cw of compositeWindows.values()) {
				if ((cw as any)._dragging) {
					const startPositions = (cw as any)._dragStartPositions as Map<string, { x: number; y: number }>;
					for (const [memberId, startPos] of startPositions) {
						const win = systemWindows.get(memberId);
						if (win) {
							win.group.position.x = startPos.x + deltaWorldX;
							win.group.position.y = startPos.y + deltaWorldY;
							updateClipPlanes(win);
						}
					}
				}
			}
		} else {
			raycaster.setFromCamera(mouse, camera);

			const closeButtons: THREE.Mesh[] = [];
			const minimizeButtons: THREE.Mesh[] = [];
			const phaseButtons: THREE.Mesh[] = [];
			const timeButtons: THREE.Mesh[] = [];
			const viewCycleButtons: THREE.Mesh[] = [];
			const editButtons: THREE.Mesh[] = [];
			const resizeHandles: THREE.Mesh[] = [];
			const headers: THREE.Mesh[] = [];
			for (const win of systemWindows.values()) {
				closeButtons.push(win.closeButton);
				minimizeButtons.push(win.minimizeButton);
				phaseButtons.push(win.phaseButton);
				timeButtons.push(win.timeButton);
				if (win.viewCycleButton.visible) viewCycleButtons.push(win.viewCycleButton);
				editButtons.push(win.editButton);
				resizeHandles.push(win.resizeHandle);
				headers.push(win.header);
			}

			const closeIntersects = raycaster.intersectObjects(closeButtons);
			const minimizeIntersects = raycaster.intersectObjects(minimizeButtons);
			const phaseIntersects = raycaster.intersectObjects(phaseButtons);
			const timeIntersects = raycaster.intersectObjects(timeButtons);
			const viewCycleIntersects = raycaster.intersectObjects(viewCycleButtons);
			const editIntersects = raycaster.intersectObjects(editButtons);
			if (closeIntersects.length > 0 || minimizeIntersects.length > 0 || phaseIntersects.length > 0 || timeIntersects.length > 0 || viewCycleIntersects.length > 0 || editIntersects.length > 0) {
				canvasEl.style.cursor = 'pointer';
				return;
			}

			const resizeIntersects = raycaster.intersectObjects(resizeHandles);
			if (resizeIntersects.length > 0) {
				canvasEl.style.cursor = 'nwse-resize';
				return;
			}

			// Composite button hover
			const compButtons: THREE.Mesh[] = [];
			const compHeaders: THREE.Mesh[] = [];
			for (const cw of compositeWindows.values()) {
				compButtons.push(cw.closeButton, cw.minimizeButton, cw.lookInsideButton);
				compHeaders.push(cw.header);
			}
			const compBtnIntersects = raycaster.intersectObjects(compButtons);
			if (compBtnIntersects.length > 0) {
				canvasEl.style.cursor = 'pointer';
				return;
			}
			const compHeaderIntersects = raycaster.intersectObjects(compHeaders);
			if (compHeaderIntersects.length > 0) {
				canvasEl.style.cursor = 'grab';
				return;
			}

			const headerIntersects = raycaster.intersectObjects(headers);
			canvasEl.style.cursor = headerIntersects.length > 0 ? 'grab' : 'default';
		}
	}

	function onMouseUp(event: MouseEvent) {
		// B6: Complete wire connection
		if (isWiring && wireSourcePort) {
			updateMousePosition(event);
			raycaster.setFromCamera(mouse, camera);

			// Check if cursor is on a valid input port of a different system
			const inputHitAreas: THREE.Mesh[] = [];
			for (const win of systemWindows.values()) {
				for (const p of win.inputPorts) {
					if (p.systemId !== wireSourcePort.systemId) {
						inputHitAreas.push(p.hitArea);
					}
				}
			}
			const inIntersects = raycaster.intersectObjects(inputHitAreas);
			if (inIntersects.length > 0) {
				const hit = inIntersects[0].object as THREE.Mesh;
				const { systemId, portIndex } = hit.userData;
				const sameAsOriginal = pendingRewire &&
					pendingRewire.toSystem === systemId &&
					pendingRewire.toPort === portIndex;

				if (!sameAsOriginal) {
					// Preserve destination system state before mutating wiring.
					const rewire = pendingRewire;
					if (rewire) {
						const targetSys = algebraic.systemList.find(s => s.id === rewire.toSystem);
						if (targetSys) {
							algebraic.setState(rewire.toSystem, [...targetSys.state]);
						}
						algebraic.unwire(rewire.id);
					}

					// Replace existing wire to this input if any (except the one we're rewiring).
					const existingWire = algebraic.wireList.find(w => w.toSystem === systemId && w.toPort === portIndex);
					if (existingWire && (!rewire || existingWire.id !== rewire.id)) {
						algebraic.unwire(existingWire.id);
					}

					algebraic.wire(wireSourcePort.systemId, wireSourcePort.portIndex, systemId, portIndex);
				}
			} else if (pendingRewire) {
				// Releasing outside any valid input intentionally drops the wire.
				const rewire = pendingRewire;
				const targetSys = algebraic.systemList.find(s => s.id === rewire.toSystem);
				if (targetSys) {
					algebraic.setState(rewire.toSystem, [...targetSys.state]);
				}
				algebraic.unwire(rewire.id);
			}

			// Clean up drag line
			if (wireDragLine) {
				scene.remove(wireDragLine);
				wireDragLine.geometry.dispose();
				(wireDragLine.material as THREE.Material).dispose();
				wireDragLine = null;
			}
			isWiring = false;
			wireSourcePort = null;
			pendingRewire = null;

			// Reset input port highlights
			for (const win of systemWindows.values()) {
				for (const p of win.inputPorts) {
					const isConnected = algebraic.wireList.some(w => w.toSystem === p.systemId && w.toPort === p.portIndex);
					(p.mesh.material as THREE.MeshBasicMaterial).opacity = isConnected ? 0.8 : 0.3;
					(p.mesh.material as THREE.MeshBasicMaterial).color.set(0x3a2e24);
				}
			}
			canvasEl.style.cursor = 'default';
			return;
		}

		if (draggedWindow && isDragging) {
			draggedWindow.group.position.z = 0;
		}
		// Clean up composite drag state
		for (const cw of compositeWindows.values()) {
			delete (cw as any)._dragging;
			delete (cw as any)._dragStartPositions;
		}
		isDragging = false;
		isResizing = false;
		isRotating = false;
		isPanning = false;
		draggedWindow = null;
		canvasEl.style.cursor = 'default';
	}

	function onWheel(event: WheelEvent) {
		event.preventDefault();
		const zoomFactor = event.deltaY > 0 ? 1.1 : 0.9;
		cameraZoom *= zoomFactor;
		cameraZoom = Math.max(0.3, Math.min(3, cameraZoom));
		updateCameraZoom();
	}

	function updateCameraZoom() {
		if (!camera) return;
		const aspect = width / height;
		const viewSize = 100 * cameraZoom;
		camera.left = -viewSize * aspect;
		camera.right = viewSize * aspect;
		camera.top = viewSize;
		camera.bottom = -viewSize;
		camera.updateProjectionMatrix();
	}

	function updateMousePosition(event: MouseEvent) {
		const rect = canvasEl.getBoundingClientRect();
		mouse.x = ((event.clientX - rect.left) / rect.width) * 2 - 1;
		mouse.y = -((event.clientY - rect.top) / rect.height) * 2 + 1;
	}

	function animate() {
		animationId = requestAnimationFrame(animate);
		renderer.render(scene, camera);
	}

	// React to systems changes
	$effect(() => {
		const systems = algebraic.systemList;

		if (!renderer || !scene) return;

		const currentIds = new Set(systems.map(s => s.id));

		for (const sys of systems) {
			updateSystemWindow(sys.id, sys);
		}

		for (const id of systemWindows.keys()) {
			if (!currentIds.has(id)) {
				removeSystemWindow(id);
			}
		}

		// B7: Sync wire visuals with store
		syncWireVisuals();

		// Sync composite windows
		const composites = algebraic.compositeGroups;
		const currentCompositeIds = new Set(composites.map(g => g.id));

		// Remove dissolved composites
		for (const [id] of compositeWindows) {
			if (!currentCompositeIds.has(id)) {
				removeCompositeWindow(id);
			}
		}

		// Create/update composite windows
		for (const group of composites) {
			let cw = compositeWindows.get(group.id);
			if (!cw) {
				cw = createCompositeWindow(group);
				compositeWindows.set(group.id, cw);
			}

			// Detect membership or name change — need to rebuild time series
			const oldMembers = cw.memberWindowIds.join(',');
			const newMembers = group.memberSystemIds.join(',');
			const membershipChanged = oldMembers !== newMembers;

			cw.memberWindowIds = [...group.memberSystemIds];

			// Update header label if name changed
			if (cw.cachedName !== group.name) {
				cw.cachedName = group.name;
				const oldLabel = cw.headerLabel;
				const newLabel = createTextSprite(group.name, cw.color);
				newLabel.position.copy(oldLabel.position);
				cw.group.remove(oldLabel);
				cw.group.add(newLabel);
				(oldLabel.material as THREE.SpriteMaterial).map?.dispose();
				(oldLabel.material as THREE.Material).dispose();
				cw.headerLabel = newLabel;
			}

			// Recompute bounds from member positions
			updateCompositeWindowBounds(cw);

			// Rebuild time series if membership or wiring changed
			const newFreeStates = algebraic.getCompositeFreeStateInfo(group.id);
			const freeStatesKey = newFreeStates.map(fs => `${fs.systemId}:${fs.stateIndex}`).join(',');
			const oldFreeStatesKey = cw.freeStates.map(fs => `${fs.systemId}:${fs.stateIndex}`).join(',');
			if (membershipChanged || freeStatesKey !== oldFreeStatesKey) {
				rebuildCompositeTimeSeries(cw);
			}

			// Update time series if not look-inside
			if (!cw.lookInside) {
				updateCompositeTimeSeries(cw);
			}

			// Rebuild ghost wires (they depend on wiring and positions)
			rebuildGhostWires(cw);

			// Apply member opacity & skeleton/ports based on look-inside state
			for (const memberId of cw.memberWindowIds) {
				const win = systemWindows.get(memberId);
				if (win) {
					if (cw.lookInside) {
						restoreGroupOpacity(win.group);
					} else {
						setGroupOpacity(win.group, 0);
					}
				}
			}
			// Internal wire opacity
			for (const wireId of group.internalWireIds) {
				const wv = wireVisuals.get(wireId);
				if (wv) {
					(wv.line.material as THREE.LineBasicMaterial).opacity = cw.lookInside ? 0.7 : 0;
					(wv.valueLine.material as THREE.LineBasicMaterial).opacity = cw.lookInside ? 0.4 : 0;
				}
			}
			// Phase 5: Skeleton overlay when not looking inside
			if (!cw.lookInside) {
				buildSkeletonOverlay(cw);
			} else {
				clearSkeletonOverlay(cw);
			}
			// Phase 6: Composite ports
			buildCompositePorts(cw);
			// Hide composite ports when in look-inside mode
			if (cw.lookInside) {
				for (const cp of cw.compositePorts) {
					cp.mesh.visible = false;
					cp.ring.visible = false;
					cp.hitArea.visible = false;
					cp.label.visible = false;
				}
			}
		}

		// Restore opacity for systems not in any composite
		for (const [sysId, win] of systemWindows) {
			const inComposite = composites.some(g => g.memberSystemIds.includes(sysId));
			if (!inComposite) {
				restoreGroupOpacity(win.group);
			}
		}
	});

	// React to size changes
	$effect(() => {
		const w = width;
		const h = height;

		if (!renderer || !camera) return;

		renderer.setSize(w, h);
		updateCameraZoom();
	});

	onMount(() => {
		// Gate on font loading so canvas textures render with CMU Serif.
		// Start init immediately but re-render text once fonts load.
		initScene();
		animate();
		Promise.all([
			document.fonts.load(`400 16px ${CANVAS_FONT}`),
			document.fonts.load(`700 16px ${CANVAS_FONT}`),
			document.fonts.load(`italic 16px ${CANVAS_FONT}`)
		]).catch(() => { /* font fallback is acceptable */ });

		return () => {
			cancelAnimationFrame(animationId);
			canvasEl.removeEventListener('mousedown', onMouseDown);
			canvasEl.removeEventListener('mousemove', onMouseMove);
			canvasEl.removeEventListener('mouseup', onMouseUp);
			canvasEl.removeEventListener('mouseleave', onMouseUp);
			canvasEl.removeEventListener('wheel', onWheel);
			// Dispose wire visuals
			for (const [, wv] of wireVisuals) {
				scene.remove(wv.line);
				wv.geometry.dispose();
				(wv.line.material as THREE.Material).dispose();
				scene.remove(wv.valueLine);
				wv.valueGeometry.dispose();
				(wv.valueLine.material as THREE.Material).dispose();
			}
			wireVisuals.clear();
			if (wireDragLine) {
				scene.remove(wireDragLine);
				wireDragLine.geometry.dispose();
				(wireDragLine.material as THREE.Material).dispose();
				wireDragLine = null;
			}
			renderer.dispose();
			for (const id of compositeWindows.keys()) {
				removeCompositeWindow(id);
			}
			for (const id of systemWindows.keys()) {
				removeSystemWindow(id);
			}
		};
	});
</script>

<div class="renderer-container">
	<canvas bind:this={canvasEl}></canvas>

	{#if editingSystemId}
		{@const sys = algebraic.systemList.find(s => s.id === editingSystemId)}
		{@const tmpl = sys ? algebraic.templateList.find(t => t.id === sys.templateId) : null}
		{@const pos = getEditorScreenPos(editingSystemId)}
		{#if sys && tmpl && pos}
			<div class="edit-overlay" style="left: {pos.x}px; top: {pos.y}px;">
				<div class="edit-header">
					<span>{tmpl.name}</span>
					<button class="edit-close" onclick={() => editingSystemId = null}>x</button>
				</div>
				{#each tmpl.parameters as param, i}
					<div class="edit-row">
						<label for="param-{i}">{param.name}</label>
						<input
							id="param-{i}"
							type="number"
							step="any"
							value={sys.parameters[param.name] ?? param.default}
							onchange={(e) => {
								const val = parseFloat((e.target as HTMLInputElement).value);
								if (!isNaN(val)) {
									algebraic.setParams(editingSystemId!, { ...sys.parameters, [param.name]: val });
								}
							}}
						/>
					</div>
				{/each}
			</div>
		{/if}
	{/if}
</div>

<style>
	.renderer-container {
		position: relative;
		width: 100%;
		height: 100%;
	}

	canvas {
		display: block;
		width: 100%;
		height: 100%;
	}

	.edit-overlay {
		position: absolute;
		background: #1a1410;
		border: 1px solid #c9a84c;
		border-radius: 4px;
		padding: 0.5rem;
		min-width: 160px;
		z-index: 10;
		font-family: 'CMU Serif', serif;
		font-size: 0.75rem;
		color: #d4c5a0;
	}

	.edit-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 0.4rem;
		color: #c9a84c;
		font-weight: bold;
	}

	.edit-close {
		background: none;
		border: none;
		color: #9a8b78;
		cursor: pointer;
		font-size: 0.8rem;
		padding: 0 0.2rem;
	}

	.edit-close:hover {
		color: #c9a84c;
	}

	.edit-row {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 0.5rem;
		margin-bottom: 0.25rem;
	}

	.edit-row label {
		color: #9a8b78;
	}

	.edit-row input {
		width: 70px;
		background: #0f0b08;
		border: 1px solid #3a2e24;
		color: #d4c5a0;
		padding: 0.2rem 0.3rem;
		font-family: monospace;
		font-size: 0.7rem;
		text-align: right;
	}

	.edit-row input:focus {
		outline: none;
		border-color: #c9a84c;
	}
</style>
