<!--
  AlgebraicRenderer.svelte

  Three.js renderer for multiple open dynamical systems.
  Each system is rendered in its own draggable "window" container.
  Windows are arranged in 2D (like OS windows) with 3D content inside each.
-->
<script lang="ts">
	import { onMount } from 'svelte';
	import * as THREE from 'three';
	import { algebraic, type SystemState } from '$lib/stores/algebraic.svelte';

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
		// Resize handle
		resizeHandle: THREE.Mesh;
		// Content
		contentGroup: THREE.Group;
		border: THREE.LineSegments;
		// Phase space view
		trailGeometry: THREE.BufferGeometry;
		trail: THREE.Line;
		point: THREE.Mesh;
		axesGroup: THREE.Group; // 3D axes for phase view
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
	}

	// Colors for individual state variables in time series view
	const STATE_COLORS = [
		new THREE.Color(0xff6b6b), // red - x
		new THREE.Color(0x4ecdc4), // teal - y
		new THREE.Color(0xffe66d), // yellow - z
		new THREE.Color(0x95e1d3), // mint
		new THREE.Color(0xf38181), // coral
		new THREE.Color(0xaa96da), // lavender
	];

	const STATE_NAMES = ['x', 'y', 'z', 'w', 'v', 'u'];
	const TIME_WINDOW_SAMPLES = 500; // Fixed number of samples shown in time view
	const MIN_WINDOW_SIZE = 25;
	const DEFAULT_WINDOW_WIDTH = 40;
	const DEFAULT_WINDOW_HEIGHT = 40;
	const TS_MARGIN = { left: 10, right: 12, bottom: 8, top: 4 };
	const NUM_TICKS = 5;

	const systemWindows = new Map<string, SystemWindow>();
	const MAX_POINTS = 2000;

	// Drag state
	let isDragging = false;
	let isResizing = false;
	let draggedWindow: SystemWindow | null = null;
	let dragStart = new THREE.Vector2();
	let windowStartPos = new THREE.Vector2();
	let windowStartSize = new THREE.Vector2();
	let isRotating = false;
	let rotateStart = new THREE.Vector2();

	// Raycaster for mouse interaction
	const raycaster = new THREE.Raycaster();
	const mouse = new THREE.Vector2();

	// Color palette for systems
	const COLORS = [
		new THREE.Color(0x7ac5cd), // cyan
		new THREE.Color(0xcd7a7a), // coral
		new THREE.Color(0x7acd8f), // green
		new THREE.Color(0xcd9b7a), // orange
		new THREE.Color(0x9b7acd), // purple
		new THREE.Color(0xcdcd7a), // yellow
		new THREE.Color(0x7a9bcd), // blue
		new THREE.Color(0xcd7acd) // pink
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
		renderer.setClearColor(0x0a0a12, 1);
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
		// Looking straight down the Z axis at the XY plane
		camera.position.set(0, 0, 200);
		camera.lookAt(0, 0, 0);

		// Ambient light
		const ambientLight = new THREE.AmbientLight(0x606070);
		scene.add(ambientLight);

		// Directional light from the camera's perspective
		const dirLight = new THREE.DirectionalLight(0xffffff, 0.6);
		dirLight.position.set(0, 0, 100);
		scene.add(dirLight);

		// Set up mouse event listeners
		canvasEl.addEventListener('mousedown', onMouseDown);
		canvasEl.addEventListener('mousemove', onMouseMove);
		canvasEl.addEventListener('mouseup', onMouseUp);
		canvasEl.addEventListener('mouseleave', onMouseUp);
		canvasEl.addEventListener('wheel', onWheel);
	}

	function createTextSprite(text: string, color: THREE.Color): THREE.Sprite {
		const canvas = document.createElement('canvas');
		const context = canvas.getContext('2d')!;
		canvas.width = 256;
		canvas.height = 64;

		context.fillStyle = 'transparent';
		context.fillRect(0, 0, canvas.width, canvas.height);

		context.font = 'bold 28px monospace';
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

		// Background
		context.fillStyle = active ? '#4a7a8a' : '#2a3a4a';
		context.roundRect(0, 0, canvas.width, canvas.height, 6);
		context.fill();

		if (active) {
			context.strokeStyle = '#6ab0c0';
			context.lineWidth = 2;
			context.stroke();
		}

		// Text
		context.font = 'bold 18px monospace';
		context.fillStyle = active ? '#ffffff' : '#808090';
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

		// Color swatch
		context.fillStyle = `rgb(${Math.floor(color.r * 255)}, ${Math.floor(color.g * 255)}, ${Math.floor(color.b * 255)})`;
		context.fillRect(4, 10, 12, 12);

		// Text
		context.font = 'bold 16px monospace';
		context.fillStyle = '#c0c0c0';
		context.textAlign = 'left';
		context.textBaseline = 'middle';
		context.fillText(name, 20, 16);

		const texture = new THREE.CanvasTexture(canvas);
		const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
		const sprite = new THREE.Sprite(material);
		sprite.scale.set(6, 3, 1);
		return sprite;
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

		ctx.font = 'bold 22px monospace';
		ctx.fillStyle = '#909098';
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
		const yAxisHeight = winH - TS_MARGIN.bottom - TS_MARGIN.top;
		sprite.scale.set(10, yAxisHeight, 1);
		sprite.position.set(-winW / 2 + TS_MARGIN.left / 2,
			(TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);
	}

	function updateXTickSprite(sprite: THREE.Sprite, startSample: number, endSample: number, winW: number, winH: number) {
		const material = sprite.material as THREE.SpriteMaterial;
		const texture = material.map!;
		const canvas = texture.image as HTMLCanvasElement;
		const ctx = canvas.getContext('2d')!;
		ctx.clearRect(0, 0, canvas.width, canvas.height);

		ctx.font = 'bold 18px monospace';
		ctx.fillStyle = '#909098';
		ctx.textAlign = 'center';

		const pad = 12;
		for (let i = 0; i <= NUM_TICKS; i++) {
			const t = i / NUM_TICKS;
			const x = pad + t * (canvas.width - 2 * pad);
			const sample = startSample + Math.floor(t * (endSample - startSample));
			const timeSec = (sample / 60).toFixed(1);

			ctx.fillRect(x - 0.5, 0, 1, 4);
			ctx.textBaseline = 'top';
			ctx.fillText(timeSec + 's', x, 6);
		}

		texture.needsUpdate = true;
		const xAxisWidth = winW - TS_MARGIN.left - TS_MARGIN.right;
		sprite.scale.set(xAxisWidth, 6, 1);
		sprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2,
			-winH / 2 + TS_MARGIN.bottom / 2 - 1, 2);
	}

	function createClipPlanes(): THREE.Plane[] {
		return [
			new THREE.Plane(new THREE.Vector3(1, 0, 0), 0),  // left: x >= bound
			new THREE.Plane(new THREE.Vector3(-1, 0, 0), 0), // right: x <= bound
			new THREE.Plane(new THREE.Vector3(0, 1, 0), 0),  // bottom: y >= bound
			new THREE.Plane(new THREE.Vector3(0, -1, 0), 0), // top: y <= bound
		];
	}

	function updateClipPlanes(win: SystemWindow) {
		const gx = win.group.position.x;
		const gy = win.group.position.y;
		const halfW = win.windowWidth / 2;
		// Content area in world space (below header)
		const left = gx - halfW;
		const right = gx + halfW;
		const bottom = gy - win.windowHeight / 2 - HEADER_HEIGHT / 2;
		const top = gy + win.windowHeight / 2 - HEADER_HEIGHT / 2;

		// Plane(normal, constant): visible where normal·point + constant >= 0
		win.clipPlanes[0].constant = -left;
		win.clipPlanes[1].constant = right;
		win.clipPlanes[2].constant = -bottom;
		win.clipPlanes[3].constant = top;
	}

	function create3DAxes(clipPlanes: THREE.Plane[], stateNames: string[]): THREE.Group {
		const axesGroup = new THREE.Group();
		const axisLength = 500; // Very long — clipped by window planes

		const axisConfigs = [
			{ color: 0xff6666, dir: new THREE.Vector3(1, 0, 0), name: stateNames[0] || 'x' },
			{ color: 0x66ff66, dir: new THREE.Vector3(0, 1, 0), name: stateNames[1] || 'y' },
			{ color: 0x6666ff, dir: new THREE.Vector3(0, 0, 1), name: stateNames[2] || 'z' },
		];

		for (const { color, dir, name } of axisConfigs) {
			const geom = new THREE.BufferGeometry().setFromPoints([
				dir.clone().multiplyScalar(-axisLength),
				dir.clone().multiplyScalar(axisLength)
			]);
			axesGroup.add(new THREE.Line(geom, new THREE.LineBasicMaterial({ color, clippingPlanes: clipPlanes })));

			const label = createTextSprite(name, new THREE.Color(color));
			label.scale.set(3, 1.5, 1);
			label.position.copy(dir.clone().multiplyScalar(18));
			(label.material as THREE.SpriteMaterial).clippingPlanes = clipPlanes;
			axesGroup.add(label);
		}

		return axesGroup;
	}

	function createSystemWindow(id: string, sys: SystemState): SystemWindow {
		const color = COLORS[colorIndex % COLORS.length];
		colorIndex++;
		const nstates = sys.nstates;
		const winWidth = DEFAULT_WINDOW_WIDTH;
		const winHeight = DEFAULT_WINDOW_HEIGHT;
		// Look up template to get state variable names
		const tmpl = algebraic.templateList.find(t => t.id === sys.templateId);
		const stateNames = tmpl?.state_names ?? STATE_NAMES;

		const group = new THREE.Group();
		group.userData = { systemId: id };

		// Position based on sys.position or default grid layout
		const windowIndex = systemWindows.size;
		const gridCols = 3;
		const spacing = winWidth + 8;
		const startX = -spacing;
		const startY = 40;
		const defaultX = startX + (windowIndex % gridCols) * spacing;
		const defaultY = startY - Math.floor(windowIndex / gridCols) * (winHeight + HEADER_HEIGHT + 8);

		group.position.set(sys.position?.x ?? defaultX, sys.position?.y ?? defaultY, 0);

		// Window border (will be updated on resize)
		const borderGeometry = new THREE.EdgesGeometry(new THREE.PlaneGeometry(winWidth, winHeight + HEADER_HEIGHT));
		const borderMaterial = new THREE.LineBasicMaterial({ color, transparent: true, opacity: 0.5 });
		const border = new THREE.LineSegments(borderGeometry, borderMaterial);
		group.add(border);

		// Header bar (draggable)
		const headerGeometry = new THREE.PlaneGeometry(winWidth, HEADER_HEIGHT);
		const headerMaterial = new THREE.MeshBasicMaterial({ color, transparent: true, opacity: 0.3, side: THREE.DoubleSide });
		const header = new THREE.Mesh(headerGeometry, headerMaterial);
		header.position.set(0, winHeight / 2, 0);
		header.userData = { isHeader: true, systemId: id };
		group.add(header);

		// Label in header
		const systemName = sys.templateId.split('_')[0];
		const label = createTextSprite(systemName, color);
		label.position.set(-winWidth / 2 + 12, winHeight / 2, 1);
		group.add(label);

		// Mode buttons: Phase and Time
		const phaseButtonGeometry = new THREE.PlaneGeometry(8, 3);
		const phaseButtonMaterial = new THREE.MeshBasicMaterial({ color: 0x4a7a8a, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const phaseButton = new THREE.Mesh(phaseButtonGeometry, phaseButtonMaterial);
		phaseButton.position.set(winWidth / 2 - 16, winHeight / 2, 1);
		phaseButton.userData = { isPhaseButton: true, systemId: id };
		group.add(phaseButton);

		const phaseButtonLabel = createButtonSprite('Phase', true);
		phaseButtonLabel.position.set(winWidth / 2 - 16, winHeight / 2, 2);
		group.add(phaseButtonLabel);

		const timeButtonGeometry = new THREE.PlaneGeometry(8, 3);
		const timeButtonMaterial = new THREE.MeshBasicMaterial({ color: 0x2a3a4a, transparent: true, opacity: 0.01, side: THREE.DoubleSide });
		const timeButton = new THREE.Mesh(timeButtonGeometry, timeButtonMaterial);
		timeButton.position.set(winWidth / 2 - 6, winHeight / 2, 1);
		timeButton.userData = { isTimeButton: true, systemId: id };
		group.add(timeButton);

		const timeButtonLabel = createButtonSprite('Time', false);
		timeButtonLabel.position.set(winWidth / 2 - 6, winHeight / 2, 2);
		group.add(timeButtonLabel);

		// Resize handle (bottom-right corner)
		const resizeHandleGeom = new THREE.PlaneGeometry(4, 4);
		const resizeHandleMat = new THREE.MeshBasicMaterial({ color, transparent: true, opacity: 0.4, side: THREE.DoubleSide });
		const resizeHandle = new THREE.Mesh(resizeHandleGeom, resizeHandleMat);
		resizeHandle.position.set(winWidth / 2 - 2, -winHeight / 2 - HEADER_HEIGHT + 2, 1);
		resizeHandle.userData = { isResizeHandle: true, systemId: id };
		group.add(resizeHandle);

		// === Phase Space View ===
		const contentGroup = new THREE.Group();
		contentGroup.position.set(0, -HEADER_HEIGHT / 2, 0);
		contentGroup.rotation.x = -0.4;
		contentGroup.rotation.y = 0.3;
		group.add(contentGroup);

		// Clipping planes for this window (world-space, updated on move/resize)
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

		// 3D Axes for phase view — long lines through origin, clipped to window
		const axesGroup = create3DAxes(clipPlanes, stateNames);
		contentGroup.add(axesGroup);

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

			// Legend
			const legendSprite = createLegendSprite(stateNames[i] || `v${i}`, stateColor);
			legendSprite.position.set(winWidth / 2 - 5, winHeight / 2 - 4 - i * 3, 1);
			timeSeriesGroup.add(legendSprite);
			legendSprites.push(legendSprite);
		}

		// Axis lines for time series (using TS_MARGIN)
		const tsAxisGeometry = new THREE.BufferGeometry();
		const tsAxisPoints = [
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			winWidth / 2 - TS_MARGIN.right, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, -winHeight / 2 + TS_MARGIN.bottom, 0,
			-winWidth / 2 + TS_MARGIN.left, winHeight / 2 - TS_MARGIN.top, 0,
		];
		tsAxisGeometry.setAttribute('position', new THREE.Float32BufferAttribute(tsAxisPoints, 3));
		const tsAxisMaterial = new THREE.LineBasicMaterial({ color: 0x404050, transparent: true, opacity: 0.5 });
		const tsAxisLines = new THREE.LineSegments(tsAxisGeometry, tsAxisMaterial);
		timeSeriesGroup.add(tsAxisLines);

		// Tick sprites for time series
		const yTickSprite = createTickSprite(128, 512);
		yTickSprite.scale.set(10, winHeight - TS_MARGIN.bottom - TS_MARGIN.top, 1);
		yTickSprite.position.set(-winWidth / 2 + TS_MARGIN.left / 2, (TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);
		timeSeriesGroup.add(yTickSprite);

		const xTickSprite = createTickSprite(512, 64);
		xTickSprite.scale.set(winWidth - TS_MARGIN.left - TS_MARGIN.right, 6, 1);
		xTickSprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2, -winHeight / 2 + TS_MARGIN.bottom / 2 - 1, 2);
		timeSeriesGroup.add(xTickSprite);

		scene.add(group);

		return {
			group, header, headerLabel: label,
			phaseButton, timeButton, phaseButtonLabel, timeButtonLabel,
			resizeHandle, contentGroup, border,
			trailGeometry, trail, point, axesGroup,
			timeSeriesGroup, timeSeriesLines, timeSeriesGeometries, legendSprites,
			tsAxisLines, yTickSprite, xTickSprite, clipPlanes,
			color, viewMode: 'phase', nstates,
			windowWidth: winWidth, windowHeight: winHeight,
			bounds: { min: new THREE.Vector3(Infinity, Infinity, Infinity), max: new THREE.Vector3(-Infinity, -Infinity, -Infinity), initialized: false },
			tsYMin: Infinity, tsYMax: -Infinity
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

	function updateBounds(win: SystemWindow, state: number[]) {
		if (state.length >= 1) {
			win.bounds.min.x = Math.min(win.bounds.min.x, state[0]);
			win.bounds.max.x = Math.max(win.bounds.max.x, state[0]);
		}
		if (state.length >= 2) {
			win.bounds.min.y = Math.min(win.bounds.min.y, state[1]);
			win.bounds.max.y = Math.max(win.bounds.max.y, state[1]);
		}
		if (state.length >= 3) {
			win.bounds.min.z = Math.min(win.bounds.min.z, state[2]);
			win.bounds.max.z = Math.max(win.bounds.max.z, state[2]);
		}
		win.bounds.initialized = true;
	}

	function mapStateToLocal(state: number[], win: SystemWindow, nstates: number): THREE.Vector3 {
		// Simple fixed scaling - attractor-specific ranges are handled by scale factors
		// Lorenz: x,y ~ [-20,20], z ~ [0,50] -> scale ~0.5 fits in ~[-15,15] window space
		// Rossler: x,y ~ [-10,10], z ~ [0,25] -> similar
		const scale = 0.5;
		const result = new THREE.Vector3(0, 0, 0);

		if (nstates >= 3) {
			// 3D system - center z around typical attractor midpoint
			result.x = state[0] * scale;
			result.y = state[1] * scale;
			result.z = (state[2] - 25) * scale; // Offset z to center typical attractors
		} else if (nstates >= 2) {
			result.x = state[0] * scale * 2;
			result.y = state[1] * scale * 2;
			result.z = 0;
		} else if (nstates >= 1) {
			result.x = state[0] * scale * 3;
			result.y = 0;
			result.z = 0;
		}

		return result;
	}

	function setViewMode(win: SystemWindow, mode: ViewMode) {
		if (win.viewMode === mode) return;
		win.viewMode = mode;

		// Update visibility
		win.contentGroup.visible = mode === 'phase';
		win.timeSeriesGroup.visible = mode === 'timeseries';

		// Update button appearances
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

	function resizeWindow(win: SystemWindow, newWidth: number, newHeight: number) {
		newWidth = Math.max(MIN_WINDOW_SIZE, newWidth);
		newHeight = Math.max(MIN_WINDOW_SIZE, newHeight);
		win.windowWidth = newWidth;
		win.windowHeight = newHeight;

		// Update border
		win.group.remove(win.border);
		const newBorderGeom = new THREE.EdgesGeometry(new THREE.PlaneGeometry(newWidth, newHeight + HEADER_HEIGHT));
		win.border.geometry.dispose();
		win.border.geometry = newBorderGeom;
		win.group.add(win.border);

		// Update header
		win.header.geometry.dispose();
		win.header.geometry = new THREE.PlaneGeometry(newWidth, HEADER_HEIGHT);
		win.header.position.set(0, newHeight / 2, 0);

		// Update header label position
		win.headerLabel.position.set(-newWidth / 2 + 12, newHeight / 2, 1);

		// Update button positions
		win.phaseButton.position.set(newWidth / 2 - 16, newHeight / 2, 1);
		win.phaseButtonLabel.position.set(newWidth / 2 - 16, newHeight / 2, 2);
		win.timeButton.position.set(newWidth / 2 - 6, newHeight / 2, 1);
		win.timeButtonLabel.position.set(newWidth / 2 - 6, newHeight / 2, 2);

		// Update resize handle position
		win.resizeHandle.position.set(newWidth / 2 - 2, -newHeight / 2 - HEADER_HEIGHT + 2, 1);

		// Scale phase content to fill resized window
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

		// Update tick sprite sizes/positions
		const yAxisHeight = newHeight - TS_MARGIN.bottom - TS_MARGIN.top;
		win.yTickSprite.scale.set(8, yAxisHeight, 1);
		win.yTickSprite.position.set(-newWidth / 2 + TS_MARGIN.left / 2 - 1, (TS_MARGIN.bottom - TS_MARGIN.top) / 2, 2);
		const xAxisWidth = newWidth - TS_MARGIN.left - TS_MARGIN.right;
		win.xTickSprite.scale.set(xAxisWidth, 5, 1);
		win.xTickSprite.position.set((TS_MARGIN.left - TS_MARGIN.right) / 2, -newHeight / 2 + TS_MARGIN.bottom / 2 - 2, 2);

		// Update clipping planes for new size/position
		updateClipPlanes(win);

		// Update legend positions in time series view
		for (let i = 0; i < win.legendSprites.length; i++) {
			win.legendSprites[i].position.set(newWidth / 2 - 5, newHeight / 2 - 4 - i * 3, 1);
		}
	}

	function updateSystemWindow(id: string, sys: SystemState) {
		const win = getOrCreateWindow(id, sys);
		const history = algebraic.getHistory(id);

		// Update bounds incrementally
		updateBounds(win, sys.state);

		if (history.length === 0) return;

		const winW = win.windowWidth;
		const winH = win.windowHeight;

		if (win.viewMode === 'phase') {
			// === Phase Space View ===
			const len = Math.min(history.length, MAX_POINTS);
			const localPos = mapStateToLocal(sys.state, win, sys.nstates);
			win.point.position.copy(localPos);

			const positions = win.trailGeometry.attributes.position.array as Float32Array;
			const colors = win.trailGeometry.attributes.color.array as Float32Array;

			for (let i = 0; i < len; i++) {
				const state = history[i];
				const pos = mapStateToLocal(state, win, sys.nstates);

				positions[i * 3] = pos.x;
				positions[i * 3 + 1] = pos.y;
				positions[i * 3 + 2] = pos.z;

				const t = i / len;
				const intensity = 0.4 + t * 0.6;
				colors[i * 3] = win.color.r * intensity;
				colors[i * 3 + 1] = win.color.g * intensity;
				colors[i * 3 + 2] = win.color.b * intensity;
			}

			win.trailGeometry.attributes.position.needsUpdate = true;
			win.trailGeometry.attributes.color.needsUpdate = true;
			win.trailGeometry.setDrawRange(0, len);
		} else {
			// === Time Series View ===
			// Fixed-width moving window: show last TIME_WINDOW_SAMPLES samples
			const startIdx = Math.max(0, history.length - TIME_WINDOW_SAMPLES);
			const visibleHistory = history.slice(startIdx);
			const len = visibleHistory.length;

			if (len === 0) return;

			// Find min/max across ALL state variables for unified Y scale
			let frameMin = Infinity;
			let frameMax = -Infinity;
			for (const state of visibleHistory) {
				for (let v = 0; v < win.nstates && v < state.length; v++) {
					frameMin = Math.min(frameMin, state[v]);
					frameMax = Math.max(frameMax, state[v]);
				}
			}

			// Monotonically expand Y range (never shrink)
			if (frameMin < win.tsYMin) win.tsYMin = frameMin;
			if (frameMax > win.tsYMax) win.tsYMax = frameMax;
			const yDataMin = win.tsYMin;
			const yDataMax = win.tsYMax;

			// Window content area (with margins)
			const xMin = -winW / 2 + TS_MARGIN.left;
			const xMax = winW / 2 - TS_MARGIN.right;
			const yMin = -winH / 2 + TS_MARGIN.bottom;
			const yMax = winH / 2 - TS_MARGIN.top;
			const xRange = xMax - xMin;
			const yRange = yMax - yMin;

			const dataRange = yDataMax - yDataMin;

			// Update each state variable's line
			for (let v = 0; v < win.nstates; v++) {
				const geometry = win.timeSeriesGeometries[v];
				if (!geometry) continue;

				const positions = geometry.attributes.position.array as Float32Array;

				for (let i = 0; i < len && i < TIME_WINDOW_SAMPLES; i++) {
					const state = visibleHistory[i];
					const value = v < state.length ? state[v] : 0;

					const x = xMin + (i / (TIME_WINDOW_SAMPLES - 1)) * xRange;
					const y = yMin + ((value - yDataMin) / (dataRange > 0.001 ? dataRange : 1)) * yRange * 0.9 + yRange * 0.05;

					positions[i * 3] = x;
					positions[i * 3 + 1] = y;
					positions[i * 3 + 2] = 0;
				}

				geometry.attributes.position.needsUpdate = true;
				geometry.setDrawRange(0, len);
			}

			// Update tick sprites
			updateYTickSprite(win.yTickSprite, yDataMin, yDataMax, winW, winH);
			updateXTickSprite(win.xTickSprite, startIdx, startIdx + len, winW, winH);
		}
	}

	function removeSystemWindow(id: string) {
		const win = systemWindows.get(id);
		if (win) {
			scene.remove(win.group);
			// Phase space
			win.trailGeometry.dispose();
			(win.trail.material as THREE.Material).dispose();
			(win.point.material as THREE.Material).dispose();
			(win.point.geometry as THREE.BufferGeometry).dispose();
			// Time series
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
			// Header & buttons
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
			(win.resizeHandle.material as THREE.Material).dispose();
			win.resizeHandle.geometry.dispose();
			win.border.geometry.dispose();
			(win.border.material as THREE.Material).dispose();
			// Axes / ticks
			win.tsAxisLines.geometry.dispose();
			(win.tsAxisLines.material as THREE.Material).dispose();
			(win.yTickSprite.material as THREE.SpriteMaterial).map?.dispose();
			(win.yTickSprite.material as THREE.Material).dispose();
			(win.xTickSprite.material as THREE.SpriteMaterial).map?.dispose();
			(win.xTickSprite.material as THREE.Material).dispose();
			systemWindows.delete(id);
		}
	}

	// Convert screen coords to world coords for 2D dragging
	function screenToWorld(screenX: number, screenY: number): THREE.Vector2 {
		const rect = canvasEl.getBoundingClientRect();
		const ndcX = ((screenX - rect.left) / rect.width) * 2 - 1;
		const ndcY = -((screenY - rect.top) / rect.height) * 2 + 1;

		// For orthographic camera, convert NDC directly to world coords
		const worldX = ndcX * (camera.right - camera.left) / 2;
		const worldY = ndcY * (camera.top - camera.bottom) / 2;

		return new THREE.Vector2(worldX, worldY);
	}

	// Mouse interaction handlers
	function onMouseDown(event: MouseEvent) {
		updateMousePosition(event);
		raycaster.setFromCamera(mouse, camera);

		// Collect clickable objects
		const phaseButtons: THREE.Mesh[] = [];
		const timeButtons: THREE.Mesh[] = [];
		const resizeHandles: THREE.Mesh[] = [];
		const headers: THREE.Mesh[] = [];
		for (const win of systemWindows.values()) {
			phaseButtons.push(win.phaseButton);
			timeButtons.push(win.timeButton);
			resizeHandles.push(win.resizeHandle);
			headers.push(win.header);
		}

		// Check phase buttons first (highest priority)
		const phaseIntersects = raycaster.intersectObjects(phaseButtons);
		if (phaseIntersects.length > 0) {
			const button = phaseIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			const win = systemWindows.get(systemId);
			if (win) {
				setViewMode(win, 'phase');
			}
			return;
		}

		// Check time buttons
		const timeIntersects = raycaster.intersectObjects(timeButtons);
		if (timeIntersects.length > 0) {
			const button = timeIntersects[0].object as THREE.Mesh;
			const systemId = button.userData.systemId;
			const win = systemWindows.get(systemId);
			if (win) {
				setViewMode(win, 'timeseries');
			}
			return;
		}

		// Check resize handles
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
				canvasEl.style.cursor = 'nwse-resize';
			}
			return;
		}

		// Check headers for dragging
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

				// Bring window to front (higher z)
				win.group.position.z = 10;

				canvasEl.style.cursor = 'grabbing';
			}
			return;
		}

		// Check if click is in any window's content area (for rotation in phase mode)
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
					isRotating = true;
					draggedWindow = win;
					rotateStart.set(event.clientX, event.clientY);
					canvasEl.style.cursor = 'move';
				}
				break;
			}
		}
	}

	function onMouseMove(event: MouseEvent) {
		updateMousePosition(event);

		if (isRotating && draggedWindow) {
			const deltaX = event.clientX - rotateStart.x;
			const deltaY = event.clientY - rotateStart.y;
			const sensitivity = 0.008;
			draggedWindow.contentGroup.rotation.y += deltaX * sensitivity;
			draggedWindow.contentGroup.rotation.x += deltaY * sensitivity;
			rotateStart.set(event.clientX, event.clientY);
			return;
		}

		if (isResizing && draggedWindow) {
			// Calculate delta in screen pixels, convert to world units
			const currentScreen = new THREE.Vector2(event.clientX, event.clientY);
			const deltaScreen = currentScreen.clone().sub(dragStart);

			// Scale screen delta to world units based on camera view
			const viewWidth = camera.right - camera.left;
			const viewHeight = camera.top - camera.bottom;
			const rect = canvasEl.getBoundingClientRect();

			const deltaWorldX = (deltaScreen.x / rect.width) * viewWidth;
			const deltaWorldY = (deltaScreen.y / rect.height) * viewHeight;

			// Resize: increase width with rightward drag, increase height with downward drag
			const newWidth = windowStartSize.x + deltaWorldX;
			const newHeight = windowStartSize.y + deltaWorldY;
			resizeWindow(draggedWindow, newWidth, newHeight);
		} else if (isDragging && draggedWindow) {
			// Calculate delta in screen pixels, convert to world units
			const currentScreen = new THREE.Vector2(event.clientX, event.clientY);
			const deltaScreen = currentScreen.clone().sub(dragStart);

			// Scale screen delta to world units based on camera view
			const viewWidth = camera.right - camera.left;
			const viewHeight = camera.top - camera.bottom;
			const rect = canvasEl.getBoundingClientRect();

			const deltaWorldX = (deltaScreen.x / rect.width) * viewWidth;
			const deltaWorldY = -(deltaScreen.y / rect.height) * viewHeight;

			draggedWindow.group.position.x = windowStartPos.x + deltaWorldX;
			draggedWindow.group.position.y = windowStartPos.y + deltaWorldY;
			updateClipPlanes(draggedWindow);
		} else {
			// Hover effect
			raycaster.setFromCamera(mouse, camera);

			const phaseButtons: THREE.Mesh[] = [];
			const timeButtons: THREE.Mesh[] = [];
			const resizeHandles: THREE.Mesh[] = [];
			const headers: THREE.Mesh[] = [];
			for (const win of systemWindows.values()) {
				phaseButtons.push(win.phaseButton);
				timeButtons.push(win.timeButton);
				resizeHandles.push(win.resizeHandle);
				headers.push(win.header);
			}

			// Check phase/time buttons
			const phaseIntersects = raycaster.intersectObjects(phaseButtons);
			const timeIntersects = raycaster.intersectObjects(timeButtons);
			if (phaseIntersects.length > 0 || timeIntersects.length > 0) {
				canvasEl.style.cursor = 'pointer';
				return;
			}

			// Check resize handles
			const resizeIntersects = raycaster.intersectObjects(resizeHandles);
			if (resizeIntersects.length > 0) {
				canvasEl.style.cursor = 'nwse-resize';
				return;
			}

			const headerIntersects = raycaster.intersectObjects(headers);
			canvasEl.style.cursor = headerIntersects.length > 0 ? 'grab' : 'default';
		}
	}

	function onMouseUp() {
		if (draggedWindow && isDragging) {
			draggedWindow.group.position.z = 0;
		}
		isDragging = false;
		isResizing = false;
		isRotating = false;
		draggedWindow = null;
		canvasEl.style.cursor = 'default';
	}

	function onWheel(event: WheelEvent) {
		event.preventDefault();
		// Zoom camera
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
		// IMPORTANT: Read dependencies BEFORE any guards to ensure tracking.
		const systems = algebraic.systemList;

		if (!renderer || !scene) return;

		const currentIds = new Set(systems.map(s => s.id));

		// Update existing and create new windows
		for (const sys of systems) {
			updateSystemWindow(sys.id, sys);
		}

		// Remove windows for deleted systems
		for (const id of systemWindows.keys()) {
			if (!currentIds.has(id)) {
				removeSystemWindow(id);
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
		initScene();
		animate();

		return () => {
			cancelAnimationFrame(animationId);
			canvasEl.removeEventListener('mousedown', onMouseDown);
			canvasEl.removeEventListener('mousemove', onMouseMove);
			canvasEl.removeEventListener('mouseup', onMouseUp);
			canvasEl.removeEventListener('mouseleave', onMouseUp);
			canvasEl.removeEventListener('wheel', onWheel);
			renderer.dispose();
			for (const id of systemWindows.keys()) {
				removeSystemWindow(id);
			}
		};
	});
</script>

<canvas bind:this={canvasEl}></canvas>

<style>
	canvas {
		display: block;
		width: 100%;
		height: 100%;
	}
</style>
