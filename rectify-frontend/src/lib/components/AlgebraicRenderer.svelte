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

	// Per-system window visualization
	interface SystemWindow {
		group: THREE.Group;
		header: THREE.Mesh;
		headerLabel: THREE.Sprite;
		contentGroup: THREE.Group; // Contains the 3D visualization
		trailGeometry: THREE.BufferGeometry;
		trail: THREE.Line;
		point: THREE.Mesh;
		color: THREE.Color;
		bounds: {
			min: THREE.Vector3;
			max: THREE.Vector3;
			initialized: boolean;
		};
	}

	const systemWindows = new Map<string, SystemWindow>();
	const MAX_POINTS = 2000;

	// Drag state
	let isDragging = false;
	let draggedWindow: SystemWindow | null = null;
	let dragStart = new THREE.Vector2();
	let windowStartPos = new THREE.Vector2();

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

	function createSystemWindow(id: string, sys: SystemState): SystemWindow {
		const color = COLORS[colorIndex % COLORS.length];
		colorIndex++;

		const group = new THREE.Group();
		group.userData = { systemId: id };

		// Position based on sys.position or default grid layout
		const windowIndex = systemWindows.size;
		const gridCols = 3;
		const spacing = WINDOW_WIDTH + 8;
		const startX = -spacing;
		const startY = 40;
		const defaultX = startX + (windowIndex % gridCols) * spacing;
		const defaultY = startY - Math.floor(windowIndex / gridCols) * (WINDOW_HEIGHT + HEADER_HEIGHT + 8);

		// Position in XY plane (Z=0 for window plane)
		group.position.set(
			sys.position?.x ?? defaultX,
			sys.position?.y ?? defaultY,
			0
		);

		// Window border
		const borderGeometry = new THREE.EdgesGeometry(new THREE.PlaneGeometry(WINDOW_WIDTH, WINDOW_HEIGHT + HEADER_HEIGHT));
		const borderMaterial = new THREE.LineBasicMaterial({
			color: color,
			transparent: true,
			opacity: 0.5
		});
		const border = new THREE.LineSegments(borderGeometry, borderMaterial);
		border.position.z = 0;
		group.add(border);

		// Header bar (draggable)
		const headerGeometry = new THREE.PlaneGeometry(WINDOW_WIDTH, HEADER_HEIGHT);
		const headerMaterial = new THREE.MeshBasicMaterial({
			color: color,
			transparent: true,
			opacity: 0.3,
			side: THREE.DoubleSide
		});
		const header = new THREE.Mesh(headerGeometry, headerMaterial);
		header.position.set(0, WINDOW_HEIGHT / 2, 0);
		header.userData = { isHeader: true, systemId: id };
		group.add(header);

		// Label in header
		const systemName = sys.templateId.split('_')[0];
		const label = createTextSprite(systemName, color);
		label.position.set(-WINDOW_WIDTH / 2 + 12, WINDOW_HEIGHT / 2, 1);
		group.add(label);

		// Content group for 3D visualization
		const contentGroup = new THREE.Group();
		contentGroup.position.set(0, -HEADER_HEIGHT / 2, 0);
		// Tilt to show 3D depth (isometric-ish view)
		contentGroup.rotation.x = -0.4;
		contentGroup.rotation.y = 0.3;
		group.add(contentGroup);

		// Trail geometry
		const trailGeometry = new THREE.BufferGeometry();
		const positions = new Float32Array(MAX_POINTS * 3);
		const colors = new Float32Array(MAX_POINTS * 3);
		trailGeometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
		trailGeometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
		trailGeometry.setDrawRange(0, 0);

		const trailMaterial = new THREE.LineBasicMaterial({
			vertexColors: true,
			transparent: true,
			opacity: 0.9
		});

		const trail = new THREE.Line(trailGeometry, trailMaterial);
		contentGroup.add(trail);

		// Current point sphere
		const pointGeometry = new THREE.SphereGeometry(0.8, 16, 16);
		const pointMaterial = new THREE.MeshBasicMaterial({
			color: color,
			transparent: true,
			opacity: 0.95
		});
		const point = new THREE.Mesh(pointGeometry, pointMaterial);
		contentGroup.add(point);

		scene.add(group);

		return {
			group,
			header,
			headerLabel: label,
			contentGroup,
			trailGeometry,
			trail,
			point,
			color,
			bounds: {
				min: new THREE.Vector3(Infinity, Infinity, Infinity),
				max: new THREE.Vector3(-Infinity, -Infinity, -Infinity),
				initialized: false
			}
		};
	}

	function getOrCreateWindow(id: string, sys: SystemState): SystemWindow {
		let win = systemWindows.get(id);
		if (!win) {
			win = createSystemWindow(id, sys);
			systemWindows.set(id, win);
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

	function updateSystemWindow(id: string, sys: SystemState) {
		const win = getOrCreateWindow(id, sys);
		const history = algebraic.getHistory(id);

		// Update bounds incrementally
		updateBounds(win, sys.state);

		// Update current point position
		const localPos = mapStateToLocal(sys.state, win, sys.nstates);
		win.point.position.copy(localPos);

		if (history.length === 0) return;

		const positions = win.trailGeometry.attributes.position.array as Float32Array;
		const colors = win.trailGeometry.attributes.color.array as Float32Array;
		const len = Math.min(history.length, MAX_POINTS);

		for (let i = 0; i < len; i++) {
			const state = history[i];
			const pos = mapStateToLocal(state, win, sys.nstates);

			positions[i * 3] = pos.x;
			positions[i * 3 + 1] = pos.y;
			positions[i * 3 + 2] = pos.z;

			// Color: consistent brightness, just fade alpha from old to new
			// Use full color intensity to avoid dark regions
			const t = i / len;
			const intensity = 0.4 + t * 0.6; // Range from 0.4 to 1.0
			colors[i * 3] = win.color.r * intensity;
			colors[i * 3 + 1] = win.color.g * intensity;
			colors[i * 3 + 2] = win.color.b * intensity;
		}

		win.trailGeometry.attributes.position.needsUpdate = true;
		win.trailGeometry.attributes.color.needsUpdate = true;
		win.trailGeometry.setDrawRange(0, len);
	}

	function removeSystemWindow(id: string) {
		const win = systemWindows.get(id);
		if (win) {
			scene.remove(win.group);
			win.trailGeometry.dispose();
			(win.trail.material as THREE.Material).dispose();
			(win.point.material as THREE.Material).dispose();
			(win.point.geometry as THREE.BufferGeometry).dispose();
			(win.header.material as THREE.Material).dispose();
			win.header.geometry.dispose();
			(win.headerLabel.material as THREE.SpriteMaterial).map?.dispose();
			(win.headerLabel.material as THREE.Material).dispose();
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

		// Get all header meshes
		const headers: THREE.Mesh[] = [];
		for (const win of systemWindows.values()) {
			headers.push(win.header);
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

				// Bring window to front (higher z)
				win.group.position.z = 10;

				canvasEl.style.cursor = 'grabbing';
			}
		}
	}

	function onMouseMove(event: MouseEvent) {
		updateMousePosition(event);

		if (isDragging && draggedWindow) {
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
		} else {
			// Hover effect
			raycaster.setFromCamera(mouse, camera);
			const headers: THREE.Mesh[] = [];
			for (const win of systemWindows.values()) {
				headers.push(win.header);
			}

			const intersects = raycaster.intersectObjects(headers);
			canvasEl.style.cursor = intersects.length > 0 ? 'grab' : 'default';
		}
	}

	function onMouseUp() {
		if (draggedWindow) {
			// Reset z position
			draggedWindow.group.position.z = 0;
		}
		isDragging = false;
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
