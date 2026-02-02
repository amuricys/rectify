<!--
  AlgebraicRenderer.svelte

  ThreeJS renderer for multiple open dynamical systems.
  Each system gets its own trail, color-coded.
  Wires are shown as connecting lines between systems.
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
	let camera: THREE.PerspectiveCamera;

	// Per-system visualization
	interface SystemVis {
		trail: THREE.Vector3[];
		geometry: THREE.BufferGeometry;
		line: THREE.Line;
		point: THREE.Mesh;
		color: THREE.Color;
	}

	const systemVisuals = new Map<string, SystemVis>();
	const MAX_POINTS = 1500;

	// Color palette for systems
	const COLORS = [
		new THREE.Color(0x7ac5cd),  // cyan
		new THREE.Color(0xcd7a7a),  // coral
		new THREE.Color(0x7acd8f),  // green
		new THREE.Color(0xcd9b7a),  // orange
		new THREE.Color(0x9b7acd),  // purple
	];
	let colorIndex = 0;

	let animationId: number;

	function initScene() {
		renderer = new THREE.WebGLRenderer({
			canvas: canvasEl,
			antialias: true,
			alpha: true
		});
		renderer.setPixelRatio(window.devicePixelRatio);
		renderer.setSize(width, height);
		renderer.setClearColor(0x000000, 1);

		scene = new THREE.Scene();
		scene.fog = new THREE.Fog(0x000000, 80, 200);

		camera = new THREE.PerspectiveCamera(60, width / height, 0.1, 1000);
		camera.position.set(0, 0, 80);
		camera.lookAt(0, 0, 25);

		// Grid
		const gridHelper = new THREE.GridHelper(100, 20, 0x1a1a2e, 0x1a1a2e);
		gridHelper.rotation.x = Math.PI / 2;
		gridHelper.position.z = 0;
		scene.add(gridHelper);
	}

	function getOrCreateSystemVis(id: string): SystemVis {
		let vis = systemVisuals.get(id);
		if (!vis) {
			const color = COLORS[colorIndex % COLORS.length];
			colorIndex++;

			// Trail geometry
			const geometry = new THREE.BufferGeometry();
			const positions = new Float32Array(MAX_POINTS * 3);
			const colors = new Float32Array(MAX_POINTS * 3);
			geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
			geometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
			geometry.setDrawRange(0, 0);

			const material = new THREE.LineBasicMaterial({
				vertexColors: true,
				transparent: true,
				opacity: 0.85
			});

			const line = new THREE.Line(geometry, material);
			scene.add(line);

			// Current point
			const pointGeometry = new THREE.SphereGeometry(0.6, 16, 16);
			const pointMaterial = new THREE.MeshBasicMaterial({
				color: color,
				transparent: true,
				opacity: 0.95
			});
			const point = new THREE.Mesh(pointGeometry, pointMaterial);
			scene.add(point);

			vis = {
				trail: [],
				geometry,
				line,
				point,
				color
			};
			systemVisuals.set(id, vis);
		}
		return vis;
	}

	function updateSystemVis(id: string, state: SystemState) {
		const vis = getOrCreateSystemVis(id);

		// Map state to 3D position
		// For 3D systems (lorenz), use x,y,z directly
		// For 2D systems, use x,y,0 or phase space
		let pos: THREE.Vector3;
		if (state.state.length >= 3) {
			pos = new THREE.Vector3(state.state[0], state.state[1], state.state[2] - 25);
		} else if (state.state.length >= 2) {
			// 2D system - spread them out in z
			const offset = Array.from(systemVisuals.keys()).indexOf(id) * 10;
			pos = new THREE.Vector3(state.state[0] * 5, state.state[1] * 5, offset);
		} else {
			pos = new THREE.Vector3(state.state[0] * 5, 0, 0);
		}

		vis.trail.push(pos);
		if (vis.trail.length > MAX_POINTS) {
			vis.trail.shift();
		}

		// Update geometry
		const positions = vis.geometry.attributes.position.array as Float32Array;
		const colors = vis.geometry.attributes.color.array as Float32Array;

		for (let i = 0; i < vis.trail.length; i++) {
			const p = vis.trail[i];
			positions[i * 3] = p.x;
			positions[i * 3 + 1] = p.y;
			positions[i * 3 + 2] = p.z;

			const t = i / vis.trail.length;
			const intensity = t * t;
			colors[i * 3] = vis.color.r * intensity;
			colors[i * 3 + 1] = vis.color.g * intensity;
			colors[i * 3 + 2] = vis.color.b * intensity;
		}

		vis.geometry.attributes.position.needsUpdate = true;
		vis.geometry.attributes.color.needsUpdate = true;
		vis.geometry.setDrawRange(0, vis.trail.length);

		// Update point
		const lastPoint = vis.trail[vis.trail.length - 1];
		if (lastPoint) {
			vis.point.position.copy(lastPoint);
		}
	}

	function removeSystemVis(id: string) {
		const vis = systemVisuals.get(id);
		if (vis) {
			scene.remove(vis.line);
			scene.remove(vis.point);
			vis.geometry.dispose();
			systemVisuals.delete(id);
		}
	}

	function animate() {
		animationId = requestAnimationFrame(animate);
		scene.rotation.z += 0.0003;
		renderer.render(scene, camera);
	}

	// React to world state changes
	$effect(() => {
		const world = algebraic.world;
		if (!world || !renderer) return;

		// Update or create visuals for each system
		const currentIds = new Set(Object.keys(world.systems));

		for (const [id, state] of Object.entries(world.systems)) {
			updateSystemVis(id, state);
		}

		// Remove visuals for deleted systems
		for (const id of systemVisuals.keys()) {
			if (!currentIds.has(id)) {
				removeSystemVis(id);
			}
		}
	});

	// React to size changes
	$effect(() => {
		if (renderer && camera) {
			renderer.setSize(width, height);
			camera.aspect = width / height;
			camera.updateProjectionMatrix();
		}
	});

	onMount(() => {
		initScene();
		animate();

		return () => {
			cancelAnimationFrame(animationId);
			renderer.dispose();
			for (const vis of systemVisuals.values()) {
				vis.geometry.dispose();
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
