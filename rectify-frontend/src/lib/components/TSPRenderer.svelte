<!--
  TSPRenderer.svelte

  ThreeJS visualization of TSP optimization.
  Shows cities as points, current tour as lines, best tour as reference.
-->
<script lang="ts">
	import { onMount } from 'svelte';
	import * as THREE from 'three';
	import { optimization } from '$lib/stores/optimization.svelte';

	interface Props {
		width: number;
		height: number;
	}

	let { width, height }: Props = $props();

	let canvasEl: HTMLCanvasElement;
	let renderer: THREE.WebGLRenderer;
	let scene: THREE.Scene;
	let camera: THREE.PerspectiveCamera;

	// Geometry
	let cityMeshes: THREE.Mesh[] = [];
	let currentTourLine: THREE.Line;
	let bestTourLine: THREE.Line;

	// Animation
	let animationId: number;

	const CITY_COLOR = 0x7ac5cd;
	const CURRENT_TOUR_COLOR = 0xcdaa7a;
	const BEST_TOUR_COLOR = 0x5a8a5a;

	function initScene() {
		renderer = new THREE.WebGLRenderer({
			canvas: canvasEl,
			antialias: true,
			alpha: true
		});
		renderer.setPixelRatio(window.devicePixelRatio);
		renderer.setSize(width, height);
		renderer.setClearColor(0x0a0a0f, 1);

		scene = new THREE.Scene();

		// Camera looking down at the map
		camera = new THREE.PerspectiveCamera(50, width / height, 0.1, 1000);
		camera.position.set(0, 0, 400);
		camera.lookAt(0, 0, 0);

		// Ambient light
		const ambient = new THREE.AmbientLight(0x404040, 0.5);
		scene.add(ambient);

		// Point light for some depth
		const pointLight = new THREE.PointLight(0xffffff, 1, 1000);
		pointLight.position.set(100, 100, 200);
		scene.add(pointLight);

		// Grid for reference
		const gridHelper = new THREE.GridHelper(400, 20, 0x1a1a2e, 0x1a1a2e);
		gridHelper.rotation.x = Math.PI / 2;
		scene.add(gridHelper);

		// Current tour line
		const currentGeom = new THREE.BufferGeometry();
		const currentMat = new THREE.LineBasicMaterial({
			color: CURRENT_TOUR_COLOR,
			linewidth: 2,
			transparent: true,
			opacity: 0.9
		});
		currentTourLine = new THREE.Line(currentGeom, currentMat);
		scene.add(currentTourLine);

		// Best tour line
		const bestGeom = new THREE.BufferGeometry();
		const bestMat = new THREE.LineBasicMaterial({
			color: BEST_TOUR_COLOR,
			linewidth: 1,
			transparent: true,
			opacity: 0.4
		});
		bestTourLine = new THREE.Line(bestGeom, bestMat);
		scene.add(bestTourLine);
	}

	function updateCities(cities: { x: number; y: number }[]) {
		// Remove old city meshes
		for (const mesh of cityMeshes) {
			scene.remove(mesh);
			mesh.geometry.dispose();
		}
		cityMeshes = [];

		// Create new city meshes
		const cityGeom = new THREE.SphereGeometry(4, 16, 16);
		const cityMat = new THREE.MeshPhongMaterial({
			color: CITY_COLOR,
			emissive: CITY_COLOR,
			emissiveIntensity: 0.3,
			transparent: true,
			opacity: 0.9
		});

		for (const city of cities) {
			const mesh = new THREE.Mesh(cityGeom, cityMat);
			mesh.position.set(city.x, city.y, 0);
			scene.add(mesh);
			cityMeshes.push(mesh);
		}
	}

	function updateTourLine(line: THREE.Line, cities: { x: number; y: number }[]) {
		if (cities.length < 2) return;

		// Create closed loop
		const points: THREE.Vector3[] = cities.map(c => new THREE.Vector3(c.x, c.y, 0));
		points.push(points[0].clone()); // close the loop

		const positions = new Float32Array(points.length * 3);
		for (let i = 0; i < points.length; i++) {
			positions[i * 3] = points[i].x;
			positions[i * 3 + 1] = points[i].y;
			positions[i * 3 + 2] = points[i].z;
		}

		line.geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
		line.geometry.attributes.position.needsUpdate = true;
	}

	function animate() {
		animationId = requestAnimationFrame(animate);
		renderer.render(scene, camera);
	}

	// React to state changes
	$effect(() => {
		const state = optimization.state;
		if (!state || !renderer) return;

		// Update cities (from current tour)
		if (state.current?.cities) {
			updateCities(state.current.cities);
			updateTourLine(currentTourLine, state.current.cities);
		}

		// Update best tour
		if (state.best?.cities) {
			updateTourLine(bestTourLine, state.best.cities);
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
			for (const mesh of cityMeshes) {
				mesh.geometry.dispose();
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
