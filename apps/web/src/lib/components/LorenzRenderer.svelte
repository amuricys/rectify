<!--
  LorenzRenderer.svelte

  ThreeJS-based renderer for the Lorenz attractor.
  Displays the trajectory as a glowing line that fades over time.
-->
<script lang="ts">
	import { onMount } from 'svelte';
	import * as THREE from 'three';
	import { dynamics, type DynamicsState } from '$lib/stores/dynamics.svelte';

	interface Props {
		width: number;
		height: number;
	}

	let { width, height }: Props = $props();

	let canvasEl: HTMLCanvasElement;
	let renderer: THREE.WebGLRenderer;
	let scene: THREE.Scene;
	let camera: THREE.PerspectiveCamera;

	// Trail management
	const MAX_POINTS = 2000;
	let trailPoints: THREE.Vector3[] = [];
	let trailGeometry: THREE.BufferGeometry;
	let trailLine: THREE.Line;

	// Current point indicator
	let pointMesh: THREE.Mesh;

	// Animation
	let animationId: number;

	function initScene() {
		// Renderer
		renderer = new THREE.WebGLRenderer({
			canvas: canvasEl,
			antialias: true,
			alpha: true
		});
		renderer.setPixelRatio(window.devicePixelRatio);
		renderer.setSize(width, height);
		renderer.setClearColor(0x000000, 1);

		// Scene
		scene = new THREE.Scene();
		scene.fog = new THREE.Fog(0x000000, 50, 150);

		// Camera - positioned to see the Lorenz attractor well
		camera = new THREE.PerspectiveCamera(60, width / height, 0.1, 1000);
		camera.position.set(0, 0, 80);
		camera.lookAt(0, 0, 25);

		// Trail geometry
		trailGeometry = new THREE.BufferGeometry();
		const positions = new Float32Array(MAX_POINTS * 3);
		const colors = new Float32Array(MAX_POINTS * 3);
		trailGeometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
		trailGeometry.setAttribute('color', new THREE.BufferAttribute(colors, 3));
		trailGeometry.setDrawRange(0, 0);

		const trailMaterial = new THREE.LineBasicMaterial({
			vertexColors: true,
			linewidth: 1,
			transparent: true,
			opacity: 0.9
		});

		trailLine = new THREE.Line(trailGeometry, trailMaterial);
		scene.add(trailLine);

		// Current point - glowing sphere
		const pointGeometry = new THREE.SphereGeometry(0.5, 16, 16);
		const pointMaterial = new THREE.MeshBasicMaterial({
			color: 0x7ac5cd,
			transparent: true,
			opacity: 0.9
		});
		pointMesh = new THREE.Mesh(pointGeometry, pointMaterial);
		scene.add(pointMesh);

		// Subtle grid for reference
		const gridHelper = new THREE.GridHelper(100, 20, 0x1a1a2e, 0x1a1a2e);
		gridHelper.rotation.x = Math.PI / 2;
		gridHelper.position.z = 0;
		scene.add(gridHelper);
	}

	function updateTrail(state: DynamicsState) {
		// Add new point
		const point = new THREE.Vector3(state.x, state.y, state.z - 25); // Center z around attractor
		trailPoints.push(point);

		// Trim if too long
		if (trailPoints.length > MAX_POINTS) {
			trailPoints.shift();
		}

		// Update geometry
		const positions = trailGeometry.attributes.position.array as Float32Array;
		const colors = trailGeometry.attributes.color.array as Float32Array;

		for (let i = 0; i < trailPoints.length; i++) {
			const p = trailPoints[i];
			positions[i * 3] = p.x;
			positions[i * 3 + 1] = p.y;
			positions[i * 3 + 2] = p.z;

			// Color gradient: older = dimmer, newer = brighter cyan
			const t = i / trailPoints.length;
			const intensity = t * t; // Quadratic falloff
			colors[i * 3] = 0.3 * intensity;     // R
			colors[i * 3 + 1] = 0.6 * intensity; // G
			colors[i * 3 + 2] = 0.8 * intensity; // B
		}

		trailGeometry.attributes.position.needsUpdate = true;
		trailGeometry.attributes.color.needsUpdate = true;
		trailGeometry.setDrawRange(0, trailPoints.length);

		// Update current point position
		const lastPoint = trailPoints[trailPoints.length - 1];
		if (lastPoint) {
			pointMesh.position.copy(lastPoint);
		}
	}

	function animate() {
		animationId = requestAnimationFrame(animate);

		// Slow rotation for visual interest
		scene.rotation.z += 0.0005;

		renderer.render(scene, camera);
	}

	// React to state changes
	$effect(() => {
		const state = dynamics.currentState;
		if (state && renderer) {
			updateTrail(state);
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
			trailGeometry.dispose();
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
