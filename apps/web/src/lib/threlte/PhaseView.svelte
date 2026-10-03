<!--
	PhaseView.svelte
	3D trajectory visualization inside a system window.
	Uses useTask for per-frame geometry updates.
-->
<script lang="ts">
	import { T, useTask } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic, type SystemState } from '$lib/stores/algebraic.svelte';
	import {
		HEADER_HEIGHT, MAX_POINTS, PHASE_HALF_EXTENT, PHASE_BOUNDS_HISTORY,
		STATE_COLORS, CANVAS_FONT
	} from './constants';
	import { createTextSprite, formatTickValue, computeNiceTicks } from './utils/sprites';

	interface Props {
		sys: SystemState;
		color: THREE.Color;
		nstates: number;
		stateNames: string[];
		windowWidth: number;
		windowHeight: number;
		clipPlanes: THREE.Plane[];
		bounds: { min: THREE.Vector3; max: THREE.Vector3; initialized: boolean };
		contentRotX: number;
		contentRotY: number;
	}

	let { sys, color, nstates, stateNames, windowWidth, windowHeight, clipPlanes, bounds, contentRotX, contentRotY }: Props = $props();

	// Pre-allocate trail geometry
	const trailPositions = new Float32Array(MAX_POINTS * 3);
	const trailColors = new Float32Array(MAX_POINTS * 3);
	const trailGeometry = new THREE.BufferGeometry();
	trailGeometry.setAttribute('position', new THREE.BufferAttribute(trailPositions, 3));
	trailGeometry.setAttribute('color', new THREE.BufferAttribute(trailColors, 3));
	trailGeometry.setDrawRange(0, 0);

	// Current point
	let pointPos = $state(new THREE.Vector3(0, 0, 0));

	// Axes
	const axisLength = 500;
	const numAxes = Math.min(nstates, 3);
	const axisConfigs = [
		{ color: 0xd4785a, dir: new THREE.Vector3(1, 0, 0), name: stateNames[0] || 'x' },
		{ color: 0xc9a84c, dir: new THREE.Vector3(0, 1, 0), name: stateNames[1] || 'y' },
		{ color: 0x8a9b68, dir: new THREE.Vector3(0, 0, 1), name: stateNames[2] || 'z' },
	];

	// Phase tick marks (pre-allocated canvas sprites)
	const tickSprites: THREE.Sprite[] = [];
	for (let i = 0; i < 15; i++) {
		const canvas = document.createElement('canvas');
		canvas.width = 64;
		canvas.height = 24;
		const texture = new THREE.CanvasTexture(canvas);
		const material = new THREE.SpriteMaterial({
			map: texture, transparent: true, depthTest: false,
			clippingPlanes: clipPlanes
		});
		const sprite = new THREE.Sprite(material);
		sprite.scale.set(4, 1.5, 1);
		sprite.visible = false;
		tickSprites.push(sprite);
	}

	let lastTickBounds: { min: THREE.Vector3; max: THREE.Vector3 } | null = null;

	function getPhaseMapping(): { center: number[]; scale: number } {
		if (!bounds.initialized) return { center: [0, 0, 0], scale: 0.5 };
		const ranges = [
			bounds.max.x - bounds.min.x,
			bounds.max.y - bounds.min.y,
			bounds.max.z - bounds.min.z
		];
		const center = [
			(bounds.min.x + bounds.max.x) / 2,
			(bounds.min.y + bounds.max.y) / 2,
			(bounds.min.z + bounds.max.z) / 2
		];
		const maxRange = Math.max(...ranges.slice(0, numAxes), 0.001);
		const scale = (PHASE_HALF_EXTENT * 2) / maxRange;
		return { center, scale };
	}

	function mapStateToLocal(state: number[]): THREE.Vector3 {
		const mapping = getPhaseMapping();
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
		} else if (nstates >= 1) {
			result.x = (sx - mapping.center[0]) * mapping.scale;
		}
		return result;
	}

	function isFiniteState(state: number[]): boolean {
		for (let i = 0; i < numAxes; i++) {
			if (!Number.isFinite(state[i])) return false;
		}
		return true;
	}

	function recomputeBounds(history: number[][], currentState: number[]) {
		const min = new THREE.Vector3(Infinity, Infinity, Infinity);
		const max = new THREE.Vector3(-Infinity, -Infinity, -Infinity);
		let hasFinite = false;
		const start = Math.max(0, history.length - PHASE_BOUNDS_HISTORY);

		for (let i = start; i < history.length; i++) {
			const state = history[i];
			if (!isFiniteState(state)) continue;
			if (nstates >= 1) { min.x = Math.min(min.x, state[0]); max.x = Math.max(max.x, state[0]); }
			if (nstates >= 2) { min.y = Math.min(min.y, state[1]); max.y = Math.max(max.y, state[1]); }
			if (nstates >= 3) { min.z = Math.min(min.z, state[2]); max.z = Math.max(max.z, state[2]); }
			hasFinite = true;
		}
		if (isFiniteState(currentState)) {
			if (nstates >= 1) { min.x = Math.min(min.x, currentState[0]); max.x = Math.max(max.x, currentState[0]); }
			if (nstates >= 2) { min.y = Math.min(min.y, currentState[1]); max.y = Math.max(max.y, currentState[1]); }
			if (nstates >= 3) { min.z = Math.min(min.z, currentState[2]); max.z = Math.max(max.z, currentState[2]); }
			hasFinite = true;
		}
		if (!hasFinite) {
			bounds.min.set(-1, -1, -1);
			bounds.max.set(1, 1, 1);
			bounds.initialized = true;
			return;
		}
		// Keep origin visible
		if (nstates >= 1) { min.x = Math.min(min.x, 0); max.x = Math.max(max.x, 0); }
		if (nstates >= 2) { min.y = Math.min(min.y, 0); max.y = Math.max(max.y, 0); }
		if (nstates >= 3) { min.z = Math.min(min.z, 0); max.z = Math.max(max.z, 0); }
		bounds.min.copy(min);
		bounds.max.copy(max);
		bounds.initialized = true;
	}

	function updatePhaseTickMarks() {
		if (!bounds.initialized) return;
		if (lastTickBounds) {
			const rangeX = bounds.max.x - bounds.min.x;
			const rangeY = bounds.max.y - bounds.min.y;
			const rangeZ = bounds.max.z - bounds.min.z;
			const dX = Math.abs(lastTickBounds.max.x - bounds.max.x) + Math.abs(lastTickBounds.min.x - bounds.min.x);
			const dY = Math.abs(lastTickBounds.max.y - bounds.max.y) + Math.abs(lastTickBounds.min.y - bounds.min.y);
			const dZ = Math.abs(lastTickBounds.max.z - bounds.max.z) + Math.abs(lastTickBounds.min.z - bounds.min.z);
			if (dX < rangeX * 0.05 && dY < rangeY * 0.05 && dZ < rangeZ * 0.05) return;
		}
		lastTickBounds = { min: bounds.min.clone(), max: bounds.max.clone() };
		const mapping = getPhaseMapping();
		let tickIdx = 0;
		const axes = [
			{ dim: 0, min: bounds.min.x, max: bounds.max.x, dir: new THREE.Vector3(1, 0, 0), offset: new THREE.Vector3(0, -1.5, 0) },
			{ dim: 1, min: bounds.min.y, max: bounds.max.y, dir: new THREE.Vector3(0, 1, 0), offset: new THREE.Vector3(-1.5, 0, 0) },
			{ dim: 2, min: bounds.min.z, max: bounds.max.z, dir: new THREE.Vector3(0, 0, 1), offset: new THREE.Vector3(0, -1.5, 0) },
		];
		for (let a = 0; a < numAxes; a++) {
			const ax = axes[a];
			const ticks = computeNiceTicks(ax.min, ax.max, 5);
			for (const val of ticks) {
				if (tickIdx >= 15) break;
				const sprite = tickSprites[tickIdx];
				const localVal = (val - mapping.center[a]) * mapping.scale;
				const pos = ax.dir.clone().multiplyScalar(localVal).add(ax.offset);
				sprite.position.copy(pos);
				sprite.visible = true;
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
		for (let i = tickIdx; i < 15; i++) tickSprites[i].visible = false;
	}

	// Animation loop: update trail and point every frame
	useTask(() => {
		const history = algebraic.getHistory(sys.id);
		if (history.length === 0) return;

		recomputeBounds(history, sys.state);

		const phaseHistory = history.filter(s => isFiniteState(s));
		const len = Math.min(phaseHistory.length, MAX_POINTS);
		if (len === 0) return;

		const localPos = mapStateToLocal(sys.state);
		pointPos = localPos;

		const positions = trailGeometry.attributes.position.array as Float32Array;
		const colors = trailGeometry.attributes.color.array as Float32Array;

		for (let i = 0; i < len; i++) {
			const state = phaseHistory[phaseHistory.length - len + i];
			const pos = mapStateToLocal(state);
			positions[i * 3] = pos.x;
			positions[i * 3 + 1] = pos.y;
			positions[i * 3 + 2] = pos.z;
			const t = i / len;
			const intensity = 0.4 + t * 0.6;
			colors[i * 3] = color.r * intensity;
			colors[i * 3 + 1] = color.g * intensity;
			colors[i * 3 + 2] = color.b * intensity;
		}

		trailGeometry.attributes.position.needsUpdate = true;
		trailGeometry.attributes.color.needsUpdate = true;
		trailGeometry.setDrawRange(0, len);

		updatePhaseTickMarks();
	});
</script>

<!-- Content group: offset for header, rotated for 3D -->
<T.Group
	position.y={-HEADER_HEIGHT / 2}
	rotation.x={contentRotX}
	rotation.y={contentRotY}
>
	<!-- Trail line -->
	<T.Line geometry={trailGeometry}>
		<T.LineBasicMaterial vertexColors transparent opacity={0.9} clippingPlanes={clipPlanes} />
	</T.Line>

	<!-- Current point sphere -->
	<T.Mesh position={[pointPos.x, pointPos.y, pointPos.z]}>
		<T.SphereGeometry args={[0.8, 16, 16]} />
		<T.MeshBasicMaterial {color} transparent opacity={0.95} clippingPlanes={clipPlanes} />
	</T.Mesh>

	<!-- Axes -->
	{#each axisConfigs.slice(0, numAxes) as axis, i}
		{@const points = [
			axis.dir.clone().multiplyScalar(-axisLength),
			axis.dir.clone().multiplyScalar(axisLength)
		]}
		<T.Line>
			<T.BufferGeometry>
				<T.BufferAttribute
					attach="attributes.position"
					args={[new Float32Array(points.flatMap(p => [p.x, p.y, p.z])), 3]}
				/>
			</T.BufferGeometry>
			<T.LineBasicMaterial color={0x3a2e24} clippingPlanes={clipPlanes} />
		</T.Line>

		<!-- Arrowhead cone -->
		{@const arrowPos = axis.dir.clone().multiplyScalar(19)}
		<T.Mesh
			position={[arrowPos.x, arrowPos.y, arrowPos.z]}
			rotation.z={i === 0 ? -Math.PI / 2 : 0}
			rotation.x={i === 2 ? Math.PI / 2 : 0}
		>
			<T.ConeGeometry args={[0.6, 2, 8]} />
			<T.MeshBasicMaterial color={axis.color} clippingPlanes={clipPlanes} />
		</T.Mesh>

		<!-- Axis label -->
		{@const labelPos = axis.dir.clone().multiplyScalar(18)}
		{@const label = createTextSprite(axis.name, new THREE.Color(axis.color))}
		<T is={label} position={[labelPos.x, labelPos.y, labelPos.z]} scale={[3, 1.5, 1]} />
	{/each}

	<!-- Phase tick marks -->
	{#each tickSprites as sprite}
		<T is={sprite} />
	{/each}
</T.Group>
