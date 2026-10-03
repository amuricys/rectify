<!--
	PortDot.svelte
	Input/output port circle + ring + hit area + label.
	Handles wire drag initiation and drop target highlighting.
-->
<script lang="ts">
	import { T } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic } from '$lib/stores/algebraic.svelte';
	import type { WireDragState } from './interactionTypes';
	import { createTextSprite } from './utils/sprites';

	interface Props {
		systemId: string;
		portIndex: number;
		isOutput: boolean;
		portName: string;
		color: THREE.Color;
		xOffset: number;
		yPos: number;
		wireDrag: WireDragState;
	}

	let { systemId, portIndex, isOutput, portName, color, xOffset, yPos, wireDrag }: Props = $props();

	let hovered = $state(false);

	// Check if this input port is connected
	let isConnected = $derived(
		!isOutput && algebraic.wireList.some(w => w.toSystem === systemId && w.toPort === portIndex)
	);

	// Port fill color and opacity
	let fillColor = $derived(
		hovered && wireDrag.active ? 0xc9a84c :
		isOutput ? new THREE.Color(color.r, color.g, color.b) : 0x3a2e24
	);
	let fillOpacity = $derived(
		hovered && wireDrag.active ? 1.0 :
		isConnected ? 0.8 : (isOutput ? 0.8 : 0.3)
	);

	// Label offset
	let labelOffset = $derived(isOutput ? 5 : -5);
	let labelY = $derived(isConnected ? yPos + 2 : yPos);

	function onPointerDown(e: any) {
		if (isOutput) {
			// Start wire drag from output
			wireDrag.active = true;
			wireDrag.sourcePort = { systemId, portIndex, isOutput: true };
			wireDrag.pendingRewire = null;
			e.stopPropagation?.();
		} else if (isConnected) {
			// Start re-wire from connected input
			const connectedWire = algebraic.wireList.find(
				w => w.toSystem === systemId && w.toPort === portIndex
			);
			if (connectedWire) {
				wireDrag.active = true;
				wireDrag.sourcePort = {
					systemId: connectedWire.fromSystem,
					portIndex: connectedWire.fromPort,
					isOutput: true
				};
				wireDrag.pendingRewire = connectedWire;
				e.stopPropagation?.();
			}
		}
	}

	function onPointerUp(e: any) {
		if (!wireDrag.active || !wireDrag.sourcePort) return;
		if (!isOutput && wireDrag.sourcePort.systemId !== systemId) {
			// Dropping on an input port of a different system
			const source = wireDrag.sourcePort;
			const sameAsOriginal = wireDrag.pendingRewire &&
				wireDrag.pendingRewire.toSystem === systemId &&
				wireDrag.pendingRewire.toPort === portIndex;

			if (!sameAsOriginal) {
				const rewire = wireDrag.pendingRewire;
				if (rewire) {
					const targetSys = algebraic.systemList.find(s => s.id === rewire.toSystem);
					if (targetSys) algebraic.setState(rewire.toSystem, [...targetSys.state]);
					algebraic.unwire(rewire.id);
				}
				const existingWire = algebraic.wireList.find(
					w => w.toSystem === systemId && w.toPort === portIndex
				);
				if (existingWire && (!rewire || existingWire.id !== rewire.id)) {
					algebraic.unwire(existingWire.id);
				}
				algebraic.wire(source.systemId, source.portIndex, systemId, portIndex);
			}

			wireDrag.active = false;
			wireDrag.sourcePort = null;
			wireDrag.pendingRewire = null;
			e.stopPropagation?.();
		}
	}

	const label = createTextSprite(portName, new THREE.Color(0x9a8b78));
</script>

<!-- Filled circle -->
<T.Mesh
	position={[xOffset, yPos, 5]}
	onpointerdown={onPointerDown}
	onpointerup={onPointerUp}
	onpointerenter={() => hovered = true}
	onpointerleave={() => hovered = false}
>
	<T.CircleGeometry args={[1.0, 16]} />
	<T.MeshBasicMaterial
		color={fillColor}
		transparent
		opacity={fillOpacity}
		side={THREE.DoubleSide}
		depthTest={false}
	/>
</T.Mesh>

<!-- Ring outline -->
<T.LineLoop position={[xOffset, yPos, 5]}>
	<T.BufferGeometry>
		{@const points = Array.from({ length: 17 }, (_, i) => {
			const angle = (i / 16) * Math.PI * 2;
			return new THREE.Vector3(Math.cos(angle) * 1.2, Math.sin(angle) * 1.2, 0);
		})}
		<T.BufferAttribute
			attach="attributes.position"
			args={[new Float32Array(points.flatMap(p => [p.x, p.y, p.z])), 3]}
		/>
	</T.BufferGeometry>
	<T.LineBasicMaterial color={color} transparent opacity={0.6} depthTest={false} />
</T.LineLoop>

<!-- Hit area (invisible, larger) -->
<T.Mesh
	position={[xOffset, yPos, 4]}
	onpointerdown={onPointerDown}
	onpointerup={onPointerUp}
	onpointerenter={() => hovered = true}
	onpointerleave={() => hovered = false}
>
	<T.PlaneGeometry args={[4, 4]} />
	<T.MeshBasicMaterial transparent opacity={0} side={THREE.DoubleSide} depthTest={false} />
</T.Mesh>

<!-- Label -->
<T is={label} position={[xOffset + labelOffset, labelY, 5]} scale={[8, 2.5, 1]} />
