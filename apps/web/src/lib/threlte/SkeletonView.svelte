<!--
	SkeletonView.svelte
	Bug #7 fix: Positions member boxes at RELATIVE positions mirroring
	actual member system positions, scaled to fit within the composite window.
	Instead of grid layout.
-->
<script lang="ts">
	import { T } from '@threlte/core';
	import * as THREE from 'three';
	import { algebraic, type CompositeGroup } from '$lib/stores/algebraic.svelte';
	import { HEADER_HEIGHT, CORNER_RADIUS } from './constants';
	import { createRoundedRectBorder } from './utils/geometry';
	import { createTextSprite } from './utils/sprites';
	import { getSystemColor } from './utils/colorMap';
	import { getPosition } from './positionStore';

	interface Props {
		group: CompositeGroup;
		compositeWidth: number;
		compositeHeight: number;
		compositeX: number;
		compositeY: number;
	}

	let { group, compositeWidth, compositeHeight, compositeX, compositeY }: Props = $props();

	const boxW = 10, boxH = 7;

	// Compute member positions relative to composite center, scaled to fit
	let memberLayout = $derived.by(() => {
		const members = group.memberSystemIds;
		const positions: Array<{ id: string; x: number; y: number; name: string; color: THREE.Color }> = [];

		// Get actual world positions of members from position store
		const worldPositions: Array<{ id: string; x: number; y: number }> = [];
		for (const memberId of members) {
			const pos = getPosition(memberId);
			worldPositions.push({
				id: memberId,
				x: pos.x,
				y: pos.y
			});
		}

		if (worldPositions.length === 0) return { positions: [], wireLines: [] };

		// Compute bounding box of member positions
		let minX = Infinity, maxX = -Infinity, minY = Infinity, maxY = -Infinity;
		for (const wp of worldPositions) {
			minX = Math.min(minX, wp.x);
			maxX = Math.max(maxX, wp.x);
			minY = Math.min(minY, wp.y);
			maxY = Math.max(maxY, wp.y);
		}

		const rangeX = maxX - minX || 1;
		const rangeY = maxY - minY || 1;
		const centerX = (minX + maxX) / 2;
		const centerY = (minY + maxY) / 2;

		// Available space in skeleton area (inside composite, below header)
		const availW = compositeWidth - boxW - 8;
		const availH = compositeHeight - HEADER_HEIGHT - boxH - 8;
		const scale = Math.min(availW / rangeX, availH / rangeY, 1);

		for (const wp of worldPositions) {
			const sys = algebraic.systemList.find(s => s.id === wp.id);
			if (!sys) continue;
			const relX = (wp.x - centerX) * scale;
			const relY = (wp.y - centerY) * scale;
			const sysName = sys.templateId.split('_')[0];
			positions.push({
				id: wp.id,
				x: relX,
				y: relY,
				name: sysName,
				color: getSystemColor(wp.id)
			});
		}

		// Internal wire lines between boxes
		const posMap = new Map(positions.map(p => [p.id, p]));
		const wireLines: Array<{ fromX: number; fromY: number; toX: number; toY: number }> = [];
		for (const wire of algebraic.wireList) {
			if (group.internalWireIds.includes(wire.id)) {
				const fromPos = posMap.get(wire.fromSystem);
				const toPos = posMap.get(wire.toSystem);
				if (fromPos && toPos) {
					wireLines.push({
						fromX: fromPos.x + boxW / 2,
						fromY: fromPos.y,
						toX: toPos.x - boxW / 2,
						toY: toPos.y
					});
				}
			}
		}

		return { positions, wireLines };
	});
</script>

<!-- Skeleton group at z=1 (below header at z=2, bug #1 fix) -->
<T.Group position.y={-HEADER_HEIGHT / 2} position.z={1}>
	<!-- Member boxes at proportional positions -->
	{#each memberLayout.positions as member}
		{@const border = createRoundedRectBorder(boxW, boxH, 1.5,
			new THREE.LineBasicMaterial({ color: 0x4d3d2e, transparent: true, opacity: 0.5 })
		)}
		<T.Group position.x={member.x} position.y={member.y}>
			<T is={border} />
			{@const label = createTextSprite(member.name, member.color)}
			<T is={label} position.z={1} scale={[8, 2.5, 1]} />
		</T.Group>
	{/each}

	<!-- Internal wire connections between boxes -->
	{#each memberLayout.wireLines as wire}
		{@const lineGeom = new THREE.BufferGeometry().setFromPoints([
			new THREE.Vector3(wire.fromX, wire.fromY, 0.5),
			new THREE.Vector3(wire.toX, wire.toY, 0.5)
		])}
		<T.Line geometry={lineGeom}>
			<T.LineBasicMaterial color={0x9a8b78} transparent opacity={0.3} />
		</T.Line>
	{/each}
</T.Group>
