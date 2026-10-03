import * as THREE from 'three';

/**
 * Fillet-based corner rounding for wire paths.
 * Replaces CatmullRom which overshoots at 90-degree turns (bug #4).
 *
 * For each interior waypoint (a 90-degree turn from the A* pathfinder):
 *   1. Find point `filletRadius` before the corner along the incoming segment
 *   2. Find point `filletRadius` after the corner along the outgoing segment
 *   3. Interpolate a quadratic Bezier arc: P0=before, P1=corner, P2=after
 * This produces tight, controlled rounding without overshoot.
 */
export function smoothCorners(waypoints: THREE.Vector3[], filletRadius = 3): THREE.Vector3[] {
	if (waypoints.length <= 2) return waypoints;

	const result: THREE.Vector3[] = [waypoints[0]];
	const arcSamples = 6; // samples per fillet arc

	for (let i = 1; i < waypoints.length - 1; i++) {
		const prev = waypoints[i - 1];
		const curr = waypoints[i];
		const next = waypoints[i + 1];

		// Compute incoming and outgoing segment directions
		const inDir = new THREE.Vector3().subVectors(curr, prev);
		const outDir = new THREE.Vector3().subVectors(next, curr);
		const inLen = inDir.length();
		const outLen = outDir.length();

		// Clamp fillet radius to not exceed half of either segment
		const maxR = Math.min(inLen / 2, outLen / 2, filletRadius);

		if (maxR < 0.1) {
			// Segments too short for fillet, just include the corner point
			result.push(curr.clone());
			continue;
		}

		// P0: point filletRadius back along incoming segment
		const inNorm = inDir.clone().normalize();
		const p0 = curr.clone().sub(inNorm.clone().multiplyScalar(maxR));

		// P2: point filletRadius forward along outgoing segment
		const outNorm = outDir.clone().normalize();
		const p2 = curr.clone().add(outNorm.clone().multiplyScalar(maxR));

		// Quadratic Bezier: B(t) = (1-t)^2 * P0 + 2*(1-t)*t * P1 + t^2 * P2
		// P1 = corner point (curr)
		result.push(p0);
		for (let s = 1; s < arcSamples; s++) {
			const t = s / arcSamples;
			const u = 1 - t;
			const pt = new THREE.Vector3(
				u * u * p0.x + 2 * u * t * curr.x + t * t * p2.x,
				u * u * p0.y + 2 * u * t * curr.y + t * t * p2.y,
				u * u * p0.z + 2 * u * t * curr.z + t * t * p2.z
			);
			result.push(pt);
		}
		result.push(p2);
	}

	result.push(waypoints[waypoints.length - 1]);
	return result;
}
