import * as THREE from 'three';

/**
 * A* wire pathfinding around rectangular obstacles.
 * Returns a simplified polyline of waypoints at z=3.
 */
export function computeWireRoute(
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

	// Fallback: simple Z-route
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
