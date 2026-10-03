import * as THREE from 'three';
import { COLORS } from '../constants';

/** Module-level color assignment for systems: each system gets a stable color. */
const systemColorMap = new Map<string, THREE.Color>();
let colorIndex = 0;

export function getSystemColor(systemId: string): THREE.Color {
	let color = systemColorMap.get(systemId);
	if (!color) {
		color = COLORS[colorIndex % COLORS.length].clone();
		colorIndex++;
		systemColorMap.set(systemId, color);
	}
	return color;
}

export function clearSystemColor(systemId: string) {
	systemColorMap.delete(systemId);
}

export function resetColors() {
	systemColorMap.clear();
	colorIndex = 0;
}
