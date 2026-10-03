/**
 * Shared position store for system windows.
 * Positions are initialized from the backend but managed locally by the renderer.
 * Wire routing and composite bounds read from this map.
 */
const positions = new Map<string, { x: number; y: number }>();

// Window dimensions per system (for port position calculations)
const windowDims = new Map<string, { width: number; height: number }>();

export function getPosition(systemId: string): { x: number; y: number } {
	return positions.get(systemId) ?? { x: 0, y: 0 };
}

export function setPosition(systemId: string, x: number, y: number) {
	positions.set(systemId, { x, y });
}

export function getWindowDims(systemId: string): { width: number; height: number } {
	return windowDims.get(systemId) ?? { width: 46, height: 46 };
}

export function setWindowDims(systemId: string, width: number, height: number) {
	windowDims.set(systemId, { width, height });
}

export function removePosition(systemId: string) {
	positions.delete(systemId);
	windowDims.delete(systemId);
}
