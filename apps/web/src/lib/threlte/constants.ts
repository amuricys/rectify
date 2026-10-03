import * as THREE from 'three';

// Window dimensions (in world units)
export const WINDOW_WIDTH = 40;
export const WINDOW_HEIGHT = 40;
export const HEADER_HEIGHT = 4;
export const CORNER_RADIUS = 2.5;
export const CANVAS_FONT = "'CMU Serif', serif";

export const DEFAULT_WINDOW_WIDTH = 46;
export const DEFAULT_WINDOW_HEIGHT = 46;
export const MIN_WINDOW_SIZE = 25;

export const TS_MARGIN = { left: 10, right: 12, bottom: 8, top: 4 };
export const NUM_TICKS = 5;
export const PHASE_HALF_EXTENT = 14;
export const PHASE_BOUNDS_HISTORY = 500;
export const MAX_POINTS = 2000;
export const TIME_WINDOW_SAMPLES = 500;

export const WIRE_DRAG_MAX_VERTS = 200;

// Colors for individual state variables in time series view
export const STATE_COLORS = [
	new THREE.Color(0xd4785a), // warm-red
	new THREE.Color(0xc9a84c), // gold
	new THREE.Color(0x8a9b68), // sage
	new THREE.Color(0xd4956b), // copper
	new THREE.Color(0xb85c4a), // brick
	new THREE.Color(0xc4b078), // pale-gold
];

export const STATE_NAMES = ['x', 'y', 'z', 'w', 'v', 'u'];

// Color palette for systems
export const COLORS = [
	new THREE.Color(0xc9a84c), // gold
	new THREE.Color(0xb85c4a), // brick
	new THREE.Color(0x8a9b68), // sage
	new THREE.Color(0xd4956b), // copper
	new THREE.Color(0x9b7a5c), // leather
	new THREE.Color(0xc4b078), // pale-gold
	new THREE.Color(0xa86e5a), // amber
	new THREE.Color(0x7a9b8a), // warm-teal
];

// View angle presets for 3D phase view
export const VIEW_PRESETS: Record<ViewAngle, { rx: number; ry: number }> = {
	'ISO': { rx: -0.4, ry: 0.3 },
	'XY': { rx: 0, ry: 0 },
	'XZ': { rx: -Math.PI / 2, ry: 0 },
	'YZ': { rx: 0, ry: -Math.PI / 2 },
};

export const VIEW_CYCLE: ViewAngle[] = ['ISO', 'XY', 'XZ', 'YZ'];

// Re-export types used by VIEW_PRESETS
import type { ViewAngle } from './types';
