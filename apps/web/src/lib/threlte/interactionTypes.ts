import * as THREE from 'three';
import type { WireState } from '$lib/stores/algebraic.svelte';

export interface PortDotInfo {
	systemId: string;
	portIndex: number;
	isOutput: boolean;
}

export interface WireDragState {
	active: boolean;
	sourcePort: PortDotInfo | null;
	pendingRewire: WireState | null;
	cursorWorld: THREE.Vector2;
}
