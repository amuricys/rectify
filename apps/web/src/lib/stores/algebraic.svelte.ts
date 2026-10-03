// Reactive state for AlgebraicDynamics (Julia) WebSocket connection
// Handles open dynamical systems with categorical composition via Catlab

export type ConnectionStatus = 'disconnected' | 'connecting' | 'connected' | 'error';

export interface SystemTemplate {
	id: string;
	name: string;
	nstates: number;
	ninputs: number;
	noutputs: number;
	parameters: { name: string; default: number }[];
	state_names: string[];
	input_names: string[];
	output_names: string[];
	input_defaults: number[];
}

export interface SystemState {
	id: string;
	templateId: string;
	state: number[];
	outputs: number[];
	parameters: Record<string, number>;
	position: { x: number; y: number };
	ninputs: number;
	noutputs: number;
	nstates: number;
}

export interface WireState {
	id: string;
	fromSystem: string;
	fromPort: number;
	toSystem: string;
	toPort: number;
	value: number;
}

export interface WorldState {
	type: 'WorldState';
	time: number;
	running: boolean;
	speed: number;
	dt: number;
	systems: SystemState[];
	wires: WireState[];
}

export interface CompositeGroup {
	id: string;
	name: string;
	memberSystemIds: string[];
	internalWireIds: string[];
	position: { x: number; y: number };
	minimized: boolean;
	lookInside: boolean;
}

// Describes a free (unconstrained) state variable in a composite
export interface FreeStateInfo {
	systemId: string;
	stateIndex: number;        // 0-based index within the member system
	globalIndex: number;       // 0-based index in the concatenated state vector
	name: string;              // e.g. "Lorenz.x"
}

// Circular buffer for efficient history management
class CircularBuffer<T> {
	private buffer: T[];
	private head = 0;
	private count = 0;
	private capacity: number;

	constructor(capacity: number) {
		this.capacity = capacity;
		this.buffer = new Array(capacity);
	}

	push(item: T) {
		this.buffer[this.head] = item;
		this.head = (this.head + 1) % this.capacity;
		if (this.count < this.capacity) this.count++;
	}

	toArray(): T[] {
		if (this.count === 0) return [];
		if (this.count < this.capacity) {
			return this.buffer.slice(0, this.count);
		}
		// Buffer is full, need to reorder from oldest to newest
		return [...this.buffer.slice(this.head), ...this.buffer.slice(0, this.head)];
	}

	get length() {
		return this.count;
	}

	clear() {
		this.head = 0;
		this.count = 0;
	}
}

// Simple reactive store using a plain object
function createAlgebraicStore() {
	let status = $state<ConnectionStatus>('disconnected');
	let error = $state<string | null>(null);
	let errorCode = $state<string | null>(null);
	let recoverAction = $state<string | null>(null);
	let time = $state(0);
	let running = $state(false);
	let speed = $state(1);
	let dt = $state(0.001);
	let systemList = $state<SystemState[]>([]);
	let wireList = $state<WireState[]>([]);
	let templateList = $state<SystemTemplate[]>([]);

	// Use circular buffers for history - much more efficient than array spread/slice
	const historyBuffers = new Map<string, CircularBuffer<number[]>>();
	// Time buffers parallel to history — real simulation timestamps
	const timeBuffers = new Map<string, CircularBuffer<number>>();
	// Track version to trigger reactivity when history updates
	let historyVersion = $state(0);

	// Composite groups: connected components of wired systems
	let compositeGroups = $state<CompositeGroup[]>([]);

	const historyLength = 2000;
	let ws: WebSocket | null = null;
	let reconnectTimer: ReturnType<typeof setTimeout> | null = null;
	const url = 'ws://localhost:8082';

	function send(msg: object) {
		if (ws?.readyState === WebSocket.OPEN) {
			ws.send(JSON.stringify(msg));
		}
	}

	function recomputeComposites() {
		// Union-find on systems connected by wires
		const parent = new Map<string, string>();
		const find = (x: string): string => {
			if (!parent.has(x)) parent.set(x, x);
			let root = x;
			while (parent.get(root) !== root) root = parent.get(root)!;
			// Path compression
			let cur = x;
			while (cur !== root) {
				const next = parent.get(cur)!;
				parent.set(cur, root);
				cur = next;
			}
			return root;
		};
		const union = (a: string, b: string) => {
			const ra = find(a);
			const rb = find(b);
			if (ra !== rb) parent.set(ra, rb);
		};

		// Initialize all systems
		for (const sys of systemList) {
			find(sys.id);
		}
		// Union systems connected by wires
		for (const wire of wireList) {
			union(wire.fromSystem, wire.toSystem);
		}

		// Group by root
		const components = new Map<string, string[]>();
		for (const sys of systemList) {
			const root = find(sys.id);
			if (!components.has(root)) components.set(root, []);
			components.get(root)!.push(sys.id);
		}

		// Build new composite groups (only for components with 2+ members)
		const oldGroupMap = new Map<string, CompositeGroup>();
		for (const g of compositeGroups) {
			// Key by sorted member IDs to match against
			const key = [...g.memberSystemIds].sort().join(',');
			oldGroupMap.set(key, g);
		}

		const newGroups: CompositeGroup[] = [];
		for (const [, members] of components) {
			if (members.length < 2) continue;
			const sorted = [...members].sort();
			const key = sorted.join(',');

			// Find internal wires
			const memberSet = new Set(sorted);
			const internalWires = wireList
				.filter(w => memberSet.has(w.fromSystem) && memberSet.has(w.toSystem))
				.map(w => w.id);

			// Check for existing group with same or overlapping membership
			// Build name from template names (always recompute to reflect membership)
			const name = sorted.map(id => {
				const sys = systemList.find(s => s.id === id);
				return sys ? sys.templateId.split('_')[0] : id;
			}).join(' \u2297 ');

			const existing = oldGroupMap.get(key);
			if (existing) {
				// Same membership — keep position, minimized, lookInside; update name & wires
				newGroups.push({
					...existing,
					name,
					memberSystemIds: sorted,
					internalWireIds: internalWires
				});
				oldGroupMap.delete(key);
			} else {
				// Try to find a group that overlaps (expanded/shrunk)
				let found: CompositeGroup | null = null;
				for (const [oldKey, oldGroup] of oldGroupMap) {
					const oldMembers = new Set(oldGroup.memberSystemIds);
					const overlap = sorted.filter(id => oldMembers.has(id));
					if (overlap.length > 0) {
						found = oldGroup;
						oldGroupMap.delete(oldKey);
						break;
					}
				}

				// Compute centroid from system positions
				let cx = 0, cy = 0;
				for (const id of sorted) {
					const sys = systemList.find(s => s.id === id);
					if (sys) { cx += sys.position.x; cy += sys.position.y; }
				}
				cx /= sorted.length;
				cy /= sorted.length;

				newGroups.push({
					id: found?.id ?? `composite_${Date.now()}_${Math.random().toString(36).slice(2, 6)}`,
					name,
					memberSystemIds: sorted,
					internalWireIds: internalWires,
					position: found?.position ?? { x: cx, y: cy },
					minimized: found?.minimized ?? false,
					lookInside: found?.lookInside ?? false
				});
			}
		}

		compositeGroups = newGroups;
	}

	function handleMessage(data: any) {
		console.log('WS message:', data.type);

		switch (data.type) {
			case 'Templates':
				templateList = [...data.templates];
				console.log('Templates updated:', templateList.length);
				break;

			case 'WorldState':
				time = data.time;
				running = data.running;
				speed = data.speed;
				dt = typeof data.dt === 'number' ? data.dt : dt;
				systemList = [...data.systems];
				wireList = [...data.wires];

				// Initialize history buffers for new systems
				const currentIds = new Set(data.systems.map((s: SystemState) => s.id));
				for (const sys of data.systems) {
					if (!historyBuffers.has(sys.id)) {
						const buffer = new CircularBuffer<number[]>(historyLength);
						buffer.push([...sys.state]);
						historyBuffers.set(sys.id, buffer);
						const tbuf = new CircularBuffer<number>(historyLength);
						tbuf.push(data.time);
						timeBuffers.set(sys.id, tbuf);
					}
				}
				// Clean up removed systems
				for (const id of historyBuffers.keys()) {
					if (!currentIds.has(id)) {
						historyBuffers.delete(id);
						timeBuffers.delete(id);
					}
				}
				historyVersion++;
				recomputeComposites();
				console.log('WorldState updated:', systemList.length, 'systems');
				break;

			case 'StateUpdate':
				time = data.time;

				// Update system states in place where possible
				for (let i = 0; i < systemList.length; i++) {
					const sys = systemList[i];
					const newState = data.states[sys.id];
					if (newState) {
						// Only create new object if state actually changed
						systemList[i] = { ...sys, state: newState, outputs: data.outputs[sys.id] || sys.outputs };
					}
				}
				// Trigger reactivity
				systemList = systemList;

				// Update history using circular buffers (no allocation per frame)
				for (const [id, state] of Object.entries(data.states) as [string, number[]][]) {
					const buffer = historyBuffers.get(id);
					if (buffer) {
						buffer.push([...state]);
					}
					const tbuf = timeBuffers.get(id);
					if (tbuf) {
						tbuf.push(data.time);
					}
				}
				historyVersion++;

				// Update wire values in place
				for (let i = 0; i < wireList.length; i++) {
					const wire = wireList[i];
					const value = data.wires[wire.id];
					if (value !== undefined && wire.value !== value) {
						wireList[i] = { ...wire, value };
					}
				}
				wireList = wireList;
				break;

			case 'Ack':
				errorCode = null;
				recoverAction = null;
				if (!data.success) {
					console.error('Command failed:', data);
				}
				recomputeComposites();
				break;

			case 'Error':
				console.error('Server error:', data.code, data.message);
				errorCode = data.code || null;
				recoverAction = data.recoverAction || null;
				if (data.code === 'NON_FINITE_STATE') {
					const stage = data.stage ? ` stage=${data.stage}` : '';
					const systemId = data.systemId ? ` system=${data.systemId}` : '';
					error = `${data.message}${stage}${systemId}`;
				} else {
					error = data.message;
				}
				break;
		}
	}

	function scheduleReconnect() {
		if (reconnectTimer) return;
		reconnectTimer = setTimeout(() => {
			reconnectTimer = null;
			if (status === 'disconnected') {
				connect();
			}
		}, 2000);
	}

	function connect() {
		if (ws?.readyState === WebSocket.OPEN) return;

		status = 'connecting';
		error = null;
		errorCode = null;
		recoverAction = null;

		try {
			ws = new WebSocket(url);

			ws.onopen = () => {
				status = 'connected';
				console.log('Connected to AlgebraicDynamics server');
			};

			ws.onmessage = (event) => {
				try {
					const data = JSON.parse(event.data);
					handleMessage(data);
				} catch (e) {
					console.warn('Failed to parse message:', event.data, e);
				}
			};

			ws.onclose = () => {
				status = 'disconnected';
				ws = null;
				scheduleReconnect();
			};

			ws.onerror = () => {
				status = 'error';
				error = 'WebSocket connection failed';
			};
		} catch (e) {
			status = 'error';
			error = e instanceof Error ? e.message : 'Unknown error';
		}
	}

	function disconnect() {
		if (reconnectTimer) {
			clearTimeout(reconnectTimer);
			reconnectTimer = null;
		}
		ws?.close();
		ws = null;
		status = 'disconnected';
	}

	return {
		// Getters for reactive state
		get status() { return status; },
		get error() { return error; },
		get errorCode() { return errorCode; },
		get recoverAction() { return recoverAction; },
		get time() { return time; },
		get running() { return running; },
		get speed() { return speed; },
		get dt() { return dt; },
		get systemList() { return systemList; },
		get wireList() { return wireList; },
		get templateList() { return templateList; },

		// For compatibility with Map-based access
		get systems() { return new Map(systemList.map(s => [s.id, s])); },
		get wires() { return new Map(wireList.map(w => [w.id, w])); },

		// Composite groups
		get compositeGroups() { return compositeGroups; },

		getCompositeForSystem(systemId: string): CompositeGroup | null {
			return compositeGroups.find(g => g.memberSystemIds.includes(systemId)) ?? null;
		},

		// Get info about free (unconstrained) states in a composite.
		// A state is constrained if an internal wire targets its state input port (port <= nstates).
		getCompositeFreeStateInfo(compositeId: string): FreeStateInfo[] {
			const group = compositeGroups.find(g => g.id === compositeId);
			if (!group) return [];

			// Build set of constrained (systemId, stateIndex 0-based) pairs
			const constrained = new Set<string>();
			const memberSet = new Set(group.memberSystemIds);
			for (const wire of wireList) {
				if (memberSet.has(wire.fromSystem) && memberSet.has(wire.toSystem)) {
					const targetSys = systemList.find(s => s.id === wire.toSystem);
					if (targetSys && wire.toPort <= targetSys.nstates) {
						constrained.add(`${wire.toSystem}:${wire.toPort - 1}`); // 0-based
					}
				}
			}

			const result: FreeStateInfo[] = [];
			let globalIdx = 0;
			for (const memberId of group.memberSystemIds) {
				const sys = systemList.find(s => s.id === memberId);
				if (!sys) continue;
				const tmpl = templateList.find(t => t.id === sys.templateId);
				const sysName = sys.templateId.split('_')[0];
				const stateNames = tmpl?.state_names ?? [];
				for (let i = 0; i < sys.nstates; i++) {
					if (!constrained.has(`${memberId}:${i}`)) {
						result.push({
							systemId: memberId,
							stateIndex: i,
							globalIndex: globalIdx,
							name: `${sysName}.${stateNames[i] || `v${i}`}`
						});
					}
					globalIdx++;
				}
			}
			return result;
		},

		// Returns history with only free (unconstrained) state columns
		getCompositeHistory(compositeId: string): number[][] {
			const _ = historyVersion;
			const group = compositeGroups.find(g => g.id === compositeId);
			if (!group) return [];

			const freeStates = this.getCompositeFreeStateInfo(compositeId);
			if (freeStates.length === 0) return [];

			// Get the shortest history length among members
			let minLen = Infinity;
			const memberHistories = new Map<string, number[][]>();
			for (const memberId of group.memberSystemIds) {
				const buf = historyBuffers.get(memberId);
				const hist = buf ? buf.toArray() : [];
				memberHistories.set(memberId, hist);
				minLen = Math.min(minLen, hist.length);
			}
			if (minLen === 0 || !Number.isFinite(minLen)) return [];

			// Build combined array with only free states
			const combined: number[][] = [];
			for (let t = 0; t < minLen; t++) {
				const row: number[] = [];
				for (const fs of freeStates) {
					const hist = memberHistories.get(fs.systemId);
					if (hist) {
						row.push(hist[t][fs.stateIndex] ?? 0);
					}
				}
				combined.push(row);
			}
			return combined;
		},

		getCompositeTimeHistory(compositeId: string): number[] {
			const _ = historyVersion;
			const group = compositeGroups.find(g => g.id === compositeId);
			if (!group || group.memberSystemIds.length === 0) return [];
			const buf = timeBuffers.get(group.memberSystemIds[0]);
			return buf ? buf.toArray() : [];
		},

		isInternalWire(wireId: string): boolean {
			return compositeGroups.some(g => g.internalWireIds.includes(wireId));
		},

		getHistory(systemId: string): number[][] {
			// Read historyVersion to create reactive dependency
			const _ = historyVersion;
			const buffer = historyBuffers.get(systemId);
			return buffer ? buffer.toArray() : [];
		},

		getTimeHistory(systemId: string): number[] {
			const _ = historyVersion;
			const buffer = timeBuffers.get(systemId);
			return buffer ? buffer.toArray() : [];
		},

		// Actions
		connect,
		disconnect,

		addSystem(
			templateId: string,
			position: { x: number; y: number } = { x: 0, y: 0 },
			parameters?: Record<string, number>,
			initialState?: number[]
		): string {
			const instanceId = `${templateId}_${Date.now()}_${Math.random().toString(36).slice(2, 6)}`;
			send({
				type: 'AddSystem',
				instanceId,
				templateId,
				position,
				parameters: parameters || {},
				initialState
			});
			return instanceId;
		},

		removeSystem(instanceId: string) {
			send({ type: 'RemoveSystem', instanceId });
		},

		setParams(instanceId: string, parameters: Record<string, number>) {
			send({ type: 'SetParams', instanceId, parameters });
		},

		setState(instanceId: string, state: number[]) {
			send({ type: 'SetState', instanceId, state });
		},

		wire(fromSystem: string, fromPort: number, toSystem: string, toPort: number): string {
			const wireId = `wire_${fromSystem}_${fromPort}_${toSystem}_${toPort}`;
			send({ type: 'Wire', wireId, fromSystem, fromPort, toSystem, toPort });
			return wireId;
		},

		unwire(wireId: string) {
			send({ type: 'Unwire', wireId });
		},

		play() {
			send({ type: 'Control', action: 'play' });
		},

		pause() {
			send({ type: 'Control', action: 'pause' });
		},

		toggle() {
			if (running) {
				send({ type: 'Control', action: 'pause' });
			} else {
				send({ type: 'Control', action: 'play' });
			}
		},

		step() {
			send({ type: 'Control', action: 'step' });
		},

		reset() {
			for (const buffer of historyBuffers.values()) {
				buffer.clear();
			}
			for (const buffer of timeBuffers.values()) {
				buffer.clear();
			}
			historyVersion++;
			send({ type: 'Control', action: 'reset' });
		},

		setSpeed(newSpeed: number) {
			speed = newSpeed;
			send({ type: 'Control', action: 'setSpeed', speed: newSpeed });
		},

		setDt(newDt: number) {
			dt = newDt;
			send({ type: 'Control', action: 'setDt', dt: newDt });
		},

		resumeFinite() {
			send({ type: 'Control', action: 'resumeFinite' });
		},

		defineCustomSystem(parsed: {
			name: string;
			stateVars: string[];
			equations: string[];
			parameters: { name: string; default: number }[];
			inputs: string[];
			initialState: number[];
		}) {
			send({
				type: 'DefineCustomSystem',
				name: parsed.name,
				stateVars: parsed.stateVars,
				equations: parsed.equations,
				parameters: parsed.parameters,
				inputs: parsed.inputs,
				initialState: parsed.initialState
			});
		}
	};
}

export const algebraic = createAlgebraicStore();
