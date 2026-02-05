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
	systems: SystemState[];
	wires: WireState[];
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
	let time = $state(0);
	let running = $state(false);
	let speed = $state(1);
	let systemList = $state<SystemState[]>([]);
	let wireList = $state<WireState[]>([]);
	let templateList = $state<SystemTemplate[]>([]);

	// Use circular buffers for history - much more efficient than array spread/slice
	const historyBuffers = new Map<string, CircularBuffer<number[]>>();
	// Time buffers parallel to history — real simulation timestamps
	const timeBuffers = new Map<string, CircularBuffer<number>>();
	// Track version to trigger reactivity when history updates
	let historyVersion = $state(0);

	const historyLength = 2000;
	let ws: WebSocket | null = null;
	let reconnectTimer: ReturnType<typeof setTimeout> | null = null;
	const url = 'ws://localhost:8082';

	function send(msg: object) {
		if (ws?.readyState === WebSocket.OPEN) {
			ws.send(JSON.stringify(msg));
		}
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
				if (!data.success) {
					console.error('Command failed:', data);
				}
				break;

			case 'Error':
				console.error('Server error:', data.code, data.message);
				error = data.message;
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
		get time() { return time; },
		get running() { return running; },
		get speed() { return speed; },
		get systemList() { return systemList; },
		get wireList() { return wireList; },
		get templateList() { return templateList; },

		// For compatibility with Map-based access
		get systems() { return new Map(systemList.map(s => [s.id, s])); },
		get wires() { return new Map(wireList.map(w => [w.id, w])); },

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
