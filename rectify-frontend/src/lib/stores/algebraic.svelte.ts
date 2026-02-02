// Reactive state for AlgebraicDynamics (Julia) WebSocket connection

export type ConnectionStatus = 'disconnected' | 'connecting' | 'connected' | 'error';

export interface SystemState {
	kind: string;
	state: number[];
	output: number[];
	ninputs: number;
	noutputs: number;
}

export interface WireInfo {
	from_system: string;
	from_port: number;
	to_system: string;
	to_port: number;
}

export interface WorldState {
	t: number;
	running: boolean;
	systems: Record<string, SystemState>;
	wires: WireInfo[];
}

class AlgebraicConnection {
	status = $state<ConnectionStatus>('disconnected');
	world = $state<WorldState | null>(null);
	error = $state<string | null>(null);

	private ws: WebSocket | null = null;
	private url: string;

	constructor(url: string = 'ws://localhost:8082') {
		this.url = url;
	}

	connect() {
		if (this.ws?.readyState === WebSocket.OPEN) return;

		this.status = 'connecting';
		this.error = null;

		try {
			this.ws = new WebSocket(this.url);

			this.ws.onopen = () => {
				this.status = 'connected';
				console.log('Connected to AlgebraicDynamics server');
			};

			this.ws.onmessage = (event) => {
				try {
					const data = JSON.parse(event.data);
					if (data.type === 'state') {
						this.world = data as WorldState;
					}
				} catch (e) {
					console.warn('Failed to parse message:', event.data);
				}
			};

			this.ws.onclose = () => {
				this.status = 'disconnected';
				this.ws = null;
			};

			this.ws.onerror = () => {
				this.status = 'error';
				this.error = 'WebSocket connection failed';
			};
		} catch (e) {
			this.status = 'error';
			this.error = e instanceof Error ? e.message : 'Unknown error';
		}
	}

	disconnect() {
		this.ws?.close();
		this.ws = null;
		this.status = 'disconnected';
	}

	private send(msg: object) {
		if (this.ws?.readyState === WebSocket.OPEN) {
			this.ws.send(JSON.stringify(msg));
		}
	}

	// Playback control
	play() { this.send({ type: 'control', action: 'play' }); }
	pause() { this.send({ type: 'control', action: 'pause' }); }
	step() { this.send({ type: 'control', action: 'step' }); }
	reset() { this.send({ type: 'control', action: 'reset' }); }

	// System management
	addSystem(id: string, kind: string, params: Record<string, number> = {}) {
		this.send({ type: 'add_system', id, kind, params });
	}

	removeSystem(id: string) {
		this.send({ type: 'remove_system', id });
	}

	// Wiring
	wire(fromSystem: string, fromPort: number, toSystem: string, toPort: number) {
		this.send({
			type: 'wire',
			from_system: fromSystem,
			from_port: fromPort,
			to_system: toSystem,
			to_port: toPort
		});
	}

	unwire(fromSystem: string, fromPort: number, toSystem: string, toPort: number) {
		this.send({
			type: 'unwire',
			from_system: fromSystem,
			from_port: fromPort,
			to_system: toSystem,
			to_port: toPort
		});
	}
}

export const algebraic = new AlgebraicConnection();
