// Reactive state for dynamics WebSocket connection

export type ConnectionStatus = 'disconnected' | 'connecting' | 'connected' | 'error';

export interface DynamicsState {
	x: number;
	y: number;
	z: number;
}

class DynamicsConnection {
	status = $state<ConnectionStatus>('disconnected');
	currentState = $state<DynamicsState | null>(null);
	error = $state<string | null>(null);

	private ws: WebSocket | null = null;
	private url: string;

	constructor(url: string = 'ws://localhost:8081') {
		this.url = url;
	}

	connect() {
		if (this.ws?.readyState === WebSocket.OPEN) return;

		this.status = 'connecting';
		this.error = null;

		try {
			this.ws = new WebSocket(this.url, 'dynamics-protocol');

			this.ws.onopen = () => {
				this.status = 'connected';
				console.log('Connected to dynamics server');
			};

			this.ws.onmessage = (event) => {
				try {
					const data = JSON.parse(event.data);
					this.currentState = data;
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

	send(message: string) {
		if (this.ws?.readyState === WebSocket.OPEN) {
			this.ws.send(message);
		}
	}

	pause() { this.send('Pause'); }
	unpause() { this.send('Unpause'); }
	step() { this.send('Step'); }

	selectSystem(system: 'HarmonicOscillator' | 'LorenzSystem' | 'DuffingOscillator' | 'VanDerPolOscillator') {
		this.send(system);
	}
}

export const dynamics = new DynamicsConnection();
