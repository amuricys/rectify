// Reactive state for Lean optimization backend

export type ConnectionStatus = 'disconnected' | 'connecting' | 'connected' | 'error';

export interface Point2D {
	x: number;
	y: number;
}

export interface TSPSolution {
	tag: 'TSPSolution';
	cities: Point2D[];
}

export interface SAState {
	current: TSPSolution;
	currentFitness: number;
	best: TSPSolution;
	bestFitness: number;
	temperature: number;
	stepCount: number;
	algorithm: string;
}

class OptimizationConnection {
	status = $state<ConnectionStatus>('disconnected');
	state = $state<SAState | null>(null);
	error = $state<string | null>(null);
	running = $state(false);
	seed = $state(42);

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
				this.running = false;
				console.log('Connected to optimization server');
				// Initialize with seed
				this.send(`Reset ${this.seed}`);
			};

			this.ws.onmessage = (event) => {
				console.log('Raw message:', event.data);
				try {
					const data = JSON.parse(event.data);
					console.log('Parsed:', data);
					if (data.current && data.bestFitness !== undefined) {
						console.log('Setting state');
						this.state = data as SAState;
					} else {
						console.log('Data missing fields. Has current:', !!data.current, 'Has bestFitness:', data.bestFitness);
					}
				} catch (e) {
					console.warn('Failed to parse message:', event.data, e);
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

	private send(msg: string) {
		if (this.ws?.readyState === WebSocket.OPEN) {
			this.ws.send(msg);
		}
	}

	play() {
		this.send('Play');
		this.running = true;
	}

	pause() {
		this.send('Pause');
		this.running = false;
	}

	toggle() {
		if (this.running) {
			this.pause();
		} else {
			this.play();
		}
	}

	step() { this.send('Step'); }

	reset() {
		this.send(`Reset ${this.seed}`);
		this.running = false;
	}

	setSeed(seed: number) {
		this.seed = seed;
	}
}

export const optimization = new OptimizationConnection();
