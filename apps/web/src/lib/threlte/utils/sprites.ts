import * as THREE from 'three';
import { CANVAS_FONT, NUM_TICKS } from '../constants';

export function createTextSprite(text: string, color: THREE.Color): THREE.Sprite {
	const canvas = document.createElement('canvas');
	const context = canvas.getContext('2d')!;
	canvas.width = 256;
	canvas.height = 64;

	context.fillStyle = 'transparent';
	context.fillRect(0, 0, canvas.width, canvas.height);

	context.font = `700 28px ${CANVAS_FONT}`;
	context.fillStyle = `rgb(${Math.floor(color.r * 255)}, ${Math.floor(color.g * 255)}, ${Math.floor(color.b * 255)})`;
	context.textAlign = 'left';
	context.textBaseline = 'middle';
	context.fillText(text, 10, canvas.height / 2);

	const texture = new THREE.CanvasTexture(canvas);
	texture.needsUpdate = true;

	const material = new THREE.SpriteMaterial({
		map: texture,
		transparent: true
	});

	const sprite = new THREE.Sprite(material);
	sprite.scale.set(20, 5, 1);

	return sprite;
}

export function createButtonSprite(text: string, active: boolean): THREE.Sprite {
	const canvas = document.createElement('canvas');
	const context = canvas.getContext('2d')!;
	canvas.width = 96;
	canvas.height = 40;

	context.fillStyle = active ? '#8a7435' : '#2a2118';
	context.roundRect(0, 0, canvas.width, canvas.height, 6);
	context.fill();

	if (active) {
		context.strokeStyle = '#c9a84c';
		context.lineWidth = 2;
		context.stroke();
	}

	context.font = `700 18px ${CANVAS_FONT}`;
	context.fillStyle = active ? '#ffffff' : '#9a8b78';
	context.textAlign = 'center';
	context.textBaseline = 'middle';
	context.fillText(text, canvas.width / 2, canvas.height / 2);

	const texture = new THREE.CanvasTexture(canvas);
	const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
	const sprite = new THREE.Sprite(material);
	sprite.scale.set(8, 3.5, 1);
	return sprite;
}

export function createLegendSprite(name: string, color: THREE.Color): THREE.Sprite {
	const canvas = document.createElement('canvas');
	const context = canvas.getContext('2d')!;
	canvas.width = 64;
	canvas.height = 32;

	context.fillStyle = `rgb(${Math.floor(color.r * 255)}, ${Math.floor(color.g * 255)}, ${Math.floor(color.b * 255)})`;
	context.fillRect(4, 10, 12, 12);

	context.font = `italic 16px ${CANVAS_FONT}`;
	context.fillStyle = '#9a8b78';
	context.textAlign = 'left';
	context.textBaseline = 'middle';
	context.fillText(name, 20, 16);

	const texture = new THREE.CanvasTexture(canvas);
	const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
	const sprite = new THREE.Sprite(material);
	sprite.scale.set(6, 3, 1);
	return sprite;
}

export function createTooltipSprite(text: string, color: string): THREE.Sprite {
	const canvas = document.createElement('canvas');
	const ctx = canvas.getContext('2d')!;
	canvas.width = 256;
	canvas.height = 32;
	ctx.font = `400 14px ${CANVAS_FONT}`;
	ctx.fillStyle = color;
	ctx.textAlign = 'left';
	ctx.textBaseline = 'middle';
	ctx.fillText(text, 4, canvas.height / 2);
	const texture = new THREE.CanvasTexture(canvas);
	const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
	const sprite = new THREE.Sprite(material);
	sprite.scale.set(22, 3, 1);
	return sprite;
}

export function createTickSprite(canvasW: number, canvasH: number): THREE.Sprite {
	const canvas = document.createElement('canvas');
	canvas.width = canvasW;
	canvas.height = canvasH;
	const texture = new THREE.CanvasTexture(canvas);
	const material = new THREE.SpriteMaterial({ map: texture, transparent: true, depthTest: false });
	return new THREE.Sprite(material);
}

export function createMinimizedValueSprite(text: string): THREE.Sprite {
	const canvas = document.createElement('canvas');
	const ctx = canvas.getContext('2d')!;
	canvas.width = 256;
	canvas.height = 32;
	ctx.font = `italic 16px ${CANVAS_FONT}`;
	ctx.fillStyle = '#9a8b78';
	ctx.textAlign = 'left';
	ctx.textBaseline = 'middle';
	ctx.fillText(text, 4, canvas.height / 2);
	const texture = new THREE.CanvasTexture(canvas);
	const material = new THREE.SpriteMaterial({ map: texture, transparent: true });
	const sprite = new THREE.Sprite(material);
	sprite.scale.set(16, 3, 1);
	return sprite;
}

export function updateMinimizedValueSprite(sprite: THREE.Sprite, text: string) {
	const mat = sprite.material as THREE.SpriteMaterial;
	const texture = mat.map!;
	const canvas = texture.image as HTMLCanvasElement;
	const ctx = canvas.getContext('2d')!;
	ctx.clearRect(0, 0, canvas.width, canvas.height);
	ctx.font = `italic 16px ${CANVAS_FONT}`;
	ctx.fillStyle = '#9a8b78';
	ctx.textAlign = 'left';
	ctx.textBaseline = 'middle';
	ctx.fillText(text, 4, canvas.height / 2);
	texture.needsUpdate = true;
}

export function formatTickValue(value: number): string {
	if (value === 0) return '0';
	const abs = Math.abs(value);
	if (abs >= 1000) return value.toExponential(1);
	if (abs >= 100) return value.toFixed(0);
	if (abs >= 10) return value.toFixed(1);
	if (abs >= 1) return value.toFixed(1);
	if (abs >= 0.01) return value.toFixed(2);
	return value.toExponential(1);
}

export function updateYTickSprite(
	sprite: THREE.Sprite,
	minVal: number,
	maxVal: number,
	winW: number,
	winH: number,
	tsMargin: { left: number; right: number; bottom: number; top: number }
) {
	const material = sprite.material as THREE.SpriteMaterial;
	const texture = material.map!;
	const canvas = texture.image as HTMLCanvasElement;
	const ctx = canvas.getContext('2d')!;
	ctx.clearRect(0, 0, canvas.width, canvas.height);

	ctx.font = `400 20px ${CANVAS_FONT}`;
	ctx.fillStyle = '#9a8b78';
	ctx.textAlign = 'right';

	const pad = 16;
	for (let i = 0; i <= NUM_TICKS; i++) {
		const t = i / NUM_TICKS;
		const y = canvas.height - pad - t * (canvas.height - 2 * pad);
		const value = minVal + t * (maxVal - minVal);

		ctx.fillRect(canvas.width - 6, y - 1, 6, 2);
		ctx.textBaseline = 'middle';
		ctx.fillText(formatTickValue(value), canvas.width - 10, y);
	}

	texture.needsUpdate = true;
	const yAxisHeight = winH - tsMargin.bottom - tsMargin.top;
	const yFixedScale = 40;
	const yScale = Math.min(yFixedScale, yAxisHeight);
	sprite.scale.set(yScale * (128 / 512), yScale, 1);
	sprite.position.set(-winW / 2 + tsMargin.left / 2,
		(tsMargin.bottom - tsMargin.top) / 2, 2);
}

export function updateXTickSprite(
	sprite: THREE.Sprite,
	startTime: number,
	endTime: number,
	winW: number,
	winH: number,
	tsMargin: { left: number; right: number; bottom: number; top: number }
) {
	const material = sprite.material as THREE.SpriteMaterial;
	const texture = material.map!;
	const canvas = texture.image as HTMLCanvasElement;
	const ctx = canvas.getContext('2d')!;
	ctx.clearRect(0, 0, canvas.width, canvas.height);

	ctx.font = `400 16px ${CANVAS_FONT}`;
	ctx.fillStyle = '#9a8b78';
	ctx.textAlign = 'center';

	const pad = 12;
	for (let i = 0; i <= NUM_TICKS; i++) {
		const t = i / NUM_TICKS;
		const x = pad + t * (canvas.width - 2 * pad);
		const timeSec = startTime + t * (endTime - startTime);

		ctx.fillRect(x - 0.5, 0, 1, 4);
		ctx.textBaseline = 'top';
		ctx.fillText(timeSec.toFixed(1) + 's', x, 6);
	}

	texture.needsUpdate = true;
	const xAxisWidth = winW - tsMargin.left - tsMargin.right;
	const xFixedScale = 40;
	const xScale = Math.min(xFixedScale, xAxisWidth);
	sprite.scale.set(xScale, xScale * (64 / 512), 1);
	sprite.position.set((tsMargin.left - tsMargin.right) / 2,
		-winH / 2 + tsMargin.bottom / 2 - 1, 2);
}

/** Compute nice tick intervals for axis labeling. */
export function computeNiceTicks(min: number, max: number, n: number): number[] {
	const range = max - min;
	if (range < 1e-10) return [min];
	const roughStep = range / n;
	const mag = Math.pow(10, Math.floor(Math.log10(roughStep)));
	const normStep = roughStep / mag;
	let niceStep: number;
	if (normStep <= 1.5) niceStep = 1 * mag;
	else if (normStep <= 3) niceStep = 2 * mag;
	else if (normStep <= 7) niceStep = 5 * mag;
	else niceStep = 10 * mag;

	const start = Math.ceil(min / niceStep) * niceStep;
	const ticks: number[] = [];
	for (let v = start; v <= max + niceStep * 0.01; v += niceStep) {
		ticks.push(v);
		if (ticks.length > n + 2) break;
	}
	return ticks;
}
