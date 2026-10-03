import * as THREE from 'three';

/** Create a rounded rectangle Shape (centered at origin). */
export function createRoundedRectShape(w: number, h: number, r: number): THREE.Shape {
	const shape = new THREE.Shape();
	const x = -w / 2, y = -h / 2;
	shape.moveTo(x + r, y);
	shape.lineTo(x + w - r, y);
	shape.absarc(x + w - r, y + r, r, -Math.PI / 2, 0, false);
	shape.lineTo(x + w, y + h - r);
	shape.absarc(x + w - r, y + h - r, r, 0, Math.PI / 2, false);
	shape.lineTo(x + r, y + h);
	shape.absarc(x + r, y + h - r, r, Math.PI / 2, Math.PI, false);
	shape.lineTo(x, y + r);
	shape.absarc(x + r, y + r, r, Math.PI, (3 * Math.PI) / 2, false);
	return shape;
}

/** Create a rounded-rect border as a THREE.Line (centered at origin). */
export function createRoundedRectBorder(
	w: number,
	h: number,
	r: number,
	material: THREE.LineBasicMaterial | THREE.LineDashedMaterial
): THREE.Line {
	const shape = createRoundedRectShape(w, h, r);
	const points = shape.getPoints(32);
	const geom = new THREE.BufferGeometry().setFromPoints(
		points.map((p) => new THREE.Vector3(p.x, p.y, 0))
	);
	const line = new THREE.Line(geom, material);
	if (material instanceof THREE.LineDashedMaterial) {
		line.computeLineDistances();
	}
	return line;
}

/** Rounded-top header shape (flat bottom, rounded top corners). */
export function createHeaderShape(width: number, headerHeight: number, radius: number): THREE.Shape {
	const shape = new THREE.Shape();
	const x = -width / 2, y = -headerHeight / 2;
	shape.moveTo(x, y);
	shape.lineTo(x + width, y);
	shape.lineTo(x + width, y + headerHeight - radius);
	shape.absarc(x + width - radius, y + headerHeight - radius, radius, 0, Math.PI / 2, false);
	shape.lineTo(x + radius, y + headerHeight);
	shape.absarc(x + radius, y + headerHeight - radius, radius, Math.PI / 2, Math.PI, false);
	shape.lineTo(x, y);
	return shape;
}
