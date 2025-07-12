import * as THREE from 'three';

export const initThree = () => {
  const renderer = new THREE.WebGLRenderer({ canvas: document.querySelector("bg") , antialias: true });
  const scene    = new THREE.Scene();
  const camera   = new THREE.PerspectiveCamera(45, 4 / 3, 0.1, 1000);
  // … build your dynamical-system geometry …

  let rafId = 0;
  function loop() {
    rafId = requestAnimationFrame(loop);
    renderer.render(scene, camera);
  }
  loop();

  return { renderer, scene, camera, rafId };
};

export const addPoints = (scene, points) => {
  const geometry = new THREE.BufferGeometry();
  const positions = new Float32Array(points.length * 3);
  for (let i = 0; i < points.length; i++) {
    positions[i * 3] = points[i].x;
    positions[i * 3 + 1] = points[i].y;
    positions[i * 3 + 2] = points[i].z;
  }
  geometry.setAttribute('position', new THREE.BufferAttribute(positions, 3));
  const material = new THREE.PointsMaterial({ color: 0x0000ff, size: 10 });
  const points = new THREE.Points(geometry, material);
  scene.add(points);
};

export const pauseThree  = b => cancelAnimationFrame(b.rafId);
export const resumeThree = b => { b.rafId = requestAnimationFrame(function loop() {
                                     b.rafId = requestAnimationFrame(loop);
                                     b.renderer.render(b.scene, b.camera);
                                   }); };

export const disposeThree = b => {
  pauseThree(b);
  b.renderer.dispose();
  // walk scene children, call geometry.dispose() / material.dispose()
};
