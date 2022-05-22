export const ctor_ = (THREE) => ({ x, y, z }) => new THREE.Vector3(x, y, z);
export const add_ = (a) => (b) => a.clone().add(b);
export const sub_ = (a) => (b) => a.clone().sub(b);
export const multiply_ = (a) => (b) => a.clone().multiply(b);
export const divide_ = (a) => (b) => a.clone().divide(b);
export const normalize_ = (a) => a.clone().normalize();
