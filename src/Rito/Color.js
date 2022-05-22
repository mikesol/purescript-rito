export const ctor_ = THREE => (rep) => new THREE.Color(rep);
export const ctorRGB_ =
	(THREE) =>
	({ r, g, b }) =>
		new THREE.Color(r, g, b);
