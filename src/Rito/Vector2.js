export const ctor_ =
	(vector2) =>
	({ x, y }) =>
		new vector2(x, y);
export const add_ = (a) => (b) => a.clone().add(b);
export const sub_ = (a) => (b) => a.clone().sub(b);
export const multiply_ = (a) => (b) => a.clone().multiply(b);
export const divide_ = (a) => (b) => a.clone().divide(b);
export const normalize_ = (a) => a.clone().normalize();
