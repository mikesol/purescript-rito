export const ctor_ = (matrix4) => new matrix4();
export const set_ =
	(matrix4) =>
	({
		n11,
		n12,
		n13,
		n14,
		n21,
		n22,
		n23,
		n24,
		n31,
		n32,
		n33,
		n34,
		n41,
		n42,
		n43,
		n44,
	}) =>
		new matrix4().set(
			n11,
			n12,
			n13,
			n14,
			n21,
			n22,
			n23,
			n24,
			n31,
			n32,
			n33,
			n34,
			n41,
			n42,
			n43,
			n44
		);
export const equals_ = (a) => (b) => a.equals(b);
export const compose_ = (matrix4) => (position) => (quaternion) => (scale) =>
	new matrix4().compose(position, quaternion, scale);
export const determinant_ = (a) => a.determinant();
export const invert_ = (a) => a.clone().invert();
export const identity_ = (matrix4) => new matrix4().identity();
export const lookAt_ = (eye) => (target) => (up) => (a) =>
	a.clone().lookAt(eye, target, up);
export const makeRotationAxis_ = (axis) => (a) => (theta) =>
	a.clone().makeRotationAxis(axis, theta);
export const makeRotationFromEuler_ = (euler) => (a) =>
	a.clone().makeRotationFromEuler(euler);
export const makeRotationFromQuaternion_ = (q) => (a) =>
	a.clone().makeRotationFromQuaternion(q);
export const makeRotationX_ = (theta) => (a) => a.clone().makeRotationX(theta);
export const makeRotationY_ = (theta) => (a) => a.clone().makeRotationY(theta);
export const makeRotationZ_ = (theta) => (a) => a.clone().makeRotationZ(theta);
export const makeScale_ = (a) => (x) => (y) => (z) =>
	a.clone().makeScale(x, y, z);
export const makeShear_ = (a) => (xy) => (xz) => (yx) => (yz) => (zx) => (zy) =>
	a.clone().makeShear(xy, xz, yx, yz, zx, zy);
export const makeTranslation_ = (a) => (x) => (y) => (z) =>
	a.clone().makeTranslation(x, y, z);
export const multiply_ = (a) => (b) => a.clone().multiply(b);
export const multiplyMatrices_ = (a) => (b) => a.clone().multiplyMatrices(a, b);
export const multiplyScalar_ = (b) => (a) => a.clone().multiplyScalar(b);
export const premultiply_ = (a) => (b) => a.clone().premultiply(b);
export const scale_ = (v) => (a) => a.clone().scale(v);
export const setPosition_ = (v) => (a) => a.clone().setPosition(v);
export const transpose_ = (a) => a.clone().transpose();
