import * as THREE from "three";

export const ctor_ = new THREE.Matrix4();
export const set_ = ({
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
	new THREE.Matrix4().set(
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
export const compose_ = (a) => (position) => (quaternion) => (scale) =>
	a.clone().compose(position, quaternion, scale);
export const determinant_ = (a) => a.determinant();
export const invert_ = (a) => a.clone().invert();
export const identity_ = (a) => new THREE.Matrix4().identity();
export const lookAt_ = (a) => (eye) => (target) => (up) =>
	a.clone().lookAt(eye, target, up);
export const makeRotationAxis_ = (a) => (axis) => (theta) =>
	a.clone().makeRotationAxis(axis, theta);
export const makeRotationFromEuler_ = (a) => (euler) =>
	a.clone().makeRotationFromEuler(euler);
export const makeRotationFromQuaternion_ = (a) => (q) =>
	a.clone().makeRotationFromQuaternion(q);
export const makeRotationX_ = (a) => (theta) => a.clone().makeRotationX(theta);
export const makeRotationY_ = (a) => (theta) => a.clone().makeRotationY(theta);
export const makeRotationZ_ = (a) => (theta) => a.clone().makeRotationZ(theta);
export const makeScale_ = (a) => (x) => (y) => (z) =>
	a.clone().makeScale(x, y, z);
export const makeShear_ = (a) => (xy) => (xz) => (yx) => (yz) => (zx) => (zy) =>
	a.clone().makeShear(xy, xz, yx, yz, zx, zy);
export const makeTranslation_ = (a) => (x) => (y) => (z) =>
	a.clone().makeTranslation(x, y, z);
export const multiply_ = (a) => (b) => a.clone().multiply(b);
export const multiplyMatrices_ = (a) => (b) =>
	new THREE.Matrix4().multiplyMatrices(a, b);
export const multiplyScalar_ = (a) => (b) => a.clone().multiplyScalar(b);
export const premultiply_ = (a) => (b) => a.clone().premultiply(b);
export const scale_ = (a) => (v) => a.clone().scale(v);
export const setPosition_ = (a) => (v) => a.clone().setPosition(v);
export const transpose_ = (a) => a.clone().transpose();
