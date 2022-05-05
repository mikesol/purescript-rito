import * as THREE from "three";

export const multiply_ = (a) => (b) => a.clone().multiply(b);
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
