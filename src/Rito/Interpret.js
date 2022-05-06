import * as THREE from "three";

var connectXToY_ = function (x) {
	return function (y) {
		return function (state) {
			return function () {
				if (y === "@portal@") {
					return;
				}
				var xmain = state.units[x].main;
				if (xmain instanceof THREE.SphereGeometry) {
					state.units[y].main.geometry = state.units[x].main;
				} else if (xmain instanceof THREE.MeshStandardMaterial) {
					state.units[y].main.material = state.units[x].main;
				} else {
					state.units[y].main.add(state.units[x].main);
				}
			};
		};
	};
};

var genericMake_ = (ctor) => (a) => (state) => () => {
	var { id, scope, parent, ...rest } = a;
	if (!state.scopes[scope]) {
		state.scopes[scope] = [];
	}
	state.scopes[scope].push(ptr);
	state.units[id] = {
		listeners: {},
		parent: parent,
		scope: scope,
		main: ctor(rest),
	};
	connectXToY_(id)(parent)(state)();
};

export const makeBox_ = genericMake_(
	({ width, height, depth, widthSegments, heightSegments, depthSegments }) =>
		new THREE.BoxGeometry(
			width,
			height,
			depth,
			widthSegments,
			heightSegments,
			depthSegments
		)
);
export const makePlane_ = genericMake_(
	({ width, height, widthSegments, heightSegments }) =>
		new THREE.PlaneGeometry(width, height, widthSegments, heightSegments)
);
export const makeTorus_ = genericMake_(
	({ radius, tube, radialSegments, tubularSegments, arc }) =>
		new THREE.TorusGeometry(radius, tube, radialSegments, tubularSegments, arc)
);
export const makeSphere_ = genericMake_(
	({
		radius,
		widthSegments,
		heightSegments,
		phiStart,
		phiLength,
		thetaStart,
		thetaLength,
	}) =>
		new THREE.SphereGeometry(
			radius,
			widthSegments,
			heightSegments,
			phiStart,
			phiLength,
			thetaStart,
			thetaLength
		)
);
export const makePerspectiveCamera_ = genericMake_(
	({ fov, aspect, near, far }) =>
		new THREE.PerspectiveCamera(fov, aspect, near, far)
);
export const makeMesh_ = (a) => (state) => () => {
	genericMake_(() => new THREE.Mesh())(a)(state)();
	state.units[a.id].main.geometry = state.units[a.geometry].main;
	state.units[a.id].main.material = state.units[a.material].main;
};
export const makeScene_ = genericMake_(() => new THREE.Scene());
// sphere
export const setRadius_ = (a) => (state) => () => {
	state.units[a.id].main.radius = a.radius;
};
export const setWidthSegments_ = (a) => (state) => () => {
	state.units[a.id].main.widthSegments = a.widthSegments;
};
export const setHeightSegments_ = (a) => (state) => () => {
	state.units[a.id].main.heightSegments = a.heightSegments;
};
export const setPhiStart_ = (a) => (state) => () => {
	state.units[a.id].main.phiStart = a.phiStart;
};
export const setPhiLength_ = (a) => (state) => () => {
	state.units[a.id].main.phiLength = a.phiLength;
};
export const setThetaStart_ = (a) => (state) => () => {
	state.units[a.id].main.thetaStart = a.thetaStart;
};
export const setThetaLength_ = (a) => (state) => () => {
	state.units[a.id].main.thetaLength = a.thetaLength;
};
// bufferGeometry
export const setMatrix4_ = (a) => (state) => () => {
	state.units[a.id].main.applyMatrix4(a.matrix4);
};
export const setQuaternion_ = (a) => (state) => () => {
	state.units[a.id].main.applyQuaternion(a.quaternion);
};
export const setRotateX_ = (a) => (state) => () => {
	state.units[a.id].main.rotateX(a.rotateX);
};
export const setRotateY_ = (a) => (state) => () => {
	state.units[a.id].main.rotateY(a.rotateY);
};
export const setRotateZ_ = (a) => (state) => () => {
	state.units[a.id].main.rotateZ(a.rotateZ);
};
export const setTranslate_ = (a) => (state) => () => {
	state.units[a.id].main.translate(a.x, a.y, a.z);
};
export const setScale_ = (a) => (state) => () => {
	state.units[a.id].main.scale(a.x, a.y, a.z);
};
export const setLookAt_ = (a) => (state) => () => {
	state.units[a.id].main.lookAt(a.v);
};
export const setCenter_ = (a) => (state) => () => {
	state.units[a.id].main.center();
};
export const getBoundingBox_ = (a) => (state) => () => {
	state.units[a.id].main.computeBoundingBox();
	a.box(state.units[a.id].main.boundingBox)();
};
export const getBoundingSphere_ = (a) => (state) => () => {
	state.units[a.id].main.computeBoundingSphere();
	a.box(state.units[a.id].main.boundingSphere)();
};
// mesh standard material
export const setColor_ = (a) => (state) => () => {
	state.units[a.id].main.color = a.color;
};
export const setRoughness_ = (a) => (state) => () => {
	state.units[a.id].main.roughness = a.roughness;
};
export const setMetalness_ = (a) => (state) => () => {
	state.units[a.id].main.metalness = a.metalness;
};
export const setMap_ = (a) => (state) => () => {
	state.units[a.id].main.map = a.map;
};
export const setLightMap_ = (a) => (state) => () => {
	state.units[a.id].main.lightMap = a.lightMap;
};
export const setLightMapIntensity_ = (a) => (state) => () => {
	state.units[a.id].main.lightMapIntensity = a.lightMapIntensity;
};
export const setAoMap_ = (a) => (state) => () => {
	state.units[a.id].main.aoMap = a.aoMap;
};
export const setAoMapIntensity_ = (a) => (state) => () => {
	state.units[a.id].main.aoMapIntensity = a.aoMapIntensity;
};
export const setEmissive_ = (a) => (state) => () => {
	state.units[a.id].main.emissive = a.emissive;
};
export const setEmissiveIntensity_ = (a) => (state) => () => {
	state.units[a.id].main.emissiveIntensity = a.emissiveIntensity;
};
export const setEmissiveMap_ = (a) => (state) => () => {
	state.units[a.id].main.emissiveMap = a.emissiveMap;
};
export const setBumpMap_ = (a) => (state) => () => {
	state.units[a.id].main.bumpMap = a.bumpMap;
};
export const setBumpScale_ = (a) => (state) => () => {
	state.units[a.id].main.bumpScale = a.bumpScale;
};
export const setNormalMap_ = (a) => (state) => () => {
	state.units[a.id].main.normalMap = a.normalMap;
};
export const setNormalMapType_ = (a) => (state) => () => {
	state.units[a.id].main.normalMapType = a.normalMapType;
};
export const setNormalScale_ = (a) => (state) => () => {
	state.units[a.id].main.normalScale = a.normalScale;
};
export const setDisplacementMap_ = (a) => (state) => () => {
	state.units[a.id].main.displacementMap = a.displacementMap;
};
export const setDisplacementScale_ = (a) => (state) => () => {
	state.units[a.id].main.displacementScale = a.displacementScale;
};
export const setDisplacementBias_ = (a) => (state) => () => {
	state.units[a.id].main.displacementBias = a.displacementBias;
};
export const setRoughnessMap_ = (a) => (state) => () => {
	state.units[a.id].main.roughnessMap = a.roughnessMap;
};
export const setMetalnessMap_ = (a) => (state) => () => {
	state.units[a.id].main.metalnessMap = a.metalnessMap;
};
export const setAlphaMap_ = (a) => (state) => () => {
	state.units[a.id].main.alphaMap = a.alphaMap;
};
export const setEnvMap_ = (a) => (state) => () => {
	state.units[a.id].main.envMap = a.envMap;
};
export const setEnvMapIntensity_ = (a) => (state) => () => {
	state.units[a.id].main.envMapIntensity = a.envMapIntensity;
};
export const setWireframe_ = (a) => (state) => () => {
	state.units[a.id].main.wireframe = a.wireframe;
};
export const setWireframeLinewidth_ = (a) => (state) => () => {
	state.units[a.id].main.wireframeLinewidth = a.wireframeLinewidth;
};
export const setFlatShading_ = (a) => (state) => () => {
	state.units[a.id].main.flatShading = a.flatShading;
};
// mesh
export const setRotationFromAxisAngle_ = (a) => (state) => () => {
	state.units[a.id].main.setRotationFromAxisAngle(a.axis, a.angle);
};
export const setRotationFromEuler_ = (a) => (state) => () => {
	state.units[a.id].main.setRotationFromEuler(a.euler);
};
export const setRotationFromMatrix_ = (a) => (state) => () => {
	state.units[a.id].main.setRotationFromMatrix(a.matrix4);
};
export const setRotationFromQuaternion_ = (a) => (state) => () => {
	state.units[a.id].main.setRotationFromQuaternion(a.quaternion);
};
export const setRotateOnAxis_ = (a) => (state) => () => {
	state.units[a.id].main.rotateOnAxis(a.axis, a.angle);
};
export const setRotateOnWorldAxis_ = (a) => (state) => () => {
	state.units[a.id].main.rotateOnWorldAxis(a.axis, a.angle);
};
export const setTranslateOnAxis_ = (a) => (state) => () => {
	state.units[a.id].main.translateOnAxis(a.axis, a.distance);
};
export const setTranslateX_ = (a) => (state) => () => {
	state.units[a.id].main.translateX(a.translateX);
};
export const setTranslateY_ = (a) => (state) => () => {
	state.units[a.id].main.translateY(a.translateY);
};
export const setTranslateZ_ = (a) => (state) => () => {
	state.units[a.id].main.translateZ(a.translateZ);
};
//
export function makeFFIThreeSnapshot() {
	return {
		units: {},
		scopes: {},
	};
}

export function makeNoop_(a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			state.units[ptr] = {
				noop: true,
			};
		};
	};
}

export function giveNewParent_(a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			var parent = a.parent;
			state.units[ptr].containingScope = a.scope;
			state.units[parent].main.prepend(state.units[ptr].main);
		};
	};
}

export function disconnect_(a) {
	return function (state) {
		return function () {
			var ptr = a.id;
			if (state.units[ptr].noop) {
				return;
			}
			if (
				state.units[ptr].containingScope &&
				state.units[ptr].containingScope !== a.scope
			) {
				return;
			}
			// check to make sure this actually works
			state.units[ptr].main.remove();
			state.units[ptr].main.destroy();
		};
	};
}

export function deleteFromCache_(a) {
	return function (state) {
		return function () {
			delete state.units[a.id];
		};
	};
}
