import * as THREE from "three";

const GLOBAL_SCOPE = "@global@";

const genericMake_ = (ctor) => (conn) => (a) => (state) => () => {
	const { id, scope, parent, ...rest } = a;
	const $scope = scope ? scope : GLOBAL_SCOPE;
	if (!state.scopes[$scope]) {
		state.scopes[$scope] = [];
	}
	state.scopes[$scope].push(id);
	state.units[id] = {
		listeners: {},
		parent: parent,
		scope: $scope,
		main: ctor(rest),
	};
	if (parent === undefined) {
		return;
	}
	conn(state.units[id], state.units[parent]);
};

export const stripUndefined_ = (a) => {
	const out = { ...a };
	for (const key in a) {
		if (a[key] === undefined) {
			delete out[key];
		}
	}
	return out;
};

export const connectToScene_ = (a) => (state) => () =>
	state.units[a.parent].main.add(state.units[a.id].main);

export const connectMesh_ = (a) => (state) => () =>
	state.units[a.parent].main.add(state.units[a.id].main);

export const connectGeometry_ = (a) => (state) => () => {
	state.units[a.parent].main.geometry = state.units[a.id].main;
};

export const connectMaterial_ = (a) => (state) => () => {
	state.units[a.parent].main.material = state.units[a.id].main;
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
)((x, y) => {
	y.main.geometry = x.main;
});
export const makePlane_ = genericMake_(
	({ width, height, widthSegments, heightSegments }) =>
		new THREE.PlaneGeometry(width, height, widthSegments, heightSegments)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeCapsule_ = genericMake_(
	({ radius, length, capSegments, radialSegments }) =>
		new THREE.CapsuleGeometry(radius, length, capSegments, radialSegments)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeTorus_ = genericMake_(
	({ radius, tube, radialSegments, tubularSegments, arc }) =>
		new THREE.TorusGeometry(radius, tube, radialSegments, tubularSegments, arc)
)((x, y) => {
	y.main.geometry = x.main;
});
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
)((x, y) => {
	y.main.geometry = x.main;
});
export const makePerspectiveCamera_ = genericMake_(
	({ fov, aspect, near, far }) =>
		new THREE.PerspectiveCamera(fov, aspect, near, far)
)(() => {});
// COPY of generic make, needed because indexed mesh is a bit different
export const makeInstancedMesh_ = (a) => (state) => () => {
	const { id, scope, parent, geometry, material, count } = a;
	const $scope = scope ? scope : GLOBAL_SCOPE;
	if (!state.scopes[$scope]) {
		state.scopes[$scope] = [];
	}
	state.scopes[$scope].push(id);
	state.units[id] = {
		listeners: {},
		parent: parent,
		scope: $scope,
		main: new THREE.InstancedMesh(
			state.units[geometry].main,
			state.units[material].main,
			count
		),
	};
	if (parent === undefined) {
		return;
	}
	state.units[parent].main.add(state.units[id].main);
};
export const makeMesh_ = genericMake_(() => new THREE.Mesh())((x, y) => {
	y.main.add(x.main);
});
export const makeAmbientLight_ = genericMake_(({ color, intensity }) => {
	console.log(color, intensity);
	return new THREE.AmbientLight(color, intensity);
})((x, y) => {
	y.main.add(x.main);
});
export const makeDirectionalLight_ = genericMake_(
	({ color, intensity }) => new THREE.DirectionalLight(color, intensity)
)((x, y) => {
	y.main.add(x.main);
});
export const makePointLight_ = genericMake_(
	({ color, intensity, distance, decay }) =>
		new THREE.PointLight(color, intensity, distance, decay)
)((x, y) => {
	y.main.add(x.main);
});
export const makeMeshBasicMaterial_ = genericMake_(
	(options) => new THREE.MeshBasicMaterial(options)
)((x, y) => {
	y.main.material = x.main;
});
export const makeMeshStandardMaterial_ = genericMake_(
	(options) => new THREE.MeshStandardMaterial(options)
)((x, y) => {
	y.main.material = x.main;
});
export const setSize_ = (a) => (state) => () => {
	state.units[a.id].main.setSize(a.width, a.height);
};
export const makeScene_ = genericMake_(() => new THREE.Scene())(() => {});
export const makeGroup_ = genericMake_(() => new THREE.Group())((x, y) => {
	y.main.add(x.main);
});
export const webGLRender_ = (a) => (state) => () => {
	state.units[a.id].main.render(
		state.units[a.scene].main,
		state.units[a.camera].main
	);
};
export const makeWebGLRenderer_ = (a) => (state) => () => {
	const { id, ...parameters } = a;
	const renderer = new THREE.WebGLRenderer(parameters);
	state.units[a.id] = { main: renderer };
	renderer.setSize(parameters.canvas.width, parameters.canvas.height);
	renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
};
// box
export const setWidth_ = (a) => (state) => () => {
	state.units[a.id].main.width = a.width;
};
export const setHeight_ = (a) => (state) => () => {
	state.units[a.id].main.height = a.height;
};
export const setDepth_ = (a) => (state) => () => {
	state.units[a.id].main.depth = a.depth;
};
export const setLength_ = (a) => (state) => () => {
	state.units[a.id].main.length = a.length;
};
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
export const setDepthSegments_ = (a) => (state) => () => {
	state.units[a.id].main.depthSegments = a.depthSegments;
};
export const setCapSegments_ = (a) => (state) => () => {
	state.units[a.id].main.capSegments = a.capSegments;
};
export const setRadialSegments_ = (a) => (state) => () => {
	state.units[a.id].main.radialSegments = a.radialSegments;
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
export const setScaleX_ = (a) => (state) => () => {
	state.units[a.id].main.scale.x = a.scaleX;
};
export const setScaleY_ = (a) => (state) => () => {
	state.units[a.id].main.scale.y = a.scaleY;
};
export const setScaleZ_ = (a) => (state) => () => {
	state.units[a.id].main.scale.z = a.scaleZ;
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
// instanced mesh
export const setInstancedMeshMatrix4_ = (a) => (state) => () => {
	const u = state.units[a.id].main;
	let updated = false;
	a.setMatrix4((i) => (m) => () => {
		updated = true;
		u.setMatrixAt(i, m);
	})();
	u.instanceMatrix.needsUpdate = updated;
};
export const setInstancedMeshColor_ = (a) => (state) => () => {
	const u = state.units[a.id].main;
	let updated = false;
	a.setColor((i) => (c) => () => {
		updated = true;
		u.setColorAt(i, c);
	})();
	u.instanceColor.needsUpdate = updated;
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
// point light
export const setDistance_ = (a) => (state) => () => {
	state.units[a.id].main.distance = a.distance;
};
export const setDecay_ = (a) => (state) => () => {
	state.units[a.id].main.decay = a.decay;
};
export const setIntensity_ = (a) => (state) => () => {
	state.units[a.id].main.intensity = a.intensity;
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
export const setPositionX_ = (a) => (state) => () => {
	state.units[a.id].main.position.x = a.positionX;
};
export const setPositionY_ = (a) => (state) => () => {
	state.units[a.id].main.position.y = a.positionY;
};
export const setPositionZ_ = (a) => (state) => () => {
	state.units[a.id].main.position.z = a.positionZ;
};
// perspective camera
export const setAspect_ = (a) => (state) => () => {
	state.units[a.id].main.aspect = a.aspect;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFar_ = (a) => (state) => () => {
	state.units[a.id].main.far = a.far;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFilmGauge_ = (a) => (state) => () => {
	state.units[a.id].main.filmGauge = a.filmGauge;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFilmOffset_ = (a) => (state) => () => {
	state.units[a.id].main.filmOffset = a.filmOffset;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFocus_ = (a) => (state) => () => {
	state.units[a.id].main.focus = a.focus;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFov_ = (a) => (state) => () => {
	state.units[a.id].main.fov = a.fov;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setNear_ = (a) => (state) => () => {
	state.units[a.id].main.updateProjectionMatrix();
	state.units[a.id].main.near = a.near;
};
export const setZoom_ = (a) => (state) => () => {
	state.units[a.id].main.zoom = a.zoom;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFocalLength_ = (a) => (state) => () => {
	state.units[a.id].main.setFocalLength(a.focalLength);
};
export const setViewOffset_ =
	({ id, fullWidth, fullHeight, x, y, width, height }) =>
	(state) =>
	() => {
		state.units[id].main.setViewOffset(
			fullWidth,
			fullHeight,
			x,
			y,
			width,
			height
		);
	};

//
export function makeFFIThreeSnapshot() {
	return {
		units: {},
		scopes: {},
	};
}

export function giveNewParent_(a) {
	return function (state) {
		return function () {
			const ptr = a.id;
			const parent = a.parent;
			state.units[ptr].containingScope = a.scope;
			state.units[parent].main.prepend(state.units[ptr].main);
		};
	};
}

export function disconnect_(a) {
	return function (state) {
		return function () {
			const ptr = a.id;
			if (
				state.units[ptr].containingScope &&
				state.units[ptr].containingScope !== a.scope
			) {
				return;
			}
			// check to make sure this actually works
			state.units[ptr].main.remove();
			if (a.scope !== GLOBAL_SCOPE) {
				if (
					state.units[ptr].main instanceof THREE.BufferGeometry ||
					state.units[ptr].main instanceof THREE.Material
				) {
					state.units[ptr].main.dispose();
				}
			}
		};
	};
}

export function deleteFromCache_(a) {
	return function (state) {
		return function () {
			if (
				state.units[a.id].main instanceof THREE.BufferGeometry ||
				state.units[a.id].main instanceof THREE.Material
			) {
				state.units[a.id].main.dispose();
			}
			delete state.units[a.id];
		};
	};
}
