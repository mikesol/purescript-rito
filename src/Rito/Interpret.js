export const three = () => import("three");
export const orbitControls = () =>
	import("three/examples/jsm/controls/OrbitControls.js").then(
		(r) => r.OrbitControls
	);
export const css2DRenderer = () =>
	import("three/examples/jsm/renderers/CSS2DRenderer.js");

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
		// uggggh
		main: ctor(state.THREE, rest, state.CSS2DObject),
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

export const connectScene_ = (a) => (state) => () => {
	// for now this is a no op
	// in the current setup, scenes are parent-less and not
	// intrinsicaally connected or disconnected to anything
	// in the future, we will want to change this so that a renderer
	// can render a revolving scene
};
export const connectCamera_ = (a) => (state) => () => {
	// for now this is a no op
	// in the current setup, cameras are parent-less and not
	// intrinsicaally connected or disconnected to anything
	// in the future, we will want to change this so that a renderer
	// can render a revolving camera
};

export const connectMaterial_ = (a) => (state) => () => {
	state.units[a.parent].main.material = state.units[a.id].main;
};

export const makeBox_ = genericMake_(
	(
		THREE,
		{ width, height, depth, widthSegments, heightSegments, depthSegments }
	) =>
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
	(THREE, { width, height, widthSegments, heightSegments }) =>
		new THREE.PlaneGeometry(width, height, widthSegments, heightSegments)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeCapsule_ = genericMake_(
	(THREE, { radius, length, capSegments, radialSegments }) =>
		new THREE.CapsuleGeometry(radius, length, capSegments, radialSegments)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeTorus_ = genericMake_(
	(THREE, { radius, tube, radialSegments, tubularSegments, arc }) =>
		new THREE.TorusGeometry(radius, tube, radialSegments, tubularSegments, arc)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeSphere_ = genericMake_(
	(
		THREE,
		{
			radius,
			widthSegments,
			heightSegments,
			phiStart,
			phiLength,
			thetaStart,
			thetaLength,
		}
	) =>
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
export const makePerspectiveCamera_ = (a) => (state) => () => {
	genericMake_(
		(THREE, { fov, aspect, near, far }) =>
			new THREE.PerspectiveCamera(fov, aspect, near, far)
	)(() => {})(a)(state)();
	const orbitControls = new state.OrbitControls(
		state.units[a.id].main,
		a.orbitControls.canvas
	);
	orbitControls.enabled = a.orbitControls.enabled;
	orbitControls.autoRotate = a.orbitControls.autoRotate;
	orbitControls.autoRotateSpeed = a.orbitControls.autoRotateSpeed;
	orbitControls.dampingFactor = a.orbitControls.dampingFactor;
	orbitControls.enableDamping = a.orbitControls.enableDamping;
	orbitControls.enableZoom = a.orbitControls.enableZoom;
	orbitControls.enablePan = a.orbitControls.enablePan;
	orbitControls.panSpeed = a.orbitControls.panSpeed;
	orbitControls.rotateSpeed = a.orbitControls.rotateSpeed;
	orbitControls.zoomSpeed = a.orbitControls.zoomSpeed;
	state.orbitControls = orbitControls;
};

const ascSort = function (a, b) {
	return a.distance - b.distance;
};
// COPY of generic make, needed because indexed mesh is a bit different
export const makeInstancedMesh_ = (a) => (state) => () => {
	// ugggghhhh
	const _instanceLocalMatrix = /*@__PURE__*/ new state.THREE.Matrix4();
	const _instanceWorldMatrix = /*@__PURE__*/ new state.THREE.Matrix4();

	const _instanceIntersects = [];
	const _mesh = /*@__PURE__*/ new state.THREE.Mesh();
	class MyInstancedMesh extends state.THREE.InstancedMesh {
		constructor(geometry, material, count) {
			super(geometry, material, count);
		}
		raycastInstances(raycaster, instances, intersects) {
			const matrixWorld = this.matrixWorld;
			const raycastTimes =
				instances !== undefined ? instances.length : this.count;

			_mesh.geometry = this.geometry;
			_mesh.material = this.material;

			if (_mesh.material === undefined) return;

			for (let i = 0; i < raycastTimes; i++) {
				const instanceId = instances !== undefined ? instances[i] : i;
				// calculate the world matrix for each instance

				this.getMatrixAt(instanceId, _instanceLocalMatrix);

				_instanceWorldMatrix.multiplyMatrices(
					matrixWorld,
					_instanceLocalMatrix
				);

				// the mesh represents this single instance

				_mesh.matrixWorld = _instanceWorldMatrix;

				_mesh.raycast(raycaster, _instanceIntersects);

				// process the result of raycast

				for (let i = 0, l = _instanceIntersects.length; i < l; i++) {
					const intersect = _instanceIntersects[i];
					intersect.instanceId = instanceId;
					intersect.object = this;
					intersects.push(intersect);
				}

				_instanceIntersects.length = 0;
			}
		}
	}
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
		main: new MyInstancedMesh(
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
export const makeMesh_ = genericMake_((THREE) => new THREE.Mesh())((x, y) => {
	y.main.add(x.main);
});
export const makeCSS2DObject_ = genericMake_(
	(_, { nut }, CSS2DObject) => new CSS2DObject(nut)
)((x, y) => {
	y.main.add(x.main);
});
export const makeAmbientLight_ = genericMake_((THREE, { color, intensity }) => {
	return new THREE.AmbientLight(color, intensity);
})((x, y) => {
	y.main.add(x.main);
});
export const makeDirectionalLight_ = genericMake_(
	(THREE, { color, intensity }) => new THREE.DirectionalLight(color, intensity)
)((x, y) => {
	y.main.add(x.main);
});
export const makePointLight_ = genericMake_(
	(THREE, { color, intensity, distance, decay }) =>
		new THREE.PointLight(color, intensity, distance, decay)
)((x, y) => {
	y.main.add(x.main);
});
export const makeMeshBasicMaterial_ = genericMake_(
	(THREE, options) => new THREE.MeshBasicMaterial(options)
)((x, y) => {
	y.main.material = x.main;
});
export const makeMeshStandardMaterial_ = genericMake_(
	(THREE, options) => new THREE.MeshStandardMaterial(options)
)((x, y) => {
	y.main.material = x.main;
});
export const setSize_ = (a) => (state) => () => {
	state.units[a.id].main.setSize(a.width, a.height);
};
export const makeScene_ = genericMake_((THREE) => new THREE.Scene())(() => {});
export const makeGroup_ = genericMake_((THREE) => new THREE.Group())((x, y) => {
	y.main.add(x.main);
});
export const webGLRender_ = (a) => (state) => () => {
	state.orbitControls.update();
	state.units[a.id].main.render(
		state.units[a.scene].main,
		state.units[a.camera].main
	);
};
export const css2DRender_ = (a) => (state) => () => {
	state.orbitControls.update();
	state.units[a.id].main.render(
		state.units[a.scene].main,
		state.units[a.camera].main
	);
};
const getAllTouches = (tl) => {
	const o = [];
	for (let i = 0; i < tl.length; i++) {
		o.push(tl.item(i));
	}
	return o;
};

const intersectInstance = (raycaster, mesh, instances, intersects = []) => {
	if (mesh.layers.test(raycaster.layers)) {
		mesh.raycastInstances(raycaster, instances, intersects);
	}

	intersects.sort(ascSort);

	return intersects;
};

export const makeWebGLRenderer_ = (a) => (state) => () => {
	const { id, ...parameters } = a;
	const canvas = parameters.canvas;
	const renderer = new state.THREE.WebGLRenderer(parameters);
	state.units[a.id] = { main: renderer };
	renderer.setSize(canvas.width, canvas.height);
	renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
	const raycaster = new state.THREE.Raycaster();
	const camera = state.units[parameters.camera].main;

	const makeListener = (eventName) => {
		canvas.addEventListener(eventName, ($e) => {
			const entries = Object.entries(state.listeners[eventName]);
			if (entries.length > 0) {
				const es =
					eventName.indexOf("touch") !== -1 ? getAllTouches($e.touches) : [$e];
				es.forEach((e) => {
					const x = (e.clientX / window.innerWidth) * 2 - 1;
					const y = -(e.clientY / window.innerHeight) * 2 + 1;
					raycaster.setFromCamera({ x, y }, camera);
					entries.forEach(([k, v]) => {
						const u = state.units[k].main;
						const intersects = raycaster.intersectObject(u);
						if (intersects.length > 0) {
							v(e)();
						}
					});
				});
			}
			/////////// instanced code
			const instancedEntries = Object.entries(
				state.listeners[eventName + "Instanced"]
			);
			if (instancedEntries.length > 0) {
				const es =
					eventName.indexOf("touch") !== -1 ? getAllTouches($e.touches) : [$e];
				es.forEach((e) => {
					const x = (e.clientX / window.innerWidth) * 2 - 1;
					const y = -(e.clientY / window.innerHeight) * 2 + 1;
					raycaster.setFromCamera({ x, y }, camera);
					instancedEntries.forEach(([k, v]) => {
						const u = state.units[k].main;
						Object.entries(v).forEach(([kk, vv]) => {
							const intersects = intersectInstance(raycaster, u, [kk]);
							if (intersects.length > 0) {
								vv(e)();
							}
						});
					});
				});
			}
		});
	};
	makeListener("click");
	makeListener("mousedown");
	makeListener("mouseup");
	makeListener("mousemove");
	makeListener("touchstart");
	makeListener("touchend");
	makeListener("touchmove");
	makeListener("touchcancel");
};
export const makeCSS2DRenderer_ = (a) => (state) => () => {
	const { id, canvas, element } = a;
	const renderer = new state.CSS2DRenderer({ element });
	renderer.setSize(canvas.offsetWidth, canvas.offsetHeight);
	state.units[id] = { main: renderer };
};
export const setOnClick_ = (a) => (state) => () => {
	state.listeners.click[a.id] = a.onClick;
};
export const setOnMouseDown_ = (a) => (state) => () => {
	state.listeners.mousedown[a.id] = a.onMouseDown;
};
export const setOnMouseUp_ = (a) => (state) => () => {
	state.listeners.mouseup[a.id] = a.onMouseUp;
};
export const setOnMouseMove_ = (a) => (state) => () => {
	state.listeners.mousemove[a.id] = a.onMouseMove;
};
export const setOnTouchStart_ = (a) => (state) => () => {
	state.listeners.touchstart[a.id] = a.onTouchStart;
};
export const setOnTouchEnd_ = (a) => (state) => () => {
	state.listeners.touchend[a.id] = a.onTouchEnd;
};
export const setOnTouchMove_ = (a) => (state) => () => {
	state.listeners.touchmove[a.id] = a.onTouchMove;
};
export const setOnTouchCancel_ = (a) => (state) => () => {
	state.listeners.touchcancel[a.id] = a.onTouchCancel;
};
export const removeOnClick_ = (a) => (state) => () => {
	delete state.listeners.click[a.id];
};
export const removeOnMouseDown_ = (a) => (state) => () => {
	delete state.listeners.mousedown[a.id];
};
export const removeOnMouseUp_ = (a) => (state) => () => {
	delete state.listeners.mouseup[a.id];
};
export const removeOnMouseMove_ = (a) => (state) => () => {
	delete state.listeners.mousemove[a.id];
};
export const removeOnTouchStart_ = (a) => (state) => () => {
	delete state.listeners.touchstart[a.id];
};
export const removeOnTouchEnd_ = (a) => (state) => () => {
	delete state.listeners.touchend[a.id];
};
export const removeOnTouchMove_ = (a) => (state) => () => {
	delete state.listeners.touchmove[a.id];
};
export const removeOnTouchCancel_ = (a) => (state) => () => {
	delete state.listeners.touchcancel[a.id];
};
export const setIMOnClick_ = (a) => (state) => () => {
	if (!state.listeners.clickInstanced[a.id]) {
		state.listeners.clickInstanced[a.id] = {};
	}
	state.listeners.clickInstanced[a.id][a.instanceId] = a.onClick;
};
export const setIMOnMouseDown_ = (a) => (state) => () => {
	if (!state.listeners.mousedownInstanced[a.id]) {
		state.listeners.mousedownInstanced[a.id] = {};
	}
	state.listeners.mousedownInstanced[a.id][a.instanceId] = a.onMouseDown;
};
export const setIMOnMouseUp_ = (a) => (state) => () => {
	if (!state.listeners.mouseupInstanced[a.id]) {
		state.listeners.mouseupInstanced[a.id] = {};
	}
	state.listeners.mouseupInstanced[a.id][a.instanceId] = a.onMouseUp;
};
export const setIMOnMouseMove_ = (a) => (state) => () => {
	if (!state.listeners.mousemoveInstanced[a.id]) {
		state.listeners.mousemoveInstanced[a.id] = {};
	}
	state.listeners.mousemoveInstanced[a.id][a.instanceId] = a.onMouseMove;
};
export const setIMOnTouchStart_ = (a) => (state) => () => {
	if (!state.listeners.touchstartInstanced[a.id]) {
		state.listeners.touchstartInstanced[a.id] = {};
	}
	state.listeners.touchstartInstanced[a.id][a.instanceId] = a.onTouchStart;
};
export const setIMOnTouchEnd_ = (a) => (state) => () => {
	if (!state.listeners.touchendInstanced[a.id]) {
		state.listeners.touchendInstanced[a.id] = {};
	}
	state.listeners.touchendInstanced[a.id][a.instanceId] = a.onTouchEnd;
};
export const setIMOnTouchMove_ = (a) => (state) => () => {
	if (!state.listeners.touchmoveInstanced[a.id]) {
		state.listeners.touchmoveInstanced[a.id] = {};
	}
	state.listeners.touchmoveInstanced[a.id][a.instanceId] = a.onTouchMove;
};
export const setIMOnTouchCancel_ = (a) => (state) => () => {
	if (!state.listeners.touchcancelInstanced[a.id]) {
		state.listeners.touchcancelInstanced[a.id] = {};
	}
	state.listeners.touchcancelInstanced[a.id][a.instanceId] = a.onTouchCancel;
};
export const removeIMOnClick_ = (a) => (state) => () => {
	delete state.listeners.clickInstanced[a.id][a.instanceId];
};
export const removeIMOnMouseDown_ = (a) => (state) => () => {
	delete state.listeners.mousedownInstanced[a.id][a.instanceId];
};
export const removeIMOnMouseUp_ = (a) => (state) => () => {
	delete state.listeners.mouseupInstanced[a.id][a.instanceId];
};
export const removeIMOnMouseMove_ = (a) => (state) => () => {
	delete state.listeners.mousemoveInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchStart_ = (a) => (state) => () => {
	delete state.listeners.touchstartInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchEnd_ = (a) => (state) => () => {
	delete state.listeners.touchendInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchMove_ = (a) => (state) => () => {
	delete state.listeners.touchmoveInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchCancel_ = (a) => (state) => () => {
	delete state.listeners.touchcancelInstanced[a.id][a.instanceId];
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
// scene (background)
export const setBackgroundColor_ = (a) => (state) => () => {
	state.units[a.id].background = a.color;
}
export const setBackgroundTexture_ = (a) => (state) => () => {
	state.units[a.id].background = a.texture;
};
export const setBackgroundCubeTexture_ = (a) => (state) => () => {
	state.units[a.id].background = a.cubeTexture;
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
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setRotateY_ = (a) => (state) => () => {
	state.units[a.id].main.rotateY(a.rotateY);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setRotateZ_ = (a) => (state) => () => {
	state.units[a.id].main.rotateZ(a.rotateZ);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setTranslate_ = (a) => (state) => () => {
	state.units[a.id].main.translate(a.x, a.y, a.z);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setScale_ = (a) => (state) => () => {
	state.units[a.id].main.scale(a.x, a.y, a.z);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setScaleX_ = (a) => (state) => () => {
	state.units[a.id].main.scale.x = a.scaleX;
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setScaleY_ = (a) => (state) => () => {
	state.units[a.id].main.scale.y = a.scaleY;
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setScaleZ_ = (a) => (state) => () => {
	state.units[a.id].main.scale.z = a.scaleZ;
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setLookAt_ = (a) => (state) => () => {
	state.units[a.id].main.lookAt(a.v);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
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
export const setSingleInstancedMeshMatrix4_ = (a) => (state) => () => {
	const u = state.units[a.id].main;
	u.setMatrixAt(a.instanceId, a.matrix4);
	u.instanceMatrix.needsUpdate = true;
};
export const setSingleInstancedMeshColor_ = (a) => (state) => () => {
	const u = state.units[a.id].main;
	u.setColorAt(a.instanceId, a.color);
	u.instanceColor.needsUpdate = true;
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
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setRotationFromEuler_ = (a) => (state) => () => {
	state.units[a.id].main.setRotationFromEuler(a.euler);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setRotationFromMatrix_ = (a) => (state) => () => {
	state.units[a.id].main.setRotationFromMatrix(a.matrix4);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setRotationFromQuaternion_ = (a) => (state) => () => {
	state.units[a.id].main.setRotationFromQuaternion(a.quaternion);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setRotateOnAxis_ = (a) => (state) => () => {
	state.units[a.id].main.rotateOnAxis(a.axis, a.angle);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setRotateOnWorldAxis_ = (a) => (state) => () => {
	state.units[a.id].main.rotateOnWorldAxis(a.axis, a.angle);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setTranslateOnAxis_ = (a) => (state) => () => {
	state.units[a.id].main.translateOnAxis(a.axis, a.distance);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setTranslateX_ = (a) => (state) => () => {
	state.units[a.id].main.translateX(a.translateX);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setTranslateY_ = (a) => (state) => () => {
	state.units[a.id].main.translateY(a.translateY);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setTranslateZ_ = (a) => (state) => () => {
	state.units[a.id].main.translateZ(a.translateZ);
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setPositionX_ = (a) => (state) => () => {
	state.units[a.id].main.position.x = a.positionX;
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setPositionY_ = (a) => (state) => () => {
	state.units[a.id].main.position.y = a.positionY;
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
export const setPositionZ_ = (a) => (state) => () => {
	state.units[a.id].main.position.z = a.positionZ;
	// updateIfCamera(state.units[a.id].main, state.orbitControls);
};
// camera
export const withWorldDirection_ = (a) => (state) => () => {
	const v3 = new state.THREE.Vector3();
	state.units[a.id].main.getWorldDirection(v3);
	a.withWorldDirection(v3)(state)();
};
// orbit controls
export const setTarget_ = (a) => (state) => () => {
	state.orbitControls.target = a.target;
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
export const makeFFIThreeSnapshot =
	({ three, orbitControls, css2DRenderer, css2DObject }) =>
	() => {
		return {
			THREE: three,
			OrbitControls: orbitControls,
			CSS2DRenderer: css2DRenderer,
			CSS2DObject: css2DObject,
			units: {},
			scopes: {},
			// it's a bit hackish
			// no, not a bit, it's mega hackish
			// but we hook up our listeners here
			listeners: {
				click: {},
				mousemove: {},
				mouseup: {},
				mousedown: {},
				touchstart: {},
				touchend: {},
				touchmove: {},
				touchcancel: {},
				clickInstanced: {},
				mousemoveInstanced: {},
				mouseupInstanced: {},
				mousedownInstanced: {},
				touchstartInstanced: {},
				touchendInstanced: {},
				touchmoveInstanced: {},
				touchcancelInstanced: {},
			},
		};
	};

export function disconnect_(a) {
	return function (state) {
		return function () {
			const ptr = a.id;
			// check to make sure this actually works
			state.units[ptr].main.removeFromParent();
			if (a.scope !== GLOBAL_SCOPE) {
				if (
					state.units[ptr].main instanceof state.THREE.BufferGeometry ||
					state.units[ptr].main instanceof state.THREE.Material
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
			if (state.units[a.id].main.dispose) {
				state.units[a.id].main.dispose();
			}
			if (state.units[a.id].main.removeFromParent) {
				state.units[a.id].main.removeFromParent();
			}
			delete state.units[a.id];
		};
	};
}
