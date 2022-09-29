const GLOBAL_SCOPE = "@global@";
const nada = () => { };
const genericMake_ = (ctor) => (conn) => (a) => (state) => {
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

export const connectToScene_ = (a) => (state) =>
	state.units[a.parent].main.add(state.units[a.id].main);

export const connectMesh_ = (a) => (state) =>
	state.units[a.parent].main.add(state.units[a.id].main);

export const connectGeometry_ = (a) => (state) => {
	state.units[a.parent].main.geometry = state.units[a.id].main;
};

export const connectScene_ = (a) => (state) => {
	// for now this is a no op
	// in the current setup, scenes are parent-less and not
	// intrinsicaally connected or disconnected to anything
	// in the future, we will want to change this so that a renderer
	// can render a revolving scene
};
export const connectCamera_ = (a) => (state) => {
	if (
		state.units[a.parent] &&
		(state.units[a.parent].main.isScene || state.units[a.parent].main.isGroup)
	) {
		state.units[a.parent].main.add(state.units[a.id].main);
	}
};

export const connectMaterial_ = (a) => (state) => {
	state.units[a.parent].main.material = state.units[a.id].main;
};

const withAtts = (f) => (ctor) => {
	const o = f(ctor);
	const bufferAttributes = Object.entries(ctor.bufferAttributes);
	for (var i = 0; i < bufferAttributes.length; i++) {
		o.setAttribute(bufferAttributes[i][0], bufferAttributes[i][1]);
	}
	const instancedBufferAttributes = Object.entries(
		ctor.instancedBufferAttributes
	);
	for (var i = 0; i < instancedBufferAttributes.length; i++) {
		o.setAttribute(
			instancedBufferAttributes[i][0],
			instancedBufferAttributes[i][1]
		);
	}
	return o;
};

export const makeBox_ = genericMake_(
	withAtts(
		(ctor) =>
			new ctor.box(
				ctor.width,
				ctor.height,
				ctor.depth,
				ctor.widthSegments,
				ctor.heightSegments,
				ctor.depthSegments
			)
	)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makePlane_ = genericMake_(
	withAtts(
		(ctor) =>
			new ctor.plane(
				ctor.width,
				ctor.height,
				ctor.widthSegments,
				ctor.heightSegments
			)
	)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeBufferGeometry_ = genericMake_(
	withAtts((ctor) => new ctor.bufferGeometry())
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeCapsule_ = genericMake_(
	withAtts(
		(ctor) =>
			new ctor.capsule(
				ctor.radius,
				ctor.length,
				ctor.capSegments,
				ctor.radialSegments
			)
	)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeCylinder_ = genericMake_(
	withAtts(
		(ctor) =>
			new ctor.cylinder(
				ctor.radiusTop,
				ctor.radiusBottom,
				ctor.height,
				ctor.radialSegments,
				ctor.heightSegments,
				ctor.openEnded,
				ctor.thetaStart,
				ctor.thetaLength
			)
	)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makeSphere_ = genericMake_(
	withAtts(
		(ctor) =>
			new ctor.sphere(
				ctor.radius,
				ctor.widthSegments,
				ctor.heightSegments,
				ctor.phiStart,
				ctor.phiLength,
				ctor.thetaStart,
				ctor.thetaLength
			)
	)
)((x, y) => {
	y.main.geometry = x.main;
});
export const makePerspectiveCamera_ = genericMake_(
	(ctor) =>
		new ctor.perspectiveCamera(ctor.fov, ctor.aspect, ctor.near, ctor.far)
)(nada);

export const makeGLTFCamera_ =
	genericMake_(({ camera }) => camera)(nada);

const ascSort = function (a, b) {
	return a.distance - b.distance;
};
// COPY of generic make, needed because indexed mesh is a bit different
export const makeInstancedMesh_ = (a) => (state) => {
	// ugggghhhh
	const _instanceLocalMatrix = /*@__PURE__*/ new a.matrix4();
	const _instanceWorldMatrix = /*@__PURE__*/ new a.matrix4();

	const _instanceIntersects = [];
	const _mesh = /*@__PURE__*/ new a.mesh();
	class MyInstancedMesh extends a.instancedMesh {
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
export const makeMesh_ = genericMake_((ctor) => new ctor.mesh())((x, y) => {
	y.main.add(x.main);
});
export const makePoints_ = genericMake_((ctor) => new ctor.points())((x, y) => {
	y.main.add(x.main);
});
export const makeCSS2DObject_ = genericMake_(
	(ctor) => new ctor.css2DObject(ctor.nut)
)((x, y) => {
	y.main.add(x.main);
});
export const makeCSS3DObject_ = genericMake_(
	(ctor) => new ctor.css3DObject(ctor.nut)
)((x, y) => {
	y.main.add(x.main);
});
export const makeAmbientLight_ = genericMake_((ctor) => {
	return new ctor.ambientLight(ctor.color, ctor.intensity);
})((x, y) => {
	y.main.add(x.main);
});
export const makeDirectionalLight_ = genericMake_(
	(ctor) => new ctor.directionalLight(ctor.color, ctor.intensity)
)((x, y) => {
	y.main.add(x.main);
});
export const makePointLight_ = genericMake_(
	(ctor) =>
		new ctor.pointLight(ctor.color, ctor.intensity, ctor.distance, ctor.decay)
)((x, y) => {
	y.main.add(x.main);
});
const withMaterialParameters_ = (f) => (c) => {
	const o = f(c);
	const kv = Object.entries(c.materialParameters);
	kv.forEach(([k, v]) => {
		o[k] = v;
	});
	return o;
};
export const makeShaderMaterial_ = genericMake_(
	withMaterialParameters_(
		({ parameters: { shaderMaterial, ...options } }) =>
			new shaderMaterial(options)
	)
)((x, y) => {
	y.main.material = x.main;
});
export const makeRawShaderMaterial_ = genericMake_(
	withMaterialParameters_(
		({ parameters: { rawShaderMaterial, ...options } }) =>
			new rawShaderMaterial(options)
	)
)((x, y) => {
	y.main.material = x.main;
});
export const makeMeshBasicMaterial_ = genericMake_(
	withMaterialParameters_(
		({ parameters: { meshBasicMaterial, ...options } }) =>
			new meshBasicMaterial(options)
	)
)((x, y) => {
	y.main.material = x.main;
});
export const makeMeshPhongMaterial_ = genericMake_(
	withMaterialParameters_(
		({ parameters: { meshPhongMaterial, ...options } }) =>
			new meshPhongMaterial(options)
	)
)((x, y) => {
	y.main.material = x.main;
});
export const makeMeshStandardMaterial_ = genericMake_(
	withMaterialParameters_(
		({ parameters: { meshStandardMaterial, ...options } }) =>
			new meshStandardMaterial(options)
	)
)((x, y) => {
	y.main.material = x.main;
});
export const makeMeshLambertMaterial_ = genericMake_(
	withMaterialParameters_(
		({ parameters: { meshLambertMaterial, ...options } }) =>
			new meshLambertMaterial(options)
	)
)((x, y) => {
	y.main.material = x.main;
});
export const setSize_ = (a) => (state) => {
	state.units[a.id].main.setSize(a.width, a.height);
};
export const setSizeThroughEffectComposer_ = (a) => (state) => {
	state.units[a.id].main.renderer.setSize(a.width, a.height);
};
export const makeScene_ = genericMake_((ctor) => {
	const o = new ctor.scene();
	if (ctor.fog) {
		o.fog = new ctor.fog.ctor(ctor.fog.color, ctor.fog.density);
	}
	return o;
})(nada);
export const makeGroup_ = genericMake_((ctor) => new ctor.group())((x, y) => {
	y.main.add(x.main);
});
export const makeGLTFGroup_ = genericMake_(({ group }) => group)((x, y) => {
	y.main.add(x.main);
});
export const effectComposerRender_ = (a) => (state) => {
	state.units[a.id].main.render();
};
export const webGLRender_ = (a) => (state) => {
	state.units[a.id].main.render(
		state.units[a.scene].main,
		state.units[a.camera].main
	);
};
export const css2DRender_ = (a) => (state) => {
	state.units[a.id].main.render(
		state.units[a.scene].main,
		state.units[a.camera].main
	);
};
export const css3DRender_ = (a) => (state) => {
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

const doFinalThunk = (e, eventName, state) => {
	if (eventName === "mouseup") {
		const entries = Object.entries(state.listeners["upformousedown"]);
		entries.forEach(([k, val]) => {
			val(e)();
		});
		state.listeners["upformousedown"] = {};
	} else if (eventName === "touchend") {
		const vals = Object.values(state.listeners["endfortouchstart"]);
		vals.forEach((val) => {
			val(e)();
		});
		state.listeners["endfortouchstart"] = {};
	} else if (eventName === "touchcancel") {
		const vals = Object.values(state.listeners["cancelfortouchstart"]);
		vals.forEach((val) => {
			val(e)();
		});
		state.listeners["cancelfortouchstart"] = {};
	}
};

const assignThunk = (k, thunk, eventName, state) => {
	if (eventName === "mousedown") {
		state.listeners["upformousedown"][k] = thunk;
	} else if (eventName === "touchstart") {
		state.listeners["endfortouchstart"][k] = thunk.end;
		state.listeners["cancelfortouchstart"][k] = thunk.cancel;
	}
};

export const makeRenderPass_ = (a) => (state) => {
	const pass = new a.renderPass(
		state.units[a.scene].main,
		state.units[a.camera].main
	);
	state.units[a.id] = { main: pass };
	if (a.parent !== undefined) {
		state.units[a.parent].main.addPass(pass);
	}
};

export const makeRaycaster_ = (a) => (state) => {
	setUpForRaycasting(a, state);
}

export const makeGlitchPass_ = (a) => (state) => {
	const pass = new a.glitchPass(a.dtSize);
	state.units[a.id] = { main: pass };
	if (a.parent !== undefined) {
		state.units[a.parent].main.addPass(pass);
	}
};

export const makeEffectComposerPass_ = (a) => (state) => {
	const pass = new a.effectComposerPass(state.units[a.effectComposer].main);
	state.units[a.id] = { main: pass };
	if (a.parent !== undefined) {
		state.units[a.parent].main.addPass(pass);
	}
}

export const makeBloomPass_ = (a) => (state) => {
	const pass = new a.bloomPass(
		a.strength,
		a.kernelSize,
		a.sigma,
		a.resolution
	);
	state.units[a.id] = { main: pass };
	if (a.parent !== undefined) {
		state.units[a.parent].main.addPass(pass);
	}
};
export const makeUnrealBloomPass_ = (a) => (state) => {
	const pass = new a.unrealBloomPass(a.resolution, a.strength, a.radius, a.threshold);
	state.units[a.id] = { main: pass };
	if (a.parent !== undefined) {
		state.units[a.parent].main.addPass(pass);
	}
};
export const makeEffectComposer_ = (a) => (state) => {
	const myId = a.id;
	const effectComposer = new a.effectComposer(state.units[a.webGLRenderer].main);
	state.units[myId] = {
		main: effectComposer,
	};
};
const setUpForRaycasting = (parameters, state) => {
	const raycaster = new parameters.raycaster();
	const camera = state.units[parameters.camera].main;

	const makeListener = (eventName) => {
		parameters.canvas.addEventListener(eventName, ($e) => {
			const entries = Object.entries(state.listeners[eventName]);
			const es =
				eventName.indexOf("touch") !== -1 ? getAllTouches($e.touches) : [$e];
			// todo: finesse for touches, some multi touch interactions will
			// break this
			doFinalThunk($e, eventName, state);
			if (entries.length > 0) {
				es.forEach((e) => {
					const x = (e.clientX / window.innerWidth) * 2 - 1;
					const y = -(e.clientY / window.innerHeight) * 2 + 1;
					raycaster.setFromCamera({ x, y }, camera);
					entries.forEach(([k, v]) => {
						const u = state.units[k].main;
						const intersects = raycaster.intersectObject(u);
						if (intersects.length > 0) {
							const thunk = v(e)();
							assignThunk(k, thunk, eventName, state);
						}
					});
				});
			}
			/////////// instanced code
			const instancedEntries = Object.entries(
				state.listeners[eventName + "Instanced"]
			);
			if (instancedEntries.length > 0) {
				es.forEach((e) => {
					const x = (e.clientX / window.innerWidth) * 2 - 1;
					const y = -(e.clientY / window.innerHeight) * 2 + 1;
					raycaster.setFromCamera({ x, y }, camera);
					instancedEntries.forEach(([k, v]) => {
						const u = state.units[k].main;
						Object.entries(v).forEach(([kk, vv]) => {
							const intersects = intersectInstance(raycaster, u, [kk]);
							if (intersects.length > 0) {
								const thunk = vv(e)();
								assignThunk(kk, thunk, eventName, state);
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

export const makeWebGLRendererInternal_ = (a, state) => {
	const { id, ...parameters } = a;
	const canvas = parameters.canvas;
	const renderer = new a.webGLRenderer(parameters);
	state.units[a.id] = { main: renderer };
	renderer.setSize(canvas.width, canvas.height);
	renderer.setPixelRatio(Math.min(window.devicePixelRatio, 2));
};
export const makeWebGLRenderer_ = (a) => (state) => {
	makeWebGLRendererInternal_(a, state);
};
export const makeCSS2DRenderer_ = (a) => (state) => {
	const { id, canvas, element } = a;
	const renderer = new a.css2DRenderer({ element });
	renderer.setSize(canvas.offsetWidth, canvas.offsetHeight);
	state.units[id] = { main: renderer };
};
export const makeCSS3DRenderer_ = (a) => (state) => {
	const { id, canvas, element } = a;
	const renderer = new a.css3DRenderer({ element });
	renderer.setSize(canvas.offsetWidth, canvas.offsetHeight);
	state.units[id] = { main: renderer };
};
export const setOnClick_ = (a) => (state) => {
	state.listeners.click[a.id] = a.onClick;
};
export const setOnMouseDown_ = (a) => (state) => {
	state.listeners.mousedown[a.id] = a.onMouseDown;
};
export const setOnMouseUp_ = (a) => (state) => {
	state.listeners.mouseup[a.id] = a.onMouseUp;
};
export const setOnMouseMove_ = (a) => (state) => {
	state.listeners.mousemove[a.id] = a.onMouseMove;
};
export const setOnTouchStart_ = (a) => (state) => {
	state.listeners.touchstart[a.id] = a.onTouchStart;
};
export const setOnTouchEnd_ = (a) => (state) => {
	state.listeners.touchend[a.id] = a.onTouchEnd;
};
export const setOnTouchMove_ = (a) => (state) => {
	state.listeners.touchmove[a.id] = a.onTouchMove;
};
export const setOnTouchCancel_ = (a) => (state) => {
	state.listeners.touchcancel[a.id] = a.onTouchCancel;
};
export const removeOnClick_ = (a) => (state) => {
	delete state.listeners.click[a.id];
};
export const removeOnMouseDown_ = (a) => (state) => {
	delete state.listeners.mousedown[a.id];
};
export const removeOnMouseUp_ = (a) => (state) => {
	delete state.listeners.mouseup[a.id];
};
export const removeOnMouseMove_ = (a) => (state) => {
	delete state.listeners.mousemove[a.id];
};
export const removeOnTouchStart_ = (a) => (state) => {
	delete state.listeners.touchstart[a.id];
};
export const removeOnTouchEnd_ = (a) => (state) => {
	delete state.listeners.touchend[a.id];
};
export const removeOnTouchMove_ = (a) => (state) => {
	delete state.listeners.touchmove[a.id];
};
export const removeOnTouchCancel_ = (a) => (state) => {
	delete state.listeners.touchcancel[a.id];
};
export const setIMOnClick_ = (a) => (state) => {
	if (!state.listeners.clickInstanced[a.id]) {
		state.listeners.clickInstanced[a.id] = {};
	}
	state.listeners.clickInstanced[a.id][a.instanceId] = a.onClick;
};
export const setIMOnMouseDown_ = (a) => (state) => {
	if (!state.listeners.mousedownInstanced[a.id]) {
		state.listeners.mousedownInstanced[a.id] = {};
	}
	state.listeners.mousedownInstanced[a.id][a.instanceId] = a.onMouseDown;
};
export const setIMOnMouseUp_ = (a) => (state) => {
	if (!state.listeners.mouseupInstanced[a.id]) {
		state.listeners.mouseupInstanced[a.id] = {};
	}
	state.listeners.mouseupInstanced[a.id][a.instanceId] = a.onMouseUp;
};
export const setIMOnMouseMove_ = (a) => (state) => {
	if (!state.listeners.mousemoveInstanced[a.id]) {
		state.listeners.mousemoveInstanced[a.id] = {};
	}
	state.listeners.mousemoveInstanced[a.id][a.instanceId] = a.onMouseMove;
};
export const setIMOnTouchStart_ = (a) => (state) => {
	if (!state.listeners.touchstartInstanced[a.id]) {
		state.listeners.touchstartInstanced[a.id] = {};
	}
	state.listeners.touchstartInstanced[a.id][a.instanceId] = a.onTouchStart;
};
export const setIMOnTouchEnd_ = (a) => (state) => {
	if (!state.listeners.touchendInstanced[a.id]) {
		state.listeners.touchendInstanced[a.id] = {};
	}
	state.listeners.touchendInstanced[a.id][a.instanceId] = a.onTouchEnd;
};
export const setIMOnTouchMove_ = (a) => (state) => {
	if (!state.listeners.touchmoveInstanced[a.id]) {
		state.listeners.touchmoveInstanced[a.id] = {};
	}
	state.listeners.touchmoveInstanced[a.id][a.instanceId] = a.onTouchMove;
};
export const setIMOnTouchCancel_ = (a) => (state) => {
	if (!state.listeners.touchcancelInstanced[a.id]) {
		state.listeners.touchcancelInstanced[a.id] = {};
	}
	state.listeners.touchcancelInstanced[a.id][a.instanceId] = a.onTouchCancel;
};
export const removeIMOnClick_ = (a) => (state) => {
	delete state.listeners.clickInstanced[a.id][a.instanceId];
};
export const removeIMOnMouseDown_ = (a) => (state) => {
	delete state.listeners.mousedownInstanced[a.id][a.instanceId];
};
export const removeIMOnMouseUp_ = (a) => (state) => {
	delete state.listeners.mouseupInstanced[a.id][a.instanceId];
};
export const removeIMOnMouseMove_ = (a) => (state) => {
	delete state.listeners.mousemoveInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchStart_ = (a) => (state) => {
	delete state.listeners.touchstartInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchEnd_ = (a) => (state) => {
	delete state.listeners.touchendInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchMove_ = (a) => (state) => {
	delete state.listeners.touchmoveInstanced[a.id][a.instanceId];
};
export const removeIMOnTouchCancel_ = (a) => (state) => {
	delete state.listeners.touchcancelInstanced[a.id][a.instanceId];
};
// box
export const setWidth_ = (a) => (state) => {
	state.units[a.id].main.width = a.width;
};
export const setHeight_ = (a) => (state) => {
	state.units[a.id].main.height = a.height;
};
export const setDepth_ = (a) => (state) => {
	state.units[a.id].main.depth = a.depth;
};
export const setLength_ = (a) => (state) => {
	state.units[a.id].main.length = a.length;
};
// material
export const setAlphaTest_ = (a) => (state) => {
	state.units[a.id].main.alphaTest = a.alphaTest;
};

export const setAlphaToCoverage_ = (a) => (state) => {
	state.units[a.id].main.alphaToCoverage = a.alphaToCoverage;
};

export const setBlendDst_ = (a) => (state) => {
	state.units[a.id].main.blendDst = a.blendDst;
};

export const setBlendDstAlpha_ = (a) => (state) => {
	state.units[a.id].main.blendDstAlpha = a.blendDstAlpha;
};

export const setBlendEquation_ = (a) => (state) => {
	state.units[a.id].main.blendEquation = a.blendEquation;
};

export const setBlendEquationAlpha_ = (a) => (state) => {
	state.units[a.id].main.blendEquationAlpha = a.blendEquationAlpha;
};

export const setBlending_ = (a) => (state) => {
	state.units[a.id].main.blending = a.blending;
};

export const setBlendSrc_ = (a) => (state) => {
	state.units[a.id].main.blendSrc = a.blendSrc;
};

export const setBlendSrcAlpha_ = (a) => (state) => {
	state.units[a.id].main.blendSrcAlpha = a.blendSrcAlpha;
};

export const setClipIntersection_ = (a) => (state) => {
	state.units[a.id].main.clipIntersection = a.clipIntersection;
};

export const setClipShadows_ = (a) => (state) => {
	state.units[a.id].main.clipShadows = a.clipShadows;
};

export const setColorWrite_ = (a) => (state) => {
	state.units[a.id].main.colorWrite = a.colorWrite;
};

export const setDepthFunc_ = (a) => (state) => {
	state.units[a.id].main.depthFunc = a.depthFunc;
};

export const setDepthTest_ = (a) => (state) => {
	state.units[a.id].main.depthTest = a.depthTest;
};

export const setDepthWrite_ = (a) => (state) => {
	state.units[a.id].main.depthWrite = a.depthWrite;
};

export const setOpacity_ = (a) => (state) => {
	state.units[a.id].main.opacity = a.opacity;
};

export const setPolygonOffset_ = (a) => (state) => {
	state.units[a.id].main.polygonOffset = a.polygonOffset;
};

export const setPolygonOffsetFactor_ = (a) => (state) => {
	state.units[a.id].main.polygonOffsetFactor = a.polygonOffsetFactor;
};

export const setPolygonOffsetUnits_ = (a) => (state) => {
	state.units[a.id].main.polygonOffsetUnits = a.polygonOffsetUnits;
};

export const setPrecision_ = (a) => (state) => {
	state.units[a.id].main.precision = a.precision;
};

export const setPremultipliedAlpha_ = (a) => (state) => {
	state.units[a.id].main.premultipliedAlpha = a.premultipliedAlpha;
};

export const setDithering_ = (a) => (state) => {
	state.units[a.id].main.dithering = a.dithering;
};

export const setShadowSide_ = (a) => (state) => {
	state.units[a.id].main.shadowSide = a.shadowSide;
};

export const setSide_ = (a) => (state) => {
	state.units[a.id].main.side = a.side;
};

export const setToneMapped_ = (a) => (state) => {
	state.units[a.id].main.toneMapped = a.toneMapped;
};

export const setTransparent_ = (a) => (state) => {
	state.units[a.id].main.transparent = a.transparent;
};

export const setVertexColors_ = (a) => (state) => {
	state.units[a.id].main.vertexColors = a.vertexColors;
};

export const setVisible_ = (a) => (state) => {
	state.units[a.id].main.visible = a.visible;
};

// scene (background)
export const setBackgroundColor_ = (a) => (state) => {
	state.units[a.id].main.background = a.color;
};
export const setBackgroundTexture_ = (a) => (state) => {
	state.units[a.id].main.background = a.texture;
};
export const setBackgroundCubeTexture_ = (a) => (state) => {
	state.units[a.id].main.background = a.cubeTexture;
};
// unrealBloom
export const setThreshold_ = (a) => (state) => {
	state.units[a.id].main.threshold = a.threshold;
};
export const setStrength_ = (a) => (state) => {
	state.units[a.id].main.strength = a.strength;
};
export const setResolution_ = (a) => (state) => {
	state.units[a.id].main.resolution = a.resolution;
};
// sphere
export const setRadius_ = (a) => (state) => {
	state.units[a.id].main.radius = a.radius;
};
export const setWidthSegments_ = (a) => (state) => {
	state.units[a.id].main.widthSegments = a.widthSegments;
};
export const setHeightSegments_ = (a) => (state) => {
	state.units[a.id].main.heightSegments = a.heightSegments;
};
export const setDepthSegments_ = (a) => (state) => {
	state.units[a.id].main.depthSegments = a.depthSegments;
};
export const setCapSegments_ = (a) => (state) => {
	state.units[a.id].main.capSegments = a.capSegments;
};
export const setRadialSegments_ = (a) => (state) => {
	state.units[a.id].main.radialSegments = a.radialSegments;
};
export const setPhiStart_ = (a) => (state) => {
	state.units[a.id].main.phiStart = a.phiStart;
};
export const setPhiLength_ = (a) => (state) => {
	state.units[a.id].main.phiLength = a.phiLength;
};
export const setThetaStart_ = (a) => (state) => {
	state.units[a.id].main.thetaStart = a.thetaStart;
};
export const setThetaLength_ = (a) => (state) => {
	state.units[a.id].main.thetaLength = a.thetaLength;
};
// bufferGeometry
export const setMatrix4_ = (a) => (state) => {
	state.units[a.id].main.applyMatrix4(a.matrix4);
};
export const setQuaternion_ = (a) => (state) => {
	state.units[a.id].main.applyQuaternion(a.quaternion);
};
export const setRotateX_ = (a) => (state) => {
	state.units[a.id].main.rotateX(a.rotateX);
};
export const setRotateY_ = (a) => (state) => {
	state.units[a.id].main.rotateY(a.rotateY);
};
export const setRotateZ_ = (a) => (state) => {
	state.units[a.id].main.rotateZ(a.rotateZ);
};
export const setTranslate_ = (a) => (state) => {
	state.units[a.id].main.translate(a.x, a.y, a.z);
};
export const setScale_ = (a) => (state) => {
	state.units[a.id].main.scale(a.x, a.y, a.z);
};
export const setScaleX_ = (a) => (state) => {
	state.units[a.id].main.scale.x = a.scaleX;
};
export const setScaleY_ = (a) => (state) => {
	state.units[a.id].main.scale.y = a.scaleY;
};
export const setScaleZ_ = (a) => (state) => {
	state.units[a.id].main.scale.z = a.scaleZ;
};
export const setLookAt_ = (a) => (state) => {
	state.units[a.id].main.lookAt(a.v);
};
export const setCenter_ = (a) => (state) => {
	state.units[a.id].main.center();
};
export const getBoundingBox_ = (a) => (state) => {
	state.units[a.id].main.computeBoundingBox();
	a.box(state.units[a.id].main.boundingBox)();
};
export const getBoundingSphere_ = (a) => (state) => {
	state.units[a.id].main.computeBoundingSphere();
	a.box(state.units[a.id].main.boundingSphere)();
};
// instanced mesh
export const setInstancedMeshMatrix4_ = (a) => (state) => {
	const u = state.units[a.id].main;
	let updated = false;
	a.setMatrix4((i, m) => {
		updated = true;
		u.setMatrixAt(i, m);
	});
	u.instanceMatrix.needsUpdate = updated;
};
export const setInstancedMeshColor_ = (a) => (state) => {
	const u = state.units[a.id].main;
	let updated = false;
	a.setColor((i, c) => {
		updated = true;
		u.setColorAt(i, c);
	});
	u.instanceColor.needsUpdate = updated;
};
export const setSingleInstancedMeshMatrix4_ = (a) => (state) => {
	const u = state.units[a.id].main;
	u.setMatrixAt(a.instanceId, a.matrix4);
	u.instanceMatrix.needsUpdate = true;
};
export const setSingleInstancedMeshColor_ = (a) => (state) => {
	const u = state.units[a.id].main;
	u.setColorAt(a.instanceId, a.color);
	u.instanceColor.needsUpdate = true;
};
// mesh phong material
export const setCombine_ = (a) => (state) => {
	state.units[a.id].main.combine = a.combine;
};
export const setFog_ = (a) => (state) => {
	state.units[a.id].main.fog = a.fog;
};
export const setReflectivity_ = (a) => (state) => {
	state.units[a.id].main.reflectivity = a.reflectivity;
};
export const setRefractionRatio_ = (a) => (state) => {
	state.units[a.id].main.refractionRatio = a.refractionRatio;
};
export const setShininess_ = (a) => (state) => {
	state.units[a.id].main.shininess = a.shininess;
};
export const setSpecular_ = (a) => (state) => {
	state.units[a.id].main.specular = a.specular;
};
export const setSpecularMap_ = (a) => (state) => {
	state.units[a.id].main.specularMap = a.specularMap;
};
export const setWireframeLinecap_ = (a) => (state) => {
	state.units[a.id].main.wireframeLinecap = a.wireframeLinecap;
};
export const setWireframeLinejoin_ = (a) => (state) => {
	state.units[a.id].main.wireframeLinejoin = a.wireframeLinejoin;
};
// mesh standard material
export const setColor_ = (a) => (state) => {
	state.units[a.id].main.color = a.color;
};
export const setRoughness_ = (a) => (state) => {
	state.units[a.id].main.roughness = a.roughness;
};
export const setMetalness_ = (a) => (state) => {
	state.units[a.id].main.metalness = a.metalness;
};
export const setMap_ = (a) => (state) => {
	state.units[a.id].main.map = a.map;
};
export const setLightMap_ = (a) => (state) => {
	state.units[a.id].main.lightMap = a.lightMap;
};
export const setLightMapIntensity_ = (a) => (state) => {
	state.units[a.id].main.lightMapIntensity = a.lightMapIntensity;
};
export const setAoMap_ = (a) => (state) => {
	state.units[a.id].main.aoMap = a.aoMap;
};
export const setAoMapIntensity_ = (a) => (state) => {
	state.units[a.id].main.aoMapIntensity = a.aoMapIntensity;
};
export const setEmissive_ = (a) => (state) => {
	state.units[a.id].main.emissive = a.emissive;
};
export const setEmissiveIntensity_ = (a) => (state) => {
	state.units[a.id].main.emissiveIntensity = a.emissiveIntensity;
};
export const setEmissiveMap_ = (a) => (state) => {
	state.units[a.id].main.emissiveMap = a.emissiveMap;
};
export const setBumpMap_ = (a) => (state) => {
	state.units[a.id].main.bumpMap = a.bumpMap;
};
export const setBumpScale_ = (a) => (state) => {
	state.units[a.id].main.bumpScale = a.bumpScale;
};
export const setNormalMap_ = (a) => (state) => {
	state.units[a.id].main.normalMap = a.normalMap;
};
export const setNormalMapType_ = (a) => (state) => {
	state.units[a.id].main.normalMapType = a.normalMapType;
};
export const setNormalScale_ = (a) => (state) => {
	state.units[a.id].main.normalScale = a.normalScale;
};
export const setDisplacementMap_ = (a) => (state) => {
	state.units[a.id].main.displacementMap = a.displacementMap;
};
export const setDisplacementScale_ = (a) => (state) => {
	state.units[a.id].main.displacementScale = a.displacementScale;
};
export const setDisplacementBias_ = (a) => (state) => {
	state.units[a.id].main.displacementBias = a.displacementBias;
};
export const setRoughnessMap_ = (a) => (state) => {
	state.units[a.id].main.roughnessMap = a.roughnessMap;
};
export const setMetalnessMap_ = (a) => (state) => {
	state.units[a.id].main.metalnessMap = a.metalnessMap;
};
export const setAlphaMap_ = (a) => (state) => {
	state.units[a.id].main.alphaMap = a.alphaMap;
};
export const setEnvMap_ = (a) => (state) => {
	state.units[a.id].main.envMap = a.envMap;
};
export const setEnvMapIntensity_ = (a) => (state) => {
	state.units[a.id].main.envMapIntensity = a.envMapIntensity;
};
export const setWireframe_ = (a) => (state) => {
	state.units[a.id].main.wireframe = a.wireframe;
};
export const setWireframeLinewidth_ = (a) => (state) => {
	state.units[a.id].main.wireframeLinewidth = a.wireframeLinewidth;
};
export const setFlatShading_ = (a) => (state) => {
	state.units[a.id].main.flatShading = a.flatShading;
};
// point light
export const setDistance_ = (a) => (state) => {
	state.units[a.id].main.distance = a.distance;
};
export const setDecay_ = (a) => (state) => {
	state.units[a.id].main.decay = a.decay;
};
export const setIntensity_ = (a) => (state) => {
	state.units[a.id].main.intensity = a.intensity;
};
// mesh
export const setRotationFromAxisAngle_ = (a) => (state) => {
	state.units[a.id].main.setRotationFromAxisAngle(a.axis, a.angle);
};
export const setRotationFromEuler_ = (a) => (state) => {
	state.units[a.id].main.setRotationFromEuler(a.euler);
};
export const setRotationFromMatrix_ = (a) => (state) => {
	state.units[a.id].main.setRotationFromMatrix(a.matrix4);
};
export const setRotationFromQuaternion_ = (a) => (state) => {
	state.units[a.id].main.setRotationFromQuaternion(a.quaternion);
};
export const setRotateOnAxis_ = (a) => (state) => {
	state.units[a.id].main.rotateOnAxis(a.axis, a.angle);
};
export const setRotateOnWorldAxis_ = (a) => (state) => {
	state.units[a.id].main.rotateOnWorldAxis(a.axis, a.angle);
};
export const setTranslateOnAxis_ = (a) => (state) => {
	state.units[a.id].main.translateOnAxis(a.axis, a.distance);
};
export const setTranslateX_ = (a) => (state) => {
	state.units[a.id].main.translateX(a.translateX);
};
export const setTranslateY_ = (a) => (state) => {
	state.units[a.id].main.translateY(a.translateY);
};
export const setTranslateZ_ = (a) => (state) => {
	state.units[a.id].main.translateZ(a.translateZ);
};
export const setPositionX_ = (a) => (state) => {
	state.units[a.id].main.position.x = a.positionX;
};
export const setPositionY_ = (a) => (state) => {
	state.units[a.id].main.position.y = a.positionY;
};
export const setPositionZ_ = (a) => (state) => {
	state.units[a.id].main.position.z = a.positionZ;
};
// uniform
export const setUniform_ = (a) => (state) => {
	state.units[a.id].main.uniforms[a.key].value = a.value;
};
// perspective camera
export const setAspect_ = (a) => (state) => {
	state.units[a.id].main.aspect = a.aspect;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFar_ = (a) => (state) => {
	state.units[a.id].main.far = a.far;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFilmGauge_ = (a) => (state) => {
	state.units[a.id].main.filmGauge = a.filmGauge;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFilmOffset_ = (a) => (state) => {
	state.units[a.id].main.filmOffset = a.filmOffset;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFocus_ = (a) => (state) => {
	state.units[a.id].main.focus = a.focus;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFov_ = (a) => (state) => {
	state.units[a.id].main.fov = a.fov;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setNear_ = (a) => (state) => {
	state.units[a.id].main.updateProjectionMatrix();
	state.units[a.id].main.near = a.near;
};
export const setZoom_ = (a) => (state) => {
	state.units[a.id].main.zoom = a.zoom;
	state.units[a.id].main.updateProjectionMatrix();
};
export const setFocalLength_ = (a) => (state) => {
	state.units[a.id].main.setFocalLength(a.focalLength);
};
export const setViewOffset_ =
	({ id, fullWidth, fullHeight, x, y, width, height }) =>
		(state) => {
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
export const makeFFIThreeSnapshot = () => {
	return {
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
			upformousedown: {},
			endfortouchstart: {},
			cancelfortouchstart: {},
		},
	};
};

export const disconnect_ = (a) => (state) => {
	const ptr = a.id;
	// check to make sure this actually works
	state.units[ptr].main.removeFromParent();
	if (a.scope !== GLOBAL_SCOPE) {
		if (
			state.units[ptr].main.isBufferGeometry ||
			state.units[ptr].main.isMaterial
		) {
			state.units[ptr].main.dispose();
		}
	}
};

export const disconnectPass_ = (a) => (state) => {
	const ptr = a.id;
	const parent = a.parent;
	state.units[parent].main.removePass(state.units[ptr].main);
};

export const deleteFromCache_ = (a) => (state) => {
	if (!state.units[a.id].main.isScene && state.units[a.id].main.dispose) {
		state.units[a.id].main.dispose();
	}
	if (state.units[a.id].main.removeFromParent) {
		state.units[a.id].main.removeFromParent();
	}
	delete state.units[a.id];
};
