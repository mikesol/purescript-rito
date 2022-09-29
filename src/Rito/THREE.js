export const css2DRenderer = () =>
	import("three/examples/jsm/renderers/CSS2DRenderer.js").then(
		(r) => r.CSS2DRenderer
	);

export const css3DRenderer = () =>
	import("three/examples/jsm/renderers/CSS3DRenderer.js").then(
		(r) => r.CSS3DRenderer
	);

export const css2DObject = () =>
	import("three/examples/jsm/renderers/CSS2DRenderer.js").then(
		(r) => r.CSS2DObject
	);

export const css3DObject = () =>
	import("three/examples/jsm/renderers/CSS3DRenderer.js").then(
		(r) => r.CSS3DObject
	);

export const vector2 = () =>
	import("three/src/math/Vector2.js").then((r) => r.Vector2);

export const vector3 = () =>
	import("three/src/math/Vector3.js").then((r) => r.Vector3);

export const textureLoader = () =>
	import("three/src/loaders/TextureLoader.js").then((r) => r.TextureLoader);

export const gltfLoader = () =>
	import("three/examples/jsm/loaders/GLTFLoader.js").then((r) => r.GLTFLoader);

export const cubeTextureLoader = () =>
	import("three/src/loaders/CubeTextureLoader.js").then(
		(r) => r.CubeTextureLoader
	);

export const sphere = () =>
	import("three/src/math/Sphere.js").then((r) => r.Sphere);

export const quaternion = () =>
	import("three/src/math/Quaternion.js").then((r) => r.Quaternion);

export const euler = () =>
	import("three/src/math/Euler.js").then((r) => r.Euler);

export const matrix4 = () =>
	import("three/src/math/Matrix4.js").then((r) => r.Matrix4);

export const color = () =>
	import("three/src/math/Color.js").then((r) => r.Color);

export const box3 = () => import("three/src/math/Box3.js").then((r) => r.Box3);

export const scene = () =>
	import("three/src/scenes/Scene.js").then((r) => r.Scene);

export const group = () =>
	import("three/src/objects/Group.js").then((r) => r.Group);

export const webGLRenderer = () =>
	import("three/src/renderers/WebGLRenderer.js").then((r) => r.WebGLRenderer);

export const mesh = () =>
	import("three/src/objects/Mesh.js").then((r) => r.Mesh);

export const instancedMesh = () =>
	import("three/src/objects/InstancedMesh.js").then((r) => r.InstancedMesh);

export const meshStandardMaterial = () =>
	import("three/src/materials/MeshStandardMaterial.js").then(
		(r) => r.MeshStandardMaterial
	);

export const meshLambertMaterial = () =>
	import("three/src/materials/MeshLambertMaterial.js").then(
		(r) => r.MeshLambertMaterial
	);
export const meshBasicMaterial = () =>
	import("three/src/materials/MeshBasicMaterial.js").then(
		(r) => r.MeshBasicMaterial
	);

export const pointLight = () =>
	import("three/src/lights/PointLight.js").then((r) => r.PointLight);

export const directionalLight = () =>
	import("three/src/lights/DirectionalLight.js").then(
		(r) => r.DirectionalLight
	);

export const ambientLight = () =>
	import("three/src/lights/AmbientLight.js").then((r) => r.AmbientLight);

export const fogExp2 = () =>
	import("three/src/scenes/FogExp2.js").then((r) => r.FogExp2);

export const sphereGeometry = () =>
	import("three/src/geometries/SphereGeometry.js").then(
		(r) => r.SphereGeometry
	);

export const boxGeometry = () =>
	import("three/src/geometries/BoxGeometry.js").then((r) => r.BoxGeometry);

export const cylinderGeometry = () =>
	import("three/src/geometries/CylinderGeometry.js").then((r) => r.CylinderGeometry);

export const capsuleGeometry = () =>
	import("three/src/geometries/CapsuleGeometry.js").then(
		(r) => r.CapsuleGeometry
	);

export const planeGeometry = () =>
	import("three/src/geometries/PlaneGeometry.js").then((r) => r.PlaneGeometry);

export const bufferGeometry = () =>
	import("three/src/core/BufferGeometry.js").then((r) => r.BufferGeometry);

export const perspectiveCamera = () =>
	import("three/src/cameras/PerspectiveCamera.js").then(
		(r) => r.PerspectiveCamera
	);

export const raycaster = () =>
	import("three/src/core/Raycaster.js").then((r) => r.Raycaster);

export const shaderMaterial = () =>
	import("three/src/materials/ShaderMaterial.js").then((r) => r.ShaderMaterial);

export const rawShaderMaterial = () =>
	import("three/src/materials/RawShaderMaterial.js").then(
		(r) => r.RawShaderMaterial
	);

export const bufferAttribute = () =>
	import("three/src/core/BufferAttribute.js").then((r) => r.BufferAttribute);

export const instancedBufferAttribute = () =>
	import("three/src/core/InstancedBufferAttribute.js").then(
		(r) => r.InstancedBufferAttribute
	);

export const points = () =>
	import("three/src/objects/Points.js").then((r) => r.Points);

export const meshPhongMaterial = () =>
	import("three/src/materials/MeshPhongMaterial.js").then(
		(r) => r.MeshPhongMaterial
	);

export const effectComposer = () =>
	import("three/examples/jsm/postprocessing/EffectComposer.js").then(
		(r) => r.EffectComposer
	);

export const renderPass = () =>
	import("three/examples/jsm/postprocessing/RenderPass.js").then(
		(r) => r.RenderPass
	);

export const glitchPass = () =>
	import("three/examples/jsm/postprocessing/GlitchPass.js").then(
		(r) => r.GlitchPass
	);

export const bloomPass = () =>
	import("three/examples/jsm/postprocessing/BloomPass.js").then(
		(r) => r.BloomPass
	);

export const unrealBloomPass = () =>
	import("three/examples/jsm/postprocessing/UnrealBloomPass.js").then(
		(r) => r.UnrealBloomPass
	);

export const effectComposerPass = () =>
	Promise.all([
		import("three/examples/jsm/postprocessing/ShaderPass.js"),
		import("three/src/materials/ShaderMaterial.js"),
	]).then(([sp, sm]) => {
		class EffectComposerPass extends sp.ShaderPass {
			constructor(effectComposer) {
				super(
					new sm.ShaderMaterial({
						uniforms: {
							tDiffuse: { value: null },
							incomingTexture: {
								value: effectComposer.renderTarget2.texture,
							},
						},

						vertexShader: /* glsl */ `
		varying vec2 vUv;
		void main() {
			vUv = uv;
			gl_Position = projectionMatrix * modelViewMatrix * vec4( position, 1.0 );
		}`,

						fragmentShader: /* glsl */ `
		uniform sampler2D tDiffuse;
    uniform sampler2D incomingTexture;

    varying vec2 vUv;

    void main() {

      gl_FragColor = ( texture2D( tDiffuse, vUv ) + texture2D( incomingTexture, vUv ) );

    }`,
					})
				);
				this.effectComposer = effectComposer;
			}
			render(renderer, writeBuffer, readBuffer, deltaTime, maskActive) {
				this.effectComposer.renderToScreen = false;
				this.effectComposer.render();
				const buffy = this.effectComposer.renderTarget2.texture;
				this.uniforms.incomingTexture.value = buffy;
				super.render(renderer, writeBuffer, readBuffer, deltaTime, maskActive);
			}
		}
		return EffectComposerPass;
	});
