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

export const cubeTextureLoader = () =>
	import("three/src/loaders/CubeTextureLoader.js").then(
		(r) => r.CubeTextureLoader
	);

export const sphere = () =>
	import("three/src/math/Sphere.js").then((r) => r.Sphere);

export const quaternion = () =>
	import("three/src/math/Quaternion.js").then((r) => r.Quaternion);

export const matrix4 = () =>
	import("three/src/math/Matrix4.js").then((r) => r.Matrix4);

export const color = () => import("three/src/math/Color.js").then((r) => r.Color);

export const box3 = () => import("three/src/math/Box3.js").then((r) => r.Box3);

export const scene = () => import("three/src/scenes/Scene.js").then((r) => r.Scene);

export const group = () => import("three/src/objects/Group.js").then((r) => r.Group);

export const webGLRenderer = () => import("three/src/renderers/WebGLRenderer.js").then((r) => r.WebGLRenderer);

export const mesh = () => import("three/src/objects/Mesh.js").then((r) => r.Mesh);

export const instancedMesh = () => import("three/src/objects/InstancedMesh.js").then((r) => r.InstancedMesh);

export const meshStandardMaterial = () => import("three/src/materials/MeshStandardMaterial.js").then((r) => r.MeshStandardMaterial);

export const meshBasicMaterial = () => import("three/src/materials/MeshBasicMaterial.js").then((r) => r.MeshBasicMaterial);

export const pointLight = () => import("three/src/lights/PointLight.js").then((r) => r.PointLight);

export const directionalLight = () => import("three/src/lights/DirectionalLight.js").then((r) => r.DirectionalLight);

export const ambientLight = () => import("three/src/lights/AmbientLight.js").then((r) => r.AmbientLight);

export const sphereGeometry = () => import("three/src/geometries/SphereGeometry.js").then((r) => r.SphereGeometry);

export const boxGeometry = () => import("three/src/geometries/BoxGeometry.js").then((r) => r.BoxGeometry);

export const capsuleGeometry = () => import("three/src/geometries/CapsuleGeometry.js").then((r) => r.CapsuleGeometry);

export const planeGeometry = () => import("three/src/geometries/PlaneGeometry.js").then((r) => r.PlaneGeometry);

export const perspectiveCamera = () => import("three/src/cameras/PerspectiveCamera.js").then((r) => r.PerspectiveCamera);

export const raycaster = () => import("three/src/core/Raycaster.js").then((r) => r.Raycaster);