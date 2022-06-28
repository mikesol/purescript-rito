export const loader = (gltfLoader) => () => new gltfLoader();

export const load = (loader) => (url) => (onLoad) => (onError) => () => {
	loader.load(
		url,
		(x) => onLoad(x)(),
		undefined,
		(x) => onError(x)()
	);
};

//export const assetImpl = (gltf) => gltf.asset;
//export const camerasImpl = (gltf) => gltf.cameras.map((x) => x.clone());
export const sceneImpl = (gltf) => gltf.scene.clone();
//export const scenesImpl = (gltf) => gltf.scenes.map((x) => x.clone());
