export const loader = (THREE) => () => new THREE.CubeTextureLoader();

export const load = (loader) => (urls) => (onLoad) => (onError) => () => {
	loader.load(
		urls,
		(x) => onLoad(x)(),
		undefined,
		(x) => onError(x)()
	);
};
