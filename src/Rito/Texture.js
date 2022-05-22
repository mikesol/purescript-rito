export const loader = (THREE) => () => THREE.TextureLoader();

export const load = (loader) => (url) => (onLoad) => (onError) => () => {
	loader.load(
		url,
		(x) => onLoad(x)(),
		undefined,
		(x) => onError(x)()
	);
};
