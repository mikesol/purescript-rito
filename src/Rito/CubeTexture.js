export const loader = (cubeTextureLoader) => () => new cubeTextureLoader();

export const load = (loader) => (urls) => (onLoad) => (onError) => () => {
	loader.load(
		urls,
		(x) => onLoad(x)(),
		undefined,
		(x) => onError(x)()
	);
};
