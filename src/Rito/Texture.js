export const loader = (textureLoader) => () => new textureLoader();

export const load = (loader) => (url) => (onLoad) => (onError) => () => {
	loader.load(
		url,
		(x) => onLoad(x)(),
		undefined,
		(x) => onError(x)()
	);
};
