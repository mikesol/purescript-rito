import * as THREE from "three";

const loader = new THREE.TextureLoader();

export const load = (url) => (onLoad) => (onError) => () => {
	loader.load(
		url,
		(x) => onLoad(x)(),
		undefined,
		(x) => onError(x)()
	);
};
