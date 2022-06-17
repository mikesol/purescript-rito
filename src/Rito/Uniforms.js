export const toUniformDeclImpl = (i) => {
	const o = { ...i };
	Object.keys(o).forEach((k) => {
		o[k] = { value: o[k] };
	});
	return o;
};
