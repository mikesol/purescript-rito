export const bufferAttributeImpl = (m) => (n) => (bufferAttribute) => (f) => {
	const fa = new Float32Array(m * n);
	for (let i = 0; i < m; i++) {
		const o = i * n;
		const v = f(i);
		for (let j = 0; j < n; j++) {
			fa[o + j] = v[j];
		}
	}
	return new bufferAttribute(fa, n);
};

export const bufferAttributesImpl = (m) => (ns) => (bufferAttribute) => (a) => (f) => {
	const fas = {};
	for (let i = 0; i < ns.length; i++) {
		fas[ns[i].k] = new Float32Array(m * ns[i].n);
	}
	let $a = a;
	for (let i = 0; i < m; i++) {
		const $v = f(i)($a);
		const v = $v.l;
		$a = $v.r;
		for (let j = 0; j < ns.length; j++) {
			const key = ns[j].k;
			const n = ns[j].n;
			const o = i * n;
			for (let k = 0; k < n; k++) {
				fas[key][o + k] = v[key][k];
			}
		}
	}
	const out = {};
	for (let i = 0; i < ns.length; i++) {
		out[ns[i].k] = new bufferAttribute(fas[ns[i].k], ns[i].n);
	}
	return out;
};
