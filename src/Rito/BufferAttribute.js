export const bufferAttributeImpl = (m) => (n) => (bufferAttribute) => (f) => {

  const fa = new Float32Array(m*n);
  for (let i = 0; i < m; i++) {
    const o = i * n;
    const v = f(i);
    for (let j = 0; j < n; j++) {
      fa[o + j] = v[j];
    }
  }
  return new bufferAttribute(fa, n);
}
