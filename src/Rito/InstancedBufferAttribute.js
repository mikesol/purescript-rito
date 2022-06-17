export const instancedBufferAttributeImpl = (m) => (n) => (instancedBufferAttribute) => (f) => {

  const fa = new Float32Array(m*n);
  for (let i = 0; i < m; i++) {
    const o = i * n;
    const v = f(i);
    for (let j = 0; j < n; j++) {
      fa[o + j] = v[j];
    }
  }
  return new instancedBufferAttribute(fa, n);
}
