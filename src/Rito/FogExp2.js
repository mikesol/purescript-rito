export const fogExp2 = (ctor) => (color) => (density) => () => new ctor(color, density);