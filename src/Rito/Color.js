export const ctor_ = (color) => (rep) => new color(rep);
export const ctorRGB_ =
	(color) =>
	({ r, g, b }) =>
		new color(r, g, b);
