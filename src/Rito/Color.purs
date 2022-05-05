module Rito.Color where

data Color

foreign import ctor_ :: forall rep. rep -> Color
foreign import ctorRGB_ :: { r :: Number, g :: Number, b :: Number } -> Color

class ColorRepresentation rep where
  color :: rep -> Color

data RGB = RGB Number Number Number

instance ColorRepresentation Number where
  color = ctor_
instance ColorRepresentation String where
  color = ctor_
instance ColorRepresentation Color where
  color = ctor_
instance ColorRepresentation RGB where
  color (RGB r g b) = ctorRGB_ { r, g, b }