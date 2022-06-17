module Rito.Color where

import Prelude

import Data.Int (toNumber)
import Rito.THREE as THREE

data Color

foreign import ctor_ :: forall rep. THREE.TColor -> rep -> Color
foreign import ctorRGB_
  :: THREE.TColor -> { r :: Number, g :: Number, b :: Number } -> Color

class ColorRepresentation rep where
  color :: THREE.TColor -> rep -> Color

data RGB = RGB Number Number Number

instance ColorRepresentation Int where
  color t = ctor_ t <<< toNumber
instance ColorRepresentation Number where
  color = ctor_
instance ColorRepresentation String where
  color = ctor_
instance ColorRepresentation Color where
  color = ctor_
instance ColorRepresentation RGB where
  color t (RGB r g b) = ctorRGB_ t { r, g, b }