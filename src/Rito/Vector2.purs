module Rito.Vector2 (vector2, Vector2, Vector2', normalize) where

import Prelude

data Vector2
type Vector2' = { x :: Number, y :: Number }

vector2 :: Vector2' -> Vector2
vector2 = ctor_

foreign import ctor_ :: Vector2' -> Vector2
foreign import add_ :: Vector2 -> Vector2 -> Vector2
foreign import multiply_ :: Vector2 -> Vector2 -> Vector2
foreign import divide_ :: Vector2 -> Vector2 -> Vector2
foreign import sub_ :: Vector2 -> Vector2 -> Vector2
foreign import normalize_ :: Vector2 -> Vector2

normalize :: Vector2 -> Vector2
normalize = normalize_

instance Semiring Vector2 where
  add = add_
  mul = multiply_
  one = vector2 { x: 1.0, y: 1.0 }
  zero = vector2 { x: 0.0, y: 0.0 }

instance Ring Vector2 where
  sub = sub_

instance CommutativeRing Vector2

instance EuclideanRing Vector2 where
  degree _ = 1
  div = divide_
  mod _ _ = zero