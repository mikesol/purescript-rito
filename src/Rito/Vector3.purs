module Rito.Vector3(vector3, Vector3, Vector3', normalize) where

import Prelude


data Vector3
type Vector3' = { x :: Number, y :: Number, z :: Number }

vector3 :: Vector3' -> Vector3
vector3 = ctor_

foreign import ctor_ :: Vector3' -> Vector3
foreign import add_ :: Vector3 -> Vector3 -> Vector3
foreign import multiply_ :: Vector3 -> Vector3 -> Vector3
foreign import divide_ :: Vector3 -> Vector3 -> Vector3
foreign import sub_ :: Vector3 -> Vector3 -> Vector3
foreign import normalize_ :: Vector3 -> Vector3

normalize :: Vector3 -> Vector3
normalize = normalize_

instance Semiring Vector3 where
  add = add_
  mul = multiply_
  one = vector3 { x: 1.0, y: 1.0, z: 1.0 }
  zero = vector3 { x: 0.0, y: 0.0, z: 0.0 }

instance Ring Vector3 where
  sub = sub_

instance CommutativeRing Vector3

instance EuclideanRing Vector3 where
  degree _ = 1
  div = divide_
  mod _ _ = zero