module Rito.Vector3 (vector3, Vector3, Vector3', normalize, add, mul, div, sub) where

import Rito.THREE as THREE

data Vector3
type Vector3' = { x :: Number, y :: Number, z :: Number }

vector3 :: THREE.TVector3 -> Vector3' -> Vector3
vector3 = ctor_

foreign import ctor_ :: THREE.TVector3 -> Vector3' -> Vector3
foreign import add_ :: Vector3 -> Vector3 -> Vector3
foreign import multiply_ :: Vector3 -> Vector3 -> Vector3
foreign import divide_ :: Vector3 -> Vector3 -> Vector3
foreign import sub_ :: Vector3 -> Vector3 -> Vector3
foreign import normalize_ :: Vector3 -> Vector3

normalize :: Vector3 -> Vector3
normalize = normalize_

add :: Vector3 -> Vector3 -> Vector3
add = add_
mul :: Vector3 -> Vector3 -> Vector3
mul = multiply_
sub :: Vector3 -> Vector3 -> Vector3
sub = sub_
div :: Vector3 -> Vector3 -> Vector3
div = divide_