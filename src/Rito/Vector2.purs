module Rito.Vector2 (vector2, Vector2, Vector2', normalize, add, mul, div, sub) where

import Rito.THREE as THREE

data Vector2
type Vector2' = { x :: Number, y :: Number }

vector2 :: THREE.TVector2 -> Vector2' -> Vector2
vector2 = ctor_

foreign import ctor_ :: THREE.TVector2 -> Vector2' -> Vector2
foreign import add_ :: Vector2 -> Vector2 -> Vector2
foreign import multiply_ :: Vector2 -> Vector2 -> Vector2
foreign import divide_ :: Vector2 -> Vector2 -> Vector2
foreign import sub_ :: Vector2 -> Vector2 -> Vector2
foreign import normalize_ :: Vector2 -> Vector2

normalize :: Vector2 -> Vector2
normalize = normalize_

add :: Vector2 -> Vector2 -> Vector2
add = add_
mul :: Vector2 -> Vector2 -> Vector2
mul = multiply_
sub :: Vector2 -> Vector2 -> Vector2
sub = sub_
div :: Vector2 -> Vector2 -> Vector2
div = divide_