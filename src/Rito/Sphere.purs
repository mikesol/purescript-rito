module Rito.Sphere (Sphere, sphere) where

import Rito.THREE as THREE
import Rito.Vector3 (Vector3)

type Sphere' = { center :: Vector3, radius :: Vector3 }
data Sphere

foreign import ctor_ :: THREE.TSphere -> Sphere' -> Sphere

sphere :: THREE.TSphere -> Sphere' -> Sphere
sphere = ctor_