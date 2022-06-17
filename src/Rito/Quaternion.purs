module Rito.Quaternion where

import Rito.THREE as THREE

data Quaternion
type Quaternion' = { x :: Number, y :: Number, z :: Number, w :: Number }

quaternion :: THREE.TQuaternion -> Quaternion' -> Quaternion
quaternion = ctor_

foreign import ctor_ :: THREE.TQuaternion -> Quaternion' -> Quaternion
