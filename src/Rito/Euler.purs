module Rito.Euler (euler, Euler, Euler') where

import Rito.THREE as THREE

data Euler
type Euler' = { x :: Number, y :: Number, z :: Number }

euler :: THREE.TEuler -> Euler' -> Euler
euler = ctor_

foreign import ctor_ :: THREE.TEuler -> Euler' -> Euler
