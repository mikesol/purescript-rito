module Rito.Box (Box, box) where

import Rito.THREE as THREE
import Rito.Vector3 (Vector3)

type Box' = { min :: Vector3, max :: Vector3 }
data Box

foreign import ctor_ :: THREE.Three -> Box' -> Box

box :: THREE.Three -> Box' -> Box
box = ctor_