module Rito.Box3 (Box3, box3) where

import Rito.THREE as THREE
import Rito.Vector3 (Vector3)

type Box3' = { min :: Vector3, max :: Vector3 }
data Box3

foreign import ctor_ :: THREE.TBox3 -> Box3' -> Box3

box3 :: THREE.TBox3 -> Box3' -> Box3
box3 = ctor_