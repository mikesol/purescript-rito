module Rito.Box (Box, box) where

import Rito.Vector3 (Vector3)

type Box' = { min :: Vector3, max :: Vector3 }
data Box

foreign import ctor_ :: Box' -> Box

box :: Box' -> Box
box = ctor_