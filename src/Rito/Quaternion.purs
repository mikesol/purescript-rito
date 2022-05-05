module Rito.Quaternion where


data Quaternion
type Quaternion' = { x :: Number, y :: Number, z :: Number, w :: Number }

quaternion :: Quaternion' -> Quaternion
quaternion = ctor_

foreign import ctor_ :: Quaternion' -> Quaternion
