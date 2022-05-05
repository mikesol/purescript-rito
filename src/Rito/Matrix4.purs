module Rito.Matrix4(Matrix4, Matrix4', matrix4) where

import Prelude
type Matrix4' = { n11 :: Number
     , n12 :: Number
     , n13 :: Number
     , n14 :: Number
     , n21 :: Number
     , n22 :: Number
     , n23 :: Number
     , n24 :: Number
     , n31 :: Number
     , n32 :: Number
     , n33 :: Number
     , n34 :: Number
     , n41 :: Number
     , n42 :: Number
     , n43 :: Number
     , n44 :: Number
     }
foreign import multiply_ :: Matrix4 -> Matrix4 -> Matrix4
foreign import ctor_ :: Matrix4
foreign import set_ :: Matrix4' -> Matrix4
matrix4 :: Matrix4' -> Matrix4
matrix4 = set_

data Matrix4
instance Semigroup Matrix4 where
  append = multiply_
instance Monoid Matrix4 where
  mempty = ctor_
