module Rito.Matrix4
  ( Matrix4
  , Matrix4'
  , matrix4
  , ctor
  , equals
  , set
  , compose
  , determinant
  , invert
  , identity
  , lookAt
  , makeRotationAxis
  , makeRotationFromEuler
  , makeRotationFromQuaternion
  , makeRotationX
  , makeRotationY
  , makeRotationZ
  , makeScale
  , makeShear
  , makeTranslation
  , multiply
  , multiplyMatrices
  , multiplyScalar
  , premultiply
  , scale
  , setPosition
  , transpose
  ) where

import Prelude

import Rito.Euler (Euler)
import Rito.Quaternion (Quaternion)
import Rito.Vector3 (Vector3)

type Matrix4' =
  { n11 :: Number
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
foreign import ctor_ :: Matrix4
foreign import equals_ :: Matrix4 -> Matrix4 -> Boolean
foreign import set_ :: Matrix4' -> Matrix4
foreign import compose_
  :: Vector3 -> Quaternion -> Vector3 -> Matrix4
foreign import determinant_ :: Matrix4 -> Number
foreign import invert_ :: Matrix4 -> Matrix4
foreign import identity_ :: Matrix4
foreign import lookAt_ :: Matrix4 -> Vector3 -> Vector3 -> Vector3 -> Matrix4
foreign import makeRotationAxis_ :: Matrix4 -> Vector3 -> Number -> Matrix4
foreign import makeRotationFromEuler_ :: Matrix4 -> Euler -> Matrix4
foreign import makeRotationFromQuaternion_ :: Matrix4 -> Quaternion -> Matrix4
foreign import makeRotationX_ :: Matrix4 -> Number -> Matrix4
foreign import makeRotationY_ :: Matrix4 -> Number -> Matrix4
foreign import makeRotationZ_ :: Matrix4 -> Number -> Matrix4
foreign import makeScale_ :: Matrix4 -> Number -> Number -> Number -> Matrix4
foreign import makeShear_
  :: Matrix4
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Matrix4
foreign import makeTranslation_
  :: Matrix4 -> Number -> Number -> Number -> Matrix4
foreign import multiply_ :: Matrix4 -> Matrix4 -> Matrix4
foreign import multiplyMatrices_ :: Matrix4 -> Matrix4 -> Matrix4
foreign import multiplyScalar_ :: Matrix4 -> Number -> Matrix4
foreign import premultiply_ :: Matrix4 -> Matrix4 -> Matrix4
foreign import scale_ :: Matrix4 -> Vector3 -> Matrix4
foreign import setPosition_ :: Matrix4 -> Vector3 -> Matrix4
foreign import transpose_ :: Matrix4 -> Matrix4

matrix4 :: Matrix4' -> Matrix4
matrix4 = set_

data Matrix4
instance Semigroup Matrix4 where
  append = multiply_
instance Monoid Matrix4 where
  mempty = identity_
instance Eq Matrix4 where
  eq = equals_

--

ctor :: Matrix4
ctor = ctor_

equals :: Matrix4 -> Matrix4 -> Boolean
equals = equals_

set :: Matrix4' -> Matrix4
set = set_

compose :: Vector3 -> Quaternion -> Vector3 -> Matrix4
compose = compose_

determinant :: Matrix4 -> Number
determinant = determinant_

invert :: Matrix4 -> Matrix4
invert = invert_

identity :: Matrix4
identity = identity_

lookAt :: Matrix4 -> Vector3 -> Vector3 -> Vector3 -> Matrix4
lookAt = lookAt_

makeRotationAxis :: Matrix4 -> Vector3 -> Number -> Matrix4
makeRotationAxis = makeRotationAxis_

makeRotationFromEuler :: Matrix4 -> Euler -> Matrix4
makeRotationFromEuler = makeRotationFromEuler_

makeRotationFromQuaternion :: Matrix4 -> Quaternion -> Matrix4
makeRotationFromQuaternion = makeRotationFromQuaternion_

makeRotationX :: Matrix4 -> Number -> Matrix4
makeRotationX = makeRotationX_

makeRotationY :: Matrix4 -> Number -> Matrix4
makeRotationY = makeRotationY_

makeRotationZ :: Matrix4 -> Number -> Matrix4
makeRotationZ = makeRotationZ_

makeScale :: Matrix4 -> Number -> Number -> Number -> Matrix4
makeScale = makeScale_

makeShear
  :: Matrix4
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Number
  -> Matrix4
makeShear = makeShear_

makeTranslation :: Matrix4 -> Number -> Number -> Number -> Matrix4
makeTranslation = makeTranslation_

multiply :: Matrix4 -> Matrix4 -> Matrix4
multiply = multiply_

multiplyMatrices :: Matrix4 -> Matrix4 -> Matrix4
multiplyMatrices = multiplyMatrices_

multiplyScalar :: Matrix4 -> Number -> Matrix4
multiplyScalar = multiplyScalar_

premultiply :: Matrix4 -> Matrix4 -> Matrix4
premultiply = premultiply_

scale :: Matrix4 -> Vector3 -> Matrix4
scale = scale_

setPosition :: Matrix4 -> Vector3 -> Matrix4
setPosition = setPosition_

transpose :: Matrix4 -> Matrix4
transpose = transpose_
