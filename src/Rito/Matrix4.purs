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
import Rito.THREE as THREE
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
foreign import ctor_ :: THREE.TMatrix4 -> Matrix4
foreign import equals_ :: Matrix4 -> Matrix4 -> Boolean
foreign import set_ :: THREE.TMatrix4 -> Matrix4' -> Matrix4
foreign import compose_
  :: THREE.TMatrix4 -> Vector3 -> Quaternion -> Vector3 -> Matrix4
foreign import determinant_ :: Matrix4 -> Number
foreign import invert_ :: Matrix4 -> Matrix4
foreign import identity_ :: THREE.TMatrix4 -> Matrix4
foreign import lookAt_ :: Vector3 -> Vector3 -> Vector3 -> Matrix4 -> Matrix4
foreign import makeRotationAxis_ :: Vector3 -> Number -> Matrix4 -> Matrix4
foreign import makeRotationFromEuler_ :: Euler -> Matrix4 -> Matrix4
foreign import makeRotationFromQuaternion_ :: Quaternion -> Matrix4 -> Matrix4
foreign import makeRotationX_ :: Number -> Matrix4 -> Matrix4
foreign import makeRotationY_ :: Number -> Matrix4 -> Matrix4
foreign import makeRotationZ_ :: Number -> Matrix4 -> Matrix4
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
foreign import multiplyScalar_ :: Number -> Matrix4 -> Matrix4
foreign import premultiply_ :: Matrix4 -> Matrix4 -> Matrix4
foreign import scale_ :: Vector3 -> Matrix4 -> Matrix4
foreign import setPosition_ :: Vector3 -> Matrix4 -> Matrix4
foreign import transpose_ :: Matrix4 -> Matrix4

matrix4 :: THREE.TMatrix4 -> Matrix4' -> Matrix4
matrix4 = set_

data Matrix4
instance Semigroup Matrix4 where
  append = multiply_
instance Eq Matrix4 where
  eq = equals_

--

ctor :: THREE.TMatrix4 -> Matrix4
ctor = ctor_

equals :: Matrix4 -> Matrix4 -> Boolean
equals = equals_

set :: THREE.TMatrix4 -> Matrix4' -> Matrix4
set = set_

compose :: THREE.TMatrix4 -> Vector3 -> Quaternion -> Vector3 -> Matrix4
compose = compose_

determinant :: Matrix4 -> Number
determinant = determinant_

invert :: Matrix4 -> Matrix4
invert = invert_

identity :: THREE.TMatrix4 -> Matrix4
identity = identity_

lookAt :: Vector3 -> Vector3 -> Vector3 -> Matrix4 -> Matrix4
lookAt = lookAt_

makeRotationAxis :: Vector3 -> Number -> Matrix4 -> Matrix4
makeRotationAxis = makeRotationAxis_

makeRotationFromEuler :: Euler -> Matrix4 -> Matrix4
makeRotationFromEuler = makeRotationFromEuler_

makeRotationFromQuaternion :: Quaternion -> Matrix4 -> Matrix4
makeRotationFromQuaternion = makeRotationFromQuaternion_

makeRotationX :: Number -> Matrix4 -> Matrix4
makeRotationX = makeRotationX_

makeRotationY :: Number -> Matrix4 -> Matrix4
makeRotationY = makeRotationY_

makeRotationZ :: Number -> Matrix4 -> Matrix4
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

multiplyScalar :: Number -> Matrix4 -> Matrix4
multiplyScalar = multiplyScalar_

premultiply :: Matrix4 -> Matrix4 -> Matrix4
premultiply = premultiply_

scale :: Vector3 -> Matrix4 -> Matrix4
scale = scale_

setPosition :: Vector3 -> Matrix4 -> Matrix4
setPosition = setPosition_

transpose :: Matrix4 -> Matrix4
transpose = transpose_
