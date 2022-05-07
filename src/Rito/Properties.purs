module Rito.Properties where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Rito.Box as Box
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Sphere as Sphere
import Rito.Vector3 (Vector3)
import Type.Proxy (Proxy(..))

radius
  :: forall nt r
   . Newtype nt (Variant (radius :: Number | r))
  => Number
  -> nt
radius = wrap <<< inj (Proxy :: Proxy "radius")

widthSegments
  :: forall nt r
   . Newtype nt (Variant (widthSegments :: Int | r))
  => Int
  -> nt
widthSegments = wrap <<< inj (Proxy :: Proxy "widthSegments")

heightSegments
  :: forall nt r
   . Newtype nt (Variant (heightSegments :: Int | r))
  => Int
  -> nt
heightSegments = wrap <<< inj (Proxy :: Proxy "heightSegments")

phiStart
  :: forall nt r
   . Newtype nt (Variant (phiStart :: Number | r))
  => Number
  -> nt
phiStart = wrap <<< inj (Proxy :: Proxy "phiStart")

phiLength
  :: forall nt r
   . Newtype nt (Variant (phiLength :: Number | r))
  => Number
  -> nt
phiLength = wrap <<< inj (Proxy :: Proxy "phiLength")

thetaStart
  :: forall nt r
   . Newtype nt (Variant (thetaStart :: Number | r))
  => Number
  -> nt
thetaStart = wrap <<< inj (Proxy :: Proxy "thetaStart")

thetaLength
  :: forall nt r
   . Newtype nt (Variant (thetaLength :: Number | r))
  => Number
  -> nt
thetaLength = wrap <<< inj (Proxy :: Proxy "thetaLength")

matrix4
  :: forall nt r
   . Newtype nt (Variant (matrix4 :: Matrix4 | r))
  => Matrix4
  -> nt
matrix4 = wrap <<< inj (Proxy :: Proxy "matrix4")

quaternion
  :: forall nt r
   . Newtype nt (Variant (quaternion :: Quaternion | r))
  => Quaternion
  -> nt
quaternion = wrap <<< inj (Proxy :: Proxy "quaternion")

rotateX
  :: forall nt r
   . Newtype nt (Variant (rotateX :: Number | r))
  => Number
  -> nt
rotateX = wrap <<< inj (Proxy :: Proxy "rotateX")

rotateY
  :: forall nt r
   . Newtype nt (Variant (rotateY :: Number | r))
  => Number
  -> nt
rotateY = wrap <<< inj (Proxy :: Proxy "rotateY")

rotateZ
  :: forall nt r
   . Newtype nt (Variant (rotateZ :: Number | r))
  => Number
  -> nt
rotateZ = wrap <<< inj (Proxy :: Proxy "rotateZ")

translate
  :: forall nt r
   . Newtype nt (Variant (translate :: { x :: Number, y :: Number, z :: Number } | r))
  => { x :: Number, y :: Number, z :: Number }
  -> nt
translate = wrap <<< inj (Proxy :: Proxy "translate")

scale
  :: forall nt r
   . Newtype nt (Variant (scale :: { x :: Number, y :: Number, z :: Number } | r))
  => { x :: Number, y :: Number, z :: Number }
  -> nt
scale = wrap <<< inj (Proxy :: Proxy "scale")

lookAt
  :: forall nt r
   . Newtype nt (Variant (lookAt :: Vector3 | r))
  => Vector3
  -> nt
lookAt = wrap <<< inj (Proxy :: Proxy "lookAt")

center
  :: forall nt r
   . Newtype nt (Variant (center :: Unit | r))
  => nt
center = wrap $ inj (Proxy :: Proxy "center") unit

boundingBox
  :: forall nt r
   . Newtype nt (Variant (boundingBox :: Box.Box -> Effect Unit | r))
  => (Box.Box -> Effect Unit)
  -> nt
boundingBox = wrap <<< inj (Proxy :: Proxy "boundingBox")

boundingSphere
  :: forall nt r
   . Newtype nt (Variant (boundingSphere :: Sphere.Sphere -> Effect Unit | r))
  => (Sphere.Sphere -> Effect Unit)
  -> nt
boundingSphere = wrap <<< inj (Proxy :: Proxy "boundingSphere")
--
translateX
  :: forall nt r
   . Newtype nt (Variant (translateX :: Number | r))
  => Number
  -> nt
translateX = wrap <<< inj (Proxy :: Proxy "translateX")

translateY
  :: forall nt r
   . Newtype nt (Variant (translateY :: Number | r))
  => Number
  -> nt
translateY = wrap <<< inj (Proxy :: Proxy "translateY")

translateZ
  :: forall nt r
   . Newtype nt (Variant (translateZ :: Number | r))
  => Number
  -> nt
translateZ = wrap <<< inj (Proxy :: Proxy "translateZ")