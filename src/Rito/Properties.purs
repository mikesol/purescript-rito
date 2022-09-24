module Rito.Properties where

import Prelude

import Data.Newtype (class Newtype, wrap)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Rito.Box3 as Box3
import Rito.Color as Color
import Rito.Euler (Euler)
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Scene (Background)
import Rito.Sphere as Sphere
import Rito.Texture (Texture)
import Rito.Vector3 (Vector3)
import Type.Proxy (Proxy(..))

radius
  :: forall nt r
   . Newtype nt (Variant (radius :: Number | r))
  => Number
  -> nt
radius = wrap <<< inj (Proxy :: Proxy "radius")

visible
  :: forall nt r
   . Newtype nt (Variant (visible :: Boolean | r))
  => Boolean
  -> nt
visible = wrap <<< inj (Proxy :: Proxy "visible")

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

rotation
  :: forall nt r
   . Newtype nt (Variant (rotation :: Euler | r))
  => Euler
  -> nt
rotation = wrap <<< inj (Proxy :: Proxy "rotation")

rotationFromEuler
  :: forall nt r
   . Newtype nt (Variant (rotationFromEuler :: Euler | r))
  => Euler
  -> nt
rotationFromEuler = wrap <<< inj (Proxy :: Proxy "rotationFromEuler")

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
   . Newtype nt
       (Variant (translate :: { x :: Number, y :: Number, z :: Number } | r))
  => { x :: Number, y :: Number, z :: Number }
  -> nt
translate = wrap <<< inj (Proxy :: Proxy "translate")

scale
  :: forall nt r
   . Newtype nt
       (Variant (scale :: { x :: Number, y :: Number, z :: Number } | r))
  => { x :: Number, y :: Number, z :: Number }
  -> nt
scale = wrap <<< inj (Proxy :: Proxy "scale")

lookAt
  :: forall nt r
   . Newtype nt (Variant (lookAt :: Vector3 | r))
  => Vector3
  -> nt
lookAt = wrap <<< inj (Proxy :: Proxy "lookAt")

map
  :: forall nt r
   . Newtype nt (Variant (map :: Texture | r))
  => Texture
  -> nt
map = wrap <<< inj (Proxy :: Proxy "map")

color
  :: forall nt r
   . Newtype nt (Variant (color :: Color.Color | r))
  => Color.Color
  -> nt
color = wrap <<< inj (Proxy :: Proxy "color")

center
  :: forall nt r
   . Newtype nt (Variant (center :: Unit | r))
  => nt
center = wrap $ inj (Proxy :: Proxy "center") unit

boundingBox
  :: forall nt r
   . Newtype nt (Variant (boundingBox :: Box3.Box3 -> Effect Unit | r))
  => (Box3.Box3 -> Effect Unit)
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

positionX
  :: forall nt r
   . Newtype nt (Variant (positionX :: Number | r))
  => Number
  -> nt
positionX = wrap <<< inj (Proxy :: Proxy "positionX")

positionY
  :: forall nt r
   . Newtype nt (Variant (positionY :: Number | r))
  => Number
  -> nt
positionY = wrap <<< inj (Proxy :: Proxy "positionY")

positionZ
  :: forall nt r
   . Newtype nt (Variant (positionZ :: Number | r))
  => Number
  -> nt
positionZ = wrap <<< inj (Proxy :: Proxy "positionZ")

scaleX
  :: forall nt r
   . Newtype nt (Variant (scaleX :: Number | r))
  => Number
  -> nt
scaleX = wrap <<< inj (Proxy :: Proxy "scaleX")

scaleY
  :: forall nt r
   . Newtype nt (Variant (scaleY :: Number | r))
  => Number
  -> nt
scaleY = wrap <<< inj (Proxy :: Proxy "scaleY")

scaleZ
  :: forall nt r
   . Newtype nt (Variant (scaleZ :: Number | r))
  => Number
  -> nt
scaleZ = wrap <<< inj (Proxy :: Proxy "scaleZ")

decay
  :: forall nt r
   . Newtype nt (Variant (decay :: Number | r))
  => Number
  -> nt
decay = wrap <<< inj (Proxy :: Proxy "decay")

distance
  :: forall nt r
   . Newtype nt (Variant (distance :: Number | r))
  => Number
  -> nt
distance = wrap <<< inj (Proxy :: Proxy "distance")

intensity
  :: forall nt r
   . Newtype nt (Variant (intensity :: Number | r))
  => Number
  -> nt
intensity = wrap <<< inj (Proxy :: Proxy "intensity")

aspect
  :: forall nt r
   . Newtype nt (Variant (aspect :: Number | r))
  => Number
  -> nt
aspect = wrap <<< inj (Proxy :: Proxy "aspect")

render
  :: forall nt r
   . Newtype nt (Variant (render :: Unit | r))
  => nt
render = wrap (inj (Proxy :: Proxy "render") unit)

size
  :: forall nt r
   . Newtype nt (Variant (size :: { width :: Number, height :: Number } | r))
  => { width :: Number, height :: Number }
  -> nt
size = wrap <<< (inj (Proxy :: Proxy "size"))

--
target
  :: forall nt r r1
   . Newtype nt
       (Variant (orbitControls :: Variant (target :: Vector3 | r1) | r))
  => Vector3
  -> nt
target = wrap <<< (inj (Proxy :: Proxy "orbitControls")) <<<
  (inj (Proxy :: Proxy "target"))


-- listeners
onClick
  :: forall onClick nt r
   . Newtype nt (Variant (onClick :: onClick | r))
  => onClick
  -> nt
onClick = wrap <<< inj (Proxy :: Proxy "onClick")

onMouseDown
  :: forall onMouseDown nt r
   . Newtype nt (Variant (onMouseDown :: onMouseDown | r))
  => onMouseDown
  -> nt
onMouseDown = wrap <<< inj (Proxy :: Proxy "onMouseDown")

onTouchStart
  :: forall onTouchStart nt r
   . Newtype nt (Variant (onTouchStart :: onTouchStart | r))
  => onTouchStart
  -> nt
onTouchStart = wrap <<< inj (Proxy :: Proxy "onTouchStart")

-- background

background
  :: forall nt r
   . Newtype nt (Variant (background :: Background | r))
  => Background
  -> nt
background = wrap <<< inj (Proxy :: Proxy "background")

uniform
  :: forall nt r uniform
   . Newtype nt (Variant (uniform :: uniform | r))
  => uniform
  -> nt
uniform = wrap <<< inj (Proxy :: Proxy "uniform")