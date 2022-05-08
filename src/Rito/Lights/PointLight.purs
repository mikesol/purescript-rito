module Rito.Lights.PointLight
  ( pointLight
  , pointLight_
  , PointLight(..)
  , PointLight'
  , class InitialPointLight
  , PointLightOptions
  , toInitializePointLight
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Color (class ColorRepresentation, Color, color)
import Rito.Core as C
import Rito.Euler (Euler)
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Vector3 (Vector3)

data PointLightOptions = PointLightOptions

instance
  ColorRepresentation n =>
  ConvertOption PointLightOptions
    "color"
    n
    Color where
  convertOption _ _ = color

instance
  ConvertOption PointLightOptions
    "intensity"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PointLightOptions
    "distance"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PointLightOptions
    "decay"
    Number
    Number where
  convertOption _ _ = identity

type PointLightOptional =
  ( color :: Color
  , intensity :: Number
  , distance :: Number
  , decay :: Number
  )

type PointLightAll =
  (| PointLightOptional)

defaultPointLight :: { | PointLightOptional }
defaultPointLight =
  { color: color 0xffffff
  , intensity: 1.0
  , distance: 0.0
  , decay: 1.0
  }

class InitialPointLight i where
  toInitializePointLight :: i -> C.InitializePointLight

instance InitialPointLight C.InitializePointLight where
  toInitializePointLight = identity

instance
  ConvertOptionsWithDefaults PointLightOptions { | PointLightOptional }
    { | provided }
    { | PointLightAll } =>
  InitialPointLight { | provided } where
  toInitializePointLight provided = C.InitializePointLight
    (convertOptionsWithDefaults PointLightOptions defaultPointLight provided)

type PointLight' = Variant
  ( color :: Color
  , intensity :: Number
  , distance :: Number
  , decay :: Number
  -- object3D
  , matrix4 :: Matrix4
  , quaternion :: Quaternion
  , rotationFromAxisAngle :: { axis :: Vector3, angle :: Number }
  , rotationFromEuler :: Euler
  , rotationFromMatrix :: Matrix4
  , rotationFromQuaternion :: Quaternion
  , rotateOnAxis :: { axis :: Vector3, angle :: Number }
  , rotateOnWorldAxis :: { axis :: Vector3, angle :: Number }
  , rotateX :: Number
  , rotateY :: Number
  , rotateZ :: Number
  , translateOnAxis :: { axis :: Vector3, distance :: Number }
  , translateX :: Number
  , translateY :: Number
  , translateZ :: Number
  , positionX :: Number
  , positionY :: Number
  , positionZ :: Number
  , scaleX :: Number
  , scaleY :: Number
  , scaleZ :: Number
  , lookAt :: Vector3
  )
newtype PointLight = PointLight PointLight'
instance Newtype PointLight PointLight'

pointLight
  :: forall i lock payload
   . InitialPointLight i
  => i
  -> Event PointLight
  -> C.Light lock payload
pointLight i' atts = C.Light go
  where
  C.InitializePointLight i = toInitializePointLight i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makePointLight
        , setColor
        , setIntensity
        , setDistance
        , setDecay
        -- object 3D
        , setMatrix4
        , setQuaternion
        , setRotationFromAxisAngle
        , setRotationFromEuler
        , setRotationFromMatrix
        , setRotationFromQuaternion
        , setRotateOnAxis
        , setRotateOnWorldAxis
        , setRotateX
        , setRotateY
        , setRotateZ
        , setTranslateOnAxis
        , setTranslateX
        , setTranslateY
        , setTranslateZ
        , setPositionX
        , setPositionY
        , setPositionZ
        , setScaleX
        , setScaleY
        , setScaleZ
        , setLookAt
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makePointLight
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , color: i.color
            , intensity: i.intensity
            , distance: i.distance
            , decay: i.decay
            }
        )
        <|>
          ( map
              ( \(PointLight e) -> match
                  { color: setColor <<< { id: me, color: _ }
                  , intensity: setIntensity <<< { id: me, intensity: _ }
                  , distance: setDistance <<< { id: me, distance: _ }
                  , decay: setDecay <<< { id: me, decay: _ }
                  -- object 3D
                  , matrix4: setMatrix4 <<< { id: me, matrix4: _ }
                  , quaternion: setQuaternion <<< { id: me, quaternion: _ }
                  , rotationFromAxisAngle: \{ axis, angle } ->
                      setRotationFromAxisAngle { id: me, axis, angle }
                  , rotationFromEuler: setRotationFromEuler <<<
                      { id: me, euler: _ }
                  , rotationFromMatrix: setRotationFromMatrix <<<
                      { id: me, matrix4: _ }
                  , rotationFromQuaternion: setRotationFromQuaternion <<<
                      { id: me, quaternion: _ }
                  , rotateOnAxis: \{ axis, angle } -> setRotateOnAxis
                      { id: me, axis, angle }
                  , rotateOnWorldAxis: \{ axis, angle } -> setRotateOnWorldAxis
                      { id: me, axis, angle }
                  , rotateX: setRotateX <<< { id: me, rotateX: _ }
                  , rotateY: setRotateY <<< { id: me, rotateY: _ }
                  , rotateZ: setRotateZ <<< { id: me, rotateZ: _ }
                  , translateOnAxis: \{ axis, distance } -> setTranslateOnAxis
                      { id: me, axis, distance }
                  , translateX: setTranslateX <<< { id: me, translateX: _ }
                  , translateY: setTranslateY <<< { id: me, translateY: _ }
                  , translateZ: setTranslateZ <<< { id: me, translateZ: _ }
                  , positionX: setPositionX <<< { id: me, positionX: _ }
                  , positionY: setPositionY <<< { id: me, positionY: _ }
                  , positionZ: setPositionZ <<< { id: me, positionZ: _ }
                  , scaleX: setScaleX <<< { id: me, scaleX: _ }
                  , scaleY: setScaleY <<< { id: me, scaleY: _ }
                  , scaleZ: setScaleZ <<< { id: me, scaleZ: _ }
                  , lookAt: setLookAt <<< { id: me, v: _ }
                  }
                  e
              )
              atts
          )

pointLight_
  :: forall i lock payload
   . InitialPointLight i
  => i
  -> C.Light lock payload
pointLight_ i = pointLight i empty