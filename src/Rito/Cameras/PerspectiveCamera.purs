module Rito.Cameras.PerspectiveCamera
  ( perspectiveCamera
  , perspectiveCamera_
  , PerspectiveCamera(..)
  , PerspectiveCamera'
  , class InitialPerspectiveCamera
  , toInitializePerspectiveCamera
  , PerspectiveCameraOptions
  ) where

import Prelude

import Bolson.Core (Entity(..))
import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.Euler (Euler)
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Vector3 (Vector3)

data PerspectiveCameraOptions = PerspectiveCameraOptions

instance
  ConvertOption PerspectiveCameraOptions
    "fov"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PerspectiveCameraOptions
    "aspect"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PerspectiveCameraOptions
    "near"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PerspectiveCameraOptions
    "far"
    Number
    Number where
  convertOption _ _ = identity

type PerspectiveCameraOptional =
  ( fov :: Number
  , aspect :: Number
  , near :: Number
  , far :: Number
  )

type PerspectiveCameraAll =
  (| PerspectiveCameraOptional)

defaultPerspectiveCamera :: { | PerspectiveCameraOptional }
defaultPerspectiveCamera =
  { fov: 50.0
  , aspect: 1.0
  , near: 0.1
  , far: 2000.0
  }

class InitialPerspectiveCamera i where
  toInitializePerspectiveCamera :: i -> C.InitializePerspectiveCamera

instance InitialPerspectiveCamera C.InitializePerspectiveCamera where
  toInitializePerspectiveCamera = identity

instance
  ConvertOptionsWithDefaults PerspectiveCameraOptions
    { | PerspectiveCameraOptional }
    { | provided }
    { | PerspectiveCameraAll } =>
  InitialPerspectiveCamera { | provided } where
  toInitializePerspectiveCamera provided = C.InitializePerspectiveCamera
    ( convertOptionsWithDefaults PerspectiveCameraOptions
        defaultPerspectiveCamera
        provided
    )

type PerspectiveCamera' = Variant
  ( aspect :: Number
  , far :: Number
  , filmGauge :: Number
  , filmOffset :: Number
  , focus :: Number
  , fov :: Number
  , near :: Number
  , zoom :: Number
  , focalLength :: Number
  , viewOffset ::
      { fullWidth :: Number
      , fullHeight :: Number
      , x :: Number
      , y :: Number
      , width :: Number
      , height :: Number
      }
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
newtype PerspectiveCamera = PerspectiveCamera PerspectiveCamera'
instance Newtype PerspectiveCamera PerspectiveCamera'

perspectiveCamera
  :: forall i lock payload
   . InitialPerspectiveCamera i
  => i
  -> Event PerspectiveCamera
  -> C.ACamera lock payload
perspectiveCamera i' atts = Element' $ C.Camera go
  where
  C.InitializePerspectiveCamera i = toInitializePerspectiveCamera i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makePerspectiveCamera
        , setAspect
        , setFar
        , setFilmGauge
        , setFilmOffset
        , setFocus
        , setFov
        , setNear
        , setZoom
        , setFocalLength
        , setViewOffset
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
        ( makePerspectiveCamera
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , aspect: i.aspect
            , far: i.far
            , fov: i.fov
            , near: i.near
            }
        )
        <|>
          ( map
              ( \(PerspectiveCamera e) -> match
                  { aspect: setAspect <<< { id: me, aspect: _ }
                  , far: setFar <<< { id: me, far: _ }
                  , filmGauge: setFilmGauge <<< { id: me, filmGauge: _ }
                  , filmOffset: setFilmOffset <<< { id: me, filmOffset: _ }
                  , focus: setFocus <<< { id: me, focus: _ }
                  , fov: setFov <<< { id: me, fov: _ }
                  , near: setNear <<< { id: me, near: _ }
                  , zoom: setZoom <<< { id: me, zoom: _ }
                  , focalLength: setFocalLength <<< { id: me, focalLength: _ }
                  , viewOffset:
                      \{ fullWidth
                       , fullHeight
                       , x
                       , y
                       , width
                       , height
                       } -> setViewOffset
                        { id: me
                        , fullWidth
                        , fullHeight
                        , x
                        , y
                        , width
                        , height
                        }
                  -- object3D
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

perspectiveCamera_
  :: forall i lock payload
   . InitialPerspectiveCamera i
  => i
  -> C.ACamera lock payload
perspectiveCamera_ i = perspectiveCamera i empty