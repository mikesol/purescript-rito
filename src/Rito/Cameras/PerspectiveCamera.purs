module Rito.Cameras.PerspectiveCamera
  ( perspectiveCamera
  , perspectiveCamera_
  , OrbitControlsAll
  , OrbitControlsOptional
  , PerspectiveCamera(..)
  , PerspectiveCamera'
  , defaultOrbitControls
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
import Record (union)
import Rito.Core (object3D)
import Rito.Core as C
import Rito.Vector3 (Vector3)
import Web.HTML (HTMLCanvasElement)

data PerspectiveCameraOptions = PerspectiveCameraOptions
data OrbitControlsOptions = OrbitControlsOptions

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

instance
  ConvertOption OrbitControlsOptions
    "canvas"
    HTMLCanvasElement
    HTMLCanvasElement where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "autoRotate"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "autoRotateSpeed"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "dampingFactor"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "enabled"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "enableDamping"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "enablePan"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "enableRotate"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "enableZoom"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "panSpeed"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "rotateSpeed"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption OrbitControlsOptions
    "zoomSpeed"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PerspectiveCameraOptions
    "orbitControls"
    C.OrbitControls
    C.OrbitControls where
  convertOption _ _ = identity

type PerspectiveCameraOptional =
  ( fov :: Number
  , aspect :: Number
  , near :: Number
  , far :: Number
  )

type OrbitControlsOptional =
  ( autoRotate :: Boolean
  , autoRotateSpeed :: Number
  , dampingFactor :: Number
  , enabled :: Boolean
  , enableDamping :: Boolean
  , enablePan :: Boolean
  , enableRotate :: Boolean
  , enableZoom :: Boolean
  , panSpeed :: Number
  , rotateSpeed :: Number
  , zoomSpeed :: Number
  )

type OrbitControlsAll =
  ( canvas :: HTMLCanvasElement
  | OrbitControlsOptional
  )

type PerspectiveCameraAll =
  (orbitControls :: C.OrbitControls | PerspectiveCameraOptional)

defaultOrbitControls :: HTMLCanvasElement -> { | OrbitControlsAll }
defaultOrbitControls canvas =
  { autoRotate: false
  , canvas
  , autoRotateSpeed: 2.0
  , dampingFactor: 0.05
  , enabled: false
  , enableDamping: false
  , enablePan: true
  , enableRotate: true
  , enableZoom: true
  , panSpeed: 1.0
  , rotateSpeed: 1.0
  , zoomSpeed: 1.0
  }

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
  , withWorldDirection :: Vector3 -> PerspectiveCamera
  , orbitControls :: Variant (target :: Vector3)
  | C.Object3D
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
    di@
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
          , withWorldDirection
          , setTarget
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
            , orbitControls: i.orbitControls
            }
        )
        <|>
          ( map
              ( let
                  fn = \(PerspectiveCamera e) -> match
                    ( union
                        { aspect: setAspect <<< { id: me, aspect: _ }
                        , far: setFar <<< { id: me, far: _ }
                        , filmGauge: setFilmGauge <<< { id: me, filmGauge: _ }
                        , filmOffset: setFilmOffset <<<
                            { id: me, filmOffset: _ }
                        , focus: setFocus <<< { id: me, focus: _ }
                        , fov: setFov <<< { id: me, fov: _ }
                        , near: setNear <<< { id: me, near: _ }
                        , zoom: setZoom <<< { id: me, zoom: _ }
                        , focalLength: setFocalLength <<<
                            { id: me, focalLength: _ }
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
                        , withWorldDirection: withWorldDirection
                            <<< { id: me, withWorldDirection: _ }
                            <<< map fn
                        , orbitControls: match
                            { target: setTarget <<< { id: me, target: _ } }
                        }
                        (object3D me di)
                    )
                    e
                in
                  fn
              )
              atts
          )

perspectiveCamera_
  :: forall i lock payload
   . InitialPerspectiveCamera i
  => i
  -> C.ACamera lock payload
perspectiveCamera_ i = perspectiveCamera i empty