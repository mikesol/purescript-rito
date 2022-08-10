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

import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event,  makeEvent, subscribe)
import Record (union)
import Rito.Core (object3D)
import Rito.Core as C
import Rito.THREE as THREE

data PerspectiveCameraOptions = PerspectiveCameraOptions

instance
  ConvertOption PerspectiveCameraOptions
    "perspectiveCamera"
    THREE.TPerspectiveCamera
    THREE.TPerspectiveCamera where
  convertOption _ _ = identity

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
  (perspectiveCamera :: THREE.TPerspectiveCamera | PerspectiveCameraOptional)

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
  | C.Object3D
  )
newtype PerspectiveCamera = PerspectiveCamera PerspectiveCamera'
instance Newtype PerspectiveCamera PerspectiveCamera'

perspectiveCamera
  :: forall i lock payload
   . InitialPerspectiveCamera i
  => i
  -> Event PerspectiveCamera
  -> C.Camera lock payload
perspectiveCamera i' atts = C.Camera go
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
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      pure
        ( makePerspectiveCamera
            { id: me
            , parent: parent.parent
            , scope: parent.scope
            , perspectiveCamera: i.perspectiveCamera
            , aspect: i.aspect
            , far: i.far
            , fov: i.fov
            , near: i.near
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
  -> C.Camera lock payload
perspectiveCamera_ i = perspectiveCamera i empty