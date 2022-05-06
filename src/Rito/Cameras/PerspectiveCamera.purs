module Rito.Geometries.PerspectiveCamera
  ( perspectiveCamera
  , perspectiveCamera_
  , PerspectiveCamera
  , class InitialPerspectiveCamera
  , toInitializePerspectiveCamera
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Number (pi)
import Data.Variant (Variant, match)
import Effect (Effect)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Record (union)
import Rito.Box as Box
import Rito.Core as C
import Rito.Matrix4 (Matrix4)
import Rito.Quaternion (Quaternion)
import Rito.Vector3 (Vector3)

twoPi = pi * 2.0 :: Number
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

newtype PerspectiveCamera = PerspectiveCamera
  ( Variant
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
      )
  )

perspectiveCamera
  :: forall i lock payload
   . InitialPerspectiveCamera i
  => i
  -> Event PerspectiveCamera
  -> C.Geometry lock payload
perspectiveCamera i' atts = C.Geometry go
  where
  C.InitializePerspectiveCamera i = toInitializePerspectiveCamera i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        --, makePerspectiveCamera
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
            , radius: i.radius
            , widthSegments: i.widthSegments
            , heightSegments: i.heightSegments
            , phiStart: i.phiStart
            , phiLength: i.phiLength
            , thetaStart: i.thetaStart
            , thetaLength: i.thetaLength
            }
        )
        <|>
          ( map
              ( \(PerspectiveCamera e) -> match
                  { radius: setRadius <<< { id: me, radius: _ }
                  , widthSegments: setWidthSegments <<<
                      { id: me, widthSegments: _ }
                  , heightSegments: setHeightSegments <<<
                      { id: me, heightSegments: _ }
                  , phiStart: setPhiStart <<< { id: me, phiStart: _ }
                  , phiLength: setPhiLength <<< { id: me, phiLength: _ }
                  , thetaStart: setThetaStart <<< { id: me, thetaStart: _ }
                  , thetaLength: setThetaLength <<< { id: me, thetaLength: _ }
                  , matrix4: setMatrix4 <<< { id: me, matrix4: _ }
                  , quaternion: setQuaternion <<< { id: me, quaternion: _ }
                  , rotateX: setRotateX <<< { id: me, rotateX: _ }
                  , rotateY: setRotateY <<< { id: me, rotateY: _ }
                  , rotateZ: setRotateZ <<< { id: me, rotateZ: _ }
                  , translate: setTranslate <<< union { id: me }
                  , scale: setScale <<< union { id: me }
                  , lookAt: setLookAt <<< { id: me, v: _ }
                  , center: \_ -> setCenter { id: me }
                  , boundingBox: getBoundingBox <<< { id: me, box: _ }
                  , boundingPerspectiveCamera: getBoundingPerspectiveCamera <<<
                      { id: me, perspectiveCamera: _ }
                  }
                  e
              )
              atts
          )

perspectiveCamera_
  :: forall i lock payload
   . InitialPerspectiveCamera i
  => i
  -> C.Geometry lock payload
perspectiveCamera_ i = perspectiveCamera i empty