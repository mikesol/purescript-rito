module Rito.Geometries.Sphere
  ( sphere
  , sphere_
  , Sphere
  , class InitialSphere
  , toInitializeSphere
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
import Rito.Sphere as Sphere
import Rito.Vector3 (Vector3)

twoPi = pi * 2.0 :: Number
data SphereOptions = SphereOptions

instance
  ConvertOption SphereOptions
    "radius"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "widthSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "heightSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "phiStart"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "phiLength"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "thetaStart"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "thetaLength"
    Number
    Number where
  convertOption _ _ = identity

type SphereOptional =
  ( radius :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , phiStart :: Number
  , phiLength :: Number
  , thetaStart :: Number
  , thetaLength :: Number
  )

type SphereAll =
  (| SphereOptional)

defaultSphere :: { | SphereOptional }
defaultSphere =
  { radius: 50.0
  , widthSegments: 8
  , heightSegments: 6
  , phiStart: zero
  , phiLength: twoPi
  , thetaStart: zero
  , thetaLength: twoPi
  }

class InitialSphere i where
  toInitializeSphere :: i -> C.InitializeSphere

instance InitialSphere C.InitializeSphere where
  toInitializeSphere = identity

instance
  ConvertOptionsWithDefaults SphereOptions { | SphereOptional } { | provided }
    { | SphereAll } =>
  InitialSphere { | provided } where
  toInitializeSphere provided = C.InitializeSphere
    (convertOptionsWithDefaults SphereOptions defaultSphere provided)

newtype Sphere = Sphere
  ( Variant
      ( radius :: Number
      , widthSegments :: Int
      , heightSegments :: Int
      , phiStart :: Number
      , phiLength :: Number
      , thetaStart :: Number
      , thetaLength :: Number
      , matrix4 :: Matrix4
      , quaternion :: Quaternion
      , rotateX :: Number
      , rotateY :: Number
      , rotateZ :: Number
      , translate :: { x :: Number, y :: Number, z :: Number }
      , scale :: { x :: Number, y :: Number, z :: Number }
      , lookAt :: Vector3
      , center :: Unit
      , boundingBox :: Box.Box -> Effect Unit
      , boundingSphere :: Sphere.Sphere -> Effect Unit
      )
  )

sphere
  :: forall i lock payload
   . InitialSphere i
  => i
  -> Event Sphere
  -> C.Geometry lock payload
sphere i' atts = C.Geometry go
  where
  C.InitializeSphere i = toInitializeSphere i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeSphere
        , setRadius
        , setWidthSegments
        , setHeightSegments
        , setPhiStart
        , setPhiLength
        , setThetaStart
        , setThetaLength
        , setMatrix4
        , setQuaternion
        , setRotateX
        , setRotateY
        , setRotateZ
        , setTranslate
        , setScale
        , setLookAt
        , setCenter
        , getBoundingBox
        , getBoundingSphere
        }
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeSphere
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
              ( \(Sphere e) -> match
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
                  , boundingSphere: getBoundingSphere <<< { id: me, sphere: _ }
                  }
                  e
              )
              atts
          )

sphere_
  :: forall i lock payload
   . InitialSphere i
  => i
  -> C.Geometry lock payload
sphere_ i = sphere i empty