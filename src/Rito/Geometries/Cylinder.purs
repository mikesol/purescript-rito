module Rito.Geometries.Cylinder
  ( cylinder
  , class InitialCylinder
  , CylinderOptions
  , toInitializeCylinder
  ) where

import Prelude

import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Number (pi)
import FRP.Event (Subscriber(..), makeLemmingEventO)
import Foreign.Object (Object, empty)
import Rito.BufferAttribute (BufferAttribute)
import Rito.Core as C
import Rito.InstancedBufferAttribute (InstancedBufferAttribute)
import Rito.THREE as THREE

data CylinderOptions = CylinderOptions

instance
  ConvertOption CylinderOptions
    "cylinder"
    THREE.TCylinderGeometry
    THREE.TCylinderGeometry where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "radiusTop"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "radiusBottom"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "height"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "radialSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "heightSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "openEnded"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "thetaStart"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "thetaLength"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "bufferAttributes"
    (Object BufferAttribute)
    (Object BufferAttribute) where
  convertOption _ _ = identity

instance
  ConvertOption CylinderOptions
    "instancedBufferAttributes"
    (Object InstancedBufferAttribute)
    (Object InstancedBufferAttribute) where
  convertOption _ _ = identity

type CylinderOptional =
  ( radiusTop :: Number
  , radiusBottom :: Number
  , height :: Number
  , radialSegments :: Int
  , heightSegments :: Int
  , openEnded :: Boolean
  , thetaStart :: Number
  , thetaLength :: Number
  , bufferAttributes :: Object BufferAttribute
  , instancedBufferAttributes :: Object InstancedBufferAttribute
  )

type CylinderAll =
  (cylinder :: THREE.TCylinderGeometry | CylinderOptional)

defaultCylinder :: { | CylinderOptional }
defaultCylinder =
  { radiusTop: 1.0
  , radiusBottom: 1.0
  , height: 1.0
  , radialSegments: 8
  , heightSegments: 1
  , openEnded: false
  , thetaStart: 0.0
  , thetaLength: 2.0 * pi
  , bufferAttributes: empty
  , instancedBufferAttributes: empty
  }

class InitialCylinder i where
  toInitializeCylinder :: i -> C.InitializeCylinder

instance InitialCylinder C.InitializeCylinder where
  toInitializeCylinder = identity

instance
  ConvertOptionsWithDefaults CylinderOptions { | CylinderOptional }
    { | provided }
    { | CylinderAll } =>
  InitialCylinder { | provided } where
  toInitializeCylinder provided = C.InitializeCylinder
    (convertOptionsWithDefaults CylinderOptions defaultCylinder provided)

cylinder
  :: forall i payload
   . InitialCylinder i
  => i
  -> C.Geometry payload
cylinder i' = C.Geometry go
  where
  C.InitializeCylinder i = toInitializeCylinder i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeCylinder
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( pure
          ( makeCylinder
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , cylinder: i.cylinder
              , radiusTop: i.radiusTop
              , radiusBottom: i.radiusBottom
              , height: i.height
              , radialSegments: i.radialSegments
              , heightSegments: i.heightSegments
              , openEnded: i.openEnded
              , thetaStart: i.thetaStart
              , thetaLength: i.thetaLength
              , bufferAttributes: i.bufferAttributes
              , instancedBufferAttributes: i.instancedBufferAttributes
              }
          )
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub
