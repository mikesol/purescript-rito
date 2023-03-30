module Rito.Geometries.Sphere
  ( sphere
  , class InitialSphere
  , SphereOptions
  , toInitializeSphere
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

twoPi = pi * 2.0 :: Number
data SphereOptions = SphereOptions

instance
  ConvertOption SphereOptions
    "sphere"
    THREE.TSphereGeometry
    THREE.TSphereGeometry where
  convertOption _ _ = identity

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

instance
  ConvertOption SphereOptions
    "bufferAttributes"
    (Object BufferAttribute)
    (Object BufferAttribute) where
  convertOption _ _ = identity

instance
  ConvertOption SphereOptions
    "instancedBufferAttributes"
    (Object InstancedBufferAttribute)
    (Object InstancedBufferAttribute) where
  convertOption _ _ = identity

type SphereOptional =
  ( radius :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , phiStart :: Number
  , phiLength :: Number
  , thetaStart :: Number
  , thetaLength :: Number
  , bufferAttributes :: Object BufferAttribute
  , instancedBufferAttributes :: Object InstancedBufferAttribute
  )

type SphereAll =
  (sphere :: THREE.TSphereGeometry | SphereOptional)

defaultSphere :: { | SphereOptional }
defaultSphere =
  { radius: 1.0
  , widthSegments: 32
  , heightSegments: 16
  , phiStart: zero
  , phiLength: twoPi
  , thetaStart: zero
  , thetaLength: twoPi
  , bufferAttributes: empty
  , instancedBufferAttributes: empty
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

sphere
  :: forall i payload
   . InitialSphere i
  => i
  -> C.Geometry payload
sphere i' = C.Geometry go
  where
  C.InitializeSphere i = toInitializeSphere i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeSphere
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( pure
          ( makeSphere
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , sphere: i.sphere
              , radius: i.radius
              , widthSegments: i.widthSegments
              , heightSegments: i.heightSegments
              , phiStart: i.phiStart
              , phiLength: i.phiLength
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
