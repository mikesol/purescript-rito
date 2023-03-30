module Rito.Geometries.Capsule
  ( capsule
  , class InitialCapsule
  , CapsuleOptions
  , toInitializeCapsule
  ) where

import Prelude

import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import FRP.Event (Subscriber(..), makeLemmingEventO)
import Foreign.Object (Object, empty)
import Rito.BufferAttribute (BufferAttribute)
import Rito.Core as C
import Rito.InstancedBufferAttribute (InstancedBufferAttribute)
import Rito.THREE as THREE

data CapsuleOptions = CapsuleOptions

instance
  ConvertOption CapsuleOptions
    "capsule"
    THREE.TCapsuleGeometry
    THREE.TCapsuleGeometry where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "radius"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "length"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "radialSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "capSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "bufferAttributes"
    (Object BufferAttribute)
    (Object BufferAttribute) where
  convertOption _ _ = identity

instance
  ConvertOption CapsuleOptions
    "instancedBufferAttributes"
    (Object InstancedBufferAttribute)
    (Object InstancedBufferAttribute) where
  convertOption _ _ = identity

type CapsuleOptional =
  ( radius :: Number
  , length :: Number
  , radialSegments :: Int
  , capSegments :: Int
  , bufferAttributes :: Object BufferAttribute
  , instancedBufferAttributes :: Object InstancedBufferAttribute
  )

type CapsuleAll =
  (capsule :: THREE.TCapsuleGeometry | CapsuleOptional)

defaultCapsule :: { | CapsuleOptional }
defaultCapsule =
  { radius: 1.0
  , length: 1.0
  , radialSegments: 4
  , capSegments: 8
  , bufferAttributes: empty
  , instancedBufferAttributes: empty
  }

class InitialCapsule i where
  toInitializeCapsule :: i -> C.InitializeCapsule

instance InitialCapsule C.InitializeCapsule where
  toInitializeCapsule = identity

instance
  ConvertOptionsWithDefaults CapsuleOptions { | CapsuleOptional } { | provided }
    { | CapsuleAll } =>
  InitialCapsule { | provided } where
  toInitializeCapsule provided = C.InitializeCapsule
    (convertOptionsWithDefaults CapsuleOptions defaultCapsule provided)

capsule
  :: forall i payload
   . InitialCapsule i
  => i
  -> C.Geometry payload
capsule i' = C.Geometry go
  where
  C.InitializeCapsule i = toInitializeCapsule i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeCapsule
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( pure
          ( makeCapsule
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , capsule: i.capsule
              , radius: i.radius
              , length: i.length
              , radialSegments: i.radialSegments
              , capSegments: i.capSegments
              , bufferAttributes: i.bufferAttributes
              , instancedBufferAttributes: i.instancedBufferAttributes
              }
          )
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub
