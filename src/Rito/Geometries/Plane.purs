module Rito.Geometries.Plane
  ( plane
  , class InitialPlane
  , PlaneOptions
  , toInitializePlane
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

data PlaneOptions = PlaneOptions

instance
  ConvertOption PlaneOptions
    "plane"
    THREE.TPlaneGeometry
    THREE.TPlaneGeometry where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "width"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "height"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "widthSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "heightSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "bufferAttributes"
    (Object BufferAttribute)
    (Object BufferAttribute) where
  convertOption _ _ = identity

instance
  ConvertOption PlaneOptions
    "instancedBufferAttributes"
    (Object InstancedBufferAttribute)
    (Object InstancedBufferAttribute) where
  convertOption _ _ = identity

type PlaneOptional =
  ( width :: Number
  , height :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , bufferAttributes :: Object BufferAttribute
  , instancedBufferAttributes :: Object InstancedBufferAttribute
  )

type PlaneAll =
  (plane :: THREE.TPlaneGeometry | PlaneOptional)

defaultPlane :: { | PlaneOptional }
defaultPlane =
  { width: 1.0
  , height: 1.0
  , widthSegments: 1
  , heightSegments: 1
  , bufferAttributes: empty
  , instancedBufferAttributes: empty
  }

class InitialPlane i where
  toInitializePlane :: i -> C.InitializePlane

instance InitialPlane C.InitializePlane where
  toInitializePlane = identity

instance
  ConvertOptionsWithDefaults PlaneOptions { | PlaneOptional } { | provided }
    { | PlaneAll } =>
  InitialPlane { | provided } where
  toInitializePlane provided = C.InitializePlane
    (convertOptionsWithDefaults PlaneOptions defaultPlane provided)

plane
  :: forall i payload
   . InitialPlane i
  => i
  -> C.Geometry payload
plane i' = C.Geometry go
  where
  C.InitializePlane i = toInitializePlane i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makePlane
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( pure
          ( makePlane
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , plane: i.plane
              , width: i.width
              , height: i.height
              , widthSegments: i.widthSegments
              , heightSegments: i.heightSegments
              , bufferAttributes: i.bufferAttributes
              , instancedBufferAttributes: i.instancedBufferAttributes
              }
          )
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub
