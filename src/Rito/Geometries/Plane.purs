module Rito.Geometries.Plane
  ( plane
  , class InitialPlane
  , PlaneOptions
  , toInitializePlane
  ) where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import FRP.Event (bang, makeEvent, subscribe)
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
  :: forall i lock payload
   . InitialPlane i
  => i
  -> C.Geometry lock payload
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
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
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
