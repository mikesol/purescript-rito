module Rito.Geometries.BufferGeometry
  ( bufferGeometry
  , class InitialBufferGeometry
  , BufferGeometryOptions
  , toInitializeBufferGeometry
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

data BufferGeometryOptions = BufferGeometryOptions

instance
  ConvertOption BufferGeometryOptions
    "bufferGeometry"
    THREE.TBufferGeometry
    THREE.TBufferGeometry where
  convertOption _ _ = identity

instance
  ConvertOption BufferGeometryOptions
    "bufferAttributes"
    (Object BufferAttribute)
    (Object BufferAttribute) where
  convertOption _ _ = identity

instance
  ConvertOption BufferGeometryOptions
    "instancedBufferAttributes"
    (Object InstancedBufferAttribute)
    (Object InstancedBufferAttribute) where
  convertOption _ _ = identity

type BufferGeometryOptional =
  ( bufferAttributes :: Object BufferAttribute
  , instancedBufferAttributes :: Object InstancedBufferAttribute
  )

type BufferGeometryAll =
  (bufferGeometry :: THREE.TBufferGeometry | BufferGeometryOptional)

defaultBufferGeometry :: { | BufferGeometryOptional }
defaultBufferGeometry =
  { bufferAttributes: empty
  , instancedBufferAttributes: empty
  }

class InitialBufferGeometry i where
  toInitializeBufferGeometry :: i -> C.InitializeBufferGeometry

instance InitialBufferGeometry C.InitializeBufferGeometry where
  toInitializeBufferGeometry = identity

instance
  ConvertOptionsWithDefaults BufferGeometryOptions { | BufferGeometryOptional }
    { | provided }
    { | BufferGeometryAll } =>
  InitialBufferGeometry { | provided } where
  toInitializeBufferGeometry provided = C.InitializeBufferGeometry
    ( convertOptionsWithDefaults BufferGeometryOptions defaultBufferGeometry
        provided
    )

bufferGeometry
  :: forall i payload
   . InitialBufferGeometry i
  => i
  -> C.Geometry payload
bufferGeometry i' = C.Geometry go
  where
  C.InitializeBufferGeometry i = toInitializeBufferGeometry i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeBufferGeometry
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( pure
          ( makeBufferGeometry
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , bufferGeometry: i.bufferGeometry
              , bufferAttributes: i.bufferAttributes
              , instancedBufferAttributes: i.instancedBufferAttributes
              }
          )
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub
