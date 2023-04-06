module Rito.Geometries.Box
  ( box
  , class InitialBox
  , BoxOptions
  , toInitializeBox
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

data BoxOptions = BoxOptions

instance
  ConvertOption BoxOptions
    "box"
    THREE.TBoxGeometry
    THREE.TBoxGeometry where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "width"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "height"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "depth"
    Number
    Number where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "widthSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "heightSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "depthSegments"
    Int
    Int where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "bufferAttributes"
    (Object BufferAttribute)
    (Object BufferAttribute) where
  convertOption _ _ = identity

instance
  ConvertOption BoxOptions
    "instancedBufferAttributes"
    (Object InstancedBufferAttribute)
    (Object InstancedBufferAttribute) where
  convertOption _ _ = identity

type BoxOptional =
  ( width :: Number
  , height :: Number
  , depth :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , depthSegments :: Int
  , bufferAttributes :: Object BufferAttribute
  , instancedBufferAttributes :: Object InstancedBufferAttribute
  )

type BoxAll =
  (box :: THREE.TBoxGeometry | BoxOptional)

defaultBox :: { | BoxOptional }
defaultBox =
  { width: 1.0
  , height: 1.0
  , depth: 1.0
  , widthSegments: 1
  , heightSegments: 1
  , depthSegments: 1
  , bufferAttributes: empty
  , instancedBufferAttributes: empty
  }

class InitialBox i where
  toInitializeBox :: i -> C.InitializeBox

instance InitialBox C.InitializeBox where
  toInitializeBox = identity

instance
  ConvertOptionsWithDefaults BoxOptions { | BoxOptional } { | provided }
    { | BoxAll } =>
  InitialBox { | provided } where
  toInitializeBox provided = C.InitializeBox
    (convertOptionsWithDefaults BoxOptions defaultBox provided)

box
  :: forall i payload
   . InitialBox i
  => i
  -> C.Geometry payload
box i' = C.Geometry go
  where
  C.InitializeBox i = toInitializeBox i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeBox
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( pure
          ( makeBox
              { id: me
              , parent: parent.parent
              , scope: parent.scope
              , box: i.box
              , width: i.width
              , height: i.height
              , depth: i.depth
              , widthSegments: i.widthSegments
              , heightSegments: i.heightSegments
              , depthSegments: i.depthSegments
              , bufferAttributes: i.bufferAttributes
              , instancedBufferAttributes: i.instancedBufferAttributes
              }
          )
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub
