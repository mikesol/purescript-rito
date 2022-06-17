module Rito.InstancedBufferAttribute where

import Data.FastVect.FastVect as Vect
import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int (class Mul)
import Rito.THREE as THREE
import Type.Proxy (Proxy(..))

data InstancedBufferAttribute

foreign import instancedBufferAttributeImpl
  :: forall n
   . Int
  -> Int
  -> THREE.TInstancedBufferAttribute
  -> (Int -> Vect.Vect n Number)
  -> InstancedBufferAttribute

instancedBufferAttribute
  :: forall m n mn
   . Reflectable m Int
  => Reflectable n Int
  => Mul m n mn
  => THREE.TInstancedBufferAttribute
  -> (Int -> Vect.Vect n Number)
  -> InstancedBufferAttribute
instancedBufferAttribute = instancedBufferAttributeImpl (reflectType (Proxy :: _ m))
  (reflectType (Proxy :: _ n))