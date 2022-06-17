module Rito.BufferAttribute where

import Data.FastVect.FastVect as Vect
import Data.Reflectable (class Reflectable, reflectType)
import Prim.Int (class Mul)
import Rito.THREE as THREE
import Type.Proxy (Proxy(..))

data BufferAttribute

foreign import bufferAttributeImpl
  :: forall n
   . Int
  -> Int
  -> THREE.TBufferAttribute
  -> (Int -> Vect.Vect n Number)
  -> BufferAttribute

bufferAttribute
  :: forall m n mn
   . Reflectable m Int
  => Reflectable n Int
  => Mul m n mn
  => THREE.TBufferAttribute
  -> (Int -> Vect.Vect n Number)
  -> BufferAttribute
bufferAttribute = bufferAttributeImpl (reflectType (Proxy :: _ m))
  (reflectType (Proxy :: _ n))