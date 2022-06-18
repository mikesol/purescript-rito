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
  => Proxy m
  -> THREE.TBufferAttribute
  -> (Int -> Vect.Vect n Number)
  -> BufferAttribute
bufferAttribute px = bufferAttributeImpl (reflectType px)
  (reflectType (Proxy :: _ n))