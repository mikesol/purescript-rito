module Rito.InstancedBufferAttribute
  ( instancedBufferAttribute
  , instancedBufferAttributes
  , class InstancedBufferAttributes
  , internalDoNotUseBA
  , InstancedBufferAttribute
  ) where

import Prelude

import Data.Array as Array
import Data.FastVect.FastVect as Vect
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Prim.Int (class Mul)
import Prim.RowList as RL
import Rito.BufferAttributeUnsafe (bufferAttributeImpl, bufferAttributesImpl)
import Rito.THREE as THREE
import Type.Proxy (Proxy(..))

data InstancedBufferAttribute

instancedBufferAttribute
  :: forall m n mn
   . Reflectable m Int
  => Reflectable n Int
  => Mul m n mn
  => Proxy m
  -> THREE.TInstancedBufferAttribute
  -> (Int -> Vect.Vect n Number)
  -> InstancedBufferAttribute
instancedBufferAttribute px = bufferAttributeImpl (reflectType px)
  (reflectType (Proxy :: _ n))

class
  InstancedBufferAttributes (i :: RL.RowList Type) (o :: RL.RowList Type)
  | i -> o where
  internalDoNotUseBA :: Proxy i -> (Array { k :: String, n :: Int }) /\ Proxy o

instance InstancedBufferAttributes RL.Nil RL.Nil where
  internalDoNotUseBA _ = [] /\ Proxy

instance
  ( InstancedBufferAttributes restI restO
  , IsSymbol i
  , Reflectable n Int
  ) =>
  InstancedBufferAttributes (RL.Cons i (Vect.Vect n Number) restI)
    (RL.Cons i InstancedBufferAttribute restO) where
  internalDoNotUseBA _ =
    Array.cons
      { k: reflectSymbol (Proxy :: _ i), n: reflectType (Proxy :: _ n) }
      tail /\ Proxy
    where
    tail /\ _ = internalDoNotUseBA (Proxy :: _ restI)

instancedBufferAttributes
  :: forall a m i ir o or
   . Reflectable m Int
  => RL.RowToList i ir
  => RL.RowToList o or
  => InstancedBufferAttributes ir or
  => Proxy m
  -> THREE.TInstancedBufferAttribute
  -> a
  -> (Int -> a -> { | i } /\ a)
  -> { | o }
instancedBufferAttributes px ba a f = bufferAttributesImpl (reflectType px)
  (fst (internalDoNotUseBA (Proxy :: _ ir)))
  ba
  a
  ((map <<< map) (\(l /\ r) -> { l, r }) f)