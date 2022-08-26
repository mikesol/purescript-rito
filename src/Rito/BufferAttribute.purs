module Rito.BufferAttribute
  ( bufferAttribute
  , bufferAttributes
  , class BufferAttributes
  , internalDoNotUseBA
  , BufferAttribute
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

data BufferAttribute

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

class
  BufferAttributes (i :: RL.RowList Type) (o :: RL.RowList Type)
  | i -> o where
  internalDoNotUseBA :: Proxy i -> (Array { k :: String, n :: Int }) /\ Proxy o

instance BufferAttributes RL.Nil RL.Nil where
  internalDoNotUseBA _ = [] /\ Proxy

instance
  ( BufferAttributes restI restO
  , IsSymbol i
  , Reflectable n Int
  ) =>
  BufferAttributes (RL.Cons i (Vect.Vect n Number) restI)
    (RL.Cons i BufferAttribute restO) where
  internalDoNotUseBA _ =
    Array.cons
      { k: reflectSymbol (Proxy :: _ i), n: reflectType (Proxy :: _ n) }
      tail /\ Proxy
    where
    tail /\ _ = internalDoNotUseBA (Proxy :: _ restI)

bufferAttributes
  :: forall a m i ir o or
   . Reflectable m Int
  => RL.RowToList i ir
  => RL.RowToList o or
  => BufferAttributes ir or
  => Proxy m
  -> THREE.TBufferAttribute
  -> a
  -> (Int -> a -> { | i } /\ a)
  -> { | o }
bufferAttributes px ba a f =
  bufferAttributesImpl (reflectType px)
    (fst (internalDoNotUseBA (Proxy :: _ ir)))
    ba
    a
    ( (map <<< map)
        (\(l /\ r) -> { l, r })
        f
    )