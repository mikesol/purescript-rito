module Rito.BufferAttributeUnsafe where

import Data.FastVect.FastVect as Vect

foreign import bufferAttributeImpl
  :: forall n ctor buf
   . Int
  -> Int
  -> ctor
  -> (Int -> Vect.Vect n Number)
  -> buf

foreign import bufferAttributesImpl
  :: forall a i o ctor
   . Int
  -> Array { k :: String, n :: Int }
  -> ctor
  -> a
  -> (Int -> a -> {l :: { | i }, r :: a })
  -> { | o }
