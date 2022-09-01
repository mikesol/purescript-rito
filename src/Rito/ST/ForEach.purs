module Rito.ST.ForEach
  ( foreachST
  )
  where

import Prelude

import Control.Monad.ST (ST)
import Effect (foreachE)
import Unsafe.Coerce (unsafeCoerce)

foreachST :: forall r a. Array a -> (a -> ST r Unit) -> ST r Unit
foreachST = unsafeCoerce foreachE