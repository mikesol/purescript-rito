module Rito.Undefinable where

import Prelude

import Data.Maybe (Maybe(..))
import Unsafe.Coerce (unsafeCoerce)

data Undefinable (a :: Type)

foreign import undefinedImpl :: forall a. Undefinable a

m2u :: Maybe ~> Undefinable
m2u (Just x) = unsafeCoerce x
m2u Nothing = undefinedImpl