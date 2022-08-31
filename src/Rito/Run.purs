module Rito.Run where

import Prelude

import Bolson.EffectFn.Control (flatten)
import Bolson.EffectFn.Core (Scope(..))
import Bolson.EffectFn.Core as Bolson
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import FRP.Event.EffectFn (subscribe)
import Rito.Core as C
import Rito.Interpret (FFIThreeSnapshot, effectfulThreeInterpret, makeFFIThreeSnapshot)

run
  :: (forall lock. (C.ARenderer lock (FFIThreeSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run s = do
  ffi <- makeFFIThreeSnapshot
  u <- subscribe
    ( flatten
        { doLogic: absurd
        , ids: unwrap >>> _.ids
        , disconnectElement: unwrap >>> _.disconnect
        , toElt: \(C.Renderer obj) -> Bolson.Element obj
        }
        { parent: Nothing, scope: Global, raiseId: pure mempty }
        effectfulThreeInterpret
        s
    )
    (_ $ ffi)
  pure u
