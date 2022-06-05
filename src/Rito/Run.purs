module Rito.Run where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Scope(..))
import Bolson.Core as Bolson
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import FRP.Event (subscribe)
import Rito.Core as C
import Rito.Interpret (FFIThreeSnapshot, effectfulThreeInterpret, makeFFIThreeSnapshot)
import Rito.THREE as THREE

run
  :: THREE.ThreeStuff
  -> (forall lock. (C.ARenderer lock (FFIThreeSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run threeStuff s = do
  ffi <- makeFFIThreeSnapshot threeStuff
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
