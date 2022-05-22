module Rito.Run where

import Prelude

import Effect (Effect)
import FRP.Event (Event, keepLatest, subscribe)
import Rito.Core as C
import Rito.Interpret (FFIThreeSnapshot, effectfulThreeInterpret, makeFFIThreeSnapshot)
import Rito.THREE as THREE

run
  :: THREE.ThreeStuff
  -> (forall lock. (C.Renderer lock (FFIThreeSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run threeStuff (C.Renderer s) = do
  ffi <- makeFFIThreeSnapshot threeStuff
  u <- subscribe (s effectfulThreeInterpret) (_ $ ffi)
  pure u

runE
  :: THREE.ThreeStuff
  -> (forall lock. Event (C.Renderer lock (FFIThreeSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
runE threeStuff s = do
  ffi <- makeFFIThreeSnapshot threeStuff
  u <- subscribe
    (keepLatest (map (\(C.Renderer i) -> i effectfulThreeInterpret) s))
    (_ $ ffi)
  pure u
