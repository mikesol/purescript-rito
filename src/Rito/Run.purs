module Rito.Run where

import Prelude

import Effect (Effect)
import FRP.Event (Event, keepLatest, subscribe)
import Rito.Core as C
import Rito.Interpret (FFIThreeSnapshot, effectfulThreeInterpret, makeFFIThreeSnapshot)

run
  :: (forall lock. (C.Renderer lock (FFIThreeSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
run (C.Renderer s) = do
  ffi <- makeFFIThreeSnapshot
  u <- subscribe (s effectfulThreeInterpret) (_ $ ffi)
  pure u

runE
  :: (forall lock. Event (C.Renderer lock (FFIThreeSnapshot -> Effect Unit)))
  -> Effect (Effect Unit)
runE s = do
  ffi <- makeFFIThreeSnapshot
  u <- subscribe
    (keepLatest (map (\(C.Renderer i) -> i effectfulThreeInterpret) s))
    (_ $ ffi)
  pure u
