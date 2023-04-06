module Rito.Run where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Scope(..))
import Bolson.Core as Bolson
import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as RRef
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import FRP.Event (subscribe)
import Rito.Core as C
import Rito.Interpret (FFIThreeSnapshot, effectfulThreeInterpret, makeFFIThreeSnapshot)

run
  :: (forall lock. (C.ARenderer (EffectFn1 FFIThreeSnapshot Unit)))
  -> Effect (Effect Unit)
run s = do
  ffi <- makeFFIThreeSnapshot
  rf <- liftST $ RRef.new 0
  u <- subscribe
    ( flatten
        { doLogic: absurd
        , ids: unwrap >>> _.ids
        , disconnectElement: unwrap >>> _.disconnect
        , toElt: \(C.Renderer obj) -> Bolson.Element obj
        }
        { parent: Nothing, scope: Global, raiseId: \_ -> pure unit }
        (effectfulThreeInterpret rf)
        s
    )
    (\f -> runEffectFn1 f ffi)
  pure u
