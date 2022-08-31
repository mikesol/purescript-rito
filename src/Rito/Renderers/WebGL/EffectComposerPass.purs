module Rito.Renderers.WebGL.EffectComposerPass where

import Prelude

import Bolson.EffectFn.Core (Scope(..))
import Bolson.EffectFn.Core as Bolson
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Effect.Ref as Ref
import FRP.Event.EffectFn ( makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE

-- todo: copy-paste from webgl effectComposerer with the avars. fix?
effectComposerPass
  :: forall lock payload
   . { effectComposerPass :: THREE.TEffectComposerPass }
  -> C.EffectComposer lock payload
  -> C.APass lock payload
effectComposerPass ii ecomp = Bolson.Element' $ C.Pass go
  where
  go
    psr
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeEffectComposerPass
          }
      ) = makeEvent \k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    effectComposerAvar <- Ref.new Nothing
    u0 <- subscribe
      ( oneOf
          [ ecomp # \(C.EffectComposer gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \i -> Ref.write (Just i) effectComposerAvar
              }
              di
          ]
      )
      k0
    effectComposerLR <- Ref.read effectComposerAvar
    u1 <- case effectComposerLR of
      Nothing -> pure (pure unit)
      Just effectComposerId -> subscribe
          ( oneOf
              [ pure $ makeEffectComposerPass
                  { id: me
                  , parent: psr.parent
                  , effectComposerPass: ii.effectComposerPass
                  , effectComposer: effectComposerId
                  }
              ]
          )
          k0
    pure do
      k0 (deleteFromCache { id: me })
      u0
      u1
