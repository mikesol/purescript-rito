module Rito.Renderers.WebGL.RenderPass where

import Prelude

import Bolson.EffectFn.Core (Scope(..))
import Bolson.EffectFn.Core as Bolson
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Effect.Ref as Ref
import FRP.Event.EffectFn ( makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE

-- todo: copy-paste from webgl renderer with the avars. fix?
renderPass
  :: forall lock payload
   . { renderPass :: THREE.TRenderPass }
  -> C.Scene lock payload
  -> C.Camera lock payload
  -> C.APass lock payload
renderPass ii sne cam = Bolson.Element' $ C.Pass go
  where
  go
    psr
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeRenderPass
          }
      ) = makeEvent \k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    sceneAvar <- Ref.new Nothing
    cameraAvar <- Ref.new Nothing
    u0 <- subscribe
      ( oneOf
          [ sne # \(C.Scene gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \i -> Ref.write (Just i) sceneAvar
              }
              di
          , cam # \(C.Camera gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \i -> Ref.write (Just i) cameraAvar
              }
              di
          ]
      )
      k0
    sceneLR <- Ref.read sceneAvar
    cameraLR <- Ref.read cameraAvar
    u1 <- case sceneLR of
      Nothing -> pure (pure unit)
      Just sceneId -> case cameraLR of
        Nothing -> pure (pure unit)
        Just cameraId -> subscribe
          ( oneOf
              [ pure $ makeRenderPass
                  { id: me
                  , parent: psr.parent
                  , renderPass: ii.renderPass
                  , camera: cameraId
                  , scene: sceneId
                  }
              ]
          )
          k0
    pure do
      k0 (deleteFromCache { id: me })
      u0
      u1
