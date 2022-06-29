module Rito.Renderers.Raycaster where

import Prelude

-- A Raycaster is not a renderer in THREE land, so this is a bit hackish.
-- However, it is a top-level object, and you can sort of think of it as "rendering"
-- something: it renders k-rate data. We make it a renderer in rito so it can be part
-- of the final renderer array.

import Bolson.Core (Scope(..))
import Bolson.Core as Bolson
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Effect.Ref as Ref
import FRP.Event (bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE
import Web.HTML (HTMLCanvasElement)

raycaster
  :: forall lock payload
   . { raycaster :: THREE.TRaycaster, canvas :: HTMLCanvasElement }
  -> C.Camera lock payload
  -> C.ARenderer lock payload
raycaster i cam = Bolson.Element' $ C.Renderer go
  where
  go
    psr
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeRaycaster
          }
      ) = makeEvent \k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    cameraAvar <- Ref.new Nothing
    u0 <- subscribe
      ( oneOf
          [ cam # \(C.Camera gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \ii -> Ref.write (Just ii) cameraAvar
              }
              di
          ]
      )
      k0
    cameraLR <- Ref.read cameraAvar
    u1 <- case cameraLR of
      Nothing -> pure (pure unit)
      Just cameraId -> subscribe
        ( oneOf
            [ bang $ makeRaycaster
                { id: me
                , raycaster: i.raycaster
                , canvas: i.canvas
                , camera: cameraId
                }
            ]
        )
        k0
    pure (k0 (deleteFromCache { id: me }) *> u0 *> u1)
