module Rito.Renderers.Raycaster where

import Prelude

import Bolson.Core (Scope(..))
import Bolson.Core as Bolson
import Control.Monad.ST.Internal as Ref
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import FRP.Event (makePureEvent, subscribePure)
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
      ) = makePureEvent \k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    cameraAvar <- Ref.new Nothing
    u0 <- subscribePure
      ( oneOf
          [ cam # \(C.Camera gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \ii -> void $ Ref.write (Just ii) cameraAvar
              }
              di
          ]
      )
      k0
    cameraLR <- Ref.read cameraAvar
    u1 <- case cameraLR of
      Nothing -> pure (pure unit)
      Just cameraId -> subscribePure
        ( oneOf
            [ pure $ makeRaycaster
                { id: me
                , raycaster: i.raycaster
                , canvas: i.canvas
                , camera: cameraId
                }
            ]
        )
        k0
    pure do
      k0 (deleteFromCache { id: me })
      u0
      u1
