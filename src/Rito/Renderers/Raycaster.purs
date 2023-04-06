module Rito.Renderers.Raycaster where

import Prelude

import Bolson.Core (Scope(..))
import Bolson.Core as Bolson
import Control.Monad.ST.Internal as Ref
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import FRP.Event (Subscriber(..), makeLemmingEventO)
import Rito.Core as C
import Rito.THREE as THREE
import Web.HTML (HTMLCanvasElement)

raycaster
  :: forall payload
   . { raycaster :: THREE.TRaycaster, canvas :: HTMLCanvasElement }
  -> C.Camera payload
  -> C.ARenderer payload
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
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    cameraAvar <- Ref.new Nothing
    u0 <- runSTFn2 mySub
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
      Just cameraId -> runSTFn2 mySub
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
      runSTFn1 k0 (deleteFromCache { id: me })
      u0
      u1
