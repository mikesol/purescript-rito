module Rito.Renderers.WebGL.RenderPass where

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

-- todo: copy-paste from webgl renderer with the avars. fix?
renderPass
  :: forall payload
   . { renderPass :: THREE.TRenderPass }
  -> C.Scene payload
  -> C.Camera payload
  -> C.APass payload
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
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    sceneAvar <- Ref.new Nothing
    cameraAvar <- Ref.new Nothing
    u0 <- runSTFn2 mySub
      ( oneOf
          [ sne # \(C.Scene gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \i -> void $ Ref.write (Just i) sceneAvar
              }
              di
          , cam # \(C.Camera gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \i -> void $ Ref.write (Just i) cameraAvar
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
        Just cameraId -> runSTFn2 mySub
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
      runSTFn1 k0 (deleteFromCache { id: me })
      u0
      u1
