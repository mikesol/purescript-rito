module Rito.Renderers.WebGL.EffectComposerPass where

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

-- todo: copy-paste from webgl effectComposerer with the avars. fix?
effectComposerPass
  :: forall payload
   . { effectComposerPass :: THREE.TEffectComposerPass }
  -> C.EffectComposer payload
  -> C.APass payload
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
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    effectComposerAvar <- Ref.new Nothing
    u0 <- runSTFn2 mySub
      ( oneOf
          [ ecomp # \(C.EffectComposer gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \i -> void $ Ref.write (Just i) effectComposerAvar
              }
              di
          ]
      )
      k0
    effectComposerLR <- Ref.read effectComposerAvar
    u1 <- case effectComposerLR of
      Nothing -> pure (pure unit)
      Just effectComposerId -> runSTFn2 mySub
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
      runSTFn1 k0 (deleteFromCache { id: me })
      u0
      u1
