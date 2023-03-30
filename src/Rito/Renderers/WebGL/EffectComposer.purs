module Rito.Renderers.WebGL.EffectComposer where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Scope(..), fixed)
import Bolson.Core as Bolson
import Control.Monad.ST.Internal as Ref
import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Rito.Core as C
import Rito.THREE as THREE

type EffectComposer' = Variant (render :: Unit)

newtype EffectComposer = EffectComposer EffectComposer'
instance Newtype EffectComposer EffectComposer'

effectComposer
  :: forall payload
   . { effectComposer :: THREE.TEffectComposer }
  -> C.WebGLRenderer payload
  -> Event EffectComposer
  -> Array (C.APass payload)
  -> C.EffectComposer payload
effectComposer i rndr props kidz = C.EffectComposer go
  where
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeEffectComposer
          , effectComposerRender
          }
      ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    scope <- ids
    rendererAvar <- Ref.new Nothing
    u0 <- runSTFn2 mySub
      ( oneOf
          [ rndr # \(C.WebGLRenderer gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \iii -> void $ Ref.write (Just iii) rendererAvar
              }
              di
          ]
      )
      k
    rendererLR <- Ref.read rendererAvar
    u1 <- case rendererLR of
      Nothing -> pure (pure unit)
      Just rendererId -> runSTFn2 mySub
        ( oneOf
            [ pure $ makeEffectComposer
                { id: me
                , effectComposer: i.effectComposer
                , webGLRenderer: rendererId
                }
            , props <#>
                ( \(EffectComposer msh) ->
                    msh # match
                      { render: \_ -> effectComposerRender
                          { id: me }
                      }
                )
            , flatten
                { doLogic: absurd
                , ids: unwrap >>> _.ids
                , disconnectElement: unwrap >>> _.disconnectPass
                , toElt: \(C.Pass obj) -> Bolson.Element obj
                }
                { parent: Just me, scope: parent.scope, raiseId: \_ -> pure unit }
                di
                (fixed kidz)
            ]
        ) k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      u0
      u1
