module Rito.Renderers.WebGL.EffectComposer where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Entity(..), Scope(..), fixed)
import Bolson.Core as Bolson
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import Effect.Ref as Ref
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.THREE as THREE

type EffectComposer' = Variant (render :: Unit)

newtype EffectComposer = EffectComposer EffectComposer'
instance Newtype EffectComposer EffectComposer'

effectComposer
  :: forall lock payload
   . { effectComposer :: THREE.TEffectComposer
     }
  -- todo: we need to enforce this being a webgl renderer!
  -> C.Renderer lock payload
  -> Event EffectComposer
  -> Array (C.APass lock payload)
  -> C.ARenderer lock payload
effectComposer i rndr props kidz = Element' $ C.Renderer go
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
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    scope <- ids
    rendererAvar <- Ref.new Nothing
    u0 <- subscribe
      ( oneOf
          [ rndr # \(C.Renderer gooo) -> gooo
              { parent: Just me
              , scope: Local scope
              , raiseId: \iii -> Ref.write (Just iii) rendererAvar
              }
              di
          ]
      )
      k
    rendererLR <- Ref.read rendererAvar
    u1 <- case rendererLR of
      Nothing -> pure (pure unit)
      Just rendererId -> k # subscribe
        ( oneOf
            [ bang $ makeEffectComposer
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
                { parent: Just me, scope: parent.scope, raiseId: pure mempty }
                di
                (fixed kidz)
            ]
        )
    pure (k (deleteFromCache { id: me }) *> u0 *> u1)
