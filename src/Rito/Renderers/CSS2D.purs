module Rito.Renderers.CSS2D where

import Prelude

import Bolson.Core (Scope(..))
import Bolson.Core as Bolson
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Effect.Ref as Ref
import FRP.Event (Event, bang, makeEvent, subscribe)
import Record (union)
import Rito.Core as C
import Rito.THREE as THREE
import Web.DOM as Web.DOM
import Web.HTML (HTMLCanvasElement)

type CSS2DRenderer' = Variant
  ( render :: Unit
  , size :: { width :: Number, height :: Number }
  -- todo, add shadow map & other goodies
  )
newtype CSS2DRenderer = CSS2DRenderer CSS2DRenderer'
instance Newtype CSS2DRenderer CSS2DRenderer'

css2DRenderer
  :: forall lock payload
   . C.Scene lock payload
  -> C.Camera lock payload
  -> { canvas :: HTMLCanvasElement
     , element :: Web.DOM.Element
     , css2DRenderer :: THREE.TCSS2DRenderer
     }
  -> Event CSS2DRenderer
  -> C.ARenderer lock payload
css2DRenderer sne cam make props = Bolson.Element' $ C.Renderer go
  where
  go
    psr
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeCSS2DRenderer
          , css2DRender
          , setSize
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
    -- elt <- window >>= document >>= createElement "div" <<< toDocument
    -- dku <- runInElement' elt nut
    u1 <- case sceneLR of
      Nothing -> pure (pure unit)
      Just sceneId -> case cameraLR of
        Nothing -> pure (pure unit)
        Just cameraId -> subscribe
          ( oneOf
              [ bang $ makeCSS2DRenderer
                  $ union
                    { id: me
                    , camera: cameraId
                    }
                    make
              , makeEvent \k -> do
                  usuRef <- Ref.new mempty
                  -- ugh, there's got to be a better way...
                  unsub <- subscribe
                    ( props <#>
                        ( \(CSS2DRenderer msh) ->
                            msh # match
                              { render: \_ -> css2DRender
                                  { id: me
                                  , scene: sceneId
                                  , camera: cameraId
                                  }
                              , size: \{ width, height } -> setSize
                                  { id: me, width, height }
                              }
                        )
                    )
                    k
                  Ref.write unsub usuRef
                  pure do
                    usu <- Ref.read usuRef
                    usu
              ]
          )
          k0
    pure (k0 (deleteFromCache { id: me }) *> u0 *> u1)
