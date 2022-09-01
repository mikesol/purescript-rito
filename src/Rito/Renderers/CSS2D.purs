module Rito.Renderers.CSS2D where

import Prelude

import Bolson.Core (Scope(..))
import Bolson.Core as Bolson
import Control.Monad.ST.Internal as Ref
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, makeLemmingEvent)
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
      ) = makeLemmingEvent \mySub k0 -> do
    me <- ids
    psr.raiseId me
    scope <- ids
    sceneAvar <- Ref.new Nothing
    cameraAvar <- Ref.new Nothing
    u0 <- mySub
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
    -- elt <- window >>= document >>= createElement "div" <<< toDocument
    -- dku <- runInElement' elt nut
    u1 <- case sceneLR of
      Nothing -> pure (pure unit)
      Just sceneId -> case cameraLR of
        Nothing -> pure (pure unit)
        Just cameraId -> mySub
          ( oneOf
              [ pure $ makeCSS2DRenderer
                  $ union
                    { id: me
                    , camera: cameraId
                    }
                    make
              , makeLemmingEvent \mySub k -> do
                  usuRef <- Ref.new (pure unit)
                  -- ugh, there's got to be a better way...
                  unsub <- mySub
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
                  void $ Ref.write unsub usuRef
                  pure do
                    usu <- Ref.read usuRef
                    usu
              ]
          )
          k0
    pure do
      k0 (deleteFromCache { id: me })
      u0
      u1
