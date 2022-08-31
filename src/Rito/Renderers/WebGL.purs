module Rito.Renderers.WebGL where

import Prelude

import Bolson.EffectFn.Core (Scope(..))
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Effect.Ref as Ref
import FRP.Event.EffectFn (Event,  makeEvent, subscribe)
import Rito.Core as C
import Rito.Renderers.WebGLRenderingPowerPreference as WPP
import Rito.Renderers.WebGLRenderingPrecision as WRP
import Rito.THREE as THREE
import Web.HTML (HTMLCanvasElement)

data WebGLRendererOptions = WebGLRendererOptions

instance
  ConvertOption WebGLRendererOptions
    "canvas"
    HTMLCanvasElement
    HTMLCanvasElement where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "webGLRenderer"
    THREE.TWebGLRenderer
    THREE.TWebGLRenderer where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "precision"
    WRP.WebGLRenderingPrecision
    WRP.WebGLRenderingPrecision where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "alpha"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "premultipliedAlpha"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "antialias"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "stencil"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "preserveDrawingBuffer"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "powerPreference"
    WPP.WebGLRenderingPowerPreference
    WPP.WebGLRenderingPowerPreference where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "failIfMajorPerformanceCaveat"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "depth"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption WebGLRendererOptions
    "logarithmicDepthBuffer"
    Boolean
    Boolean where
  convertOption _ _ = identity

type WebGLRendererOptional =
  ( precision :: WRP.WebGLRenderingPrecision
  , alpha :: Boolean
  , premultipliedAlpha :: Boolean
  , antialias :: Boolean
  , stencil :: Boolean
  , preserveDrawingBuffer :: Boolean
  , powerPreference :: WPP.WebGLRenderingPowerPreference
  , failIfMajorPerformanceCaveat :: Boolean
  , depth :: Boolean
  , logarithmicDepthBuffer :: Boolean
  )

type WebGLRendererAll =
  ( canvas :: HTMLCanvasElement
  , webGLRenderer :: THREE.TWebGLRenderer
  | WebGLRendererOptional
  )

defaultWebGLRenderer :: { | WebGLRendererOptional }
defaultWebGLRenderer =
  { precision: WRP.High
  , alpha: false
  , premultipliedAlpha: true
  , antialias: false
  , stencil: true
  , preserveDrawingBuffer: false
  , powerPreference: WPP.Default
  , failIfMajorPerformanceCaveat: false
  , depth: true
  , logarithmicDepthBuffer: false
  }

class InitialWebGLRenderer i where
  toInitializeWebGLRenderer :: i -> C.InitializeWebGLRenderer

instance InitialWebGLRenderer C.InitializeWebGLRenderer where
  toInitializeWebGLRenderer = identity

instance
  ConvertOptionsWithDefaults WebGLRendererOptions { | WebGLRendererOptional }
    { | provided }
    { | WebGLRendererAll } =>
  InitialWebGLRenderer { | provided } where
  toInitializeWebGLRenderer provided = C.InitializeWebGLRenderer
    ( convertOptionsWithDefaults WebGLRendererOptions defaultWebGLRenderer
        provided
    )

type WebGLRenderer' = Variant
  ( render :: Unit
  , size :: { width :: Number, height :: Number }
  -- todo, add shadow map & other goodies
  )
newtype WebGLRenderer = WebGLRenderer WebGLRenderer'
instance Newtype WebGLRenderer WebGLRenderer'

webGLRenderer
  :: forall i lock payload
   . InitialWebGLRenderer i
  => C.Scene lock payload
  -> C.Camera lock payload
  -> i
  -> Event WebGLRenderer
  -> C.WebGLRenderer lock payload
webGLRenderer sne cam i' props = C.WebGLRenderer go
  where
  C.InitializeWebGLRenderer i = toInitializeWebGLRenderer i'
  go
    psr
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeWebGLRenderer
          , webGLRender
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
    u1 <- case sceneLR of
      Nothing -> pure (pure unit)
      Just sceneId -> case cameraLR of
        Nothing -> pure (pure unit)
        Just cameraId -> subscribe
          ( oneOf
              [ pure $ makeWebGLRenderer
                  { id: me
                  , webGLRenderer: i.webGLRenderer
                  , canvas: i.canvas
                  , camera: cameraId
                  , precision: i.precision
                  , alpha: i.alpha
                  , premultipliedAlpha: i.premultipliedAlpha
                  , antialias: i.antialias
                  , stencil: i.stencil
                  , preserveDrawingBuffer: i.preserveDrawingBuffer
                  , powerPreference: i.powerPreference
                  , failIfMajorPerformanceCaveat: i.failIfMajorPerformanceCaveat
                  , depth: i.depth
                  , logarithmicDepthBuffer: i.logarithmicDepthBuffer
                  }
              , makeEvent \k -> do
                  usuRef <- Ref.new mempty
                  -- ugh, there's got to be a better way...
                  unsub <- subscribe
                    ( props <#>
                        ( \(WebGLRenderer msh) ->
                            msh # match
                              { render: \_ -> webGLRender
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
