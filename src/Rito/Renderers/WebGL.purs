module Rito.Renderers.WebGL where

import Prelude

import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import Effect.AVar as AVar
import Effect.Exception (throwException)
import Effect.Ref as Ref
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.Renderers.WebGLRenderingPowerPreference as WPP
import Rito.Renderers.WebGLRenderingPrecision as WRP
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
  (canvas :: HTMLCanvasElement | WebGLRendererOptional)

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
  -> C.Renderer lock payload
webGLRenderer (C.Scene sne) (C.Camera cam) i' props = C.Renderer go
  where
  C.InitializeWebGLRenderer i = toInitializeWebGLRenderer i'
  go
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeWebGLRenderer
          , webGLRender
          }
      ) = makeEvent \k0 -> do
    me <- ids
    scope <- ids
    sceneAvar <- AVar.empty
    cameraAvar <- AVar.empty
    map (k0 (deleteFromCache { id: me }) *> _) $ flip subscribe k0 $
      oneOf
        [ bang $ makeWebGLRenderer
            { id: me
            , canvas: i.canvas
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
        , sne
            { parent: me
            , scope: scope
            , raiseId: \i -> void $ AVar.tryPut i sceneAvar
            }
            di
        , cam
            { parent: me
            , scope: scope
            , raiseId: \i -> void $ AVar.tryPut i cameraAvar
            }
            di
        , makeEvent \k -> do
            usuRef <- Ref.new mempty
            -- ugh, there's got to be a better way...
            void $ AVar.take sceneAvar \sceneLR -> do
              void $ AVar.take cameraAvar \cameraLR -> do
                case sceneLR of
                  Left err -> throwException err
                  Right sceneId -> case cameraLR of
                    Left err -> throwException err
                    Right cameraId -> do
                      unsub <- subscribe
                        ( props <#>
                            ( \(WebGLRenderer msh) ->
                                msh # match
                                  { render: \_ -> webGLRender
                                      { id: me, scene: sceneId, camera: cameraId }
                                  }
                            )
                        )
                        k
                      Ref.write unsub usuRef
            pure do
              usu <- Ref.read usuRef
              usu
        ]
