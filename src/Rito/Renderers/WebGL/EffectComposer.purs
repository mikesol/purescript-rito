module Rito.Renderers.WebGL.EffectComposer where

import Prelude

import Bolson.Control (flatten)
import Bolson.Core (Entity(..), fixed)
import Bolson.Core as Bolson
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Core as C
import Rito.Renderers.WebGLRenderingPowerPreference as WPP
import Rito.Renderers.WebGLRenderingPrecision as WRP
import Rito.THREE as THREE
import Web.HTML (HTMLCanvasElement)

data EffectComposerOptions = EffectComposerOptions

instance
  ConvertOption EffectComposerOptions
    "canvas"
    HTMLCanvasElement
    HTMLCanvasElement where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "raycaster"
    THREE.TRaycaster
    THREE.TRaycaster where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "webGLRenderer"
    THREE.TEffectComposer
    THREE.TEffectComposer where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "precision"
    WRP.WebGLRenderingPrecision
    WRP.WebGLRenderingPrecision where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "alpha"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "premultipliedAlpha"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "antialias"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "stencil"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "preserveDrawingBuffer"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "powerPreference"
    WPP.WebGLRenderingPowerPreference
    WPP.WebGLRenderingPowerPreference where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "failIfMajorPerformanceCaveat"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "depth"
    Boolean
    Boolean where
  convertOption _ _ = identity

instance
  ConvertOption EffectComposerOptions
    "logarithmicDepthBuffer"
    Boolean
    Boolean where
  convertOption _ _ = identity

type EffectComposerOptional =
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

type EffectComposerAll =
  ( canvas :: HTMLCanvasElement
  , effectComposer :: THREE.TEffectComposer
  , webGLRenderer :: THREE.TWebGLRenderer
  , raycaster :: THREE.TRaycaster
  | EffectComposerOptional
  )

defaultEffectComposer :: { | EffectComposerOptional }
defaultEffectComposer =
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

class InitialEffectComposer i where
  toInitializeEffectComposer :: i -> C.InitializeEffectComposer

instance InitialEffectComposer C.InitializeEffectComposer where
  toInitializeEffectComposer = identity

instance
  ConvertOptionsWithDefaults EffectComposerOptions { | EffectComposerOptional }
    { | provided }
    { | EffectComposerAll } =>
  InitialEffectComposer { | provided } where
  toInitializeEffectComposer provided = C.InitializeEffectComposer
    ( convertOptionsWithDefaults EffectComposerOptions defaultEffectComposer
        provided
    )

type EffectComposer' = Variant
  ( render :: Unit
  , size :: { width :: Number, height :: Number }
  -- todo, add shadow map & other goodies
  )
newtype EffectComposer = EffectComposer EffectComposer'
instance Newtype EffectComposer EffectComposer'

effectComposer
  :: forall i lock payload
   . InitialEffectComposer i
  => i
  -> Event EffectComposer
  -> Array (C.APass lock payload)
  -> C.ARenderer lock payload
effectComposer i' props kidz = Element' $ C.Renderer go
  where
  C.InitializeEffectComposer i = toInitializeEffectComposer i'
  go
    parent
    di@
      ( C.ThreeInterpret
          { ids
          , deleteFromCache
          , makeEffectComposer
          , effectComposerRender
          , setSizeThroughEffectComposer
          }
      ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      oneOf
        [ bang $ makeEffectComposer
            { id: me
            , effectComposer: i.effectComposer
            , canvas: i.canvas
            , webGLRenderer: i.webGLRenderer
            , raycaster: i.raycaster
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
        , props <#>
            ( \(EffectComposer msh) ->
                msh # match
                  { render: \_ -> effectComposerRender
                      { id: me }
                  , size: \{ width, height } -> setSizeThroughEffectComposer
                      { id: me, width, height }
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
