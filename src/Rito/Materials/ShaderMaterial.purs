module Rito.Materials.ShaderMaterial
  ( shaderMaterial
  , shaderMaterial_
  , ShaderMaterial(..)
  , ShaderMaterial'
  , ShaderMaterialOptions(..)
  , class InitialShaderMaterial
  , toInitializeShaderMaterial
  ) where

import Prelude

import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (reflectSymbol)
import Data.Variant (Unvariant(..), Variant, match, unvariant)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Foreign (Foreign)
import Prim.RowList (class RowToList)
import Record (union)
import Rito.BlendDst (BlendDst)
import Rito.BlendEquation (BlendEquation)
import Rito.BlendSrc (BlendSrc)
import Rito.Blending (Blending)
import Rito.Core (AllMaterials, defaultMaterials, initializeDefaultMaterials)
import Rito.Core as C
import Rito.DepthMode (DepthMode)
import Rito.Precision (Precision)
import Rito.Side (Side)
import Rito.THREE as THREE
import Rito.Uniforms (class IsUniform, toUniformDecl)
import Unsafe.Coerce (unsafeCoerce)

data ShaderMaterialOptions = ShaderMaterialOptions

instance
  ConvertOption ShaderMaterialOptions
    "shaderMaterial"
    THREE.TShaderMaterial
    THREE.TShaderMaterial where
  convertOption _ _ = identity

instance
  ConvertOption ShaderMaterialOptions
    "vertexShader"
    String
    String where
  convertOption _ _ = identity

instance
  ConvertOption ShaderMaterialOptions
    "fragmentShader"
    String
    String where
  convertOption _ _ = identity

instance
  ConvertOption ShaderMaterialOptions
    "alphaTest"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "alphaToCoverage"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "blendDst"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "blendDstAlpha"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "blendEquation"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "blendEquationAlpha"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "blending"
    Blending
    (Maybe Blending) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "blendSrc"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "blendSrcAlpha"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "clipIntersection"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "clipShadows"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "colorWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "depthFunc"
    DepthMode
    (Maybe DepthMode) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "depthTest"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "depthWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "opacity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "polygonOffset"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "polygonOffsetFactor"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "polygonOffsetUnits"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "precision"
    Precision
    (Maybe Precision) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "premultipliedAlpha"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "dithering"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "shadowSide"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "side"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "toneMapped"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "transparent"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "vertexColors"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption ShaderMaterialOptions
    "visible"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

type ShaderMaterialOptional =
  ( | AllMaterials Maybe BlendDst BlendEquation Blending
      BlendSrc
      DepthMode
      Precision
      Side
  )

type ShaderMaterialAll =
  ( shaderMaterial :: THREE.TShaderMaterial
  , vertexShader :: String
  , fragmentShader :: String
  | ShaderMaterialOptional
  )

defaultShaderMaterial :: { | ShaderMaterialOptional }
defaultShaderMaterial = defaultMaterials

class InitialShaderMaterial i u where
  toInitializeShaderMaterial
    :: u -> i -> C.InitializeShaderMaterial u

instance InitialShaderMaterial (C.InitializeShaderMaterial u) u where
  toInitializeShaderMaterial _ = identity

instance
  ConvertOptionsWithDefaults ShaderMaterialOptions
    { | ShaderMaterialOptional }
    { | provided }
    { | ShaderMaterialAll } =>
  InitialShaderMaterial { | provided } u where
  toInitializeShaderMaterial u provided = C.InitializeShaderMaterial
    ( ( convertOptionsWithDefaults ShaderMaterialOptions
          defaultShaderMaterial
          provided
      ) `union` { uniforms: u }
    )

type ShaderMaterial' u = Variant
  ( uniform :: Variant u
  )

newtype ShaderMaterial u = ShaderMaterial (ShaderMaterial' u)
instance Newtype (ShaderMaterial u) (ShaderMaterial' u)

shaderMaterial
  :: forall i u url payload
   . InitialShaderMaterial i { | u }
  => RowToList u url
  => IsUniform url
  => { | u }
  -> i
  -> Event (ShaderMaterial u)
  -> C.Material payload
shaderMaterial unifs i' atts = C.Material go
  where
  C.InitializeShaderMaterial i = toInitializeShaderMaterial unifs i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeShaderMaterial
        , setUniform
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makeShaderMaterial
                  ( { id: me
                    , parent: parent.parent
                    , scope: parent.scope
                    , parameters:
                        { shaderMaterial: i.shaderMaterial
                        , fragmentShader: i.fragmentShader
                        , vertexShader: i.vertexShader
                        , uniforms: toUniformDecl i.uniforms
                        }
                    , materialParameters: initializeDefaultMaterials i
                    }
                  )
              )
          , map
              ( \(ShaderMaterial e) -> match
                  { uniform: \v -> do
                      let Unvariant uv = unvariant v
                      let
                        { key, value } = uv \k v ->
                          { key: reflectSymbol k
                          , value: ((unsafeCoerce :: forall a. a -> Foreign) v)
                          }
                      setUniform { id: me, key, value }
                  }
                  e
              )
              atts
          ]
      )
      k
    pure do
      runSTFn1 k (deleteFromCache { id: me })
      unsub

shaderMaterial_
  :: forall i u url payload
   . InitialShaderMaterial i { | u }
  => RowToList u url
  => IsUniform url
  => { | u }
  -> i
  -> C.Material payload
shaderMaterial_ u i = shaderMaterial u i empty
