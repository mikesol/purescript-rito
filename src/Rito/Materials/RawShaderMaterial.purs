module Rito.Materials.RawShaderMaterial
  ( rawShaderMaterial
  , rawShaderMaterial_
  , RawShaderMaterial(..)
  , RawShaderMaterial'
  , RawShaderMaterialOptions(..)
  , class InitialRawShaderMaterial
  , toInitializeRawShaderMaterial
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

data RawShaderMaterialOptions = RawShaderMaterialOptions

instance
  ConvertOption RawShaderMaterialOptions
    "rawShaderMaterial"
    THREE.TRawShaderMaterial
    THREE.TRawShaderMaterial where
  convertOption _ _ = identity

instance
  ConvertOption RawShaderMaterialOptions
    "vertexShader"
    String
    String where
  convertOption _ _ = identity

instance
  ConvertOption RawShaderMaterialOptions
    "fragmentShader"
    String
    String where
  convertOption _ _ = identity

instance
  ConvertOption RawShaderMaterialOptions
    "alphaTest"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "alphaToCoverage"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "blendDst"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "blendDstAlpha"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "blendEquation"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "blendEquationAlpha"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "blending"
    Blending
    (Maybe Blending) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "blendSrc"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "blendSrcAlpha"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "clipIntersection"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "clipShadows"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "colorWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "depthFunc"
    DepthMode
    (Maybe DepthMode) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "depthTest"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "depthWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "opacity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "polygonOffset"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "polygonOffsetFactor"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "polygonOffsetUnits"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "precision"
    Precision
    (Maybe Precision) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "premultipliedAlpha"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "dithering"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "shadowSide"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "side"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "toneMapped"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "transparent"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "vertexColors"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption RawShaderMaterialOptions
    "visible"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

type RawShaderMaterialOptional =
  ( | AllMaterials Maybe BlendDst BlendEquation Blending
      BlendSrc
      DepthMode
      Precision
      Side
  )

type RawShaderMaterialAll =
  ( rawShaderMaterial :: THREE.TRawShaderMaterial
  , vertexShader :: String
  , fragmentShader :: String
  | RawShaderMaterialOptional
  )

defaultRawShaderMaterial :: { | RawShaderMaterialOptional }
defaultRawShaderMaterial = defaultMaterials

class InitialRawShaderMaterial i u where
  toInitializeRawShaderMaterial
    :: u -> i -> C.InitializeRawShaderMaterial u

instance InitialRawShaderMaterial (C.InitializeRawShaderMaterial u) u where
  toInitializeRawShaderMaterial _ = identity

instance
  ConvertOptionsWithDefaults RawShaderMaterialOptions
    { | RawShaderMaterialOptional }
    { | provided }
    { | RawShaderMaterialAll } =>
  InitialRawShaderMaterial { | provided } u where
  toInitializeRawShaderMaterial u provided = C.InitializeRawShaderMaterial
    ( ( convertOptionsWithDefaults RawShaderMaterialOptions
          defaultRawShaderMaterial
          provided
      ) `union` { uniforms: u }
    )

type RawShaderMaterial' u = Variant
  ( uniform :: Variant u
  )

newtype RawShaderMaterial u = RawShaderMaterial (RawShaderMaterial' u)
instance Newtype (RawShaderMaterial u) (RawShaderMaterial' u)

rawShaderMaterial
  :: forall i u url payload
   . InitialRawShaderMaterial i { | u }
  => RowToList u url
  => IsUniform url
  => { | u }
  -> i
  -> Event (RawShaderMaterial u)
  -> C.Material payload
rawShaderMaterial unifs i' atts = C.Material go
  where
  C.InitializeRawShaderMaterial i = toInitializeRawShaderMaterial unifs i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeRawShaderMaterial
        , setUniform
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makeRawShaderMaterial
                  ( { id: me
                    , parent: parent.parent
                    , scope: parent.scope
                    , parameters:
                        { rawShaderMaterial: i.rawShaderMaterial
                        , fragmentShader: i.fragmentShader
                        , vertexShader: i.vertexShader
                        , uniforms: toUniformDecl i.uniforms
                        }
                    , materialParameters: initializeDefaultMaterials i
                    }
                  )
              )
          , map
              ( \(RawShaderMaterial e) -> match
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

rawShaderMaterial_
  :: forall i u url payload
   . InitialRawShaderMaterial i { | u }
  => RowToList u url
  => IsUniform url
  => { | u }
  -> i
  -> C.Material payload
rawShaderMaterial_ u i = rawShaderMaterial u i empty
