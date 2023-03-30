module Rito.Materials.MeshLambertMaterial
  ( meshLambertMaterial
  , meshLambertMaterial_
  , MeshLambertMaterial(..)
  , MeshLambertMaterial'
  , class InitialMeshLambertMaterial
  , toInitializeMeshLambertMaterial
  , MeshLambertMaterialOptions
  ) where

import Prelude

import Control.Monad.ST.Uncurried (mkSTFn2, runSTFn1, runSTFn2)
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Foldable (oneOf)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, Subscriber(..), makeLemmingEventO)
import Record (union)
import Rito.BlendDst (BlendDst)
import Rito.BlendEquation (BlendEquation)
import Rito.BlendSrc (BlendSrc)
import Rito.Blending (Blending)
import Rito.Color (Color)
import Rito.Core (AllMaterials, defaultMaterials, initializeDefaultMaterials)
import Rito.Core as C
import Rito.DepthMode (DepthMode)
import Rito.NormalMapType (NormalMapType)
import Rito.Precision (Precision)
import Rito.Side (Side)
import Rito.THREE as THREE
import Rito.Texture (Texture)
import Rito.Vector2 (Vector2)

data MeshLambertMaterialOptions = MeshLambertMaterialOptions

instance
  ConvertOption MeshLambertMaterialOptions
    "meshLambertMaterial"
    THREE.TMeshLambertMaterial
    THREE.TMeshLambertMaterial where
  convertOption _ _ = identity

instance
  ConvertOption MeshLambertMaterialOptions
    "color"
    Color
    (Maybe Color) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "roughness"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "metalness"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "map"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "lightMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "lightMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "aoMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "aoMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "emissive"
    Color
    (Maybe Color) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "emissiveIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "emissiveMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "bumpMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "bumpScale"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "normalMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "normalMapType"
    NormalMapType
    (Maybe NormalMapType) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "normalScale"
    Vector2
    (Maybe Vector2) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "displacementMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "displacementScale"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "displacementBias"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "roughnessMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "metalnessMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "alphaMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "envMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "envMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "wireframe"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "wireframeLinewidth"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "flatShading"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "alphaTest"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "alphaToCoverage"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "blendDst"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "blendDstAlpha"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "blendEquation"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "blendEquationAlpha"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "blending"
    Blending
    (Maybe Blending) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "blendSrc"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "blendSrcAlpha"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "clipIntersection"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "clipShadows"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "colorWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "depthFunc"
    DepthMode
    (Maybe DepthMode) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "depthTest"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "depthWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "opacity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "polygonOffset"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "polygonOffsetFactor"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "polygonOffsetUnits"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "precision"
    Precision
    (Maybe Precision) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "premultipliedAlpha"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "dithering"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "shadowSide"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "side"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "toneMapped"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "transparent"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "vertexColors"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshLambertMaterialOptions
    "visible"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

type MeshLambertMaterialOptional =
  ( color :: Maybe Color
  , roughness :: Maybe Number
  , metalness :: Maybe Number
  , map :: Maybe Texture
  , lightMap :: Maybe Texture
  , lightMapIntensity :: Maybe Number
  , aoMap :: Maybe Texture
  , aoMapIntensity :: Maybe Number
  , emissive :: Maybe Color
  , emissiveIntensity :: Maybe Number
  , emissiveMap :: Maybe Texture
  , bumpMap :: Maybe Texture
  , bumpScale :: Maybe Number
  , normalMap :: Maybe Texture
  , normalMapType :: Maybe NormalMapType
  , normalScale :: Maybe Vector2
  , displacementMap :: Maybe Texture
  , displacementScale :: Maybe Number
  , displacementBias :: Maybe Number
  , roughnessMap :: Maybe Texture
  , metalnessMap :: Maybe Texture
  , alphaMap :: Maybe Texture
  , envMap :: Maybe Texture
  , envMapIntensity :: Maybe Number
  , wireframe :: Maybe Boolean
  , wireframeLinewidth :: Maybe Number
  , flatShading :: Maybe Boolean
  | AllMaterials Maybe BlendDst BlendEquation Blending
      BlendSrc
      DepthMode
      Precision
      Side
  )

type MeshLambertMaterialAll =
  ( meshLambertMaterial :: THREE.TMeshLambertMaterial
  | MeshLambertMaterialOptional
  )

defaultMeshLambertMaterial :: { | MeshLambertMaterialOptional }
defaultMeshLambertMaterial =
  { color: Nothing
  , roughness: Nothing
  , metalness: Nothing
  , map: Nothing
  , lightMap: Nothing
  , lightMapIntensity: Nothing
  , aoMap: Nothing
  , aoMapIntensity: Nothing
  , emissive: Nothing
  , emissiveIntensity: Nothing
  , emissiveMap: Nothing
  , bumpMap: Nothing
  , bumpScale: Nothing
  , normalMap: Nothing
  , normalMapType: Nothing
  , normalScale: Nothing
  , displacementMap: Nothing
  , displacementScale: Nothing
  , displacementBias: Nothing
  , roughnessMap: Nothing
  , metalnessMap: Nothing
  , alphaMap: Nothing
  , envMap: Nothing
  , envMapIntensity: Nothing
  , wireframe: Nothing
  , wireframeLinewidth: Nothing
  , flatShading: Nothing
  } `union` defaultMaterials

class InitialMeshLambertMaterial i where
  toInitializeMeshLambertMaterial :: i -> C.InitializeMeshLambertMaterial

instance InitialMeshLambertMaterial C.InitializeMeshLambertMaterial where
  toInitializeMeshLambertMaterial = identity

instance
  ConvertOptionsWithDefaults MeshLambertMaterialOptions
    { | MeshLambertMaterialOptional }
    { | provided }
    { | MeshLambertMaterialAll } =>
  InitialMeshLambertMaterial { | provided } where
  toInitializeMeshLambertMaterial provided = C.InitializeMeshLambertMaterial
    ( convertOptionsWithDefaults MeshLambertMaterialOptions
        defaultMeshLambertMaterial
        provided
    )

type MeshLambertMaterial' = Variant
  ( color :: Color
  , roughness :: Number
  , metalness :: Number
  , map :: Texture
  , lightMap :: Texture
  , lightMapIntensity :: Number
  , aoMap :: Texture
  , aoMapIntensity :: Number
  , emissive :: Color
  , emissiveIntensity :: Number
  , emissiveMap :: Texture
  , bumpMap :: Texture
  , bumpScale :: Number
  , normalMap :: Texture
  , normalMapType :: NormalMapType
  , normalScale :: Vector2
  , displacementMap :: Texture
  , displacementScale :: Number
  , displacementBias :: Number
  , roughnessMap :: Texture
  , metalnessMap :: Texture
  , alphaMap :: Texture
  , envMap :: Texture
  , envMapIntensity :: Number
  , wireframe :: Boolean
  , wireframeLinewidth :: Number
  , flatShading :: Boolean
  )

newtype MeshLambertMaterial = MeshLambertMaterial MeshLambertMaterial'
instance Newtype MeshLambertMaterial MeshLambertMaterial'

meshLambertMaterial
  :: forall i payload
   . InitialMeshLambertMaterial i
  => i
  -> Event MeshLambertMaterial
  -> C.Material payload
meshLambertMaterial i' atts = C.Material go
  where
  C.InitializeMeshLambertMaterial i = toInitializeMeshLambertMaterial i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeMeshLambertMaterial
        , setColor
        , setRoughness
        , setMetalness
        , setMap
        , setLightMap
        , setLightMapIntensity
        , setAoMap
        , setAoMapIntensity
        , setEmissive
        , setEmissiveIntensity
        , setEmissiveMap
        , setBumpMap
        , setBumpScale
        , setNormalMap
        , setNormalMapType
        , setNormalScale
        , setDisplacementMap
        , setDisplacementScale
        , setDisplacementBias
        , setRoughnessMap
        , setMetalnessMap
        , setAlphaMap
        , setEnvMap
        , setEnvMapIntensity
        , setWireframe
        , setWireframeLinewidth
        , setFlatShading
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makeMeshLambertMaterial
                  ( { id: me
                    , parent: parent.parent
                    , scope: parent.scope
                    , parameters:
                        { meshLambertMaterial: i.meshLambertMaterial
                        , color: i.color
                        , roughness: i.roughness
                        , metalness: i.metalness
                        , map: i.map
                        , lightMap: i.lightMap
                        , lightMapIntensity: i.lightMapIntensity
                        , aoMap: i.aoMap
                        , aoMapIntensity: i.aoMapIntensity
                        , emissive: i.emissive
                        , emissiveIntensity: i.emissiveIntensity
                        , emissiveMap: i.emissiveMap
                        , bumpMap: i.bumpMap
                        , bumpScale: i.bumpScale
                        , normalMap: i.normalMap
                        , normalMapType: i.normalMapType
                        , normalScale: i.normalScale
                        , displacementMap: i.displacementMap
                        , displacementScale: i.displacementScale
                        , displacementBias: i.displacementBias
                        , roughnessMap: i.roughnessMap
                        , metalnessMap: i.metalnessMap
                        , alphaMap: i.alphaMap
                        , envMap: i.envMap
                        , envMapIntensity: i.envMapIntensity
                        , wireframe: i.wireframe
                        , wireframeLinewidth: i.wireframeLinewidth
                        , flatShading: i.flatShading
                        }
                    , materialParameters: initializeDefaultMaterials i
                    }
                  )
              )
          , map
              ( \(MeshLambertMaterial e) -> match
                  { color: setColor <<< { id: me, color: _ }
                  , roughness: setRoughness <<< { id: me, roughness: _ }
                  , metalness: setMetalness <<< { id: me, metalness: _ }
                  , map: setMap <<< { id: me, map: _ }
                  , lightMap: setLightMap <<< { id: me, lightMap: _ }
                  , lightMapIntensity: setLightMapIntensity <<<
                      { id: me, lightMapIntensity: _ }
                  , aoMap: setAoMap <<< { id: me, aoMap: _ }
                  , aoMapIntensity: setAoMapIntensity <<<
                      { id: me, aoMapIntensity: _ }
                  , emissive: setEmissive <<< { id: me, emissive: _ }
                  , emissiveIntensity: setEmissiveIntensity <<<
                      { id: me, emissiveIntensity: _ }
                  , emissiveMap: setEmissiveMap <<< { id: me, emissiveMap: _ }
                  , bumpMap: setBumpMap <<< { id: me, bumpMap: _ }
                  , bumpScale: setBumpScale <<< { id: me, bumpScale: _ }
                  , normalMap: setNormalMap <<< { id: me, normalMap: _ }
                  , normalMapType: setNormalMapType <<<
                      { id: me, normalMapType: _ }
                  , normalScale: setNormalScale <<< { id: me, normalScale: _ }
                  , displacementMap: setDisplacementMap <<<
                      { id: me, displacementMap: _ }
                  , displacementScale: setDisplacementScale <<<
                      { id: me, displacementScale: _ }
                  , displacementBias: setDisplacementBias <<<
                      { id: me, displacementBias: _ }
                  , roughnessMap: setRoughnessMap <<<
                      { id: me, roughnessMap: _ }
                  , metalnessMap: setMetalnessMap <<<
                      { id: me, metalnessMap: _ }
                  , alphaMap: setAlphaMap <<< { id: me, alphaMap: _ }
                  , envMap: setEnvMap <<< { id: me, envMap: _ }
                  , envMapIntensity: setEnvMapIntensity <<<
                      { id: me, envMapIntensity: _ }
                  , wireframe: setWireframe <<< { id: me, wireframe: _ }
                  , wireframeLinewidth: setWireframeLinewidth <<<
                      { id: me, wireframeLinewidth: _ }
                  , flatShading: setFlatShading <<< { id: me, flatShading: _ }
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

meshLambertMaterial_
  :: forall i payload
   . InitialMeshLambertMaterial i
  => i
  -> C.Material payload
meshLambertMaterial_ i = meshLambertMaterial i empty
