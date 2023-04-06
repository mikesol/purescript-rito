module Rito.Materials.MeshStandardMaterial
  ( meshStandardMaterial
  , meshStandardMaterial_
  , MeshStandardMaterial(..)
  , MeshStandardMaterial'
  , class InitialMeshStandardMaterial
  , toInitializeMeshStandardMaterial
  , MeshStandardMaterialOptions
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

data MeshStandardMaterialOptions = MeshStandardMaterialOptions

instance
  ConvertOption MeshStandardMaterialOptions
    "meshStandardMaterial"
    THREE.TMeshStandardMaterial
    THREE.TMeshStandardMaterial where
  convertOption _ _ = identity

instance
  ConvertOption MeshStandardMaterialOptions
    "color"
    Color
    (Maybe Color) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "roughness"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "metalness"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "map"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "lightMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "lightMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "aoMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "aoMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "emissive"
    Color
    (Maybe Color) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "emissiveIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "emissiveMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "bumpMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "bumpScale"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "normalMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "normalMapType"
    NormalMapType
    (Maybe NormalMapType) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "normalScale"
    Vector2
    (Maybe Vector2) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "displacementMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "displacementScale"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "displacementBias"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "roughnessMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "metalnessMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "alphaMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "envMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "envMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "wireframe"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "wireframeLinewidth"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "flatShading"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "alphaTest"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "alphaToCoverage"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "blendDst"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "blendDstAlpha"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "blendEquation"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "blendEquationAlpha"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "blending"
    Blending
    (Maybe Blending) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "blendSrc"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "blendSrcAlpha"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "clipIntersection"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "clipShadows"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "colorWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "depthFunc"
    DepthMode
    (Maybe DepthMode) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "depthTest"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "depthWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "opacity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "polygonOffset"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "polygonOffsetFactor"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "polygonOffsetUnits"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "precision"
    Precision
    (Maybe Precision) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "premultipliedAlpha"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "dithering"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "shadowSide"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "side"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "toneMapped"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "transparent"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "vertexColors"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshStandardMaterialOptions
    "visible"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

type MeshStandardMaterialOptional =
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

type MeshStandardMaterialAll =
  ( meshStandardMaterial :: THREE.TMeshStandardMaterial
  | MeshStandardMaterialOptional
  )

defaultMeshStandardMaterial :: { | MeshStandardMaterialOptional }
defaultMeshStandardMaterial =
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

class InitialMeshStandardMaterial i where
  toInitializeMeshStandardMaterial :: i -> C.InitializeMeshStandardMaterial

instance InitialMeshStandardMaterial C.InitializeMeshStandardMaterial where
  toInitializeMeshStandardMaterial = identity

instance
  ConvertOptionsWithDefaults MeshStandardMaterialOptions
    { | MeshStandardMaterialOptional }
    { | provided }
    { | MeshStandardMaterialAll } =>
  InitialMeshStandardMaterial { | provided } where
  toInitializeMeshStandardMaterial provided = C.InitializeMeshStandardMaterial
    ( convertOptionsWithDefaults MeshStandardMaterialOptions
        defaultMeshStandardMaterial
        provided
    )

type MeshStandardMaterial' = Variant
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

newtype MeshStandardMaterial = MeshStandardMaterial MeshStandardMaterial'
instance Newtype MeshStandardMaterial MeshStandardMaterial'

meshStandardMaterial
  :: forall i payload
   . InitialMeshStandardMaterial i
  => i
  -> Event MeshStandardMaterial
  -> C.Material payload
meshStandardMaterial i' atts = C.Material go
  where
  C.InitializeMeshStandardMaterial i = toInitializeMeshStandardMaterial i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeMeshStandardMaterial
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
              ( makeMeshStandardMaterial
                  ( { id: me
                    , parent: parent.parent
                    , scope: parent.scope
                    , parameters:
                        { meshStandardMaterial: i.meshStandardMaterial
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
              ( \(MeshStandardMaterial e) -> match
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

meshStandardMaterial_
  :: forall i payload
   . InitialMeshStandardMaterial i
  => i
  -> C.Material payload
meshStandardMaterial_ i = meshStandardMaterial i empty
