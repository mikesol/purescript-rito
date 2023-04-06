module Rito.Materials.MeshPhongMaterial
  ( meshPhongMaterial
  , meshPhongMaterial_
  , MeshPhongMaterial(..)
  , MeshPhongMaterial'
  , class InitialMeshPhongMaterial
  , toInitializeMeshPhongMaterial
  , MeshPhongMaterialOptions
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
import Rito.CombineOperation (CombineOperation)
import Rito.Core (AllMaterials, defaultMaterials, initializeDefaultMaterials)
import Rito.Core as C
import Rito.DepthMode (DepthMode)
import Rito.NormalMapType (NormalMapType)
import Rito.Precision (Precision)
import Rito.Side (Side)
import Rito.THREE as THREE
import Rito.Texture (Texture)
import Rito.Vector2 (Vector2)
import Rito.WireframeLinecap (WireframeLinecap)
import Rito.WireframeLinejoin (WireframeLinejoin)

data MeshPhongMaterialOptions = MeshPhongMaterialOptions

instance
  ConvertOption MeshPhongMaterialOptions
    "meshPhongMaterial"
    THREE.TMeshPhongMaterial
    THREE.TMeshPhongMaterial where
  convertOption _ _ = identity

instance
  ConvertOption MeshPhongMaterialOptions
    "color"
    Color
    (Maybe Color) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "map"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "lightMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "lightMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "aoMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "aoMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "emissive"
    Color
    (Maybe Color) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "emissiveIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "emissiveMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "bumpMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "bumpScale"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "normalMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "normalMapType"
    NormalMapType
    (Maybe NormalMapType) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "normalScale"
    Vector2
    (Maybe Vector2) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "displacementMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "displacementScale"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "displacementBias"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "alphaMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "envMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "wireframe"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "wireframeLinewidth"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "flatShading"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "alphaTest"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "alphaToCoverage"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "blendDst"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "blendDstAlpha"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "blendEquation"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "blendEquationAlpha"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "blending"
    Blending
    (Maybe Blending) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "blendSrc"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "blendSrcAlpha"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "clipIntersection"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "clipShadows"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "colorWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "depthFunc"
    DepthMode
    (Maybe DepthMode) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "depthTest"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "depthWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "opacity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "polygonOffset"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "polygonOffsetFactor"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "polygonOffsetUnits"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "precision"
    Precision
    (Maybe Precision) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "premultipliedAlpha"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "dithering"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "shadowSide"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "side"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "toneMapped"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "transparent"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "vertexColors"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshPhongMaterialOptions
    "visible"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

type MeshPhongMaterialOptional =
  ( color :: Maybe Color
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
  , alphaMap :: Maybe Texture
  , envMap :: Maybe Texture
  , wireframe :: Maybe Boolean
  , wireframeLinewidth :: Maybe Number
  , flatShading :: Maybe Boolean
  --
  , combine :: Maybe CombineOperation
  , fog :: Maybe Boolean
  , reflectivity :: Maybe Number
  , refractionRatio :: Maybe Number
  , shininess :: Maybe Number
  , specular :: Maybe Color
  , specularMap :: Maybe Texture
  , wireframeLinecap :: Maybe WireframeLinecap
  , wireframeLinejoin :: Maybe WireframeLinejoin
  | AllMaterials Maybe BlendDst BlendEquation Blending
      BlendSrc
      DepthMode
      Precision
      Side
  )

type MeshPhongMaterialAll =
  (meshPhongMaterial :: THREE.TMeshPhongMaterial | MeshPhongMaterialOptional)

defaultMeshPhongMaterial :: { | MeshPhongMaterialOptional }
defaultMeshPhongMaterial =
  { color: Nothing
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
  , alphaMap: Nothing
  , envMap: Nothing
  , wireframe: Nothing
  , wireframeLinewidth: Nothing
  , flatShading: Nothing
  --
  , combine: Nothing
  , fog: Nothing
  , reflectivity: Nothing
  , refractionRatio: Nothing
  , shininess: Nothing
  , specular: Nothing
  , specularMap: Nothing
  , wireframeLinecap: Nothing
  , wireframeLinejoin: Nothing
  } `union` defaultMaterials

class InitialMeshPhongMaterial i where
  toInitializeMeshPhongMaterial :: i -> C.InitializeMeshPhongMaterial

instance InitialMeshPhongMaterial C.InitializeMeshPhongMaterial where
  toInitializeMeshPhongMaterial = identity

instance
  ConvertOptionsWithDefaults MeshPhongMaterialOptions
    { | MeshPhongMaterialOptional }
    { | provided }
    { | MeshPhongMaterialAll } =>
  InitialMeshPhongMaterial { | provided } where
  toInitializeMeshPhongMaterial provided = C.InitializeMeshPhongMaterial
    ( convertOptionsWithDefaults MeshPhongMaterialOptions
        defaultMeshPhongMaterial
        provided
    )

type MeshPhongMaterial' = Variant
  ( color :: Color
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
  , alphaMap :: Texture
  , envMap :: Texture
  , wireframe :: Boolean
  , wireframeLinewidth :: Number
  , flatShading :: Boolean
  --
  , combine :: CombineOperation
  , fog :: Boolean
  , reflectivity :: Number
  , refractionRatio :: Number
  , shininess :: Number
  , specular :: Color
  , specularMap :: Texture
  , wireframeLinecap :: WireframeLinecap
  , wireframeLinejoin :: WireframeLinejoin
  )

newtype MeshPhongMaterial = MeshPhongMaterial MeshPhongMaterial'
instance Newtype MeshPhongMaterial MeshPhongMaterial'

meshPhongMaterial
  :: forall i payload
   . InitialMeshPhongMaterial i
  => i
  -> Event MeshPhongMaterial
  -> C.Material payload
meshPhongMaterial i' atts = C.Material go
  where
  C.InitializeMeshPhongMaterial i = toInitializeMeshPhongMaterial i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeMeshPhongMaterial
        , setColor
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
        , setAlphaMap
        , setEnvMap
        , setWireframe
        , setWireframeLinewidth
        , setFlatShading
        --
        , setCombine
        , setFog
        , setReflectivity
        , setRefractionRatio
        , setShininess
        , setSpecular
        , setSpecularMap
        , setWireframeLinecap
        , setWireframeLinejoin
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makeMeshPhongMaterial
                  ( { id: me
                    , parent: parent.parent
                    , scope: parent.scope
                    , parameters:
                        { meshPhongMaterial: i.meshPhongMaterial
                        , color: i.color
                        , map: i.map
                        , combine: i.combine
                        , fog: i.fog
                        , reflectivity: i.reflectivity
                        , refractionRatio: i.refractionRatio
                        , shininess: i.shininess
                        , specular: i.specular
                        , specularMap: i.specularMap
                        , wireframeLinecap: i.wireframeLinecap
                        , wireframeLinejoin: i.wireframeLinejoin
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
                        , alphaMap: i.alphaMap
                        , envMap: i.envMap
                        , wireframe: i.wireframe
                        , wireframeLinewidth: i.wireframeLinewidth
                        , flatShading: i.flatShading
                        }
                    , materialParameters: initializeDefaultMaterials i
                    }
                  )
              )
          , map
              ( \(MeshPhongMaterial e) -> match
                  { color: setColor <<< { id: me, color: _ }
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
                  , alphaMap: setAlphaMap <<< { id: me, alphaMap: _ }
                  , envMap: setEnvMap <<< { id: me, envMap: _ }
                  , wireframe: setWireframe <<< { id: me, wireframe: _ }
                  , wireframeLinewidth: setWireframeLinewidth <<<
                      { id: me, wireframeLinewidth: _ }
                  , flatShading: setFlatShading <<< { id: me, flatShading: _ }
                  --
                  , combine: setCombine <<< { id: me, combine: _ }
                  , fog: setFog <<< { id: me, fog: _ }
                  , reflectivity: setReflectivity <<<
                      { id: me, reflectivity: _ }
                  , refractionRatio: setRefractionRatio <<<
                      { id: me, refractionRatio: _ }
                  , shininess: setShininess <<< { id: me, shininess: _ }
                  , specular: setSpecular <<< { id: me, specular: _ }
                  , specularMap: setSpecularMap <<<
                      { id: me, specularMap: _ }
                  , wireframeLinecap: setWireframeLinecap <<<
                      { id: me, wireframeLinecap: _ }
                  , wireframeLinejoin: setWireframeLinejoin <<<
                      { id: me, wireframeLinejoin: _ }
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

meshPhongMaterial_
  :: forall i payload
   . InitialMeshPhongMaterial i
  => i
  -> C.Material payload
meshPhongMaterial_ i = meshPhongMaterial i empty