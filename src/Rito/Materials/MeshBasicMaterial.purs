module Rito.Materials.MeshBasicMaterial
  ( meshBasicMaterial
  , meshBasicMaterial_
  , MeshBasicMaterial(..)
  , MeshBasicMaterial'
  , class InitialMeshBasicMaterial
  , toInitializeMeshBasicMaterial
  , MeshBasicMaterialOptions
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
import Rito.Precision (Precision)
import Rito.Side (Side)
import Rito.THREE as THREE
import Rito.Texture (Texture)

data MeshBasicMaterialOptions = MeshBasicMaterialOptions

instance
  ConvertOption MeshBasicMaterialOptions
    "meshBasicMaterial"
    THREE.TMeshBasicMaterial
    THREE.TMeshBasicMaterial where
  convertOption _ _ = identity

instance
  ConvertOption MeshBasicMaterialOptions
    "color"
    Color
    (Maybe Color) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "map"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "lightMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "lightMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "aoMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "aoMapIntensity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "alphaMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "envMap"
    Texture
    (Maybe Texture) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "wireframe"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "wireframeLinewidth"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "alphaTest"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "alphaToCoverage"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "blendDst"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "blendDstAlpha"
    BlendDst
    (Maybe BlendDst) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "blendEquation"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "blendEquationAlpha"
    BlendEquation
    (Maybe BlendEquation) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "blending"
    Blending
    (Maybe Blending) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "blendSrc"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "blendSrcAlpha"
    BlendSrc
    (Maybe BlendSrc) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "clipIntersection"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "clipShadows"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "colorWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "depthFunc"
    DepthMode
    (Maybe DepthMode) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "depthTest"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "depthWrite"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "opacity"
    Number
    (Maybe Number) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "polygonOffset"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "polygonOffsetFactor"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "polygonOffsetUnits"
    Int
    (Maybe Int) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "precision"
    Precision
    (Maybe Precision) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "premultipliedAlpha"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "dithering"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "shadowSide"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "side"
    Side
    (Maybe Side) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "toneMapped"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "transparent"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "vertexColors"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

instance
  ConvertOption MeshBasicMaterialOptions
    "visible"
    Boolean
    (Maybe Boolean) where
  convertOption _ _ = Just

type MeshBasicMaterialOptional =
  ( color :: Maybe Color
  , map :: Maybe Texture
  , lightMap :: Maybe Texture
  , lightMapIntensity :: Maybe Number
  , aoMap :: Maybe Texture
  , aoMapIntensity :: Maybe Number
  , alphaMap :: Maybe Texture
  , envMap :: Maybe Texture
  , wireframe :: Maybe Boolean
  , wireframeLinewidth :: Maybe Number
  | AllMaterials Maybe BlendDst BlendEquation Blending
      BlendSrc
      DepthMode
      Precision
      Side
  )

type MeshBasicMaterialAll =
  (meshBasicMaterial :: THREE.TMeshBasicMaterial | MeshBasicMaterialOptional)

defaultMeshBasicMaterial :: { | MeshBasicMaterialOptional }
defaultMeshBasicMaterial =
  { color: Nothing
  , map: Nothing
  , lightMap: Nothing
  , lightMapIntensity: Nothing
  , aoMap: Nothing
  , aoMapIntensity: Nothing
  , alphaMap: Nothing
  , envMap: Nothing
  , wireframe: Nothing
  , wireframeLinewidth: Nothing
  } `union` defaultMaterials

class InitialMeshBasicMaterial i where
  toInitializeMeshBasicMaterial :: i -> C.InitializeMeshBasicMaterial

instance InitialMeshBasicMaterial C.InitializeMeshBasicMaterial where
  toInitializeMeshBasicMaterial = identity

instance
  ConvertOptionsWithDefaults MeshBasicMaterialOptions
    { | MeshBasicMaterialOptional }
    { | provided }
    { | MeshBasicMaterialAll } =>
  InitialMeshBasicMaterial { | provided } where
  toInitializeMeshBasicMaterial provided = C.InitializeMeshBasicMaterial
    ( convertOptionsWithDefaults MeshBasicMaterialOptions
        defaultMeshBasicMaterial
        provided
    )

type MeshBasicMaterial' = Variant
  ( color :: Color
  , map :: Texture
  , lightMap :: Texture
  , lightMapIntensity :: Number
  , aoMap :: Texture
  , aoMapIntensity :: Number
  , alphaMap :: Texture
  , envMap :: Texture
  , wireframe :: Boolean
  , wireframeLinewidth :: Number
  )

newtype MeshBasicMaterial = MeshBasicMaterial MeshBasicMaterial'
instance Newtype MeshBasicMaterial MeshBasicMaterial'

meshBasicMaterial
  :: forall i payload
   . InitialMeshBasicMaterial i
  => i
  -> Event MeshBasicMaterial
  -> C.Material payload
meshBasicMaterial i' atts = C.Material go
  where
  C.InitializeMeshBasicMaterial i = toInitializeMeshBasicMaterial i'
  go
    parent
    ( C.ThreeInterpret
        { ids
        , deleteFromCache
        , makeMeshBasicMaterial
        , setColor
        , setMap
        , setLightMap
        , setLightMapIntensity
        , setAoMap
        , setAoMapIntensity
        , setAlphaMap
        , setEnvMap
        , setWireframe
        , setWireframeLinewidth
        }
    ) = makeLemmingEventO $ mkSTFn2 \(Subscriber mySub) k -> do
    me <- ids
    parent.raiseId me
    unsub <- runSTFn2 mySub
      ( oneOf
          [ pure
              ( makeMeshBasicMaterial
                  ( { id: me
                    , parent: parent.parent
                    , scope: parent.scope
                    , parameters:
                        { meshBasicMaterial: i.meshBasicMaterial
                        , color: i.color
                        , map: i.map
                        , lightMap: i.lightMap
                        , lightMapIntensity: i.lightMapIntensity
                        , aoMap: i.aoMap
                        , aoMapIntensity: i.aoMapIntensity
                        , alphaMap: i.alphaMap
                        , envMap: i.envMap
                        , wireframe: i.wireframe
                        , wireframeLinewidth: i.wireframeLinewidth
                        }
                    , materialParameters: initializeDefaultMaterials i
                    }
                  )
              )
          , map
              ( \(MeshBasicMaterial e) -> match
                  { color: setColor <<< { id: me, color: _ }
                  , map: setMap <<< { id: me, map: _ }
                  , lightMap: setLightMap <<< { id: me, lightMap: _ }
                  , lightMapIntensity: setLightMapIntensity <<<
                      { id: me, lightMapIntensity: _ }
                  , aoMap: setAoMap <<< { id: me, aoMap: _ }
                  , aoMapIntensity: setAoMapIntensity <<<
                      { id: me, aoMapIntensity: _ }
                  , alphaMap: setAlphaMap <<< { id: me, alphaMap: _ }
                  , envMap: setEnvMap <<< { id: me, envMap: _ }
                  , wireframe: setWireframe <<< { id: me, wireframe: _ }
                  , wireframeLinewidth: setWireframeLinewidth <<<
                      { id: me, wireframeLinewidth: _ }
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
meshBasicMaterial_
  :: forall i payload
   . InitialMeshBasicMaterial i
  => i
  -> C.Material payload
meshBasicMaterial_ i = meshBasicMaterial i empty
