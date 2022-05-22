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

import Control.Alt ((<|>))
import Control.Plus (empty)
import ConvertableOptions (class ConvertOption, class ConvertOptionsWithDefaults, convertOptionsWithDefaults)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Variant (Variant, match)
import FRP.Event (Event, bang, makeEvent, subscribe)
import Rito.Color (Color)
import Rito.Core as C
import Rito.Texture (Texture)

data MeshBasicMaterialOptions = MeshBasicMaterialOptions

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
  )

type MeshBasicMaterialAll =
  (| MeshBasicMaterialOptional)

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
  }

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
  :: forall i lock payload
   . InitialMeshBasicMaterial i
  => i
  -> Event MeshBasicMaterial
  -> C.Material lock payload
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
    ) = makeEvent \k -> do
    me <- ids
    parent.raiseId me
    map (k (deleteFromCache { id: me }) *> _) $ flip subscribe k $
      bang
        ( makeMeshBasicMaterial
            { id: me
            , parent: parent.parent
            , scope: parent.scope
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
        )
        <|>
          ( map
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
          )

meshBasicMaterial_
  :: forall i lock payload
   . InitialMeshBasicMaterial i
  => i
  -> C.Material lock payload
meshBasicMaterial_ i = meshBasicMaterial i empty
