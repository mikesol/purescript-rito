module Rito.Interpret
  ( FFIThreeSnapshot
  , effectfulThreeInterpret
  , makeFFIThreeSnapshot
  ) where

import Prelude

import Bolson.Core (Scope(..))
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Random as R
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (get)
import Record.Builder (Builder, insert, build)
import Rito.Color (Color)
import Rito.Core as Core
import Rito.NormalMapTypes (NormalMapType(..))
import Rito.Renderers.WebGLRenderingPowerPreference as WPP
import Rito.Renderers.WebGLRenderingPrecision as WRP
import Rito.Texture (Texture)
import Rito.Undefinable (Undefinable, m2u)
import Rito.Vector2 (Vector2)
import Rito.Vector3 (Vector3)
import Type.Proxy (Proxy(..))
import Web.HTML (HTMLCanvasElement)

-- foreign
data FFIThreeSnapshot

{-
 TangentSpaceNormalMap = 0;
export const ObjectSpaceNormalMap = 1;
-}

foreign import makeFFIThreeSnapshot :: Effect FFIThreeSnapshot

--
foreign import webGLRender_ :: Core.WebGLRender -> FFIThreeSnapshot -> Effect Unit
--
foreign import makeWebGLRenderer_ :: Core.MakeWebGLRenderer' -> FFIThreeSnapshot -> Effect Unit
foreign import makePointLight_ :: Core.MakePointLight Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeAmbientLight_ :: Core.MakeAmbientLight Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeDirectionalLight_ :: Core.MakeDirectionalLight Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeScene_ :: Core.MakeScene Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeGroup_ :: Core.MakeGroup Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeMesh_ :: Core.MakeMesh Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeSphere_ :: Core.MakeSphere Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeBox_ :: Core.MakeBox Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeTorus_ :: Core.MakeTorus Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makePlane_ :: Core.MakePlane Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeMeshStandardMaterial_
  :: Core.MakeMeshStandardMaterial' Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import makeMeshBasicMaterial_
  :: Core.MakeMeshBasicMaterial' Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
--
foreign import deleteFromCache_
  :: Core.DeleteFromCache -> FFIThreeSnapshot -> Effect Unit
--
foreign import setRadius_ :: Core.SetRadius -> FFIThreeSnapshot -> Effect Unit
foreign import setWidthSegments_
  :: Core.SetWidthSegments -> FFIThreeSnapshot -> Effect Unit
foreign import setHeightSegments_
  :: Core.SetHeightSegments -> FFIThreeSnapshot -> Effect Unit
foreign import setPhiStart_
  :: Core.SetPhiStart -> FFIThreeSnapshot -> Effect Unit
foreign import setPhiLength_
  :: Core.SetPhiLength -> FFIThreeSnapshot -> Effect Unit
foreign import setThetaStart_
  :: Core.SetThetaStart -> FFIThreeSnapshot -> Effect Unit
foreign import setThetaLength_
  :: Core.SetThetaLength -> FFIThreeSnapshot -> Effect Unit
--
foreign import setMatrix4_ :: Core.SetMatrix4 -> FFIThreeSnapshot -> Effect Unit
foreign import setQuaternion_
  :: Core.SetQuaternion -> FFIThreeSnapshot -> Effect Unit
foreign import setRotateX_ :: Core.SetRotateX -> FFIThreeSnapshot -> Effect Unit
foreign import setRotateY_ :: Core.SetRotateY -> FFIThreeSnapshot -> Effect Unit
foreign import setRotateZ_ :: Core.SetRotateZ -> FFIThreeSnapshot -> Effect Unit
foreign import setTranslate_
  :: Core.SetTranslate -> FFIThreeSnapshot -> Effect Unit
foreign import setScale_ :: Core.SetScale -> FFIThreeSnapshot -> Effect Unit
foreign import setLookAt_ :: Core.SetLookAt -> FFIThreeSnapshot -> Effect Unit
foreign import setCenter_ :: Core.SetCenter -> FFIThreeSnapshot -> Effect Unit
foreign import getBoundingBox_
  :: Core.GetBoundingBox -> FFIThreeSnapshot -> Effect Unit
foreign import getBoundingSphere_
  :: Core.GetBoundingSphere -> FFIThreeSnapshot -> Effect Unit
-- point light
foreign import setDecay_ :: Core.SetDecay -> FFIThreeSnapshot -> Effect Unit
foreign import setIntensity_ :: Core.SetIntensity -> FFIThreeSnapshot -> Effect Unit
foreign import setDistance_ :: Core.SetDistance -> FFIThreeSnapshot -> Effect Unit
-- mesh standard material
foreign import setColor_ :: Core.SetColor -> FFIThreeSnapshot -> Effect Unit
foreign import setRoughness_
  :: Core.SetRoughness -> FFIThreeSnapshot -> Effect Unit
foreign import setMetalness_
  :: Core.SetMetalness -> FFIThreeSnapshot -> Effect Unit
foreign import setMap_ :: Core.SetMap -> FFIThreeSnapshot -> Effect Unit
foreign import setLightMap_
  :: Core.SetLightMap -> FFIThreeSnapshot -> Effect Unit
foreign import setLightMapIntensity_
  :: Core.SetLightMapIntensity -> FFIThreeSnapshot -> Effect Unit
foreign import setAoMap_ :: Core.SetAoMap -> FFIThreeSnapshot -> Effect Unit
foreign import setAoMapIntensity_
  :: Core.SetAoMapIntensity -> FFIThreeSnapshot -> Effect Unit
foreign import setEmissive_
  :: Core.SetEmissive -> FFIThreeSnapshot -> Effect Unit
foreign import setEmissiveIntensity_
  :: Core.SetEmissiveIntensity -> FFIThreeSnapshot -> Effect Unit
foreign import makePerspectiveCamera_
  :: Core.MakePerspectiveCamera Undefinable (Undefinable String) -> FFIThreeSnapshot -> Effect Unit
foreign import setEmissiveMap_
  :: Core.SetEmissiveMap -> FFIThreeSnapshot -> Effect Unit
foreign import setBumpMap_ :: Core.SetBumpMap -> FFIThreeSnapshot -> Effect Unit
foreign import setBumpScale_
  :: Core.SetBumpScale -> FFIThreeSnapshot -> Effect Unit
foreign import setNormalMap_
  :: Core.SetNormalMap -> FFIThreeSnapshot -> Effect Unit
foreign import setNormalMapType_
  :: Core.SetNormalMapType' -> FFIThreeSnapshot -> Effect Unit
foreign import setNormalScale_
  :: Core.SetNormalScale -> FFIThreeSnapshot -> Effect Unit
foreign import setDisplacementMap_
  :: Core.SetDisplacementMap -> FFIThreeSnapshot -> Effect Unit
foreign import setDisplacementScale_
  :: Core.SetDisplacementScale -> FFIThreeSnapshot -> Effect Unit
foreign import setDisplacementBias_
  :: Core.SetDisplacementBias -> FFIThreeSnapshot -> Effect Unit
foreign import setRoughnessMap_
  :: Core.SetRoughnessMap -> FFIThreeSnapshot -> Effect Unit
foreign import setMetalnessMap_
  :: Core.SetMetalnessMap -> FFIThreeSnapshot -> Effect Unit
foreign import setAlphaMap_
  :: Core.SetAlphaMap -> FFIThreeSnapshot -> Effect Unit
foreign import setEnvMap_ :: Core.SetEnvMap -> FFIThreeSnapshot -> Effect Unit
foreign import setEnvMapIntensity_
  :: Core.SetEnvMapIntensity -> FFIThreeSnapshot -> Effect Unit
foreign import setWireframe_
  :: Core.SetWireframe -> FFIThreeSnapshot -> Effect Unit
foreign import setWireframeLinewidth_
  :: Core.SetWireframeLinewidth -> FFIThreeSnapshot -> Effect Unit
foreign import setFlatShading_
  :: Core.SetFlatShading -> FFIThreeSnapshot -> Effect Unit
-- mesh
foreign import setRotationFromAxisAngle_ :: Core.SetRotationFromAxisAngle -> FFIThreeSnapshot -> Effect Unit
foreign import setRotationFromEuler_ :: Core.SetRotationFromEuler -> FFIThreeSnapshot -> Effect Unit
foreign import setRotationFromMatrix_ :: Core.SetRotationFromMatrix -> FFIThreeSnapshot -> Effect Unit
foreign import setRotationFromQuaternion_ :: Core.SetRotationFromQuaternion -> FFIThreeSnapshot -> Effect Unit
foreign import setRotateOnAxis_ :: Core.SetRotateOnAxis -> FFIThreeSnapshot -> Effect Unit
foreign import setRotateOnWorldAxis_ :: Core.SetRotateOnWorldAxis -> FFIThreeSnapshot -> Effect Unit
foreign import setTranslateOnAxis_ :: Core.SetTranslateOnAxis -> FFIThreeSnapshot -> Effect Unit
foreign import setTranslateX_ :: Core.SetTranslateX -> FFIThreeSnapshot -> Effect Unit
foreign import setTranslateY_ :: Core.SetTranslateY -> FFIThreeSnapshot -> Effect Unit
foreign import setTranslateZ_ :: Core.SetTranslateZ -> FFIThreeSnapshot -> Effect Unit
foreign import setPositionX_ :: Core.SetPositionX -> FFIThreeSnapshot -> Effect Unit
foreign import setPositionY_ :: Core.SetPositionY -> FFIThreeSnapshot -> Effect Unit
foreign import setPositionZ_ :: Core.SetPositionZ -> FFIThreeSnapshot -> Effect Unit
foreign import setScaleX_ :: Core.SetScaleX -> FFIThreeSnapshot -> Effect Unit
foreign import setScaleY_ :: Core.SetScaleY -> FFIThreeSnapshot -> Effect Unit
foreign import setScaleZ_ :: Core.SetScaleZ -> FFIThreeSnapshot -> Effect Unit
-- renderer
foreign import setSize_ :: Core.SetSize -> FFIThreeSnapshot -> Effect Unit
-- perspective camera
foreign import setAspect_ :: Core.SetAspect -> FFIThreeSnapshot -> Effect Unit
foreign import setFar_ :: Core.SetFar -> FFIThreeSnapshot -> Effect Unit
foreign import setFilmGauge_ :: Core.SetFilmGauge -> FFIThreeSnapshot -> Effect Unit
foreign import setFilmOffset_ :: Core.SetFilmOffset -> FFIThreeSnapshot -> Effect Unit
foreign import setFocus_ :: Core.SetFocus -> FFIThreeSnapshot -> Effect Unit
foreign import setFov_ :: Core.SetFov -> FFIThreeSnapshot -> Effect Unit
foreign import setNear_ :: Core.SetNear -> FFIThreeSnapshot -> Effect Unit
foreign import setZoom_ :: Core.SetZoom -> FFIThreeSnapshot -> Effect Unit
foreign import setFocalLength_ :: Core.SetFocalLength -> FFIThreeSnapshot -> Effect Unit
foreign import setViewOffset_ :: Core.SetViewOffset -> FFIThreeSnapshot -> Effect Unit
--
foreign import connectToScene_
  :: Core.ConnectToScene -> FFIThreeSnapshot -> Effect Unit
foreign import connectMesh_
  :: Core.ConnectMesh -> FFIThreeSnapshot -> Effect Unit
foreign import connectGeometry_
  :: Core.ConnectGeometry -> FFIThreeSnapshot -> Effect Unit
foreign import connectMaterial_
  :: Core.ConnectMaterial -> FFIThreeSnapshot -> Effect Unit
foreign import disconnect_
  :: Core.Disconnect -> FFIThreeSnapshot -> Effect Unit

class FFIMe i o | i -> o where
  ffiMe :: i -> o

instance FFIMe Int Int where
  ffiMe = identity

instance FFIMe Boolean Boolean where
  ffiMe = identity

instance FFIMe Scope (Undefinable String) where
  ffiMe Global = m2u Nothing
  ffiMe (Local s) = m2u (Just s)

instance FFIMe Number Number where
  ffiMe = identity

instance FFIMe String String where
  ffiMe = identity

instance FFIMe Color Color where
  ffiMe = identity

instance FFIMe Texture Texture where
  ffiMe = identity

instance FFIMe Vector2 Vector2 where
  ffiMe = identity

instance FFIMe Vector3 Vector3 where
  ffiMe = identity

instance FFIMe HTMLCanvasElement HTMLCanvasElement where
  ffiMe = identity

instance FFIMe NormalMapType Int where
  ffiMe TangentSpaceNormalMap = 0
  ffiMe ObjectSpaceNormalMap = 1

instance FFIMe WRP.WebGLRenderingPrecision String where
  ffiMe WRP.High = "highp"
  ffiMe WRP.Medium = "mediump"
  ffiMe WRP.Low = "lowp"

instance FFIMe WPP.WebGLRenderingPowerPreference String where
  ffiMe WPP.High = "high-performance"
  ffiMe WPP.Default = "default"
  ffiMe WPP.Low = "low-power"

instance FFIMe a b => FFIMe (Maybe a) (Undefinable b) where
  ffiMe = m2u <<< map ffiMe

class FFIIze :: forall k. k -> Row Type -> Row Type -> Constraint
class FFIIze ri i o | ri -> o where
  ffiize' :: Proxy ri -> { | i } -> Builder {} { | o }

instance FFIIze RL.Nil i () where
  ffiize' _ _ = identity

instance
  ( IsSymbol key
  , FFIMe val ffid
  , FFIIze rest i o'
  , Lacks key o'
  , Lacks key i'
  , Cons key ffid o' o
  , Cons key val i' i
  ) =>
  FFIIze (RL.Cons key val rest) i o where
  ffiize' _ i = insert key (ffiMe (get key i)) <<< ffiize' (Proxy :: _ rest) i
    where
    key = Proxy :: _ key

foreign import stripUndefined_ :: forall a. a -> a

ffiize :: forall ri i o. RowToList i ri => FFIIze ri i o => { | i } -> { | o }
ffiize i = stripUndefined_ (build (ffiize' (Proxy :: _ ri) i) {})

effectfulThreeInterpret :: Core.ThreeInterpret (FFIThreeSnapshot -> Effect Unit)
effectfulThreeInterpret = Core.ThreeInterpret
  { ids: map show R.random
  -- render
  , webGLRender: webGLRender_
  -- makers
  , makeWebGLRenderer: lcmap ffiize makeWebGLRenderer_
  , makeScene: lcmap ffiize makeScene_
  , makeGroup: lcmap ffiize makeGroup_
  , makeMesh: lcmap ffiize makeMesh_
  , makeSphere: lcmap ffiize makeSphere_
  , makeBox: lcmap ffiize makeBox_
  , makeTorus: lcmap ffiize makeTorus_
  , makePlane: lcmap ffiize makePlane_
  , makeDirectionalLight: lcmap ffiize makeDirectionalLight_
  , makeAmbientLight: lcmap ffiize makeAmbientLight_
  , makePointLight: lcmap ffiize makePointLight_
  , makePerspectiveCamera: lcmap ffiize makePerspectiveCamera_
  , makeMeshBasicMaterial: lcmap ffiize makeMeshBasicMaterial_
  , makeMeshStandardMaterial: lcmap ffiize makeMeshStandardMaterial_
  -- sphere geometry
  , setRadius: setRadius_
  , setWidthSegments: setWidthSegments_
  , setHeightSegments: setHeightSegments_
  , setPhiStart: setPhiStart_
  , setPhiLength: setPhiLength_
  , setThetaStart: setThetaStart_
  , setThetaLength: setThetaLength_
  -- buffer geometry
  , setMatrix4: setMatrix4_
  , setQuaternion: setQuaternion_
  , setRotateX: setRotateX_
  , setRotateY: setRotateY_
  , setRotateZ: setRotateZ_
  , setTranslate: setTranslate_
  , setScale: setScale_
  , setLookAt: setLookAt_
  , setCenter: setCenter_
  , getBoundingBox: getBoundingBox_
  , getBoundingSphere: getBoundingSphere_
  -- material
  , setColor: setColor_
  , setRoughness: setRoughness_
  , setMetalness: setMetalness_
  , setMap: setMap_
  , setLightMap: setLightMap_
  , setLightMapIntensity: setLightMapIntensity_
  , setAoMap: setAoMap_
  , setAoMapIntensity: setAoMapIntensity_
  , setEmissive: setEmissive_
  , setEmissiveIntensity: setEmissiveIntensity_
  , setEmissiveMap: setEmissiveMap_
  , setBumpMap: setBumpMap_
  , setBumpScale: setBumpScale_
  , setNormalMap: setNormalMap_
  , setNormalMapType: lcmap (over (prop (Proxy :: _ "normalMapType")) ffiMe)
      setNormalMapType_
  , setNormalScale: setNormalScale_
  , setDisplacementMap: setDisplacementMap_
  , setDisplacementScale: setDisplacementScale_
  , setDisplacementBias: setDisplacementBias_
  , setRoughnessMap: setRoughnessMap_
  , setMetalnessMap: setMetalnessMap_
  , setAlphaMap: setAlphaMap_
  , setEnvMap: setEnvMap_
  , setEnvMapIntensity: setEnvMapIntensity_
  , setWireframe: setWireframe_
  , setWireframeLinewidth: setWireframeLinewidth_
  , setFlatShading: setFlatShading_
  --
  , setRotationFromAxisAngle: setRotationFromAxisAngle_
  , setRotationFromEuler: setRotationFromEuler_
  , setRotationFromMatrix: setRotationFromMatrix_
  , setRotationFromQuaternion: setRotationFromQuaternion_
  , setRotateOnAxis: setRotateOnAxis_
  , setRotateOnWorldAxis: setRotateOnWorldAxis_
  , setTranslateOnAxis: setTranslateOnAxis_
  , setTranslateX: setTranslateX_
  , setTranslateY: setTranslateY_
  , setTranslateZ: setTranslateZ_
  , setPositionX: setPositionX_
  , setPositionY: setPositionY_
  , setPositionZ: setPositionZ_
  , setScaleX: setScaleX_
  , setScaleY: setScaleY_
  , setScaleZ: setScaleZ_
  -- point light
  , setDecay: setDecay_
  , setDistance: setDistance_
  , setIntensity: setIntensity_
  -- perspective camera
  , setAspect: setAspect_
  , setFar: setFar_
  , setFilmGauge: setFilmGauge_
  , setFilmOffset: setFilmOffset_
  , setFocus: setFocus_
  , setFov: setFov_
  , setNear: setNear_
  , setZoom: setZoom_
  , setFocalLength: setFocalLength_
  , setViewOffset: setViewOffset_
  -- renderer
  , setSize: setSize_
  -- connectors
  , connectMesh: connectMesh_
  , connectGeometry: connectGeometry_
  , connectMaterial: connectMaterial_
  , connectToScene: connectToScene_
  , disconnect: disconnect_
  --
  , deleteFromCache: deleteFromCache_
  }
