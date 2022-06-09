module Rito.Interpret
  ( FFIThreeSnapshot
  , effectfulThreeInterpret
  , makeFFIThreeSnapshot
  , threeAff
  , orbitControlsAff
  , css2DRendererAff
  ) where

import Prelude

import Bolson.Core (Scope(..))
import Control.Promise (Promise, toAffE)
import Data.Lens (over)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Profunctor (lcmap)
import Data.Symbol (class IsSymbol)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Random as R
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Prim.RowList as RL
import Record (get)
import Record.Builder (Builder, insert, build)
import Rito.Color (Color)
import Rito.Core (OrbitControls)
import Rito.Core as Core
import Rito.NormalMapTypes (NormalMapType(..))
import Rito.Renderers.WebGLRenderingPowerPreference as WPP
import Rito.Renderers.WebGLRenderingPrecision as WRP
import Rito.THREE as THREE
import Rito.Texture (Texture)
import Rito.Undefinable (Undefinable, m2u)
import Rito.Vector2 (Vector2)
import Rito.Vector3 (Vector3)
import Type.Proxy (Proxy(..))
import Web.DOM as Web.DOM
import Web.HTML (HTMLCanvasElement)

type Payload = FFIThreeSnapshot -> Effect Unit

-- foreign
data FFIThreeSnapshot

foreign import three :: Effect (Promise THREE.Three)
threeAff :: Aff THREE.Three
threeAff = toAffE three
foreign import orbitControls :: Effect (Promise THREE.OrbitControls)
orbitControlsAff :: Aff THREE.OrbitControls
orbitControlsAff = toAffE orbitControls
type CSS2DRendererModuleCaps =
  { "CSS2DRenderer" :: THREE.CSS2DRenderer
  , "CSS2DObject" :: THREE.CSS2DObject
  }
type CSS2DRendererModule =
  { css2DRenderer :: THREE.CSS2DRenderer
  , css2DObject :: THREE.CSS2DObject
  }
foreign import css2DRenderer :: Effect (Promise CSS2DRendererModuleCaps)
css2DRendererAff :: Aff CSS2DRendererModule
css2DRendererAff = toAffE css2DRenderer <#>
  \{ "CSS2DRenderer": css2DRenderer', "CSS2DObject": css2DObject } ->
    { css2DRenderer: css2DRenderer', css2DObject }
foreign import makeFFIThreeSnapshot
  :: THREE.ThreeStuff
  -> Effect FFIThreeSnapshot

--
foreign import webGLRender_ :: Core.WebGLRender -> Payload
foreign import css2DRender_ :: Core.CSS2DRender -> Payload
--
foreign import makeWebGLRenderer_ :: Core.MakeWebGLRenderer' -> Payload
foreign import makeCSS2DRenderer_ :: Core.MakeCSS2DRenderer -> Payload
foreign import makePointLight_
  :: Core.MakePointLight Undefinable (Undefinable String) -> Payload
foreign import makeAmbientLight_
  :: Core.MakeAmbientLight Undefinable (Undefinable String) -> Payload
foreign import makeDirectionalLight_
  :: Core.MakeDirectionalLight Undefinable (Undefinable String) -> Payload
foreign import makeScene_
  :: Core.MakeScene Undefinable (Undefinable String) -> Payload
foreign import makeGroup_
  :: Core.MakeGroup Undefinable (Undefinable String) -> Payload
foreign import makeMesh_
  :: Core.MakeMesh Undefinable (Undefinable String) -> Payload
foreign import makeInstancedMesh_
  :: Core.MakeInstancedMesh Undefinable (Undefinable String) -> Payload
foreign import makeCapsule_
  :: Core.MakeCapsule Undefinable (Undefinable String) -> Payload
foreign import makeSphere_
  :: Core.MakeSphere Undefinable (Undefinable String) -> Payload
foreign import makeBox_
  :: Core.MakeBox Undefinable (Undefinable String) -> Payload
foreign import makeTorus_
  :: Core.MakeTorus Undefinable (Undefinable String) -> Payload
foreign import makePlane_
  :: Core.MakePlane Undefinable (Undefinable String) -> Payload
foreign import makeMeshStandardMaterial_
  :: Core.MakeMeshStandardMaterial' Undefinable (Undefinable String) -> Payload
foreign import makeMeshBasicMaterial_
  :: Core.MakeMeshBasicMaterial' Undefinable (Undefinable String) -> Payload
foreign import makeCSS2DObject_
  :: Core.MakeCSS2DObject Undefinable (Undefinable String) -> Payload
--
foreign import deleteFromCache_ :: Core.DeleteFromCache -> Payload
--
foreign import setInstancedMeshMatrix4_
  :: Core.SetInstancedMeshMatrix4 -> Payload
foreign import setInstancedMeshColor_ :: Core.SetInstancedMeshColor -> Payload
foreign import setSingleInstancedMeshMatrix4_
  :: Core.SetSingleInstancedMeshMatrix4 -> Payload
foreign import setSingleInstancedMeshColor_
  :: Core.SetSingleInstancedMeshColor -> Payload
--
foreign import setWidth_ :: Core.SetWidth -> Payload
foreign import setHeight_ :: Core.SetHeight -> Payload
foreign import setDepth_ :: Core.SetDepth -> Payload
--
foreign import setRadius_ :: Core.SetRadius -> Payload
foreign import setLength_ :: Core.SetLength -> Payload
foreign import setWidthSegments_ :: Core.SetWidthSegments -> Payload
foreign import setHeightSegments_ :: Core.SetHeightSegments -> Payload
foreign import setDepthSegments_ :: Core.SetDepthSegments -> Payload
foreign import setCapSegments_ :: Core.SetCapSegments -> Payload
foreign import setRadialSegments_ :: Core.SetRadialSegments -> Payload
foreign import setPhiStart_ :: Core.SetPhiStart -> Payload
foreign import setPhiLength_ :: Core.SetPhiLength -> Payload
foreign import setThetaStart_ :: Core.SetThetaStart -> Payload
foreign import setThetaLength_ :: Core.SetThetaLength -> Payload
--
foreign import setMatrix4_ :: Core.SetMatrix4 -> Payload
foreign import setQuaternion_ :: Core.SetQuaternion -> Payload
foreign import setRotateX_ :: Core.SetRotateX -> Payload
foreign import setRotateY_ :: Core.SetRotateY -> Payload
foreign import setRotateZ_ :: Core.SetRotateZ -> Payload
foreign import setTranslate_ :: Core.SetTranslate -> Payload
foreign import setScale_ :: Core.SetScale -> Payload
foreign import setLookAt_ :: Core.SetLookAt -> Payload
foreign import setCenter_ :: Core.SetCenter -> Payload
foreign import getBoundingBox_ :: Core.GetBoundingBox -> Payload
foreign import getBoundingSphere_ :: Core.GetBoundingSphere -> Payload
-- point light
foreign import setDecay_ :: Core.SetDecay -> Payload
foreign import setIntensity_ :: Core.SetIntensity -> Payload
foreign import setDistance_ :: Core.SetDistance -> Payload
-- mesh standard material
foreign import setColor_ :: Core.SetColor -> Payload
foreign import setRoughness_ :: Core.SetRoughness -> Payload
foreign import setMetalness_ :: Core.SetMetalness -> Payload
foreign import setMap_ :: Core.SetMap -> Payload
foreign import setLightMap_ :: Core.SetLightMap -> Payload
foreign import setLightMapIntensity_ :: Core.SetLightMapIntensity -> Payload
foreign import setAoMap_ :: Core.SetAoMap -> Payload
foreign import setAoMapIntensity_ :: Core.SetAoMapIntensity -> Payload
foreign import setEmissive_ :: Core.SetEmissive -> Payload
foreign import setEmissiveIntensity_ :: Core.SetEmissiveIntensity -> Payload
foreign import makePerspectiveCamera_
  :: Core.MakePerspectiveCamera Undefinable (Undefinable String) -> Payload
foreign import setEmissiveMap_ :: Core.SetEmissiveMap -> Payload
foreign import setBumpMap_ :: Core.SetBumpMap -> Payload
foreign import setBumpScale_ :: Core.SetBumpScale -> Payload
foreign import setNormalMap_ :: Core.SetNormalMap -> Payload
foreign import setNormalMapType_ :: Core.SetNormalMapType' -> Payload
foreign import setNormalScale_ :: Core.SetNormalScale -> Payload
foreign import setDisplacementMap_ :: Core.SetDisplacementMap -> Payload
foreign import setDisplacementScale_ :: Core.SetDisplacementScale -> Payload
foreign import setDisplacementBias_ :: Core.SetDisplacementBias -> Payload
foreign import setRoughnessMap_ :: Core.SetRoughnessMap -> Payload
foreign import setMetalnessMap_ :: Core.SetMetalnessMap -> Payload
foreign import setAlphaMap_ :: Core.SetAlphaMap -> Payload
foreign import setEnvMap_ :: Core.SetEnvMap -> Payload
foreign import setEnvMapIntensity_ :: Core.SetEnvMapIntensity -> Payload
foreign import setWireframe_ :: Core.SetWireframe -> Payload
foreign import setWireframeLinewidth_ :: Core.SetWireframeLinewidth -> Payload
foreign import setFlatShading_ :: Core.SetFlatShading -> Payload
-- scene
foreign import setBackgroundCubeTexture_ :: Core.SetBackgroundCubeTexture -> Payload
foreign import setBackgroundTexture_ :: Core.SetBackgroundTexture -> Payload
foreign import setBackgroundColor_ :: Core.SetBackgroundColor -> Payload
-- (faux) listeners
foreign import setOnClick_ :: Core.SetOnClick -> Payload
foreign import setOnMouseDown_ :: Core.SetOnMouseDown -> Payload
foreign import setOnMouseUp_ :: Core.SetOnMouseUp -> Payload
foreign import setOnMouseMove_ :: Core.SetOnMouseMove -> Payload
foreign import setOnTouchStart_ :: Core.SetOnTouchStart -> Payload
foreign import setOnTouchEnd_ :: Core.SetOnTouchEnd -> Payload
foreign import setOnTouchMove_ :: Core.SetOnTouchMove -> Payload
foreign import setOnTouchCancel_ :: Core.SetOnTouchCancel -> Payload
foreign import removeOnClick_ :: Core.RemoveOnClick -> Payload
foreign import removeOnMouseDown_ :: Core.RemoveOnMouseDown -> Payload
foreign import removeOnMouseUp_ :: Core.RemoveOnMouseUp -> Payload
foreign import removeOnMouseMove_ :: Core.RemoveOnMouseMove -> Payload
foreign import removeOnTouchStart_ :: Core.RemoveOnTouchStart -> Payload
foreign import removeOnTouchEnd_ :: Core.RemoveOnTouchEnd -> Payload
foreign import removeOnTouchMove_ :: Core.RemoveOnTouchMove -> Payload
foreign import removeOnTouchCancel_ :: Core.RemoveOnTouchCancel -> Payload
-- (faux) IMlisteners
foreign import setIMOnClick_ :: Core.SetIMOnClick -> Payload
foreign import setIMOnMouseDown_ :: Core.SetIMOnMouseDown -> Payload
foreign import setIMOnMouseUp_ :: Core.SetIMOnMouseUp -> Payload
foreign import setIMOnMouseMove_ :: Core.SetIMOnMouseMove -> Payload
foreign import setIMOnTouchStart_ :: Core.SetIMOnTouchStart -> Payload
foreign import setIMOnTouchEnd_ :: Core.SetIMOnTouchEnd -> Payload
foreign import setIMOnTouchMove_ :: Core.SetIMOnTouchMove -> Payload
foreign import setIMOnTouchCancel_ :: Core.SetIMOnTouchCancel -> Payload
foreign import removeIMOnClick_ :: Core.RemoveIMOnClick -> Payload
foreign import removeIMOnMouseDown_ :: Core.RemoveIMOnMouseDown -> Payload
foreign import removeIMOnMouseUp_ :: Core.RemoveIMOnMouseUp -> Payload
foreign import removeIMOnMouseMove_ :: Core.RemoveIMOnMouseMove -> Payload
foreign import removeIMOnTouchStart_ :: Core.RemoveIMOnTouchStart -> Payload
foreign import removeIMOnTouchEnd_ :: Core.RemoveIMOnTouchEnd -> Payload
foreign import removeIMOnTouchMove_ :: Core.RemoveIMOnTouchMove -> Payload
foreign import removeIMOnTouchCancel_ :: Core.RemoveIMOnTouchCancel -> Payload
-- mesh
foreign import setRotationFromAxisAngle_
  :: Core.SetRotationFromAxisAngle -> Payload
foreign import setRotationFromEuler_ :: Core.SetRotationFromEuler -> Payload
foreign import setRotationFromMatrix_ :: Core.SetRotationFromMatrix -> Payload
foreign import setRotationFromQuaternion_
  :: Core.SetRotationFromQuaternion -> Payload
foreign import setRotateOnAxis_ :: Core.SetRotateOnAxis -> Payload
foreign import setRotateOnWorldAxis_ :: Core.SetRotateOnWorldAxis -> Payload
foreign import setTranslateOnAxis_ :: Core.SetTranslateOnAxis -> Payload
foreign import setTranslateX_ :: Core.SetTranslateX -> Payload
foreign import setTranslateY_ :: Core.SetTranslateY -> Payload
foreign import setTranslateZ_ :: Core.SetTranslateZ -> Payload
foreign import setPositionX_ :: Core.SetPositionX -> Payload
foreign import setPositionY_ :: Core.SetPositionY -> Payload
foreign import setPositionZ_ :: Core.SetPositionZ -> Payload
foreign import setScaleX_ :: Core.SetScaleX -> Payload
foreign import setScaleY_ :: Core.SetScaleY -> Payload
foreign import setScaleZ_ :: Core.SetScaleZ -> Payload
-- renderer
foreign import setSize_ :: Core.SetSize -> Payload
-- camera
foreign import withWorldDirection_ :: Core.WithWorldDirection Payload -> Payload
-- orbit controls
foreign import setTarget_ :: Core.SetTarget -> Payload
-- perspective camera
foreign import setAspect_ :: Core.SetAspect -> Payload
foreign import setFar_ :: Core.SetFar -> Payload
foreign import setFilmGauge_ :: Core.SetFilmGauge -> Payload
foreign import setFilmOffset_ :: Core.SetFilmOffset -> Payload
foreign import setFocus_ :: Core.SetFocus -> Payload
foreign import setFov_ :: Core.SetFov -> Payload
foreign import setNear_ :: Core.SetNear -> Payload
foreign import setZoom_ :: Core.SetZoom -> Payload
foreign import setFocalLength_ :: Core.SetFocalLength -> Payload
foreign import setViewOffset_ :: Core.SetViewOffset -> Payload
--
foreign import connectToScene_ :: Core.ConnectToScene -> Payload
foreign import connectMesh_ :: Core.ConnectMesh -> Payload
foreign import connectScene_ :: Core.ConnectScene -> Payload
foreign import connectCamera_ :: Core.ConnectCamera -> Payload
foreign import connectGeometry_ :: Core.ConnectGeometry -> Payload
foreign import connectMaterial_ :: Core.ConnectMaterial -> Payload
foreign import disconnect_ :: Core.Disconnect -> Payload

class FFIMe i o | i -> o where
  ffiMe :: i -> o

instance FFIMe Int Int where
  ffiMe = identity

instance FFIMe Web.DOM.Element Web.DOM.Element where
  ffiMe = identity

instance FFIMe OrbitControls OrbitControls where
  ffiMe = identity

instance (RowToList i ri, FFIIze ri i o) => FFIMe { | i } { | o } where
  ffiMe = ffiize

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

effectfulThreeInterpret :: Core.ThreeInterpret (Payload)
effectfulThreeInterpret = Core.ThreeInterpret
  { ids: map show R.random
  -- render
  , webGLRender: webGLRender_
  , css2DRender: css2DRender_
  -- makers
  , makeWebGLRenderer: lcmap ffiize makeWebGLRenderer_
  , makeCSS2DRenderer: lcmap ffiize makeCSS2DRenderer_
  , makeScene: lcmap ffiize makeScene_
  , makeGroup: lcmap ffiize makeGroup_
  , makeMesh: lcmap ffiize makeMesh_
  , makeInstancedMesh: lcmap ffiize makeInstancedMesh_
  , makeCapsule: lcmap ffiize makeCapsule_
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
  , makeCSS2DObject: lcmap ffiize makeCSS2DObject_
  -- scene
  , setBackgroundCubeTexture: setBackgroundCubeTexture_
  , setBackgroundTexture: setBackgroundTexture_
  , setBackgroundColor: setBackgroundColor_
  -- (faux) listeners
  , setOnClick: setOnClick_
  , setOnMouseDown: setOnMouseDown_
  , setOnMouseUp: setOnMouseUp_
  , setOnMouseMove: setOnMouseMove_
  , setOnTouchStart: setOnTouchStart_
  , setOnTouchEnd: setOnTouchEnd_
  , setOnTouchMove: setOnTouchMove_
  , setOnTouchCancel: setOnTouchCancel_
  , removeOnClick: removeOnClick_
  , removeOnMouseDown: removeOnMouseDown_
  , removeOnMouseUp: removeOnMouseUp_
  , removeOnMouseMove: removeOnMouseMove_
  , removeOnTouchStart: removeOnTouchStart_
  , removeOnTouchEnd: removeOnTouchEnd_
  , removeOnTouchMove: removeOnTouchMove_
  , removeOnTouchCancel: removeOnTouchCancel_
  -- (faux) im listeners
  , setIMOnClick: setIMOnClick_
  , setIMOnMouseDown: setIMOnMouseDown_
  , setIMOnMouseUp: setIMOnMouseUp_
  , setIMOnMouseMove: setIMOnMouseMove_
  , setIMOnTouchStart: setIMOnTouchStart_
  , setIMOnTouchEnd: setIMOnTouchEnd_
  , setIMOnTouchMove: setIMOnTouchMove_
  , setIMOnTouchCancel: setIMOnTouchCancel_
  , removeIMOnClick: removeIMOnClick_
  , removeIMOnMouseDown: removeIMOnMouseDown_
  , removeIMOnMouseUp: removeIMOnMouseUp_
  , removeIMOnMouseMove: removeIMOnMouseMove_
  , removeIMOnTouchStart: removeIMOnTouchStart_
  , removeIMOnTouchEnd: removeIMOnTouchEnd_
  , removeIMOnTouchMove: removeIMOnTouchMove_
  , removeIMOnTouchCancel: removeIMOnTouchCancel_
  -- box geometry
  , setWidth: setWidth_
  , setHeight: setHeight_
  , setDepth: setDepth_
  -- capsule
  , setLength: setLength_
  , setCapSegments: setCapSegments_
  , setRadialSegments: setRadialSegments_
  -- sphere geometry
  , setRadius: setRadius_
  , setWidthSegments: setWidthSegments_
  , setHeightSegments: setHeightSegments_
  , setDepthSegments: setDepthSegments_
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
  -- instancedMesh
  , setInstancedMeshMatrix4: setInstancedMeshMatrix4_
  , setInstancedMeshColor: setInstancedMeshColor_
  , setSingleInstancedMeshMatrix4: setSingleInstancedMeshMatrix4_
  , setSingleInstancedMeshColor: setSingleInstancedMeshColor_
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
  -- camera
  , withWorldDirection: withWorldDirection_
  -- orbit controls
  , setTarget: setTarget_
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
  , connectScene: connectScene_
  , connectCamera: connectCamera_
  , connectGeometry: connectGeometry_
  , connectMaterial: connectMaterial_
  , connectToScene: connectToScene_
  , disconnect: disconnect_
  --
  , deleteFromCache: deleteFromCache_
  }
