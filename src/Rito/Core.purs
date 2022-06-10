module Rito.Core where

import Prelude

import Bolson.Core (Entity, Scope)
import Bolson.Core as Bolson
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Deku.Core (ANut)
import Effect (Effect)
import FRP.Event (Event)
import Record (union)
import Rito.Box (Box)
import Rito.Box as Box
import Rito.Color (Color)
import Rito.CubeTexture (CubeTexture)
import Rito.Euler (Euler)
import Rito.Matrix4 (Matrix4)
import Rito.NormalMapTypes (NormalMapType)
import Rito.Quaternion (Quaternion)
import Rito.Renderers.WebGLRenderingPowerPreference as WPP
import Rito.Renderers.WebGLRenderingPrecision as WRP
import Rito.Sphere (Sphere)
import Rito.Sphere as Sphere
import Rito.Texture (Texture)
import Rito.Undefinable (Undefinable)
import Rito.Vector2 (Vector2)
import Rito.Vector3 (Vector3)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as Web.DOM
import Web.HTML (HTMLCanvasElement)
import Web.TouchEvent (Touch)
import Web.UIEvent.MouseEvent (MouseEvent)

class Sceneable ctor where
  toScene
    :: forall lock payload
     . Entity Void (ctor lock payload) Effect lock
    -> Entity Void (Sceneful lock payload) Effect lock

class Groupable ctor where
  toGroup
    :: forall lock payload
     . Entity Void (ctor lock payload) Effect lock
    -> Entity Void (Groupful lock payload) Effect lock

cameraToGroup :: forall lock payload. Camera lock payload -> Entity Void (Groupful lock payload) Effect lock
cameraToGroup c = Bolson.Element' $ (unsafeCoerce :: Camera lock payload -> Groupful lock payload) c

type Ctor payload =
  { parent :: Maybe String
  , scope :: Scope
  , raiseId :: String -> Effect Unit
  }
  -> SimpleCtor payload

type SimpleCtor payload =
  ThreeInterpret payload
  -> Event payload

newtype Renderer (lock :: Type) payload = Renderer (Ctor payload)
type ARenderer lock payload = Entity Void (Renderer lock payload) Effect lock
newtype Light (lock :: Type) payload = Light (Ctor payload)
type ALight lock payload = Entity Void (Light lock payload) Effect lock
newtype CSS2DObject (lock :: Type) payload = CSS2DObject (Ctor payload)
type ACSS2DObject lock payload = Entity Void (CSS2DObject lock payload) Effect
  lock
newtype Geometry (lock :: Type) payload = Geometry (Ctor payload)
newtype Material (lock :: Type) payload = Material (Ctor payload)
newtype Mesh (lock :: Type) payload = Mesh (Ctor payload)
newtype Instance (lock :: Type) payload = Instance (SimpleCtor payload)
type AMesh lock payload = Entity Void (Mesh lock payload) Effect lock
newtype Group (lock :: Type) payload = Group (Ctor payload)
type AGroup lock payload = Entity Void (Group lock payload) Effect lock
newtype Scene (lock :: Type) payload = Scene (Ctor payload)
-- type AScene lock payload = Entity Void (Scene lock payload) Effect lock
newtype Sceneful (lock :: Type) payload = Sceneful (Ctor payload)
type ASceneful lock payload = Entity Void (Sceneful lock payload) Effect lock
newtype Groupful (lock :: Type) payload = Groupful (Ctor payload)
type AGroupful lock payload = Entity Void (Groupful lock payload) Effect lock
newtype Camera (lock :: Type) payload = Camera (Ctor payload)
-- type ACamera lock payload = Entity Void (Camera lock payload) Effect lock

instance Sceneable Light where
  toScene = unsafeCoerce

instance Sceneable CSS2DObject where
  toScene = unsafeCoerce

instance Sceneable Mesh where
  toScene = unsafeCoerce

instance Sceneable Camera where
  toScene = unsafeCoerce

instance Sceneable Group where
  toScene = unsafeCoerce

instance Groupable Light where
  toGroup = unsafeCoerce

instance Groupable Mesh where
  toGroup = unsafeCoerce

instance Groupable Camera where
  toGroup = unsafeCoerce

instance Groupable Group where
  toGroup = unsafeCoerce

instance Groupable Groupful where
  toGroup = unsafeCoerce

instance Groupable Sceneful where
  toGroup = unsafeCoerce

instance Groupable CSS2DObject where
  toGroup = unsafeCoerce

type WebGLRender = { id :: String, scene :: String, camera :: String }
type MakeWebGLRenderer =
  { id :: String
  , camera :: String
  | InitializeWebGLRenderer' WRP.WebGLRenderingPrecision
      WPP.WebGLRenderingPowerPreference
  }
type MakeWebGLRenderer' =
  { id :: String
  , camera :: String
  | InitializeWebGLRenderer' String String
  }
type InitializeWebGLRenderer' precision powerPreference =
  ( canvas :: HTMLCanvasElement
  , precision :: precision
  , alpha :: Boolean
  , premultipliedAlpha :: Boolean
  , antialias :: Boolean
  , stencil :: Boolean
  , preserveDrawingBuffer :: Boolean
  , powerPreference :: powerPreference
  , failIfMajorPerformanceCaveat :: Boolean
  , depth :: Boolean
  , logarithmicDepthBuffer :: Boolean
  )

newtype InitializeWebGLRenderer = InitializeWebGLRenderer
  { | InitializeWebGLRenderer' WRP.WebGLRenderingPrecision
      WPP.WebGLRenderingPowerPreference
  }

type CSS2DRender = { id :: String, scene :: String, camera :: String }

type MakeCSS2DRenderer =
  { id :: String
  , camera :: String
  , canvas :: HTMLCanvasElement
  , element :: Web.DOM.Element
  }

type MakeScene f s =
  { id :: String
  , scope :: s
  , parent :: f String
  }
type MakeGroup f s =
  { id :: String
  , scope :: s
  , parent :: f String
  }
type MakeMesh f s =
  { id :: String
  , scope :: s
  , parent :: f String
  }
type MakeInstancedMesh f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , geometry :: String
  , material :: String
  , count :: Int
  }
type MakeSphere f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeSphere'
  }
newtype InitializeSphere = InitializeSphere { | InitializeSphere' }
type InitializeSphere' =
  ( radius :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , phiStart :: Number
  , phiLength :: Number
  , thetaStart :: Number
  , thetaLength :: Number
  )
-- css
type MakeCSS2DObject f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeCSS2DObject' Web.DOM.Element
  }
type InitializeCSS2DObject' (nut :: Type) =
  ( nut :: nut
  )
newtype InitializeCSS2DObject = InitializeCSS2DObject
  { | InitializeCSS2DObject' ANut }
-- light
type MakePointLight f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializePointLight'
  }
type InitializePointLight' =
  ( color :: Color
  , intensity :: Number
  , distance :: Number
  , decay :: Number
  )
newtype InitializePointLight = InitializePointLight { | InitializePointLight' }
type MakeAmbientLight f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeAmbientLight'
  }
type InitializeAmbientLight' =
  ( color :: Color
  , intensity :: Number
  )
newtype InitializeAmbientLight = InitializeAmbientLight
  { | InitializeAmbientLight' }
type MakeDirectionalLight f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeDirectionalLight'
  }
type InitializeDirectionalLight' =
  ( color :: Color
  , intensity :: Number
  )
newtype InitializeDirectionalLight = InitializeDirectionalLight
  { | InitializeDirectionalLight' }
type MakeMeshStandardMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | (InitializeMeshStandardMaterial' Maybe NormalMapType)
  }
type MakeMeshStandardMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | (InitializeMeshStandardMaterial' Undefinable Int)
  }
type InitializeMeshStandardMaterial' (opt :: Type -> Type) normalMapType =
  ( color :: opt Color
  , roughness :: opt Number
  , metalness :: opt Number
  , map :: opt Texture
  , lightMap :: opt Texture
  , lightMapIntensity :: opt Number
  , aoMap :: opt Texture
  , aoMapIntensity :: opt Number
  , emissive :: opt Color
  , emissiveIntensity :: opt Number
  , emissiveMap :: opt Texture
  , bumpMap :: opt Texture
  , bumpScale :: opt Number
  , normalMap :: opt Texture
  , normalMapType :: opt normalMapType
  , normalScale :: opt Vector2
  , displacementMap :: opt Texture
  , displacementScale :: opt Number
  , displacementBias :: opt Number
  , roughnessMap :: opt Texture
  , metalnessMap :: opt Texture
  , alphaMap :: opt Texture
  , envMap :: opt Texture
  , envMapIntensity :: opt Number
  , wireframe :: opt Boolean
  , wireframeLinewidth :: opt Number
  , flatShading :: opt Boolean
  )
newtype InitializeMeshStandardMaterial = InitializeMeshStandardMaterial
  { | (InitializeMeshStandardMaterial' Maybe NormalMapType) }
type MakeMeshBasicMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | (InitializeMeshBasicMaterial' Maybe)
  }
type MakeMeshBasicMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | (InitializeMeshBasicMaterial' Undefinable)
  }
type InitializeMeshBasicMaterial' (opt :: Type -> Type) =
  ( color :: opt Color
  , map :: opt Texture
  , lightMap :: opt Texture
  , lightMapIntensity :: opt Number
  , aoMap :: opt Texture
  , aoMapIntensity :: opt Number
  , alphaMap :: opt Texture
  , envMap :: opt Texture
  , wireframe :: opt Boolean
  , wireframeLinewidth :: opt Number
  )
newtype InitializeMeshBasicMaterial = InitializeMeshBasicMaterial
  { | (InitializeMeshBasicMaterial' Maybe) }
type MakeBox f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeBox'
  }
type InitializeBox' =
  ( width :: Number
  , height :: Number
  , depth :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , depthSegments :: Int
  )
newtype InitializeBox = InitializeBox { | InitializeBox' }
type MakeTorus f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeTorus'
  }
type InitializeTorus' =
  ( radius :: Number
  , tube :: Number
  , radialSegments :: Number
  , tubularSegments :: Number
  , arc :: Number
  )
newtype InitializeTorus = InitializeTorus { | InitializeTorus' }
type MakeCapsule f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeCapsule'
  }
type InitializeCapsule' =
  ( radius :: Number
  , length :: Number
  , capSegments :: Int
  , radialSegments :: Int
  )
newtype InitializeCapsule = InitializeCapsule { | InitializeCapsule' }
type MakePlane f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializePlane'
  }
type InitializePlane' =
  ( width :: Number
  , height :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  )
newtype InitializePlane = InitializePlane { | InitializePlane' }
type MakePerspectiveCamera f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializePerspectiveCamera'
  }

type InitializePerspectiveCamera' =
  ( fov :: Number
  , aspect :: Number
  , near :: Number
  , far :: Number
  )
newtype InitializePerspectiveCamera = InitializePerspectiveCamera
  { | InitializePerspectiveCamera' }

--
type SetWidth = { id :: String, width :: Number }
type SetHeight = { id :: String, height :: Number }
type SetDepth = { id :: String, depth :: Number }
type SetRadius = { id :: String, radius :: Number }
type SetLength = { id :: String, length :: Number }
type SetWidthSegments = { id :: String, widthSegments :: Int }
type SetHeightSegments = { id :: String, heightSegments :: Int }
type SetDepthSegments = { id :: String, depthSegments :: Int }
type SetCapSegments = { id :: String, capSegments :: Int }
type SetRadialSegments = { id :: String, radialSegments :: Int }
type SetPhiStart = { id :: String, phiStart :: Number }
type SetPhiLength = { id :: String, phiLength :: Number }
type SetThetaStart = { id :: String, thetaStart :: Number }
type SetThetaLength = { id :: String, thetaLength :: Number }
type SetMatrix4 = { id :: String, matrix4 :: Matrix4 }
type SetQuaternion = { id :: String, quaternion :: Quaternion }
type SetRotateX = { id :: String, rotateX :: Number }
type SetRotateY = { id :: String, rotateY :: Number }
type SetRotateZ = { id :: String, rotateZ :: Number }
type SetTranslate = { id :: String, x :: Number, y :: Number, z :: Number }
type SetScale = { id :: String, x :: Number, y :: Number, z :: Number }
type SetLookAt = { id :: String, v :: Vector3 }
type SetCenter = { id :: String }
type GetBoundingBox = { id :: String, box :: Box.Box -> Effect Unit }
type GetBoundingSphere =
  { id :: String, sphere :: Sphere.Sphere -> Effect Unit }
type SetInstancedMeshMatrix4 =
  { id :: String, setMatrix4 :: (Int -> Matrix4 -> Effect Unit) -> Effect Unit }
type SetInstancedMeshColor =
  { id :: String, setColor :: (Int -> Color -> Effect Unit) -> Effect Unit }
type SetSingleInstancedMeshMatrix4 =
  { id :: String, instanceId :: Int, matrix4 :: Matrix4 }
type SetSingleInstancedMeshColor =
  { id :: String, instanceId :: Int, color :: Color }
type SetColor = { id :: String, color :: Color }
type SetRoughness = { id :: String, roughness :: Number }
type SetMetalness = { id :: String, metalness :: Number }
type SetMap = { id :: String, map :: Texture }
type SetLightMap = { id :: String, lightMap :: Texture }
type SetLightMapIntensity = { id :: String, lightMapIntensity :: Number }
type SetAoMap = { id :: String, aoMap :: Texture }
type SetAoMapIntensity = { id :: String, aoMapIntensity :: Number }
type SetEmissive = { id :: String, emissive :: Color }
type SetEmissiveIntensity = { id :: String, emissiveIntensity :: Number }
type SetEmissiveMap = { id :: String, emissiveMap :: Texture }
type SetBumpMap = { id :: String, bumpMap :: Texture }
type SetBumpScale = { id :: String, bumpScale :: Number }
type SetNormalMap = { id :: String, normalMap :: Texture }
type SetNormalMapType = { id :: String, normalMapType :: NormalMapType }
type SetNormalMapType' = { id :: String, normalMapType :: Int }
type SetNormalScale = { id :: String, normalScale :: Vector2 }
type SetDisplacementMap = { id :: String, displacementMap :: Texture }
type SetDisplacementScale = { id :: String, displacementScale :: Number }
type SetDisplacementBias = { id :: String, displacementBias :: Number }
type SetRoughnessMap = { id :: String, roughnessMap :: Texture }
type SetMetalnessMap = { id :: String, metalnessMap :: Texture }
type SetAlphaMap = { id :: String, alphaMap :: Texture }
type SetEnvMap = { id :: String, envMap :: Texture }
type SetEnvMapIntensity = { id :: String, envMapIntensity :: Number }
type SetWireframe = { id :: String, wireframe :: Boolean }
type SetWireframeLinewidth = { id :: String, wireframeLinewidth :: Number }
type SetFlatShading = { id :: String, flatShading :: Boolean }
-- scene
type SetBackgroundCubeTexture = { id :: String, cubeTexture :: CubeTexture }
type SetBackgroundTexture = { id :: String, texture :: Texture }
type SetBackgroundColor = { id :: String, color :: Color }
-- listeners
type SetOnClick = { id :: String, onClick :: MouseEvent -> Effect Unit }
type SetOnMouseDown = { id :: String, onMouseDown :: MouseEvent -> Effect Unit }
type SetOnMouseUp = { id :: String, onMouseUp :: MouseEvent -> Effect Unit }
type SetOnMouseMove = { id :: String, onMouseMove :: MouseEvent -> Effect Unit }
type SetOnTouchStart =
  { id :: String, onTouchStart :: Touch -> Effect Unit }
type SetOnTouchEnd = { id :: String, onTouchEnd :: Touch -> Effect Unit }
type SetOnTouchMove = { id :: String, onTouchMove :: Touch -> Effect Unit }
type SetOnTouchCancel =
  { id :: String, onTouchCancel :: Touch -> Effect Unit }
type RemoveOnClick = { id :: String, onClick :: MouseEvent -> Effect Unit }
type RemoveOnMouseDown =
  { id :: String, onMouseDown :: MouseEvent -> Effect Unit }
type RemoveOnMouseUp = { id :: String, onMouseUp :: MouseEvent -> Effect Unit }
type RemoveOnMouseMove =
  { id :: String, onMouseMove :: MouseEvent -> Effect Unit }
type RemoveOnTouchStart =
  { id :: String, onTouchStart :: Touch -> Effect Unit }
type RemoveOnTouchEnd =
  { id :: String, onTouchEnd :: Touch -> Effect Unit }
type RemoveOnTouchMove =
  { id :: String, onTouchMove :: Touch -> Effect Unit }
type RemoveOnTouchCancel =
  { id :: String, onTouchCancel :: Touch -> Effect Unit }
-- im listeners
type SetIMOnClick =
  { id :: String, instanceId :: Int, onClick :: MouseEvent -> Effect Unit }
type SetIMOnMouseDown =
  { id :: String, instanceId :: Int, onMouseDown :: MouseEvent -> Effect Unit }
type SetIMOnMouseUp =
  { id :: String, instanceId :: Int, onMouseUp :: MouseEvent -> Effect Unit }
type SetIMOnMouseMove =
  { id :: String, instanceId :: Int, onMouseMove :: MouseEvent -> Effect Unit }
type SetIMOnTouchStart =
  { id :: String, instanceId :: Int, onTouchStart :: Touch -> Effect Unit }
type SetIMOnTouchEnd =
  { id :: String, instanceId :: Int, onTouchEnd :: Touch -> Effect Unit }
type SetIMOnTouchMove =
  { id :: String, instanceId :: Int, onTouchMove :: Touch -> Effect Unit }
type SetIMOnTouchCancel =
  { id :: String, instanceId :: Int, onTouchCancel :: Touch -> Effect Unit }
type RemoveIMOnClick =
  { id :: String, instanceId :: Int, onClick :: MouseEvent -> Effect Unit }
type RemoveIMOnMouseDown =
  { id :: String, instanceId :: Int, onMouseDown :: MouseEvent -> Effect Unit }
type RemoveIMOnMouseUp =
  { id :: String, instanceId :: Int, onMouseUp :: MouseEvent -> Effect Unit }
type RemoveIMOnMouseMove =
  { id :: String, instanceId :: Int, onMouseMove :: MouseEvent -> Effect Unit }
type RemoveIMOnTouchStart =
  { id :: String, instanceId :: Int, onTouchStart :: Touch -> Effect Unit }
type RemoveIMOnTouchEnd =
  { id :: String, instanceId :: Int, onTouchEnd :: Touch -> Effect Unit }
type RemoveIMOnTouchMove =
  { id :: String, instanceId :: Int, onTouchMove :: Touch -> Effect Unit }
type RemoveIMOnTouchCancel =
  { id :: String, instanceId :: Int, onTouchCancel :: Touch -> Effect Unit }
-- mesh
type SetRotationFromAxisAngle =
  { id :: String, axis :: Vector3, angle :: Number }
type SetRotationFromEuler = { id :: String, euler :: Euler }
type SetRotationFromMatrix = { id :: String, matrix4 :: Matrix4 }
type SetRotationFromQuaternion = { id :: String, quaternion :: Quaternion }
type SetRotateOnAxis = { id :: String, axis :: Vector3, angle :: Number }
type SetRotateOnWorldAxis = { id :: String, axis :: Vector3, angle :: Number }
type SetTranslateOnAxis = { id :: String, axis :: Vector3, distance :: Number }
type SetPositionX = { id :: String, positionX :: Number }
type SetPositionY = { id :: String, positionY :: Number }
type SetPositionZ = { id :: String, positionZ :: Number }
type SetScaleX = { id :: String, scaleX :: Number }
type SetScaleY = { id :: String, scaleY :: Number }
type SetScaleZ = { id :: String, scaleZ :: Number }
type SetTranslateX = { id :: String, translateX :: Number }
type SetTranslateY = { id :: String, translateY :: Number }
type SetTranslateZ = { id :: String, translateZ :: Number }
-- camera
type WithWorldDirection payload =
  { id :: String, withWorldDirection :: Vector3 -> payload }
-- perspective camera
type SetAspect = { id :: String, aspect :: Number }
type SetFar = { id :: String, far :: Number }
type SetFilmGauge = { id :: String, filmGauge :: Number }
type SetFilmOffset = { id :: String, filmOffset :: Number }
type SetFocus = { id :: String, focus :: Number }
type SetFov = { id :: String, fov :: Number }
type SetNear = { id :: String, near :: Number }
type SetZoom = { id :: String, zoom :: Number }
type SetFocalLength = { id :: String, focalLength :: Number }
type SetViewOffset =
  { id :: String
  , fullWidth :: Number
  , fullHeight :: Number
  , x :: Number
  , y :: Number
  , width :: Number
  , height :: Number
  }
-- point light
type SetIntensity = { id :: String, intensity :: Number }
type SetDistance = { id :: String, distance :: Number }
type SetDecay = { id :: String, decay :: Number }
-- renderer
type SetSize = { id :: String, width :: Number, height :: Number }
--
type ConnectMesh =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type ConnectToScene =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type Disconnect =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type ConnectGeometry =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type ConnectScene =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type ConnectCamera =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type ConnectMaterial =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type DeleteFromCache = { id :: String }

derive instance Newtype (ThreeInterpret payload) _

type BufferGeometry =
  ( matrix4 :: Matrix4
  , quaternion :: Quaternion
  , rotateX :: Number
  , rotateY :: Number
  , rotateZ :: Number
  , translate :: { x :: Number, y :: Number, z :: Number }
  , scale :: { x :: Number, y :: Number, z :: Number }
  , lookAt :: Vector3
  , center :: Unit
  , boundingBox :: Box.Box -> Effect Unit
  , boundingSphere :: Sphere.Sphere -> Effect Unit
  )

type Object3D =
  ( matrix4 :: Matrix4
  , quaternion :: Quaternion
  , rotationFromAxisAngle :: { axis :: Vector3, angle :: Number }
  , rotationFromEuler :: Euler
  , rotationFromMatrix :: Matrix4
  , rotationFromQuaternion :: Quaternion
  , rotateOnAxis :: { axis :: Vector3, angle :: Number }
  , rotateOnWorldAxis :: { axis :: Vector3, angle :: Number }
  , rotateX :: Number
  , rotateY :: Number
  , rotateZ :: Number
  , translateOnAxis :: { axis :: Vector3, distance :: Number }
  , translateX :: Number
  , translateY :: Number
  , translateZ :: Number
  , positionX :: Number
  , positionY :: Number
  , positionZ :: Number
  , scaleX :: Number
  , scaleY :: Number
  , scaleZ :: Number
  , lookAt :: Vector3
  )

bufferGeometry
  :: forall payload
   . String
  -> ThreeInterpret payload
  -> { boundingBox :: (Box -> Effect Unit) -> payload
     , boundingSphere :: (Sphere -> Effect Unit) -> payload
     , center :: Unit -> payload
     , lookAt :: Vector3 -> payload
     , matrix4 :: Matrix4 -> payload
     , quaternion :: Quaternion -> payload
     , rotateX :: Number -> payload
     , rotateY :: Number -> payload
     , rotateZ :: Number -> payload
     , scale ::
         { x :: Number
         , y :: Number
         , z :: Number
         }
         -> payload
     , translate ::
         { x :: Number
         , y :: Number
         , z :: Number
         }
         -> payload
     }
bufferGeometry
  me
  ( ThreeInterpret
      { setMatrix4
      , setQuaternion
      , setRotateX
      , setRotateY
      , setRotateZ
      , setTranslate
      , setScale
      , setLookAt
      , setCenter
      , getBoundingBox
      , getBoundingSphere
      }
  ) =
  { matrix4: setMatrix4 <<< { id: me, matrix4: _ }
  , quaternion: setQuaternion <<< { id: me, quaternion: _ }
  , rotateX: setRotateX <<< { id: me, rotateX: _ }
  , rotateY: setRotateY <<< { id: me, rotateY: _ }
  , rotateZ: setRotateZ <<< { id: me, rotateZ: _ }
  , translate: setTranslate <<< union { id: me }
  , scale: setScale <<< union { id: me }
  , lookAt: setLookAt <<< { id: me, v: _ }
  , center: \_ -> setCenter { id: me }
  , boundingBox: getBoundingBox <<< { id: me, box: _ }
  , boundingSphere: getBoundingSphere <<< { id: me, sphere: _ }
  }

object3D
  :: forall payload
   . String
  -> ThreeInterpret payload
  -> { lookAt :: Vector3 -> payload
     , matrix4 :: Matrix4 -> payload
     , positionX :: Number -> payload
     , positionY :: Number -> payload
     , positionZ :: Number -> payload
     , quaternion :: Quaternion -> payload
     , rotateOnAxis ::
         { angle :: Number
         , axis :: Vector3
         }
         -> payload
     , rotateOnWorldAxis ::
         { angle :: Number
         , axis :: Vector3
         }
         -> payload
     , rotateX :: Number -> payload
     , rotateY :: Number -> payload
     , rotateZ :: Number -> payload
     , rotationFromAxisAngle ::
         { angle :: Number
         , axis :: Vector3
         }
         -> payload
     , rotationFromEuler :: Euler -> payload
     , rotationFromMatrix :: Matrix4 -> payload
     , rotationFromQuaternion :: Quaternion -> payload
     , scaleX :: Number -> payload
     , scaleY :: Number -> payload
     , scaleZ :: Number -> payload
     , translateOnAxis ::
         { axis :: Vector3
         , distance :: Number
         }
         -> payload
     , translateX :: Number -> payload
     , translateY :: Number -> payload
     , translateZ :: Number -> payload
     }
object3D
  me
  ( ThreeInterpret
      { setMatrix4
      , setQuaternion
      , setRotationFromAxisAngle
      , setRotationFromEuler
      , setRotationFromMatrix
      , setRotationFromQuaternion
      , setRotateOnAxis
      , setRotateOnWorldAxis
      , setRotateX
      , setRotateY
      , setRotateZ
      , setTranslateOnAxis
      , setTranslateX
      , setTranslateY
      , setTranslateZ
      , setPositionX
      , setPositionY
      , setPositionZ
      , setScaleX
      , setScaleY
      , setScaleZ
      , setLookAt
      }
  ) =
  { matrix4: setMatrix4 <<< { id: me, matrix4: _ }
  , quaternion: setQuaternion <<< { id: me, quaternion: _ }
  , rotationFromAxisAngle: \{ axis, angle } ->
      setRotationFromAxisAngle { id: me, axis, angle }
  , rotationFromEuler: setRotationFromEuler <<<
      { id: me, euler: _ }
  , rotationFromMatrix: setRotationFromMatrix <<<
      { id: me, matrix4: _ }
  , rotationFromQuaternion: setRotationFromQuaternion <<<
      { id: me, quaternion: _ }
  , rotateOnAxis: \{ axis, angle } -> setRotateOnAxis
      { id: me, axis, angle }
  , rotateOnWorldAxis: \{ axis, angle } -> setRotateOnWorldAxis
      { id: me, axis, angle }
  , rotateX: setRotateX <<< { id: me, rotateX: _ }
  , rotateY: setRotateY <<< { id: me, rotateY: _ }
  , rotateZ: setRotateZ <<< { id: me, rotateZ: _ }
  , translateOnAxis: \{ axis, distance } -> setTranslateOnAxis
      { id: me, axis, distance }
  , translateX: setTranslateX <<< { id: me, translateX: _ }
  , translateY: setTranslateY <<< { id: me, translateY: _ }
  , translateZ: setTranslateZ <<< { id: me, translateZ: _ }
  , positionX: setPositionX <<< { id: me, positionX: _ }
  , positionY: setPositionY <<< { id: me, positionY: _ }
  , positionZ: setPositionZ <<< { id: me, positionZ: _ }
  , scaleX: setScaleX <<< { id: me, scaleX: _ }
  , scaleY: setScaleY <<< { id: me, scaleY: _ }
  , scaleZ: setScaleZ <<< { id: me, scaleZ: _ }
  , lookAt: setLookAt <<< { id: me, v: _ }
  }

newtype ThreeInterpret payload = ThreeInterpret
  { ids :: Effect String
  , webGLRender :: WebGLRender -> payload
  , css2DRender :: CSS2DRender -> payload
  --
  , makeWebGLRenderer :: MakeWebGLRenderer -> payload
  , makeCSS2DRenderer :: MakeCSS2DRenderer -> payload
  , makeGroup :: MakeGroup Maybe Scope -> payload
  , makeScene :: MakeScene Maybe Scope -> payload
  , makeMesh :: MakeMesh Maybe Scope -> payload
  , makeInstancedMesh :: MakeInstancedMesh Maybe Scope -> payload
  , makeSphere :: MakeSphere Maybe Scope -> payload
  , makeBox :: MakeBox Maybe Scope -> payload
  , makeCapsule :: MakeCapsule Maybe Scope -> payload
  , makeTorus :: MakeTorus Maybe Scope -> payload
  , makePlane :: MakePlane Maybe Scope -> payload
  , makeDirectionalLight :: MakeDirectionalLight Maybe Scope -> payload
  , makeAmbientLight :: MakeAmbientLight Maybe Scope -> payload
  , makePointLight :: MakePointLight Maybe Scope -> payload
  , makeMeshBasicMaterial :: MakeMeshBasicMaterial Maybe Scope -> payload
  , makeMeshStandardMaterial :: MakeMeshStandardMaterial Maybe Scope -> payload
  , makePerspectiveCamera :: MakePerspectiveCamera Maybe Scope -> payload
  , makeCSS2DObject :: MakeCSS2DObject Maybe Scope -> payload
  -- scene
  , setBackgroundCubeTexture :: SetBackgroundCubeTexture -> payload
  , setBackgroundTexture :: SetBackgroundTexture -> payload
  , setBackgroundColor :: SetBackgroundColor -> payload
  -- (faux) Listeners
  , setOnClick :: SetOnClick -> payload
  , setOnMouseDown :: SetOnMouseDown -> payload
  , setOnMouseUp :: SetOnMouseUp -> payload
  , setOnMouseMove :: SetOnMouseMove -> payload
  , setOnTouchStart :: SetOnTouchStart -> payload
  , setOnTouchEnd :: SetOnTouchEnd -> payload
  , setOnTouchMove :: SetOnTouchMove -> payload
  , setOnTouchCancel :: SetOnTouchCancel -> payload
  , removeOnClick :: RemoveOnClick -> payload
  , removeOnMouseDown :: RemoveOnMouseDown -> payload
  , removeOnMouseUp :: RemoveOnMouseUp -> payload
  , removeOnMouseMove :: RemoveOnMouseMove -> payload
  , removeOnTouchStart :: RemoveOnTouchStart -> payload
  , removeOnTouchEnd :: RemoveOnTouchEnd -> payload
  , removeOnTouchMove :: RemoveOnTouchMove -> payload
  , removeOnTouchCancel :: RemoveOnTouchCancel -> payload
  -- (faux) IM Listeners
  , setIMOnClick :: SetIMOnClick -> payload
  , setIMOnMouseDown :: SetIMOnMouseDown -> payload
  , setIMOnMouseUp :: SetIMOnMouseUp -> payload
  , setIMOnMouseMove :: SetIMOnMouseMove -> payload
  , setIMOnTouchStart :: SetIMOnTouchStart -> payload
  , setIMOnTouchEnd :: SetIMOnTouchEnd -> payload
  , setIMOnTouchMove :: SetIMOnTouchMove -> payload
  , setIMOnTouchCancel :: SetIMOnTouchCancel -> payload
  , removeIMOnClick :: RemoveIMOnClick -> payload
  , removeIMOnMouseDown :: RemoveIMOnMouseDown -> payload
  , removeIMOnMouseUp :: RemoveIMOnMouseUp -> payload
  , removeIMOnMouseMove :: RemoveIMOnMouseMove -> payload
  , removeIMOnTouchStart :: RemoveIMOnTouchStart -> payload
  , removeIMOnTouchEnd :: RemoveIMOnTouchEnd -> payload
  , removeIMOnTouchMove :: RemoveIMOnTouchMove -> payload
  , removeIMOnTouchCancel :: RemoveIMOnTouchCancel -> payload
  -- CapsuleGeometry
  , setCapSegments :: SetCapSegments -> payload
  , setRadialSegments :: SetRadialSegments -> payload
  -- BoxGeometry
  , setWidth :: SetWidth -> payload
  , setHeight :: SetHeight -> payload
  , setDepth :: SetDepth -> payload
  , setLength :: SetLength -> payload
  -- SphereGeometry
  , setRadius :: SetRadius -> payload
  , setWidthSegments :: SetWidthSegments -> payload
  , setHeightSegments :: SetHeightSegments -> payload
  , setDepthSegments :: SetDepthSegments -> payload
  , setPhiStart :: SetPhiStart -> payload
  , setPhiLength :: SetPhiLength -> payload
  , setThetaStart :: SetThetaStart -> payload
  , setThetaLength :: SetThetaLength -> payload
  -- BufferGeometry
  , setMatrix4 :: SetMatrix4 -> payload
  , setQuaternion :: SetQuaternion -> payload
  , setRotateX :: SetRotateX -> payload
  , setRotateY :: SetRotateY -> payload
  , setRotateZ :: SetRotateZ -> payload
  , setTranslate :: SetTranslate -> payload
  , setScale :: SetScale -> payload
  , setLookAt :: SetLookAt -> payload
  , setCenter :: SetCenter -> payload
  , getBoundingBox :: GetBoundingBox -> payload
  , getBoundingSphere :: GetBoundingSphere -> payload
  -- InstancedMesh
  , setInstancedMeshMatrix4 :: SetInstancedMeshMatrix4 -> payload
  , setInstancedMeshColor :: SetInstancedMeshColor -> payload
  , setSingleInstancedMeshMatrix4 :: SetSingleInstancedMeshMatrix4 -> payload
  , setSingleInstancedMeshColor :: SetSingleInstancedMeshColor -> payload
  -- MeshStandardMaterial
  , setColor :: SetColor -> payload
  , setRoughness :: SetRoughness -> payload
  , setMetalness :: SetMetalness -> payload
  , setMap :: SetMap -> payload
  , setLightMap :: SetLightMap -> payload
  , setLightMapIntensity :: SetLightMapIntensity -> payload
  , setAoMap :: SetAoMap -> payload
  , setAoMapIntensity :: SetAoMapIntensity -> payload
  , setEmissive :: SetEmissive -> payload
  , setEmissiveIntensity :: SetEmissiveIntensity -> payload
  , setEmissiveMap :: SetEmissiveMap -> payload
  , setBumpMap :: SetBumpMap -> payload
  , setBumpScale :: SetBumpScale -> payload
  , setNormalMap :: SetNormalMap -> payload
  , setNormalMapType :: SetNormalMapType -> payload
  , setNormalScale :: SetNormalScale -> payload
  , setDisplacementMap :: SetDisplacementMap -> payload
  , setDisplacementScale :: SetDisplacementScale -> payload
  , setDisplacementBias :: SetDisplacementBias -> payload
  , setRoughnessMap :: SetRoughnessMap -> payload
  , setMetalnessMap :: SetMetalnessMap -> payload
  , setAlphaMap :: SetAlphaMap -> payload
  , setEnvMap :: SetEnvMap -> payload
  , setEnvMapIntensity :: SetEnvMapIntensity -> payload
  , setWireframe :: SetWireframe -> payload
  , setWireframeLinewidth :: SetWireframeLinewidth -> payload
  , setFlatShading :: SetFlatShading -> payload
  -- mesh
  , setRotationFromAxisAngle :: SetRotationFromAxisAngle -> payload
  , setRotationFromEuler :: SetRotationFromEuler -> payload
  , setRotationFromMatrix :: SetRotationFromMatrix -> payload
  , setRotationFromQuaternion :: SetRotationFromQuaternion -> payload
  , setRotateOnAxis :: SetRotateOnAxis -> payload
  , setRotateOnWorldAxis :: SetRotateOnWorldAxis -> payload
  , setTranslateOnAxis :: SetTranslateOnAxis -> payload
  , setTranslateX :: SetTranslateX -> payload
  , setTranslateY :: SetTranslateY -> payload
  , setTranslateZ :: SetTranslateZ -> payload
  , setPositionX :: SetPositionX -> payload
  , setPositionY :: SetPositionY -> payload
  , setPositionZ :: SetPositionZ -> payload
  , setScaleX :: SetScaleX -> payload
  , setScaleY :: SetScaleY -> payload
  , setScaleZ :: SetScaleZ -> payload
  -- camera
  , withWorldDirection :: WithWorldDirection payload -> payload
  -- orbit controls
  -- perspective camera
  , setAspect :: SetAspect -> payload
  , setFar :: SetFar -> payload
  , setFilmGauge :: SetFilmGauge -> payload
  , setFilmOffset :: SetFilmOffset -> payload
  , setFocus :: SetFocus -> payload
  , setFov :: SetFov -> payload
  , setNear :: SetNear -> payload
  , setZoom :: SetZoom -> payload
  , setFocalLength :: SetFocalLength -> payload
  , setViewOffset :: SetViewOffset -> payload
  -- point light
  , setIntensity :: SetIntensity -> payload
  , setDistance :: SetDistance -> payload
  , setDecay :: SetDecay -> payload
  -- webgl
  , setSize :: SetSize -> payload
  -- connectors
  , connectMesh :: ConnectMesh -> payload
  , connectScene :: ConnectScene -> payload
  , connectCamera :: ConnectCamera -> payload
  , connectGeometry :: ConnectGeometry -> payload
  , connectMaterial :: ConnectMaterial -> payload
  , connectToScene :: ConnectToScene -> payload
  , disconnect :: Disconnect -> payload
  --
  , deleteFromCache :: DeleteFromCache -> payload
  }