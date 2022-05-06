module Rito.Core where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import FRP.Event (Event, makeEvent)
import Rito.Box as Box
import Rito.Color (Color)
import Rito.Euler (Euler)
import Rito.Matrix4 (Matrix4)
import Rito.NormalMapTypes (NormalMapType)
import Rito.Quaternion (Quaternion)
import Rito.Sphere as Sphere
import Rito.Texture (Texture)
import Rito.Undefinable (Undefinable)
import Rito.Vector2 (Vector2)
import Rito.Vector3 (Vector3)

type Ctor payload =
  { parent :: String
  , scope :: String
  , raiseId :: String -> Effect Unit
  }
  -> ThreeInterpret payload
  -> Event payload

class Connectable :: forall k. (k -> Type -> Type) -> Constraint
class Connectable ctor where
  toCtor :: forall lock payload. ctor lock payload -> Ctor payload
  fromCtor :: forall lock payload. Ctor payload -> ctor lock payload
  connect :: forall lock payload. String -> ctor lock payload

newtype Camera (lock :: Type) payload = Camera (Ctor payload)
data Renderer :: forall k. Type -> k -> Type
data Renderer (lock :: Type) payload
newtype Geometry (lock :: Type) payload = Geometry (Ctor payload)
newtype Material (lock :: Type) payload = Material (Ctor payload)
newtype Mesh (lock :: Type) payload = Mesh (Ctor payload)
newtype Scene (lock :: Type) payload = Scene (Ctor payload)

instance Connectable Mesh where
  toCtor (Mesh c) = c
  fromCtor c = Mesh c
  connect me = Mesh \parent (ThreeInterpret { connectMesh }) -> makeEvent \k -> do
    parent.raiseId me
    k $ connectMesh { id: me, parent: parent.parent, scope: parent.scope }
    pure (pure unit)

type MakeScene =
  { id :: String
  , scope :: String
  , parent :: String
  }
type MakeMesh =
  { id :: String
  , scope :: String
  , parent :: String
  }
type MakeSphere =
  { id :: String
  , scope :: String
  , parent :: String
  | InitializeSphere'
  }
type InitializeSphere' =
  ( radius :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , phiStart :: Number
  , phiLength :: Number
  , thetaStart :: Number
  , thetaLength :: Number
  )
newtype InitializeSphere = InitializeSphere { | InitializeSphere' }
type MakeMeshStandardMaterial =
  { id :: String
  , scope :: String
  , parent :: String
  | (InitializeMeshStandardMaterial' Maybe NormalMapType)
  }
type MakeMeshStandardMaterial' =
  { id :: String
  , scope :: String
  , parent :: String
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
type MakeBox =
  { id :: String
  , scope :: String
  , parent :: String
  | InitializeBox'
  }
type InitializeBox' =
  ( width :: Number
  , height :: Number
  , depth :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , depthSegments :: Number
  )
newtype InitializeBox = InitializeBox { | InitializeBox' }
type MakeTorus =
  { id :: String
  , scope :: String
  , parent :: String
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
type MakePlane =
  { id :: String
  , scope :: String
  , parent :: String
  | InitializePlane'
  }
type InitializePlane' =
  ( width :: Number
  , height :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  )
newtype InitializePlane = InitializePlane { | InitializePlane' }
--
type SetRadius = { id :: String, radius :: Number }
type SetWidthSegments = { id :: String, widthSegments :: Int }
type SetHeightSegments = { id :: String, heightSegments :: Int }
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
-- mesh
type SetRotationFromAxisAngle =
  { id :: String, axis :: Vector3, angle :: Number }
type SetRotationFromEuler = { id :: String, euler :: Euler }
type SetRotationFromMatrix = { id :: String, matrix4 :: Matrix4 }
type SetRotationFromQuaternion = { id :: String, quaternion :: Quaternion }
type SetRotateOnAxis = { id :: String, axis :: Vector3, angle :: Number }
type SetRotateOnWorldAxis = { id :: String, axis :: Vector3, angle :: Number }
type SetTranslateOnAxis = { id :: String, axis :: Vector3, distance :: Number }
type SetTranslateX = { id :: String, translateX :: Number }
type SetTranslateY = { id :: String, translateY :: Number }
type SetTranslateZ = { id :: String, translateZ :: Number }

type MakeNoop =
  { id :: String
  , scope :: String
  , parent :: String
  }
type ConnectMesh =
  { id :: String
  , parent :: String
  , scope :: String
  }
type Disconnect =
  { id :: String
  , parent :: String
  , scope :: String
  }
type ConnectGeometry =
  { id :: String
  , parent :: String
  , scope :: String
  }
type ConnectMaterial =
  { id :: String
  , parent :: String
  , scope :: String
  }
type DeleteFromCache = { id :: String }

newtype ThreeInterpret payload = ThreeInterpret
  { ids :: Effect String
  --
  , makeScene :: MakeScene -> payload
  , makeMesh :: MakeMesh -> payload
  , makeSphere :: MakeSphere -> payload
  , makeBox :: MakeBox -> payload
  , makeTorus :: MakeTorus -> payload
  , makePlane :: MakePlane -> payload
  , makeMeshStandardMaterial :: MakeMeshStandardMaterial -> payload
  , makeNoop :: MakeNoop -> payload
  -- SphereGeometry
  , setRadius :: SetRadius -> payload
  , setWidthSegments :: SetWidthSegments -> payload
  , setHeightSegments :: SetHeightSegments -> payload
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
  -- connectors
  , connectMesh :: ConnectMesh -> payload
  , connectGeometry :: ConnectGeometry -> payload
  , connectMaterial :: ConnectMaterial -> payload
  , disconnect :: Disconnect -> payload
  --
  , deleteFromCache :: DeleteFromCache -> payload
  }