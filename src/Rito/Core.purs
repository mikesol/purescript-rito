module Rito.Core where

import Prelude

import Bolson.Core (Entity, Scope)
import Bolson.Core as Bolson
import Control.Monad.ST (ST)
import Control.Monad.ST as ST
import Control.Monad.ST.Global as Region
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Deku.Core (ANut)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2)
import FRP.Event (Event)
import Foreign (Foreign)
import Foreign.Object as Object
import Record (union)
import Rito.BlendDst (BlendDst)
import Rito.BlendEquation (BlendEquation)
import Rito.BlendSrc (BlendSrc)
import Rito.Blending (Blending)
import Rito.Box3 as Box3
import Rito.BufferAttribute (BufferAttribute)
import Rito.Color (Color)
import Rito.CombineOperation (CombineOperation)
import Rito.CubeTexture (CubeTexture)
import Rito.DepthMode (DepthMode)
import Rito.Euler (Euler)
import Rito.InstancedBufferAttribute (InstancedBufferAttribute)
import Rito.Matrix4 (Matrix4)
import Rito.NormalMapType (NormalMapType)
import Rito.Precision (Precision)
import Rito.Quaternion (Quaternion)
import Rito.Renderers.WebGLRenderingPowerPreference as WPP
import Rito.Renderers.WebGLRenderingPrecision as WRP
import Rito.Side (Side)
import Rito.Sphere as Sphere
import Rito.THREE as THREE
import Rito.Texture (Texture)
import Rito.Undefinable (Undefinable)
import Rito.Vector2 (Vector2)
import Rito.Vector3 (Vector3)
import Rito.WireframeLinecap (WireframeLinecap)
import Rito.WireframeLinejoin (WireframeLinejoin)
import Safe.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as Web.DOM
import Web.HTML (HTMLCanvasElement)
import Web.TouchEvent (Touch, TouchEvent)
import Web.UIEvent.MouseEvent (MouseEvent)

plain :: forall logic obj. obj -> Entity logic obj
plain = Bolson.Element'

class Sceneable ctor where
  toScene
    :: forall payload
     . Entity Void (ctor payload)
    -> Entity Void (Sceneful payload)

class Groupable ctor where
  toGroup
    :: forall payload
     . Entity Void (ctor payload)
    -> Entity Void (Groupful payload)

cameraToGroup
  :: forall payload
   . Camera payload
  -> Entity Void (Groupful payload)
cameraToGroup c = Bolson.Element' $
  (unsafeCoerce :: Camera payload -> Groupful payload) c

type Ctor payload =
  { parent :: Maybe String
  , scope :: Scope
  , raiseId :: String -> ST Region.Global Unit
  }
  -> SimpleCtor payload

type SimpleCtor payload =
  ThreeInterpret payload
  -> Event payload

newtype WebGLRenderer payload = WebGLRenderer (Ctor payload)

webGLRendererToRenderer
  :: forall payload
   . WebGLRenderer payload
  -> Renderer payload
webGLRendererToRenderer = coerce

newtype EffectComposer payload = EffectComposer (Ctor payload)

effectComposerToRenderer
  :: forall payload
   . EffectComposer payload
  -> Renderer payload
effectComposerToRenderer = coerce

newtype Renderer payload = Renderer (Ctor payload)
type ARenderer payload = Entity Void (Renderer payload)
newtype Pass payload = Pass (Ctor payload)
type APass payload = Entity Void (Pass payload)
newtype Light payload = Light (Ctor payload)
type ALight payload = Entity Void (Light payload)
newtype CSS2DObject payload = CSS2DObject (Ctor payload)
type ACSS2DObject payload = Entity Void (CSS2DObject payload)
newtype CSS3DObject payload = CSS3DObject (Ctor payload)
type ACSS3DObject payload = Entity Void (CSS3DObject payload)
newtype Geometry payload = Geometry (Ctor payload)
newtype Material payload = Material (Ctor payload)
newtype Mesh payload = Mesh (Ctor payload)
newtype Instance payload = Instance (SimpleCtor payload)
type AMesh payload = Entity Void (Mesh payload)
newtype Points payload = Points (Ctor payload)
type APoints payload = Entity Void (Points payload)
newtype Group payload = Group (Ctor payload)
type AGroup payload = Entity Void (Group payload)
newtype Scene payload = Scene (Ctor payload)
-- type AScene payload = Entity Void (Scene payload)
newtype Sceneful payload = Sceneful (Ctor payload)
type ASceneful payload = Entity Void (Sceneful payload)
newtype Groupful payload = Groupful (Ctor payload)
type AGroupful payload = Entity Void (Groupful payload)
newtype Camera payload = Camera (Ctor payload)
-- type ACamera payload = Entity Void (Camera payload)

instance Sceneable Light where
  toScene = unsafeCoerce

instance Sceneable CSS2DObject where
  toScene = unsafeCoerce

instance Sceneable CSS3DObject where
  toScene = unsafeCoerce

instance Sceneable Mesh where
  toScene = unsafeCoerce

instance Sceneable Camera where
  toScene = unsafeCoerce

instance Sceneable Points where
  toScene = unsafeCoerce

instance Sceneable Group where
  toScene = unsafeCoerce

instance Groupable Light where
  toGroup = unsafeCoerce

instance Groupable Points where
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

instance Groupable CSS3DObject where
  toGroup = unsafeCoerce

type WebGLRender = { id :: String, scene :: String, camera :: String }
type MakeRaycaster =
  { id :: String
  , camera :: String
  , canvas :: HTMLCanvasElement
  , raycaster :: THREE.TRaycaster
  }
type MakeEffectComposer =
  { id :: String
  , effectComposer :: THREE.TEffectComposer
  , webGLRenderer :: String
  }
newtype InitializeEffectComposer = InitializeEffectComposer
  { effectComposer :: THREE.TEffectComposer
  | InitializeWebGLRenderer' WRP.WebGLRenderingPrecision
      WPP.WebGLRenderingPowerPreference
  }
type MakeRenderPass f =
  { id :: String
  , parent :: f String
  , renderPass :: THREE.TRenderPass
  , camera :: String
  , scene :: String
  }
type MakeGlitchPass f =
  { id :: String
  , parent :: f String
  | InitializeGlitchPass'
  }
type InitializeGlitchPass' =
  ( glitchPass :: THREE.TGlitchPass
  , dtSize :: Int
  )
newtype InitializeGlitchPass = InitializeGlitchPass
  { | InitializeGlitchPass'
  }
type MakeEffectComposerPass f =
  { id :: String
  , parent :: f String
  , effectComposer :: String
  | InitializeEffectComposerPass'
  }
type InitializeEffectComposerPass' =
  ( effectComposerPass :: THREE.TEffectComposerPass
  )
newtype InitializeEffectComposerPass = InitializeEffectComposerPass
  { | InitializeEffectComposerPass'
  }
type MakeBloomPass f =
  { id :: String
  , parent :: f String
  | InitializeBloomPass'
  }
type InitializeBloomPass' =
  ( bloomPass :: THREE.TBloomPass
  , strength :: Number
  , kernelSize :: Int
  , sigma :: Number
  , resolution :: Int
  )
newtype InitializeBloomPass = InitializeBloomPass
  { | InitializeBloomPass'
  }
type MakeUnrealBloomPass f =
  { id :: String
  , parent :: f String
  | InitializeUnrealBloomPass'
  }
type InitializeUnrealBloomPass' =
  ( unrealBloomPass :: THREE.TUnrealBloomPass
  , resolution :: Vector2
  , strength :: Number
  , radius :: Number
  , threshold :: Number
  )
newtype InitializeUnrealBloomPass = InitializeUnrealBloomPass
  { | InitializeUnrealBloomPass'
  }
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
  , webGLRenderer :: THREE.TWebGLRenderer
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
type EffectComposerRender = { id :: String }
type MakeCSS2DRenderer =
  { id :: String
  , camera :: String
  , css2DRenderer :: THREE.TCSS2DRenderer
  , canvas :: HTMLCanvasElement
  , element :: Web.DOM.Element
  }

type CSS3DRender = { id :: String, scene :: String, camera :: String }

type MakeCSS3DRenderer =
  { id :: String
  , camera :: String
  , css3DRenderer :: THREE.TCSS3DRenderer
  , canvas :: HTMLCanvasElement
  , element :: Web.DOM.Element
  }

type MakeScene f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , scene :: THREE.TScene
  , fog :: f { ctor :: THREE.TFogExp2, color :: Color, density :: Number }
  }

data FogInfo = FogExp2Info
  { ctor :: THREE.TFogExp2, color :: Color, density :: Number }

newtype InitializeScene = InitializeScene
  { scene :: THREE.TScene
  , fog :: Maybe FogInfo
  }
type MakeGroup f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , group :: THREE.TGroup
  }

data RawGroup

type MakeGLTFGroup f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , group :: RawGroup
  }
data RawCamera
type MakeGLTFCamera f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , camera :: RawCamera
  }
type MakeMesh f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , mesh :: THREE.TMesh
  }
type MakePoints f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , points :: THREE.TPoints
  }
type MakeInstancedMesh f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , geometry :: String
  , material :: String
  , count :: Int
  , instancedMesh :: THREE.TInstancedMesh
  , matrix4 :: THREE.TMatrix4
  , mesh :: THREE.TMesh
  }
type MakeSphere f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeSphere'
  }
newtype InitializeSphere = InitializeSphere { | InitializeSphere' }
type InitializeSphere' =
  ( sphere :: THREE.TSphereGeometry
  , radius :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , phiStart :: Number
  , phiLength :: Number
  , thetaStart :: Number
  , thetaLength :: Number
  | Buffy
  )
-- css
type MakeCSS2DObject f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeCSS2DObject' Web.DOM.Element
  }
type InitializeCSS2DObject' (nut :: Type) =
  ( css2DObject :: THREE.TCSS2DObject
  , nut :: nut
  )
newtype InitializeCSS2DObject = InitializeCSS2DObject
  { | InitializeCSS2DObject' ANut }
-- css3d
type MakeCSS3DObject f s =
  { id :: String
  , scope :: s
  , parent :: f String

  | InitializeCSS3DObject' Web.DOM.Element
  }
type InitializeCSS3DObject' (nut :: Type) =
  ( css3DObject :: THREE.TCSS3DObject
  , nut :: nut
  )
newtype InitializeCSS3DObject = InitializeCSS3DObject
  { | InitializeCSS3DObject' ANut }
-- light
type MakePointLight f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializePointLight'
  }
type InitializePointLight' =
  ( pointLight :: THREE.TPointLight
  , color :: Color
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
  ( ambientLight :: THREE.TAmbientLight
  , color :: Color
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
  ( directionalLight :: THREE.TDirectionalLight
  , color :: Color
  , intensity :: Number
  )
newtype InitializeDirectionalLight = InitializeDirectionalLight
  { | InitializeDirectionalLight' }
---- material
type MakeRawShaderMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeRawShaderMaterial' Foreign () }
  , materialParameters ::
      { | AllMaterials Maybe BlendDst BlendEquation Blending
          BlendSrc
          DepthMode
          Precision
          Side
      }
  }
type MakeRawShaderMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeRawShaderMaterial' Foreign () }
  , materialParameters ::
      { | AllMaterials Undefinable Int Int Int Int Int String Int
      }
  }
type InitializeRawShaderMaterial'
  u
  r =
  ( rawShaderMaterial :: THREE.TRawShaderMaterial
  , uniforms :: u
  , fragmentShader :: String
  , vertexShader :: String
  | r
  )
newtype InitializeRawShaderMaterial u = InitializeRawShaderMaterial
  { | InitializeRawShaderMaterial' u
      ( AllMaterials Maybe BlendDst BlendEquation Blending
          BlendSrc
          DepthMode
          Precision
          Side
      )
  }
type MakeShaderMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeShaderMaterial' Foreign () }
  , materialParameters ::
      { | AllMaterials Maybe BlendDst BlendEquation Blending
          BlendSrc
          DepthMode
          Precision
          Side
      }
  }
type MakeShaderMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeShaderMaterial' Foreign () }
  , materialParameters ::
      { | AllMaterials Undefinable Int Int Int Int Int String Int
      }
  }
type InitializeShaderMaterial'
  u
  r =
  ( shaderMaterial :: THREE.TShaderMaterial
  , uniforms :: u
  , fragmentShader :: String
  , vertexShader :: String
  | r
  )
newtype InitializeShaderMaterial u = InitializeShaderMaterial
  { | InitializeShaderMaterial' u
      ( AllMaterials Maybe BlendDst BlendEquation Blending
          BlendSrc
          DepthMode
          Precision
          Side
      )
  }
--
type MakeMeshStandardMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeMeshStandardMaterial' Maybe NormalMapType () }
  , materialParameters ::
      { | AllMaterials Maybe BlendDst BlendEquation
          Blending
          BlendSrc
          DepthMode
          Precision
          Side
      }
  }
type MakeMeshStandardMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeMeshStandardMaterial' Undefinable Int () }
  , materialParameters ::
      { | AllMaterials Undefinable Int Int Int Int Int String Int }
  }
type InitializeMeshStandardMaterial'
  (opt :: Type -> Type)
  normalMapType
  r =
  ( meshStandardMaterial :: THREE.TMeshStandardMaterial
  , color :: opt Color
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
  | r
  )
newtype InitializeMeshStandardMaterial = InitializeMeshStandardMaterial
  { | InitializeMeshStandardMaterial' Maybe NormalMapType
      ( AllMaterials Maybe BlendDst BlendEquation
          Blending
          BlendSrc
          DepthMode
          Precision
          Side
      )
  }
-- lambert
type MakeMeshLambertMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeMeshLambertMaterial' Maybe NormalMapType () }
  , materialParameters ::
      { | AllMaterials Maybe BlendDst BlendEquation
          Blending
          BlendSrc
          DepthMode
          Precision
          Side
      }
  }
type MakeMeshLambertMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeMeshLambertMaterial' Undefinable Int () }
  , materialParameters ::
      { | AllMaterials Undefinable Int Int Int Int Int String Int }
  }
type InitializeMeshLambertMaterial'
  (opt :: Type -> Type)
  normalMapType
  r =
  ( meshLambertMaterial :: THREE.TMeshLambertMaterial
  , color :: opt Color
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
  | r
  )
newtype InitializeMeshLambertMaterial = InitializeMeshLambertMaterial
  { | InitializeMeshLambertMaterial' Maybe NormalMapType
      ( AllMaterials Maybe BlendDst BlendEquation
          Blending
          BlendSrc
          DepthMode
          Precision
          Side
      )
  }
-- phong
type MakeMeshPhongMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters ::
      { | InitializeMeshPhongMaterial' Maybe CombineOperation NormalMapType
          WireframeLinecap
          WireframeLinejoin
          ()
      }
  , materialParameters ::
      { | AllMaterials Maybe BlendDst
          BlendEquation
          Blending
          BlendSrc
          DepthMode
          Precision
          Side
      }
  }
type MakeMeshPhongMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters ::
      { | InitializeMeshPhongMaterial' Undefinable Int Int String String () }
  , materialParameters ::
      { | AllMaterials Undefinable Int Int Int
          Int
          Int
          String
          Int
      }
  }
type InitializeMeshPhongMaterial'
  (opt :: Type -> Type)
  combineOperation
  normalMapType
  wireframeLinecap
  wireframeLinejoin
  r =
  ( meshPhongMaterial :: THREE.TMeshPhongMaterial
  , alphaMap :: opt Texture
  , aoMap :: opt Texture
  , aoMapIntensity :: opt Number
  , bumpMap :: opt Texture
  , bumpScale :: opt Number
  , color :: opt Color
  , combine :: opt combineOperation
  , displacementMap :: opt Texture
  , displacementScale :: opt Number
  , displacementBias :: opt Number
  , emissive :: opt Color
  , emissiveMap :: opt Texture
  , emissiveIntensity :: opt Number
  , envMap :: opt Texture
  , flatShading :: opt Boolean
  , fog :: opt Boolean
  , lightMap :: opt Texture
  , lightMapIntensity :: opt Number
  , map :: opt Texture
  , normalMap :: opt Texture
  , normalMapType :: opt normalMapType
  , normalScale :: opt Vector2
  , reflectivity :: opt Number
  , refractionRatio :: opt Number
  , shininess :: opt Number
  , specular :: opt Color
  , specularMap :: opt Texture
  , wireframe :: opt Boolean
  , wireframeLinecap :: opt wireframeLinecap
  , wireframeLinejoin :: opt wireframeLinejoin
  , wireframeLinewidth :: opt Number
  | r
  )
newtype InitializeMeshPhongMaterial = InitializeMeshPhongMaterial
  { | InitializeMeshPhongMaterial' Maybe CombineOperation NormalMapType
      WireframeLinecap
      WireframeLinejoin
      ( AllMaterials Maybe BlendDst BlendEquation
          Blending
          BlendSrc
          DepthMode
          Precision
          Side
      )
  }
-- ALL MATERIALS
type AllMaterials'
  (a :: Type -> Type)
  blendDst
  blendEquation
  blending
  blendSrc
  depthMode
  precision
  side
  r =
  ( alphaTest :: a Number
  , alphaToCoverage :: a Number
  , blendDst :: a blendDst
  , blendDstAlpha :: a blendDst
  , blendEquation :: a blendEquation
  , blendEquationAlpha :: a blendEquation
  , blending :: a blending
  , blendSrc :: a blendSrc
  , blendSrcAlpha :: a blendSrc
  , clipIntersection :: a Boolean
  , clipShadows :: a Boolean
  , colorWrite :: a Boolean
  , depthFunc :: a depthMode
  , depthTest :: a Boolean
  , depthWrite :: a Boolean
  , opacity :: a Number
  , polygonOffset :: a Boolean
  , polygonOffsetFactor :: a Int
  , polygonOffsetUnits :: a Int
  , precision :: a precision
  , premultipliedAlpha :: a Boolean
  , dithering :: a Boolean
  , shadowSide :: a side
  , side :: a side
  , toneMapped :: a Boolean
  , transparent :: a Boolean
  , vertexColors :: a Boolean
  , visible :: a Boolean
  | r
  )
type AllMaterials
  (a :: Type -> Type)
  blendDst
  blendEquation
  blending
  blendSrc
  depthMode
  precision
  side =
  ( | AllMaterials' a blendDst blendEquation blending blendSrc depthMode
      precision
      side
      ()

  )
defaultMaterials
  :: forall blendDst blendEquation blending blendSrc depthMode precision side
   . { | AllMaterials Maybe blendDst blendEquation blending blendSrc depthMode
         precision
         side
     }
defaultMaterials =
  { alphaTest: Nothing
  , alphaToCoverage: Nothing
  , blendDst: Nothing
  , blendDstAlpha: Nothing
  , blendEquation: Nothing
  , blendEquationAlpha: Nothing
  , blending: Nothing
  , blendSrc: Nothing
  , blendSrcAlpha: Nothing
  , clipIntersection: Nothing
  , clipShadows: Nothing
  , colorWrite: Nothing
  , depthFunc: Nothing
  , depthTest: Nothing
  , depthWrite: Nothing
  , opacity: Nothing
  , polygonOffset: Nothing
  , polygonOffsetFactor: Nothing
  , polygonOffsetUnits: Nothing
  , precision: Nothing
  , premultipliedAlpha: Nothing
  , dithering: Nothing
  , shadowSide: Nothing
  , side: Nothing
  , toneMapped: Nothing
  , transparent: Nothing
  , vertexColors: Nothing
  , visible: Nothing
  }
initializeDefaultMaterials
  :: forall opt blendDst blendEquation blending blendSrc
       depthMode precision side r
   . { | AllMaterials' opt blendDst blendEquation
         blending
         blendSrc
         depthMode
         precision
         side
         r
     }
  -> { | AllMaterials opt blendDst blendEquation
         blending
         blendSrc
         depthMode
         precision
         side
     }
initializeDefaultMaterials i =
  { alphaTest: i.alphaTest
  , alphaToCoverage: i.alphaToCoverage
  , blendDst: i.blendDst
  , blendDstAlpha: i.blendDstAlpha
  , blendEquation: i.blendEquation
  , blendEquationAlpha: i.blendEquationAlpha
  , blending: i.blending
  , blendSrc: i.blendSrc
  , blendSrcAlpha: i.blendSrcAlpha
  , clipIntersection: i.clipIntersection
  , clipShadows: i.clipShadows
  , colorWrite: i.colorWrite
  , depthFunc: i.depthFunc
  , depthTest: i.depthTest
  , depthWrite: i.depthWrite
  , opacity: i.opacity
  , polygonOffset: i.polygonOffset
  , polygonOffsetFactor: i.polygonOffsetFactor
  , polygonOffsetUnits: i.polygonOffsetUnits
  , precision: i.precision
  , premultipliedAlpha: i.premultipliedAlpha
  , dithering: i.dithering
  , shadowSide: i.shadowSide
  , side: i.side
  , toneMapped: i.toneMapped
  , transparent: i.transparent
  , vertexColors: i.vertexColors
  , visible: i.visible
  }
--
type MakeMeshBasicMaterial f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeMeshBasicMaterial' Maybe () }
  , materialParameters ::
      { | AllMaterials Maybe BlendDst BlendEquation Blending BlendSrc
          DepthMode
          Precision
          Side
      }
  }
type MakeMeshBasicMaterial' f s =
  { id :: String
  , scope :: s
  , parent :: f String
  , parameters :: { | InitializeMeshBasicMaterial' Undefinable () }
  , materialParameters ::
      { | AllMaterials Undefinable Int Int Int Int
          Int
          String
          Int
      }
  }
type InitializeMeshBasicMaterial'
  (opt :: Type -> Type)
  r =
  ( meshBasicMaterial :: THREE.TMeshBasicMaterial
  , color :: opt Color
  , map :: opt Texture
  , lightMap :: opt Texture
  , lightMapIntensity :: opt Number
  , aoMap :: opt Texture
  , aoMapIntensity :: opt Number
  , alphaMap :: opt Texture
  , envMap :: opt Texture
  , wireframe :: opt Boolean
  , wireframeLinewidth :: opt Number
  | r
  )
newtype InitializeMeshBasicMaterial = InitializeMeshBasicMaterial
  { | InitializeMeshBasicMaterial' Maybe
      ( AllMaterials Maybe BlendDst BlendEquation Blending
          BlendSrc
          DepthMode
          Precision
          Side
      )
  }
type MakeBox f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeBox'
  }
type Buffy =
  ( bufferAttributes :: Object.Object BufferAttribute
  , instancedBufferAttributes :: Object.Object InstancedBufferAttribute
  )
type InitializeBox' =
  ( box :: THREE.TBoxGeometry
  , width :: Number
  , height :: Number
  , depth :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  , depthSegments :: Int
  | Buffy
  )
newtype InitializeBox = InitializeBox { | InitializeBox' }

type MakeCapsule f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeCapsule'
  }
type InitializeCapsule' =
  ( capsule :: THREE.TCapsuleGeometry
  , radius :: Number
  , length :: Number
  , capSegments :: Int
  , radialSegments :: Int
  | Buffy
  )
newtype InitializeCapsule = InitializeCapsule { | InitializeCapsule' }
type MakeCylinder f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeCylinder'
  }
type InitializeCylinder' =
  ( cylinder :: THREE.TCylinderGeometry
  , radiusTop :: Number
  , radiusBottom :: Number
  , height :: Number
  , radialSegments :: Int
  , heightSegments :: Int
  , openEnded :: Boolean
  , thetaStart :: Number
  , thetaLength :: Number
  | Buffy
  )
newtype InitializeCylinder = InitializeCylinder { | InitializeCylinder' }
type MakePlane f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializePlane'
  }
type InitializePlane' =
  ( plane :: THREE.TPlaneGeometry
  , width :: Number
  , height :: Number
  , widthSegments :: Int
  , heightSegments :: Int
  | Buffy
  )
newtype InitializePlane = InitializePlane { | InitializePlane' }
type MakeBufferGeometry f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializeBufferGeometry'
  }
type InitializeBufferGeometry' =
  ( bufferGeometry :: THREE.TBufferGeometry
  | Buffy
  )
newtype InitializeBufferGeometry = InitializeBufferGeometry
  { | InitializeBufferGeometry' }
type MakePerspectiveCamera f s =
  { id :: String
  , scope :: s
  , parent :: f String
  | InitializePerspectiveCamera'
  }

type InitializePerspectiveCamera' =
  ( perspectiveCamera :: THREE.TPerspectiveCamera
  , fov :: Number
  , aspect :: Number
  , near :: Number
  , far :: Number
  )
newtype InitializePerspectiveCamera = InitializePerspectiveCamera
  { | InitializePerspectiveCamera' }

--
type SetUniform = { id :: String, key :: String, value :: Foreign }
-- All materials
type SetAlphaTest = { id :: String, alphaTest :: Number }
type SetAlphaToCoverage = { id :: String, alphaToCoverage :: Number }
type SetBlendDst = { id :: String, blendDst :: BlendDst }
type SetBlendDstAlpha = { id :: String, blendDstAlpha :: BlendDst }
type SetBlendEquation = { id :: String, blendEquation :: BlendEquation }
type SetBlendEquationAlpha =
  { id :: String, blendEquationAlpha :: BlendEquation }
type SetBlending = { id :: String, blending :: Blending }
type SetBlendSrc = { id :: String, blendSrc :: BlendSrc }
type SetBlendSrcAlpha = { id :: String, blendSrcAlpha :: BlendSrc }
type SetClipIntersection = { id :: String, clipIntersection :: Boolean }
type SetClipShadows = { id :: String, clipShadows :: Boolean }
type SetColorWrite = { id :: String, colorWrite :: Boolean }
type SetDepthFunc = { id :: String, depthFunc :: DepthMode }
type SetDepthTest = { id :: String, depthTest :: Boolean }
type SetDepthWrite = { id :: String, depthWrite :: Boolean }
type SetOpacity = { id :: String, opacity :: Number }
type SetPolygonOffset = { id :: String, polygonOffset :: Boolean }
type SetPolygonOffsetFactor = { id :: String, polygonOffsetFactor :: Int }
type SetPolygonOffsetUnits = { id :: String, polygonOffsetUnits :: Int }
type SetPrecision = { id :: String, precision :: Precision }
type SetPremultipliedAlpha = { id :: String, premultipliedAlpha :: Boolean }
type SetDithering = { id :: String, dithering :: Boolean }
type SetShadowSide = { id :: String, shadowSide :: Side }
type SetSide = { id :: String, side :: Side }
type SetToneMapped = { id :: String, toneMapped :: Boolean }
type SetTransparent = { id :: String, transparent :: Boolean }
type SetVertexColors = { id :: String, vertexColors :: Boolean }
type SetVisible = { id :: String, visible :: Boolean }
-- unreal bloom
type SetResolution = { id :: String, resolution :: Vector2 }
type SetStrength = { id :: String, strength :: Number }
type SetThreshold = { id :: String, threshold :: Number }
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
type GetBoundingBox = { id :: String, box :: Box3.Box3 -> Effect Unit }
type GetBoundingSphere =
  { id :: String, sphere :: Sphere.Sphere -> Effect Unit }
type SetInstancedMeshMatrix4 =
  { id :: String, setMatrix4 :: EffectFn1 (EffectFn2 Int Matrix4 Unit) Unit }
type SetInstancedMeshColor =
  { id :: String, setColor :: EffectFn1 (EffectFn2 Int Color Unit) Unit }
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
-- phong
type SetCombine = { id :: String, combine :: CombineOperation }
type SetCombine' = { id :: String, combine :: Int }
type SetFog = { id :: String, fog :: Boolean }
type SetReflectivity = { id :: String, reflectivity :: Number }
type SetRefractionRatio = { id :: String, refractionRatio :: Number }
type SetShininess = { id :: String, shininess :: Number }
type SetSpecular = { id :: String, specular :: Color }
type SetSpecularMap = { id :: String, specularMap :: Texture }
type SetWireframeLinecap =
  { id :: String, wireframeLinecap :: WireframeLinecap }
type SetWireframeLinecap' = { id :: String, wireframeLinecap :: String }
type SetWireframeLinejoin =
  { id :: String, wireframeLinejoin :: WireframeLinejoin }
type SetWireframeLinejoin' = { id :: String, wireframeLinejoin :: String }
-- scene
type SetBackgroundCubeTexture = { id :: String, cubeTexture :: CubeTexture }
type SetBackgroundTexture = { id :: String, texture :: Texture }
type SetBackgroundColor = { id :: String, color :: Color }
-- listeners
type SetOnClick = { id :: String, onClick :: MouseEvent -> Effect Unit }
type SetOnMouseDown =
  { id :: String
  , onMouseDown :: MouseEvent -> Effect (MouseEvent -> Effect Unit)
  }
type SetOnMouseUp = { id :: String, onMouseUp :: MouseEvent -> Effect Unit }
type SetOnMouseMove = { id :: String, onMouseMove :: MouseEvent -> Effect Unit }
type SetOnTouchStart =
  { id :: String
  , onTouchStart ::
      Touch
      -> Effect
           { end :: TouchEvent -> Effect Unit
           , cancel :: TouchEvent -> Effect Unit
           }
  }
type SetOnTouchEnd = { id :: String, onTouchEnd :: Touch -> Effect Unit }
type SetOnTouchMove = { id :: String, onTouchMove :: Touch -> Effect Unit }
type SetOnTouchCancel =
  { id :: String, onTouchCancel :: Touch -> Effect Unit }
type RemoveOnClick = { id :: String, onClick :: MouseEvent -> Effect Unit }
type RemoveOnMouseDown =
  { id :: String
  , onMouseDown :: MouseEvent -> Effect (MouseEvent -> Effect Unit)
  }
type RemoveOnMouseUp = { id :: String, onMouseUp :: MouseEvent -> Effect Unit }
type RemoveOnMouseMove =
  { id :: String, onMouseMove :: MouseEvent -> Effect Unit }
type RemoveOnTouchStart =
  { id :: String
  , onTouchStart ::
      Touch
      -> Effect
           { end :: TouchEvent -> Effect Unit
           , cancel :: TouchEvent -> Effect Unit
           }
  }
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
  { id :: String
  , instanceId :: Int
  , onMouseDown :: MouseEvent -> Effect (MouseEvent -> Effect Unit)
  }
type SetIMOnMouseUp =
  { id :: String, instanceId :: Int, onMouseUp :: MouseEvent -> Effect Unit }
type SetIMOnMouseMove =
  { id :: String, instanceId :: Int, onMouseMove :: MouseEvent -> Effect Unit }
type SetIMOnTouchStart =
  { id :: String
  , instanceId :: Int
  , onTouchStart ::
      Touch
      -> Effect
           { end :: TouchEvent -> Effect Unit
           , cancel :: TouchEvent -> Effect Unit
           }
  }
type SetIMOnTouchEnd =
  { id :: String, instanceId :: Int, onTouchEnd :: Touch -> Effect Unit }
type SetIMOnTouchMove =
  { id :: String, instanceId :: Int, onTouchMove :: Touch -> Effect Unit }
type SetIMOnTouchCancel =
  { id :: String, instanceId :: Int, onTouchCancel :: Touch -> Effect Unit }
type RemoveIMOnClick =
  { id :: String, instanceId :: Int, onClick :: MouseEvent -> Effect Unit }
type RemoveIMOnMouseDown =
  { id :: String
  , instanceId :: Int
  , onMouseDown :: MouseEvent -> Effect (MouseEvent -> Effect Unit)
  }
type RemoveIMOnMouseUp =
  { id :: String, instanceId :: Int, onMouseUp :: MouseEvent -> Effect Unit }
type RemoveIMOnMouseMove =
  { id :: String, instanceId :: Int, onMouseMove :: MouseEvent -> Effect Unit }
type RemoveIMOnTouchStart =
  { id :: String
  , instanceId :: Int
  , onTouchStart ::
      Touch
      -> Effect
           { end :: TouchEvent -> Effect Unit
           , cancel :: TouchEvent -> Effect Unit
           }
  }
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
type SetSizeThroughEffectComposer =
  { id :: String, width :: Number, height :: Number }
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
type DisconnectPass =
  { id :: String
  , parent :: String
  , scope :: Scope
  }
type ConnectGeometry =
  { id :: String
  , parent :: String
  , scope :: Scope
  , raiseId :: String -> ST.ST Region.Global Unit
  }
type ConnectScene =
  { id :: String
  , parent :: String
  , scope :: Scope
  , raiseId :: String -> ST.ST Region.Global Unit
  }
type ConnectCamera =
  { id :: String
  , parent :: String
  , scope :: Scope
  , raiseId :: String -> ST.ST Region.Global Unit
  }
type ConnectMaterial =
  { id :: String
  , parent :: String
  , scope :: Scope
  , raiseId :: String -> ST.ST Region.Global Unit
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
  , boundingBox :: Box3.Box3 -> Effect Unit
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
  -> { boundingBox :: (Box3.Box3 -> Effect Unit) -> payload
     , boundingSphere :: (Sphere.Sphere -> Effect Unit) -> payload
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

pureObject3D
  :: forall m payload
   . Applicative m
  => String
  -> ThreeInterpret payload
  -> { lookAt :: Vector3 -> m payload
     , matrix4 :: Matrix4 -> m payload
     , positionX :: Number -> m payload
     , positionY :: Number -> m payload
     , positionZ :: Number -> m payload
     , quaternion :: Quaternion -> m payload
     , rotateOnAxis ::
         { angle :: Number
         , axis :: Vector3
         }
         -> m payload
     , rotateOnWorldAxis ::
         { angle :: Number
         , axis :: Vector3
         }
         -> m payload
     , rotateX :: Number -> m payload
     , rotateY :: Number -> m payload
     , rotateZ :: Number -> m payload
     , rotationFromAxisAngle ::
         { angle :: Number
         , axis :: Vector3
         }
         -> m payload
     , rotationFromEuler :: Euler -> m payload
     , rotationFromMatrix :: Matrix4 -> m payload
     , rotationFromQuaternion :: Quaternion -> m payload
     , scaleX :: Number -> m payload
     , scaleY :: Number -> m payload
     , scaleZ :: Number -> m payload
     , translateOnAxis ::
         { axis :: Vector3
         , distance :: Number
         }
         -> m payload
     , translateX :: Number -> m payload
     , translateY :: Number -> m payload
     , translateZ :: Number -> m payload
     }
pureObject3D
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
  { matrix4: pure <<< setMatrix4 <<< { id: me, matrix4: _ }
  , quaternion: pure <<< setQuaternion <<< { id: me, quaternion: _ }
  , rotationFromAxisAngle: \{ axis, angle } ->
      pure $ setRotationFromAxisAngle { id: me, axis, angle }
  , rotationFromEuler: pure <<< setRotationFromEuler <<<
      { id: me, euler: _ }
  , rotationFromMatrix: pure <<< setRotationFromMatrix <<<
      { id: me, matrix4: _ }
  , rotationFromQuaternion: pure <<< setRotationFromQuaternion <<<
      { id: me, quaternion: _ }
  , rotateOnAxis: \{ axis, angle } -> pure $ setRotateOnAxis
      { id: me, axis, angle }
  , rotateOnWorldAxis: \{ axis, angle } -> pure $ setRotateOnWorldAxis
      { id: me, axis, angle }
  , rotateX: pure <<< setRotateX <<< { id: me, rotateX: _ }
  , rotateY: pure <<< setRotateY <<< { id: me, rotateY: _ }
  , rotateZ: pure <<< setRotateZ <<< { id: me, rotateZ: _ }
  , translateOnAxis: \{ axis, distance } -> pure $ setTranslateOnAxis
      { id: me, axis, distance }
  , translateX: pure <<< setTranslateX <<< { id: me, translateX: _ }
  , translateY: pure <<< setTranslateY <<< { id: me, translateY: _ }
  , translateZ: pure <<< setTranslateZ <<< { id: me, translateZ: _ }
  , positionX: pure <<< setPositionX <<< { id: me, positionX: _ }
  , positionY: pure <<< setPositionY <<< { id: me, positionY: _ }
  , positionZ: pure <<< setPositionZ <<< { id: me, positionZ: _ }
  , scaleX: pure <<< setScaleX <<< { id: me, scaleX: _ }
  , scaleY: pure <<< setScaleY <<< { id: me, scaleY: _ }
  , scaleZ: pure <<< setScaleZ <<< { id: me, scaleZ: _ }
  , lookAt: pure <<< setLookAt <<< { id: me, v: _ }
  }

newtype ThreeInterpret payload = ThreeInterpret
  { ids :: ST Region.Global String
  , effectComposerRender :: EffectComposerRender -> payload
  , webGLRender :: WebGLRender -> payload
  , css2DRender :: CSS2DRender -> payload
  , css3DRender :: CSS3DRender -> payload
  --
  , makeEffectComposer :: MakeEffectComposer -> payload
  , makeRenderPass :: MakeRenderPass Maybe -> payload
  , makeGlitchPass :: MakeGlitchPass Maybe -> payload
  , makeEffectComposerPass :: MakeEffectComposerPass Maybe -> payload
  , makeBloomPass :: MakeBloomPass Maybe -> payload
  , makeUnrealBloomPass :: MakeUnrealBloomPass Maybe -> payload
  , makeWebGLRenderer :: MakeWebGLRenderer -> payload
  , makeCSS2DRenderer :: MakeCSS2DRenderer -> payload
  , makeCSS3DRenderer :: MakeCSS3DRenderer -> payload
  , makeRaycaster :: MakeRaycaster -> payload
  , makeGroup :: MakeGroup Maybe Scope -> payload
  , makeGLTFGroup :: MakeGLTFGroup Maybe Scope -> payload
  , makeScene :: MakeScene Maybe Scope -> payload
  , makeMesh :: MakeMesh Maybe Scope -> payload
  , makePoints :: MakePoints Maybe Scope -> payload
  , makeInstancedMesh :: MakeInstancedMesh Maybe Scope -> payload
  , makeSphere :: MakeSphere Maybe Scope -> payload
  , makeBox :: MakeBox Maybe Scope -> payload
  , makeCapsule :: MakeCapsule Maybe Scope -> payload
  , makeCylinder :: MakeCylinder Maybe Scope -> payload
  , makePlane :: MakePlane Maybe Scope -> payload
  , makeBufferGeometry :: MakeBufferGeometry Maybe Scope -> payload
  , makeDirectionalLight :: MakeDirectionalLight Maybe Scope -> payload
  , makeAmbientLight :: MakeAmbientLight Maybe Scope -> payload
  , makePointLight :: MakePointLight Maybe Scope -> payload
  , makeRawShaderMaterial :: MakeRawShaderMaterial Maybe Scope -> payload
  , makeShaderMaterial :: MakeShaderMaterial Maybe Scope -> payload
  , makeMeshBasicMaterial :: MakeMeshBasicMaterial Maybe Scope -> payload
  , makeMeshStandardMaterial :: MakeMeshStandardMaterial Maybe Scope -> payload
  , makeMeshLambertMaterial :: MakeMeshLambertMaterial Maybe Scope -> payload
  , makeMeshPhongMaterial :: MakeMeshPhongMaterial Maybe Scope -> payload
  , makePerspectiveCamera :: MakePerspectiveCamera Maybe Scope -> payload
  , makeGLTFCamera :: MakeGLTFCamera Maybe Scope -> payload
  , makeCSS2DObject :: MakeCSS2DObject Maybe Scope -> payload
  , makeCSS3DObject :: MakeCSS3DObject Maybe Scope -> payload
  -- passes
  ---- unreal bloom pass
  -- radius already exists elsewhere
  , setResolution :: SetResolution -> payload
  , setStrength :: SetStrength -> payload
  , setThreshold :: SetThreshold -> payload
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
  -- All Meshes
  , setAlphaTest :: SetAlphaTest -> payload
  , setAlphaToCoverage :: SetAlphaToCoverage -> payload
  , setBlendDst :: SetBlendDst -> payload
  , setBlendDstAlpha :: SetBlendDstAlpha -> payload
  , setBlendEquation :: SetBlendEquation -> payload
  , setBlendEquationAlpha :: SetBlendEquationAlpha -> payload
  , setBlending :: SetBlending -> payload
  , setBlendSrc :: SetBlendSrc -> payload
  , setBlendSrcAlpha :: SetBlendSrcAlpha -> payload
  , setClipIntersection :: SetClipIntersection -> payload
  , setClipShadows :: SetClipShadows -> payload
  , setColorWrite :: SetColorWrite -> payload
  , setDepthFunc :: SetDepthFunc -> payload
  , setDepthTest :: SetDepthTest -> payload
  , setDepthWrite :: SetDepthWrite -> payload
  , setOpacity :: SetOpacity -> payload
  , setPolygonOffset :: SetPolygonOffset -> payload
  , setPolygonOffsetFactor :: SetPolygonOffsetFactor -> payload
  , setPolygonOffsetUnits :: SetPolygonOffsetUnits -> payload
  , setPrecision :: SetPrecision -> payload
  , setPremultipliedAlpha :: SetPremultipliedAlpha -> payload
  , setDithering :: SetDithering -> payload
  , setShadowSide :: SetShadowSide -> payload
  , setSide :: SetSide -> payload
  , setToneMapped :: SetToneMapped -> payload
  , setTransparent :: SetTransparent -> payload
  , setVertexColors :: SetVertexColors -> payload
  , setVisible :: SetVisible -> payload
  -- RawShaderMaterial and ShaderMaterial
  , setUniform :: SetUniform -> payload
  -- MeshStandardMaterial / MeshPhongMaterial
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
  -- Phong only
  , setCombine :: SetCombine -> payload
  , setFog :: SetFog -> payload
  , setReflectivity :: SetReflectivity -> payload
  , setRefractionRatio :: SetRefractionRatio -> payload
  , setShininess :: SetShininess -> payload
  , setSpecular :: SetSpecular -> payload
  , setSpecularMap :: SetSpecularMap -> payload
  , setWireframeLinecap :: SetWireframeLinecap -> payload
  , setWireframeLinejoin :: SetWireframeLinejoin -> payload
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
  -- effect composer
  , setSizeThroughEffectComposer :: SetSizeThroughEffectComposer -> payload
  -- connectors
  , connectMesh :: ConnectMesh -> payload
  , connectScene :: ConnectScene -> payload
  , connectCamera :: ConnectCamera -> payload
  , connectGeometry :: ConnectGeometry -> payload
  , connectMaterial :: ConnectMaterial -> payload
  , connectToScene :: ConnectToScene -> payload
  , disconnect :: Disconnect -> payload
  , disconnectPass :: DisconnectPass -> payload
  --
  , deleteFromCache :: DeleteFromCache -> payload
  --
  -- when a webgl renderer is in a portal, like for example
  -- if it is part of multiple render passes,
  -- we always operate off of the raised ID and do not need
  -- any additional connection logic
  , webGLRendererConnectionNoop :: {} -> payload
  , effectComposerConnectionNoop :: {} -> payload
  }