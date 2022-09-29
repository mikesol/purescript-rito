module Rito.THREE where

import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff (Aff)

data TCSS2DRenderer
data TCSS2DObject
data TCSS3DRenderer
data TCSS3DObject
data TVector2
data TVector3
data TTextureLoader
data TGLTFLoader
data TQuaternion
data TEuler
data TCubeTextureLoader
data TSphere
data TMatrix4
data TColor
data TBox3
data TScene
data TWebGLRenderer
data TMesh
data TInstancedMesh
data TMeshStandardMaterial
data TMeshLambertMaterial
data TMeshPhongMaterial
data TMeshBasicMaterial
data TShaderMaterial
data TRawShaderMaterial
data TPointLight
data TDirectionalLight
data TAmbientLight
data TGroup
data TPlaneGeometry
data TSphereGeometry
data TBoxGeometry
data TCylinderGeometry
data TCapsuleGeometry
data TPerspectiveCamera
data TRaycaster
data TBufferAttribute
data TInstancedBufferAttribute
data TPoints
data TBufferGeometry
data TEffectComposer
data TRenderPass
data TGlitchPass
data TBloomPass
data TUnrealBloomPass
data TEffectComposerPass
data TFogExp2

foreign import css2DRenderer :: Effect (Promise TCSS2DRenderer)
foreign import css2DObject :: Effect (Promise TCSS2DObject)
foreign import css3DRenderer :: Effect (Promise TCSS3DRenderer)
foreign import css3DObject :: Effect (Promise TCSS3DObject)
foreign import vector2 :: Effect (Promise TVector2)
foreign import vector3 :: Effect (Promise TVector3)
foreign import textureLoader :: Effect (Promise TTextureLoader)
foreign import cubeTextureLoader :: Effect (Promise TCubeTextureLoader)
foreign import gltfLoader :: Effect (Promise TGLTFLoader)
foreign import sphere :: Effect (Promise TSphere)
foreign import quaternion :: Effect (Promise TQuaternion)
foreign import euler :: Effect (Promise TEuler)
foreign import matrix4 :: Effect (Promise TMatrix4)
foreign import color :: Effect (Promise TColor)
foreign import box3 :: Effect (Promise TBox3)
foreign import scene :: Effect (Promise TScene)
foreign import webGLRenderer :: Effect (Promise TWebGLRenderer)
foreign import mesh :: Effect (Promise TMesh)
foreign import meshStandardMaterial :: Effect (Promise TMeshStandardMaterial)
foreign import meshLambertMaterial :: Effect (Promise TMeshLambertMaterial)
foreign import meshPhongMaterial :: Effect (Promise TMeshPhongMaterial)
foreign import meshBasicMaterial :: Effect (Promise TMeshBasicMaterial)
foreign import pointLight :: Effect (Promise TPointLight)
foreign import directionalLight :: Effect (Promise TDirectionalLight)
foreign import ambientLight :: Effect (Promise TAmbientLight)
foreign import group :: Effect (Promise TGroup)
foreign import instancedMesh :: Effect (Promise TInstancedMesh)
foreign import planeGeometry :: Effect (Promise TPlaneGeometry)
foreign import sphereGeometry :: Effect (Promise TSphereGeometry)
foreign import cylinderGeometry :: Effect (Promise TCylinderGeometry)
foreign import boxGeometry :: Effect (Promise TBoxGeometry)
foreign import capsuleGeometry :: Effect (Promise TCapsuleGeometry)
foreign import perspectiveCamera :: Effect (Promise TPerspectiveCamera)
foreign import raycaster :: Effect (Promise TRaycaster)
foreign import shaderMaterial :: Effect (Promise TShaderMaterial)
foreign import rawShaderMaterial :: Effect (Promise TRawShaderMaterial)
foreign import bufferAttribute :: Effect (Promise TBufferAttribute)
foreign import instancedBufferAttribute :: Effect (Promise TInstancedBufferAttribute)
foreign import points :: Effect (Promise TPoints)
foreign import bufferGeometry :: Effect (Promise TBufferGeometry)
foreign import effectComposer :: Effect (Promise TEffectComposer)
foreign import renderPass :: Effect (Promise TRenderPass)
foreign import glitchPass :: Effect (Promise TGlitchPass)
foreign import bloomPass :: Effect (Promise TBloomPass)
foreign import unrealBloomPass :: Effect (Promise TUnrealBloomPass)
foreign import effectComposerPass :: Effect (Promise TEffectComposerPass)
foreign import fogExp2 :: Effect (Promise TFogExp2)

css2DRendererAff = toAffE css2DRenderer :: Aff TCSS2DRenderer
css2DObjectAff = toAffE css2DObject :: Aff TCSS2DObject
css3DRendererAff = toAffE css3DRenderer :: Aff TCSS3DRenderer
css3DObjectAff = toAffE css3DObject :: Aff TCSS3DObject
vector2Aff = toAffE vector2 :: Aff TVector2
vector3Aff = toAffE vector3 :: Aff TVector3
textureLoaderAff = toAffE textureLoader :: Aff TTextureLoader
cubeTextureLoaderAff = toAffE cubeTextureLoader :: Aff TCubeTextureLoader
gltfLoaderAff = toAffE gltfLoader :: Aff TGLTFLoader
sphereAff = toAffE sphere :: Aff TSphere
quaternionAff = toAffE quaternion :: Aff TQuaternion
eulerAff = toAffE euler :: Aff TEuler
matrix4Aff = toAffE matrix4 :: Aff TMatrix4
colorAff = toAffE color :: Aff TColor
box3Aff = toAffE box3 :: Aff TBox3
sceneAff = toAffE scene :: Aff TScene
webGLRendererAff = toAffE webGLRenderer :: Aff TWebGLRenderer
meshAff = toAffE mesh :: Aff TMesh
meshStandardMaterialAff = toAffE meshStandardMaterial :: Aff TMeshStandardMaterial
meshLambertMaterialAff = toAffE meshLambertMaterial :: Aff TMeshLambertMaterial
meshBasicMaterialAff = toAffE meshBasicMaterial :: Aff TMeshBasicMaterial
pointLightAff = toAffE pointLight :: Aff TPointLight
directionalLightAff = toAffE directionalLight :: Aff TDirectionalLight
ambientLightAff = toAffE ambientLight :: Aff TAmbientLight
groupAff = toAffE group :: Aff TGroup
instancedMeshAff = toAffE instancedMesh :: Aff TInstancedMesh
planeGeometryAff = toAffE planeGeometry :: Aff TPlaneGeometry
sphereGeometryAff = toAffE sphereGeometry :: Aff TSphereGeometry
boxGeometryAff = toAffE boxGeometry :: Aff TBoxGeometry
cylinderGeometryAff = toAffE cylinderGeometry :: Aff TCylinderGeometry
capsuleGeometryAff = toAffE capsuleGeometry :: Aff TCapsuleGeometry
perspectiveCameraAff = toAffE perspectiveCamera :: Aff TPerspectiveCamera
raycasterAff = toAffE raycaster :: Aff TRaycaster
shaderMaterialAff = toAffE shaderMaterial :: Aff TShaderMaterial
rawShaderMaterialAff = toAffE rawShaderMaterial :: Aff TRawShaderMaterial
bufferAttributeAff = toAffE bufferAttribute :: Aff TBufferAttribute
instancedBufferAttributeAff = toAffE instancedBufferAttribute :: Aff TInstancedBufferAttribute
pointsAff = toAffE points :: Aff TPoints
meshPhongMaterialAff = toAffE meshPhongMaterial :: Aff TMeshPhongMaterial
bufferGeometryAff = toAffE bufferGeometry :: Aff TBufferGeometry
effectComposerAff = toAffE effectComposer :: Aff TEffectComposer
renderPassAff = toAffE renderPass :: Aff TRenderPass
glitchPassAff = toAffE glitchPass :: Aff TGlitchPass
bloomPassAff = toAffE bloomPass :: Aff TBloomPass
unrealBloomPassAff = toAffE unrealBloomPass :: Aff TUnrealBloomPass
effectComposerPassAff = toAffE effectComposerPass :: Aff TEffectComposerPass
fogExp2Aff = toAffE fogExp2 :: Aff TFogExp2