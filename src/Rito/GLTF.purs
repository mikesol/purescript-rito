module Rito.GLTF where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Exception (Error)
import FRP.Event (Event)
import Rito.Core (ThreeInterpret(..))
import Rito.Core as C
import Rito.Group (Group, unsafeInternalGroup)
import Rito.THREE as THREE

data GLTF
data GLTFLoader

foreign import loader :: THREE.TGLTFLoader -> Effect GLTFLoader

foreign import load
  :: GLTFLoader
  -> String
  -> (GLTF -> Effect Unit)
  -> (Error -> Effect Unit)
  -> Effect Unit

foreign import sceneImpl :: GLTF -> C.RawGroup

scene
  :: forall payload
   . GLTF
  -> Event Group
  -> Array (C.AGroupful payload)
  -> C.AGroup payload
scene gltf = unsafeInternalGroup
  (\(ThreeInterpret { makeGLTFGroup }) -> makeGLTFGroup)
  { group: sceneImpl gltf }

-- todo: support rest of API
-- as we don't support animations yet, this won't be exposed
-- todo: support animations!
-- animations ::  // Array<THREE.AnimationClip>


-- foreign import scenesImpl :: GLTF -> Array C.RawGroup

-- scenes :: forall payload. GLTF -> Array (C.AGroup payload)
-- scenes gltf = map
--   ( \group -> unsafeInternalGroup
--       \(ThreeInterpret { makeGLTFGroup }) { id, scope, parent } ->
--         makeGLTFGroup { id, scope, parent, group: group }
--   )
--   (scenesImpl gltf)

-- foreign import camerasImpl :: forall payload. GLTF -> Array C.RawCamera

-- cameras :: forall payload. GLTF -> Array (C.Camera payload)
-- cameras = camerasImpl

-- foreign import assetImpl :: GLTF -> Foreign

-- asset :: GLTF -> Foreign
-- asset = assetImpl

loadAff :: GLTFLoader -> String -> Aff GLTF
loadAff l url = makeAff \f -> do
  load l url (Right >>> f) (Left >>> f)
  mempty