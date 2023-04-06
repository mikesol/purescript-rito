module Rito.Portal where

import Prelude

import Bolson.Control as Bolson
import Bolson.Core (Element(..), Entity)
import Data.FastVect.FastVect (Vect, index, singleton)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Prim.Int (class Compare)
import Prim.Ordering (GT)
import Rito.Core (ThreeInterpret)
import Rito.Core as C
import Safe.Coerce (class Coercible, coerce)
import Type.Proxy (Proxy(..))

class Portable (obj :: Type -> Type)

instance Portable C.Light
instance Portable C.Mesh
instance Portable C.Group
instance Portable C.Groupful
instance Portable C.Scene
instance Portable C.Sceneful
instance Portable C.Camera
instance Portable C.Renderer

globalGeometryPortal1
  :: forall obj payload
   . Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => C.Geometry payload
  -> (C.Geometry payload -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalGeometryPortal1 v c = globalGeometryPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalGeometryPortal
  :: forall n obj payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => Vect n (C.Geometry payload)
  -> (Vect n (C.Geometry payload) -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalGeometryPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj payload ->  Element (ThreeInterpret payload) () payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ _ -> (unwrap a).connectGeometry b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalMaterialPortal1
  :: forall obj payload
   . Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => C.Material payload
  -> (C.Material payload -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalMaterialPortal1 v c = globalMaterialPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalMaterialPortal
  :: forall n obj payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => Vect n (C.Material payload)
  -> (Vect n (C.Material payload) -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalMaterialPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj payload ->  Element (ThreeInterpret payload) () payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ _ -> (unwrap a).connectMaterial b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

-- scene
globalScenePortal
  :: forall n obj payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => Vect n (C.Scene payload)
  -> (Vect n (C.Scene payload) -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalScenePortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj payload ->  Element (ThreeInterpret payload) () payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ _ -> (unwrap a).connectScene b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalScenePortal1
  :: forall obj payload
   . Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => C.Scene payload
  -> (C.Scene payload -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalScenePortal1 v c = globalScenePortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- camera
globalCameraPortal
  :: forall n obj payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => Vect n (C.Camera payload)
  -> (Vect n (C.Camera payload) -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalCameraPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj payload ->  Element (ThreeInterpret payload) () payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ _ -> (unwrap a).connectCamera b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalCameraPortal1
  :: forall obj payload
   . Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => C.Camera payload
  -> (C.Camera payload -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalCameraPortal1 v c = globalCameraPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- webgl renderer
globalWebGLRendererPortal
  :: forall n obj payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => Vect n (C.WebGLRenderer payload)
  -> (Vect n (C.WebGLRenderer payload) -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalWebGLRendererPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj payload ->  Element (ThreeInterpret payload) () payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a _ _ _ -> (unwrap a).webGLRendererConnectionNoop {}
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalWebGLRendererPortal1
  :: forall obj payload
   . Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => C.WebGLRenderer payload
  -> (C.WebGLRenderer payload -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalWebGLRendererPortal1 v c = globalWebGLRendererPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- webgl renderer
globalEffectComposerPortal
  :: forall n obj payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => Vect n (C.EffectComposer payload)
  -> (Vect n (C.EffectComposer payload) -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalEffectComposerPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj payload ->  Element (ThreeInterpret payload) () payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a _ _ _  -> (unwrap a).effectComposerConnectionNoop {}
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalEffectComposerPortal1
  :: forall obj payload
   . Portable obj
  => Coercible (obj payload) (Element (ThreeInterpret payload) () payload)
  => C.EffectComposer payload
  -> (C.EffectComposer payload -> Entity Void (obj payload))
  -> Entity Void (obj payload)
globalEffectComposerPortal1 v c = globalEffectComposerPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)