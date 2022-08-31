module Rito.Portal where

import Prelude

import Bolson.EffectFn.Control as Bolson
import Bolson.EffectFn.Core (Element(..), Entity)
import Data.FastVect.FastVect (Vect, index, singleton)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Prim.Int (class Compare)
import Prim.Ordering (GT)
import Rito.Core (ThreeInterpret)
import Rito.Core as C
import Safe.Coerce (class Coercible, coerce)
import Type.Proxy (Proxy(..))

class Portable (obj :: Type -> Type -> Type)

instance Portable C.Light
instance Portable C.Mesh
instance Portable C.Group
instance Portable C.Groupful
instance Portable C.Scene
instance Portable C.Sceneful
instance Portable C.Camera
instance Portable C.Renderer

globalGeometryPortal1
  :: forall obj lock payload
   . Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => C.Geometry lock payload
  -> (C.Geometry lock payload -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalGeometryPortal1 v c = globalGeometryPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalGeometryPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => Vect n (C.Geometry lock payload)
  -> (Vect n (C.Geometry lock payload) -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalGeometryPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj lock payload ->  Element (ThreeInterpret payload) () lock payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ -> (unwrap a).connectGeometry b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalMaterialPortal1
  :: forall obj lock payload
   . Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => C.Material lock payload
  -> (C.Material lock payload -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalMaterialPortal1 v c = globalMaterialPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalMaterialPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => Vect n (C.Material lock payload)
  -> (Vect n (C.Material lock payload) -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalMaterialPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj lock payload ->  Element (ThreeInterpret payload) () lock payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ -> (unwrap a).connectMaterial b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

-- scene
globalScenePortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => Vect n (C.Scene lock payload)
  -> (Vect n (C.Scene lock payload) -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalScenePortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj lock payload ->  Element (ThreeInterpret payload) () lock payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ -> (unwrap a).connectScene b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalScenePortal1
  :: forall obj lock payload
   . Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => C.Scene lock payload
  -> (C.Scene lock payload -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalScenePortal1 v c = globalScenePortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- camera
globalCameraPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => Vect n (C.Camera lock payload)
  -> (Vect n (C.Camera lock payload) -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalCameraPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj lock payload ->  Element (ThreeInterpret payload) () lock payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a b _ -> (unwrap a).connectCamera b
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalCameraPortal1
  :: forall obj lock payload
   . Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => C.Camera lock payload
  -> (C.Camera lock payload -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalCameraPortal1 v c = globalCameraPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- webgl renderer
globalWebGLRendererPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => Vect n (C.WebGLRenderer lock payload)
  -> (Vect n (C.WebGLRenderer lock payload) -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalWebGLRendererPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj lock payload ->  Element (ThreeInterpret payload) () lock payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a _ _  -> (unwrap a).webGLRendererConnectionNoop {}
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalWebGLRendererPortal1
  :: forall obj lock payload
   . Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => C.WebGLRenderer lock payload
  -> (C.WebGLRenderer lock payload -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalWebGLRendererPortal1 v c = globalWebGLRendererPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- webgl renderer
globalEffectComposerPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => Vect n (C.EffectComposer lock payload)
  -> (Vect n (C.EffectComposer lock payload) -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalEffectComposerPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce :: obj lock payload ->  Element (ThreeInterpret payload) () lock payload
    }
    { fromEltO1: coerce
    , fromEltO2: coerce
    , toElt: coerce
    , giveNewParent: \a _ _  -> (unwrap a).effectComposerConnectionNoop {}
    , deleteFromCache: unwrap >>> _.deleteFromCache
    }
    v
    (lcmap (map (_ $ unit)) c)

globalEffectComposerPortal1
  :: forall obj lock payload
   . Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) () lock payload)
  => C.EffectComposer lock payload
  -> (C.EffectComposer lock payload -> Entity Void (obj lock payload) lock)
  -> Entity Void (obj lock payload) lock
globalEffectComposerPortal1 v c = globalEffectComposerPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)