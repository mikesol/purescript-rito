module Rito.Portal where

import Prelude

import Bolson.Control as Bolson
import Bolson.Core (Element(..), Entity)
import Data.FastVect.FastVect (Vect, index, singleton)
import Data.Newtype (unwrap)
import Data.Profunctor (lcmap)
import Effect (Effect)
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
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => C.Geometry lock payload
  -> (C.Geometry lock payload -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalGeometryPortal1 v c = globalGeometryPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalGeometryPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => Vect n (C.Geometry lock payload)
  -> (Vect n (C.Geometry lock payload) -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalGeometryPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce
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
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => C.Material lock payload
  -> (C.Material lock payload -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalMaterialPortal1 v c = globalMaterialPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalMaterialPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => Vect n (C.Material lock payload)
  -> (Vect n (C.Material lock payload) -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalMaterialPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce
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
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => Vect n (C.Scene lock payload)
  -> (Vect n (C.Scene lock payload) -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalScenePortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce
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
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => C.Scene lock payload
  -> (C.Scene lock payload -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalScenePortal1 v c = globalScenePortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- camera
globalCameraPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => Vect n (C.Camera lock payload)
  -> (Vect n (C.Camera lock payload) -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalCameraPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce
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
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => C.Camera lock payload
  -> (C.Camera lock payload -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalCameraPortal1 v c = globalCameraPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

-- camera
globalWebGLRendererPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => Vect n (C.WebGLRenderer lock payload)
  -> (Vect n (C.WebGLRenderer lock payload) -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalWebGLRendererPortal v c =
  Bolson.globalPortalSimpleComplex
    { doLogic: absurd
    , ids: unwrap >>> _.ids
    , disconnectElement:
        \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
          disconnect { id, scope, parent }
    , toElt: coerce
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
  => Coercible (obj lock payload) (Element (ThreeInterpret payload) Effect lock payload)
  => C.WebGLRenderer lock payload
  -> (C.WebGLRenderer lock payload -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalWebGLRendererPortal1 v c = globalWebGLRendererPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)