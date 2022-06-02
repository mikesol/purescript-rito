module Rito.Portal where

import Prelude

import Bolson.Control as Bolson
import Bolson.Core (Element(..), Entity)
import Data.FastVect.FastVect (Vect, index, singleton)
import Data.Newtype (unwrap)
import Data.Profunctor (dimap, lcmap)
import Effect (Effect)
import Prim.Int (class Compare)
import Prim.Ordering (GT)
import Rito.Core as C
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class Portable :: forall k. k -> Constraint
class Portable obj

instance Portable C.Light
instance Portable C.Mesh
instance Portable C.Group
instance Portable C.Groupful
instance Portable C.Scene
instance Portable C.Sceneful
instance Portable C.Camera

unsafeCoerceClosure
  :: forall lock payload obj1 obj2
   . Entity Void (obj1 lock payload) Effect lock
  -> Entity Void (obj2 lock payload) Effect lock
unsafeCoerceClosure = unsafeCoerce

globalGeometryPortal1
  :: forall obj lock payload
   . Portable obj
  => C.Geometry lock payload
  -> (C.Geometry lock payload -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalGeometryPortal1 v c = globalGeometryPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalGeometryPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Vect n (C.Geometry lock payload)
  -> (Vect n (C.Geometry lock payload) -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalGeometryPortal v c = unsafeCoerceClosure $ Bolson.globalPortalSimple
  { doLogic: absurd
  , ids: unwrap >>> _.ids
  , disconnectElement:
      \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
        disconnect { id, scope, parent }
  , toElt: \(C.Geometry e) -> Element e
  }
  { fromElt: \(Element e) -> C.Geometry e
  , giveNewParent: \a b _ -> (unwrap a).connectGeometry b
  , deleteFromCache: unwrap >>> _.deleteFromCache
  }
  v
  (dimap (map (_ $ unit)) unsafeCoerceClosure c)

globalMaterialPortal1
  :: forall obj lock payload
   . Portable obj
  => C.Material lock payload
  -> (C.Material lock payload -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalMaterialPortal1 v c = globalMaterialPortal (singleton v) (lcmap (index (Proxy :: _ 0)) c)

globalMaterialPortal
  :: forall n obj lock payload
   . Compare n (-1) GT
  => Portable obj
  => Vect n (C.Material lock payload)
  -> (Vect n (C.Material lock payload) -> Entity Void (obj lock payload) Effect lock)
  -> Entity Void (obj lock payload) Effect lock
globalMaterialPortal v c = unsafeCoerceClosure $ Bolson.globalPortalSimple
  { doLogic: absurd
  , ids: unwrap >>> _.ids
  , disconnectElement:
      \(C.ThreeInterpret { disconnect }) { id, scope, parent } ->
        disconnect { id, scope, parent }
  , toElt: \(C.Material e) -> Element e
  }
  { fromElt: \(Element e) -> C.Material e
  , giveNewParent: \a b _ -> (unwrap a).connectMaterial b
  , deleteFromCache: unwrap >>> _.deleteFromCache
  }
  v
  (dimap (map (_ $ unit)) unsafeCoerceClosure c)
